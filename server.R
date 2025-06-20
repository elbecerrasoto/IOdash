#!/usr/bin/Rscript
library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(ggrepel)
library(igraph)
library(heatmaply)
library(viridis)

server <- function(input, output, session) {
  # Reactive value to store uploaded data
  uploaded_data <- reactiveVal(NULL)
  matrix_L <- reactiveVal(NULL)
  sum_result <- reactiveVal(NULL)

  # :::::::::::::::::::::::::::::: TAB 1 ::::::::::::::::::::::::::::::::::::::::::

  # Render a barplot
  output$multiplier_plot <- renderPlot({
    # Order
    multi <- multi %>%
      arrange(desc(multi)) %>%
      mutate(sector = factor(sector, levels = unique(sector)))

    # Reduce sector label
    # multi$sector <- str_trunc(multi$sector, 25, side = "right")
    multi$sector <- substr(multi$sector, start = 0, stop = 40)

    # Graph
    ggplot(multi, aes(
      x = region,
      y = multiplier,
      fill = sector,
      label = round(multiplier, 2)
    )) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.96)) +
      # geom_label(
      #   position = position_dodge(width = 0.96),
      #   fill = "white"
      #   vjust = -0.9,
      #   check_overlap = TRUE,
      #   fontface = "bold",
      #   size = 3,
      #   angle = 0,
      #   color = "black",
      # )
      geom_label_repel(
        aes(group = sector), # Alinea con los grupos creados por fill
        position = position_dodge(width = 0.96),
        fill = "white",
        fontface = "bold",
        size = 3,
        angle = 0,
        color = "black"
      ) +
      labs(
        title = "Multiplicadores Económicos por Región y Sector",
        x = "Región",
        y = "Valor del Multiplicador",
        fill = "Sector"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")
      ) +
      guides(
        fill = guide_legend(ncol = 4, nrow = 10)
      ) +
      scale_fill_viridis_d(option = "G")
  })

  # States data
  state_data <- reactive({
    req(input$state)

    # Find file with regex
    pattern <- sprintf("mip_ixi_br_%s_d_\\d{4}\\.(tsv|xlsx)", input$state)
    files <- list.files("data/", pattern = pattern, full.names = TRUE)

    if (length(files) == 0) {
      showNotification("No file found for this State", type = "warning")
      return(NULL)
    }

    # Search for the most recent file
    selected_file <- files[1]

    # Read file by it's MIME
    ext1 <- tools::file_ext(selected_file)

    tryCatch(
      {
        if (ext1 == "tsv") {
          dfState <- read.delim(selected_file, na.strings = c("", "NA", "N/A"))
        } else if (ext1 == "xlsx") {
          dfState <- read_excel(selected_file, na = c("", "NA", "N/A"))
        }

        # Limpieza segura de datos:
        if (!is.null(dfState)) {
          dfState <- dfState %>%
            mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>%
            mutate(across(where(is.character), ~ ifelse(is.na(.), "", .)))
        }

        return(dfState)
      },
      error = function(e) {
        showNotification(paste("Error loading state data:", e$messageState), type = "error")
        return(NULL)
      }
    )
  })

  # Render state table
  output$state_data <- renderDT({
    req(state_data())
    datatable(
      state_data(),
      options = list(
        scrollX = TRUE,
        pageLength = 35,
        language = list(search = "Search:"),

        # Give tooltip the title of the column
        initComplete = JS("
        function(settings, json) {
          $('table.dataTable thead th').each(function() {
            var title = $(this).text();
            $(this).attr('title', title);
          });
        }
      ")
      )
    )
  })

  # Data for summary
  datos <- reactive({
    data.frame(
      ID = 1:10,
      Valor = rnorm(10, mean = 50, sd = 10)
    )
  })

  # Summary tab
  output$tabla_summary <- renderDT({
    datatable(datos())
  })

  # :::::::::::::::::::::::::::::: TAB 2 ::::::::::::::::::::::::::::::::::::::::::

  # Show titles only if file is TRUE
  output$h3UpData <- renderUI({
    req(input$uploadFile)
    h3("Uploaded Data")
  })

  output$sumMatL <- renderUI({
    req(matrix_L())
    h3("Sum Matrix (L)")
  })

  # Show actionButton
  output$buttonCalc <- renderUI({
    req(input$uploadFile) # Solo continua si hay archivo
    div(
      style = "margin-top: 25px;",
      actionButton("calcule", "Calculate matrix")
    )
  })

  # Handle file upload
  observeEvent(input$uploadFile, {
    req(input$uploadFile)

    tryCatch(
      {
        ext <- tools::file_ext(input$uploadFile$name)

        if (!ext %in% c("tsv", "xlsx")) {
          stop("Not supported. Use .tsv or .xlsx instead.")
        }

        # Read tab file
        df <- read.delim(input$uploadFile$datapath)
        df[, 2] <- ifelse(is.na(df[, 2]), 0, df[, 2]) # Replace NAs in column 2

        if (ncol(df) < 2 || !is.numeric(df[[2]])) {
          stop("The file must have at least 2 columns with numeric values in the second column")
        }

        uploaded_data(df)
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  # Show uploaded data
  output$uploadedTable <- renderDT({
    req(uploaded_data())
    datatable(uploaded_data())
  })

  # Calculation
  observeEvent(input$calcule, {
    req(uploaded_data())

    values <- uploaded_data()[[2]]

    # Calculate matrix L (sum matrix)
    l_matrix <- outer(values, values, `+`)
    colnames(l_matrix) <- paste0("V", 1:ncol(l_matrix))
    rownames(l_matrix) <- paste0("V", 1:nrow(l_matrix))
    matrix_L(l_matrix)
  })

  # Show matrix L
  output$matrixL <- renderDT({
    req(matrix_L())
    datatable(matrix_L(), options = list(scrollX = TRUE))
  })

  # :::::::::::::::::::::::::::::: TAB 3 ::::::::::::::::::::::::::::::::::::::::::

  # Explore tab
  output$heatmap <- renderPlotly({
    req(matrix_L())
    heatmaply(matrix_L(), main = "Sum Matrix Heatmap")
  })

  output$grafo <- renderPlot({
    req(uploaded_data())
    g <- graph_from_data_frame(data.frame(
      from = rep("Source", nrow(uploaded_data())),
      to = paste0("Node-", uploaded_data()[[1]]),
      weight = uploaded_data()[[2]]
    ))

    plot(g,
      edge.width = E(g)$weight / 10,
      vertex.size = 20, vertex.label.cex = 0.8,
      main = "Data Graph"
    )
  })
}
