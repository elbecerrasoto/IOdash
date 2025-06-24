#!/usr/bin/Rscript
library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(ggrepel)
library(viridis)
library(writexl)
source("simulator.R")

# ---- helpers

make_bar_multipliers <- function(multi) {
  # Graph
  ggplot(multi, aes(
    x = region,
    y = multiplier,
    fill = sector,
    label = round(multiplier, 2)
  )) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.96)) +
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
      plot.margin = margin(t = 20, r = 10, b = 40, l = 10, unit = "pt"), # Ajuste clave
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        margin = margin(b = 20) # Espacio bajo el título
      ),
    ) +
    guides(
      fill = guide_legend(ncol = 4, nrow = 10)
    ) +
    scale_fill_viridis_d(option = "G")
}

# ---- server

server <- function(input, output, session) {
  # Reactive value to store uploaded data
  Z_aug <- reactive(MIPS_BR[[input$state]])
  ZALfx_multipliers <- reactive(get_ZALfx_multipliers(Z_aug(), N_SECTORS))

  L <- reactive(ZALfx_multipliers()$L)
  f <- reactive(ZALfx_multipliers()$f)
  x <- reactive(ZALfx_multipliers()$x)
  multipliers <- reactive(ZALfx_multipliers()$multipliers)

  uploaded_data <- reactiveVal(NULL)
  matrix_L <- reactiveVal(NULL)
  sum_result <- reactiveVal(NULL)

  # :::::::::::::::::::::::::: TAB 2 - SUMMARY :::::::::::::::::::::::::::::::::

  # Custom header
  output$summary_header <- renderUI({
    req(input$state) # Requires state selection

    # Get full state name from states data
    full_name <- STATES_CSV$fullname[STATES_CSV$id == input$state]

    h2(paste("Data Summary for", full_name))
  })

  output$multipliers_title <- renderUI({
    req(multipliers) # Requires state selection
    h2("Multipliers")
  })

  # debug_values <- reactiveValues(multi_data = NULL)

  # Render a barplot
  output$multiplier_plot <- renderPlot({
    req(multipliers())

    # 1. Store multipliers() in 'multi' and sort by multiplier (descending)
    multi <- multipliers() %>%
      arrange(desc(multiplier))

    # 2. Trim sector names to first 40 characters
    multi$sector <- substr(multi$sector, start = 1, stop = 40) # start=1 (no 0) para evitar errores

    # 3. Convert to factor while preserving current order
    multi <- multi %>%
      mutate(sector = factor(sector, levels = unique(sector)))

    # Debug errors
    # debug_values$multi_data <- multi
    # print(head(debug_values$multi_data))

    # Graph
    make_bar_multipliers(multi)
  })


  # States multiplier table
  shock_multipliers <- reactive(rep(input$shock_multiplier, N_SECTORS))

  results_raw <- reactive(
    simulate_demand_shocks(
      shock_multipliers(),
      L(),
      f(),
      x(),
      shocks_are_multipliers = TRUE
    )
  )

  results <- reactive({
    results_raw() |>
      mutate(
        sector = multipliers()$sector,
        region = multipliers()$region
      )
  })

  output$results <- renderDataTable(results())

  # Render state table
  output$multipliers <- renderDT({
    req(multipliers())
    datatable(
      multipliers(),
      options = list(
        scrollX = TRUE,
        pageLength = 15,
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

  output$download_buttons_mult <- renderUI({
    req(multipliers())
    div(
      style = "margin-top: 20px;",
      downloadButton("download_xlsx_mult", "Download XLSX",
        style = "color: white; background-color: #4CAF50; margin-right: 10px;"
      ),
      downloadButton("download_tsv_mult", "Download TSV",
        style = "color: white; background-color: #2196F3;"
      )
    )
  })

  # XLSX Download Handler
  output$download_xlsx_mult <- downloadHandler(
    filename = function() {
      paste("multipliers-", input$state, "-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(multipliers())
      writexl::write_xlsx(multipliers(), file)
    }
  )

  # TSV Download Handler
  output$download_tsv_mult <- downloadHandler(
    filename = function() {
      paste("multipliers-", input$state, "-", Sys.Date(), ".tsv", sep = "")
    },
    content = function(file) {
      req(multipliers())
      readr::write_tsv(multipliers(), file)
    }
  )

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
      actionButton("calcule", "Calculate Matrix")
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

    # Calculate Matrix L (sum matrix)
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

  # ::::::::::::::::::::: TAB 3  PRODUCTION SIM ::::::::::::::::::::::::::::::::::

  # Static values for valueBox
  output$gdp_state_box <- renderValueBox({
    valueBox(
      value = "1.1",
      subtitle = "GDP State (millions)",
      icon = icon("landmark", lib = "font-awesome"),
      color = "purple"
    )
  })

  output$gdp_rest_box <- renderValueBox({
    valueBox(
      value = "12.4",
      subtitle = "GDP Rest (millions)",
      icon = icon("globe-americas", lib = "font-awesome"),
      color = "blue" # bg-blue en CSS
    )
  })

  output$employment_state_box <- renderValueBox({
    valueBox(
      value = "45",
      subtitle = "Employment State (thousands)",
      icon = icon("industry", lib = "font-awesome"),
      color = "green" # bg-green en CSS
    )
  })

  output$employment_rest_box <- renderValueBox({
    valueBox(
      value = "12.2",
      subtitle = "Employment Rest (thousands)",
      icon = icon("users", lib = "font-awesome"),
      color = "yellow"
    )
  })

  # Row number
  n_rows <- 70

  # Generate input table
  output$input_table <- renderUI({
    # Dividimos los inputs en dos grupos
    primera_mitad <- lapply(1:35, function(i) {
      splitLayout(
        cellWidths = c("30%", "70%"),
        tags$div(paste("Sector", i), style = "text-align: right; padding-right: 10px;"),
        numericInput(
          inputId = paste0("input_", i),
          label = NULL,
          value = 1,
          width = "100%"
        )
      )
    })

    segunda_mitad <- lapply(36:70, function(i) {
      splitLayout(
        cellWidths = c("30%", "70%"),
        tags$div(paste("Sector", i), style = "text-align: right; padding-right: 10px;"),
        numericInput(
          inputId = paste0("input_", i),
          label = NULL,
          value = 1,
          width = "100%"
        )
      )
    })

    # Creamos dos columnas
    fluidRow(
      column(6, do.call(tagList, primera_mitad)),
      column(6, do.call(tagList, segunda_mitad))
    )
  })

  # Fill all inputs
  observeEvent(input$fill_all, {
    if (!is.na(input$fill_value)) {
      for (i in 1:n_rows) {
        updateNumericInput(
          session,
          inputId = paste0("input_", i),
          value = input$fill_value
        )
      }
    }
  })

  # Get all the values
  observeEvent(input$get_values, {
    values <- sapply(1:n_rows, function(i) {
      val <- input[[paste0("input_", i)]]
      if (is.na(val)) 1 else val
    })

    # Values summary
    output$value_output <- renderPrint({
      cat("Values:\n")
      print(values)
      cat("\nSummary:\n")
      cat("AVG:", mean(values, na.rm = TRUE), "\n")
      cat("Total:", sum(values, na.rm = TRUE))
    })
  })
}
