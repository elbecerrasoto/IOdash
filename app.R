#!/usr/bin/Rscript
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(igraph)
library(heatmaply)
library(dplyr)

# User Interface
ui <- dashboardPage(
  dashboardHeader(title = "IO Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("table")),
      menuItem("Simulator", tabName = "simulator", icon = icon("calculator")),
      menuItem("Explore", tabName = "explore", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    # CSS component
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # First tab: Summary
      tabItem(tabName = "summary",
              h2("Data Summary"),
              DTOutput("tabla_summary")),
      
      # Second tab: Simulator
      tabItem(tabName = "simulator",
              h2("Simulator"),
              fileInput("uploadFile", "Upload TSV file", 
                        accept = c(".tsv", "text/tsv", ".xlsx"), 
                        buttonLabel = "Upload..."),
              actionButton("calcule", "Calculate matrix"),
              h3("Uploaded Data"),
              DTOutput("uploadedTable"),
              h3("Sum Matrix (L)"),
              DTOutput("matrixL")),
      
      # Third tab: Explore
      tabItem(tabName = "explore",
              h2("Visualizations"),
              fluidRow(
                box(title = "Heatmap", plotlyOutput("heatmap")),
                box(title = "Directed graph", plotOutput("grafo"))
              )
      )
    )
  )
)
  
  server <- function(input, output, session) {
    # Reactive value to store uploaded data
    uploaded_data <- reactiveVal(NULL)
    matrix_L <- reactiveVal(NULL)
    sum_result <- reactiveVal(NULL)
    
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
    
    # Handle file upload
    observeEvent(input$uploadFile, {
      req(input$uploadFile)
      
      tryCatch({
        ext <- tools::file_ext(input$uploadFile$name)
        
        if (!ext %in% c("tsv", "xlsx")) {
          stop("Not supported. Use .tsv or .xlsx instead.")
        }
        
        # Read tab file
        df <- read.delim(input$uploadFile$datapath)
        df[, 2] <- ifelse(is.na(df[, 2]), 0, df[, 2])  # Replace NAs in column 2
        
        if(ncol(df) < 2 || !is.numeric(df[[2]])) {
          stop("The file must have at least 2 columns with numeric values in the second column")
        }
        
        uploaded_data(df)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
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
      
      plot(g, edge.width = E(g)$weight/10, 
           vertex.size = 20, vertex.label.cex = 0.8,
           main = "Data Graph")
    })
  }
  
  shinyApp(ui, server)