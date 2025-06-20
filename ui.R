#!/usr/bin/Rscript
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)

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
      tabItem(
        tabName = "summary",
        h2("Data Summary"),
        h3("Análisis de Multiplicadores"),
        box(
          width = 12,
          plotOutput("multiplier_plot", height = "700px") # Custom height
        ),
        selectInput(
          "state",
          "Select a State:",
          choices = setNames(states$corto, states$nombre), # Shows complete name, but uses short name
          selected = "cdmx"
        ),
        DTOutput("state_data")
      ),

      # Second tab: Simulator
      tabItem(
        tabName = "simulator",
        h2("Simulator"),
        fluidRow(
          column(
            width = 4,
            fileInput(
              "uploadFile",
              "Upload TSV file",
              accept = c(".tsv", "text/tsv", ".xlsx"),
              buttonLabel = "Upload..."
            )
          ),
          column(
            width = 4,
            div(
              style = "margin-top: 25px;",
              uiOutput("buttonCalc")
            )
          )
        ),
        # fileInput("uploadFile", "Upload TSV file",
        #   accept = c(".tsv", "text/tsv", ".xlsx"),
        #   buttonLabel = "Upload..."
        # ),
        # actionButton("calcule", "Calculate matrix"),
        uiOutput("h3UpData"),
        DTOutput("uploadedTable"),
        uiOutput("sumMatL"),
        DTOutput("matrixL")
      ),

      # Third tab: Explore
      tabItem(
        tabName = "explore",
        h2("Visualizations"),
        fluidRow(
          box(title = "Heatmap", plotlyOutput("heatmap")),
          box(title = "Directed graph", plotOutput("grafo"))
        )
      )
    )
  )
)
