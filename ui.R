#!/usr/bin/Rscript
library(tidyverse)
library(shiny)
library(shinydashboard) # We'll keep this for styles but could remove it
library(plotly)
library(DT)

ui <- fluidPage(
  tags$style(".container-fluid {
                             background-color: #2C3E50;
                             color: #ffffff;
  }"),
  # CSS component (you can keep your custom.css)
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  # Application title
  titlePanel("IO Dashboard"),

  # Main panel with tabs
  tabsetPanel(
    id = "mainTabs",
    type = "tabs",

    # First tab: Welcome
    tabPanel(
      "Home",
      h2("Welcome"),
      p("This is a simulator"),
      p("1. Before exploring the economic multipliers or running simulations, please select your state of interest:"),
      selectInput(
        "state",
        "Select a State:",
        choices = setNames(STATES_CSV$id, STATES_CSV$fullname), # Shows full name but uses short name
        selected = "ciudad_mexico"
      ),
      p("2. Increase on final demand by sector:"),
      sliderInput("shock_multiplier", "Multiply by this amount",
        value = 1.01, min = 0.50, max = 2.00
      ),
    ),

    # Second tab: Summary
    tabPanel(
      "Summary",
      uiOutput("summary_header"), # Dynamic header
      h3("Multiplier Analysis"),
      box(
        width = 12,
        plotOutput("multiplier_plot", height = "700px"), # Custom height
      ),
      div(
        style = "margin-top: 20px; margin-bottom: 20px;",
        uiOutput("multipliers_title"),
        DTOutput("multipliers")
      ),
      uiOutput("download_buttons_mult")
    ),

    # Third tab: Production Simulator
    tabPanel(
      "Production Sim",
      h2("Production Simulator"),
      p("This is a text for explaining how to use the simulator"),
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
      uiOutput("h3UpData"),
      DTOutput("uploadedTable"),
      uiOutput("sumMatL"),
      DTOutput("matrixL")
    ),

    # Fourth tab: Employment Simulator
    tabPanel(
      "Employment Sim",
      h2("Employment Simulator"),
      p("This is a text for explaining how to use the simulator"),
      h3("1. Upload your file"),
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
      h3("2. Choose options for your analysis"),
      uiOutput("h3UpData"),
      DTOutput("uploadedTable"),
      uiOutput("sumMatL"),
      DTOutput("matrixL")
    ),

    # Fifth tab: Explore
    tabPanel(
      "Explore",
      h2("Data Visualizations"),
      fluidRow(
        box(title = "Heatmap", plotlyOutput("heatmap")),
        box(title = "Directed Graph", plotOutput("grafo"))
      )
    )
  )
)
