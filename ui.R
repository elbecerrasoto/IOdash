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
  titlePanel("Input Output Economic Models for Mexico"),

  # Main panel with tabs
  tabsetPanel(
    id = "mainTabs",
    type = "tabs",

    # tab: Summary
    tabPanel(
      "Mulitpliers",
      uiOutput("summary_header"), # Dynamic header
      selectInput(
        "state",
        "Select a State:",
        choices = setNames(STATES_CSV$id, STATES_CSV$fullname), # Shows full name but uses short name
        selected = "ciudad_mexico"
      ),
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

    # tab: Simulator
    tabPanel(
      "Simulator",
      h2("Simulator"),
      valueBoxOutput("gdp_state_box"),
      valueBoxOutput("gdp_rest_box"),
      valueBoxOutput("employment_state_box"),
      valueBoxOutput("employment_rest_box")
    )
  )
)
