library(shiny)
library(shinydashboard)
library(DT)
library(readr)

# Carga datos compartidos
states <- read.csv("data/states.csv", encoding = "UTF-8")
multi <- read.delim("data/multiplier_cdmx.tsv", sep = "\t")
mipsBR <- read_rds("data/mipsBR.Rds")
