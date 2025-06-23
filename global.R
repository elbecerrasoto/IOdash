library(tidyverse)
library(shiny)
library(shinydashboard)
library(readr)
library(rlang)
library(dplyr)

# Load all data files at once
data_files <- list(
  states = read.csv("data/states.csv", encoding = "UTF-8"),
  multiplier = read_tsv("data/multiplier_cdmx.tsv"),
  mips_data = read_rds("data/mipsBR.Rds")
)

# Process states data in a single pipeline
STATES_CSV <- data_files$states %>%
  mutate(id = names(data_files$mips_data)) %>% # Assign IDs from MIPS_BR names
  arrange(fullname) # Sort alphabetically by state name

# Constants
N_SECTORS <- 35 * 2 # Total number of sectors (35 original × 2 types)
