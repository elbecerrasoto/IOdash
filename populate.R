#!/usr/bin/Rscript
library(tidyverse)
library(glue)
library(furrr)
source("clean.R")

# ---- globals

DOWNLOAD <- TRUE
STEM <- "https://www.inegi.org.mx/contenidos/investigacion/coumip/tabulados"

DATA_DIR <- "data"
# Create directory outside function calls
# as to avoid any weird concurrent effects
if (!file.exists(DATA_DIR)) {
  dir.create(DATA_DIR)
}

CORES <- future::availableCores()
if (.Platform$OS.type == "windows") {
  plan(multicore, workers = CORES)
} else {
  plan(multisession, workers = CORES)
}

STATE_CODES <- c(
  aguascalientes = "ags",
  baja_california = "bc",
  baja_california_sur = "bcs",
  campeche = "camp",
  coahuila = "coah",
  coliman = "col",
  chiapas = "chis",
  chihuhua = "chih",
  ciudad_mexico = "cdmx",
  durango = "dgo",
  guanajuato = "gto",
  guerrero = "gro",
  hidalgo = "hgo",
  jalisco = "jal",
  estado_mexico = "mex",
  michoacan = "mich",
  morelia = "mor",
  nayarit = "nay",
  nuevo_leon = "nl",
  oaxaca = "oax",
  puebla = "pue",
  queretaro = "qro",
  quintana_roo = "qr",
  san_luis_potosi = "slp",
  sinaloa = "sin",
  sonora = "son",
  tabasco = "tab",
  tamaulipas = "tamps",
  tlaxcala = "tlax",
  veracruz = "ver",
  yucatan = "yuc",
  zacatecas = "zac"
)

URLs <- str_c(STEM, "/mip_ixi_br_", STATE_CODES, "_d_2018.xlsx")
TSVs <- str_c("data/mip_ixi_br_", STATE_CODES, "_d_2018.tsv")

# ---- code

main <- function(i) {
  url_i <- URLs[[i]]
  tsv_i <- TSVs[[i]]

  xlsx_i <- get_xlsx(url_i, DATA_DIR, download = DOWNLOAD)

  clean_mipbr_xlsx(xlsx_i) |>
    write_tsv(tsv_i)
}

done <- future_map(1:length(URLs), main)
