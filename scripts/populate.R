#!/usr/bin/Rscript
library(tidyverse)
library(glue)
library(furrr)
source("scripts/clean.R")

# ---- globals

DOWNLOAD <- TRUE
WRITE_TO_TSV <- TRUE
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

URLs <- str_c(STEM, "/mip_ixi_br_", STATE_CODES, "_d_2018.xlsx") |>
  set_names(STATE_CODES)
TSVs <- str_c(STEM, "/mip_ixi_br_", STATE_CODES, "_d_2018.tsv") |>
  set_names(STATE_CODES)
# ---- code

main <- function(state_code) {
  url_i <- URLs[[state_code]]
  tsv_i <- TSVs[[state_code]]

  xlsx_i <- get_xlsx(url_i, DATA_DIR, download = DOWNLOAD)
  out_tib <- clean_mipbr_xlsx(xlsx_i)

  if (WRITE_TO_TSV) {
    write_tsv(out_tib, tsv_i)
  }

  out_tib
}

done <- future_map(STATE_CODES, main)
write_rds(done, glue("{DATA_DIR}/mipsBR.Rds"))