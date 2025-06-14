#!/usr/bin/Rscript

library(tidyverse)
library(janitor)
library(readxl)
library(glue)

STEM <- "https://www.inegi.org.mx/contenidos/investigacion/coumip/tabulados"
MIP_BR <- "mip_ixi_br_sin_d_2018.xlsx"

if (!file.exists(MIP_BR)) {
  system(glue("wget {STEM}/{MIP_BR}"))
}

raw <- read_xlsx(MIP_BR, col_names = FALSE)
mip_br <- raw # Copy for edition

# Get headers
h5_regions <- as.character(raw[5, ])
h6_sectors <- as.character(raw[6, ])

# Remove first six rows
mip_br <- mip_br[-1:-6, ]

# Remove last two rows
mip_br <- mip_br[-(nrow(raw) - 1):-nrow(raw), ]

# Remove second to last row
row_target <- unlist(mip_br[(nrow(mip_br) - 1), ])
stopifnot("Unexpected Data" = all(is.na(row_target)))
mip_br <- mip_br[-(nrow(mip_br) - 1), ]

# Remove second to last col
col_target <- unlist(mip_br[, (ncol(mip_br) - 1)])
stopifnot("Unexpected Data" = all(is.na(col_target)))
mip_br <- mip_br[, -(ncol(mip_br) - 1)]
