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

mip_br <- read_xlsx(MIP_BR, col_names = FALSE)

# Remove the last 2 rows
mip_br <- mip_br |> slice(-(n() - 1):-n())

# Remove second to last row
row_target <- unlist(mip_br[(nrow(mip_br) - 1), ])
stopifnot("Unexpected Data" = all(is.na(row_target)))
mip_br <- mip_br[-(nrow(mip_br) - 1), ]

# Remove second to last col
col_target <- unlist(mip_br[, (ncol(mip_br) - 1)])
stopifnot("Unexpected Data" = all(is.na(col_target)))
mip_br <- mip_br[, -(ncol(mip_br) - 1)]

# Get col headers
h5_regions <- as.character(mip_br[5, ])
h6_sectors <- as.character(mip_br[6, ])
h5_regions[is.na(h5_regions)] <- ""
h6_sectors[is.na(h6_sectors)] <- ""

# Remove the first 6 rows
mip_br <- mip_br |>
  slice(-1:-6)

# Ger row headers
mip_br[[1]][is.na(mip_br[[1]])] <- ""
mip_br[[2]][is.na(mip_br[[2]])] <- ""

col_keys <- str_c(h5_regions, " ::: ", h6_sectors)
row_keys <- str_c(mip_br[[1]], " ::: ", mip_br[[2]])


# Drop first two cols
mip_br <- mip_br |>
  select(-...1, -...2)

mip_br <- mip_br |>
  mutate(across(everything(), as.double))
