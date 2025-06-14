#!/usr/bin/Rscript

library(tidyverse)
library(janitor)
library(readxl)
library(glue)

STEM <- "https://www.inegi.org.mx/contenidos/investigacion/coumip/tabulados"
MIP_BR <- "mip_ixi_br_sin_d_2018.xlsx"

if (!file.exists(MIP_BR)) {
  system(glue("wget {STEM}/{MIP_BR_SIN}"))
}

raw <- read_xlsx(MIP_BR, col_names = FALSE)
h5_regions <- as.character(x[5, ])
h6_sectors <- as.character(x[6, ])

mip_br <- raw
tail(raw)
str_c(h5_regions, " ::: ", h6_sectors)

SKIP <- 6
?read_xlsx
mip_br_sin <- read_xlsx(MIP_BR_SIN, col_names = FALSE, skip = 6)

mip_br_sin |> tail(10)


dim(x)

HEADERS <-
  mip_br_sin <- read_xlsx(MIP_BR_SIN, col_names = FALSE, skip = 6)

# rm totals and NA rows
nr <- nrow(mip_br_sin)
mip_br_sin <- mip_br_sin[-(nr - 4):-nr, ]

# h1 <- h1 |> str_replace("Resto del País", "rest") |> str_replace("Sinaloa", "sin")
h2 <- str_replace(h2, "Resto del País", "rest")

h2
h2_sectors <-
  view(mip_br_sin)
