#!/usr/bin/Rscript
library(leontief)
library(tidyverse)
library(glue)

# Script to calculate Z, A, L

# ---- globals

N_SECTORS <- 70
TOLERANCE <- 0.001

# ARGV <- commandArgs(trailingOnly = TRUE)
# MIP_BR <- ARGV[[1]]

MIP_BR <- "mip_br.tsv"

OUT_Z <- "Z.tsv"
OUT_A <- "A.tsv"
OUT_L <- "L.tsv"

if (!file.exists(MIP_BR)) {
  stop(glue("The file specified by {MIP_BR} does not exist."))
}

# ---- helpers

remove_colnames <- function(tib) {
  names(tib) <- str_c("V", 1:ncol(tib))
  tib
}

# ---- code

mip_br <- read_tsv(MIP_BR)
row_keys <- mip_br$row_keys
mip_br <- mip_br |> select(-row_keys)

# First approach less general
# using the numbers on columns
Z <- mip_br[1:N_SECTORS, 1:N_SECTORS]

xjs_byrow <- rowSums(mip_br)[1:N_SECTORS]
xjs_bycol <- colSums(mip_br)[1:N_SECTORS]

is_accounting_good <- all(near(xjs_byrow, xjs_bycol, TOLERANCE))
# stopifnot("Row and Col totals do NOT match." = is_accounting_good)

xjs <- xjs_byrow
names(xjs) <- names(Z)

normalize_sector <- function(sector_inputs, sector) {
  AVOID_UNDEF <- 1

  xj <- xjs[sector]

  if (xj == 0) {
    return(sector_inputs / AVOID_UNDEF)
  } else {
    return(sector_inputs / xj)
  }
}

A <- Z |>
  imap(normalize_sector) |>
  as_tibble()

L <- A |>
  as.matrix() |>
  leontief_inverse() |>
  as_tibble()
names(L) <- names(A)

write_tsv(Z, OUT_Z)
write_tsv(A, OUT_A)
write_tsv(L, OUT_L)
