#!/usr/bin/Rscript
library(tidyverse)
library(glue)

# Script to calculate Z, A, L

# ---- globals

N_SECTORS <- 35
TOLERANCE <- 0.0001

# ARGV <- commandArgs(trailingOnly = TRUE)
# MIP_BR <- ARGV[[1]]

MIP_BR <- "mip_br.tsv"

if (!file.exists(MIP_BR)) {
  stop(glue("The file specified by {MIP_BR} does not exist."))
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
stopifnot("Row and Col totals do NOT match." = is_accounting_good)
