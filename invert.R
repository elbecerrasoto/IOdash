#!/usr/bin/Rscript
library(tidyverse)
library(glue)

ARGV <- commandArgs(trailingOnly = TRUE)
# MIP_BR <- ARGV[[1]]

MIP_BR <- "mip_br.tsv"
Z <- read_tsv(MIP_BR)
names(Z)

if (!file.exists(MIP_BR)) {
  stop(glue("The file specified by {MIP_BR} does not exist."))
}

row_keys <- Z$row_keys

Z <- Z |> select(-row_keys)
