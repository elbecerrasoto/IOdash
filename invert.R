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

# First approach less general
# using the numbers of on columns

Z <- Z |> select(-row_keys)
your_tibble %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))

N_SECTORS <- 35
names(Z)

names(Z)
(Z)
# Calculate the xj's to calculate the aij
row_keys
