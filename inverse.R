#!/usr/bin/Rscript

library(tidyverse)

INPUT <- "mip_br.tsv"
Z <- read_tsv(INPUT)

row_key <- Z$row_key
