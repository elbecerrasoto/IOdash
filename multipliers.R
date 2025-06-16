#!/usr/bin/Rscript
library(leontief)
library(tidyverse)
library(glue)

# ---- globals
# ---- helpers
# ---- code

A <- read_tsv("A.tsv")
L <- read_tsv("L.tsv")

mult_01 <- colSums(L)
multipliers_vect <- sort(mult_01, decreasing = TRUE)

multipliers <- tibble(sector = names(multipliers_vect), value = multipliers_vect)

write_tsv(multipliers, "multipliers.tsv")
view(multipliers)
