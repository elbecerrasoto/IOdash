#!/usr/bin/Rscript
library(tidyverse)
library(leontief)
library(janitor)
library(glue)

# ---- globals

TOLERANCE <- 1e-4

# ---- helpers

tib_as_matrix <- function(tib) {
  tib |>
    select(where(is.numeric)) |>
    as.matrix()
}

drop_names <- function(mat) {
  mat |>
    `colnames<-`(NULL) |>
    `rownames<-`(NULL)
}

# ---- code

A <- read_tsv("A.tsv")
L <- read_tsv("L.tsv")
L <- read_tsv("L.tsv")

multipliers_package <-
  L |>
  tib_as_matrix() |>
  output_multiplier() |>
  as_tibble()

multipliers_manual <- colSums(L)

are_mult_equal <- all(near(
  multipliers_manual,
  multipliers_package[1], TOLERANCE
))
stopifnot("Error in Multipliers." = are_mult_equal)

L |>
  clean_names() |>
  names()
help(janitor)

multipliers <- tibble(
  sector = names(multipliers_manual),
  region = "",
  output_multiplier = multipliers_manual
)
