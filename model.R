#!/usr/bin/Rscript
library(leontief) # Use it only to check correctness, and chaining
library(tidyverse)
library(glue)

# ---- globals

TOLERANCE <- 1e-2
N_SECTORS <- 35 * 2
Z_AUG <- "data/mip_ixi_br_cdmx_d_2018.tsv"

EXTRA_ROWS <- c(
  "importaciones_internacionales_c_i_f",
  "impuestos_netos_de_subsidios_sobre_los_productos",
  "valor_agregado_bruto"
)

# ---- helpers

tib2mat <- function(tib, drop_names = FALSE) {
  mat <- tib |>
    select(where(is.numeric)) |>
    as.matrix()
  if (drop_names) {
    colnames(mat) <- NULL
    rownames(mat) <- NULL
  }
  mat
}

normalize_sector <- function(sector_inputs, sector, x) {
  AVOID_UNDEF <- 1
  xj <- x[sector]

  if (xj == 0) {
    return(sector_inputs / AVOID_UNDEF)
  } else {
    return(sector_inputs / xj)
  }
}

get_A <- function(Z, x) {
  Z |>
    imap(normalize_sector, x = x) |>
    as_tibble()
}

get_L <- function(A) {
  is_square <- (nrow(A) == ncol(A))
  if (!is_square) {
    stop("Input requirement matrix must be square.")
  }
  I <- diag(ncol(A))
  solve(I - A) |>
    as_tibble() |>
    set_names(names(A))
}

# ---- get Z, A, L, f

Z_aug <- read_tsv(Z_AUG) |> select(where(is.numeric))
Z <- Z_aug[1:N_SECTORS, 1:N_SECTORS]

x_row <- rowSums(Z_aug[1:N_SECTORS, ])
x_col <- colSums(Z_aug[, 1:N_SECTORS])

are_xs_equal <- all(near(x_row, x_col, TOLERANCE))
stopifnot("Row and Col totals do NOT match." = are_xs_equal)

x <- x_row
names(x) <- names(Z)

f <- Z_aug[1:N_SECTORS, -1:-N_SECTORS] |>
  rowSums()
names(f) <- names(Z)

A <- get_A(Z, x)
L <- get_L(A)

print(L)

# ---- get multipliers

# ---- get model

# ---- chaining
