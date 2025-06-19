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

STATE_CODES <- c(
  aguascalientes = "ags",
  baja_california = "bc",
  baja_california_sur = "bcs",
  campeche = "camp",
  coahuila = "coah",
  colima = "col",
  chiapas = "chis",
  chihuahua = "chih",
  ciudad_mexico = "cdmx",
  durango = "dgo",
  guanajuato = "gto",
  guerrero = "gro",
  hidalgo = "hgo",
  jalisco = "jal",
  estado_mexico = "mex",
  morelos = "mor",
  michoacan = "mich",
  nayarit = "nay",
  nuevo_leon = "nl",
  oaxaca = "oax",
  puebla = "pue",
  queretaro = "qro",
  quintana_roo = "qr",
  san_luis_potosi = "slp",
  sinaloa = "sin",
  sonora = "son",
  tabasco = "tab",
  tamaulipas = "tamps",
  tlaxcala = "tlax",
  veracruz = "ver",
  yucatan = "yuc",
  zacatecas = "zac"
)

TSVs <- str_c("data/mip_ixi_br_", STATE_CODES, "_d_2018.tsv")
state_mipbr <- tibble(
  state = names(STATE_CODES),
  code = STATE_CODES,
  path = TSVs
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

x <- x_row |> set_names(names(Z))

f <- Z_aug[1:N_SECTORS, -1:-N_SECTORS] |>
  rowSums() |>
  set_names(names(Z))

A <- get_A(Z, x)
L <- get_L(A)

# ---- get multipliers

multipliers_vec <- colSums(L)
are_not_less_than_1 <- all(multipliers_vec >= 1)
stopifnot("Multipliers are less than 1." = are_not_less_than_1)

multipliers <- tibble(
  multiplier = multipliers_vec,
  sector_raw = names(multipliers_vec)
)

naics <- multipliers$sector_raw |>
  str_extract_all("\\d+") |>
  map_chr(str_flatten, collapse = "-")

sector <- multipliers$sector_raw |>
  str_extract("\\d+.*?$") |>
  str_remove_all("\\d+_")

region <- multipliers$sector_raw |>
  str_remove("_\\d+.*$")

multipliers <- multipliers |>
  mutate(
    region = region,
    code = naics,
    sector = sector
  ) |>
  select(-sector_raw) |>
  arrange(desc(multiplier))

# ---- get gosh

population <- read_tsv("data/population_2020.tsv") |>
  left_join(state_mipbr, join_by(state))

# ---- get employment
