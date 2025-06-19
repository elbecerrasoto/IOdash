#!/usr/bin/Rscript
library(leontief)
library(tidyverse)
library(glue)

# ---- globals

TOLERANCE <- 1e-2
N_SECTORS <- 35 * 2
Z_AUG <- "data/mip_ixi_br_sin_d_2018.tsv"

POPULATION <- "data/population_2020.tsv"

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
sector_names <- names(Z)
Zm <- tib2mat(Z, drop_names = TRUE)

x_row <- rowSums(Z_aug[1:N_SECTORS, ])
x_col <- colSums(Z_aug[, 1:N_SECTORS])

are_xs_equal <- all(near(x_row, x_col, TOLERANCE))
stopifnot("Row and Col totals do NOT match." = are_xs_equal)

x <- x_row |> set_names(names(Z))

# final demand
f <- Z_aug[1:N_SECTORS, -1:-N_SECTORS] |>
  rowSums() |>
  set_names(names(Z))

A <- get_A(Z, x)
Am <- tib2mat(A, drop_names = TRUE)

L <- get_L(A)
Lm <- tib2mat(L, drop_names = TRUE)

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

# output multipliers
multipliers <- multipliers |>
  mutate(
    region = region,
    code = naics,
    sector = sector
  ) |>
  select(-sector_raw)

assert_matches_leontief_library <-
  output_multiplier(Lm) |>
  as.numeric() |>
  near(multipliers$multiplier, TOLERANCE) |>
  all()
stopifnot("Multipliers, does NOT matches leontief library." = assert_matches_leontief_library)

# ---- expand multipliers to other summaries

BL <- leontief::forward_linkage(Am) |>
  as.numeric() |>
  set_names(sector_names)
FL <- leontief::backward_linkage(Am) |>
  as.numeric() |>
  set_names(sector_names)

link_class <- vector(mode = "character", length = length(BL))

link_class[BL < 1 & FL < 1] <- "independent"
link_class[BL < 1 & FL >= 1] <- "demand_dependent"
link_class[BL >= 1 & FL < 1] <- "supply_dependent"
link_class[BL >= 1 & FL >= 1] <- "dependent"

multipliers <- multipliers |>
  mutate(
    link_backward = BL,
    link_forward = FL,
    link_class = link_class
  ) |>
  select(multiplier, link_class, sector, code, everything())


# ---- production simulator

production <- tibble(f_2018 = f, x_2018 = x, sector = multipliers$sector)

simulate_demand_shocks <-
  function(shocks, x_old, f_old,
           shocks_are_multipliers = FALSE,
           shocks_are_total_demand = FALSE) {
    if (shocks_are_multipliers) {
      f_new <- shocks * f_old
    } else if (shocks_are_total_demand) {
      f_new <- shocks
    } else {
      f_new <- shocks + f_old
    }

    x_new <- Lm %*% f_new |> as.numeric()
    delta <- x_new - x_old
    delta_rel <-
      if_else(x_old != 0, (delta + x_old) / x_old, (delta + x_old) / 1)

    tibble(f_old, x_old, f_new, x_new, delta, delta_rel)
  }

simulate_demand_shocks(rep(1.01, length(f)), x, f, shocks_are_multipliers = TRUE)

# ---- get T

population <- read_tsv(POPULATION) |>
  left_join(state_mipbr, join_by(state))

selected_population <- population |>
  filter(path == Z_AUG) |>
  pull(n)

working_population <- 0.46 * selected_population
employment_struture <- rep(1 / N_SECTORS, N_SECTORS)

e <- working_population * employment_struture


# ---- employment simulator
