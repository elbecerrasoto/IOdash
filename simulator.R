#!/usr/bin/Rscript
library(leontief)
library(tidyverse)
library(glue)

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

get_ZALfx_multipliers <- function(Z_aug, n_sectors) {
  # ---- globals

  TOLERANCE <- 1e-2
  N_SECTORS <- n_sectors
  OUT_NAMES <- c("Z", "A", "L", "f", "x", "multipliers")
  OUT <- vector(mode = "list", length = length(OUT_NAMES)) |> set_names(OUT_NAMES)

  # ---- helpers


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

  Z_aug <- Z_aug |> select(where(is.numeric))
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

  link_class <- factor(link_class, levels = c(
    "independent",
    "demand_dependent",
    "supply_dependent",
    "dependent"
  ))

  multipliers <- multipliers |>
    mutate(
      link_backward = BL,
      link_forward = FL,
      link_class = link_class
    ) |>
    select(multiplier, link_class, sector, code, everything())

  OUT[["Z"]] <- Z
  OUT[["A"]] <- A
  OUT[["L"]] <- L
  OUT[["f"]] <- f
  OUT[["x"]] <- x
  OUT[["multipliers"]] <- multipliers

  OUT
}

simulate_demand_shocks <-
  function(shocks, L, f_old, x_old,
           shocks_are_multipliers = FALSE,
           shocks_are_total_demand = FALSE) {
    Lm <- L |> tib2mat()

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

    tibble(f_new, x_new, delta_rel, delta, f_old, x_old, sector = names(L))
  }
