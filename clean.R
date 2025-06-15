#!/usr/bin/Rscript
library(tidyverse)
library(readxl)
library(glue)

ARGV <- commandArgs(trailingOnly = TRUE)
IN <- ARGV[[1]]
OUT <- ARGV[[2]]

# IN <- "mip_ixi_br_sin_d_2018.xlsx"
# OUT <- "mip_br.tsv"

if (!file.exists(IN)) {
  stop(glue("The file specified by {IN} does not exist."))
}

mip_br <- read_xlsx(IN, col_names = FALSE)

# Remove the last 4 rows
# Metadata (2 rows), Total (1 row) and Empty (1 row)
mip_br <- mip_br |> slice(-(n() - 3):-n())

# Remove the last 2 columns
mip_br <- mip_br[-(ncol(mip_br) - 1):-ncol(mip_br)]

# Extract row and col names
# row keys and col keys
ck5 <- mip_br |>
  slice(5) |>
  as.character()

ck6 <- mip_br |>
  slice(6) |>
  as.character()

ck5[is.na(ck5)] <- ""
ck6[is.na(ck6)] <- ""

rk1 <- mip_br[[1]]
rk2 <- mip_br[[2]]

rk1[is.na(rk1)] <- ""
rk2[is.na(rk2)] <- ""

row_keys <- str_c(rk1, " ::: ", rk2)
col_keys <- str_c(ck5, " ::: ", ck6)

# Remove the first 6 rows
mip_br <- mip_br |> slice(-1:-6)
row_keys <- row_keys[-1:-6]

# Drop first two cols
mip_br <- mip_br[-1:-2]
col_keys <- col_keys[-1:-2]

# Make everything numeric
mip_br <- mip_br |>
  mutate(across(everything(), as.double))


names(mip_br) <- col_keys
mip_br$row_keys <- row_keys

# Input 0 on any NA cell
# and then write into a tsv
mip_br |>
  mutate(across(everything(), ~ replace_na(.x, 0))) |>
  write_tsv(OUT)
