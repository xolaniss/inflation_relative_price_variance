# Description
# RVP calculations - Xolaini Sibande October 2025
# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(xts)
library(broom)
library(glue)
library(scales)
library(kableExtra)
library(pins)
library(timetk)
library(uniqtag)
library(quantmod)
library(qs2)

# graphs
library(PNWColors)
library(patchwork)

# eda
library(psych)
library(DataExplorer)
library(skimr)

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)

# Parallel processing
library(furrr)
library(parallel)
library(tictoc)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
price_data_tbl <- read_rds(here("Outputs", "artifacts_price_data.rds")) |> 
  pluck(1, 1) |> 
  dplyr::select(-rvp) |> 
  mutate(
    across(-Date, ~ .x / 100)
  )

weights_tbl <- read_rds(here("Outputs", "artifacts_price_data.rds")) |> 
  pluck(1, 2) |> 
  mutate(
    Weight = Weight /100
  )
 
# RVP no division ----------------------------------------------------------
rvp_no_division_tbl <- 
  price_data_tbl |> 
  mutate(across(
    -Date, ~ (.x - headline_inflation)^2
  )) |> 
  dplyr::select(-headline_inflation) |>
  tidyr::pivot_longer(-Date, names_to = "Series", values_to = "Value") |> 
  left_join(weights_tbl, by = c("Series")) |> 
  group_by(Date) |> 
  mutate(
    weight_diff = Value * Weight
  ) |> 
 summarise_by_time(.date_var = Date, .by = "month", rvp_no_division = sqrt(sum(weight_diff, na.rm = TRUE))) |> 
  ungroup() |> 
  mutate(
    rvp_no_division = rvp_no_division *100
  )

# RVP division ------------------------------------------------------------
rvp_base_version <- 
  price_data_tbl  |> 
  mutate(across(
    -Date, ~ (.x - headline_inflation)^2
  )) |>  
  dplyr::select(-headline_inflation) |>
  tidyr::pivot_longer(-c(Date), names_to = "Series", values_to = "Value") |> 
  left_join(weights_tbl, by = c("Series")) |> 
  group_by(Date) |> 
  mutate(
    weight_diff = Value * Weight
  ) |> 
  summarise_by_time(
    .date_var = Date,
    .by = "month",
    rvp_division = sqrt(sum(weight_diff, na.rm = TRUE)) / abs(1 - price_data_tbl$headline_inflation)
  ) |> 
  ungroup() |>
  mutate(
    rvp_division = rvp_division *100
  )



# Export ---------------------------------------------------------------
artifacts_rvp_measures <- list (
  rvp_no_division_tbl = rvp_no_division_tbl,
  rvp_base_version = rvp_base_version
)

combined_measures_tbl <- 
  rvp_no_division_tbl |> 
  left_join(rvp_base_version, by = "Date")

write_excel_csv(combined_measures_tbl, file = here("Outputs", "rvp_measures.csv"))
write_rds(artifacts_rvp_measures, file = here("Outputs", "artifacts_rvp_measures.rds"))


