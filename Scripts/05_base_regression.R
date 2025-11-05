# Description
# Base regression - Xolani Sibande 2025 October
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
source(here("Functions", "stars.R"))
options(scipen = 999)

# Import -------------------------------------------------------------
rvp_data <- read_rds(here("Outputs", "artifacts_rvp_measures.rds")) |> 
  pluck(1)

# Picking significant lags using var -----------------------------------
varselect_rvp_tbl <- VARselect(rvp_data |> dplyr::select(rvp, headline_inflation), lag.max = 10)$selection |>
  as_tibble() |>
  mutate(across(everything(), ~ as.numeric(.x))) |>
  summarise(mean = mean(value))

varselect_rvp_division_tbl <- VARselect(rvp_data |> dplyr::select(rvp_division, headline_inflation),
                                        lag.max = 10)$selection |>
  as_tibble() |>
  mutate(across(everything(), ~ as.numeric(.x))) |>
  summarise(mean = mean(value))

# Base regression ---------------------------------------------------------
rvp_ols <- lm(rvp ~ 
                abs(headline_inflation) + 
                lag(rvp, 1) + lag(rvp, 2) + 
                lag(rvp, 3) + lag(rvp, 4) ,
                data = rvp_data)
rvp_robust_ols <- 
  lmtest::coeftest(rvp_ols, vcov = NeweyWest(rvp_ols)) |>
  tidy() |>
  stars()

rvp_division_ols <- lm(
  rvp_division ~ 
    abs(headline_inflation) +
    lag(rvp_division, 1) +
    lag(rvp_division, 2) +
    lag(rvp_division, 3) +
    lag(rvp_division, 4),
    data = rvp_data
)
rvp_division_robust_ols <- 
  lmtest::coeftest(rvp_division_ols, vcov = NeweyWest(rvp_division_ols)) |>
  tidy() |> 
  stars()
  

# Export ---------------------------------------------------------------
artifacts_base_regression <- list (
  varselect_rvp_tbl = varselect_rvp_tbl,
  varselect_rvp_division_tbl = varselect_rvp_division_tbl,
  rvp_robust_ols = rvp_robust_ols,
  rvp_division_robust_ols = rvp_division_robust_ols
)

write_rds(artifacts_base_regression, file = here("Outputs", "artifacts_base_regression.rds"))


