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
library(forecast)
library(estimatr)

# Parallel processing
library(furrr)
library(parallel)
library(tictoc)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "stars.R"))
options(scipen = 999)

# Import -------------------------------------------------------------
rpd_data_tbl <- read_rds(here("Outputs", "artifacts_rpd_measures.rds")) |> 
  pluck(1) |> 
  mutate(Date = as.Date(Date)) |>
  mutate(Date = lubridate::floor_date(Date, "month")) |> 
  mutate(
    dum_2011m12 = ifelse(Date == "2011-12-01", 1, 0),
    dum_2011m12 = ifelse(Date == "2011-12-01", 1, 0),
    dum_2014m7 = ifelse(Date == "2014-07-01", 1, 0),
    dum_2017m7 = ifelse(Date == "2017-07-01", 1, 0),
    dum_2020m2 = ifelse(Date == "2020-02-01", 1, 0),
    dum_2022m8 = ifelse(Date == "2022-08-01", 1, 0),
    dum_policy_2017 = ifelse(Date >= "2017-06-01", 1, 0)
  ) |>
  mutate(
    across(starts_with("dum_"), ~as_factor(.x))
  ) |>
  mutate(
    squared_headline_inflation = headline_inflation^2,
    log_rpd = log(rpd),
    headline_inflation = abs(headline_inflation)
  )

# Picking significant lags using var -----------------------------------
varselect_rpd_tbl <- VARselect(rpd_data_tbl |> dplyr::select(log_rpd, headline_inflation), lag.max = 10)$selection |>
  as_tibble() |> 
  mutate(Tests = c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")) |> 
  relocate(Tests, .before = value) |> 
  rename("Optimal Lags" = "value") |>
  mutate(across(2, ~ as.numeric(.x))) 

# Base regression ---------------------------------------------------------
## Full sample -----------
formula <- as.formula(
  "log_rpd ~ headline_inflation + lag(log_rpd, 1) + lag(log_rpd, 2) + lag(log_rpd, 3) +
  dum_2011m12 + dum_2014m7 + dum_2017m7 + dum_2020m2 + dum_2022m8"
)

rpd_full_sample <- lm(formula, 
                         data = rpd_data_tbl) 

robust_rpd_full_sample <- 
  lmtest::coeftest(rpd_full_sample) |>
  tidy() |>
  stars()

## Pre 2017 sample -----------
formula_pre <- as.formula(
  "log_rpd ~ headline_inflation + lag(log_rpd, 1) + lag(log_rpd, 2) + lag(log_rpd, 3) +
  dum_2011m12 + dum_2014m7"
)

rpd_pre_2017 <- lm(formula_pre, 
                      data = rpd_data_tbl |>  filter(dum_policy_2017 == 0))

robust_rpd_pre_2017 <- 
  lmtest::coeftest(rpd_pre_2017) |>
  tidy() |>
  stars()

## Post 2017 sample -----------
formula_post <- as.formula(
  "log_rpd ~ headline_inflation + lag(log_rpd, 1) + lag(log_rpd, 2) + lag(log_rpd, 3) +
   dum_2020m2 + dum_2022m8"
)

rpd_post_2017 <- lm(formula_post, 
                       data = rpd_data_tbl |> filter(dum_policy_2017 == 1))
robust_rpd_post_2017 <- 
  lmtest::coeftest(rpd_post_2017) |>
  tidy() |>
  stars()

# Regression with squared inflation ---------------------------------------
formula_sq <- as.formula(
  "log_rpd ~ headline_inflation + squared_headline_inflation + lag(log_rpd, 1) + lag(log_rpd, 2) + lag(log_rpd, 3) +
  dum_2011m12 + dum_2014m7 + dum_2017m7 + dum_2020m2 + dum_2022m8"
)

## Full sample ---------
rpd_full_sample_sq <- lm(formula_sq, 
                            data = rpd_data_tbl) 
robust_rpd_full_sample_sq <-
  lmtest::coeftest(rpd_full_sample_sq) |>
  tidy() |>
  stars()

## Pre 2017 -------------
formula_sq_pre <- as.formula(
  "log_rpd ~ headline_inflation + squared_headline_inflation +  lag(log_rpd, 1) + lag(log_rpd, 2) + lag(log_rpd, 3) +
  dum_2011m12 + dum_2014m7"
)

rpd_pre_2017_sq <- lm(formula_sq_pre, 
                         data = rpd_data_tbl |> filter(dum_policy_2017 == 0)) 

robust_rpd_pre_2017_sq <-
  lmtest::coeftest(rpd_pre_2017_sq) |> 
  tidy() |>
  stars()

## Post 2017 ---------------
formula_sq_post <- as.formula(
  "log_rpd ~ headline_inflation + squared_headline_inflation + lag(log_rpd, 1) + lag(log_rpd, 2) + lag(log_rpd, 3) +
   dum_2020m2 + dum_2022m8"
)

post_2017_sq <- lm(formula_sq_post, 
                          data = rpd_data_tbl |> filter(dum_policy_2017 == 1))
robust_rpd_post_2017_sq <-
  lmtest::coeftest(post_2017_sq) |> 
  tidy() |>
  stars()


# Combined models ----------------------------------
combined_models_tbl <- 
  bind_rows(
    .id = "model_type",
    robust_rpd_full_sample ,
    robust_rpd_pre_2017,
    robust_rpd_post_2017,
    robust_rpd_full_sample_sq,
    robust_rpd_pre_2017_sq,
    robust_rpd_post_2017_sq 
  ) |> 
  mutate(
    model_type = case_when(
      model_type ==  1 ~  "Full Sample",
      model_type ==  2 ~  "Pre 2017",
      model_type == 3 ~  "Post 2017",
      model_type == 4 ~ "Full Sample: Squared Inflation",
      model_type == 5 ~ "Pre 2017: Squared Inflation",
      model_type == 6 ~ "Post 2017: Squared Inflation"
    )
  ) |> 
  dplyr::select(-std.error, -statistic, -p.value) |> 
  mutate(across(where(is.numeric), ~ round(.x, 3))) |> 
  mutate(estimate = paste0(estimate, stars)) |> 
  dplyr::select(-stars) |> 
  pivot_wider(names_from = model_type, values_from = estimate)

# Export ---------------------------------------------------------------
artifacts_base_regression <- list (
  varselect_rpd_tbl = varselect_rpd_tbl,
  combined_models_tbl = combined_models_tbl
)

write_rds(artifacts_base_regression, file = here("Outputs", "artifacts_base_regression.rds"))


