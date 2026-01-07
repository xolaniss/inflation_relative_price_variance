# Description
# Base regression core - Xolani Sibande 2025
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
sheets_names <- excel_sheets(here("Data", "Price_dispersion_data_core.xlsx"))

# Read the data sheet (assuming it's the 3rd sheet as in the original script, or inspecting names)
# Original script: data <- read_excel(..., sheet = sheets_names[3], skip = 0)
# We will start by importing the raw data similar to 01_price_data.R

data_core_tbl <-
       read_excel(here("Data", "Price_dispersion_data_core.xlsx"), sheet = sheets_names[7], skip = 0) |> 
       dplyr::select(month, corerpv, corecpi) |> 
       rename(
              Date = month,
              rvp_core = corerpv,
              core_inflation = corecpi
       ) |> 
       mutate(Date = lubridate::floor_date(Date, "month")) |> 
       mutate(Date = as.Date(Date)) |> 
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
              across(starts_with("dum_"), ~ as_factor(.x))
       ) |>
       mutate(
              squared_core_inflation = core_inflation^2,
              log_rvp_core = log(rvp_core),
       )

# Graphing ----------------------------------------------------------------
rvp_core_gg <-
       data_core_tbl |>
       ggplot(aes(x = Date, y = rvp_core)) +
       geom_line(color = pnw_palette("Winter", 1)) +
       theme_minimal() +
       theme(
              legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()
       ) +
       theme(
              text = element_text(size = 8),
              strip.background = element_rect(colour = "white", fill = "white"),
              axis.text.x = element_text(angle = 90),
              axis.title = element_text(size = 8),
              plot.tag = element_text(size = 8),
              axis.line = element_line(color = "black", linewidth = 0.2),
              strip.text = element_blank()
       ) +
       labs(x = "", y = "Core Inflation relative price variability")

# Picking significant lags using var -----------------------------------
varselect_rvp_core_tbl <- VARselect(data_core_tbl |> dplyr::select(log_rvp_core, core_inflation), lag.max = 10)$selection |>
       as_tibble() |>
       mutate(Tests = c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")) |>
       relocate(Tests, .before = value) |>
       rename("Optimal Lags" = "value") |>
       mutate(across(2, ~ as.numeric(.x)))

# Structural breaks -----------------------------------------------------
breakpoints_core_rvp <- breakpoints(rvp_core ~ 1, data = data_core_tbl)
summary(breakpoints_core_rvp)
plot(breakpoints_core_rvp)

breakpoints_core_inflation <- breakpoints(core_inflation ~ 1, data = data_core_tbl)
summary(breakpoints_core_inflation)
plot(breakpoints_core_inflation)

# Stationarity ------------------------------------------------------------
core_rvp_stationarity_tbl <- 
  data_core_tbl|> 
  dplyr::select(Date, rvp_core, core_inflation) |>
  pivot_longer(cols = -Date, names_to = "Measure", values_to = "Value") |> 
  group_by(Measure) |>
  summarise(
    adf_test = tseries::adf.test(Value)$p.value,
    pp_test = pp.test(Value)$p.value,
    kp_test = kpss.test(Value)$p.value
  ) |> 
  mutate(
    Stationarity = ifelse(adf_test < 0.1 | pp_test < 0.1 | kp_test < 0.1, "Stationary", "Non-Stationary")
  ) |>
  filter(Measure %in% c("rvp_core", "core_inflation")) |> 
  rename(
    "ADF test (p-value)" = adf_test,
    "PP test (p-value)" = pp_test,
    "KPSS test (p-value)" = kp_test
  )


# Base regression ---------------------------------------------------------
## Full sample -----------

formula <- as.formula(
       "log_rvp_core ~ core_inflation + lag(log_rvp_core, 1)  + dum_2011m12 + dum_2014m7 + dum_2017m7 + dum_2020m2 + dum_2022m8"
)

rvp_full_sample <- 
  lm(formula, data = data_core_tbl
)

robust_rvp_full_sample <-
       lmtest::coeftest(rvp_full_sample, vcov = vcovHC(rvp_full_sample, "HC1")) |>
       tidy() |>
       stars()

## Pre 2017 sample -----------
formula_pre <- as.formula(
       "log_rvp_core ~ core_inflation + lag(log_rvp_core, 1)  + dum_2011m12 + dum_2014m7"
)

rvp_pre_2017 <- lm(formula_pre,
       data = data_core_tbl |> filter(dum_policy_2017 == 0)
)

robust_rvp_pre_2017 <-
       lmtest::coeftest(rvp_pre_2017) |>
       tidy() |>
       stars()

## Post 2017 sample -----------
formula_post <- as.formula(
       "log_rvp_core ~ core_inflation + lag(log_rvp_core, 1)  + dum_2020m2 + dum_2022m8"
)

rvp_post_2017 <- lm(formula_post,
       data = data_core_tbl |> filter(dum_policy_2017 == 1)
)
robust_rvp_post_2017 <-
       lmtest::coeftest(rvp_post_2017) |>
       tidy() |>
       stars()

# Regression with squared inflation ---------------------------------------
formula_sq <- as.formula(
       "log_rvp_core ~ core_inflation + squared_core_inflation + lag(log_rvp_core, 1)  + 
       dum_2011m12 + dum_2014m7 + dum_2017m7 + dum_2020m2 + dum_2022m8"
)

## Full sample ---------
rvp_full_sample_sq <- lm(formula_sq,
       data = data_core_tbl
)

robust_rvp_full_sample_sq <-
       lmtest::coeftest(rvp_full_sample_sq) |>
       tidy() |>
       stars()

## Pre 2017 -------------
formula_sq_pre <- as.formula(
       "log_rvp_core ~ core_inflation + squared_core_inflation + lag(log_rvp_core, 1)  +
  dum_2011m12 + dum_2014m7"
)

rvp_pre_2017_sq <- lm(formula_sq_pre,
       data = data_core_tbl |> filter(dum_policy_2017 == 0)
)

robust_rvp_pre_2017_sq <-
       lmtest::coeftest(rvp_pre_2017_sq) |>
       tidy() |>
       stars()

## Post 2017 ---------------
formula_sq_post <- as.formula(
       "log_rvp_core ~ core_inflation + squared_core_inflation + lag(log_rvp_core, 1)  +
   dum_2020m2 + dum_2022m8"
)

post_2017_sq <- lm(formula_sq_post,
       data = data_core_tbl |> filter(dum_policy_2017 == 1)
)
robust_rvp_post_2017_sq <-
       lmtest::coeftest(post_2017_sq) |>
       tidy() |>
       stars()


# Combined models ----------------------------------
combined_models_core_tbl <-
       bind_rows(
              .id = "model_type",
              robust_rvp_full_sample,
              robust_rvp_pre_2017,
              robust_rvp_post_2017,
              robust_rvp_full_sample_sq,
              robust_rvp_pre_2017_sq,
              robust_rvp_post_2017_sq
       ) |>
       mutate(
              model_type = case_when(
                     model_type == 1 ~ "Core RVP Full Sample",
                     model_type == 2 ~ "Core RVP Pre 2017",
                     model_type == 3 ~ "Core RVP Post 2017",
                     model_type == 4 ~ "Core RVP Full Sample with Squared Inflation",
                     model_type == 5 ~ "Core RVP Pre 2017 with Squared Inflation",
                     model_type == 6 ~ "Core RVP Post 2017 with Squared Inflation"
              )
       ) |>
       dplyr::select(-std.error, -statistic, -p.value) |>
       mutate(across(where(is.numeric), ~ round(.x, 3))) |>
       mutate(estimate = paste0(estimate, stars)) |>
       dplyr::select(-stars)


# Export ---------------------------------------------------------------
artifacts_base_regression_core <- list(
       data_core_tbl = data_core_tbl,
       rvp_core_gg = rvp_core_gg,
       varselect_rvp_core_tbl = varselect_rvp_core_tbl,
       core_rvp_stationarity_tbl = core_rvp_stationarity_tbl,
       combined_models_core_tbl = combined_models_core_tbl
)

write_rds(artifacts_base_regression_core, file = here("Outputs", "artifacts_base_regression_core.rds"))
