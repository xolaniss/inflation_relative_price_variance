# Description
# rolling window regression with controls - Xolani Sibande October 2025
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
roll_modelling <- 
  function(data, window, dep_var){
    roll_spec <-   slidify(.f =  ~lm(..1 ~ ..2 + ..3 + ..4 + ..5 + ..6 + ..7 ), 
                           .period = window,
                           .align = "right",
                           .unlist = FALSE,
                           .partial = TRUE
    )
    
    data |> 
      mutate(roll = roll_spec({{dep_var}}, lagged_headline_inflation, rpd_lag1, rpd_lag2, rpd_lag3, zar, oil)) |> 
      mutate(tidy = map(roll, broom::tidy)) |> 
      unnest(cols = tidy) |> 
      dplyr::select(Date, term:estimate, statistic, p.value) |> 
      drop_na() |> 
      pivot_wider(names_from = term, values_from = c(estimate, statistic, p.value)) |> 
      dplyr::select(Date, estimate_..2, statistic_..2) |>
      pivot_longer(-Date, names_to = "Series", values_to = "Value") |>
      mutate(Series = dplyr::recode(
        Series,
        "estimate_..2" = "Estimate",
        "statistic_..2" = "t-statistic"))
  }

# Import -------------------------------------------------------------
rpd_data_tbl <- read_rds(here("Outputs", "artifacts_rpd_measures.rds")) |> 
  pluck(1)
controls_tbl <- read_excel(here("Data", "controls.xlsx")) |> 
  rename("Date" = month)


# Rolling window ---------------------------------------------------------------
rpd_data_modelling_tbl <- 
  rpd_data_tbl |> 
  mutate(
    rpd_lag1 = lag(rpd, 1),
    rpd_lag2 = lag(rpd, 2),
    rpd_lag3 = lag(rpd, 3),
    lagged_headline_inflation = lag(headline_inflation, 1)) |> 
  dplyr::select(-headline_inflation) |> 
  inner_join(controls_tbl, by = "Date") |> 
  drop_na() 

## 2-year window --------------- 
roll_rpd_models_2_year_tbl <- 
  rpd_data_modelling_tbl |> 
  roll_modelling(dep_var = rpd, window = 24) |> 
  filter(Date >= ymd("2011-05-31")) 

roll_rpd_2_year_gg <-
  fx_plot(roll_rpd_models_2_year_tbl, variables_color = 2) +
  facet_wrap (. ~ Series, scale = "free", labeller = label_parsed) +
  labs(subtitle = "2-year window")

# roll_rpd_division_models_2_year_tbl <- 
#   rpd_data_modelling_tbl |> 
#   roll_modelling(dep_var = rpd_division, window = 24) |> 
#   filter(Date >= ymd("2011-05-31"))
# 
# roll_rpd_division_2_year_gg <- 
#   fx_plot(roll_rpd_division_models_2_year_tbl, variables_color = 2) +
#   facet_wrap (. ~ Series, scale = "free", labeller = label_parsed) +
#   labs(subtitle = "2-year window")


## 5_year window ---------------
roll_rpd_models_5_year_tbl <- 
  rpd_data_modelling_tbl |> 
  roll_modelling(dep_var = rpd, window = 60) |> 
  filter(Date >= ymd("2014-05-31"))

roll_rpd_5_year_gg <- fx_plot(roll_rpd_models_5_year_tbl, variables_color = 2) +
  facet_wrap (. ~ Series, scale = "free", labeller = label_parsed) +
  labs(subtitle = "5-year window") 

# roll_rpd_division_models_5_year_tbl <- 
#   rpd_data_modelling_tbl |> 
#   roll_modelling(dep_var = rpd_division, window = 60) |> 
#   filter(Date >= ymd("2014-05-31"))
# 
# roll_rpd_division_5_year_gg <- fx_plot(roll_rpd_division_models_5_year_tbl, variables_color = 2) +
#   facet_wrap (. ~ Series, scale = "free", labeller = label_parsed) +
#   labs(subtitle = "5-year window") 

## Combined plots ----------------
rpd_combined_gg <- 
  roll_rpd_2_year_gg /
  roll_rpd_5_year_gg 

# rpd_division_combined_gg <- 
#   roll_rpd_division_2_year_gg /
#   roll_rpd_division_5_year_gg 

# Export ---------------------------------------------------------------
artifacts_roll_controls <- list (
  rpd_combined_gg = rpd_combined_gg
)

write_rds(artifacts_roll_controls, file = here("Outputs", "artifacts_roll_controls.rds"))


