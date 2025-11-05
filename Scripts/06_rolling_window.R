# Description
# rolling window regression - Xolani Sibande October 2025
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
    roll_spec <-   slidify(.f =  ~lm(..1 ~ ..2 + ..3 + ..4 + ..5 + ..6), 
                           .period = window,
                           .align = "right",
                           .unlist = FALSE,
                           .partial = TRUE
    )
    
    data |> 
      mutate(roll = roll_spec({{dep_var}}, abs_headline_inflation, rvp_lag1, rvp_lag2, rvp_lag3, rvp_lag4)) |> 
      mutate(tidy = map(roll, broom::tidy)) |> 
      unnest(cols = tidy) |> 
      dplyr::select(Date, term:estimate, statistic, p.value) |> 
      drop_na() |> 
      pivot_wider(names_from = term, values_from = c(estimate, statistic, p.value)) |> 
      dplyr::select(Date, estimate_..2, statistic_..2) |>
      pivot_longer(-Date, names_to = "Series", values_to = "Value") |>
      mutate(Series = dplyr::recode(
        Series,
        "estimate_..2" = "beta[1]",
        "statistic_..2" = "t-statistic"))
  }

# Import -------------------------------------------------------------
rvp_data_tbl <- read_rds(here("Outputs", "artifacts_rvp_measures.rds")) |> 
  pluck(1)


# Rolling window ---------------------------------------------------------------
rvp_data_modelling_tbl <- 
  rvp_data_tbl |> 
  mutate(
  rvp_lag1 = lag(rvp, 1),
  rvp_lag2 = lag(rvp, 2),
  rvp_lag3 = lag(rvp, 3),
  rvp_lag4 = lag(rvp, 4),
  abs_headline_inflation = abs(headline_inflation)) |> 
  dplyr::select(-headline_inflation) |> 
  drop_na() 

## 2-year window --------------- 
roll_rvp_models_2_year_tbl <- 
  rvp_data_modelling_tbl |> 
  roll_modelling(dep_var = rvp, window = 24) |> 
  filter(Date >= ymd("2011-05-31")) 

roll_rvp_2_year_gg <- 
  fx_plot(roll_rvp_models_2_year_tbl, variables_color = 2) +
  facet_wrap (. ~ Series, scale = "free", labeller = label_parsed) + 
  labs(subtitle = "2-year window")

roll_rvp_division_models_2_year_tbl <- 
  rvp_data_modelling_tbl |> 
  roll_modelling(dep_var = rvp_division, window = 24) |> 
  filter(Date >= ymd("2011-05-31"))

roll_rvp_division_2_year_gg <- 
  fx_plot(roll_rvp_division_models_2_year_tbl, variables_color = 2) +
  facet_wrap (. ~ Series, scale = "free", labeller = label_parsed) +
  labs(subtitle = "2-year window")

## 5_year window ---------------
roll_rvp_models_5_year_tbl <- 
  rvp_data_modelling_tbl |> 
  roll_modelling(dep_var = rvp, window = 60) |> 
  filter(Date >= ymd("2014-05-31"))

roll_rvp_5_year_gg <- fx_plot(roll_rvp_models_5_year_tbl, variables_color = 2) +
  facet_wrap (. ~ Series, scale = "free", labeller = label_parsed) +
  labs(subtitle = "5-year window") 

roll_rvp_division_models_5_year_tbl <- 
  rvp_data_modelling_tbl |> 
  roll_modelling(dep_var = rvp_division, window = 60) |> 
  filter(Date >= ymd("2014-05-31"))

roll_rvp_division_5_year_gg <- fx_plot(roll_rvp_division_models_5_year_tbl, variables_color = 2) +
  facet_wrap (. ~ Series, scale = "free", labeller = label_parsed) +
  labs(subtitle = "5-year window") 

## Combined plots ----------------
rvp_combined_gg <- 
  roll_rvp_2_year_gg /
  roll_rvp_5_year_gg 

rvp_division_combined_gg <- 
  roll_rvp_division_2_year_gg /
  roll_rvp_division_5_year_gg 
  
# Export ---------------------------------------------------------------
artifacts_roll <- list (
  data = list(
    roll_rvp_models_2_year_tbl = roll_rvp_models_2_year_tbl,
    roll_rvp_division_models_2_year_tbl = roll_rvp_division_models_2_year_tbl,
    roll_rvp_models_5_year_tbl = roll_rvp_models_5_year_tbl,
    roll_rvp_division_models_5_year_tbl = roll_rvp_division_models_5_year_tbl
  ),
  graphs = list(
      rvp_combined_gg = rvp_combined_gg,
      rvp_division_combined_gg = rvp_division_combined_gg
  )
)

write_rds(artifacts_roll, file = here("Outputs", "artifacts_roll.rds"))


