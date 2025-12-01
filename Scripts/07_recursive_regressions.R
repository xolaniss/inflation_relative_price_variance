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

library(slider)
library(timetk)


# Import -------------------------------------------------------------
rvp_data_tbl <- read_rds(here("Outputs", "artifacts_rvp_measures.rds")) |> 
  pluck(1)

# Data ---------------------------------------------------------------
rvp_data_modelling_tbl <- 
  rvp_data_tbl |> 
  mutate(
    rvp_lag1 = lag(rvp, 1),
    rvp_lag2 = lag(rvp, 2),
    rvp_lag3 = lag(rvp, 3)
    ) |> 
  drop_na() 

# Recursive regression function -------------------------

## Recursive regression ------------------------------
results <- tibble()
data <- rvp_data_modelling_tbl
data[40, ]

for(i in 40:nrow(data)) {
  train_data <- data[1:i, ]
  
  model <- lm(log(rvp) ~ headline_inflation + log(rvp_lag1) + log(rvp_lag2) + log(rvp_lag3), data = train_data)
  
  results <- bind_rows(
    results,
    tibble(
      date = data$Date[i],
      headline_inflation = coef(model)["headline_inflation"],
      rvp_lag1 = coef(model)["rvp_lag1"],
      rvp_lag2 = coef(model)["rvp_lag2"],
      rvp_lag3 = coef(model)["rvp_lag3"],
      intercept = coef(model)["(Intercept)"]
    )
  )
}

results_gg <- 
  results |> 
  pivot_longer(-date, names_to = "Series", values_to = "Value") |> 
  rename(Date = date) |> 
  filter(Series == "headline_inflation") |> 
  fx_plot(variables_color = 1) +
  labs(title = "",
       x = " ",
       y = "Estimate",
       caption = "Recursive regression estimates of the coefficient on absolute headline inflation with a fixed start date of 2012-07."
       ) +
  theme(strip.text = element_blank())

# Export ---------------------------------------------------------------
artifacts_recursive <- list (
  results_gg = results_gg
)

write_rds(artifacts_recursive, file = here("Outputs", "artifacts_recursive.rds"))


