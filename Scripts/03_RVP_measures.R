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
 
# RVP  ------------------------------------------------------------
rvp_tbl <- 
  price_data_tbl |> 
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
  reframe(
    rvp = sqrt(sum(weight_diff, na.rm = TRUE))) |> 
  left_join(
      price_data_tbl |> 
        dplyr::select(Date, headline_inflation),
      by = "Date"
    ) |> 
  mutate(
    rvp_division = (rvp/abs(1+headline_inflation)) *100,
    headline_inflation = headline_inflation * 100
  ) |> 
  relocate(
    headline_inflation,
    .after = rvp_division
  )

rvp_base_version_gg <- 
  rvp_tbl |>
  ggplot(aes(x = Date, y = rvp_division)) +
  geom_line() +
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
    axis.line = element_line(color = "black", linewidth = 0.2)
  ) +
  labs(x = "", y = "Relative price variance") +
  scale_fill_manual(values = pnw_palette("Winter", 1))



# Export ---------------------------------------------------------------
artifacts_rvp_measures <- list (
  rvp_tbl = rvp_tbl,
  rvp_base_version_gg = rvp_base_version_gg
)


write_excel_csv(rvp_tbl, file = here("Outputs", "rvp_measures.csv"))
write_rds(artifacts_rvp_measures, file = here("Outputs", "artifacts_rvp_measures.rds"))


