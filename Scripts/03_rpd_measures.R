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
sheets_names <- excel_sheets(here("Data", "Price_dispersion_data_20260123.xlsx"))

rpd_tbl <-
  read_excel(here("Data", "Price_dispersion_data_20260123.xlsx"), sheet = sheets_names[14], skip = 0) |> 
  rename(Date = month,
         headline_inflation = hcpi,
         core_inflation = corecpi) |> 
  dplyr::select(Date, headline_inflation, core_inflation, rpd, rpd_cov) 
  # mutate(
  #   across(-Date, ~ .x / 100)
  # )

# price_data_tbl |> glimpse()
# weights_tbl <- read_rds(here("Outputs", "artifacts_price_data.rds")) |> 
#   pluck(1, 2) |> 
#   mutate(
#     Weight = Weight /100
#   )
#  
# # RVP  ------------------------------------------------------------
# rvp_tbl <- 
#   price_data_tbl |> 
#   mutate(across(
#     -Date, ~ (.x - headline_inflation)^2
#   )) |> 
#   dplyr::select(-headline_inflation) |> 
#   tidyr::pivot_longer(-c(Date), names_to = "Series", values_to = "Value") |> 
#   left_join(weights_tbl, by = c("Series")) |> 
#   group_by(Date) 
#   mutate(
#     weight_diff = Value * Weight
#   ) |> 
#   reframe(
#     rvp = sqrt(sum(weight_diff, na.rm = TRUE))) |> 
#   left_join(
#       price_data_tbl |> 
#         dplyr::select(Date, headline_inflation),
#       by = "Date"
#     ) |> 
#   mutate(
#     rvp_division = (rvp/abs(1+headline_inflation)) *100,
#     headline_inflation = headline_inflation * 100,
#     rvp = rvp * 100
#   ) |> 
#   relocate(
#     headline_inflation,
#     .after = rvp_division
#   ) |> 
#   mutate(log_rvp = log(rvp),
#          log_rvp_division = log(rvp_division),
#          log_headline_inflation = log(headline_inflation),
#          policy_2017_dum = ifelse(Date > as.Date("2017-07-01"), 1, 0)
#   ) 
# 
# rvp_tbl <- price_data_tbl |> 
#   dplyr::select(Date, headline_inflation, rvp)
# Graphing ----------------------------------------------------------------
rpd_gg <- 
  rpd_tbl |>
  ggplot(aes(x = Date, y = rpd)) +
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
    axis.line = element_line(color = "black", linewidth = 0.2),
    strip.text = element_blank()
  ) +
  labs(x = "", y = "Relative price dispersion") +
  scale_fill_manual(values = pnw_palette("Winter", 1))


rpd_cov_gg <- 
  rpd_tbl |>
  ggplot(aes(x = Date, y = rpd_cov)) +
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
    axis.line = element_line(color = "black", linewidth = 0.2),
    strip.text = element_blank()
  ) +
  labs(x = "", y = "Relative price dispersion (CoV)") +
  scale_fill_manual(values = pnw_palette("Winter", 1))





# Export ---------------------------------------------------------------
artifacts_rpd_measures <- list (
  rpd_tbl = rpd_tbl,
  rpd_gg = rpd_gg,
  rpd_cov_gg = rpd_cov_gg
)


write_excel_csv(rpd_tbl, file = here("Outputs", "rdp_measures.csv"))
write_rds(artifacts_rpd_measures, file = here("Outputs", "artifacts_rpd_measures.rds"))


