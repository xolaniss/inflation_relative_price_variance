# Description
# Quantile regressions - Xolani Sibande 2025
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
library(quantreg)
library(np)
library(KernSmooth)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "QQR_functions.R"))

# Import -------------------------------------------------------------
rpd_data_tbl <- read_rds(here("Outputs", "artifacts_rpd_measures.rds")) |> 
  pluck(1)

# quantile regressions ---------------------------------------------------------------
quants_regs_tbl <- rq(
  formula = log(rpd)  ~ headline_inflation + log(lag(rpd, 1)) + log(lag(rpd, 2)) + log(lag(rpd, 3)),
  data = rpd_data_tbl,
  tau = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  ) |> 
  tidy() |> 
  mutate(
    tau = case_when(
      tau == 0.10 ~ "10th Percentile",
      tau == 0.20 ~ "20th Percentile",
      tau == 0.30 ~ "30th Percentile",
      tau == 0.40 ~ "40th Percentile",
      tau == 0.50 ~ "50th Percentile",
      tau == 0.60 ~ "60th Percentile",
      tau == 0.70 ~ "70th Percentile",
      tau == 0.80 ~ "80th Percentile",
      tau == 0.90 ~ "90th Percentile"
    ))

# graphing ---------------------------------------------------------------
quantile_reg_plot <- quants_regs_tbl |>
  filter(term == "headline_inflation") |>
  ggplot(aes(y = tau, x = estimate, color = tau)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0.5, position = position_dodge(0.8)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) +
  theme(
    text = element_text(size = 10),
    strip.background = element_rect(colour = "white", fill = "white"),
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(size = 7),
    axis.line = element_line(color = 'black'),
    plot.tag = element_text(size = 8)) +
  labs(x = bquote(beta[1][t]), y = "", col = "") +
  scale_color_manual(values = pnw_palette("Winter", 9)) +
  coord_flip()

# QQR ---------------------------------------------------------------------
## Full sample QQR -----------------------------------------------------------
qqr_rpd_full <- QQR(x = rpd_data_tbl$headline_inflation,
               y = log(rpd_data_tbl$rpd),
               hm = "CV"
)
qqr_rpd_full_gg <- ggplot.QQR(qqr_rpd_full)

## Pre 2017 QQR -----------------------------------------------------------
rpd_data_pre_tbl <- rpd_data_tbl |> filter(Date < "2017-01-01")
qqr_rpd_pre_2017 <-QQR(x = rpd_data_pre_tbl$headline_inflation,
                       y = log(rpd_data_pre_tbl$rpd),
                       hm = "CV"
)
qqr_rpd_pre_2017_gg <- ggplot.QQR(qqr_rpd_pre_2017)

## Post 2017 QQR -----------------------------------------------------------
rpd_data_post_tbl <- rpd_data_tbl |> filter(Date >= "2017-01-01")
qqr_rpd_post_2017 <-QQR(x = rpd_data_post_tbl$headline_inflation,
                        y = log(rpd_data_post_tbl$rpd),
                        hm = "CV"
)
qqr_rpd_post_2017_gg <- ggplot.QQR(qqr_rpd_post_2017)


# Export ---------------------------------------------------------------
artifacts_quant_reg <- list (
  quantile_reg_plot = quantile_reg_plot,
  quants_regs_tbl = quants_regs_tbl,
  qqr_rpd_full_gg = qqr_rpd_full_gg,
  qqr_rpd_pre_2017_gg = qqr_rpd_pre_2017_gg,
  qqr_rpd_post_2017_gg = qqr_rpd_post_2017_gg
)

write_rds(artifacts_quant_reg, file = here("Outputs", "artifacts_quant_reg.rds"))

# save all the ggplots as pngs
ggsave(filename = here("Outputs", "qqr_rpd_full_gg.png"), plot = qqr_rpd_full_gg, width = 6, height = 4, dpi = 300)
ggsave(filename = here("Outputs", "qqr_rpd_pre_2017_gg.png"), plot = qqr_rpd_pre_2017_gg, width = 6, height = 4, dpi = 300)
ggsave(filename = here("Outputs", "qqr_rpd_post_2017_gg.png"), plot = qqr_rpd_post_2017_gg, width = 6, height = 4, dpi = 300)


