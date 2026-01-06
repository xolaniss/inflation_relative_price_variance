# Description
# Price data - Xolani Sibande October 2025
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

# Import and cleaning -------------------------------------------------------------
sheets_names <- excel_sheets(here("Data", "Price_dispersion_data.xlsx"))

data <-
  read_excel(here("Data", "Price_dispersion_data.xlsx"), sheet = sheets_names[3], skip = 0)

weights_tbl <-
  data |>
  slice((1:2)) |>
  rename(
    "Cereal products" = cerealprod,
    "Meat" = meat,
    "Fish and other seafood" = fish_seafood,
    "Milk, other dairy products and eggs" = milk_dairy_eggs,
    "Oils and fats" = oil_fats,
    "Fruit and nuts" = fruit_nuts,
    "Vegetables" = veggies,
    "Sugar, confectionary and desserts" = sugar_dess,
    "Other food" = other_food,
    "Hot beverages" = hot_bev,
    "Cold beverages" = cold_bev,
    "Spirits" = spirits,
    "Wine" = wine,
    "Beer" = beer,
    "Tobacco" = tobacco,
    "Clothing" = clothing,
    "Footwear" = footwear,
    "Actual rentals for housing" = rentals,
    "Owners' equivalent rent" = OER,
    "Maintenance and repairs" = main_repairs,
    "Water supply and miscellaneous services" = water_sup_serv,
    "Electricity and other fuels" = electr_ofuels,
    "Furniture, furnishings and carpets" = furn_carp,
    "Appliances, tableware and equipment" = app_table_equip,
    "Goods and services for routine household maintenance" = gs_for_housemain,
    "Medicines and health products" = med_healthpro,
    "Health services" = health_serv,
    "Purchase of vehicles" = purch_vehicle,
    "Passenger transport services" = pass_trans_serv,
    "Information and communication equipment" = inf_com_equip,
    "Information and communication services" = info_com_serv,
    "Recreational and cultural services" = recr_cult_serv,
    "Newspapers, books and stationery" = news_book_stat,
    "Package holidays" = pack_hol,
    "Primary and secondary education" = prim_sec_edu,
    "Tertiary education" = tert_edu,
    "Restaurants" = restaurants,
    "Accommodation services" = accom_serv,
    "Personal care" = pers_care,
    "Other services" = other_serv,
    "Insurance" = insurance,
    "Financial services" = fin_serv,
    "Fuel" = fuel
  ) |>
  slice(-1) |>
  dplyr::select(-month, -`headline inflation`, -rvp, -`...47`) |>
  pivot_longer(everything(), names_to = "Series", values_to = "Weight")

price_data_tbl <-
  data |>
  slice(-(1:2)) |>
  janitor::clean_names() |>
  mutate(month = as.Date(month, format = "%Y-%m-%d")) |>
  rename(Date = month) |>
  dplyr::select(-starts_with("x")) |>
  rename(
    "Cereal products" = cerealprod,
    "Meat" = meat,
    "Fish and other seafood" = fish_seafood,
    "Milk, other dairy products and eggs" = milk_dairy_eggs,
    "Oils and fats" = oil_fats,
    "Fruit and nuts" = fruit_nuts,
    "Vegetables" = veggies,
    "Sugar, confectionary and desserts" = sugar_dess,
    "Other food" = other_food,
    "Hot beverages" = hot_bev,
    "Cold beverages" = cold_bev,
    "Spirits" = spirits,
    "Wine" = wine,
    "Beer" = beer,
    "Tobacco" = tobacco,
    "Clothing" = clothing,
    "Footwear" = footwear,
    "Actual rentals for housing" = rentals,
    "Owners' equivalent rent" = oer,
    "Maintenance and repairs" = main_repairs,
    "Water supply and miscellaneous services" = water_sup_serv,
    "Electricity and other fuels" = electr_ofuels,
    "Furniture, furnishings and carpets" = furn_carp,
    "Appliances, tableware and equipment" = app_table_equip,
    "Goods and services for routine household maintenance" = gs_for_housemain,
    "Medicines and health products" = med_healthpro,
    "Health services" = health_serv,
    "Purchase of vehicles" = purch_vehicle,
    "Passenger transport services" = pass_trans_serv,
    "Information and communication equipment" = inf_com_equip,
    "Information and communication services" = info_com_serv,
    "Recreational and cultural services" = recr_cult_serv,
    "Newspapers, books and stationery" = news_book_stat,
    "Package holidays" = pack_hol,
    "Primary and secondary education" = prim_sec_edu,
    "Tertiary education" = tert_edu,
    "Restaurants" = restaurants,
    "Accommodation services" = accom_serv,
    "Personal care" = pers_care,
    "Other services" = other_serv,
    "Insurance" = insurance,
    "Financial services" = fin_serv,
    "Fuel" = fuel
  )

price_data_names <-
  read_excel(here("Data", "Price_dispersion_data.xlsx"), sheet = sheets_names[8], skip = 0) |>
  dplyr::select(-1) |>
  slice(-(1:3)) |>
  slice(-((n() - 6):n())) |>
  rename("Old_names" = 1, "New_names" = 2) |>
  drop_na() |>
  mutate(
    vec = paste0("`", New_names, "`", " = ", Old_names, ",")
  )
old_names_vec <- as_vector(price_data_names$Old_names)
new_names_vec <- as_vector(price_data_names$New_names)
cat(price_data_names$vec, sep = "\n")
# EDA ---------------------------------------------------------------
price_data_tbl |> glimpse()
price_data_tbl |> skimr::skim()

# Graphing ---------------------------------------------------------------
price_data_long_tbl <-
  price_data_tbl |>
  pivot_longer(-Date, names_to = "Series", values_to = "Value")

price_data_gg <-
  price_data_long_tbl |>
  filter(!Series %in% c("rvp", "headline_inflation")) |>
  fx_plot(variables_color = 45, ncol = 6)

median_headline_inflation <-
  price_data_long_tbl |>
  filter(Series %in% c("headline_inflation")) |>
  summarise(median_value = median(Value, na.rm = TRUE))

headline_inflation_gg <-
  price_data_long_tbl |>
  filter(Series %in% c("headline_inflation")) |>
  mutate(Series = str_replace(Series, "headline_inflation", "Headline Inflation")) |>
  fx_plot(variables_color = 1) +
  # geom_hline(yintercept = median_headline_inflation$median_value, linewidth = .8, linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.Date("2017-07-10"), color = "red", linetype = "dashed") +
  labs(y = "Headline Inflation") +
  theme(strip.text = element_blank(), strip.background = element_blank())

rvp_gg <-
  price_data_long_tbl |>
  filter(Series %in% c("rvp")) |>
  mutate(Series = str_replace(Series, "rvp", "Relative price variance")) |>
  fx_plot(variables_color = 1)

weights_gg <-
  weights_tbl |>
  # do a bar plot
  ggplot(aes(x = reorder(Series, Weight), y = Weight, fill = Series)) +
  geom_bar(stat = "identity") +
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
  labs(x = "", y = "Weight (%)") +
  scale_fill_manual(values = pnw_palette("Winter", 48)) +
  coord_flip()


# Export ---------------------------------------------------------------
artifacts_price_data <- list(
  data = list(
    price_data_tbl = price_data_tbl,
    weights_tbl = weights_tbl,
    price_data_names = price_data_names
  ),
  plots = list(
    price_data_gg = price_data_gg,
    headline_inflation_gg = headline_inflation_gg,
    rvp_gg = rvp_gg,
    weights_gg = weights_gg
  )
)

write_rds(artifacts_price_data, file = here("Outputs", "artifacts_price_data.rds"))
