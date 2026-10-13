hong_rolling_test <-
  function(data,
           M,
           selected_country,
           selected_anomaly = "temp_anomaly",
           var_one_col_selected = "returns",
           var_two_col_selected = "temp_anomaly"){
    rolling_haugh_hong_tests(data = data |>
                                       dplyr::arrange(country, date) |>
                                       dplyr::filter(country == selected_country) |>
                                       tidyr::drop_na() |>
                                       dplyr::select(date, returns, {{selected_anomaly}}),
                                     M = M,
                                     var_col_one = var_one_col_selected,
                                     var_col_two = var_two_col_selected

    )

  }

hong_rolling_plot <- function(hong_rolling_results, selected_country, line_color){
  plot <- rug_plot(
    data = hong_rolling_results,
    y.var = H1, z.var = ph1,
    plot.name = "H1",
    country_label = selected_country,
    line_color = line_color)

  return(plot)
}

multi_hong_rolling_plots <- function(
    full_data,
    lag_data,
    selected_anomaly = "temp_anomaly",
    var_one_col_selected = "returns",
    var_two_col_selected = "temp_anomaly"){
  lag_data |>
    pmap(~{
      hong_rolling_test(
                data = full_data,
                M = as.numeric(..1),
                selected_country = ..3,
                selected_anomaly,
                var_one_col_selected,
                var_two_col_selected
      ) |>
        hong_rolling_plot(selected_country = ..3, line_color = ..4)
    })
}



