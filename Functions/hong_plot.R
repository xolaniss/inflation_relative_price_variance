hong_test <-
  function(data,
           M,
           var_one_col_selected = "returns",
           var_two_col_selected = "temp_anomaly") {
    hong_test <- dcc_hong_haugh_test(
      data,
      M = M,
      var_one_col = var_one_col_selected,
      var_two_col = var_two_col_selected,
      date_col = "date"
    )

    hong_results <- hong_test$processed_results

    return(hong_results)
  }

hong_plot <- function(hong_results, y_var = H1, z_var = ph1, line_color) {
  plot <- rug_plot(
    data = hong_results,
    y.var = {{ y_var }}, z.var = {{ z_var }},
    plot.name = "Bi-directional",
    line_color = line_color
  )

  return(plot)
}

multi_hong_plots <- function(full_data,
                             lag_data,
                             selected_anomaly = "temp_anomaly",
                             var_one_col_selected = "returns",
                             var_two_col_selected = "temp_anomaly") {
  # Ensure lag_data has a line_color column
  if (!"line_color" %in% names(lag_data)) {
    lag_data$line_color <- "steelblue" # Default color
  }

  # Debug: print what we're passing
  print("Column names in lag_data:")
  print(names(lag_data))
  print("First row values:")
  print(as.list(lag_data[1, ]))

  lag_data |>
    pmap(~ {
      hong_test(
        data = full_data,
        M = as.numeric(..1),
        selected_country = ..3,
        selected_anomaly,
        var_one_col_selected,
        var_two_col_selected
      ) |>
        hong_plot(selected_country = ..3, line_color = ..4)
    })
}

multi_hong_data <- function(full_data,
                            lag_data,
                            selected_anomaly = "temp_anomaly",
                            var_one_col_selected = "returns",
                            var_two_col_selected = "temp_anomaly") {
  lag_data |>
    pmap(~ {
      hong_test(
        data = full_data,
        M = as.numeric(..1),
        selected_country = ..3,
        selected_anomaly,
        var_one_col_selected,
        var_two_col_selected
      )
    })
}
