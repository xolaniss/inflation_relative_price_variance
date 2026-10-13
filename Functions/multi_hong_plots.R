multi_hong_plots <-
function(full_data,
         lag_data,
         selected_anomaly = "temp_anomaly",
         var_one_col_selected = "returns",
         var_two_col_selected = "temp_anomaly"){
  lag_data |>
  pmap(~{hong_plot(data = full_data,
                  M = as.numeric(..1),
                  selected_country = ..3,
                  selected_anomaly = selected_anomaly,
                  var_one_col_selected = var_one_col_selected,
                  var_two_col_selected = var_two_col_selected
      )})
}
