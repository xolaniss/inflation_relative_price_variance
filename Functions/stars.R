stars <-
function(data){
    data |> 
      mutate(
        stars = ifelse(p.value < 0.01, "***",
                       ifelse(p.value < 0.05, "**",
                              ifelse(p.value < 0.1, "*", " ")))
      )
}
