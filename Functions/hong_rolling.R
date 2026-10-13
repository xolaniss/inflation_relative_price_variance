# Rolling Haugh and Hong tests function
rolling_haugh_hong_tests <- function(data, M, var_col_one, var_col_two, intermediate_output = FALSE) {



  # Required libraries
  required_packages <- c("dplyr", "purrr", "zoo", "tidyverse", "scales", "rugarch")
  lapply(required_packages, require, character.only = TRUE)


  # Defining Variables
  u = data[[var_col_one]]
  v = data[[var_col_two]]
  date = data[["date"]]

  u = u - mean(u)
  v = v - mean(v)

  # GARCH setup
  df_garch = cbind(u, v)
  df_garch <- zoo(df_garch, date)

  # ARMA(1,1)-GARCH(1,1) specification
  ctrl = list(rho = 1, delta = 1e-8, outer.iter = 100, inner.iter = 650, tol = 1e-6)
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(1,1), include.mean = FALSE),
    distribution.model = "std"
  )

  # Fit GARCH models
  sgarch.u <- ugarchfit(data = df_garch[, "u", drop = FALSE], spec = spec,
                        solver = "solnp", solver.control = ctrl)
  sgarch.v <- ugarchfit(data = df_garch[, "v", drop = FALSE], spec = spec,
                        solver = "solnp", solver.control = ctrl)

  # Save intermediate outputs if requested
  if (intermediate_output) {
    write.table(sgarch.u@fit$matcoef, file = "arma_garch_u.txt")
    write.table(sgarch.v@fit$matcoef, file = "arma_garch_v.txt")
  }

  # Window size calculation
  alpha <- 0.05
  beta <- 0.025
  delta <- 0.5
  za <- qnorm(1 - alpha/2)
  zb <- qnorm(1 - beta)
  S <- round(2 * (za + zb)^2 / delta^2)
  S <- 125  # Approximately 125 business days

  date <- date[-(1:(S-1))]

  # Rolling CCF function
  do_roll_ccf <- function(data, x1, x2, window = 125, lag = 9) {
    res <- rollapply(data, window, function(x) {
      ccf(x[, x1], x[, x2], lag.max = lag, type = 'correlation')
    }, by.column = FALSE)
    return(res)
  }

  # Reshape CCF function
  reshape_ccf <- function(df) {
    n <- dim(df)[1]
    corr0 <- vector(mode = 'numeric', length = n)
    corrp0 <- matrix(0, n, M + 1)
    corrn0 <- matrix(0, n, M + 1)

    for (ii in 1:n) {
      corrp0[ii, 1] <- df[[ii]][M + 1, 1, 1]
      corrn0[ii, 1] <- df[[ii]][M + 1, 1, 1]
      for (jj in 1:M) {
        corrn0[ii, jj + 1] <- df[[ii]][M - jj + 1, 1, 1]
        corrp0[ii, jj + 1] <- df[[ii]][M + jj + 1, 1, 1]
      }
    }

    res <- list(corrp0 = corrp0, corrn0 = corrn0)
    return(res)
  }

  # Calculate rolling CCF
  ccf_data <- lapply(list(df_garch), FUN = do_roll_ccf, x1 = 'u', x2 = 'v', window = S, lag = M)
  ccf_data <- ccf_data[[1]]

  T <- dim(ccf_data)[1]
  roll_ccf <- reshape_ccf(ccf_data)

  # Prepare correlation matrices
  corrp0 <- roll_ccf$corrp0[, 1:M]
  corrn0 <- roll_ccf$corrn0[, 1:M]
  corrp <- roll_ccf$corrp0[, 2:(M + 1)]
  corrn <- roll_ccf$corrn0[, 2:(M + 1)]
  corr0 <- roll_ccf$corrp0[, 1]

  # Initialize test statistics and p-values
  H1 = ph1 = matrix(0, T, 1)
  H2 = ph2 = H1
  H10 = ph10 = H1
  H20 = ph20 = H1
  Hb = phb = H1

  # Kernel function: Bartlett function K(z) = 1-|z|, |z| <= 1
  J = 1:M
  Kbar = 1 - abs(J/M)
  C1 = sum((1 - J/T) * Kbar^2)
  D1 = sum((1 - J/T) * (1 - J/T - 1/T) * Kbar^4)

  # Bidirectional Hong test parameters
  Jb = (-M):M
  Kbarb = 1 - abs(Jb/M)
  C2 = sum((1 - abs(Jb)/T) * Kbarb^2)
  D2 = sum((1 - abs(Jb)/T) * (1 - abs(Jb)/T - 1/T) * Kbarb^4)

  # Time-varying Hong tests and p-values
  for (t in 1:T) {
    H1[t] = (T * sum(Kbar^2 * corrp[t, ]^2) - C1) / sqrt(2 * D1)
    ph1[t] = 1 - pnorm(H1[t])

    H2[t] = (T * sum(Kbar^2 * corrn[t, ]^2) - C1) / sqrt(2 * D1)
    ph2[t] = 1 - pnorm(H2[t])

    H10[t] = (T * sum(Kbar^2 * corrp0[t, ]^2) - C1) / sqrt(2 * D1)
    ph10[t] = 1 - pnorm(H10[t])

    H20[t] = (T * sum(Kbar^2 * corrn0[t, ]^2) - C1) / sqrt(2 * D1)
    ph20[t] = 1 - pnorm(H20[t])

    Hb[t] = (T * sum(Kbar^2 * corrn[t, ]^2) + T * corr0[t]^2 + T * sum(Kbar^2 * corrp[t, ]^2) - C2) / sqrt(2 * D2)
    phb[t] = 1 - pnorm(Hb[t])
  }

  # Time-varying Haugh tests
  Q1 = p1 = Q2 = p2 = Q10 = p10 = Q20 = p20 = Qb = pb = H1
  M0 = M

  for (t in 1:T) {
    Q1[t] = T * sum(corrp[t, 1:M0]^2)
    p1[t] = 1 - pchisq(Q1[t], M0)
    Q2[t] = T * sum(corrn[t, 1:M0]^2)
    p2[t] = 1 - pchisq(Q2[t], M0)
    Q10[t] = T * sum(corrp0[t, 1:M0]^2)
    p10[t] = 1 - pchisq(Q10[t], M0)
    Q20[t] = T * sum(corrn0[t, 1:M0]^2)
    p20[t] = 1 - pchisq(Q20[t], M0)
    Qb[t] = T * (sum(corrn[t, 1:M0]^2) + corr0[t]^2 + sum(corrp[t, 1:M0]^2))
    pb[t] = 1 - pchisq(Qb[t], (2 * M0 + 1))
  }

  # Prepare outputs
  outhong = cbind(date, H1, ph1, H2, ph2, H10, ph10, H20, ph20, Hb, phb)
  outHong = outhong[(M:T), ]

  outhaugh = cbind(date, Q1, p1, Q2, p2, Q10, p10, Q20, p20, Qb, pb)
  outHaugh = outhaugh[(M:T), ]

  # Save results
  if(intermediate_output) {
    write.csv(outHaugh, file = "roll_Haugh_Granger.csv")
  }

  # Process results for plotting
  results <-  as.data.frame(outHaugh)
  # results[, 1] <- NULL
  colnames(results) <- c("date", "H1", "ph1", "H2", "ph2", "H10", "ph10", "H20", "ph20", "Hb", "phb")

  results$date <- date[-c(1:(M-1))]

  # Discrete p-values function
  discrete <- function(pvalue) {
    case_when(pvalue < 0.05 ~ "1", pvalue > 0.05 ~ "0", TRUE ~ "Other")
  }

  results$ph1 <- discrete(results$ph1)
  results$ph2 <- discrete(results$ph2)
  results$ph10 <- discrete(results$ph10)
  results$ph20 <- discrete(results$ph20)
  results$phb <- discrete(results$phb)

  # Initialize rug plots
  results$ph1[1] <- 0
  results$ph2[1] <- 0
  results$ph10[1] <- 0
  results$ph20[1] <- 0
  results$phb[1] <- 0

  # Export final results
  if(intermediate_output){
  write.csv(results, file = "results.csv", row.names = FALSE)
  }

  # Return results and helper function
  return(results)
}

# Helper function for plotting
rug_plot <- function(data, y.var, z.var, plot.name, country_label,
                     rug_colors = c("#ffffff", "#2C3E50"),  # Rug colors
                     line_color = "#2C3E50",                # Line color
                     line_size = 0.7) {

  y.var <- enquo(y.var)
  z.var <- enquo(z.var)

  data %>%
    ggplot(aes(x = date, y = !!y.var)) +
    geom_line(color = line_color, linewidth =  line_size) +
    geom_rug(aes(color = !!z.var), inherit.aes = TRUE, sides = "t",
             show.legend = FALSE, length = unit(0.08, "npc")) +
    scale_x_date(labels = date_format("%Y"), date_breaks = "5 years") +
    labs(x = "",
         y = paste0("Causality Test Statistic (", plot.name,")"),
         subtitle = paste0("Country: ", country_label)) +
    scale_color_manual(values = rug_colors) +  # Explicitly named for clarity
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(
      text = element_text(size = 8),
      strip.background = element_rect(colour = "white", fill = "white"),
      axis.text.x = element_text(angle = 90),
      axis.title = element_text(size = 7),
      plot.tag = element_text(size = 8)
    )
}


# Example usage:
# result <- rolling_haugh_hong_tests(data = my_data, M = 5,
#                                   var_col_one = "Coal_Price_log_diff",
#                                   var_col_two = "MAX_log_diff",
#                                   intermediate_output = TRUE)
