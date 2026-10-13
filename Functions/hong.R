dcc_hong_haugh_test <- function(data, var_one_col, var_two_col, date_col, M) {
  # DCC-GARCH+ time-varying Haugh and Hong tests

  # Required libraries
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")
  if (!requireNamespace("zoo", quietly = TRUE)) install.packages("zoo")
  if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
  if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")

  library(dplyr)
  library(purrr)
  library(zoo)
  library(tidyverse)
  library(scales)

  file_sources <- list.files(here("Functions", "dcc_functions"), pattern = "*.R", full.names = TRUE)
  for (file in file_sources) {
    source(file)
  }
  dyn.load(here("Functions", "dcc_functions", "R_vector_garch.so"))
  dyn.load(here("Functions", "dcc_functions", "R_dcc.so"))

  # Extract variables
  u <- data[[var_one_col]]
  v <- data[[var_two_col]]
  date <- data[[date_col]]

  # Center the variables
  u <- u - mean(u)
  v <- v - mean(v)

  T <- length(date)

  # Initialize correlation matrices
  corrp <- matrix(0, T, M)
  corrn <- matrix(0, T, M)
  corr0 <- matrix(0, T, 1)

  dcc.data <- cbind(u, v)

  # Initial parameter values
  a1 <- cov(u, u)
  a2 <- cov(v, v)
  cri <- cor(u, v)

  a <- c(a1, a2)
  A <- diag(c(0.05, 0.05))
  B <- diag(c(0.9, 0.9))
  uncR <- matrix(c(1, cri, cri, 1), 2, 2)
  dcc.para <- c(0.05, 0.9)

  # Estimate DCC-GARCH(1,1) model for contemporaneous correlation
  dcc.results_list <- dcc.estimation(inia = a, iniA = A, iniB = B,
                                     ini.dcc = dcc.para, dvar = dcc.data,
                                     model = "diagonal")

  corr0 <- dcc.results_list$DCC[, 2]

  # Positive lags (V Granger causes U)
  for (k in 1:M) {
    uk <- u[(1 + k):T]
    vk <- v[1:(T - k)]

    dcc.data <- cbind(uk, vk)

    a1 <- cov(uk, uk)
    a2 <- cov(vk, vk)
    cri <- cor(uk, vk)

    a <- c(a1, a2)
    A <- diag(c(0.05, 0.05))
    B <- diag(c(0.9, 0.9))
    uncR <- matrix(c(1, cri, cri, 1), 2, 2)
    dcc.para <- c(0.05, 0.85)

    dcc.results_list <- dcc.estimation(inia = a, iniA = A, iniB = B,
                                       ini.dcc = dcc.para, dvar = dcc.data,
                                       model = "diagonal")

    corrp[((k + 1):T), k] <- dcc.results_list$DCC[, 2]
  }

  # Negative lags (U Granger causes V)
  for (k in 1:M) {
    uk <- u[1:(T - k)]
    vk <- v[(1 + k):T]

    dcc.data <- cbind(uk, vk)

    a1 <- cov(uk, uk)
    a2 <- cov(vk, vk)
    cri <- cor(uk, vk)

    a <- c(a1, a2)
    A <- diag(c(0.05, 0.05))
    B <- diag(c(0.9, 0.9))
    uncR <- matrix(c(1, cri, cri, 1), 2, 2)
    dcc.para <- c(0.05, 0.85)

    dcc.results_list <- dcc.estimation(inia = a, iniA = A, iniB = B,
                                       ini.dcc = dcc.para, dvar = dcc.data,
                                       model = "diagonal")

    corrn[((k + 1):T), k] <- dcc.results_list$DCC[, 2]
  }

  # Prepare correlation matrices including contemporaneous effects
  corrp0 <- cbind(corr0, corrp[, 1:(M - 1)])
  corrn0 <- cbind(corr0, corrn[, 1:(M - 1)])

  # Initialize test statistics and p-values
  H1 <- ph1 <- matrix(0, T, 1)
  H2 <- ph2 <- H1
  H10 <- ph10 <- H1
  H20 <- ph20 <- H1
  Hb <- phb <- H1

  # Kernel function (Bartlett)
  J <- 1:M
  Kbar <- 1 - abs(J / M)
  C1 <- sum((1 - J / T) * Kbar^2)
  D1 <- sum((1 - J / T) * (1 - J / T - 1 / T) * Kbar^4)

  # Bidirectional test parameters
  Jb <- (-M):M
  Kbarb <- 1 - abs(Jb / M)
  C2 <- sum((1 - abs(Jb) / T) * Kbarb^2)
  D2 <- sum((1 - abs(Jb) / T) * (1 - abs(Jb) / T - 1 / T) * Kbarb^4)

  # Calculate Hong tests and p-values
  for (t in 1:T) {
    H1[t] <- (T * sum(Kbar^2 * corrp[t, ]^2) - C1) / sqrt(2 * D1)
    ph1[t] <- 1 - pnorm(H1[t])

    H2[t] <- (T * sum(Kbar^2 * corrn[t, ]^2) - C1) / sqrt(2 * D1)
    ph2[t] <- 1 - pnorm(H2[t])

    H10[t] <- (T * sum(Kbar^2 * corrp0[t, ]^2) - C1) / sqrt(2 * D1)
    ph10[t] <- 1 - pnorm(H10[t])

    H20[t] <- (T * sum(Kbar^2 * corrn0[t, ]^2) - C1) / sqrt(2 * D1)
    ph20[t] <- 1 - pnorm(H20[t])

    Hb[t] <- (T * sum(Kbar^2 * corrn[t, ]^2) + T * corr0[t]^2 +
                T * sum(Kbar^2 * corrp[t, ]^2) - C2) / sqrt(2 * D2)
    phb[t] <- 1 - pnorm(Hb[t])
  }

  # Haugh tests
  Q1 <- p1 <- Q2 <- p2 <- Q10 <- p10 <- Q20 <- p20 <- Qb <- pb <- H1

  for (t in 1:T) {
    Q1[t] <- T * sum(corrp[t, 1:M]^2)
    p1[t] <- 1 - pchisq(Q1[t], M)

    Q2[t] <- T * sum(corrn[t, 1:M]^2)
    p2[t] <- 1 - pchisq(Q2[t], M)

    Q10[t] <- T * sum(corrp0[t, 1:M]^2)
    p10[t] <- 1 - pchisq(Q10[t], M)

    Q20[t] <- T * sum(corrn0[t, 1:M]^2)
    p20[t] <- 1 - pchisq(Q20[t], M)

    Qb[t] <- T * (sum(corrn[t, 1:M]^2) + corr0[t]^2 + sum(corrp[t, 1:M]^2))
    pb[t] <- 1 - pchisq(Qb[t], (2 * M + 1))
  }

  # Prepare output
  outhong <- cbind(date, H1, ph1, H2, ph2, H10, ph10, H20, ph20, Hb, phb)
  outHong <- outhong[(M:T), ]

  outhaugh <- cbind(date, Q1, p1, Q2, p2, Q10, p10, Q20, p20, Qb, pb)
  outHaugh <- outhaugh[(M:T), ]

  # Convert to data frame
  results <- as.data.frame(outHong)
  colnames(results) <- c("date", "H1", "ph1", "H2", "ph2", "H10", "ph10",
                         "H20", "ph20", "Hb", "phb")

  # Add dates for graphing (removing first M-1 observations)
  date_adjustment <- function(dates, M) {
    if (M <= 1) {
      return(dates)
    } else {
    return(dates[-c(1:(M - 1))])
    }
  }
  results$date <- date_adjustment(date, M)

  # Convert p-values to discrete indicators
  discrete <- function(pvalue) {
    case_when(pvalue < 0.05 ~ "1", pvalue > 0.05 ~ "0", TRUE ~ "Other")
  }

  results$ph1 <- discrete(results$ph1)
  results$ph2 <- discrete(results$ph2)
  results$ph10 <- discrete(results$ph10)
  results$ph20 <- discrete(results$ph20)
  results$phb <- discrete(results$phb)

  # Initialize first observation
  results$ph1[1] <- "0"
  results$ph2[1] <- "0"
  results$ph10[1] <- "0"
  results$ph20[1] <- "0"
  results$phb[1] <- "0"

  return(list(
    Hong_results = outHong,
    Haugh_results = outHaugh,
    processed_results = results
  ))
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
    scale_x_date(labels = date_format("%Y"), date_breaks = "2 years") +
    labs(x = "",
         y = paste0("Causality Test Statistic (", plot.name,")")
         ) +
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
