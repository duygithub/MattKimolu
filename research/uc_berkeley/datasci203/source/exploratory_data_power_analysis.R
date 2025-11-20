stats_with_sd <- summary_exploratory_data_stats[!is.na(summary_exploratory_data_stats$standard_deviation), ]

make_data_from_exploratory_data <- function(n) { 
  data_frame <- data.frame(
    t_lap = rnorm(
      n = n * nrow(stats_with_sd),
      mean = rep(stats_with_sd$mean_time, each = n),
      sd = rep(stats_with_sd$standard_deviation, each = n)
    ),
    
    strokes = rep(stats_with_sd$stroke_count, each = n)
  )
  
  return(data_frame)
}

power_analysis <- function(simulation_count, data_size) {
  reject_vector <- NA
  
  for (i in 1 : simulation_count) {
    exploratory_data_simulated_data <- make_data_from_exploratory_data(data_size)
    model <- lm(t_lap ~ strokes + I(strokes^2), data = exploratory_data_simulated_data)
    reject_vector[i] <- broom::tidy(anova(model))[1, "p.value"] < alpha
  }
  
  return (mean(reject_vector))
}

power_analysis_simulation_count <- 1000
power_analysis_data_size <- 16

exploratory_data_power <- power_analysis(simulation_count=power_analysis_simulation_count, data_size=power_analysis_data_size)