
### Function for simulate data and sample size analysis

make_data_from_exploratory_data <- function(summary_df, total_n) { 
  
  
  # Number of groups
  G <- nrow(summary_df)
  
  # Get pooled SD 
  pooled_sd <- summary_df$Pooled_SD[1]
  
  group_probs <- rep(1 / G, G)
  
  # Determine n per group using a multinomial draw
  n_group <- as.numeric(rmultinom(1, size = total_n, prob = group_probs))
  
  
  # Simulate values for each group
  sim_list <- lapply(seq_len(G), function(i) {
    n_i <- n_group[i]
    if (n_i == 0) return(NULL)
    
    data.frame(
      strokes = summary_df$stroke_count[i],
      t_lap = rnorm(n_i, mean = summary_df$Mean[i], sd =pooled_sd )
    )
  })
  
  sim_data <- do.call(rbind, sim_list)
  
  # Add record_id
  sim_data$record_id <- seq_len(nrow(sim_data))
  
  # Reorder columns
  sim_data <- sim_data[, c("record_id", "strokes", "t_lap")]
  
  return(sim_data)
  
  
  
}



power_analysis <- function(simulation_count, summary_df, total_n, alpha) {
  reject_vector <- NA
  
  for (i in 1 : simulation_count) {
    exploratory_data_simulated_data <- make_data_from_exploratory_data( summary_df,
                                                                        total_n)
    model <- lm(t_lap ~ strokes + I(strokes^2), data = exploratory_data_simulated_data)
    reject_vector[i] <- broom::tidy(anova(model))[1, "p.value"] < alpha
  }
  print(mean(reject_vector))
  return (mean(reject_vector))
}


### Test

# exploratory_data_simulated_data<- make_data_from_exploratory_data(exploratory_summary, 1000) 
# 
# 
# 
# exploratory_summary <- summary_stats2
# 
# power_analysis_simulation_count <- 3000
# power_analysis_data_size <- 51
# alpha <- 0.05
# 
# exploratory_data_power <- power_analysis(power_analysis_simulation_count,
#                                          exploratory_summary , 
#                                          power_analysis_data_size,
#                                          0.05)