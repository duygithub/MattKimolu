define_causal_sem_plot_data <- function() {
  # We don't need to define the residual variances of the endogenous variables
  model_sem <- '
  lap_time ~ stroke_count + glide_distance
  stroke_count ~ glide_distance + technique
  glide_distance ~ technique + glide_time
  technique ~ fatigue + flow_state + training
  fatigue ~~ stroke_count
  flow_state ~~ stroke_count
  glide_time ~ training
  '
  
  plot_labels <- c(
    "lap_time"       = expression(paste("T"[Lap])),
    "stroke_count"   = expression(paste("S"[C])),
    "glide_distance" = "GlideDistance",
    "technique"      = "Technique",
    "glide_time"     = "GlideTime",
    "fatigue"        = "Fatigue",
    "flow_state"     = "FlowState",
    "training"     = "Training"
    
    # 
    # ,
    # "epsilon_0" = expression(epsilon[0]),
    # "epsilon_1" = expression(epsilon[1]),
    # "epsilon_2" = expression(epsilon[2]),
    # "epsilon_3" = expression(epsilon[3]),
    # "epsilon_4" = expression(epsilon[4]),
    # "epsilon_5" = expression(epsilon[5]
  )
  
  vars <- names(plot_labels)
  dummy_data <- data.frame(matrix(rnorm(length(vars) * 10), ncol = length(vars)))
  colnames(dummy_data) <- vars
  
  # fitted_sem_model <- sem(model_sem, data = dummy_data, do.fit = FALSE)
  fitted_sem_model <- sem(model_sem, data = dummy_data)
  
  return (list(fitted_sem_model = fitted_sem_model, plot_labels = plot_labels))
}