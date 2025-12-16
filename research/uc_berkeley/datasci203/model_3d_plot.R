

# List of required packages
required_packages <- c(
  "ggplot2", "readr", "dplyr", "tidyr", "tidyverse", 
  "rmarkdown", "here", "emmeans", "readxl", "data.table",
  "parsedate", "mmrm", "patchwork",
  "rstudioapi",
  "units", "stargazer", "sandwich",
  "knitr", "modelsummary", "broom", "rlang",
  "MASS", "plotly"
  
)

# Function to check and install missing packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Call the function to install and load the necessary packages
install_if_missing(required_packages)

# Set the encoding to UTF-8
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
#################################################################


source("source/compile_main_data.R")

data_files_names <- c("20251118_-8.csv", "20251121_-8.csv", "20251122_-8.csv", "20251129_-8.csv", "20251204_-8.csv", "20251208_-8.csv", "20251211_-8.csv","20251213_-8.csv")
main_data <- compile_data_files(data_files_names)
main_data_for_plotting <- prep_data_for_plotting(main_data)
main_data_for_modeling <- prep_data_for_modeling(main_data)


################################################################



main_data_for_modeling2 <- main_data_for_modeling%>%
  mutate(record_id = row_number(), .before = 1) %>%
  mutate(stroke_count_square = stroke_count ^2)



model_v4 <- lm(time_milliseconds ~ 
                 stroke_count + 
                 I(stroke_count^2) + 
                 data_entry_type + 
                 event_ordinal + 
                 event_ordinal  * stroke_count +
                 event_ordinal  *I(stroke_count^2)  , 
               data = main_data_for_modeling  )
#######################################################################

model <- model_v4

main_data_y_min <- 24000
main_data_y_max <- 42000 



main_data_y_seq <- seq(from = main_data_y_min, to = main_data_y_max, by = 1000)

color_mapping <- c(
  "25 or 50L1" = "blue",
  "50L2" = "red",
  "100IM" = "darkgreen")


# Create prediction data for each data_entry_type
stroke_range <- range(main_data_for_modeling$stroke_count)

ordinal_mean <- ceiling(mean(main_data_for_modeling$event_ordinal))


pred_data <- expand.grid(
  event_ordinal = seq(ordinal_mean,ordinal_mean, length.out = 1),
  stroke_count = seq(stroke_range[1], stroke_range[2], length.out = 100),
  data_entry_type = unique(main_data_for_modeling$data_entry_type)
)

pred_results <- predict(model , newdata = pred_data, interval = "confidence", level = 0.95)
pred_data <- cbind(pred_data, pred_results)

#######################################################################

# Create a grid that varies BOTH stroke_count and event_ordinal
stroke_range <- range(main_data_for_modeling$stroke_count)
ordinal_range <- range(main_data_for_modeling$event_ordinal)

# Create prediction grid for 3D surface
pred_data_3d <- expand.grid(
  stroke_count = seq(stroke_range[1], stroke_range[2], length.out = 50),
  event_ordinal = seq(ordinal_range[1], ordinal_range[2], length.out = 50),
  data_entry_type = levels(factor(main_data_for_modeling$data_entry_type))[1]  # Pick one type or loop through
)

# Get predictions
pred_data_3d$predicted_time <- predict(model_v4, newdata = pred_data_3d)



###################################################################



# library(plotly)

# Create grid for ALL data_entry_types
pred_data_all <- expand.grid(
  stroke_count = seq(stroke_range[1], stroke_range[2], length.out = 50),
  event_ordinal = seq(ordinal_range[1], ordinal_range[2], length.out = 50),
  data_entry_type = unique(main_data_for_modeling$data_entry_type)
)
pred_data_all$predicted_time <- predict(model_v4, newdata = pred_data_all)

# Create plot with multiple surfaces
fig <- plot_ly()

for (type in unique(pred_data_all$data_entry_type)) {
  subset_data <- pred_data_all[pred_data_all$data_entry_type == type, ]
  
  z_mat <- matrix(subset_data$predicted_time, nrow = 50, ncol = 50)
  
  fig <- fig %>%
    add_surface(
      x = unique(subset_data$stroke_count),
      y = unique(subset_data$event_ordinal),
      z = t(z_mat),
      name = type,
      opacity = 0.7,
      showscale = FALSE
    )
}

fig <- fig %>%
  layout(
    scene = list(
      xaxis = list(title = "Stroke Count"),
      yaxis = list(title = "Event Ordinal"),
      zaxis = list(title = "Time (ms)")
    ),
    title = "Predicted Swimming Time by Event Type"
  )

fig