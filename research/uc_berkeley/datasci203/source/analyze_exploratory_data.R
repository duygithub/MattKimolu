exploratory_data_file1 <- read.csv("data/20251106_-8.csv")
exploratory_data_file2 <- read.csv("data/20251111_-8.csv")

exploratory_data <- rbind(exploratory_data_file1, exploratory_data_file2)

# prepped_exploratory_data <- mutate(
#   exploratory_data,
#   # Create the new grouping variable
#   swim_group = case_when(
#     data_entry_type %in% c("25", "50L1") ~ "25_50L1",
#     data_entry_type == "50L2" ~ "50L2"
#   ),
#     
#   time_seconds = as.numeric(ms(elapsed_time))
# )

prepped_exploratory_data <- mutate(
  exploratory_data,
  time_milliseconds = set_units(set_units(.data$elapsed_time, "s"), "ms")
)

# test_time_seconds <- set_units(set_units(elapsed_time, "s"), "ms")

# summary_exploratory_data_stats <- summarize(group_by(prepped_exploratory_data, swim_group), mean_time = mean(time_seconds), standard_deviation=sd(time_seconds))

# summary_exploratory_data_stats <- summarize(prepped_exploratory_data, mean_time = mean(time_seconds), standard_deviation=sd(time_seconds))

summary_exploratory_data_stats <- summarize(group_by(prepped_exploratory_data, stroke_count), mean_time = as.numeric(mean(time_milliseconds)), standard_deviation=sd(time_milliseconds))