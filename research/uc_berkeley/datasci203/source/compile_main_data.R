compile_data_files <- function(file_names_list) {
  full_paths <- paste0("data/", file_names_list)
  list_of_data_files <- lapply(full_paths, read.csv)
  main_data <- do.call(rbind, list_of_data_files)
  return (main_data)
}

prep_data_for_plotting <- function(data) {
  data_for_plotting <- mutate(
    data,
    time_milliseconds = set_units(set_units(.data$elapsed_time, "s"), "ms")
  )
  
  return (data_for_plotting)
}

prep_data_for_modeling <- function(data) {
  data_for_modeling <- prep_data_for_plotting(data) %>%
    mutate(data_entry_type = ifelse(data_entry_type %in% c("25", "50L1"), 
                                    "25 or 50L1", data_entry_type) ) %>%
    mutate(data_entry_type = factor(data_entry_type,
                                    levels = c("25 or 50L1", "50L2", "100IM")),
           unwell_feedback = as.character(unwell_feedback),
           unwell_feedback = factor(unwell_feedback,
                                    levels = c("0", "-1", "1")),
           stroke_count = as.numeric(stroke_count),
           event_ordinal = as.numeric(event_ordinal),
           time_milliseconds = as.numeric(time_milliseconds)
    )
  return (data_for_modeling)
}