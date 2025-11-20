compile_data_files <- function(file_names_list) {
  full_paths <- paste0("data/", file_names_list)
  list_of_data_files <- lapply(full_paths, read.csv)
  main_data <- do.call(rbind, list_of_data_files)
  return (main_data)
}

prep_data <- function(data) {
  prepped_main_data <- mutate(
    data,
    time_milliseconds = set_units(set_units(.data$elapsed_time, "s"), "ms")
  )
  
  return (prepped_main_data)
}