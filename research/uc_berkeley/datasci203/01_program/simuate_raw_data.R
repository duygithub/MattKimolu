

# List of required packages
required_packages <- c(
  "ggplot2", "readr", "dplyr", "tidyr", "tidyverse", 
  "rmarkdown", "here", "emmeans", "readxl", "data.table"
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


#######################################################################


###############################################################


## function for simulate raw data

simulate_data <- function(
    total_sample,
    elapsed_time_mean, elapsed_time_sd,
    stroke_count_mean, stroke_count_sd,
    data_entry_type_probability,
    seed = NULL
) {
  # ----- setup & validations -------------------------------------------------
  if (!is.null(seed)) set.seed(seed)
  
  # basic checks
  stopifnot(is.numeric(total_sample), length(total_sample) == 1, total_sample > 0)
  N <- as.integer(total_sample)
  
  stopifnot(is.numeric(elapsed_time_mean), is.numeric(elapsed_time_sd),
            length(elapsed_time_mean) == 1, length(elapsed_time_sd) == 1,
            elapsed_time_sd >= 0)
  
  stopifnot(is.numeric(stroke_count_mean), is.numeric(stroke_count_sd),
            length(stroke_count_mean) == 1, length(stroke_count_sd) == 1,
            stroke_count_sd >= 0)
  
  levels_det <- c("25", "50L1", "50L2", "100IM")
  
  # probabilities: allow named or unnamed vector of length 4
  stopifnot(length(data_entry_type_probability) == 4, is.numeric(data_entry_type_probability),
            all(is.finite(data_entry_type_probability)), all(data_entry_type_probability >= 0))
  
  # if named, reorder to expected levels
  if (!is.null(names(data_entry_type_probability))) {
    missing_names <- setdiff(levels_det, names(data_entry_type_probability))
    if (length(missing_names))
      stop("data_entry_type_probability must contain names: ", paste(levels_det, collapse = ", "))
    data_entry_type_probability <- data_entry_type_probability[levels_det]
  }
  
  # must sum to 1 (within a tiny tolerance)
  if (abs(sum(data_entry_type_probability) - 1) > 1e-8) {
    stop("data_entry_type_probability must sum to 1. Current sum = ", sum(data_entry_type_probability))
  }
  
  # ----- generate variables --------------------------------------------------
  # 1) event_ordinal: integer record id 1..N
  event_ordinal <- seq_len(N)
  
  # 2) elapsed_time: Normal(mean, sd), non-negative, 2 decimal places
  if (elapsed_time_sd == 0) {
    elapsed_time <- rep(elapsed_time_mean, N)
  } else {
    elapsed_time <- rnorm(N, mean = elapsed_time_mean, sd = elapsed_time_sd)
  }
  elapsed_time <- round(pmax(elapsed_time, 0), 2)
  
  # 3) stroke_count: Normal(mean, sd) -> integer, non-negative
  if (stroke_count_sd == 0) {
    stroke_count <- rep(stroke_count_mean, N)
  } else {
    stroke_count <- rnorm(N, mean = stroke_count_mean, sd = stroke_count_sd)
  }
  stroke_count <- as.integer(round(pmax(stroke_count, 0), 0))
  
  # 4) data_entry_type: categorical with specified probs
  data_entry_type <- factor(
    sample(levels_det, size = N, replace = TRUE, prob = data_entry_type_probability),
    levels = levels_det
  )
  
  # ----- assemble & return ---------------------------------------------------
  data.frame(
    event_ordinal = as.integer(event_ordinal),
    elapsed_time = elapsed_time,
    stroke_count = stroke_count,
    data_entry_type = data_entry_type,
    stringsAsFactors = FALSE
  )
}


### Example test run:

dat <- simulate_data(
  total_sample = 1000,
  elapsed_time_mean = 20, elapsed_time_sd = 4,
  stroke_count_mean = 16, stroke_count_sd = 3,
  data_entry_type_probability = c("25"=0.25, "50L1"=0.25, "50L2"=0.25, "100IM"=0.25),
  seed = 203
)

head(dat)
table(dat$data_entry_type)


dat$data_entry_type<- as.character(dat$data_entry_type)
write.csv(dat,here(
                   "simulated_raw_data1.csv"),
          row.names = FALSE)
