calculate_model_coefficients_summary <- function(model) {
  model_summary <- summary(model)
  coefficients <- as.data.frame(model_summary$coefficients)
  coefficient_table_entries <- rownames_to_column(coefficients, var = "Coefficient")
  model_summary_table_entries <- coefficient_table_entries %>%
    select("Coefficient", "Estimate" ,     "Pr(>|t|)"  ) %>%
    rename(Value = 'Estimate', Pvalue = 'Pr(>|t|)')
  return (list(table_summary_data = model_summary_table_entries,
               r_squared = model_summary$r.squared,
               adjusted_r_squared = model_summary$adj.r.squared))
}

derive_detailed_analysis_by_stroke_count <- function(data_frame, data_entry_type_filter_value) {
  data_frame %>%
    filter(data_entry_type == data_entry_type_filter_value) %>%
    group_by( stroke_count) %>%
    summarize(n = length((event_ordinal)),
              !!r"($\bar{x}$)" := mean(time_milliseconds, na.rm = TRUE),
              !!r"($s$)" := sd(time_milliseconds, na.rm = TRUE),
              !!r"($x_{\text{Fastest}}$)" := quantile(time_milliseconds,probs = 0,type=2,na.rm = TRUE) ,
              !!r"($\tilde{x}$)" := median(time_milliseconds, na.rm = TRUE),
              !!r"($s_{\bar{x}}$)" := sd(time_milliseconds, na.rm = TRUE)/sqrt(length(time_milliseconds)),
              !!r"($x_{\text{Slowest}}$)" := quantile(time_milliseconds,probs = 1,type=2,na.rm = TRUE)
    ) %>%
    rename(!!r"($S_c$)" := stroke_count)
}