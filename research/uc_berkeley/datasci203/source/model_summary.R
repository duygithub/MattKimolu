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