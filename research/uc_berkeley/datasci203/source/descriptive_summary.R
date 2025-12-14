
descriptive_summary <- function(prepped_data)  {
  
  summary_stats2 <-prepped_data %>%
    group_by(stroke_count) %>%
    summarize(n=length(event_ordinal),
              Mean = mean(elapsed_time, na.rm = TRUE),
              SD=sd(elapsed_time, na.rm = TRUE),
              SE = sd(elapsed_time, na.rm = TRUE)/sqrt(length(elapsed_time)),
              Min=quantile(elapsed_time,probs = 0,type=2,na.rm = TRUE) ,
              Q1=quantile(elapsed_time,probs = 0.25,type=2,na.rm = TRUE),
              Median= median(elapsed_time, na.rm = TRUE),
              Q3=quantile(elapsed_time,probs = 0.75,type=2,na.rm = TRUE),
              Max=quantile(elapsed_time,probs = 1,type=2,na.rm = TRUE)
    )
  
  
  ## Calculate Pooled SD
  summary_stats2_sd <- summary_stats2 %>%
    filter(!is.na(SD))
  
  pooled_sd <- with(summary_stats2_sd, sqrt(sum((n - 1) * SD^2) / sum(n - 1)))
  
  summary_stats2$Pooled_SD <- pooled_sd 
  
  return(summary_stats2)
  
}

