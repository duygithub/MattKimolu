# function for display multipe model summary into a dataframe
modelSummary2df <- function(model_summary_list) {
  
  
  sub_list_name<-unique(names(model_summary_list))
  
  model_summary_df <- data.frame()
  
  ## Test:
  i <- "model_v1"
  
  
  for (i in sub_list_name) {
    
    
    combined_model_summary<-data.frame()
    
    ## Test:
    # i <- "model_v1"
    
    ###################################################
    ## process model summary into a dataframe
    
    model <- model_summary_list[[i]]
    rhs_name <- i # deparse(substitute(model_v1))
    
    
    coef_df <- tidy(model) %>%
      mutate(
        stars = case_when(
          p.value < 0.01 ~ "***",
          p.value < 0.05 ~ "**",
          p.value < 0.1  ~ "*",
          TRUE ~ ""
        )
      )
    
    # Convert by to seconds:
    # coef_df2 <- coef_df %>%
    #   mutate(across(c("estimate" , "std.error"), ~ .x / 1000))
    #
    ## Round up
    coef_df2 <- coef_df 
    coef_df3 <- coef_df2  %>%
      mutate(across(c("estimate" , "std.error"), ~ round(.x, 0)))
    
    
    coef_df4 <- coef_df3 %>%
      dplyr::select(term , estimate, std.error, statistic,  p.value, stars) %>%
      mutate(estimate = as.character(estimate),
             term = as.character(term),
             std.error = as.character(std.error),
             stars = as.character(stars)) %>%
      mutate(estimate= paste0(estimate, stars),
             # estimate = paste0(estimate, "\n(", std.error, ")")
             )%>%
      dplyr::select(term , estimate)
    
    
    
    ###########################################
    ## Process Model summary
    model_df <- glance(model)
    
    model_stats_long <- model_df %>%
      dplyr::select(nobs, r.squared, adj.r.squared, sigma, statistic, p.value) %>%
      tidyr::pivot_longer(everything(), names_to = "term", values_to = "estimate") %>%
      mutate(estimate = round(estimate, 4)) %>%
      mutate(estimate = as.character(estimate),
             estimate = ifelse(estimate == "0", "<.0001", estimate))
    

    
    ##########################################
    ## Combine them:
    coef_df_final <- coef_df4 
    
    # names(coef_df_final)[names(coef_df_final) == "estimate"] <- rhs_name #"model"
    
    model_stats_long_final <- model_stats_long %>%
      mutate(estimate = as.character(estimate))
    
    
    combined_model_summary <- bind_rows(coef_df_final, 
                                        model_stats_long_final)
    
    combined_model_summary <- combined_model_summary %>%
      mutate(term = ifelse(term == "nobs",
                           "n", term))
    
    
    names(combined_model_summary)[names(combined_model_summary) == "estimate"] <- rhs_name #"model"
    
    if (nrow(model_summary_df) == 0) {
      
      model_summary_df <- combined_model_summary
    } else {
      model_summary_df  <- model_summary_df  %>%
        full_join(combined_model_summary, by = "term")
    }
    
    ## Sorting
    keep_last <- c("n", "r.squared", "adj.r.squared",
                   "sigma", "statistic", "p.value")
    

    model_summary_df <- model_summary_df %>%
      arrange(
        term %in% keep_last,
        if_else(term %in% keep_last, match(term, keep_last), 0L),
        term
      )
    
  }
  
  return(model_summary_df)
  
}



######################################################
## Testing:

model_summary_list<- rlang::set_names(
  list(model_v1, model_v2),
  c("model_v1", "model_v2")
)

test2 <- modelSummary2df(model_summary_list)