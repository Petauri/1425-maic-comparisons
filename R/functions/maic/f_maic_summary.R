f_maic_summary <- function(ild_df, match_characteristics, weighted, weights) {
  
  # Create a summary table based on the patterns
  ild_subset <- ild_df %>%
    dplyr::select(all_of(match_characteristics)) 
  
  # Add weights to df 
  
  ild_subset$weights <- weights
  
  # Create a tibble from the data frame
  ild_tibble <- as_tibble(ild_subset)
  # don't need weights here
  ild_tibble$weights <- NULL
  
  # Pivot the data frame into a longer format
  ild_long <- ild_tibble %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
  
  # Summarize the data
  
  if(weighted == TRUE) {
    
    # Summarize the data
    summary_df<- ild_long %>%
      group_by(Variable) %>%
      summarize(
        Median = if (any(grepl("median", Variable))) Hmisc::wtd.quantile(Value, probs = c(0.50), weights = weights) else NA_real_,
        Proportion = if (any(grepl("proportion", Variable))) Hmisc::wtd.mean(Value, weights = weights) * 100 else NA_real_,
        Mean = if (any(grepl("mean", Variable))) Hmisc::wtd.mean(Value, weights = weights) else NA_real_
      )
    
  } else {
    
    summary_df <- ild_long %>%
      group_by(Variable) %>%
      summarize(
        Median = if (any(grepl("median", Variable))) median(Value, na.rm = TRUE) else NA_real_,
        Proportion = if (any(grepl("proportion", Variable))) mean(Value, na.rm = TRUE) * 100 else NA_real_,
        Mean = if (any(grepl("mean", Variable))) mean(Value, na.rm = TRUE) else NA_real_
      )
    
  }
  
  # Transpose
  summary_wide_t <- as.data.frame(t(summary_df))
  # Make first row columnnames
  summary_wide_t <- summary_wide_t %>%
    row_to_names(row_number = 1)
  rownames(summary_wide_t) <- NULL
  
  # Pivot to long format
  df_long <- summary_wide_t %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
  
  # Filter out NA values
  df_filtered <- df_long %>% filter(!is.na(Value))
  
  # Pivot back to wide format with a single row
  df_final <- df_filtered %>%
    pivot_wider(names_from = Variable, values_from = Value)
  
  if(weighted == TRUE) {
    
    df_final <- df_final %>%
      dplyr::mutate(labelling_name = "ILD weighted") %>%
      relocate(labelling_name, .before = 1) %>%
      mutate(n_patients = nrow(ild_df)) %>%
      relocate(n_patients, .before = 2) %>%
      # ESS
      mutate(ESS = round2(ESS(ild_subset$weights), digits = 1)) %>%
      relocate(ESS, .before = 3) %>%
      # ESS %
      mutate("ESS (%)" = (ESS / n_patients) * 100) %>%
      relocate("ESS (%)", .before = 4)
    # mutate(n_patients = paste0(n_patients, " (", ESS, ")")) %>%
    # dplyr::select(-c("ESS"))
    
  } else {
    
    df_final <- df_final %>%
      dplyr::mutate(labelling_name = "ILD unweighted") %>%
      relocate(labelling_name, .before = 1) %>%
      mutate(n_patients = nrow(ild_df)) %>%
      relocate(n_patients, .before = 2) %>%
      # ESS
      mutate(ESS = NA) %>%
      relocate(ESS, .before = 3) %>%
      # ESS %
      mutate("ESS (%)" = NA) %>%
      relocate("ESS (%)", .before = 4)
    
  }
  
  return(df_final)
  
  suppressWarnings(rm(df_final, df_filtered, df_long, summary_df, summary_wide_t, ild_long, ild_tibble, ild_subset))
  
} # End of function 

