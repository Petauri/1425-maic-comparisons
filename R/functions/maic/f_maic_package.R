# DEFINE FUNCTION ---------------------------------------------------------

# Testing function

# ild_dat <- ild_dat
# ald_dat <- ald_data_t
# matching_vars <- match_maic_5
# comparator_drug <- "Treatment X"
# match_no <- 5

f_maic_package <- function(ild_dat, ald_dat, matching_vars, characteristic_vars, comparator_drug, match_no) {

  #' @description This function performs the MAIC analyses for the Jazz project. In its current form, the function is not directly reusable, but with further work and refinements, it could be made more so. 
  #' for example, there are file paths in the function at the moment only relevant to Jazz project. This is v0.1. Future versions can address these issues. 
  #' The function uses (and therefore depends on) various other functions such as the km plot function, all of which are stored within this repository. 
  #' 
  #' ild_dat: This is the Jazz ILD dataset which is created using the data build script.
  #' 
  #' ald_dat: This is the comparator data extracted for the MAIC. This has been pre-processed prior to entering this function (see the code in the BTC MAIC script)
  #' 
  #' cohort: Cohort of interest to run the MAIC on
  #' 
  #' subgroup: subgroup of interest to run the MAIC on
  #' 
  #' matching_vars: a vector of variable names that we want to match on for the MAIC - these need to align with variable names in the ild_data and ald_data 
  #' 
  #' characteristic_vars: variables that the user wishes to see in comparisons 
  #' This can differ to matching_vats. For example, if you only want to match on age but want to see the characteristics 
  #' for age, sex, weight, height you would put "age" in matching vars and "age", "sex", "weight", "height" in characteristic_vars 
  #' 
  #' comparator_drug: The name of the comparator drug for the MAIC run - to align with the ald_data sheet 
  #' 
  #' match_no: to distinguish between different runs of the MAICs to have different options for each
  #' 
  #'The function uses the "f_maic_summary" function which produces summary characteristic tables for weighted/unweighted/comparator data comparisons. 
  #'
  #' @details Author: Kurt Taylor, Delta Hat
  
  
  # Filter ILD data ---------------------------------------------------------
  
  #***********************************************************************
  # Get MAIC weights ---------------------------------------------------------
  #***********************************************************************
  
  # select characteristics to match on and filer to the comparator we are using
  
  maic_target <- ald_dat %>% 
    dplyr::filter(comparator == comparator_drug) %>%
    dplyr::select(all_of(matching_vars)) 
  
  maic_dict <- data.frame("match.id" = names(maic_target),
                          "target.variable" = matching_vars,
                          "index.variable" = matching_vars,
                          "match.type" = matching_vars,
                          stringsAsFactors = FALSE)
  
  match_types <- c("proportion", "mean", "median")
  
  # Create a regular expression pattern to capture the match type
  pattern <- paste0(".*(", paste(match_types, collapse = "|"), ").*")
  
  # Use str_replace to replace the entire string with the captured match type
  maic_dict$match.type <- str_replace(maic_dict$match.type, pattern, "\\1")
  
  # if anything isn't reported - given we are iterating through, get ready to omit from matching
  
  maic_valid<- as.data.frame(!is.na(maic_target)) 
  
  # Set items to FALSE to not match on them, some of which are conditional on if better data is available. 
  
  # Not needed for now but can add this code later as and when needed 
  
  # Subset the maic_target and maic_dict dataframes to only include VALID variables (i.e., variables we have data on)
  
  maic_valid <- as.data.frame(t(maic_valid))
  maic_valid$match.id <- rownames(maic_valid)
  maic_valid <- maic_valid %>%
    dplyr::rename("valid" = "V1")
  
  maic_dict_set <- maic_dict %>%
    left_join(maic_valid) %>%
    dplyr::filter(valid == TRUE)
  
  # Remove any that are NA (which means we don't have data and therefore cannot run MAIC)
  
  maic_target_set <- maic_target %>%
    select_if(~ !any(is.na(.)))
  
  # maic_target_set needs to be a list for the function - setting it to numeric as this is what class the example is in (see: ? createMAICInput )
  
  class(maic_target_set) <- "numeric"
  
  # Create MAIC input 
  
  maic_input <- createMAICInput(index = ild_dat, # Think about how to do this without explicitly calling the variables out ??
                                target = maic_target_set,
                                dictionary = maic_dict_set,
                                matching.variables = maic_dict_set$match.id)
  
  maic <- maicWeight(maic_input) #generates the weights
  
  weights_from_maic <- if (sum(maic) == 0) {NA} else {maic} #save weights to my results sheet, unless the sum is 0 i.e. it didn't converge
  
  # Remove objects that we no longer need to try and free up memory
  
  suppressWarnings(rm(maic_dict_set, maic_input, maic))
  
  #***********************************************************************
  # Generate summary characteristics table ---------------------------------------------------------
  #***********************************************************************
  
  # Get the comparator summary 
  
  comparator_summary <- ald_dat %>%
    dplyr::filter(comparator == comparator_drug) %>%
    dplyr::select('labelling_name', 'n_patients', any_of(characteristic_vars)) %>%
    # Proportion to %
    mutate(across(contains("proportion"), ~ . * 100)) %>%
    # ESS
    mutate(ESS = NA) %>%
    relocate(ESS, .before = 3) %>%
    # ESS %
    mutate("ESS (%)" = NA) %>%
    relocate("ESS (%)", .before = 4)
  
  # Get unweighted and weighted ILD summary's using function
  
  unweighted_summary <- f_maic_summary(ild_df = ild_dat, match_characteristics = characteristic_vars, weighted = FALSE, weights = FALSE)
  weighted_summary <- f_maic_summary(ild_df = ild_dat, match_characteristics = characteristic_vars, weighted = TRUE, weights = weights_from_maic)
  
  # rbind to get them all in the same df 
  
  overall_summary <- do.call("rbind", list(comparator_summary, unweighted_summary, weighted_summary))
  
  overall_summary_w <- as.data.frame(t(overall_summary))
  
  overall_summary_w <- overall_summary_w %>%
    row_to_names(row_number = 1) %>%
    rownames_to_column("Characteristic") %>%
    # Match var ignoring n patients because we don't match on N 
    mutate(Matched = ifelse(Characteristic %in% c("n_patients", "ESS", "ESS (%)"), NA, !rowSums(is.na(.))))
  
  # Change the TRUE / FALSES to be correct 
  
  overall_summary_w <- overall_summary_w %>% 
    dplyr::mutate(Matched = ifelse(Characteristic %in% matching_vars, TRUE, 
                                   ifelse(Characteristic == "n_patients", NA,
                                          ifelse(Characteristic == "ESS", NA,
                                                 ifelse(Characteristic == "ESS (%)", NA, FALSE)))))
  
  # Clean up the df 
  
  overall_summary_w <- overall_summary_w %>%
    mutate(
      Characteristic = Characteristic %>%
        str_replace_all("proportion", "percentage")  %>% 
        str_replace_all("_", " ") %>% 
        str_to_title() %>%
        str_replace_all("Ecog", "ECOG") %>%
        str_replace_all("Ess", "ESS")
    ) %>%
    mutate_if(
      !names(.) %in% c("Characteristic", "Matched"),
      list(~ as.numeric(.))
    )
  
  # Round 
  
  # overall_summary_w <- round_df(overall_summary_w, digits = 1)
  
  # Generate table of whether MAIC was complete or not 
  
  # Determine if Complete should be TRUE or FALSE
  complete <- sum(weights_from_maic) > 0
  
  # Add the results to the summary table
  # summary_table <- rbind(summary_table, data.frame(study_name = study, Complete = complete))
  
  #***********************************************************************
  # ANALYSIS -----------------------------------------------
  #***********************************************************************

  # Create new dataset to include weights and weighted outcomes
  ild_dat <- ild_dat %>%
    # Add each patients weight to the dataset
    mutate(weights = weights_from_maic) %>%
    mutate(w_intervention_outcome_pop_a_untreated = weights*intervention_outcome_pop_a_untreated,
           w_intervention_outcome_pop_a_with_intervention = weights*intervention_outcome_pop_a_with_intervention) 
  
  # Define the variables
  variables <- c("intervention_outcome_pop_a_untreated", 
                 "intervention_outcome_pop_a_with_intervention", 
                   "w_intervention_outcome_pop_a_untreated", 
                 "w_intervention_outcome_pop_a_with_intervention")
  
  # Function to calculate mean and confidence interval
  calculate_ci <- function(var) {
    mean_value <- mean(ild_dat[[var]], na.rm = TRUE)
    ci <- t.test(ild_dat[[var]], na.rm = TRUE)$conf.int
    return(data.frame(variable = var, mean = mean_value, ci_lower = ci[1], ci_upper = ci[2]))
  }
  
  # Apply the function to each variable and combine results
  results <- lapply(variables, calculate_ci) %>% bind_rows()
  
  # Get outcomes from ALD data to compare to 
  
  ald_outcomes <- openxlsx::readWorkbook(file.path("data", "ALD", "ITC_data_extraction.xlsx"), 
                                         sheet = 2, 
                                         rowNames = TRUE, 
                                         colNames = FALSE,
                                         startRow = 11)
  
  ald_outcomes_t <- as.data.frame(t(ald_outcomes))
  ald_outcomes_t <- clean_names(ald_outcomes_t)
  rownames(ald_outcomes_t) <- NULL
  ald_outcomes_t$notes <- NULL
  ald_outcomes_t <- ald_outcomes_t %>%
    mutate_if(is.character,as.numeric)
  
  # tidy and format results 
  
  # Create data frame with summary of outcomes for weighted, unweighted and comparator group
  outcome_names <- c("Treated median survival (months)", 
                     "Intervention median survival (months)")
  
  unweighted_outcomes <- c(paste0(round(results$mean[results$variable=="intervention_outcome_pop_a_untreated"], digits = 3), " (95% CI: ",
                                  round(results$ci_lower[results$variable=="intervention_outcome_pop_a_untreated"], digits = 3), ", ",
                                  round(results$ci_upper[results$variable=="intervention_outcome_pop_a_untreated"], digits = 3), ")"
                                  ),
                           
                           paste0(round(results$mean[results$variable=="intervention_outcome_pop_a_with_intervention"], digits = 3), " (95% CI: ",
                                  round(results$ci_lower[results$variable=="intervention_outcome_pop_a_with_intervention"], digits = 3), ", ",
                                  round(results$ci_upper[results$variable=="intervention_outcome_pop_a_with_intervention"], digits = 3), ")"
                                  )
                           )
  
  
  weighted_outcomes <- c(paste0(round(results$mean[results$variable=="w_intervention_outcome_pop_a_untreated"], digits = 3), " (95% CI: ",
                                round(results$ci_lower[results$variable=="w_intervention_outcome_pop_a_untreated"], digits = 3), ", ",
                                round(results$ci_upper[results$variable=="w_intervention_outcome_pop_a_untreated"], digits = 3), ")"
                                ),
                         paste0(round(results$mean[results$variable=="w_intervention_outcome_pop_a_with_intervention"], digits = 3), " (95% CI: ",
                                round(results$ci_lower[results$variable=="w_intervention_outcome_pop_a_with_intervention"], digits = 3), ", ",
                                round(results$ci_upper[results$variable=="w_intervention_outcome_pop_a_with_intervention"], digits = 3), ")"
                                )
                         )
  
  ald_outcomes <- c(paste0(round(ald_outcomes_t$mean_outcome_untreated, digits = 3), " (95% CI: ",
                                round(ald_outcomes_t$untreated_ci_lower, digits = 3), ", ",
                                round(ald_outcomes_t$untreated_ci_upper, digits = 3), ")"
                           ),
                    paste0(round(ald_outcomes_t$mean_outcome_intervention, digits = 3), " (95% CI: ",
                           round(ald_outcomes_t$intervention_ci_lower, digits = 3), ", ",
                           round(ald_outcomes_t$intervention_ci_upper, digits = 3), ")"
                           )
                    )
  
  outcome_summary <- data.frame(outcome_names, unweighted_outcomes, weighted_outcomes, ald_outcomes)
  colnames(outcome_summary) <- c("Outcome", "Unweighted (Population A)", "Weighted (Population A)", "Comparator (Population B)")
  outcome_summary$Match <- paste0("Match ", match_no)
  
  #***********************************************************************
  # SAVE RESULTS -----------------------------------------------
  #***********************************************************************
  
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = "Characteristics")
  addWorksheet(wb, sheetName = "Outcomes")
  writeDataTable(wb, sheet = "Characteristics", x = overall_summary_w, startCol = 1, startRow = 1)
  writeDataTable(wb, sheet = "Outcomes", x = outcome_summary, startCol = 1, startRow = 1)
  saveWorkbook(wb, file = file.path(results_folder,
                                    version, 
                                    "1. maic package",
                                    paste0("match-", match_no, ".xlsx")),
               overwrite = TRUE)
  
  #***********************************************************************
  # Return objects from function --------------------------
  #***********************************************************************
  # completed is a TRUE/FALSE value on whether the MAIC was successful on given run (determined by weights > 0)
  # filing name is the file name from the ald_data 
  
  return(list(completed = complete, matching_vars = matching_vars, outcome_summary = outcome_summary))

} # End of function 
