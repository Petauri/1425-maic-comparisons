# Roche 

devtools::install_github(
  "roche/MAIC",
  ref = "main"
)


#***********************************************************************
# Get MAIC weights ---------------------------------------------------------
#***********************************************************************

ild_dat_roche <- ild_dat %>%
  # Make centered variable like in the vignette 
  mutate(mean_characteristic_1_centered = mean_characteristic_1 - ald_data_t$mean_characteristic_1)

# estimate weights 
roche_weights <- MAIC::estimate_weights(
  intervention_data = ild_dat_roche,
  matching_vars = "mean_characteristic_1_centered"
)

# Get ESS using in built function
ESS <- MAIC::estimate_ess(roche_weights$analysis_data)
# Get ESS using WeightI as before 
ESS_other <-  round2(WeightIt::ESS(roche_weights$analysis_data$wt), digits = 1)

weights_from_maic <- roche_weights$analysis_data$wt

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

unweighted_summary <- f_maic_summary(ild_df = ild_dat_roche, match_characteristics = "mean_characteristic_1_centered", weighted = FALSE, weights = FALSE)
weighted_summary <- f_maic_summary(ild_df = ild_dat_roche, match_characteristics = "mean_characteristic_1_centered", weighted = TRUE, weights = weights_from_maic)

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

