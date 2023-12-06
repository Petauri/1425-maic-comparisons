# Libraries 

# pacman::p_load(
#   haven, 
#   dplyr, 
#   tidyverse, 
#   lubridate,
#   readr,
#   cowplot, 
#   ggplot2, 
#   zoo,
#   janitor,
#   flexsurv,
#   biostat3, 
#   bshazard,
#   docstring,
#   crayon, 
#   maic,
#   openxlsx,
#   WeightIt
# )

# DEFINE FUNCTION ---------------------------------------------------------

# Testing function

# ild_dat <- jazz_ild_dat
# ald_dat <- ald_data_t
# cohort <- "Overall"
# subgroup <- "All"
# matching_vars <- match_characteristics
# surv_measure <- "OS"
# comparator_drug <- "FOLFOX"

jazz_maic <- function(ild_dat, ald_dat, cohort, subgroup, matching_vars, comparator_drug, surv_measure) {
  
  #' jazz_maic v-0.1 - perform match adjusted indirect comparisons for the Jazz BTC Zani project. 
  #'
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
  #' comparator_drug: The name of the comparator drug for the MAIC run - to align with the ald_data sheet 
  #' 
  #' surv_measure: Choose between "OS" or "PFS" or "TOT" (time on treatment)
  #' 
  #'The function uses the "f_maic_summary" function which produces summary characteristic tables for weighted/unweighted/comparator data comparisons. 
  #'
  #' @details Author: Kurt Taylor, Delta Hat


  # Filter ILD data ---------------------------------------------------------

  # Cohort 1 
  
  if(cohort == "Cohort-1") {
    
    ild_dat <- ild_dat %>%
      dplyr::filter(cohort == "Cohort 1")
    
  }
  
  # IHC3+ subgroup 
  
  if(subgroup == "IHC3+") {
    
    ild_dat <- ild_dat %>%
      dplyr::filter(ihccnt == "3+")
    
  }
  
  # Gallbladder cancer 
  
  if(subgroup == "GBC") {
    
    ild_dat <- ild_dat %>%
      dplyr::filter(primdiag_subgroup  == "GBC")
    
  }
  
  # Cholangiocarcinoma 
  
  if(subgroup == "CC") {
    
    ild_dat <- ild_dat %>%
      dplyr::filter(primdiag_subgroup  == "CC")
    
  }
  
  # Change matching variables for specific comparators if necessary ------------------
  
  if ((comparator_drug == "T-DXd-Overall" | comparator_drug == "T-DXd-IHC3+")){
    
    matching_vars <- c(matching_vars, "proportion_her2_ihc_3")
    
  }
  
  if ((comparator_drug == "Trastuzumab-FOLFOX-2")){
    
    # Remove match on Asian region 
    
    matching_vars <- matching_vars[matching_vars != "proportion_region_asia"]
    
  }
  
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
  
  weights_from_maic <- if (sum(maic) == 0) {0} else {maic} #save weights to my results sheet, unless the sum is 0 i.e. it didn't converge
  
  # Remove objects that we no longer need to try and free up memory
  
  # Generate table of whether MAIC was complete or not 
  
  # Determine if Complete should be TRUE or FALSE
  complete <- sum(weights_from_maic) > 0
  
  # Get filing name 
  
  filing_name <- ald_dat %>% 
    dplyr::filter(comparator == comparator_drug)
  filing_name <- filing_name$filing_name
  
  suppressWarnings(rm(maic_dict_set, maic_input, maic))
  
  if(complete == TRUE) {
    
    #***********************************************************************
    # Generate summary characteristics table ---------------------------------------------------------
    #***********************************************************************
    
    # Get the comparator summary 
    
    comparator_summary <- ald_dat %>%
      dplyr::filter(comparator == comparator_drug) %>%
      dplyr::select('labelling_name', 'n_patients', any_of(matching_vars)) %>%
      # Proportion to %
      mutate(across(contains("proportion"), ~ . * 100)) %>%
      # ESS
      mutate(ESS = NA) %>%
      relocate(ESS, .before = 3)
    
    # Get unweighted and weighted ILD summary's using function
    
    unweighted_summary <- f_maic_summary(ild_df = ild_dat, match_characteristics = matching_vars, weighted = FALSE, weights = FALSE)
    weighted_summary <- f_maic_summary(ild_df = ild_dat, match_characteristics = matching_vars, weighted = TRUE, weights = weights_from_maic)
    
    # rbind to get them all in the same df 
    
    overall_summary <- do.call("rbind", list(comparator_summary, unweighted_summary, weighted_summary))
    
    overall_summary_w <- as.data.frame(t(overall_summary))
    
    overall_summary_w <- overall_summary_w %>%
      row_to_names(row_number = 1) %>%
      rownames_to_column("Characteristic") %>%
      # Match var ignoring n patients because we don't match on N 
      mutate(Matched = ifelse(Characteristic %in% c("n_patients", "ESS"), NA, !rowSums(is.na(.))))
    
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
    
    overall_summary_w <- round_df(overall_summary_w, digits = 1)
    
    # Add the results to the summary table
    # summary_table <- rbind(summary_table, data.frame(study_name = study, Complete = complete))
    
    # SAVE THE SUMMARY CHARACTERISTICS TABLE 
    
    dir.create(file.path(results_folder, version, "4. MAIC",
                         filing_name, paste0(cohort, "-", subgroup)), recursive = TRUE, showWarnings = FALSE)
    
    # Save
    openxlsx::write.xlsx(overall_summary_w, file.path(results_folder, version, "4. MAIC",
                                                      filing_name, paste0(cohort, "-", subgroup), "characteristic-comparison.xlsx"),
                         headerStyle = createStyle(textDecoration = "Bold"))
    
    # Free up memory
    
    suppressWarnings(rm(comparator_summary, weighted_summary, unweighted_summary, overall_summary, overall_summary_w))
    
    #***********************************************************************
    # Generate COX PH models -----------------------------------------------
    #***********************************************************************
    
    # Add weights from the MAIC to the ild dataframe 
    
    ild_dat$maic_weights <- weights_from_maic
    
    # Read in file which says what type of PFS data we have for each comparator (INV or IRC)
    
    comparator_pfs <- read_csv(file.path("ITC-comparators", 
                                         "Comparator-PFS-definitions.csv"), show_col_types = FALSE)
  
    # Filter down to comparator of interest 
    
    comparator_pfs <- comparator_pfs %>%
      dplyr::filter(Comparator == comparator_drug)
    
    # Create a combined dataset of the ild and the comparator data 
    
    if(surv_measure == "OS"){
      
      ild_dat_to_rbind <- ild_dat %>%
        dplyr::select(os_months, os_event, maic_weights) %>%
        dplyr::rename("time" = "os_months",
                      "event" = "os_event") %>%
        dplyr::mutate(ild_vs_comparator = 1)
      
      # Read in comparator
      
      comparator_surv_dat <- read_csv(file.path("ITC-comparators", 
                                                paste0(filing_name), "OS-data", paste0(comparator_drug, "-os-Tab4.csv")), show_col_types = FALSE)
        
    } else if (surv_measure == "PFS_inv") {
      
      ild_dat_to_rbind <- ild_dat %>%
        dplyr::select(pfsinv_months, pfsinv_event, maic_weights) %>%
        dplyr::rename("time" = "pfsinv_months",
                      "event" = "pfsinv_event") %>%
        dplyr::mutate(ild_vs_comparator = 1)
      
      # For ASC, we do not have PFS data, but still want to plot the weighted Zani PFS curves 
      
      if (comparator_drug != "ASC") {
        
        comparator_surv_dat <- read_csv(file.path("ITC-comparators", 
                                                  paste0(filing_name), "PFS-data", paste0(comparator_drug, "-pfs-Tab4.csv")), show_col_types = FALSE)
        
      }
      
    } else if (surv_measure == "PFS_irc") {
      
      ild_dat_to_rbind <- ild_dat %>%
        dplyr::select(pfsicr_months, pfsicr_event, maic_weights) %>%
        dplyr::rename("time" = "pfsicr_months",
                      "event" = "pfsicr_event") %>%
        dplyr::mutate(ild_vs_comparator = 1)
      
      # For ASC, we do not have PFS data, but still want to plot the weighted Zani PFS curves 
      
      if (comparator_drug != "ASC") {
        
        comparator_surv_dat <- read_csv(file.path("ITC-comparators",
                                                  paste0(filing_name), "PFS-data", paste0(comparator_drug, "-pfs-Tab4.csv")), show_col_types = FALSE)
        
      }
      
    } else if (surv_measure == "ToT") {
      
      ild_dat_to_rbind <- ild_dat %>%
        dplyr::select(tot_months, tot_event, maic_weights) %>%
        dplyr::rename("time" = "tot_months",
                      "event" = "tot_event") %>%
        dplyr::mutate(ild_vs_comparator = 1)
      
    }
    
    # We now want an IF statement to determine if we have comparator data 
    # For example, we have no ToT comparator data so we don't want to do certain analyses with that
    # Also in the case where we are running PFS_inv but the comparator only has PFS_irc, we won't have comparator data 
    # For ASC we do not have any PFS data 
    
    if(surv_measure == "ToT" | (comparator_drug == "ASC" & surv_measure != "OS") ) {
      
      cat(blue("Cox models not running because we do not have ToT data"))
      
      ild_ald_dat <- ild_dat_to_rbind
      
    } else {
      
      ald_dat_to_rbind <- comparator_surv_dat %>%
        dplyr::select(time, event) %>%
        dplyr::mutate(ild_vs_comparator = 0,
                      maic_weights = 1)
      
      ild_ald_dat <- rbind(ild_dat_to_rbind, ald_dat_to_rbind)
      
      # Cox model using unweighted data 
      
      unw_cox <- coxph(Surv(time, event) ~ ild_vs_comparator, data = ild_ald_dat)
      unw_cox_res <- broom::tidy(unw_cox, conf.int = TRUE, exponentiate = TRUE)
      unw_cox_res <- unw_cox_res %>%
        dplyr::mutate(model = "Unweighted",
                      robust.se = NA,
                      comparator = comparator_drug,
                      surv_param = surv_measure)
      
      # Cox model using ild weighted data 
      
      # need to remove those where the weight == 0 
      ild_ald_dat_w <- ild_ald_dat %>%
        dplyr::filter(maic_weights > 0)
      
      w_cox <- coxph(Surv(time, event) ~ ild_vs_comparator, data = ild_ald_dat_w, weights = maic_weights)
      w_cox_res <- broom::tidy(w_cox, conf.int = TRUE, exponentiate = TRUE)
      w_cox_res <- w_cox_res %>%
        dplyr::mutate(model = "Weighted",
                      comparator = comparator_drug,
                      surv_param = surv_measure)
      
      cox_combined_res <- rbind(unw_cox_res, w_cox_res)
      # cox_combined_res <- round_df(cox_combined_res, digits = 2)
      
      # re-order columns for modelling team
      
      cox_combined_res <- cox_combined_res %>%
        dplyr::select(term, estimate, conf.low, conf.high, std.error, statistic, p.value, model, robust.se, comparator, surv_param)
      
      # Save cox model results 
      
      openxlsx::write.xlsx(cox_combined_res, file.path(results_folder, version, "4. MAIC",
                                                       filing_name, paste0(cohort, "-", subgroup), paste0(surv_measure, "_cox-results.xlsx")),
                           overwrite = TRUE,
                           headerStyle = createStyle(textDecoration = "Bold"))
      
    }
    
    # Free up memory 
    
    suppressWarnings(rm(ild_dat_to_rbind, ald_dat_to_rbind, unw_cox_res, w_cox_res, cox_combined_res))
    
    #***********************************************************************
    # Kaplan-Meir plots -----------------------------------------------
    #***********************************************************************
    
    # We want to get a dataframe with ILD data unweighted (so weights all set to 1), ILD data weighted and ALD data 
    
    if(surv_measure == "OS"){
      
      ild_dat_to_add <- ild_dat %>%
        dplyr::select(os_months, os_event, maic_weights) %>%
        dplyr::rename("time" = "os_months",
                      "event" = "os_event") %>%
        dplyr::mutate(ild_vs_comparator = 2,
                      maic_weights = 1)
      
    } else if (surv_measure == "PFS_inv") {
      
      ild_dat_to_add <- ild_dat %>%
        dplyr::select(pfsinv_months, pfsinv_event, maic_weights) %>%
        dplyr::rename("time" = "pfsinv_months",
                      "event" = "pfsinv_event") %>%
        dplyr::mutate(ild_vs_comparator = 2,
                      maic_weights = 1)
      
    } else if (surv_measure == "PFS_irc") {
      
      ild_dat_to_add <- ild_dat %>%
        dplyr::select(pfsicr_months, pfsicr_event, maic_weights) %>%
        dplyr::rename("time" = "pfsicr_months",
                      "event" = "pfsicr_event") %>%
        dplyr::mutate(ild_vs_comparator = 2,
                      maic_weights = 1)
      
    } else if (surv_measure == "ToT") {
      
      ild_dat_to_add <- ild_dat %>%
        dplyr::select(tot_months, tot_event, maic_weights) %>%
        dplyr::rename("time" = "tot_months",
                      "event" = "tot_event") %>%
        dplyr::mutate(ild_vs_comparator = 2,
                      maic_weights = 1)
      
    }
    
    ild_ald_dat <- rbind(ild_ald_dat, ild_dat_to_add)
    
    suppressWarnings(rm(ild_dat_to_add))
    
    # Same IF statement again because we will only have Jazz data on some plots (weighted vs unweighted) and Jazz data plus comparator on other plots 
    
    if(surv_measure == "ToT" | (comparator_drug == "ASC" & surv_measure != "OS") ) {
      
      # Rename variables so appear neatly on plot 
      
      ild_ald_dat <- ild_ald_dat %>%
        dplyr::mutate(ild_vs_comparator = ifelse(ild_vs_comparator == 1, "Zanidatamab-Weighted",
                                                 ifelse(ild_vs_comparator == 2, "Zanidatamab-Unweighted", NA))) %>%
        dplyr::mutate(maic_weights = as.numeric(maic_weights))
      
      # tibble to df 
      
      ild_ald_dat <- as.data.frame(ild_ald_dat)
      
      km_save <- file.path(results_folder, version, "4. MAIC",
                           filing_name, paste0(cohort, "-", subgroup), paste0(surv_measure, "_km_plot.png"))
      
      km_legend_labs <- c("Zanidatamab-Unweighted", "Zanidatamab-Weighted")
      
      km_cols <- c("lightblue", "darkblue")
      
      
      f_km_function(timevar = "time", eventvar = "event", stratify = "ild_vs_comparator", maic = TRUE, maic_weights = "maic_weights", df = ild_ald_dat, surv_measure = surv_measure, 
                    km_cols = km_cols, km_legend_labs = km_legend_labs, time_int = 2, file_path_name = km_save, save_plot_data = TRUE)
      
    } else {
      
      # Rename variables so appear neatly on plot 
      
      ild_ald_dat <- ild_ald_dat %>%
        dplyr::mutate(ild_vs_comparator = ifelse(ild_vs_comparator == 0, paste0(comparator_drug),
                                                 ifelse(ild_vs_comparator == 1, "Zanidatamab-Weighted",
                                                        ifelse(ild_vs_comparator == 2, "Zanidatamab-Unweighted", NA)))) %>%
        dplyr::mutate(maic_weights = as.numeric(maic_weights))
      
      # tibble to df 
      
      ild_ald_dat <- as.data.frame(ild_ald_dat)
      
      km_save <- file.path(results_folder, version, "4. MAIC",
                           filing_name, paste0(cohort, "-", subgroup), paste0(surv_measure, "_km_plot.png"))
      
      km_legend_labs <- c("Zanidatamab-Unweighted", "Zanidatamab-Weighted", paste0(comparator_drug))
      km_legend_labs <- as.character(km_legend_labs)
      
      km_cols <- c("lightblue", "darkblue", "red")
      
      f_km_function(timevar = "time", eventvar = "event", stratify = "ild_vs_comparator", maic = TRUE, maic_weights = "maic_weights", df = ild_ald_dat, surv_measure = surv_measure, 
                    km_cols = km_cols, km_legend_labs = km_legend_labs, time_int = 2, file_path_name = km_save, save_plot_data = TRUE)  
      
    }
    
    #***********************************************************************
    # Survival models (coefficients / AIC / BIC)  --------------------------
    #***********************************************************************
    
    # SURVIVAL FORMULA - also used for splines
    
    survival_formula <- Surv(time, event) ~ 1
    
    # Read in the template that we want to fill in for the modelling team
      
    param_template <- read_csv(file.path("Templates", "PSM_simple_models_template_v0.2.csv"))
    param_template_spline <- read_csv(file.path("Templates", "PSM_spline_models_template_v0.1.csv"))
  
    # Fill template using function - separately
    
    ild_ald_dat_unw <- ild_ald_dat %>%
      dplyr::filter(ild_vs_comparator == "Zanidatamab-Unweighted")
    
    ild_ald_dat_w <- ild_ald_dat %>%
      dplyr::filter(ild_vs_comparator == "Zanidatamab-Weighted")
    
    # Simple models
    
    simple_model_params_unw <- f_model_extract(survival_formula = survival_formula, df = ild_ald_dat_unw, template = param_template,
                                               model_type = "simple", arms =  "one", maic_weights = FALSE)
    
    simple_model_params_w <- f_model_extract(survival_formula = survival_formula, df = ild_ald_dat_w, template = param_template,
                                             model_type = "simple", arms =  "one", maic_weights = weights_from_maic)
    
    # Spline models
    
    spline_model_params_unw <- f_model_extract(survival_formula = survival_formula, df = ild_ald_dat_unw, template = param_template_spline,
                                               model_type = "spline", arms =  "one", maic_weights = FALSE)
    
    spline_model_params_w <- f_model_extract(survival_formula = survival_formula, df = ild_ald_dat_w, template = param_template_spline,
                                             model_type = "spline", arms =  "one", maic_weights = "maic_weights")
    
    # ROUNDING
    
    simple_model_params_unw$AIC <- round2(simple_model_params_unw$AIC, digits=1)
    simple_model_params_unw$BIC <- round2(simple_model_params_unw$BIC, digits=1)
    
    simple_model_params_w$AIC <- round2(simple_model_params_w$AIC, digits=1)
    simple_model_params_w$BIC <- round2(simple_model_params_w$BIC, digits=1)
    
    spline_model_params_unw$AIC <- round2(spline_model_params_unw$AIC, digits=1)
    spline_model_params_unw$BIC <- round2(spline_model_params_unw$BIC, digits=1)
    
    spline_model_params_w$AIC <- round2(spline_model_params_w$AIC, digits=1)
    spline_model_params_w$BIC <- round2(spline_model_params_w$BIC, digits=1)
    
    # Free memory
    
    suppressWarnings(rm(ild_ald_dat_w, ild_ald_dat_unw))
    
    # Same IF statement again because we will only have Jazz data on some plots (weighted vs unweighted) and Jazz data plus comparator on other plots
    
    if(surv_measure == "ToT" | (comparator_drug == "ASC" & surv_measure != "OS") ) {
      
      cat(blue("Not adding comparator co-efficients and AIC/BICs because we do not have comparator ToT"))
      
      # SAVE TO EXCEL FILES (SEPARATE FOR SIMPLE AND ONE FOR SPLINE)
      
      openxlsx::write.xlsx(simple_model_params_unw, file.path(results_folder, version, "4. MAIC",
                                                       filing_name, paste0(cohort, "-", subgroup), paste0(surv_measure, "_unw_simp_mod.xlsx")),
                           overwrite = TRUE,
                           headerStyle = createStyle(textDecoration = "Bold"))
      
      openxlsx::write.xlsx(simple_model_params_w, file.path(results_folder, version, "4. MAIC",
                                                              filing_name, paste0(cohort, "-", subgroup), paste0(surv_measure, "_w_simp_mod.xlsx")),
                           overwrite = TRUE,
                           headerStyle = createStyle(textDecoration = "Bold"))
      
      # Spline
      
      openxlsx::write.xlsx(spline_model_params_unw, file.path(results_folder, version, "4. MAIC",
                                                              filing_name, paste0(cohort, "-", subgroup), paste0(surv_measure, "_unw_spl_mod.xlsx")),
                           overwrite = TRUE,
                           headerStyle = createStyle(textDecoration = "Bold"))
      
      openxlsx::write.xlsx(spline_model_params_w, file.path(results_folder, version, "4. MAIC",
                                                              filing_name, paste0(cohort, "-", subgroup), paste0(surv_measure, "_w_spl_mod.xlsx")),
                           overwrite = TRUE,
                           headerStyle = createStyle(textDecoration = "Bold"))

    } else {
      
      # Repeating what we did above for Zani, but now for the comparator to add to the results
      
      ild_ald_dat_comp <- ild_ald_dat %>%
        dplyr::filter(ild_vs_comparator == comparator_drug)
      
      
      simple_model_params_comp <- f_model_extract(survival_formula = survival_formula, df = ild_ald_dat_comp, template = param_template,
                                                  model_type = "simple", arms =  "one", maic_weights = FALSE)
      
      spline_model_params_comp <- f_model_extract(survival_formula = survival_formula, df = ild_ald_dat_comp, template = param_template_spline,
                                                  model_type = "spline", arms =  "one", maic_weights = FALSE)
      
      simple_model_params_comp$AIC <- round2(simple_model_params_comp$AIC, digits=1)
      simple_model_params_comp$BIC <- round2(simple_model_params_comp$BIC, digits=1)
      
      spline_model_params_comp$AIC <- round2(spline_model_params_comp$AIC, digits=1)
      spline_model_params_comp$BIC <- round2(spline_model_params_comp$BIC, digits=1)
      
      # SAVE TO EXCEL FILES (ONE FOR SIMPLE AND ONE FOR SPLINE)
      # Changing this to now save to separate Excel Workbooks which later get collated 
      
      comparator_drug <- as.character(comparator_drug)
      
      openxlsx::write.xlsx(simple_model_params_unw, file.path(results_folder, version, "4. MAIC",
                                                              filing_name, paste0(cohort, "-", subgroup), paste0(surv_measure, "_unw_simp_mod.xlsx")),
                           overwrite = TRUE,
                           headerStyle = createStyle(textDecoration = "Bold"))
      
      openxlsx::write.xlsx(simple_model_params_w, file.path(results_folder, version, "4. MAIC",
                                                            filing_name, paste0(cohort, "-", subgroup), paste0(surv_measure, "_w_simp_mod.xlsx")),
                           overwrite = TRUE,
                           headerStyle = createStyle(textDecoration = "Bold"))
      
      openxlsx::write.xlsx(simple_model_params_comp, file.path(results_folder, version, "4. MAIC",
                                                            filing_name, paste0(cohort, "-", subgroup), paste0(surv_measure, "_", comparator_drug ,"_simp_mod.xlsx")),
                           overwrite = TRUE,
                           headerStyle = createStyle(textDecoration = "Bold"))
      
      # Spline
      
      openxlsx::write.xlsx(spline_model_params_unw, file.path(results_folder, version, "4. MAIC",
                                                              filing_name, paste0(cohort, "-", subgroup), paste0(surv_measure, "_unw_spl_mod.xlsx")),
                           overwrite = TRUE,
                           headerStyle = createStyle(textDecoration = "Bold"))
      
      openxlsx::write.xlsx(spline_model_params_w, file.path(results_folder, version, "4. MAIC",
                                                            filing_name, paste0(cohort, "-", subgroup), paste0(surv_measure, "_w_spl_mod.xlsx")),
                           overwrite = TRUE,
                           headerStyle = createStyle(textDecoration = "Bold"))
      
      openxlsx::write.xlsx(spline_model_params_comp, file.path(results_folder, version, "4. MAIC",
                                                               filing_name, paste0(cohort, "-", subgroup), paste0(surv_measure, "_", comparator_drug ,"_spl_mod.xlsx")),
                           overwrite = TRUE,
                           headerStyle = createStyle(textDecoration = "Bold"))
      
    }
    
    cat(green(paste0(cohort, " ", subgroup, " ", comparator_drug, " ", surv_measure, " function run complete\n")))
    
  } else {
    
    print("MAIC not run, did not converge (weights = 0)")
    
  }
  
  #***********************************************************************
  # Return objects from function --------------------------
  #***********************************************************************
  # completed is a TRUE/FALSE value on whether the MAIC was successful on given run (determined by weights > 0)
  # filing name is the file name from the ald_data 
  
  return(list(completed = complete, file_name = filing_name))

} # End of jazz_maic function 