# Roche 

ild_dat_roche <- ild_dat %>%
  # Make centered variable like in the vignette 
  mutate(median_characteristic_1_centered = median_characteristic_1 - ald_data_t$median_characteristic_1)

# estimate weights 
roche_weights <- MAIC::estimate_weights(intervention_data = ild_dat_roche,
                                matching_vars = "median_characteristic_1_centered")

# Get ESS using in built function
ESS <- MAIC::estimate_ess(roch_test)
# Get ESS using WeightI as before 
ESS_other <-  round2(WeightIt::ESS(roch_test$wt), digits = 1)
