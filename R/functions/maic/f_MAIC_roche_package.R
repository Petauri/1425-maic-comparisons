# Roche 

ild_dat_roche <- ild_dat %>%
  mutate(median_characteristic_1_centered = median_characteristic_1 - ald_data_t$median_characteristic_1)
         
roche_weights <- MAIC::estimate_weights(intervention_data = ild_dat_roche,
                                matching_vars = "median_characteristic_1_centered")
roch_test <- roche_weights$analysis_data
# ESS using in built function
ESS <- MAIC::estimate_ess(roch_test)
# ESS using same method as before 

ESS_other <-  round2(WeightIt::ESS(roch_test$wt), digits = 1)
