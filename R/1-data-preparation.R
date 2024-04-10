### MAIC comparisons ###
# Script aim; to prepare datasets ready for MAIC analyses 

#***********************************************************************
# Libraries ---------------------------------------------------------------
#***********************************************************************

pacman::p_load(
  haven, 
  dplyr, 
  tidyverse, 
  readr,
  cowplot, 
  ggplot2, 
  zoo,
  janitor,
  flexsurv,
  biostat3, 
  bshazard,
  docstring,
  crayon, 
  maic,
  openxlsx,
  WeightIt, 
  survminer,
  purrr,
  janitor,
  ggsurvfit
)

#***********************************************************************
# Delta Hat options and functions -----------------------------------------
#***********************************************************************

options(scipen = 999) # Remove scientific notation
#rewrite table to use 'ifany' as automatic
table <- function(..., useNA = 'ifany') base::table(..., useNA = useNA)
s_daysinmonth <- 365.25/12 # days to month and vice versa 

# SOURCE FUNCTIONS


#***********************************************************************
# Read data and format -------------------------------------------------
#***********************************************************************
# We need: Scenario 22 - 2 high importance, 2 low importance 

dat_s_22 <- read.csv(file.path("data", "ILD", "Hatswell-2020-VIH-Simulation", 
                               "scenarios", "Scenario22- 2 high imp, 2 low im.csv")) %>%
  janitor::row_to_names(row_number = 1) %>%
  janitor::clean_names() %>%
  dplyr::rename("pid" = "x1") %>%
  dplyr::mutate_if(is.character, as.numeric)

# Create a couple of factor variables that we can use to match on

dat_s_22 <- dat_s_22 %>%
  dplyr::mutate(intervention_characteristic_1_bin = ntile(intervention_characteristic_1, 2)) %>%
  dplyr::mutate(intervention_characteristic_1_bin = ifelse(intervention_characteristic_1_bin == 2, 1, 0)) %>%
  dplyr::mutate(intervention_characteristic_2_bin = ntile(intervention_characteristic_2, 2)) %>%
  dplyr::mutate(intervention_characteristic_2_bin = ifelse(intervention_characteristic_2_bin == 2, 1, 0)) %>%
  # Do the same for control 
  dplyr::mutate(control_characteristic_1_bin = ifelse(control_characteristic_1 > median(intervention_characteristic_1), 1, 0)) %>%
  dplyr::mutate(control_characteristic_2_bin = ifelse(control_characteristic_2 > median(intervention_characteristic_2), 1, 0))
  
#***********************************************************************
# Inspect data ---------------------------------------------------------
#***********************************************************************

# Plot histogram summary so we can quickly visualise the dataset 
num_cols <- ncol(dat_s_22)

# Set up the layout for the plots
par(mfrow = c(ceiling(sqrt(num_cols)), ceiling(sqrt(num_cols))))

# Loop through each column and plot histogram
for (i in 1:num_cols) {
  hist(dat_s_22[,i], main = paste("Histogram of", names(dat_s_22)[i]))
}

#***********************************************************************
# Summarise ILD data ---------------------------------------------------
#***********************************************************************

# multiply all characteristics by 100 for easier interpretation 
# dat_s_22 <- dat_s_22 %>%
#   dplyr::mutate(across(matches("characteristic_[0-9]+"), ~ .*100))

dat_s_22 %>%
summary() 

# Remove ALD variables now as we technically don't have these in the data!

dat_s_22 <- dat_s_22 %>%
  dplyr::select(-contains("control_"))

# Replace "intervention" with "median_intervention" in column names
# to match the variables that are fed from the extraction sheet

# Do proportions first
dat_s_22 <- dat_s_22 %>%
  dplyr::mutate(proportion_characteristic_1_yes = ifelse(intervention_characteristic_1_bin == 1, 1, 0)) %>%
  dplyr::mutate(proportion_characteristic_2_yes = ifelse(intervention_characteristic_2_bin == 1, 1, 0))

# means and medians
dat_s_22 <- dat_s_22 %>%
  dplyr::mutate(mean_characteristic_1 = intervention_characteristic_1,
                mean_characteristic_2 = intervention_characteristic_2,
                mean_characteristic_3 = intervention_characteristic_3,
                mean_characteristic_4 = intervention_characteristic_4,
                mean_characteristic_5 = intervention_characteristic_5,
                mean_characteristic_6 = intervention_characteristic_6,
                median_characteristic_1 = intervention_characteristic_1,
                median_characteristic_2 = intervention_characteristic_2,
                median_characteristic_3 = intervention_characteristic_3,
                median_characteristic_4 = intervention_characteristic_4,
                median_characteristic_5 = intervention_characteristic_5,
                median_characteristic_6 = intervention_characteristic_6)

#***********************************************************************
# Save data ------------------------------------------------------------
#***********************************************************************

write_rds(dat_s_22, file.path("data", "analysis-derived" , "dat_scenario_22_derived.Rda"))
