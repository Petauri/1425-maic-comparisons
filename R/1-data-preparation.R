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
# Directories --------------------------------------------------------------
#***********************************************************************

# results folder - 1336 Jazz 2L BTC and 1L GEA\3. Project work\2. BTC Tasks\3. HTA stats and CEM\3. R outputs

# if (!exists("ild_folder")) {ild_folder <- rstudioapi::selectDirectory()}
# if (!exists("base_folder")) {base_folder <- rstudioapi::selectDirectory()}
# if (!exists("results_folder")) {results_folder <- rstudioapi::selectDirectory()}
  

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
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  dplyr::rename("pid" = "x1") %>%
  dplyr::mutate_if(is.character, as.numeric)

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

dat_s_22 <- dat_s_22 %>%
  dplyr::mutate(across(matches("characteristic_[0-9]+"), ~ .*100))

dat_s_22 %>%
summary(across(matches("characteristic_[0-9]+"), median))
