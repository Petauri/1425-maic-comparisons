# Script aim; 
# The script 

#***********************************************************************
# Libraries ---------------------------------------------------------------
#***********************************************************************

# Roche 

devtools::install_github(
  "roche/MAIC",
  ref = "main"
)

# Maicplus

devtools::install_github(
  "hta-pharma/maicplus",
  ref = "main"
)

library(MAIC)
library(maicplus)

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
  ggsurvfit,
  readxl,
  gt
)

#***********************************************************************
# Directories --------------------------------------------------------------
#***********************************************************************

# results folder - 1425 maic comparisons - project work - results 
if (!exists("results_folder")) {results_folder <- rstudioapi::selectDirectory()}

#***********************************************************************
# Delta Hat options and functions -----------------------------------------
#***********************************************************************

options(scipen = 999) # Remove scientific notation
#rewrite table to use 'ifany' as automatic
table <- function(..., useNA = 'ifany') base::table(..., useNA = useNA)
s_daysinmonth <- 365.25/12 # days to month and vice versa 

# SOURCE FUNCTIONS

source("R/functions/misc/rounding/f_func_misc_rounding.R")
source("R/functions/maic/f_maic_summary.R")
source("R/functions/maic/f_maic_pathway_figure.R")
source("R/functions/maic/f_multi_maic_package.R")

# VERSION OF RESULTS

version <- "v0-5"

#***********************************************************************
# Read data ---------------------------------------------------
#***********************************************************************

# Iterate over maic packages 

maic_packages <- c(
  "maic", 
  "MAIC_roche", 
  "Maicplus",
  "maicChecks"
)

# maic 

# Create an empty list to store the dataframes
maic_results <- list()

# Iterate over maic packages and read in the Excel files
for (package in maic_packages) {
  
  file_path <- file.path(
    results_folder, 
    version, 
    package, 
    paste0(package, "_outcome_summary.xlsx"))
  
  maic_results[[package]] <- read.xlsx(file_path)
  
  # Add a column with the package name
  df$package <- pkg
  maic_data[[pkg]] <- df
  
}


# Iterate over maic packages and read in the Excel files
for (i in seq_along(maic_packages)) {
  pkg <- maic_packages[i]
  file_path <- file.path(results_folder, version, pkg, paste0(pkg, "_outcome_summary.xlsx"))
  df <- read.xlsx(file_path)
  # Add a column with the package name
  df$package <- pkg
  maic_results[[i]] <- df
}

# Combine the dataframes into one dataframe
combined_df <- do.call(rbind, maic_results)
combined_df <- clean_names(combined_df)

### NEED TO CHANGE THE RESULTS SO THAT ESTIMATES AND CIs ARE JUST NUMERICAL COLUMNS 