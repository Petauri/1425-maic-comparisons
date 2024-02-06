# Script aim; 
# The script 

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
source("R/functions/maic/f_maic_package.R")
source("R/functions/maic/f_maic_summary.R")
source("R/functions/maic/f_maic_pathway_figure.R")

# VERSION OF RESULTS

version <- "v0-1"
dir.create(file.path(results_folder, version, "1. maic package"),
           showWarnings = FALSE, recursive = TRUE)

#***********************************************************************
# Read data ---------------------------------------------------
#***********************************************************************
  
ild_dat <- read_rds(file.path("data", "analysis-derived", "dat_scenario_22_derived.Rda"))

# Import characteristics table 

ald_data <- openxlsx::readWorkbook(file.path("data", "ALD", "ITC_data_extraction.xlsx"), 
                                   sheet = 1, 
                                   rowNames = TRUE, 
                                   colNames = FALSE)

#***********************************************************************
# Preparing study characteritstics -------------------------------------
#***********************************************************************

# Transpose dataset so we have one study per row which is more intuitive to work with 

ald_data_t <- as.data.frame(t(ald_data))

# Clean up column names

ald_data_t <- clean_names(ald_data_t)

# Remove row names

rownames(ald_data_t) <- NULL

# Ensure numeric columns are numeric 

# Get strings for columns that should be numeric 
columns_to_convert <- grep("median|range|n_", names(ald_data_t))
# Convert identified columns to numeric
ald_data_t[, columns_to_convert] <- sapply(ald_data_t[, columns_to_convert], as.numeric)
# Check other columns that need to be numeric are numeric (year and study group?)
ald_data_t <- ald_data_t %>%
  mutate_at(c('year'), as.numeric)

# Calculate proportions 

# Get the column names starting with "n_" which means they are counts and therefore we need proportions for them to run MAIC
n_columns <- grep("^n_", names(ald_data_t), value = TRUE)

# Iterate through the "n_" columns and create new "proportion_" columns
for (n_col in n_columns) {
  # Create the new "proportion_" column
  new_col_name <- sub("^n_", "proportion_", n_col)
  ald_data_t[new_col_name] <- (ald_data_t[n_col] / ald_data_t$n_patients)
}

#***********************************************************************
# Run MAIC ---------------------------------------------------------
#***********************************************************************

# characteristics we want to show differences between (regardless of matching)

characteristic_vars <- c("median_characteristic_1",
                         "median_characteristic_2",
                         "median_characteristic_3",
                         "median_characteristic_4",
                         "median_characteristic_5",
                         "median_characteristic_6")

# Set match characteristics

match_maic_1 <- c("median_characteristic_1")

match_maic_2 <- c("median_characteristic_1", 
                  "median_characteristic_2")

match_maic_3 <- c("median_characteristic_1",
                  "median_characteristic_2",
                  "median_characteristic_3")

match_maic_4 <- c("median_characteristic_1",
                  "median_characteristic_2",
                  "median_characteristic_3",
                  "median_characteristic_4")

# match_maic_5 <- c("median_characteristic_1",
#                   "median_characteristic_2",
#                   "median_characteristic_3",
#                   "median_characteristic_4",
#                   "median_characteristic_5")
# 
# match_maic_6 <- c("median_characteristic_1",
#                   "median_characteristic_2",
#                   "median_characteristic_3",
#                   "median_characteristic_4",
#                   "median_characteristic_5",
#                   "median_characteristic_6")

# Get all variables in the environment that start with "match_"
match_vectors <- ls(pattern = "^match_maic")

# Combine into a list
matches_list <- mget(match_vectors)

# Run  MAIC function with custom inputs

# Initialize an empty list to store results
results_list <- list()

# Loop through matches_list
for (i in seq_along(matches_list)) {
  # Call the function with the current matching_vars and match_no
  result <- f_maic_package(
    ild_dat = ild_dat,
    ald_dat = ald_data_t,
    matching_vars = matches_list[[i]],
    characteristic_vars = characteristic_vars,
    comparator_drug = "Treatment X",
    match_no = i
  )
  
  # Add the match_no to the result
  result$match_no <- i
  
  # Modify the "matching_vars" in the result to a comma-separated string
  result$matching_vars <- toString(result$matching_vars)
  
  # Add the result to the list
  results_list[[i]] <- result
}

# Compile the results into a dataframe
result_df <- do.call(rbind, lapply(results_list, function(x) data.frame(x)))

# Save the dataframe to an Excel file

write.xlsx(result_df,
           file.path(results_folder,
                     version, 
                     "1. maic package",
                     "maic_summary_run.xlsx"))

#***********************************************************************
# GENERATE SUMMARY MAIC FIGURE -------------------------------------------
#***********************************************************************

# Inputs needed for function 

directory_path <- file.path(results_folder, version, "1. maic package")
label_name <- ald_data_t$labelling_name

f_maic_pathway_figure(directory_name = directory_name, directory_path = directory_path, label_name = label_name)
# END

