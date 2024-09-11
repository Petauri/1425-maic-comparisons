# Script aim; 
# The script 

#***********************************************************************
# Libraries ---------------------------------------------------------------
#***********************************************************************

# Roche 

# devtools::install_github(
#   "roche/MAIC",
#   ref = "main"
# )
# 
# # Maicplus
# 
# devtools::install_github(
#   "hta-pharma/maicplus",
#   ref = "main"
# )

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

version <- "v0-7"

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
  # Add a new column with the package name
  maic_results[[package]]$package <- package
  
}


# Iterate over maic packages and read in the Excel files
# for (i in seq_along(maic_packages)) {
#   pkg <- maic_packages[i]
#   file_path <- file.path(results_folder, version, pkg, paste0(pkg, "_outcome_summary.xlsx"))
#   df <- read.xlsx(file_path)
#   # Add a column with the package name
#   df$package <- pkg
#   maic_results[[i]] <- df
# }
# 
# # Combine the dataframes into one dataframe
# combined_df <- do.call(rbind, maic_results)
# combined_df <- clean_names(combined_df)

#*************************************************************
# FOREST PLOT ------------------------------------------------
#*************************************************************

# Filter the data for each outcome
untreated_data <- maic_results$maic %>%
  filter(outcome == "Untreated median survival (months)")

intervention_data <- maic_results$maic %>%
  filter(outcome == "Intervention median survival (months)")

# Combine data for both outcomes
combined_data <- bind_rows(
  untreated_data %>% mutate(outcome_label = "Untreated"),
  intervention_data %>% mutate(outcome_label = "Intervention")
)

# Define a function to create the forest plot without individual titles
create_forest_plot <- function(data) {
  
  # Reverse the levels of the 'match' factor to ensure Match 1 is on top
  data$match <- factor(data$match, levels = rev(unique(data$match)))
  
  # Prepare the data in long format
  data_long <- data %>%
    gather(key = "type", value = "estimate", 
           unweighted_estimate_population_a, weighted_estimate_population_a, comparator_estimate_population_b) %>%
    mutate(lower = case_when(
      type == "unweighted_estimate_population_a" ~ unweighted_lower_95_percent_ci,
      type == "weighted_estimate_population_a" ~ weighted_lower_95_percent_ci,
      type == "comparator_estimate_population_b" ~ comparator_lower_95_percent_ci
    ),
    upper = case_when(
      type == "unweighted_estimate_population_a" ~ unweighted_upper_95_percent_ci,
      type == "weighted_estimate_population_a" ~ weighted_upper_95_percent_ci,
      type == "comparator_estimate_population_b" ~ comparator_upper_95_percent_ci
    ),
    type = factor(type, levels = c("unweighted_estimate_population_a", 
                                   "weighted_estimate_population_a", 
                                   "comparator_estimate_population_b"),
                  labels = c("Unweighted", "Weighted", "Comparator"))
    )
  
  # Define position dodge to stagger points
  pd <- position_dodge(width = 0.5)
  
  # Create the forest plot
  ggplot(data_long, aes(x = estimate, y = match, color = type)) +
    geom_point(position = pd, size = 3) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, position = pd) +
    scale_color_manual(values = c("blue", "red", "green")) +
    labs(x = "Estimate (Months)", y = "Match") +
    theme_minimal() +
    theme(legend.title = element_blank(), 
          plot.title = element_blank(), # Remove individual titles
          strip.text = element_text(size = 12)) # Adjust facet labels size
}

# Generate the multipanel plot
combined_plot <- create_forest_plot(combined_data) +
  facet_wrap(~ outcome_label, scales = "free_y", ncol = 2)

# Display the combined plot
print(combined_plot)
