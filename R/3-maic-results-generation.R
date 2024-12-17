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

version <- "v0-9"

#***********************************************************************
# Read data ---------------------------------------------------
#***********************************************************************

# Iterate over maic packages 

maic_packages <- c(
  "maic", 
  "MAIC_roche", 
  "Maicplus",
  "maicChecks",
  "maicChecks_alternateWT"
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

# Iterate over each package
combined_data_list <- map(maic_packages, function(pkg) {
  
  # Dynamically refer to the package's data frame in maic_results
  untreated_data <- maic_results[[pkg]] %>%
    filter(outcome == "Untreated median survival (months)")
  
  intervention_data <- maic_results[[pkg]] %>%
    filter(outcome == "Intervention median survival (months)")
  
  # Combine data for both outcomes
  combined_data <- bind_rows(
    untreated_data %>% mutate(outcome_label = "Untreated"),
    intervention_data %>% mutate(outcome_label = "Intervention")
  )
  
  return(combined_data)
})

# combine all package data into a single data frame
all_combined_data <- bind_rows(combined_data_list)

# Formatting
all_combined_data <- all_combined_data %>% 
mutate(across(everything(), ~ str_replace_all(., "MAIC_roche", "MAIC (Roche)"))) %>%
  mutate(across(everything(), ~ str_replace_all(., "Maicplus", "maicplus"))) %>%
  mutate(across(everything(), ~ str_replace_all(., "maicChecks_alternateWT", "maicChecks (alternate weights)")))
  
# Define a function to create the forest plot without individual titles
create_forest_plot <- function(data) {
  
  # Reverse the levels of the 'match' factor to ensure Match 1 is on top
  data$match <- factor(data$match, levels = rev(unique(data$match)))
  data$package <- factor(data$package, levels = rev(unique(data$package)))
  
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
    ) %>% 
    # only show intervention results
    dplyr::filter(outcome == "Intervention median survival (months)") %>%
    # Only show weighted
    dplyr::filter(type == "Weighted") %>% 
    mutate(across(c(estimate, lower, upper, unweighted_lower_95_percent_ci, unweighted_upper_95_percent_ci,
                    weighted_lower_95_percent_ci, weighted_upper_95_percent_ci, comparator_lower_95_percent_ci,
                    comparator_upper_95_percent_ci, ess_value), ~ as.numeric(as.character(.))))
  
  # Define position dodge to stagger points
  pd <- position_dodge(width = 0.8)
  
  # Create the forest plot
  ggplot(data_long, aes(x = estimate, y = match, color = package)) +
    geom_point(position = pd, size = 3) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, position = pd) +
    scale_color_manual(values = c("grey", "black", "slateblue4", "tomato1", "tomato4")) +
    labs(x = "Estimate", y = "Match", color = "R Package") +
    theme(legend.title = element_blank(), 
          plot.title = element_blank(), # Remove individual titles
          strip.text = element_text(size = 12),
          element_text(face = "bold")) + # Adjust facet labels size
    geom_text(
      aes(
        x = 18,
        group = package,
        label = sprintf(
          "%0.2f (%0.2f, %0.2f)",
          estimate, lower, upper
        )
      ),
      hjust = 0, vjust = 0.5, size = 4, color = "black",
      position = position_dodge(width = 0.8)
    ) +
    geom_text(
      aes(
        x = 19.5,
        group = package,
        label = paste0("ESS = ", round2(ess_value, digits = 0))
      ),
      hjust = 0, vjust = 0.5, size = 4, color = "black",
      position = position_dodge(width = 0.8)
    ) +
    guides(color = guide_legend(reverse=TRUE), shape = guide_legend(reverse=TRUE)) +
    xlim(12.5, 20) +
    theme_minimal(base_size = 20)
}

fplot <- create_forest_plot(all_combined_data)

# save plot
dir.create(file.path(results_folder, version, "comparison_figures"),
           showWarnings = FALSE)

png(file.path(results_folder, version, "comparison_figures",
              paste0("forest_weighted_outcomes_compare.png")),
    units = "mm", width=400, height=230, res = 800)
print(fplot)
dev.off() 
