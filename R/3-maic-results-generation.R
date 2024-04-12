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
source("R/functions/maic/f_maic_package.R")
source("R/functions/maic/f_maic_summary.R")
source("R/functions/maic/f_maic_pathway_figure.R")
source("R/functions/maic/f_multi_maic_package.R")

# VERSION OF RESULTS

version <- "v0-3"
dir.create(file.path(results_folder, version),
           showWarnings = FALSE, recursive = TRUE)

#***********************************************************************
# Read data ---------------------------------------------------
#***********************************************************************

