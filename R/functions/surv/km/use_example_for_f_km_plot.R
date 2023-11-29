# Example using Delta Hat Kaplan Meir function - assumed function is already loaded

library(survival)
library(tidyverse)

# load functions - assuming one is using the R project for relative file paths 

verbose <- FALSE
source("./R/dh_standard/dh_master.R")
rm(verbose)

s_daysinmonth <- 365.25/12 # days to month and vice versa 

dat <- survival::lung

# Change time to months 

dat$time_months <- dat$time/s_daysinmonth

table(dat$sex)
# Recode sex to character 
dat <- dat %>% 
  mutate(sex_c = ifelse(sex == 1, "Male",
                        ifelse(sex == 2, "Female", NA)))

table(dat$sex_c)

# Function inputs 

km_legend_labs <- c("Overall", "Male", "Female")
km_cols <- c("black", "blue", "red")
km_save <- "C:/Users/Kurt/Documents/lungtest.png" # File path and name 

# Fun function 

f_km_function(timevar = "time_months", eventvar = "status", stratify = "sex_c", df = dat, surv_measure = "OS", 
              km_cols, km_legend_labs, time_int = 2, km_save, save_plot_data = FALSE)

