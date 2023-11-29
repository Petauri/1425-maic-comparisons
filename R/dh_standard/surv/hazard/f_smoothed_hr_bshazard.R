## Load libraries

library(tidyverse)
library(survival)
library(survminer)
library(muhaz)
library(ggplot2)
library(muhaz)
library(bshazard)

f_smoothed_hr_bshazard <- function(df, timevar, eventvar, stratify, legendlabel) {
  
  df$time <- df[[timevar]]
  df$event <- df[[eventvar]]
  
  ## Create panel data...
  
  all_times <- data.frame(end = unique(sort(df$time)))
  
  df <- df %>%
    mutate(id = row_number()) %>%
    merge(all_times) %>%
    filter(time >= end) %>%
    mutate(event = ifelse(time > end, 0, event)) %>%
    arrange(id, end) %>%
    group_by(id) %>%
    mutate(start = ifelse(is.na(lag(end)), 0.0, lag(end))) %>%
    ungroup() %>%
    filter(start != end)
  
  ## Subset data
  
  dat1 <- df %>%
    filter(arm == stratify[1])
  
  dat2 <- df %>%
    filter(arm == stratify[2])
  
  ## bshazard fits
  
  haz1 <- bshazard(Surv(start, end, event) ~ 1, dat1, lambda = 100)
  haz2 <- bshazard(Surv(start, end, event) ~ 1, dat2, lambda = 100)
  
  ## Create hazard ratio dataset
  
  haz1_dat <- as.data.frame(summary(haz1)$HazardEstimates) %>%
    transmute(time,
              loghaz_1 = log(hazard),
              seloghaz_1 = (log(upper) - log(lower)) / (2 * 1.96))
  haz2_dat <- as.data.frame(summary(haz2)$HazardEstimates) %>%
    transmute(time,
              loghaz_2 = log(hazard),
              seloghaz_2 = (log(upper) - log(lower)) / (2 * 1.96))
  
  
  hr_dat <- haz1_dat %>%
    full_join(haz2_dat,
              by = "time") %>%
    mutate(loghr_2 = loghaz_2 - loghaz_1,
           seloghr_2 = sqrt(seloghaz_2^2 + seloghaz_1^2)) %>%
    mutate(hr_2 = exp(loghr_2),
           lcl_2 = exp(loghr_2 - 1.96 * seloghr_2),
           ucl_2 = exp(loghr_2 + 1.96 * seloghr_2)) %>%
    dplyr::select(time, hr_2:ucl_2) %>%
    pivot_longer(hr_2:ucl_2,
                 names_sep = "_",
                 names_to = c("var", "comp")) %>%
    pivot_wider(id_cols = c("time", "comp"),
                names_from = "var",
                values_from = "value") %>%
    mutate(comparison = case_when(comp == 2 ~ legendlabel)) %>%
    mutate(comparison = factor(comparison,
                               levels = c(legendlabel))) %>%
    dplyr::select(-comp)
  
  ## Plot hazard with CI
  
  smoothed_hrs_bshazard <- ggplot(hr_dat,
                                  aes(x = time,
                                      y = hr,
                                      ymin = lcl,
                                      ymax = ucl,    ## Limit for y-axis limits
                                      colour = comparison,
                                      fill = comparison)) +
    geom_ribbon(colour = NA, alpha = 0.2) +
    xlab('Time (years)') + ylab('Hazard Ratio') +
    geom_line() +
    geom_hline(yintercept = 1,
               linetype = "dashed") +
    theme_bw() + 
    scale_fill_manual(name = "", 
                      values = c(legendlabel = "black"),
                      labels = c(legendlabel)) +
    scale_color_manual(name = "",
                       values = c(legendlabel = "black"),
                       labels = c(legendlabel)) +
    # scale_y_continuous(limits = c(0, 5)) +
    theme(legend.position = "bottom") +
    scale_y_continuous(expand = c(0, 0.01), breaks = scales::breaks_extended(n = 6)) +
    scale_x_continuous(expand = c(0, 0.01), breaks = scales::breaks_extended(n = 6)) +
    theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))
  
  return(smoothed_hrs_bshazard)
} # Function end 
