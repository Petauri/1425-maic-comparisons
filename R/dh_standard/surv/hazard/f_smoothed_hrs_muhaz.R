## Load libraries

library(tidyverse)
library(survival)
library(survminer)
library(muhaz)
library(ggplot2)

f_smoothed_hr_muhaz <- function(df, timevar, eventvar, stratify, legendlabel) {
  
  df$time <- df[[timevar]]
  df$event <- df[[eventvar]]
  
  ## Subset data
  
  dat1 <- df %>%
    filter(arm == stratify[1])
  
  dat2 <- df %>%
    filter(arm == stratify[2])
  
  ## Pilot muhaz plots
  
  haz1 <- muhaz(dat1$time, dat1$event)
  haz2 <- muhaz(dat2$time, dat2$event)
  
  ## Plot hazards
  
  # plot(haz1)
  # plot(haz2)
  
  ## Obtain max min.time & min max.time
  
  min.time <- max(haz1$pin$min.time, haz2$pin$min.time)
  max.time <- min(haz1$pin$max.time, haz2$pin$max.time)
  
  ## Estimate hazard ratio
  
  haz1 <- muhaz(dat1$time, dat1$event, min.time = min.time, max.time = max.time)
  haz2 <- muhaz(dat2$time, dat2$event, min.time = min.time, max.time = max.time)

  hr_dat <- full_join(
    data.frame(
      time = haz1$est.grid,
      haz1 = haz1$haz.est
    ),
    data.frame(
      time = haz2$est.grid,
      haz2 = haz2$haz.est
    ),
    by = "time"
  ) %>%
    mutate(hr_2 = haz2 / haz1)
  
  ## Bootstrap hr
  
  B <- 1000
  
  ## Bootstrap dataset indices
  
  N1 <- nrow(dat1)
  idx1 <- sample(1:N1, N1 * B, replace = TRUE)
  
  N2 <- nrow(dat2)
  idx2 <- sample(1:N2, N2 * B, replace = TRUE)
  
  time1 <- matrix(dat1$time[idx1], B, N1)
  event1 <- matrix(dat1$event[idx1], B, N1)
  
  time2 <- matrix(dat2$time[idx2], B, N2)
  event2 <- matrix(dat2$event[idx2], B, N2)
  
  haz1 <- lapply(1:B,
                 function(x) {
                   muhaz(time1[x, ], event1[x, ], min.time = min.time, max.time = max.time)$haz.est
                 }) %>%
    do.call(rbind, .)
  haz2 <- lapply(1:B,
                 function(x) {
                   muhaz(time2[x, ], event2[x, ], min.time = min.time, max.time = max.time)$haz.est
                 }) %>%
    do.call(rbind, .)
  
  hr_2 <- haz2 / haz1
  
  hr2_lcl <- apply(hr_2, 2, quantile, probs = 0.025, na.rm = TRUE)
  hr2_ucl <- apply(hr_2, 2, quantile, probs = 0.975, na.rm = TRUE)
  
  
  hr_dat$lcl_2 <- hr2_lcl
  hr_dat$ucl_2 <- hr2_ucl
  
  ## Restructure HR data
  
  hr_dat <- hr_dat %>%
    dplyr::select(time, hr_2, lcl_2, ucl_2) %>%
    pivot_longer(hr_2:ucl_2,
                 names_sep = "_",
                 names_to = c("var", "comp")) %>%
    pivot_wider(id_cols = c("time", "comp"),
                names_from = "var",
                values_from = "value") %>%
    mutate(comparison = case_when(comp == 2 ~ "Docetaxel vs Nivo")) %>%
    mutate(comparison = factor(comparison,
                               levels = c("Docetaxel vs Nivo"))) %>%
    dplyr::select(-comp)
  
  ## Plot hazard with CI
  
  smoothed_hrs_muhaz <- ggplot(hr_dat,
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
    scale_y_continuous(breaks = scales::breaks_extended(n = 6)) +
    scale_x_continuous(breaks = scales::breaks_extended(n = 6)) +
    theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))
  
  return(smoothed_hrs_muhaz)
  
} # Function end 
