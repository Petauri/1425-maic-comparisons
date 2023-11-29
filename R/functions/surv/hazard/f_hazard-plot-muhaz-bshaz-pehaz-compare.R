# Create a function to create a multipanel hazard plot:
# 1. bshazards with CIs
# 2. muhaz smoothed hazard
# 3. unsmoothed hazards pehaz

# Libraries 

pacman::p_load(
  tidyverse, 
  cowplot, 
  ggplot2, 
  zoo,
  janitor,
  survival, 
  survminer, 
  muhaz, 
  cowplot, 
  flexsurv,
  biostat3, 
  bshazard,
  docstring,
  ggpp, 
  ggplotify,
  openxlsx
)

# Single arm trial for now - can adapt in future with IF statements and options if necessary for multi arms and groups

f_hazard_compare <- function(df, surv_time_var, event_var, file_path_name, save_multi) {
  
  #' Plot a range of different hazard plots
  #'
  #' @description This function generates a multipanel figure showing comparisons of different hazards.
  #' 
  #' A is a smoothed hazard plot using the "bshazard" function with 95% CIs
  #' 
  #' B is a smoothed hazard plot using the "muhaz" function with 95% CIs generated via bootstrapping
  #' 
  #' C is a smoothed hazard plot using the "muhaz" function with default settings
  #' 
  #' D is an sunmoothed hazard plot using the "pehaz" function and smoothed (using "muhaz" overlaid)
  #' 
  #' df is the dataframe in question (the same as the one used in the survfit)
  #' 
  #' surv_time_var: Survival variable such as overall survival in months or years or PFS in months in years.
  #' 
  #' event_var: Event variable, usually binary 1,0.
  #' 
  #' file_path_name: file path and name. e.g.: km_save <- file.path("save_folder", "save_fig.png")
  #' 
  #' save_multi: TRUE or FALSE. If TRUE, a multi-panel plot will be saved using the specified file path and name. If FALSE, a list of hazard plots will be returned for the analyst to view and save themselves.
  #'
  #' @details Author: Kurt Taylor, Delta Hat
  
  message("Generating 5 hazard plots which will be returned in a list:
          1. Using bshazard with 95% CIs
          2. Using muhaz with 95% CIs generated via bootstrapping
          3. Using muhaz (standard with no CIs)
          4. Using pehaz for unsmoothed hazards
          5. Unsmoothed and smoothed hazards overlaid on the same plot (pehaz and muhaz)
          6. A multipanel plot of #1, #2, #3 and #5")
  
  # 1. BSHAZARD 
  
  as.data.frame.bshazard <- function(x, ...) {
    with(x, data.frame(time,hazard,lower.ci,upper.ci))
  }
  
  bshazard_fit <- as.data.frame(bshazard(Surv(df[[surv_time_var]], df[[event_var]]) ~ 1, verbose=FALSE))
  
  bshazard_plot <- ggplot(bshazard_fit, aes(x=time,y=hazard)) + 
    geom_line(size = 1) +
    geom_ribbon(aes(ymin=lower.ci, ymax=upper.ci), alpha=0.3) +
    xlab('Time (months)') + ylab('Estimated hazard') +
    theme_bw() +
    theme(axis.title.y = element_text(size = 10, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 10, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 10)) + 
    theme(axis.text.x = element_text(size = 10)) + 
    theme(legend.text = element_text(size = 10)) +
    theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))
  
  # ggtitle("Smoothed hazard plot (bshazard with 95% CI)") 
  
  # 2. Muhaz with CIs 
  
  # source muhaz_ci function to generate CIs with bootstrapping 
  source("R/dh_standard/surv/hazard/f_muhaz_ci.R")
  
  muhaz_cis <- muhaz_ci(df[[surv_time_var]], df[[event_var]], B = 1000, max.time = max(df[[surv_time_var]]))
  
  muhaz_extract_cis <- data.frame(
    Time = muhaz_cis$est.grid,
    Hazard = muhaz_cis$haz.est,
    lcl = muhaz_cis$haz.lcl,
    ucl = muhaz_cis$haz.ucl
  )
  
  #Plot hazard function
  muhaz_plot_cis <- ggplot(muhaz_extract_cis, aes(x = Time, y = Hazard, ymin = lcl, ymax = ucl)) +
    geom_ribbon(colour = NA,
                alpha = 0.2) +
    geom_line(size = 1) +
    theme_bw() +
    theme(legend.position = "bottom") + 
    scale_x_continuous(breaks = scales::breaks_extended(n = 6),
                       # limits = c(0, max(mh_eventlist)),
                       # expand = c(0, 0)
    ) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 6),
                       # expand = c(0, 0)
    ) +
    scale_colour_manual() +
    scale_linetype_manual() +
    guides(colour = guide_legend(title = "", nrow = 1),linetype = guide_legend(title = "", nrow = 1)) +
    xlab("Time (months)") + 
    ylab("Estimated hazard") + 
    # ggtitle("Smoothed hazard plot using muhaz") +
    theme(axis.title.y = element_text(size = 10, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 10, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 10)) + 
    theme(axis.text.x = element_text(size = 10)) + 
    theme(legend.text = element_text(size = 10)) +
    theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))
  
  # 3. Muhaz using muhaz2 which allows a survfit to be called
  
  muhaz_fit <- muhaz2(Surv(df[[surv_time_var]], df[[event_var]]) ~ 1, df)
  
  #extract hazard estimates and times
  muhaz_extract <- data.frame(Hazard = muhaz_fit$haz.est, Time = muhaz_fit$est.grid)
  
  #Plot hazard function
  muhaz_plot <- ggplot(muhaz_extract, aes(x = Time, y = Hazard)) +
    geom_line(size = 1) +
    theme_bw() +
    theme(legend.position = "bottom") + 
    scale_x_continuous(breaks = scales::breaks_extended(n = 6),
                       # limits = c(0, max(mh_eventlist)),
                       # expand = c(0, 0)
    ) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 6),
                       # expand = c(0, 0)
    ) +
    scale_colour_manual() +
    scale_linetype_manual() +
    guides(colour = guide_legend(title = "", nrow = 1),linetype = guide_legend(title = "", nrow = 1)) +
    xlab("Time (months)") + 
    ylab("Estimated hazard") + 
    # ggtitle("Smoothed hazard plot using muhaz") +
    theme(axis.title.y = element_text(size = 10, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 10, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 10)) + 
    theme(axis.text.x = element_text(size = 10)) + 
    theme(legend.text = element_text(size = 10)) +
    theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))
  
  # 4. pehaz 
  
  pehaz_fit <- pehaz(time = df[[surv_time_var]], delta = df[[event_var]], max.time = max(df[[surv_time_var]]))
  # remove 0 cut
  pehaz_fit$Cuts <- pehaz_fit$Cuts[-1]
  # extract
  pehaz_extract <- data.frame(Hazard = pehaz_fit$Hazard, Time = pehaz_fit$Cuts)
  
  pehaz_plot <- ggplot(pehaz_extract, aes(x = Time, y = Hazard)) +
    geom_step(size = 1) +
    theme_bw() +
    theme(legend.position = "bottom") + 
    scale_x_continuous(breaks = scales::breaks_extended(n = 6),
                       # limits = c(0, max(mh_eventlist)),
                       # expand = c(0, 0)
    ) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 6),
                       # expand = c(0, 0)
    ) +
    scale_colour_manual() +
    scale_linetype_manual() +
    guides(colour = guide_legend(title = "", nrow = 1),linetype = guide_legend(title = "", nrow = 1)) +
    xlab("Time (months)") + 
    ylab("Estimated hazard") + 
    # ggtitle("Smoothed hazard plot using muhaz") +
    theme(axis.title.y = element_text(size = 10, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 10, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 10)) + 
    theme(axis.text.x = element_text(size = 10)) + 
    theme(legend.text = element_text(size = 10)) +
    theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))
  
  # 5. Pehaz and Muhaz same plot 
  
  pehaz_extract$Strata <- "Unsmoothed (pehaz)"
  muhaz_extract$Strata <- "Smoothed (muhaz)"
  
  muhaz_max <- muhaz(time = df[[surv_time_var]], delta = df[[event_var]], max.time = max(df[[surv_time_var]]))
  
  muhaz_extract_max <- data.frame(Hazard = muhaz_max$haz.est, Time = muhaz_max$est.grid)
  muhaz_extract_max$Strata <- "Smoothed (muhaz) with max follow-up"
  
  cols <- c("Unsmoothed (pehaz)" = "black", "Smoothed (muhaz)" = "black", "Smoothed (muhaz) with max follow-up" = "black")
  linetypes <- c("Unsmoothed (pehaz)" = "solid", "Smoothed (muhaz)" = "dashed", "Smoothed (muhaz) with max follow-up" = "dotted")
  
  overlay_plot <- ggplot() +
    geom_step(data = pehaz_extract, aes(x=Time, y=Hazard, colour= Strata, linetype = Strata), size = 1) +
    geom_line(data = muhaz_extract, aes(x=Time, y=Hazard, colour= Strata, linetype = Strata), size = 1) +
    geom_line(data = muhaz_extract_max, aes(x=Time, y=Hazard, colour= Strata, linetype = Strata), size = 1) +
    theme_classic() +
    theme(legend.position = "bottom") + 
    scale_x_continuous(breaks = scales::breaks_extended(n = 6),
                       # limits = c(0, max(mh_eventlist)),
                       # expand = c(0, 0)
    ) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 6),
                       # expand = c(0, 0)
    ) +
    scale_colour_manual(values = cols)+
    scale_linetype_manual(values = linetypes)+
    theme(legend.spacing.y = unit(0.05, 'cm')) +
    guides(colour=guide_legend(title="", nrow=3),linetype = guide_legend(title="", nrow=1),
           fill = guide_legend(byrow = TRUE)) +
    xlab("Time (months)") + 
    ylab("Estimated hazard") + 
    theme(axis.title.y = element_text(size = 10, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 10, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 10)) + 
    theme(axis.text.x = element_text(size = 10)) + 
    theme(legend.text = element_text(size = 7)) +
    theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))
  
  # 6. Combined plot
  
  multipanel <- plot_grid(bshazard_plot, muhaz_plot_cis, muhaz_plot, overlay_plot,
                          labels = c("A: Smoothed hazard plot ('bshazard' with 95% CI)",
                                     "B: Smoothed hazard plot ('muhaz' with 95% CI)",
                                     "C: Smoothed hazard plot ('muhaz')",
                                     "D: Unsmoothed ('pehaz') & smoothed ('muhaz') overlaid"),
                          label_size = 10,
                          hjust = 0,
                          # vjust = -0.05,
                          ncol = 2, nrow = 2)
  
  # Combine plots into a list to return from the function ]
  
  hazard_plots <- list(
    bshazard = bshazard_plot,
    muhaz_cis = muhaz_plot_cis,
    muhaz = muhaz_plot,
    pehaz = pehaz_plot,
    overlay = overlay_plot,
    multipanel = multipanel
  )
  
  if (save_multi == FALSE) {
    
    print(paste0("List of different plots returned for: ", cohort, " subgroup and ", surv_measure, " survival measure successfully saved!"))
    
    return(hazard_plots)
    
  } else {
    
    # SAVE
    
    png(file_path_name,
        units = "mm", width=330, height=220, res = 1000)
    print(hazard_plots$multipanel)
    dev.off()
    
    print(paste0("4 panel hazard plots to compare hazards successfully saved to loaction: ", file_path_name))   
    
  }
  
  
}
