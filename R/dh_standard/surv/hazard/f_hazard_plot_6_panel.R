# Create a function to create a multipanel hazard plot:
# 1. bshazards with CIs
# 2. muhaz smoothed hazard
# 3. unsmoothed hazards pehaz
# 4. 
# 5. 
# 6. 

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

f_hazardplot_6panel <- function(df, survobject, surv_time_var, event_var, file_path_name, save_multi, save_plot_data) {
  
  #' Plot a range of different hazard plots
  #'
  #' @description This function generates a multipanel figure showing comparisons of different hazards.
  #' 
  #' df is the dataframe in question (the same as the one used in the survfit)
  #' 
  #' survobject is a survfit. e.g., survfit(Surv(os_months, os_event) ~ 1, data = df)
  #' 
  #' surv_time_var: Survival variable such as overall survival in months or years or PFS in months in years.
  #' 
  #' event_var: Event variable, usually binary 1,0.
  #' 
  #' file_path_name: file path and name. e.g.: km_save <- file.path("save_folder", "save_fig.png")
  #' 
  #' save_multi: TRUE or FALSE. If TRUE, a multi-panel plot will be saved using the specified file path and name. If FALSE, a list of hazard plots will be returned for the analyst to view and save themselves.
  #' 
  #' save_plot_data: if TRUE, the function will save an xlsx of all the datasets used to create the plots (e.g., for reproducing plots in Excel if desired)
  #'
  #' @details Author: Kurt Taylor, Delta Hat
  
  message("Generating 6 hazard plots which can be plotted in a multipanel figure or returned as separate figures in a list:
          A. Log-cumulative hazard plot
          B. Logit survival plot
          C. Inverse normal survival plot
          D. Unsmoothed and smoothed hazards overlaid
          E. Using bshazard with 95% CIs
          F. Using muhaz with 95% CIs generated via bootstrapping")
  
  #***********************************************************************
  # 1. Log-cumulative hazard plot ----------------------------------------
  #***********************************************************************
  
  #extract time and survival from KM on a granular level for plotting
  haztimes <- c(0, summary(survobject, t = seq(0, max(survobject$time), 0.1))$time) 
  hazsurv <- c(1, summary(survobject, t = seq(0, max(survobject$time), 0.1))$surv)
  #combine survival estimates and times
  data_loglog <- data.frame(surv = hazsurv, time = haztimes)
  #create log(Time) and log(-log(Survival))
  data_loglog$log_time <- log(data_loglog$time)
  data_loglog$log_surv <- log(-log(data_loglog$surv))
  #plot log-log plot
  loglog_plot <- ggplot(data_loglog, aes(log_time, log_surv)) +
    geom_line(size = 1) +
    theme_bw() +
    theme(legend.position = "bottom") + 
    scale_y_continuous(breaks = scales::breaks_extended(n = 6)) +
    scale_x_continuous(breaks = scales::breaks_extended(n = 6)) +
    scale_colour_manual() +
    scale_linetype_manual() +
    guides(colour = guide_legend(title = "", nrow = 1),linetype = guide_legend(title = "", nrow = 1)) +
    xlab("Log(t)") + 
    ylab("Log(-log(S(t)))") + 
    # ggtitle("Log-cumulative hazard plot") +
    theme(axis.title.y = element_text(size = 10, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 10, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 10)) + 
    theme(axis.text.x = element_text(size = 10)) + 
    theme(legend.text = element_text(size = 10)) +
    theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))
  # loglog_plot
  
  #***********************************************************************
  # 2. Logit survival plot ----------------------------------------
  #***********************************************************************
  
  #combine survival estimates and times
  data_logit <- data.frame(surv = hazsurv, time = haztimes)
  #create log(Time) and log((Survival)/1-survival)
  data_logit$log_time <- log(data_logit$time)
  data_logit$log_surv <- log(data_logit$surv/(1 - data_logit$surv))
  #plot logit survival plot
  logit_plot <- ggplot(data_logit, aes(log_time, log_surv)) +
    geom_line(size = 1) +
    theme_bw() +
    theme(legend.position = "bottom") + 
    scale_y_continuous(breaks = scales::breaks_extended(n = 6)) +
    scale_x_continuous(breaks = scales::breaks_extended(n = 6)) +
    scale_colour_manual() +
    scale_linetype_manual() +
    guides(colour = guide_legend(title = "", nrow = 1),linetype = guide_legend(title = "", nrow = 1)) +
    xlab("Log(t)") + 
    ylab("Log(S(t)/1-S(t))") + 
    # ggtitle("Logit survival plot") +
    theme(axis.title.y = element_text(size = 10, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 10, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 10)) + 
    theme(axis.text.x = element_text(size = 10)) + 
    theme(legend.text = element_text(size = 10)) + 
    theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))
  # logit_plot
  
  #***********************************************************************
  # 3. Log normal plot  ----------------------------------------
  #***********************************************************************
  
  #combine survival estimates and times
  data_invnorm <- data.frame(surv = hazsurv, time = haztimes)
  #create log(Time) and qnorm(1-survival)
  data_invnorm$log_time <- log(data_invnorm$time)
  data_invnorm$log_surv <- qnorm(1 - data_invnorm$surv)
  #plot logit survival plot
  invnorm_plot <- ggplot(data_invnorm, aes(log_time, log_surv)) +
    geom_line(size = 1) +
    theme_bw() +
    theme(legend.position = "bottom") + 
    scale_y_continuous(breaks = scales::breaks_extended(n = 6)) +
    scale_x_continuous(breaks = scales::breaks_extended(n = 6)) +
    scale_colour_manual() +
    scale_linetype_manual() +
    guides(colour = guide_legend(title = "", nrow = 1),linetype = guide_legend(title = "", nrow = 1)) +
    xlab("Log(t)") + 
    ylab("Inv.normal(1-S(t))") + 
    # ggtitle("Inverse normal survival plot") +
    theme(axis.title.y = element_text(size = 10, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 10, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 10)) + 
    theme(axis.text.x = element_text(size = 10)) + 
    theme(legend.text = element_text(size = 10)) +
    theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))
  # invnorm_plot
  
  #***********************************************************************
  # 4. Smoothed hazard - BSHAZARD   ----------------------------------------
  #***********************************************************************
  
  as.data.frame.bshazard <- function(x, ...) {
    with(x, data.frame(time,hazard,lower.ci,upper.ci))
  }
  
  bshazard_fit <- as.data.frame(bshazard(Surv(df[[surv_time_var]], df[[event_var]]) ~ 1, verbose=FALSE))
  
  bshazard_plot <- ggplot(bshazard_fit, aes(x = time, y = hazard, ymin = lower.ci, ymax = upper.ci)) +
    geom_ribbon(colour = NA,
                alpha = 0.2) +
    geom_line(size = 1, linetype = "dotted") +
    theme_bw() +
    scale_x_continuous(breaks = scales::breaks_extended(n = 6),
                       expand = c(0, 0),
                       limits = c(0, NA)  # Set x-axis limits to start at 0
    ) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 6),
                       expand = c(0, 0),
                       limits = c(0, NA)  # Set y-axis limits to start at 0
    ) +
    scale_colour_manual() +
    scale_linetype_manual() +
    guides(colour = guide_legend(title = "", nrow = 1),linetype = guide_legend(title = "", nrow = 1)) +
    xlab("Time (months)") + 
    ylab("Estimated hazard") + 
    theme(axis.title.y = element_text(size = 10, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 10, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 10)) + 
    theme(axis.text.x = element_text(size = 10)) + 
    theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))
  
  #***********************************************************************
  # 5. Smoothed hazard - MUHAZ  ----------------------------------------
  #***********************************************************************
  
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
    geom_line(size = 1, linetype = "dashed") +
    theme_bw() +
    theme(legend.position = "bottom") + 
    scale_x_continuous(breaks = scales::breaks_extended(n = 6),
                       # limits = c(0, max(mh_eventlist)),
                       expand = c(0, 0)
    ) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 6),
                       expand = c(0, 0)
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
  
  #***********************************************************************
  # 6. Stepped and smoothed hazards overlaid  ----------------------------
  #***********************************************************************
  
  # PEHAZ
  
  pehaz_fit <- pehaz(time = df[[surv_time_var]], delta = df[[event_var]], max.time = max(df[[surv_time_var]]))
  # remove 0 cut
  pehaz_fit$Cuts <- pehaz_fit$Cuts[-1]
  # extract
  pehaz_extract <- data.frame(Hazard = pehaz_fit$Hazard, Time = pehaz_fit$Cuts)
  
  pehaz_extract$Strata <- "Unsmoothed (pehaz)"
  muhaz_extract_cis$Strata <- "Smoothed (muhaz)"
  bshazard_fit$Strata <- "Smoothed (bshazard)"
  
  # muhaz_max <- muhaz(time = df[[surv_time_var]], delta = df[[event_var]], max.time = max(df[[surv_time_var]]))
  # 
  # muhaz_extract_max <- data.frame(Hazard = muhaz_max$haz.est, Time = muhaz_max$est.grid)
  # muhaz_extract_max$Strata <- "Smoothed (muhaz) with max follow-up"
  
  cols <- c("Unsmoothed (pehaz)" = "black", "Smoothed (muhaz)" = "black", "Smoothed (bshazard)" = "black")
  linetypes <- c("Unsmoothed (pehaz)" = "solid", "Smoothed (muhaz)" = "dashed", "Smoothed (bshazard)" = "dotted")
  
  overlay_plot <- ggplot() +
    geom_step(data = pehaz_extract, aes(x=Time, y=Hazard, colour= Strata, linetype = Strata), size = 1) +
    geom_line(data = muhaz_extract_cis, aes(x=Time, y=Hazard, colour= Strata, linetype = Strata), size = 1) +
    geom_line(data = bshazard_fit, aes(x=time, y=hazard, colour= Strata, linetype = Strata), size = 1) +
    theme_bw() +
    theme(legend.position = "none") + 
    scale_x_continuous(breaks = scales::breaks_extended(n = 6),
                       # limits = c(0, max(mh_eventlist)),
                       expand = c(0, 0)
    ) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 6),
                       expand = c(0, 0)
    ) +
    scale_colour_manual(values = cols)+
    scale_linetype_manual(values = linetypes)+
    theme(legend.spacing.y = unit(0.05, 'cm')) +
    guides(colour=guide_legend(title="", nrow=1),linetype = guide_legend(title="", nrow=1),
           fill = guide_legend(byrow = TRUE)) +
    xlab("Time (months)") + 
    ylab("Estimated hazard") + 
    theme(axis.title.y = element_text(size = 10, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 10, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 10)) + 
    theme(axis.text.x = element_text(size = 10)) + 
    theme(legend.text = element_text(size = 7)) +
    theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))
  
  #***********************************************************************
  # COMBINE INTO MULTI PANEL PLOT ----------------------------
  #***********************************************************************
  
  multipanel <- plot_grid(loglog_plot, logit_plot,
                          invnorm_plot, overlay_plot,
                          bshazard_plot, muhaz_plot_cis, 
                          labels = c("A: Log-cumulative hazard plot",
                                     "B: Logit survival plot",
                                     "C: Inverse normal survival plot",
                                     "D: Unsmoothed (solid) & smoothed (dotted = 'bshazard' & dashed = 'muhaz') hazards",
                                     "E: Smoothed hazard plot ('bshazard' with 95% CI)",
                                     "F: Smoothed hazard plot ('muhaz' with 95% CI)"
                          ),
                          label_size = 8,
                          hjust = 0,
                          # vjust = -0.05,
                          ncol = 2, nrow = 3)
  
  # Combine plots into a list to return from the function ]
  
  hazard_plots <- list(
    loglog = loglog_plot,
    logit = logit_plot,
    invnorm = invnorm_plot,
    bshazard = bshazard_plot,
    muhaz = muhaz_plot_cis,
    overlay = overlay_plot,
    multipanel = multipanel
  )
  
  if (save_multi == FALSE) {
    
    print(paste0("List of different plots returned!"))
    
    return(hazard_plots)
    
  } else {
    
    # SAVE
    
    png(file_path_name,
        units = "mm", width=240, height=330, res = 1000)
    print(hazard_plots$multipanel)
    dev.off()
    
    print(paste0("6 panel hazard plots to compare hazards successfully saved to loaction: ", file_path_name))   
    
  }
  
  if (save_plot_data == TRUE) {
    
    # Combine dataframes into a list 
    
    hazard_plots_dfs <- list(
      loglog = data_loglog,
      logit = data_logit,
      invnorm = data_invnorm,
      bshazard = bshazard_fit,
      muhaz = muhaz_extract_cis,
      pehaz = pehaz_extract
    )
    
    # Create a new directory called "plot_data" within the "survival" directory
    
    new_directory <- file.path(file_path_name, "plot_data")
    dir.create(new_directory, showWarnings = FALSE)
    
    # Add back the file name to the new directory
    
    file_path_name_dat <- file.path(new_directory, basename(file_path_name))
    
    # Change png to xlsx as we are saving Excel file here 
    
    file_path_name_dat <- sub("\\.png$", ".xlsx", file_path_name_dat)
    
    # Create a new Excel workbook
    wb <- createWorkbook()
    
    # Loop through the hazards_list and add each data frame as a new sheet
    for (name in names(hazard_plots_dfs)) {
      df <- hazard_plots_dfs[[name]]
      
      # Add the data frame to a new sheet with the name as sheet name
      addWorksheet(wb, sheetName = name)
      writeDataTable(wb, sheet = name, x = df, startCol = 1, startRow = 1)
    }
    
    # Save the Excel workbook to a file
    saveWorkbook(wb, file_path_name_dat, overwrite = TRUE)
    
    print(paste0("KM coordinate data successfully saved to location: ", file_path_name_dat))
    
  }
  
  
} # Function end 

