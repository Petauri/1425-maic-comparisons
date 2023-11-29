f_hazardplots <- function(survobject, p_label, p_weight = 0, file_path_name) {
  # f_tcatch_dash(f_hazardplots(fit_pfs_ut_uw, "PFS", 0))
  # survobject <- fit_pfs_ut_uw
  ## 1. Log-cumulative hazard plot
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
    ggtitle("Log-cumulative hazard plot") +
    theme(axis.title.y = element_text(size = 13, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 13, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 13)) + 
    theme(axis.text.x = element_text(size = 13)) + 
    theme(legend.text = element_text(size = 13)) 
  # loglog_plot
  
  ## 2. Logit survival plot
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
    ggtitle("Logit survival plot") +
    theme(axis.title.y = element_text(size = 13, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 13, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 13)) + 
    theme(axis.text.x = element_text(size = 13)) + 
    theme(legend.text = element_text(size = 13))
  # logit_plot
  
  ## 3. Log normal plot
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
    ggtitle("Inverse normal survival plot") +
    theme(axis.title.y = element_text(size = 13, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 13, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 13)) + 
    theme(axis.text.x = element_text(size = 13)) + 
    theme(legend.text = element_text(size = 13))
  # invnorm_plot
  
  ## 4. Smoothed hazard plot
  muhaz_data <- cbind(c(rep(survobject$time, survobject$n.event), rep(survobject$time, survobject$n.censor)), 
                      c(rep(1, sum(survobject$n.event)), rep(0, sum(survobject$n.censor))))
  mh_eventlist <- muhaz_data[,1][muhaz_data[,2] == 1]
  muhaz_fit <- muhaz(time = muhaz_data[,1], delta = muhaz_data[,2]) 
  muhaz_fit_bw <- muhaz(time = muhaz_data[,1], delta = muhaz_data[,2], bw.grid = 6)
  
  #extract hazard estimates and times
  muhaz_extract <- data.frame(Hazard = muhaz_fit$haz.est, Time = muhaz_fit$est.grid)
  muhaz_extract_bw <- data.frame(Hazard = muhaz_fit_bw$haz.est, Time = muhaz_fit_bw$est.grid)
  
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
    ggtitle("Smoothed hazard plot") +
    theme(axis.title.y = element_text(size = 13, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 13, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 13)) + 
    theme(axis.text.x = element_text(size = 13)) + 
    theme(legend.text = element_text(size = 13))
  # muhaz_plot
  
  #maxeg muhaz plot
  suppressWarnings(muhaz_plot_maxeg <- ggplot(muhaz_extract, aes(x = Time, y = Hazard)) +
                     geom_line(size = 1) +
                     theme_bw() +
                     theme(legend.position = "bottom") + 
                     scale_x_continuous(breaks = scales::breaks_extended(n = 6),
                                        limits = c(0, 1.1*(max(muhaz_fit$est.grid))),
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
                     ggtitle("Smoothed hazard plot") +
                     theme(axis.title.y = element_text(size = 13, angle = 90, face = "bold")) + 
                     theme(axis.title.x = element_text(size = 13, angle = 0, face = "bold")) + 
                     theme(axis.text.y = element_text(size = 13)) + 
                     theme(axis.text.x = element_text(size = 13)) + 
                     theme(legend.text = element_text(size = 13))
  )
  # muhaz_plot_maxeg
  
  #Plot bw hazard function
  muhaz_plot_bw <- ggplot(muhaz_extract_bw, aes(x = Time, y = Hazard)) +
    geom_line(size = 1) +
    theme_bw() +
    theme(legend.position = "bottom") + 
    scale_x_continuous(breaks = scales::breaks_extended(n = 6),
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
    ggtitle("Smoothed hazard plot") +
    theme(axis.title.y = element_text(size = 13, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 13, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 13)) + 
    theme(axis.text.x = element_text(size = 13)) + 
    theme(legend.text = element_text(size = 13))
  # muhaz_plot_bw
  
  muhaz_plot_bwmaxeg <- ggplot(muhaz_extract_bw, aes(x = Time, y = Hazard)) +
    geom_line(size = 1) +
    theme_bw() +
    theme(legend.position = "bottom") + 
    scale_x_continuous(breaks = scales::breaks_extended(n = 6),
                       limits = c(0, 1.1*(max(muhaz_fit$est.grid))),
                       expand = c(0, 0)
    ) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 6),
                       # expand = c(0, 0)
    ) +
    scale_colour_manual() +
    scale_linetype_manual() +
    guides(colour = guide_legend(title = "", nrow = 1),linetype = guide_legend(title = "", nrow = 1)) +
    xlab("Time (months)") + 
    ylab("Estimated hazard") + 
    ggtitle("Smoothed hazard plot") +
    theme(axis.title.y = element_text(size = 13, angle = 90, face = "bold")) + 
    theme(axis.title.x = element_text(size = 13, angle = 0, face = "bold")) + 
    theme(axis.text.y = element_text(size = 13)) + 
    theme(axis.text.x = element_text(size = 13)) + 
    theme(legend.text = element_text(size = 13))
  # muhaz_plot_bwmaxeg
  
  # 5. Combined plot
  diagnostic_plots <- plot_grid(loglog_plot, logit_plot, invnorm_plot, muhaz_plot, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
  # suppressWarnings(diagnostic_plots_maxeg <- plot_grid(loglog_plot, logit_plot, invnorm_plot, muhaz_plot_maxeg, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2))
  # suppressWarnings(diagnostic_plots_bw <- plot_grid(loglog_plot, logit_plot, invnorm_plot, muhaz_plot_bw, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2))
  # suppressWarnings(diagnostic_plots_bwmaxeg <- plot_grid(loglog_plot, logit_plot, invnorm_plot, muhaz_plot_bwmaxeg, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2))
  # diagnostic_plots
  # diagnostic_plots_diagnostic_plots_maxel
  # diagnostic_plots_diagnostic_plots_maxeg
  
  # return(diagnostic_plots)
  
  # SAVE PLOT
  
  png(file_path_name,
      units = "mm", width=330, height=220, res = 1000)
  print(diagnostic_plots)
  dev.off()
  
  print(paste0("4 panel hazard plot successfully saved to loaction: ", file_path_name))
  
}

# END