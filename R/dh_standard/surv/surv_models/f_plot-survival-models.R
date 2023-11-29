# Libraries 

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
  ggfortify,
  scales,
  openxlsx
)

f_plot_models <- function(df, survival_formula, surv_fit, model_type, arms, max_extrap, extrap_seq, conv_to_years, surv_measure, file_path_name, save_plot_data) {
  
  #' Plot survival models
  #'
  #' @description This function plots survival models (simple or flexible (splines)) for comparison
  #' 
  #' df is the dataframe in question 
  #'   
  #' survival_formula is self explanatory. e.g., survival_formula <- Surv(os_months, os_event) ~ 1
  #' 
  #' surv_fit - e.g.,  survfit(Surv(os_months, os_event) ~ 1, data = df)
  #' 
  #' model_type - either "simple" or "spline"
  #' 
  #' arms - "single" only option for now - look to update this in future for multi-arm studies. 
  #' 
  #' max_extrap is the number of months or years that you want to extrapolate by - numeric
  #' 
  #' extrap_seq - the cuts for max_extrap. if max_extrap is 10 years and extrap_seq is 0.1 the model coefs will be extracted every 0.1 year and then plotted.
  #' 
  #' conv_to_years - if you survival variable is in months, then set this to TRUE to convert to years 
  #' 
  #' surv_measure - "OS", "PFS" or "ToT"
  #' 
  #' file_path_name: file path and name. e.g.: km_save <- file.path("save_folder", "save_fig.png")
  #' 
  #' save_plot_data: if TRUE, the function will save an xlsx of all the datasets used to create the plots (e.g., for reproducing plots in Excel if desired)
  #' 
  #' @details Author: Kurt Taylor, Delta Hat
  
  # Error catching for models 
  
  f_tcatch_dnc <- function(code) {
    tryCatch({
      eval(code)
    }, 
    warning = function(warning) {"Did not converge"}, error = function(error) {"Did not converge"})
  }
  
  # Survival var as months or years
  
  if(conv_to_years == TRUE){
    
    print("Converting survival variable from 'months' to 'years' on the plot")
    
    time_var <- 12
    
  } else {
    
    print("Survival variable supplied in 'years' - leaving as is")
    
    time_var <- 1
    
  }
  
  
  if(model_type == "simple" & arms == "one") {
    
    message("Running these parametric models and plotting along with Kaplan-Meir estimate:
            1 = exponential, 
            2 = generalised gamma, 
            3 = gompertz, 
            4 = log logistic, 
            5 = log normal, 
            6 = weibull, 
            7 = gamma")
    
    # Build the model list 
    
    model_list <- list()
    
    model_list <- list(
      parametric_exp = f_tcatch_dnc(flexsurvreg(survival_formula, data = df, dist = "exp")),
      parametric_gengamma = f_tcatch_dnc(flexsurvreg(survival_formula, data = df, dist = "gengamma")),
      parametric_gompertz = f_tcatch_dnc(flexsurvreg(survival_formula, data = df, dist = "gompertz")),
      parametric_llogis = f_tcatch_dnc(flexsurvreg(survival_formula, data = df, dist = "llogis")),
      parametric_lnorm = f_tcatch_dnc(flexsurvreg(survival_formula, data = df, dist = "lnorm")),
      parametric_weibull = f_tcatch_dnc(flexsurvreg(survival_formula, data = df, dist = "weibull")),
      parametric_gamma = f_tcatch_dnc(flexsurvreg(survival_formula, data = df, dist = "gamma"))
    )
    
    legend_lbl <- "Model" # Specify label for the KM here
    
    # Format DF so that it can be added to the fits line lists
    
    survival_KM_line <- broom::tidy(surv_fit) %>%
      mutate(lbl = "Kaplan-Meir")
    
    # Extract coordinates 
    
    simple_fits_line <- list() 
    
    simple_fits_line <- list(
      parametric_exp = summary(model_list$parametric_exp, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T),
      parametric_gengamma = summary(model_list$parametric_gengamma, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T),
      parametric_gompertz = summary(model_list$parametric_gompertz, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T),
      parametric_llogis = summary(model_list$parametric_llogis, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T),
      parametric_lnorm = summary(model_list$parametric_lnorm, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T),
      parametric_weibull = summary(model_list$parametric_weibull, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T),
      parametric_gamma = summary(model_list$parametric_gamma, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T)
    )
    
    # Labels for each curve saved here for reference later
    
    simple_fits_line$parametric_exp$lbl <- "Exponential"
    simple_fits_line$parametric_gengamma$lbl <- "Generalised gamma"
    simple_fits_line$parametric_gompertz$lbl <- "Gompertz"
    simple_fits_line$parametric_llogis$lbl <- "Log-logistic"
    simple_fits_line$parametric_lnorm$lbl <- "Log-normal"
    simple_fits_line$parametric_weibull$lbl <- "Weibull"
    simple_fits_line$parametric_gamma$lbl <- "Gamma"
    
    # Add KM data frames to the existing list
    
    simple_fits_line <- c(simple_fits_line, list(survival_KM_line = survival_KM_line))
    
    # Assign colours and line types according to labels 
    
    col_list <- c(
      "temp" = "black",
      "Exponential" = "red", 
      "Generalised gamma" = "orange", 
      "Gompertz" = "yellow",
      "Log-logistic" = "cyan", 
      "Log-normal" = "blue", 
      "Weibull" = "pink",
      "Gamma" = "darkblue",
      "Kaplan-Meir" = "black") 
    names(col_list)[1] <- legend_lbl
    
    lines_list <- c(
      "temp" = "solid",
      "Exponential" = "solid", 
      "Generalised gamma" = "solid", 
      "Gompertz" = "solid",
      "Log-logistic" = "solid", 
      "Log-normal" = "solid", 
      "Weibull" = "solid",
      "Gamma" = "solid",
      "Kaplan-Meir" = "solid")
    names(lines_list)[1] <- legend_lbl
    
    # PLOT SURVIVAL 
    
    legend_order <- c(
      "Exponential", 
      "Generalised gamma", 
      "Gompertz",
      "Log-logistic", 
      "Log-normal", 
      "Weibull",
      "Gamma",
      "Kaplan-Meir")
    
    survival_plot <- ggplot() +
      geom_ribbon(data = simple_fits_line$survival_KM_line, aes(x = time/time_var, y = estimate*100, ymin = conf.low*100, ymax = conf.high*100), fill = "gray", alpha = 0.4)
    
    # Conditional statements for when models fail - do not plot if model has not converged 
    
    if (is.data.frame(simple_fits_line$parametric_exp) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = simple_fits_line$parametric_exp, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    if (is.data.frame(simple_fits_line$parametric_gengamma) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = simple_fits_line$parametric_gengamma, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    if (is.data.frame(simple_fits_line$parametric_gompertz) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = simple_fits_line$parametric_gompertz, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    if (is.data.frame(simple_fits_line$parametric_llogis) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = simple_fits_line$parametric_llogis, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    if (is.data.frame(simple_fits_line$parametric_lnorm) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = simple_fits_line$parametric_lnorm, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    if (is.data.frame(simple_fits_line$parametric_weibull) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = simple_fits_line$parametric_weibull, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    if (is.data.frame(simple_fits_line$parametric_gamma) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = simple_fits_line$parametric_gamma, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    survival_plot <- survival_plot +
      geom_step(data = simple_fits_line$survival_KM_line, aes(x = time/time_var, y = estimate*100, col = lbl, lty = lbl), linewidth = 1) +
      guides(col = guide_legend(title = "", nrow = 3),
             linetype = guide_legend(title = "", nrow = 3),
             fill = guide_legend(title = "", nrow = 3)) +
      scale_colour_manual(values = col_list, breaks = legend_order) + 
      scale_linetype_manual(values = lines_list, breaks = legend_order) + 
      scale_y_continuous(expand = c(0,0), breaks = scales::breaks_extended(n = 6)) +
      scale_x_continuous(expand = c(0,0), breaks = scales::breaks_extended(n = 6)) +
      xlab('Time (years)') + ylab('Survival probability (%)') +
      theme_bw() +
      theme(axis.title.y = element_text(size = 13, angle = 90, face = "bold")) + 
      theme(axis.title.x = element_text(size = 13, angle = 0, face = "bold")) + 
      theme(axis.text.y = element_text(size = 13)) + 
      theme(axis.text.x = element_text(size = 13)) + 
      theme(legend.text = element_text(size = 13)) +
      theme(plot.margin = margin(1,1,1,1, "cm"),
            legend.position = "top",
            axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            panel.background = element_blank())
    
    # Conditional axis titles 
    
    if(surv_measure == "PFS") {
      
      survival_plot <- survival_plot +
        ylab('Progression free survival (%)')
      
    } else if (surv_measure == "ToT") {
      
      survival_plot <- survival_plot +
        ylab('Time on treatment (%)')
      
    }
    
    if(save_plot_data == TRUE) {
      
      # Extract the directory path from the vector
      
      directory_path <- dirname(file_path_name)
      
      # Create a new directory called "plot_data" within the "survival" directory
      
      new_directory <- file.path(directory_path, "plot_data")
      dir.create(new_directory, recursive = TRUE, showWarnings = FALSE)
      
      # Add back the file name to the new directory
      
      file_path_name_dat <- file.path(new_directory, basename(file_path_name))
      
      # Change png to xlsx as we are saving Excel file here 
      
      file_path_name_dat <- sub("\\.png$", ".xlsx", file_path_name_dat)
      
      # Create a new Excel workbook
      wb <- createWorkbook()
      
      # Loop through the hazards_list and add each data frame as a new sheet
      for (model_name in names(simple_fits_line)) {
        
        if (is.data.frame(simple_fits_line[[model_name]]) == TRUE) {
          
          df <- simple_fits_line[[model_name]]
          
          # Add the data frame to a new sheet with the model_name as sheet name
          addWorksheet(wb, sheetName = model_name)
          writeDataTable(wb, sheet = model_name, x = df, startCol = 1, startRow = 1)
          
        }
        
      }
      
      # Save the Excel workbook to a file
      saveWorkbook(wb, file_path_name_dat, overwrite = TRUE)
      
    }
    
  } else if(model_type == "spline" & arms == "one") {
    
    message("Running these spline models and plotting along with Kaplan-Meir estimate:
            1 = hazard 1 knot, 
            2 = hazard 2 knot, 
            3 = hazard 3 knot, 
            4 = odds 1 knot, 
            5 = odds 2 knot, 
            6 = odds 3 knot,            
            7 = normal 1 knot, 
            8 = normal 2 knot, 
            9 = normal 3 knot")
    
    # Build the model list 
    
    model_list <- list()
    
    model_list <- list(
      onekhaz = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 1, scale = "hazard")),
      twokhaz = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 2, scale = "hazard")),
      threekhaz = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 3, scale = "hazard")),
      onekodd = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 1, scale = "odds")),
      twokodd = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 2, scale = "odds")),
      threekodd = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 3, scale = "odds")),
      oneknorm = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 1, scale = "normal")),
      twoknorm = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 2, scale = "normal")),
      threeknorm = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 3, scale = "normal"))
    )
    
    legend_lbl <- "Model" # Specify label for the KM here
    
    # Format DF so that it can be added to the fits line lists
    
    survival_KM_line <- broom::tidy(surv_fit) %>%
      mutate(lbl = "Kaplan-Meir")
    
    # Extract coordinates 
    
    spline_fits_line <- list() 
    
    spline_fits_line <- list(
      onekhaz = summary(model_list$onekhaz, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T),
      twokhaz = summary(model_list$twokhaz, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T),
      threekhaz = summary(model_list$threekhaz, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T),
      onekodd = summary(model_list$onekodd, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T),
      twokodd = summary(model_list$twokodd, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T),
      threekodd = summary(model_list$threekodd, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T),
      oneknorm = summary(model_list$oneknorm, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T),
      twoknorm = summary(model_list$twoknorm, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T),
      threeknorm = summary(model_list$threeknorm, t=seq(0, max_extrap, extrap_seq),type="survival",ci=F,tidy=T)
    )
    
    # Labels for each curve saved here for reference later
    
    spline_fits_line$onekhaz$lbl <- "hazard 1 knot"
    spline_fits_line$twokhaz$lbl <- "hazard 2 knot"
    spline_fits_line$threekhaz$lbl <- "hazard 3 knot"
    spline_fits_line$onekodd$lbl <- "odds 1 knot"
    spline_fits_line$twokodd$lbl <- "odds 2 knot"
    spline_fits_line$threekodd$lbl <- "odds 3 knot"
    spline_fits_line$oneknorm$lbl <- "normal 1 knot"
    spline_fits_line$twoknorm$lbl <- "normal 2 knot"
    spline_fits_line$threeknorm$lbl <- "normal 3 knot"
    
    
    # Add KM data frames to the existing list
    
    spline_fits_line <- c(spline_fits_line, list(survival_KM_line = survival_KM_line))
    
    # Assign colours and line types according to labels 
    
    col_list <- c(
      "temp" = "black",
      "hazard 1 knot" = "red", 
      "hazard 2 knot" = "orange", 
      "hazard 3 knot" = "yellow",
      "odds 1 knot" = "cyan", 
      "odds 2 knot" = "blue", 
      "odds 3 knot" = "darkblue",
      "normal 1 knot" = "green", 
      "normal 2 knot" = "lightgreen", 
      "normal 3 knot" = "darkgreen",
      "Kaplan-Meir" = "black") 
    names(col_list)[1] <- legend_lbl
    
    lines_list <- c(
      "temp" = "solid",
      "hazard 1 knot" = "solid", 
      "hazard 2 knot" = "solid", 
      "hazard 3 knot" = "solid",
      "odds 1 knot" = "solid", 
      "odds 2 knot" = "solid", 
      "odds 3 knot" = "solid",
      "normal 1 knot" = "solid", 
      "normal 2 knot" = "solid", 
      "normal 3 knot" = "solid",
      "Kaplan-Meir" = "solid")
    names(lines_list)[1] <- legend_lbl
    
    # PLOT SURVIVAL 
    
    legend_order <- c(
      "hazard 1 knot", 
      "hazard 2 knot", 
      "hazard 3 knot",
      "odds 1 knot", 
      "odds 2 knot", 
      "odds 3 knot",
      "normal 1 knot", 
      "normal 2 knot", 
      "normal 3 knot",
      "Kaplan-Meir")
    
    survival_plot <- ggplot() +
      geom_ribbon(data = spline_fits_line$survival_KM_line, aes(x = time/time_var, y = estimate*100, ymin = conf.low*100, ymax = conf.high*100), fill = "gray", alpha = 0.4)
    
    # Conditional statements for when models fail - do not plot if model has not converged 
    
    if (is.data.frame(spline_fits_line$onekhaz) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = spline_fits_line$onekhaz, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    if (is.data.frame(spline_fits_line$twokhaz) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = spline_fits_line$twokhaz, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    if (is.data.frame(spline_fits_line$threekhaz) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = spline_fits_line$threekhaz, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    if (is.data.frame(spline_fits_line$onekodd) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = spline_fits_line$onekodd, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    if (is.data.frame(spline_fits_line$twokodd) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = spline_fits_line$twokodd, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    if (is.data.frame(spline_fits_line$threekodd) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = spline_fits_line$threekodd, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    if (is.data.frame(spline_fits_line$oneknorm) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = spline_fits_line$oneknorm, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    if (is.data.frame(spline_fits_line$twoknorm) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = spline_fits_line$twoknorm, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    if (is.data.frame(spline_fits_line$threeknorm) == TRUE) {
      survival_plot <- survival_plot + geom_line(data = spline_fits_line$threeknorm, aes(x = time/time_var, y = est*100, col = lbl, lty = lbl), linewidth = 1)
    }
    
    survival_plot <- survival_plot +
      geom_step(data = spline_fits_line$survival_KM_line, aes(x = time/time_var, y = estimate*100, col = lbl, lty = lbl), linewidth = 1) +
      guides(col = guide_legend(title = "", nrow = 3),
             linetype = guide_legend(title = "", nrow = 3),
             fill = guide_legend(title = "", nrow = 3)) +
      scale_colour_manual(values = col_list, breaks = legend_order) + 
      scale_linetype_manual(values = lines_list, breaks = legend_order) + 
      scale_y_continuous(expand = c(0,0), breaks = scales::breaks_extended(n = 6)) +
      scale_x_continuous(expand = c(0,0), breaks = scales::breaks_extended(n = 6)) +
      xlab('Time (years)') + ylab('Survival probability (%)') +
      theme_bw() +
      theme(axis.title.y = element_text(size = 13, angle = 90, face = "bold")) + 
      theme(axis.title.x = element_text(size = 13, angle = 0, face = "bold")) + 
      theme(axis.text.y = element_text(size = 13)) + 
      theme(axis.text.x = element_text(size = 13)) + 
      theme(legend.text = element_text(size = 13)) +
      theme(plot.margin = margin(1,1,1,1, "cm"),
            legend.position = "top",
            axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            panel.background = element_blank())
    
    # Conditional axis titles
    
    if(surv_measure == "PFS") {
      
      survival_plot <- survival_plot +
        ylab('Progression free survival (%)')
      
    } else if (surv_measure == "ToT") {
      
      survival_plot <- survival_plot +
        ylab('Time on treatment (%)')
      
    }
    
    if(save_plot_data == TRUE) {
      
      # Extract the directory path from the vector
      
      directory_path <- dirname(file_path_name)
      
      # Create a new directory called "plot_data" within the "survival" directory
      
      new_directory <- file.path(directory_path, "plot_data")
      dir.create(new_directory, recursive = TRUE, showWarnings = FALSE)
      
      # Add back the file name to the new directory
      
      file_path_name_dat <- file.path(new_directory, basename(file_path_name))
      
      # Change png to xlsx as we are saving Excel file here 
      
      file_path_name_dat <- sub("\\.png$", ".xlsx", file_path_name_dat)
      
      # Create a new Excel workbook
      wb <- createWorkbook()
      
      # Loop through the hazards_list and add each data frame as a new sheet
      for (model_name in names(spline_fits_line)) {
        
        if (is.data.frame(spline_fits_line[[model_name]]) == TRUE) {
          
          df <- spline_fits_line[[model_name]]
          
          # Add the data frame to a new sheet with the model_name as sheet name
          addWorksheet(wb, sheetName = model_name)
          writeDataTable(wb, sheet = model_name, x = df, startCol = 1, startRow = 1)
          
        }
        
      }
      
      # Save the Excel workbook to a file
      saveWorkbook(wb, file_path_name_dat, overwrite = TRUE)
      
    }
    
  } else if(model_type == "simple" & arms == "two") {
    
  } else if(model_type == "spline" & arms == "two") {
    
  }
  
  # SAVE
  
  png(file_path_name,
      units = "mm", width=330, height=220, res = 1000)
  print(survival_plot)
  dev.off()
  
  print(paste0("Survival model plot successfully saved to loaction: ", file_path_name))
  
  # return(survival_plot)
  
  # END
}
