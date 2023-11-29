# Function to extract parameters from models 

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

f_hazard_plot_extrapolated <- function(df, survival_formula, model_type, arms, max_extrap, extrap_seq, conv_to_years, file_path_name, save_plot_data) {
  
  #' Plot hazards from survival models
  #'
  #' @description This function plots hazards from survival models (simple or flexible (splines)) for comparison
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
  #' file_path_name: file path and name. e.g.: km_save <- file.path("save_folder", "save_fig.png")
  #'
  #' save_plot_data: if TRUE, the function will save an xlsx of all the datasets used to create the plots (e.g., for reproducing plots in Excel if desired)
  #'
  #' @details Author: Kurt Taylor, Delta Hat
  #' 
  
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
    
    # Labels 
    
    model_list$parametric_exp$lbl <- "Exponential"
    model_list$parametric_gengamma$lbl <- "Generalised gamma"
    model_list$parametric_gompertz$lbl <- "Gompertz"
    model_list$parametric_llogis$lbl <- "Log-logistic"
    model_list$parametric_lnorm$lbl <- "Log-normal"
    model_list$parametric_weibull$lbl <- "Weibull"
    model_list$parametric_gamma$lbl <- "Gamma"
    
    legend_lbl <- "Model" # Specify label for the KM here
    
    # HAZARD PLOT
    
    hazards_list <- list()
    
    # # Extract coordinates from models for plotting
    
    for (b in 1:length(model_list)){
      model_name <- names(model_list)[b]  # Get the name of the model
      hazards_list[[model_name]] <- summary(model_list[[b]], t=seq(0, max_extrap, extrap_seq), type="hazard", ci=FALSE, tidy=TRUE)
      hazards_list[[model_name]]$Strata <- model_list[[b]]$lbl[1]
    }
    
    # for (b in 1:length(model_list)){
    #   hazards_list[[b]] <- summary(model_list[[b]], t=seq(0, max_extrap, extrap_seq),type="hazard",ci=FALSE,tidy=TRUE)
    #   hazards_list[[b]]$Strata <- model_list[[b]]$lbl[1]
    # } 
    
    # Get observed hazards using muhaz to add to the plot
    
    # muhaz_fit <- muhaz2(survival_formula, df)
    # 
    # #extract hazard estimates and times
    # muhaz_extract <- data.frame(Hazard = muhaz_fit$haz.est, Time = muhaz_fit$est.grid)
    # muhaz_extract <- muhaz_extract %>%
    #   dplyr::rename(est = Hazard,
    #                 time = Time) %>%
    #   dplyr::mutate(Strata = "KM hazard smooth")
    # 
    # hazards_list <- c(list(muhaz_smooth = muhaz_extract), hazards_list)
    
    # and bshazard
    
    as.data.frame.bshazard <- function(x, ...) {
      with(x, data.frame(time,hazard,lower.ci,upper.ci))
    }
    
    bshazard_fit <- as.data.frame(bshazard(survival_formula, df))
    bshazard_fit$est <- bshazard_fit$hazard
    bshazard_fit$Strata <- "KM hazard smooth (95% CI)"
    bshazard_fit$hazard <- NULL
    
    hazards_list <- c(hazards_list, list(bshazard_smooth = bshazard_fit))
    
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
      "KM hazard smooth (95% CI)" = "black") 
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
      "KM hazard smooth (95% CI)" = "dashed")
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
      "KM hazard smooth (95% CI)")
    
    # Generate plot 
    
    hazard_plot <- ggplot() +
      geom_ribbon(data = hazards_list$bshazard_smooth, aes(x = time/time_var, y = est, ymin = lower.ci, ymax = upper.ci), fill= "gray", alpha=0.4)
    
    # Conditional statements for when models fail - do not plot if model has not converged 
    
    if (is.data.frame(hazards_list$parametric_exp) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$parametric_exp, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    if (is.data.frame(hazards_list$parametric_gengamma) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$parametric_gengamma, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    if (is.data.frame(hazards_list$parametric_gompertz) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$parametric_gompertz, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    if (is.data.frame(hazards_list$parametric_llogis) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$parametric_llogis, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    if (is.data.frame(hazards_list$parametric_lnorm) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$parametric_lnorm, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    if (is.data.frame(hazards_list$parametric_weibull) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$parametric_weibull, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    if (is.data.frame(hazards_list$parametric_gamma) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$parametric_gamma, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    hazard_plot <- hazard_plot +
      geom_line(data = hazards_list$bshazard_smooth, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1) +
      guides(col = guide_legend(title="", nrow=3),
             linetype = guide_legend(title="", nrow=3),
             fill = guide_legend(title="", nrow=3)) +
      scale_colour_manual(values=col_list, breaks = legend_order) + 
      scale_linetype_manual(values=lines_list, breaks = legend_order) + 
      scale_y_continuous(expand = c(0,0), breaks = scales::breaks_extended(n = 6)) +
      scale_x_continuous(expand = c(0,0), breaks = scales::breaks_extended(n = 6)) +
      # scale_fill_manual(values=fill_list) +
      xlab('Time (years)') + ylab('Hazard') +
      theme_bw() +
      theme(axis.title.y = element_text(size = 13, angle = 90, face = "bold")) + 
      theme(axis.title.x = element_text(size = 13, angle = 0, face = "bold")) + 
      theme(axis.text.y = element_text(size = 13)) + 
      theme(axis.text.x = element_text(size = 13)) + 
      theme(legend.text = element_text(size = 13)) +
      theme(plot.margin = margin(1,1,1,1, "cm"),
            legend.position = "top",
            axis.line = element_line(colour = "black"),
            # panel.grid.major = element_blank(),
            # panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
    
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
      for (model_name in names(hazards_list)) {
        
        if (is.data.frame(hazards_list[[model_name]]) == TRUE) {
          
          df <- hazards_list[[model_name]]
          
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
    
    # Labels 
    
    model_list$onekhaz$lbl <- "hazard 1 knot"
    model_list$twokhaz$lbl <- "hazard 2 knot"
    model_list$threekhaz$lbl <- "hazard 3 knot"
    model_list$onekodd$lbl <- "odds 1 knot"
    model_list$twokodd$lbl <- "odds 2 knot"
    model_list$threekodd$lbl <- "odds 3 knot"
    model_list$oneknorm$lbl <- "normal 1 knot"
    model_list$twoknorm$lbl <- "normal 2 knot"
    model_list$threeknorm$lbl <- "normal 3 knot"
    
    # HAZARD PLOT
    
    hazards_list <- list()
    
    # # Extract coordinates from models for plotting
    
    for (b in 1:length(model_list)){
      model_name <- names(model_list)[b]  # Get the name of the model
      hazards_list[[model_name]] <- summary(model_list[[b]], t=seq(0, max_extrap, extrap_seq), type="hazard", ci=FALSE, tidy=TRUE)
      hazards_list[[model_name]]$Strata <- model_list[[b]]$lbl[1]
    }
    
    # Get observed hazards using muhaz to add to the plot - leaving code here in case someone using function specifically wants muhaz over bshazard 
    
    # muhaz_fit <- muhaz2(survival_formula, df)
    # 
    # #extract hazard estimates and times
    # muhaz_extract <- data.frame(Hazard = muhaz_fit$haz.est, Time = muhaz_fit$est.grid)
    # muhaz_extract <- muhaz_extract %>%
    #   dplyr::rename(est = Hazard,
    #                 time = Time) %>%
    #   dplyr::mutate(Strata = "KM hazard smooth")
    # 
    # hazards_list <- c(list(muhaz_smooth = muhaz_extract), hazards_list)
    
    # and bshazard
    
    as.data.frame.bshazard <- function(x, ...) {
      with(x, data.frame(time,hazard,lower.ci,upper.ci))
    }
    
    bshazard_fit <- as.data.frame(bshazard(survival_formula, df))
    bshazard_fit$est <- bshazard_fit$hazard
    bshazard_fit$Strata <- "KM hazard smooth (95% CI)"
    bshazard_fit$hazard <- NULL
    
    hazards_list <- c(hazards_list, list(bshazard_smooth = bshazard_fit))
    
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
      "KM hazard smooth (95% CI)" = "black") 
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
      "KM hazard smooth (95% CI)" = "dashed")
    names(lines_list)[1] <- legend_lbl
    
    # Legend order
    
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
      "KM hazard smooth (95% CI)")
    
    # Generate plot 
    
    hazard_plot <- ggplot() +
      geom_ribbon(data = hazards_list$bshazard_smooth, aes(x = time/time_var, y = est, ymin = lower.ci, ymax = upper.ci), fill= "gray", alpha=0.4) 
    
    # Conditional statements for when models fail - do not plot if model has not converged 
    
    if (is.data.frame(hazards_list$onekhaz) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$onekhaz, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    if (is.data.frame(hazards_list$twokhaz) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$twokhaz, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    if (is.data.frame(hazards_list$threekhaz) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$threekhaz, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    if (is.data.frame(hazards_list$onekodd) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$onekodd, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    if (is.data.frame(hazards_list$twokodd) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$twokodd, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    if (is.data.frame(hazards_list$threekodd) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$threekodd, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    if (is.data.frame(hazards_list$oneknorm) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$oneknorm, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    if (is.data.frame(hazards_list$twoknorm) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$twoknorm, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    if (is.data.frame(hazards_list$threeknorm) == TRUE) {
      hazard_plot <- hazard_plot + geom_line(data = hazards_list$threeknorm, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1)
    }
    
    hazard_plot <- hazard_plot +
      geom_line(data = hazards_list$bshazard_smooth, aes(x = time/time_var, y = est, colour = Strata, linetype = Strata), size = 1) +
      guides(col = guide_legend(title="", nrow=3),
             linetype = guide_legend(title="", nrow=3),
             fill = guide_legend(title="", nrow=3)) +
      scale_colour_manual(values=col_list, breaks = legend_order) + 
      scale_linetype_manual(values=lines_list, breaks = legend_order) + 
      scale_y_continuous(expand = c(0,0), breaks = scales::breaks_extended(n = 6)) +
      scale_x_continuous(expand = c(0,0), breaks = scales::breaks_extended(n = 6)) +
      # scale_fill_manual(values=fill_list) +
      xlab('Time (years)') + ylab('Hazard') +
      theme_bw() +
      theme(axis.title.y = element_text(size = 13, angle = 90, face = "bold")) + 
      theme(axis.title.x = element_text(size = 13, angle = 0, face = "bold")) + 
      theme(axis.text.y = element_text(size = 13)) + 
      theme(axis.text.x = element_text(size = 13)) + 
      theme(legend.text = element_text(size = 13)) +
      theme(plot.margin = margin(1,1,1,1, "cm"),
            legend.position = "top",
            axis.line = element_line(colour = "black"),
            # panel.grid.major = element_blank(),
            # panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
    
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
      for (model_name in names(hazards_list)) {
        
        if (is.data.frame(hazards_list[[model_name]]) == TRUE) {
          
          df <- hazards_list[[model_name]]
          
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
  print(hazard_plot)
  dev.off()
  
  print(paste0("Modelled hazard plots successfully saved to loaction: ", file_path_name))
  
  # return(hazard_plot)
  
  # END
}
