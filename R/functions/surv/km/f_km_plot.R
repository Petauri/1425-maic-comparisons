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
  openxlsx
)

# Requires: f_make_km_data_stepped

f_km_function <- function(timevar, eventvar, stratify, df, surv_measure, km_cols, km_legend_labs, time_int, file_path_name, save_plot_data) {
  
  #' Plot Kaplan-Meier curves by cohort using ggsurvplot
  #'
  #' @description This function generates a Kaplan-Meir plot using the ggsurvplot function from survminer. 
  #' 
  #' NEEDS round_df function loaded (can be found in the dh_standard folder and should be loaded already for those using that)
  #' 
  #' Example code to use this function on the lung dataset can be found in the script "use_example_for_f_km_plot.R" located within the same directory as this function.
  #' 
  #' km_fit should be a survfit object. e.g: survfit(Surv(OS_month, OS_event) ~ status, data = df)
  #' 
  #' df is the dataframe in question (the same as the one used in the survfit)
  #' 
  #' surv_measure: Choose between "OS" or "PFS" or "TOT" (time on treatment)
  #' 
  #' stratify - is the model stratified (e.g., sex)? If yes, then specify TRUE
  #' 
  #' km_cols: a vector of colours depending on how many subgroups there are. e.g. if two subgroups: km_cols <- c("blue", "red")
  #' 
  #' km_legend_labs: as km_cols but for legend labels: km_cols <- c("blue cohort", "red cohort")
  #' 
  #' time_int: a numerical for x axis and risk table breaks 
  #' 
  #' file_path_name: file path and name. e.g.: km_save <- file.path("save_folder", "save_fig.png")
  #' 
  #' save_plot_data: if TRUE, the function will save a CSV of coordinates which can be used to plot KM's in excel (data formatted so that stepped KM's are plotted)
  #'
  #' @details Author: Kurt Taylor, Delta Hat
  
  if(stratify != FALSE) {
    
    # We want to include an "overall" on the graph - if we use "add.all = TRUE" this works, but it doesn't give us median survival in the table so we have to do it this way
    
    df2 <- df
    df2[[stratify]] <- "Overall"
    df <- rbind(df2, df)
    # Check order of stratified var so it appears correctly with corresponding specified colors 
    df[[stratify]] <- factor(df[[stratify]], levels = km_legend_labs)
    
    # SURV FIT 
    # # Creating a formula as a string
    form <<- paste0("Surv(", timevar, ", ", eventvar,") ~ ",stratify)
    km_fit <- survfit(as.formula(form), data=df)
    
  } else {
    
    # SURV FIT 
    # # Creating a formula as a string
    form <<- paste0("Surv(", timevar, ", ", eventvar,") ~ ",1)
    km_fit <- survfit(as.formula(form), data=df)
    
  }
  
  # SURV FIT 
  
  km_plot <- ggsurvplot(km_fit, 
                        data = df, 
                        palette = km_cols,
                        risk.table = TRUE, 
                        fontsize = 3,
                        cumevents = FALSE,
                        # add.all = TRUE,
                        # cumevents.title = "Cumulative No. of Events",
                        risk.table.height = 0.18,
                        cumevents.height = 0.18,
                        break.x.by = time_int, # Set x-axis breaks to X months intervals (90 days)
                        x.tick.by = time_int, # Set x-axis tick marks to X months intervals (90 days)
                        time.inc = time_int, # Set risk table to group by X months intervals (90 days)
                        risk.table.by = time_int, # Set risk table to group by X months intervals (90 days)
                        legend.title = "", # Set legend title
                        legend.labs = km_legend_labs, # Set legend labels
                        tables.y.text = FALSE, # Remove text next to risk table and just have colours
                        surv.median.line = "hv",
                        xlim = c(0, max(km_fit$time*1.2)),
                        xlab = "Time (months)"
  )
  
  # Customise KM aesthetics 
  
  km_plot$table <- km_plot$table +
    theme(plot.title = element_text(size = 10, color = "black", face = "bold"))
  km_plot$cumevents <- km_plot$cumevents +
    theme(plot.title = element_text(size = 10, color = "black", face = "bold"))
  
  km_plot$table$theme$axis.text.y$size <- 8
  km_plot$cumevents$theme$axis.text.y$size <- 8
  km_plot$table$theme$axis.text.x$size <- 8
  km_plot$cumevents$theme$axis.text.x$size <- 8
  km_plot$table$theme$axis.title.x$size <- 8
  km_plot$cumevents$theme$axis.title.x$size <- 8
  
  km_plot$plot <- km_plot$plot +
    scale_y_continuous(expand = c(0.01,0), breaks = seq(0,1,by=0.2), labels = seq(0,100,by=20), minor_breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(expand = c(0.01,0), breaks = seq(0, round(max(km_plot$data.survplot$time*1.2)), time_int), minor_breaks = seq(0, round(max(km_plot$data.survplot$time*1.2)), time_int / time_int)) +
    theme(panel.grid.minor.y = element_line(color = "grey80",
                                            linewidth = 0.5,
                                            linetype = 1),
          panel.grid.minor.x = element_line(color = "grey80",
                                            linewidth = 0.5,
                                            linetype = 1),
          panel.grid.major.y = element_line(color = "grey80",
                                            linewidth = 0.5,
                                            linetype = 1),
          panel.grid.major.x = element_line(color = "grey80",
                                            linewidth = 0.5,
                                            linetype = 1)) +
    theme(legend.text = element_text(size = 12, color = "black"))
  
  km_plot$table <- km_plot$table + layer_scales(km_plot$plot)$x
  
  # Y axis IF statement
  if (surv_measure == "OS") {
    km_plot$plot$labels$y <- "Survival probability (%)"
  } else if (surv_measure == "PFS"){
    km_plot$plot$labels$y <- "Progression free survival probability (%)"
  } else if (surv_measure == "TOT") {
    km_plot$plot$labels$y <- "Proportion of patients still on treatment (%)"
  }
  
  # Add custom table of medians 
  
  median_df <- as.data.frame(surv_median(km_fit))
  median_df$strata <- gsub("^[^=]*=", "", median_df$strata)
  median_df <- median_df[order(match(median_df$strata, km_legend_labs)), ]
  median_df <- round_df(median_df, digits = 1)
  median_df$median[is.na(median_df$median)] <- "NR"
  median_df$lower[is.na(median_df$lower)] <- "NE"
  median_df$upper[is.na(median_df$upper)] <- "NE"
  median_df$`Median survival (95% CI)` <- paste0(median_df$median, " (", median_df$lower, ", ", median_df$upper, ")")
  median_df$median <- NULL
  median_df$lower <- NULL
  median_df$upper <- NULL
  colnames(median_df) <- c("Strata", "Median survival (95% CI)")
  
  km_plot$plot <- km_plot$plot + ggpp::annotate(geom = "table", 
                                                x = max(km_plot$data.survplot$time)*1.1, 
                                                table.theme = ttheme_gtbw, 
                                                y = 1.0, 
                                                label = list(median_df))
  
  # remove axis line from table as it is covering up numbers 
  km_plot$table <- km_plot$table + theme(axis.line.y = element_blank())
  # remove km plot table axis label 
  km_plot$table <- km_plot$table + theme(axis.title.x = element_blank())
  
  # SAVE plot using specified file path
  
  png(file_path_name,
      units = "mm", width=320, height=200, res = 1000)
  print(km_plot)
  dev.off()
  
  print(paste0("KM plot successfully saved to loaction: ", file_path_name))
  
  if (save_plot_data == TRUE) {
    
    # Generate a CSV file of KM coordinates for plotting in Excel
    
    km_data <- km_plot$data.survplot
    
    # Get results for stratified levels 
    
    if(stratify != FALSE) {
      
      # Get unique strata values
      unique_strata <- unique(km_data$strata)
      
      # Initialize an empty dataframe to store the results
      result_df <- data.frame()
      
      # Loop through each unique strata and apply the function
      for (stratum in unique_strata) {
        subset_data <- km_data[km_data$strata == stratum, ]
        stratum_result <- f_make_km_dat_stepped(subset_data)
        stratum_result$strata <- stratum
        result_df <- rbind(result_df, stratum_result)
      }
      
      result_df <- result_df %>%
        dplyr::rename("time" = "t",
                      "surv" = "s_t")
      
    } else{
      
      result_df <- km_data %>%
        dplyr::select(time, surv)
      result_df <- f_make_km_dat_stepped(result_df)
      
    }
    
    # SAVE
    
    # Extract the directory path from the vector
    
    directory_path <- dirname(file_path_name)
    
    # Create a new directory called "plot_data" within the "survival" directory
    
    new_directory <- file.path(directory_path, "plot_data")
    dir.create(new_directory, showWarnings = FALSE)
    
    # Add back the file name to the new directory
    
    file_path_name_dat <- file.path(new_directory, basename(file_path_name))
    
    # Change png to xlsx as we are saving Excel file here 
    
    file_path_name_dat <- sub("\\.png$", ".csv", file_path_name_dat)
    
    write_csv(result_df, file_path_name_dat)
    
    print(paste0("KM coordinate data successfully saved to location: ", file_path_name_dat))
    
  }
  
  
  
  # return(km_plot)
  
}
