f_maic_pathway_figure <- function(directory_path, label_name){
  
  #***********************************************************************
  # Read results in and create table -------------------------------------
  #***********************************************************************
  
  # Define MAIC reference (folder name in results)
  # Could add multiple here in a vector if we wanted to iterate in future 
  
  # maic_ref <- c("Efanesotcocog-VonDrygalski-etal")
  
  # Specify the directory containing your Excel files
  # directory_path <- file.path(base_folder, "3. Project work", "4. Task 5 - MAIC", "2. Results", version, maic_ref)
  
  # Get the list of Excel files in the specified directory using "match"
  excel_files <- list.files(directory_path, pattern = "match", full.names = TRUE)
  
  # Create a function to read the "Characteristics" sheet and name the data frame
  read_characteristics_sheet <- function(file_path) {
    # Extract the file name without extension
    file_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Read the "Characteristics" sheet
    characteristics_sheet <- read_excel(file_path, sheet = "Characteristics")
    
    # Append extra text to the new column name
    new_column_name <- paste0(file_name, " indicator")
    
    # Rename the "ILD weighted" column to the file name
    characteristics_sheet <- characteristics_sheet %>%
      dplyr::rename(!!file_name := `ILD weighted`) %>%
      dplyr::rename(!!new_column_name := `Matched`)
    
    return(characteristics_sheet)
  }
  
  # Use lapply to apply the function to each Excel file
  characteristics_data_list <- lapply(excel_files, read_characteristics_sheet)
  
  # Use reduce to merge the data frames in characteristics_data_list
  results_df <- reduce(characteristics_data_list, merge)
  
  # Reorder data frame based on characteristic column 
  
  # Firstly order on number of TRUE values, then separate out 
  
  results_df <- results_df[order(-rowSums(results_df[, grepl("indicator", colnames(results_df))])), ]
  
  # Get the first three values
  first_three_values <- c("N Patients", "ESS", "ESS (%)")
  
  # Identify the rest of the unique values in the "Characteristic" column
  remaining_values <- setdiff(unique(results_df$Characteristic), first_three_values)
  
  # Combine the first three values and the remaining values to create the desired order
  desired_order <- c(first_three_values, remaining_values)
  
  # Reorder the levels of the "Characteristic" column based on the desired order
  results_df$Characteristic <- factor(results_df$Characteristic, levels = desired_order)
  
  # Sort the dataframe based on the new order of levels in the "Characteristic" column
  results_df <- results_df[order(results_df$Characteristic), ]
  
  #***********************************************************************
  # Design table -------------------------------------
  #***********************************************************************
  
  # Characteristics excluding N Patients, ESS and ESS %
  
  chars_vars <- as.character(results_df$Characteristic)
  names_to_remove <- c("N Patients", "ESS", "ESS (%)")
  chars_vars <- chars_vars[! chars_vars %in% names_to_remove]
  
  # Create a gt table
  gt_table <- gt(results_df)
  
  # Define spanner columns
  match_string <- "^match-[0-9]+$"
  spanner_cols <- colnames(results_df)[grepl(match_string, colnames(results_df))]
  
  # Apply styling based on conditions
  for (i in seq_along(spanner_cols)) {
    col <- spanner_cols[i]
    gt_table <- gt_table %>%
      tab_spanner(
        label = paste0("spanner_", i),
        columns = all_of(col)
      ) %>%
      tab_style(
        style = cell_fill(color = "grey"),
        locations = cells_body(
          columns = col,
          rows = !results_df[[paste0(col, " indicator")]]
        )
      ) %>%
      tab_style(
        style = cell_fill(color = "red", alpha = 0.7),
        locations = cells_body(
          columns = col,
          rows = results_df[[col]] < 50 & results_df$Characteristic == "ESS (%)"
        )
      ) %>%
      tab_style(
        style = cell_fill(color = "yellow", alpha = 0.7),
        locations = cells_body(
          columns = col,
          rows = results_df[[col]] < 75 & results_df[[col]] > 50 & results_df$Characteristic == "ESS (%)"
        )
      ) %>%
      tab_style(
        style = cell_fill(color = "lightgreen", alpha = 0.7),
        locations = cells_body(
          columns = col,
          rows = results_df[[col]] < 90 & results_df[[col]] > 75 & results_df$Characteristic == "ESS (%)"
        )
      ) %>%
      tab_style(
        style = cell_fill(color = "darkgreen", alpha = 0.7),
        locations = cells_body(
          columns = col,
          rows = results_df[[col]] > 90 & results_df$Characteristic == "ESS (%)"
        )
      ) %>%
      tab_style(
        style = cell_text(color = "black"),
        locations = cells_body(
          columns = col,
          rows = (
            abs(results_df[[col]] - results_df[[label_name]]) / results_df[[col]] <= 0.10
          ) & results_df$Characteristic %in% chars_vars
        )
      ) %>%
      # Conditions for variables within 10 % 
      tab_footnote(
        footnote = "The value of a variable is within 10% of the comparators value for the same variable",
        locations = cells_body(
          columns = col,
          rows = (
            abs(results_df[[col]] - results_df[[label_name]]) / results_df[[col]] <= 0.10
          ) & results_df$Characteristic %in% chars_vars
        )
      ) %>%
      # Make asterix instead of number 
      opt_footnote_marks(marks = "standard") %>%
      # Left align
      cols_align(
        align = c("left"),
        columns = everything()
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "bottom",
          color = "black"),
        locations = cells_body(
          columns = everything(),
          rows = (
            results_df$Characteristic == "ESS (%)"
          )
        )
      ) %>%
      # Format N patient row to 0 decimal places 
      fmt_number(rows = (results_df$Characteristic == "N Patients"),
                 decimals = 0) %>%
      # and same for ESS %
      fmt_number(rows = (results_df$Characteristic == "ESS (%)"),
                 decimals = 0) %>%
      # and same for ESS 
      fmt_number(rows = (results_df$Characteristic == "ESS"),
                 decimals = 0) %>%
      # Center align column names 
      # tab_style(
      #   style = cell_text(align = "center"),
      #   locations = cells_column_labels(columns = everything())) %>%
      # Font size and style 
      opt_table_font(
        font = "Trebuchet MS",
      ) %>%
      tab_options(
        table.font.size = px(10L)
      ) %>%
      # Footnote 
      tab_footnote(
        footnote = "ESS fill: Red = ESS is 50% or less than the original sample size. Yellow = ESS between 50% and 75%. Light green = ESS between 75% and 90%. Dark green = ESS is 90% or more of the original sample size."
      ) %>%
      tab_footnote(
        footnote = "Grey fill = The variable was not matched on."
      )
    
  }
  
  # Remove spanner labels
  gt_table <- gt_table %>%
    rm_spanners() %>%
    # format missing values
    sub_missing(
      missing_text = "-"
    )
  
  # Remove indicator columns
  gt_table <- gt_table %>%
    cols_hide(
      columns = ends_with(" indicator")
    )
  
  # Display the table
  gt_table
  
  # SAVE 
  
  # PNG
  gtExtras::gtsave_extra(gt_table, 
                         file = file.path(paste0(directory_path), "maic_markup.png"),
                         expand = c(0, 1000, 0, 1000))
  # DOCX
  gtsave(gt_table,
         file = file.path(paste0(directory_path), "maic_markup.docx"))
  # HTML
  gtsave(gt_table,
         file = file.path(paste0(directory_path), "maic_markup.html"))
  
} # end of function