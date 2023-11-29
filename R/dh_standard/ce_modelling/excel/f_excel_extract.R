# The functions in this script deal with extracting information from Microsoft
# Excel. 
# 


#' Extract all named ranges from a Microsoft Excel spreadsheet and put the 
#' results in a list
#' 
#' 
f_ce_excel_extractNamedRanges <- function(path_to_wb = NULL) {
  
  # Validation - openxlsx package must be installed, path must be to a file.
  requireNamespace("openxlsx")
  requireNamespace("rstudioapi")
  
  # if the user does not provide path_to_wb, then they want a popup to choose it!
  if (!is.null(path_to_wb)) {
    stopifnot(file.exists(path_to_wb))
  } else {
    path_to_wb <- rstudioapi::selectFile(
      caption = "Select excel file to extract named ranges from",
      label = "Excel file",
      existing = TRUE,
      filter = "Excel Files (*.xlsm;*.xlsx)"
    )
    stopifnot(file.exists(path_to_wb))
  }
  

  # Load the workbook as an object
  wb <- openxlsx::loadWorkbook(path_to_wb)
  
  sheets      <- wb$sheet_names
  named       <- openxlsx::getNamedRegions(wb)
  nams        <- unlist(as.list(named))
  names(nams) <- nams
  
  
  # ...
  
  
  
}
