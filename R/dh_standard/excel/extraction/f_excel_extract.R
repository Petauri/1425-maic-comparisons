#' NOTE THAT THIS FUNCTION WAS TAKEN FROM PATT project - code will be publicly available
#' in the future so we should rewrite this at that time.
#' 
#' 
#' Function to automatically extract all named ranges from an excel workbook
#'
#' @param path_to_excel_file the path to the excel file. Ensure that this is normalized for the operating system you are using
#' @param verbose if FALSE (the default), no printing will occur. If TRUE the name and file path of each named range will be printed to trace errors.
#'
f_excel_extract <- function(path_to_excel_file, verbose = FALSE) {
  
  # require(openxlsx)
  
  # Note that pre-loading the workbook to an object vastly improves computational
  # efficiency, because it's in RAM and not on disk. Reading from disk repeatedly
  # is extremely slow and even a small task like this can take time unnecessarily.
  # Load workbook, get the named regions from it ready for lapply.
  
  wb          <- loadWorkbook(path_to_excel_file)
  sheets      <- wb$sheet_names
  named       <- getNamedRegions(wb)
  nams        <- unlist(as.list(named))
  names(nams) <- nams
  
  # Cycle through all the named ranges, making a decision on cleaning as we go
  output <- lapply(nams, function(named_range) {
    
    if (verbose) cat(paste0("Extracting named range ", named_range, " from ", path_to_excel_file, "\n"))
    
    # It is impossible to presume the inclusion or exclusion of headers without
    # further information. This is because if the headers are included in the columns
    # the columns become character data, and tables may contain text in rows
    # so we can't try to convert to numbers to identify whether the header
    # row is erroneously included in the named range. Therefore we assume
    # that all tables for R DO include the column names, and these are converted
    # to R-safe column names (i.e. replacing special characters with dots)
    
    dat <- read.xlsx(wb,namedRegion = named_range, check.names = TRUE,colNames = TRUE)
    
    if (nrow(dat) == 0) {
      # if it has 0 rows, then the input must be a 1 cell one, so we can safely assume
      # no row names
      dat <- read.xlsx(wb,namedRegion = named_range,colNames = FALSE,rowNames = FALSE)
      
      if (all(length(dat) == 1, names(dat)[1] == "X1")) {
        dat <- unlist(dat, use.names = FALSE)
      } else if(nrow(dat) == 1) {
        dat <- unlist(read.xlsx(wb,namedRegion = named_range,colNames = FALSE,rowNames = FALSE), use.names = F)
      }
      
      return(dat)
    } else if(ncol(dat) == 1) {
      # if there's 1 column, then it's actually just a vector:
      dat <- unlist(read.xlsx(wb,namedRegion = named_range,colNames = FALSE,rowNames = FALSE), use.names = F)
    } else {
      # logically we can't identify whether a table has appropriate names or not
      # at this point. this will have to be left to the cleaning functions.
      return(dat)
    }
  })
  
  # Drop the workbook at the end (I think in {} this would happen anyway, but want to ensure!)
  rm(wb)
  return(output)
}