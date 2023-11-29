# script for misc functions. 
# 
# 
# THIS SCRIPT COULD BECOME A MASTER SCRIPT FOR MISC FUNCTIONS.
# 
# 
# To do that, make a subfolder for the breakdown if this script gets too long
# (too long is more than a couple of hundred lines!)
# 
# 


#' Function to perform essentially a vlookup. Will return the value from the first
#' column corresponding to the lookup table provided using the nth column. This
#' is a vectorised form which performs a whole column of vlookups in one go.
#' 
#' 
#' @param vals the values to match from the nth column of `table`
#' @param table the lookup table. Values to return from in the 1st column, values to look up using in the others
#' @param col the index of the column to use to look up (e.g. return the corresponding 1st column values from the 3rd column)
#' 
#' @details Function that is kind of parallel to Excel's vlookup function but vectorised.
#'          Allows filling in of blanks in column via `ifelse()` within `mutate` or otherwise.
#'          For instance, if text and numeric categories are mutually exclusive in columns,
#'          place text version in nth column of `table` and numeric in first column. then
#'          use this function. See example below.
#'          
#'          Author: Darren Burns, Delta hat: `dburns@deltahat.com`
#' 
#' @return A vector of values from the first column of `table` where the matches were found.
#' @export
#'
#' @examples
#' data <- data.frame(id = 1:5, name = c("A", "B", "C", "D", "E"))
#' f_vlookup(c("C", "A"), data, 2)
#' 
f_vlookup <- function(vals,table,col) {
  table[match(vals,table[,col]),1]
}

# Example
if (FALSE) {
  # Lookup table
  
  library(data.table)
  library(tidyverse)
  lu_txt <- data.frame(
    n = 1:3,
    txt = c("OS", "PFS", "TTD")
  )
  
  
  rn_nu <- floor(round(runif(10) * 2))+1
  rn_txt <- sample(c("OS", "PFS", "TTD"),10,replace = TRUE)
  
  rn_nu[c(1,3,5)] <- NA
  rn_txt[c(2,4,6:10)] <- NA
  
  # DF to play with
  df <- data.frame(
    nums = rn_nu,
    txt  = rn_txt
  )
  
  # Replace nums looking up values in the lookup table:
  df %>% 
    mutate(nums = ifelse(!is.na(nums),nums,f_vlookup(txt,lu_txt,2)))
  
  
  # Basic use of function
  f_vlookup(df$txt,lu_txt,2)
}







