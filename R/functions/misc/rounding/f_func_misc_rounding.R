# Functions to help with rounding.
# 
# 



# ROUNDING FOR REPORTING ONLY ---------------------------------------------

# DO NOT USE THESE FUNCTIONS FOR ANY STATISTICS EVER!!!
# 
# It is actually correct to round 0.45 to 0.4 to 1 dp WHEN DEALING WITH MULTIPLE
# VALUES. This is why this convention is in R.
# 
# However, when reporting individual numbers this is no longer the case. 
# 
# 
# Therefore the function below rounds 0.45 to 0.5 to 1dp. 
# 
# This is important when presenting values in documents and outputs.
# 
# 


#' round 0.45 up to 0.5 at 1dp by adding a really small number to it
#' 
#' @param x a value or vector of values. Will also work on a numeric matrix.
#' @param digits the number of decimal places to round to.
#' 
round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

#' Round a data.frame using the reporting function ONLY EVER USE THIS FOR REPORTING NOT FOR STATS
#' 
#' @param df a `data.frame` object
#' @param digits the number of decimal places to round to
#' 
round_df <- function(df, digits) {
  stopifnot("data.frame" %in% class(df))
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round2(df[,nums], digits = digits)
  return(df)
}



