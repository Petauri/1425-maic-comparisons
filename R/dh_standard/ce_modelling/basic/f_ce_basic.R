# Simple functions to do some general cost-effectiveness
# functions. These should be useful for most if not all
# CE models


#' Calculate discount factors
#'
#' @param r the annual discount rate to apply
#' @param cl cycle length in years
#' @param method method to apply (default is annual blocks)
#' @param th time horizon in CYCLES
#' 
#' 
f_ce_basic_discFac <- function(r, cl, method, th) {
  
  # Compute the time vector
  t <- 0:(th-1)
  t_yr <- t * cl
  
  # Depending on the method, compute the discoun 
  
  switch (
    method,
    "annual blocks" = 1/((1+r)^floor(t_yr)),
    "continuous time per cycle discounting" = 1/((1+r)^t_yr),
    "continuous discounting" = exp(-r*t_yr)
  )
  
}




