
f_make_km_dat_stepped <- function(DATA) {
  # Make KM testing ground
  # Load example data and do survfit
  z <- DATA
  # y <- Surv(x[,timevar], x[,eventvar])
  # z <- survfit(y ~ 1)
  
  # Make adjustments to data required for graph (i.e. double both datasets)
  t   <- rep(z$time, 2)
  s_t <- rep(z$surv, 2)
  
  # Sort parameters as required (by time ascending, by survival descending), adding extra data points
  t   <- append(0, t[order(t)])
  s_t <- append(1, append(1, s_t[order(s_t, decreasing = TRUE)]))[1:length(t)]
  
  # Create a dataframe and return it
  df <- data.frame(t = t, s_t = s_t)
  return(df)
}
