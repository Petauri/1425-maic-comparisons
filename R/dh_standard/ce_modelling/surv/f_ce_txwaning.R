
#' Gradual treatment effect waning using linear interpolation of transition probabilities
#'
#' 
#'
#' @examples
#' t <- 0:1000 * (1/52)
#' 
#' f_gradual_waning(
#'   t         = t,
#'   ex_tx     = 1-pexp(q = t,rate = 0.4),
#'   ex_con    = 1-pexp(q = t,rate = 0.5),
#'   start     = 2,
#'   end       = 5,
#'   remaining = 0
#' )
#' 
#'
f_gradual_waning <- function(t, ex_tx, ex_con, start, end, remaining = 0) {
  
  requireNamespace("data.table")
  
  t_step <- (t[2] - t[1])
  
  n_steps <- (end - start) * (1/t_step)
  v_steps <- seq(1, remaining, length.out = n_steps)
  
  teff <- c(
    rep(1, start * (1/t_step)),
    v_steps,
    rep(remaining, length(t) - (end* (1/t_step)))
  )
  
  # tp = 1 - s(t) / s(t-1)
  
  tp_tx  <- 1 - (ex_tx  / data.table::shift(ex_tx,fill  = 1))
  tp_con <- 1 - (ex_con / data.table::shift(ex_con,fill = 1))
  
  tp_w   <- (tp_tx * teff) + (tp_con * (1-teff))
  
  ex_w   <- cumprod(c(1,1-tp_w[2:length(t)]))
  
  return(list(
    weighting     = teff,
    probs         = tp_w,
    extrapolation = ex_w
  ))
}






