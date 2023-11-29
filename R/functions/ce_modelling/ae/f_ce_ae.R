#'
#' Function to compute a rate from the proper components
#' 
#' @param obs The observed number of events
#' @param median_tot median time on treatment 
#' @param N number of patients
#' @param t_mult multiplier on time relative to `median_tot`
#' 
#' 
#' @details Often with adverse events the most accurate way #'          to estimate the rate of adverse reactions 
#'          is to estimate the total time at risk of having
#'          an adverse event, and then dividing the total
#'          observed events by that time. The result is an
#'          estimate of the rate of adverse events per unit
#'          of time. Note that the unit of time for the 
#'          time at risk should be considered carefully.
#'          
#'          e.g., 20 events observed, 0.65 years TOT on
#'          average, 124 people, and I want the rate per
#'          week. This would be `(20/(0.65*124)) * (1/52)`
#'          
#'          Another simpler example could be 5 events seen
#'          in a population of 10 patients, and the average
#'          exposure time to the treatment is 6 months. I
#'          want the rate per year: `(5/(0.5*10)) * 1`, yep
#'          a rate of 1 event per year at risk.
#'          
#'          Recommended that `median_tot` is in years so that
#'          the multiplier applied can be compared to years.
#'          If using an alternative measure be careful with
#'          your conversions!
#'
#'
#' @examples 
#' 
#' f_ce_ae_computeRate(20,0.65,124,1/52)
#' f_ce_ae_computeRate(5,0.5,10,1)
#' f_ce_ae_computeRate(c(5,10,25,13,100),1.3,138,1)
#'
#' @export
#'
f_ce_ae_computeRate <- function(obs, median_tot, N, t_mult) (obs/(median_tot * N)) * t_mult






