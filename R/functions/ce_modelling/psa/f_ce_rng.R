
#' Modification of `gtools::rdirichlet` to allow uniform draws to be provided
#' 
#' 
#' @param rands vector of uniform draws of length divisible by length of alpha
#' @param alpha vector of integer values of observed frequency for dirichlet distribution
#' 
#' 
#' @examples
#' 
#' # example code
#' set.seed(1354)
#' qdirichlet(
#'   rands = runif(99),
#'   alpha = c(52, 45, 12)
#' )
#' 
#' @export
#' 
qdirichlet <- function (rands, alpha) {
  
  l <- length(alpha)
  
  stopifnot(length(rands)%%l==0)
  
  x <- matrix(qgamma(rands,alpha), ncol = l, byrow = TRUE)
  
  sm <- x %*% rep(1, l)
  
  x / as.vector(sm)
}


