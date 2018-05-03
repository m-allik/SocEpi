#' Calculate slope/beta of an OLS regression using matrix algebra.
#' This will be the Slope Index of Inequality (SII).
#'
#' @param Y standardized rates for calculation
#' @param x population distribution
#' @param a alpha, default \code{a=1}
#'
#' @return Slope of an OLS regression.
#'


beta <- function(a=1, x, Y){
  X <- cbind(a, x)
  beta <- t(solve(t(X)%*%X)%*%t(X)%*%Y)[,2]
  return(beta)
}
