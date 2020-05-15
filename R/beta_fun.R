#' Slope of an OLS regression
#'
#' \code{beta} will calculate the slope of an OLS regression using matrix algebra.
#' This will be the Slope Index of Inequality (SII). Weighted least squares slope is calculated
#' when weights are supplied.
#'
#' @param x Population distribution.
#' @param y Standardized rates for calculation.
#' @param w Weight for weighted least squares regression, default \code{NULL}.
#'
#' @return Slope of an OLS regression.
#'
#' @keywords internal
#'


beta <- function(x, y, w = NULL){

  X <- cbind(1, x)

  if (is.null(w)) {
   beta <- t(solve(t(X)%*%X)%*%t(X)%*%y)[, 2]
  } else {
   beta <- t(solve(t(X)%*% diag(w) %*%X)%*%t(X)%*% diag(w) %*%y)[, 2] #for weighted data
  }

  return(beta)
}
