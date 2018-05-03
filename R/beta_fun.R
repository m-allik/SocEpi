#' Calculate slope/beta of an OLS regression using matrix algebra.
#' This will be the Slope Index of Inequality (SII).
#'
#' @param x population distribution
#' @param y standardized rates for calculation
#' @param w weight, default \code{NULL}
#'
#' @return Slope of an OLS regression.
#'


beta <- function(x, y, w=NULL){

  X <- cbind(1, x)

  if (is.null(w)) {
   beta <- t(solve(t(X)%*%X)%*%t(X)%*%y)[, 2]
  } else {
   beta <- t(solve(t(X)%*% diag(w) %*%X)%*%t(X)%*% diag(w) %*%y)[, 2] #for weighted data
  }

  return(beta)
}
