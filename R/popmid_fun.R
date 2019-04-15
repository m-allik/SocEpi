#' Population midpoints
#'
#' \code{popmid} calculates the cumulative midpoints for population percentiles that can then be used as the
#'   predictor in the OLS model for calculating the slope, i.e. the slope index of inequality (SII).
#'   For internal use within the \code{rii} function.
#' @param x Vector of population distribution that sums to 1.
#'
#' @return A vector of population midpoints.
#'
#' @keywords internal
#'

popmid <- function(x) {
  c(0, cumsum(as.numeric(x))[-length(x)]) + x/2
}
