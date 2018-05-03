#' Calculates midpoints for population distribution.
#'
#' \code{popmid} calculates the (cumulative) midpoints for population percentiles that can then be used as the
#'   predictor in the OLS model for calculating the slope or the relative index of inequality (SII).
#'   The function is mainly for internal use within the \code{rii} function.
#' @param x vector of population distribution that sums up to 1
#'
#' @return A vector of population midpoints
#'

popmid <- function(x) {
  c(0, cumsum(as.numeric(x))[-length(x)]) + x/2
}
