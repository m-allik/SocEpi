#' Multinomial simulation for CIs
#'
#' \code{CI_multinom} simulates CI using the multinomial distribution, internal function.
#'
#'
#' @param df Data frame of observed health counts and population by deprivation.
#' @param N Number of simulations.
#' @param n_g Number of deprivation categories/groups.
#'
#' @return Data frame of simulated confidence intervals.
#'
#' @keywords internal
#'
#' @importFrom stats rmultinom
#'

CI_multinom <- function(df, N, n_g) {

  age_g <- unique(df$age)[order(unique(df$age))] # order age groups
  probs <- tapply(df$health, list(df$age, df$ses), sum)
  probs[probs==0] <- 0.01
  probs[is.na(probs)] <- 0.01
  rs <- rowSums(probs)
  probs <- cbind(probs/rs, rs)

  pop <- tapply(df$population, list(df$age, df$ses), sum)
  pop[pop==0] <- 1

  #simulated data
  Ds <- apply(probs, 1, function(f) {rmultinom(N, f[(n_g + 1)], f[1:n_g])})
  Ds <- lapply(split(Ds, rep(c(1:N), each=n_g)), matrix, ncol=n_g, byrow=T)
  Ds[] <- lapply(Ds, function(f) {f/pop})
  Ds <- data.frame(sim=rep(1:N, each=length(age_g)), age=rep(age_g, N), do.call(rbind, Ds), row.names = NULL)

  return(Ds)
}
