#' Calculates z-scores
#'
#' \code{zscores} calculates standardized scores with a mean close to zero and standard deviation close to one
#' for the variable given in \code{variable}.
#' The standardization uses the population weighted mean and standard deviation, which are calculated based on the
#' population distribution given by \code{population}.
#' @param population distribution (in numbers)
#' @param variable the deprivation measure to calculate the z-scores for
#'
#' @return A list including the following:
#'  \item{z.score}{standardized score using weighted mean and sd}
#'  \item{w.mean}{weighted mean}
#'  \item{w.sd}{weighted standard deviation}
#'  \item{weight}{population weight}
#' @export
#'
#'
#' @examples
#' data <- dep_data
#'
#' #store all data in object z_oz
#' z_oz <- zscore(data$total_pop, data$pcnt_overcrowding)
#'
#' #extract z-score
#' data$z_overcrowd <- z_oz$z.score
#' mean(data$z_overcrowd) #mean of z-score
#' sd(data$z_overcrowd) #sd z-score
#'
#' #compare weighted mean to actual mean
#' mean(data$pcnt_overcrowding)
#' sd(data$pcnt_overcrowding)


zscore <- function(population, variable) {

  data <- data.frame(population, variable)

  #weights
  data$weight <- data$population/sum(data$population, na.rm=T)

  #calculate weighted mean and sd
  w.mean <- sum(data$variable*data$weight, na.rm=T)
  x <- ((data$variable - w.mean)^2)*data$weight
  w.sd <- sqrt(sum(x, na.rm=T))

  #z-score
  data$z.score <- (data$variable - w.mean)/w.sd

  cat("Weighted mean =", w.mean, "\n", "Weigted SD =", w.sd, "\n")

  output <-list("z.score" = data$z.score, "w.mean" = w.mean, "w.sd" = w.sd, "weight" = data$weight)
  #output <- cbind(w.mean, w.sd)
  return(output)

  }
