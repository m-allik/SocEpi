#' Function to create population weighted deprivation percentiles
#'
#' @param data name of the dataset
#' @param population distribution (in numbers)
#' @param variable the deprivation measure to calculate the percentiles for
#' @param p the number of percentiles/groups to split the data in. Default is deciles \code{p=10}.
#' @param low should low values of the percentile correspond to high deprivation, defaults to FALSE --
#' higher values correspond to higher deprivation
#' @return Numeric vector of percentiles (default 1 to 10) of same length as \code{population} and \code{variable}
#'
#' @importFrom stats sd
#'
#' @export
#' @examples
#' data <- dep_data
#'
#' #calculate deciles for overcrowding
#' data$dec_overcrowd <- w_pcntile(data, total_pop, pcnt_overcrowding)
#'
#' #average percent of overcrowding by decile
#' tapply(data$pcnt_overcrowding, data$dec_overcrowd, mean)
#'
#' #percent of people in each decile
#' round(tapply(data$total_pop, data$dec_overcrowd, sum)/sum(data$total_pop)*100, 1)
#'
#' #calculate quintiles from deciles
#' data$Q_overcrowd <- cut(data$dec_overcrowd, breaks=5, labels=1:5)
#'
#' #calculate quintiles with w_pcntile
#' data$Q_overcrowd2 <- w_pcntile(data, total_pop, pcnt_overcrowding, p=5)
#'
#' #compare results - note small differences
#' table(data$Q_overcrowd, data$Q_overcrowd2)


w_pcntile <- function(data, population, variable, p=10, low=F) {



  # function starts - subset data
  df <- subset_q(data, NULL, substitute(c(population, variable))) #select data from data frame
  names(df) <- c("population", "variable") #give names to use within function

  # number of cases
  N <- length(df$population)
  df$index <- 1:N

  # Remove missing data
  if (any(is.na(df$variable))) {
    has.missing <- TRUE
    original.index <- df[, "index", drop = F]
    df <- df[!is.na(df$variable), ]

    # if ()... deal with missing population variable?
  } else {has.missing <- FALSE}


  # Order dataset
  df <- df[order(df$variable, decreasing=low), ]

  # calculate total population and cut-points/size of the decile
  pop <- sum(df$population)
  cp <- pop/p + mean(df$population)/2 + sd(df$population)/2

  # pcnt_loop is a c++ function within the package
  df$decile <- pcnt_loop(df$population, df$variable, p, cp)

  if (has.missing) {
    # add data to original index, thereby padding again with NA values
    df <- merge(original.index, df, by = "index", all=T)

  }

  # order data by index
  df <- df[order(df$index),]

  return(df$decile)
}
