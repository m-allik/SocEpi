#' Age grouping function
#'
#' Age grouping function creates standard population weights for user selected age groups.
#' Allows for flexible age groups selection in smr, rii and st_rate functions. Internal use only.
#'
#' @param age_group character string of age groups for which the results are to be provided.
#' @param st_pop standard population weights to use.
#' @param min_age minimum age group (to check if it is 0 or 0-4).
#' @param n_age number of age groups.
#'
#' @return A small data frame of weights for user selected age groups.
#'
#' @keywords internal
#'

age_grouping <- function(age_group, st_pop, min_age, n_age) {

  #define standard population weights
 if (!is.null(st_pop)) {
   if (is.numeric(st_pop)) {
      sp = st_pop
    } else {
      sp = stn_data[[st_pop]]
    }
 }

  ##age groups
  if (!is.null(age_group)) {
    age1 <- c(0, 1, seq(5, 95, 5))
    age2 <- c(0, seq(4, 99, 5))
    Ta <- data.frame(age1, age2)

    a_n <- length(age_group)
    a <- strsplit(age_group, "-", fixed=TRUE)

    if (length(a[[a_n]])==1) {a[[a_n]][2] <- 100}

    g1 <- NULL
    for (i in 1:a_n) {
      g1[as.numeric(a[[i]][1]) <= Ta$age1 & as.numeric(a[[i]][2]) >= Ta$age2] <- i
    }

  } else {
    g1 <- c(1, rep(1:5, each=3), rep(6, 6))
  }

  ##adjust for minimum age group
  if (min_age==0) {
    g1 <- g1[1:n_age]
    g2 <- c(rep(1, 14), rep(2, n_age-14))
    age <- 0:(n_age-1)
    } else {
    g1 <- g1[2:(n_age+1)]
    g2 <- c(rep(1, 13), rep(2, n_age-13))
    age <- 1:n_age
    }

  #weights data to be merged with observed data
  if (is.null(st_pop)) {
    dw <- data.frame(age=age, g1, g2)
  } else {
    dw <- data.frame(age=age, sp=sp, g1, g2)
    dw$sp_g1 <- NA
    dw$sp_g1[!is.na(dw$g1)] <- rep(tapply(dw$sp, dw$g1, sum), table(dw$g1))
    dw$sp_g2 <- rep(tapply(dw$sp, dw$g2, sum), table(dw$g2))
  }

  return(dw)
}
