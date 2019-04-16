#' Standardized mortality rates and confidence intervals
#'
#' \code{st_rate} calculates direct standardized mortality rates and the 95\% or 99\% confidence intervals.
#'   Default standardization uses the 2013 European Standard Population, but other built-in or user supplied
#'   weights can also be supplied.
#'
#' @param data Name of data set.
#' @param health Health outcome of interest.
#' @param population Population counts. Should correspond to data provided in \code{health}.
#' @param ses Categorical deprivation measure used for splitting the data.
#' @param age Variable that defines 5-year age groups, should be ordered and numeric, such as 1 through 18.
#'   If the age group coding starts with 0 it will be assumed that age groups 0 and 1-4 are separate.
#'   If the age group starts with 1, it will be assumed that the first age group is 0-4 (ages 0 and 1-4 are combined).
#' @param groups Conditions such as sex or ethnicity that together define the sub-population for which the
#'   rates are calculated for. Default is \code{NULL}, no sub-population is selected.
#' @param age_group The age groups the standardized rates should be calculated for. By default the function calculates
#'   results for the following age groups: 0-14, 15-29, 30-44, 45-59, 60-74, 75+, 0-64 and all ages.
#'   User supplied age groups should be provided using the standard population groups as cut-offs, e.g. use
#'   \code{age_group=c("20-29", "30-39")} and not \code{c("19-30", "31-41")}. Open ended age groups can be supplied by giving
#'   a single age, e.g. "45" means 45 and above. Overlapping age groups, such as \code{c("20-29", "25-34")}, are not supported.
#'   Results for ages 0-64 and all ages will always be provided.
#' @param st_pop The standard population weights used for calculating rates, default 2013 ESP for 18 age groups with 0-4 as
#'   the first age group. See \code{\link{st_pop}} for other predefined options. Can be user supplied, but must match
#'   the number of age groups given in \code{age} and add up to 1.
#' @param CI Confidence intervals, 95 by default but can be set to any number between 0 and 100.
#' @param total The total number of people in the standard population, i.e. are rates be calculated per 1000 or 100 000, default 1000
#'
#' @return A data frame giving standardized rates by age group and deprivation together with CI.
#'
#' @import tidyr
#' @import dplyr
#' @importFrom stats qnorm
#'
#' @export
#' @examples
#' d <- health_data
#'
#' # Standardized rates for all people
#' st_rate(d, bad, pop, quintile, age, ethnicity=="all")
#'
#' # Or save results
#' rate_data <- st_rate(d, bad, pop, quintile, age, ethnicity=="all")
#' # Then use View(rate_data) to view results
#'
#' # Standardized rates for Scottish, with 99% CI
#' st_rate(d, bad, pop, quintile, age, ethnicity=="Scot", age_group=c("15-29", "30-44"), CI=99)
#'


st_rate <- function(data, health, population, ses, age, groups = NULL, age_group = NULL,
          st_pop = "esp2013_18ag", CI = 95, total = 1000) {

  # For package building only - to get rid of NOTEs
  cr <- rate <- . <- sp_g1 <- sp_g2 <-  CI_low <- CI_high <- sp <- g1 <- g2 <- NULL

  # Function starts
  df <- subset_q(data, substitute(groups), substitute(c(health, population, ses, age))) # select data from data frame
  names(df) <- c("health", "population", "ses", "age") # Give names to use within function

  n_g <- length(table(df$ses)) # Number of groups of the ses variable
  name <- names(table(df$ses)) # Names of the grous for the ses variable
  if (!is.numeric(df$ses)) {df$ses <- as.integer(df$ses)}

  min_age <- min(df$age) # minimum age age group in the data
  n_age <- length(unique(df$age)) # number of age groups
  dw <- age_grouping(age_group, st_pop, min_age, n_age) # weights table for all age groups

  if (is.null(age_group)) {age_group=c("0-14", "15-29", "30-44", "45-59", "60-74", "75")}

  # observed data - summarized up by age across all SES
  D_A <- df %>% group_by(age) %>%
      summarise_all(sum) %>%
      mutate(ses=n_g+1)

  # Observed data - by SES and age
  DT <- df %>% group_by(ses, age) %>%
      summarise_all(sum) %>%
      bind_rows(D_A) %>%
      mutate(population = ifelse(population < 1, 1, population),
        cr = health/population*total) %>%
      ungroup() %>%
      right_join(dw, by="age")

    # standardization
    da <- DT %>%
      mutate(rate=cr*sp) %>%
      select(ses, rate, health) %>%
      group_by(ses) %>%
      summarise_all(sum) %>%
      mutate(age = "all")

    d6g <- DT %>%
      filter(is.na(g1)==F) %>%
      mutate(rate=cr*sp/sp_g1) %>%
      select(ses, g1, rate, health) %>%
      group_by(ses, g1) %>%
      summarise_all(sum) %>%
      mutate(age=age_group) %>%
      select(-g1)

    # standardization for 0-64
    d64 <- DT %>%
      filter(g2==1) %>%
      mutate(rate=cr*sp/sp_g2) %>%
      select(ses, g2, rate, health) %>%
      group_by(ses) %>%
      summarise_all(sum) %>%
      mutate(age = "0-64") %>%
      select(-g2)

    # CI for the rates (z score)
    p <- CI/100 + (1-CI/100)/2
    z <- qnorm(p)

  # output
  rates <- bind_rows(d6g, d64, da) %>%
    ungroup() %>%
    mutate(CI_low = rate - z*(rate/sqrt(health)),
           CI_high = rate + z*(rate/sqrt(health)),
           ses = factor(ses, labels = c(name, "overall"))) %>%
    select(ses, age, rate, CI_low, CI_high)

  return(rates)
}
