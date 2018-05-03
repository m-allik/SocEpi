#' Calculate RII/SII and confidence intervals
#'
#' \code{rii} calculates the relative index of inequality (RII) or the slope index of inequality (SII) and
#' confidence intervals for either measure. The SII is obtained via OLS regression of the health variable
#' on the midpoints of the cummulative population distribution.
#' The RII is calculated as SII divided by health outcome across all socioeconomic positions. Simulation
#' is used to calculate confidence intervals (see \code{method} below).
#'
#' @param data Name of dataset.
#' @param health Health outcome of interest.
#' @param population Population counts. Should correspond to data provided in \code{health}.
#' @param ses Categorical deprivation measure used for splitting the data.
#' @param age Variable that defines 5-year age groups, should be ordered and numeric, such as 1 through 18 or
#'   0 through 18 if the first age group is under 1 years of age.
#' @param groups Conditions such as sex or ethnicity that together define the sub-population for which the
#'   RII/SII is calculated for. See examples. Default is \code{NULL}, no sub-population is selected.
#' @param age_group The age groups the standardized rates should be calculated for. By default the function calculates
#'   results for the following age groups: 0-14, 15-29, 30-44, 45-59, 60-74, 75+, 0-64 and all ages.
#'   User supplied age groups should be provided using the standard population groups as cut-offs, e.g.
#'   \code{age_group=c("20-29", "25-39")} and not "19-30" or "21-41". Open ended age groups can be supplied by giving
#'   a single age, e.g. "45" means 45 and above. Overlaping age groups, e.g. \code{c("20-29", "25-34")}, are not supported.
#'   Results for ages 0-64 and all ages will always be provided.
#' @param st_pop the standard population weights used for calculating rates, default 2013 ESP for 18 age groups with 0-4 as
#'   the first age group. See \code{\link{st_pop}} for other predefined options. Can be user supplied (must add up to 1).
#' @param N Number of simulations for the confidence intervals, defaults to 1000
#' @param RII Logical, should RII or SII be calculated, default \code{RII=TRUE}
#' @param CI Confidence intervals, 95 by default but can be set to any number between 0 and 100.
#' @param W Logical, should data be weighted before calculating RII/SII, default \code{W=FALSE}
#' @param thousand Should rates be calculated per 1000 (default) or 100 000, relevant for SII only
#' @param method The method to be used for simulating confidence intervals. Default \code{method = "multinomial"}.
#'    The CI are calculated using a multinomial distribution as described in Lumme et al (2015)
#'    "A Monte Carlo method to estimate the confidence intervals for the concentration index
#'    using aggregated population register data." Health Services & Outcomes Research Methodology, 15(2),
#'    82-98. \url{http://doi.org/10.1007/s10742-015-0137-1}
#'
#' @return A data frame giving RII/SII by age groups and for ages 0-64 and all ages together with confidence intervals
#'
#' @import tidyr
#' @import dplyr
#' @importFrom stats quantile
#'
#' @export
#' @examples
#' d <- dep_data_long
#'
#' #RII with 95% CI
#' rii(d, bad, pop, quintile, age, ethnicity == "all")
#'
#' #SII with 99% CI
#' rii(d, bad, pop, quintile, age, ethnicity == "all", RII=FALSE, CI=99)
#'
#' #supply own population weights
#' new_w <- c(0.075, 0.075, 0.075, 0.06, 0.060, 0.060, 0.06, 0.070, 0.050,
#'    0.050, 0.050, 0.06, 0.060, 0.055, 0.050, 0.040, 0.025, 0.025)
#'
#' rii(d, bad, pop, quintile, age, ethnicity == "all", RII=FALSE, CI=99, st_pop=new_w)
#'
#' #SII for new age groups with 95% CI
#' rii(d, bad, pop, quintile, age, ethnicity == "Scot" &  ur2fold == "Urban",
#'    age_group=c("0-19", "20-34", "35-49"), RII=FALSE)


rii <- function(data, health, population, ses, age, groups=NULL, age_group=NULL, st_pop="esp2013_18ag",
                N=1000, RII=T, CI=95, W=F, thousand=1000, method = "multinomial") {

  #For package building only - to get rid of NOTEs
  cr <- rate <- sii <- . <- w <- age_health <- ci_low <- ci_high <- NULL
  tp <- sp_g1 <- sp <- g1 <- g2 <- pop <- sp_g2 <- cum_p <- prob <- sim <- dep <- rate_sim <- NULL

  #function starts
  #=====================================================================
  df <- subset_q(data, substitute(groups), substitute(c(health, population, ses, age))) #select data from data frame
  names(df) <- c("health", "population", "ses", "age") #give names to use within function

  n_g <- length(table(df$ses))#Number of groups
  name <- names(table(df$ses)) #names of groups
  if (!is.numeric(df$ses)) {df$ses <- as.integer(df$ses)} #factor variable converted to integer

  min_age <- min(df$age) #minimum age age group in the data
  n_age <- length(unique(df$age)) #number of age groups
  dw <- age_grouping(age_group, st_pop, min_age, n_age) #weights table for all age groups

  if (is.null(age_group)) {age_group=c("0-14", "15-29", "30-44", "45-59", "60-74", "75")} #set default age group names


  #observed data - summarized up by age across all SES
  D_A <- df %>% group_by(age) %>%
    summarise_all(sum) %>%
    mutate(ses=n_g+1, tp=NA)

  #Observed data - by SES and age
  DT <- df %>% group_by(ses, age) %>%
    summarise_all(sum) %>%
    group_by(age) %>%
    mutate(tp = sum(population)) %>%
    bind_rows(D_A) %>%
    mutate(population = ifelse(population < 1, 1, population),
           cr = health/population*thousand) %>%
    ungroup() %>%
    right_join(dw, by="age")


  ##Standardisation of observed data
  #=========================================================================================
  #for all ages
  da <- DT %>%
    mutate(rate = cr*sp) %>%
    select(ses, rate, population, tp) %>%
    group_by(ses) %>%
    summarise_all(sum) %>%
    mutate(pop=population/tp, age="all")

  #standardization for age groups
  d6g <- DT %>%
    filter(is.na(g1)==F) %>%
    mutate(rate=cr*sp/sp_g1) %>%
    select(ses, rate, population, tp, g1) %>%
    group_by(ses, g1) %>%
    summarise_all(sum) %>%
    mutate(pop=population/tp, age=age_group) %>%
    select(-g1)

  #standardization for 0-64
  d64 <- DT %>%
    filter(g2==1) %>%
    mutate(rate=cr*sp/sp_g2) %>%
    select(ses, rate, population, tp) %>%
    group_by(ses) %>%
    summarise_all(sum) %>%
    mutate(pop=population/tp, age="0-64")


  #Calculate RII/SII
  #================================================================================================================

  if (RII==T) {
  #healt rates by age across all SES - needed for RII later
  all_rates <- bind_rows(d6g, d64, da) %>%
    select(ses, age, rate)  %>%
    filter(ses==n_g+1)
  }

  rates <- bind_rows(d6g, d64, da) %>%
    select(-population, -tp) %>%
    filter(ses != n_g+1) %>%
    group_by(age) %>%
    mutate(cum_p = popmid(pop)) %>% rename(dep=ses)

  if (W==F) {
    sii_obs <- rates %>%
      summarise(sii = beta(x=cum_p, Y=rate))
  } else {
    sii_obs <- rates %>%
      mutate(w = sqrt(pop), rate=rate*w, cum_p = w*cum_p) %>%
      summarise(sii = beta(a=w, x=cum_p, Y=rate))
  }


  #Calculate CI for RII/SII
  #================================================================================================================
  if (method == "multinomial") {Ds <- CI_multinom(df, N, n_g)} #simulated data

  Ds <- merge(Ds, dw, by="age") #merge simulated data with weights

  #standardization of simulated data
  #for all ages
  das <- Ds %>%
    mutate_at(vars(3:(2+n_g)), funs(.*sp)) %>%
    select(1:(2+n_g)) %>%
    group_by(sim) %>%
    summarise_all(sum) %>%
    mutate(age="all")

  #standardization for provided age groups
  d6gs <- Ds %>%
    filter(is.na(g1)==F) %>%
    mutate_at(vars(3:(2+n_g)), funs(.*sp/sp_g1)) %>%
    select(sim:g1, -sp) %>%
    group_by(sim, g1) %>%
    summarise_all(sum) %>%
    mutate(age = as.character(factor(g1, labels=(age_group)))) %>%
    select(-g1)

  #standardization for ages 0-64
  d64s <- Ds %>%
    filter(g2==1) %>%
    mutate_at(3:(2+n_g), funs(.*sp/sp_g2)) %>%
    select(1:(2+n_g)) %>%
    group_by(sim) %>%
    summarise_all(sum) %>%
    mutate(age="0-64")

  #bind together standardized rates
  rates_s <- bind_rows(d64s, das, d6gs) %>%
    gather("dep", "rate_sim", 3:(n_g+2)) %>%
    mutate(dep=as.numeric(gsub("X", "", dep))) %>%
    left_join(rates[,c("age", "dep", "pop", "cum_p")], c("age", "dep")) %>%
    group_by(sim, age)

  if (W==F) {
    sii_s <- rates_s %>%
      summarise(sii = beta(x=cum_p, Y=rate_sim))
  } else {
    sii_s <- rates_s %>%
      mutate(w = sqrt(pop), rate_sim = rate_sim*w, cum_p = w*cum_p) %>%
      summarise(sii = beta(a=w, x=cum_p, Y=rate_sim))
  }


  #prepare final output
  #====================================================================================
  #pick out CI
  pl <- (1-CI/100)/2 #lower CI
  ph <- CI/100 + (1-CI/100)/2 #higher CI

  OUT <- sii_s %>%
    group_by(age) %>%
    summarise_at(vars(sii), funs(ci_low=quantile(.,probs=pl)*thousand, ci_high=quantile(., probs=ph)*thousand)) %>%
    left_join(sii_obs, ., by="age") %>%
    ungroup()

  #calculate RII from SII
  if (RII==T) {OUT <- OUT %>% left_join(all_rates, by="age") %>%
    mutate_at(vars(sii:ci_high), funs(./rate)) %>%
    rename(rii=sii) %>% select(1:4)}

  #Order output
  OUT <- OUT[order(match(OUT$age, c(age_group, "0-64", "all"))),]
  return(OUT)
}
