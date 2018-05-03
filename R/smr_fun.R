#' Standardized mortality ratios and CIs
#'
#' The function \code{smr} calculates standardized mortality ratios (SMR) and confidence intervals for SMR.
#' @param data Name of dataset.
#' @param health Health outcome of interest.
#' @param population Population counts. Should correspond to data provided in \code{health}.
#' @param age Variable that defines 5-year age groups, should be ordered and numeric, such as 1 through 18 or
#'   0 through 18 if the first age group is under 1 years of age.
#' @param compare Categorical variable that splits the data into groups that are to be compared, such as ethnicity.
#' @param sets groups that are to be compared (values taken by \code{compare}). The group listed first will be the
#'    reference category. Must take at least two values.
#' @param age_group The age groups the standardized rates should be calculated for. By default the function calculates
#'   results for the following age groups: 0-14, 15-29, 30-44, 45-59, 60-74, 75+, 0-64 and all ages.
#'   Results for ages 0-64 and all ages will always be provided.
#'   User supplied age groups should be provided using the standard population groups as cut-offs, e.g.
#'   \code{age_group=c("20-29", "25-39")} and not "19-30" or "21-41". Open ended age groups can be supplied by giving
#'   a single age, e.g. "45" means 45 and above. Overlaping age groups, e.g. \code{c("20-29", "25-34")}, are not supported.
#' @param CI Confidence intervals, 95 by default but can be set to any number between 0 and 100.
#'   For calculation method see user guide.
#'
#' @return A data frame of standardized mortality ratios (SMR) and CIs
#'
#' @importFrom stats qnorm
#' @importFrom stats aggregate
#'
#' @export
#'
#' @examples
#' d <- health_data
#'
#' # Asian population compared to Scottish (reference)
#' smr(d, bad, pop, age, ethnicity, sets = c("Scot", "asian"))
#'
#' # Asian, White British and Irish population compared to Scottish (reference)
#' smr(d, bad, pop, age, ethnicity, sets = c("Scot", "asian", "WB", "Irish"),
#'   age_group = c("15-29", "30-44"), CI=99)
#'


smr <- function(data, health, population, age, compare, sets, age_group=NULL, CI=95) {

  #For package building only - to get rid of NOTEs


  #function starts
  #=====================================================================
  #CI for the rates (z-score)
  p <- CI/100 + (1-CI/100)/2
  z <- qnorm(p)

  # subset data from data frame
  #####
  df <- subset_q(data, substitute(compare %in% sets), substitute(c(health, population, age, compare)))
  names(df) <- c("health", "population", "age", "compare") #give names to use within function
  df$compare <- droplevels(df$compare)

  # Age groups
  #####
  min_age <- min(df$age) #minimum age in the data
  n_age <- length(unique(df$age)) #number of age groups
  dw <- age_grouping(age_group, st_pop = NULL, min_age, n_age) #age group IDs

  if (is.null(age_group)) {age_group=c("0-14", "15-29", "30-44", "45-59", "60-74", "75")} #set default age group names

  # Merge data to age group IDs
  df <- merge(df, dw, by="age")


  #### Aggregate data for age groups
  # All ages
  t0 <- aggregate(df[, c("health", "population")], by = list(compare=df$compare), FUN=sum)
  t0$age <- "all"

  # Supplied age groups
  t1 <- aggregate(df[, c("health", "population")], by = list(df$g1, compare=df$compare), FUN=sum)
  t1$age <- age_group
  t1 <- t1[, -1]

  # Ages 0-64
  t2 <- aggregate(df[, c("health", "population")], by = list(df$g2, compare=df$compare), FUN=sum)
  t2 <- t2[t2$Group.1==1, -1]
  t2$age <- "0-64"


  # Bind data on age groups and split to reference and comparison populations
  #####
  da <- rbind(t1, t2, t0)
  da <- split(da, da$compare == sets[1])

  # Extract reference data and calculate rate
  ref <- da[["TRUE"]]
  ref$ref_rate <- ref$health/ref$population
  ref <- ref[, c("ref_rate", "age")]

  # Merge reference rate to comparison groups
  da <- merge(da[["FALSE"]], ref, by="age")

  # Calculate SMR and CI
  da$expected <- da$population*da$ref_rate
  da$smr <- da$health/da$expected*100
  da$se <- sqrt(da$health)/da$expected*100
  da$ci_low <- da$smr - z*da$se
  da$ci_high <- da$smr + z*da$se

  # Pick out variables for output and order data
  SMR <- da[, c("age", "compare", "smr", "ci_low", "ci_high")]
  names(SMR)[2] <- "group"
  SMR <- SMR[order(match(SMR$age, c(age_group, "0-64", "all"))),]
  SMR <- SMR[order(SMR$group), ]
  rownames(SMR) <- NULL

  return(SMR)
}
