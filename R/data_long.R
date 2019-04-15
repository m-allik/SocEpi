#' Scottish health and deprivation data
#'
#' A data set on deprivation and self-rated health outcomes for postcode sectors from 2011 Scottish Census by ethnicity.
#'
#' @format A data frame with 182160 rows and 15 variables:
#' \describe{
#'   \item{PS_code}{postcode sector code}
#'   \item{PS_name}{postcode sector name}
#'   \item{health_board}{health board name}
#'   \item{council_area}{Council area name}
#'   \item{ur2fold}{Two-fold urban rural classification (created based on output area level data).
#'      Used in this data set for illustration only.}
#'   \item{total_pop}{Total population of the postcode sector}
#'   \item{carstairs}{The 2011 Carstairs score}
#'   \item{quintile}{Quintiles of the Carstairs score; 1 - least deprived, 5 - most deprived}
#'   \item{decile}{Deciles of the Carstairs score; 1 - least deprived, 10 - most deprived}
#'   \item{ethnicity}{Ethnic group, "all" for all people}
#'   \item{age}{age group, 1 - "0-4", 2 - "5-9", up to 18 - "85 and above"}
#'   \item{bad}{number of people in bad general health}
#'   \item{fg}{number of people in fairly good health}
#'   \item{good}{number of people in good health}
#'   \item{pop}{number of people in age and ethnic group in postcode sector}
#'   }
#' @source For Carstairs deprivation data look at
#'  \url{https://www.gla.ac.uk/researchinstitutes/healthwellbeing/research/mrccsosocialandpublichealthsciencesunit/programmes/inequalities/carstairsscores/}
#' @source For health data see the 2011 census commissioned tables \url{http://www.scotlandscensus.gov.uk/ods-web/data-warehouse.html#additionaltab}

"health_data"
