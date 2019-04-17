#' Mountain plot
#'
#' The function draws a mountain plot for decomposed RII.
#'
#' @param data Name of the data set
#' @param age Should be an ordered factor or numeric. If numberic, age group labels need to be given!
#' @param RII The decomposed RII values
#' @param cause List of causes (e.g. causes of death).
#' @param supress Shold negative RII values be supressed, default supress = TRUE
#'
#' @return
#'
#' @import ggplot2
#'
#' @export
#' @examples
#'
#' # RII rates
#' sii <- sii %>% filter(!( age %in% c("all", "0-64")))
#' sii$age <- factor(sii$age)
#'
#' mountain_plot(sii, age, rii_dc, cause)
#'

mountain_plot <- function(data, age, RII, cause, supress = T) {

  # Subset data from the data frame
  groups = NULL
  df <- subset_q(data, substitute(groups), substitute(c(age, RII, cause))) #select data from data frame
  names(df) <- c("age",  "RII", "cause") # names to use within function

  # Prepare data for plotting
  if (is.factor(df$cause)) { name_cause <- levels(df$cause) } # causes listed
  if (is.character(df$cause)) { name_cause <- unique(df$cause) } # causes listed

  name_age <- levels(df$age) # names for age groups

  n_age <- length(name_age) # number of age groups
  n_cause <- length(name_cause) # number of causes

  if (!is.numeric(df$age)) {df$age <- as.numeric(df$age)} # convert age to numeric

  max_rii <- tapply(df$RII, df$age, sum) # total RII as sum of all RII
  max_rii <- ceiling(max(max_rii)) # y axis maximum
  min_rii <- 0 # y axis minimum

  # Dealing with negative RII values
  if (any(df$RII < 0)) {

    if (supress == T) {

      df$RII[df$RII < 0] <- 0 # negative rates set to zero

       } else {

         # Idea is to put negative RIIs at the bottom of the graph, but this needs to be done/sorted by age!
      neg_cause <- df$cause[which(df$RII < 0)]
      neg_cause <- as.character(unique(neg_cause)) # causes with negative RII

      min_rii <- floor(min(df$RII))
      order_level <- c(levels(df$cause)[!(levels(df$cause)  %in% neg_cause )], neg_cause)
      df$cause <- factor(df$cause, levels = order_level, ordered = T)

      }
  }


  # Main plot
  p <- ggplot(df, aes(age, RII))
  p + geom_area(aes(fill = cause)) +
  scale_x_continuous(breaks = 1:n_age,  labels = name_age) +
  coord_cartesian(xlim = c(1, n_age), ylim = c(min_rii, max_rii)) +
  ylab("Relative index of inequality") + xlab("Age") + labs(fill = "Cause") +
  theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))

# if () # If color given by user
#  p + scale_fill_manual(breaks = name_cause, ...)

}
