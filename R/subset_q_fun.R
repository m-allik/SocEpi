#' Subset data
#'
#' Internal use only, allows subsetting within another function (for use of NSE in functions).
#'
#' @param df Data frame to be subsetted.
#' @param condition Subsetting condition, such as which rows to select.
#' @param variables Variables to include in subset.
#'
#' @return A smaller data frame of subsetted data.
#'
#' @keywords internal
#'
#' @importFrom stats setNames
#'

subset_q <- function(df, condition, variables) {
  r <- eval(condition, df, parent.frame())
  var_pos <- setNames(as.list(seq_along(df)), names(df))
  pos <- eval(variables, var_pos, parent.frame())

  if (!is.null(condition)) {df[r, pos, drop=F]} else {
    df[, pos, drop=F]
    }
  }
