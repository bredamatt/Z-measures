#' Z3
#'
#' Estimates the Z-score by using ROA and CAR observations at every time t. If total years = T, you get T Z-scores in stead of T-k. Note that the standard deviation is estimated over the entire sample period.
#' @param data The dataset of interest. Note that ROA should be in column number 3 and CAR in column number 4.
#' @keywords data
#' @export
z3 <- function(data) {
  ROA_sigma <- sd(data[,3])
  scores <- (data[,3] + data[,4]) / ROA_sigma
  scores
}
