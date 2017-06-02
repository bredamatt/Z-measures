#' Z5
#'
#' Estimates Z-scores with the overall sample period mean and sigma for ROA, whilst letting CAR be observed per time t. Result will be T Z-scores.
#' @param data The dataset of interest. Note that ROA should be in column number 3 and CAR in column number 4.
#' @keywords data
#' @export
z5 <- function(data) {
  ROA_mean <- mean(data[,3])
  ROA_sigma <- sd(data[,3])
  scores <- (ROA_mean + data[,4]) / ROA_sigma
  scores
}
