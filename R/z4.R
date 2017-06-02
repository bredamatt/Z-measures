#' Z4
#'
#' Similar to Z3 in the sense that it is observed for every t. However, the standard deviation is computed differently. Here it is the absolute value of the deviation from the sample period mean of ROA for every t.
#' @param data The dataset of interest. Note that ROA should be in column number 3 and CAR in column number 4.
#' @keywords data
#' @export
z4 <- function(data) {
  inst_sigma <- abs(data[,3] - mean(data[,3]))
  scores <- (data[,3] + data[,4]) / inst_sigma
  scores
}
