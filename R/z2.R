#' Z2
#'
#' Estimates rolling Z-scores with a fixed window length, k, set to 3. However, only the mean and standard deviation of ROA are computed on a rolling basis.
#' @param data The dataset of interest. Note that ROA should be in column number 3 and CAR in column number 4.
#' @keywords data
#' @export
z2 <- function(data) {

  ROA_rmean <- rollapply(data[,3], 3, mean)
  ROA_rsigma <- rollapply(data[,3], 3, sd)
  CAR <- data[,4]
  scores <- (ROA_rmean + CAR) / (ROA_rsigma)
  scores
}
