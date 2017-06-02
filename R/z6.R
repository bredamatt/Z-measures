#' Z6
#'
#' This function allows you to specify the window length k when estimating the Z-score. It is similar to Z2 in the sense that CAR is left observable per time t, but ROA sigma and mean are estimated on a rolling basis, where k is the window length specified for computing the rolling estimates.
#' @param data The dataset of interest. Note that ROA should be in column number 3 and CAR in column number 4.
#' @param k The window length k for calculating the rolling mean and sigma with rollapply.
#' @keywords data
#' @export
z6 <- function(data, k) {
  ROA_rmean <- rollapply(data[,3], k, mean) # Window length = k and specified as parameter
  ROA_rsigma <- rollapply(data[,3], k, sd) # Same as above
  CAR <- data[,4]

  scores <- (ROA_rmean + CAR) / ROA_rsigma
  scores
}
