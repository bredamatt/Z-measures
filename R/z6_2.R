#' Z6_2
#'
#' This function allows you to specify the window length k when estimating the Z-score. It is similar to Z2 in the sense that CAR is left observable per time t, but ROA sigma and mean are estimated on a rolling basis, where k is the window length specified for computing the rolling estimates. The difference between Z6 and Z6_2 is that Z6_2 corrects for the unbiasedness of the sample standard deviation by using a simple approximation of Cochran's theorem thorugh exploiting the correction factor for the estimator of the coefficient of variation according to Salkind(2010). 
#' @param data The dataset of interest. Note that ROA should be in column number 3 and CAR in column number 4.
#' @param k The window length k for calculating the rolling mean and sigma with rollapply.
#' @keywords data
#' @export
z6_2 <- function(data, k) {
  ROA_rmean <- rollapply(data[,3], k, mean) # Window length = k and specified as parameter
  ROA_rsigma <- rollapply(data[,3], k, sd) # Same as above
  CAR <- data[,4]
  Salkind <- (1+1/(4*k))
  
  scores <- (ROA_rmean + CAR) / (ROA_rsigma*Salkind)
  scores
}
