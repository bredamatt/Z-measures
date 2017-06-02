#' Z1
#'
#' Estimates rolling Z-scores with a fixed window length, k, set to 3. Hence, you estimate T-k Z-scores with rolling means and sigmas computed with window length k.
#' @param data The dataset of interest. Note that ROA should be in column number 3 and CAR in column number 4.
#' @keywords data
#' @export
z1 <- function(data) {
  ROA_rmean <- rollapply(data[,3], 3, mean)
  CAR_rmean <- rollapply(data[,4], 3, mean)
  ROA_rsigma <- rollapply(data[,3], 3, sd)
  scores <- (ROA_rmean + CAR_rmean) / (ROA_rsigma)
  scores
}
