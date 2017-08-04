#' Z7
#'
#' This algorithm is defined as Z7 in Mare, Rossi and Moreira (2017) "Nonstationary Z-measures", European Journal of Operational Research.
#' In essence, it is a nonstationary measure in the sense that it is applicable to any nonstationary ROA series of interest.
#' The algorithm can be broken down into these steps:
#' Step 1: Fit rolling linear models with window length k, ROA = intercept + beta*t
#' Step 2: Extract coefficients and residuals from each fitted linear model
#' Step 3: Extract the middle points of the residuals, call them detrends
#' Step 4: Extract ROA observations corresponding to detrends, call them MPR
#' Step 5: Define middle points as MPR - detrends and calculate its mean, m
#' Step 6: Compute the standard deviation of the residuals,s
#' Step 7: Calculate the coefficient of variation, taubar
#' Step 8: Adjust the biasedness in s, and compute an unbiased standard deviation S
#' Step 9: Predict ROA at time T+1 using coefficients from the last fitted linear model
#' Step 10: Define the final value of EA at time T = fEA
#' Step 11: Calculate the variance of the prediction, tft
#' Step 12: Compte Z7 with a conservative variance if the prediction variance is small, otherwise, use tft.
#' @param dataset A dataset of type dataframe, containing a time index t; t=1, t=2, ... , t=T, return on assets = ROA, and equity/assets = EA, both observed for each time t.
#' @param k The window length for rolling linear models
#' @param e A threshold determining the denominator of the calculation of the Z score.
#' @keywords data
#' @keywords k
#' @keywords e
#' @export

dataset = instance

Z7 = function(dataset, k, e){
  library(zoo)
  require(zoo)
  
  #Step 1 + 2; models
  models = rollapply(dataset, width = k, FUN = function(x) 
    coef(lm(ROA~t, data = as.data.frame(x))), by.column = FALSE, align ="right")
  
  #Step 1 + 2; residuals
  res = rollapply(dataset, width = k, FUN = function(z) 
    residuals(lm(ROA~t, data = as.data.frame(z))), by.column = FALSE, align ="right")
  
  #Step 3; detrended observations from residuals, for j=2 in k.j and k{1,2,3}
  detrends = res[, 1+(k-1)/2]
  detrends
  
  #Step 4; extract the (i+k-1)/2 observations of ROA
  ROA = dataset$ROA
  MPR = ROA[sapply(1:length(detrends), function(i) {i + round(((k - 1)/2)) } )]
  MPR
  #Step 5; define midpoints as MPR - detrends and calculate its mean
  midpoints = MPR - detrends
  midpoints
  m = mean(midpoints)
  
  #Step 6; standard deviation of midpoint
  s = sd(detrends)
  s
  
  #Step 7; calculate the coefficient of variation tau,
  tau = s/m
  taubar = (1+1/(4*(length(detrends)+1)))*tau
  taubar
  
  #Step 8; unbiased standard deviation, s2
  # Notice how the meanchi is the mean of a chi distribution
  j = length(midpoints)
  c4n = (sqrt(2/(j-1)))*((gamma(j/2))/gamma((j-1)/2))
  c4n
  S = c4n*s
  S
  
  #Step 9; prediction of ROAT+1
  x = nrow(models)
  lastmodel = models[x,]
  lastm = matrix(lastmodel)
  intercept = lastm[1,1]
  beta = lastm[2,1]
  
  t_plusone = nrow(dataset)+1
  prediction = intercept + (beta*t_plusone)
  
  #Step 10; Define EA_T = fEA
  EA = dataset$EA
  l = nrow(dataset)
  fEA = EA[l]
  
  #Step 11; Calculate the variance of the prediction
  tft = taubar*prediction
  tft
  #Step 12; Compute Z
  if(tft < e){
    Z = (-fEA-prediction)/S
    print(Z)
  }
  else 
    Z = (-fEA-prediction)/tft
    print(Z)
}

TEST = data.frame(cbind(t,ROA,EA))
Z = Z7(dataset,3,0.1)

#Development dataset
Dev = data.frame(dataset)
Dev
