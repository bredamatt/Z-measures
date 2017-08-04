#' Z7
#'
#' This algorithm is defined as Z7 in Mare, Rossi and Moreira (2017) "Nonstationary Z-measures", European Journal of Operational Research.
#' In essence, it is a nonstationary measure in the sense that it is applicable to any nonstationary ROA series of interest.
#' The algorithm can be broken down into several steps:
#' Step 1: Fit rolling linear models with window length k, ROA = intercept + beta*t
#' Step 2: Extract coefficients and residuals from each fitted linear model
#' Step 3: Extract the middle points of the residuals
#' Step 4: Compute the standard deviation of the residuals, s
#' Step 5: Adjust the standard deviation to make it unbiased using Cochran's theorem, s2
#' Step 6: Predict ROA for T+1
#' Steo 7: Define EA_T = fEA
#' Step 8: Compute the Z7 = (-fEA - ROAT+1) /s2
#' @param dataset A dataset containing a time index t; t=1, t=2, ... , t=T return on assets = ROA, and equity/assets = EA, both observed for each time t.
#' @param k 
#' @keywords data
#' @keywords k
#' @export

Z7 = function(dataset, k){
  library(zoo)
  require(zoo)
  
  #Step 1 + 2; models
  models = rollapply(dataset, width = k, FUN = function(x) 
  coef(lm(ROA~t, data = as.data.frame(x))), by.column = FALSE, align ="right")
  models
  
  #Step 1 + 2; residuals
  res = rollapply(dataset, width = k, FUN = function(z) 
  residuals(lm(ROA~t, data = as.data.frame(z))), by.column = FALSE, align ="right")

  #Step 3; midpoint from residuals, for j=2 in k.j and k{1,2,3}
  detrends = res[, 1+(k-1)/2]
  detrends
  
  #Step 4; standard deviation of midpoint
  s = sd(detrends)
  s
  
  #Step 5; unbiased standard deviation, s2
  # Notice how the meanchi is the mean of a chi distribution
  j = nrow(mp)
  c4n = (sqrt(2/(j-1)))*((gamma(j/2))/gamma((j-1)/2))
  c4n
  S = c4n*s
  S
  

  #Step 6; prediction of ROAT+1
  x = nrow(models)
  lastmodel = models[x,]
  lastm = matrix(lastmodel)
  intercept = lastm[1,1]
  beta = lastm[2,1]
  
  t_plusone = nrow(dataset)+1
  prediction = intercept + (beta*t_plusone)
  prediction
  
  #Step 7; Define EA_T = fEA
  EA = dataset$EA
  l = nrow(dataset)
  fEA = EA[l]
  
   #Step 8; Compute Z
    Z = (-fEA-prediction)/S
    print(Z)
}

dataset = instance
FirstZ = Z7(instance, 3)

