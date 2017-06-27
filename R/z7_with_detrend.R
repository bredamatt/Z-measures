t = seq(0,15,1)
r = (100+50*sin(0.8*t))
EA = rgamma(16,0.8)
df = data.frame(t,r,EA)
df 


z7_detrend = function(data, width, threshold){
  require(zoo)
  
  k = width
  df = data
  e = threshold
  r = df$r
  t = df$t
  EA = df$EA
  l = length(EA)
  fEA = EA[l]
  
  #Coefficients from rolling linear models
  models = rollapply(df, width = k, FUN = function(z) 
  coef(lm(r~t, data = as.data.frame(z))), by.column = FALSE, align ="right")
  #Residuals for each rolling linear model, in other words for each t within k
  res = rollapply(df, width = k, FUN = function(x) 
  residuals(lm(r~t, data = as.data.frame(x))), by.column = FALSE, align = "right")
   
  #Midpoint = middle observation of k, so if k = 3, it is equal to the second observation
  i = 1
  midpoint = res[, i+(k-1)/2]
  
  #Mean and standard deviation of the midpoints
  m = mean(midpoint)
  s = sd(midpoint)
  
  #Reshape the vector r so it matches midpoints
  d_r = r[c(1, length(r))]
  
  #Detrend, if needed, and recompute the standard deviation
  detrend = d_r - midpoint
  s2 = sd(detrend)
  
  #Compute tau, different version according to which one is required
  tau2 = s2/m
  tau4 = (1+(1/(4*(nrow(models)+1))))*tau2
  
  #Prediction
  x = nrow(models)
  xplusone = x + 1
  last = models[x,]
  lastm = matrix(last)
  intercept = lastm[1,1]
  beta = lastm[2,1]
  prediction = intercept + beta*xplusone
  
  #Some standard deviations
  mchi = sqrt(2)*((gamma((k+1)/2))/gamma(k/2))
  S2 = s2*mchi*(x+1)/sqrt(k)
  
  #Computing Z
  
  #For tau2 and S2
  if(abs(tau2*prediction) < e){
    z7_tau2_2 = -(fEA + prediction) / S2
  }
  else 
    z7_tau2_2 = -(fEA + prediction) / (tau2*prediction)
}

test3 = z7_detrend(df,3,0.001)
test3

