z7 = function(data, width, threshold){
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
  
  #Compute tau, different version according to which one is required
  tau1 = s/m
  tau3 = (1+(1/(4*(nrow(models)+1))))*tau1
  
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
  S1 = s*mchi*(x+1)/sqrt(k)
  
  #Computing Z
  
  #For tau1 and S1
  if(abs(tau1*prediction) < e){
    z7_tau1_1 = -(fEA + prediction) / S1
  }
  else 
    z7_tau1_1 = -(fEA + prediction) / (tau1*prediction)
}

t = seq(0,15,1)
r = (100+50*sin(0.8*t))
EA = rgamma(16,0.8)
df = data.frame(t,r,EA)
df 

test1 = z7(df,3,0.001)
test1
