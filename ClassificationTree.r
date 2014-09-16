% Measure the impurity of a list of binary values using the GINI metric
% We use the trick that summing counts how many 1 instances there are
impurity = function(x) {
  p1 <- sum(x) / length(x)
  p1 * (1-p1) }


% Inputs:
% x: a list of numeric values
% y: a list of classifications for each entry in x
% Outputs: the split point which reduces the impurity the most
bestsplit <- function(x,y) {
  n <- length(y)
  index <- (1:(n-1) )
  index
  
  xsorted <- sort(x)
  xFirsts <- xsorted[1:(n - 1)]
  xFirsts
  xLasts <- xsorted[2:n]
  xLasts
  splitPoints <- 0.5*(xFirsts + xLasts)
  splitPoints
  splitPoints
  
  rootImp <- impurity(y)
  
  leftImps <- 1:(n-1)
  rightImps <- 1:(n-1)
  
  
  for (i in 1:(n-1)){
    leftImps[i] <- impurity(y[x <= splitPoints[i]])
    rightImps[i] <- impurity(y[x > splitPoints[i]])
  }
  
  p0 = length(y[y == 0])/n
  p1 = length(y[y == 1])/n
  
  diffs <- rootImp - (p1*leftImps + p0*rightImps)
  resultIndex = which.max(diffs)
  
  splitPoints[resultIndex]
  diffs
  
  
  
  
}

