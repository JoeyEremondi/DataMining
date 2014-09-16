# We use lists to store trees
# Leaf nodes are singleton lists, of the format [MajorityClass]

# Interior nodes are of the form [isNumeric, column, splitPoint, trueNode, falseNode]
# The first element tells us whether we are splitting on numeric or category data
# We then look at which column we split on
# If numeric, we drop to true node if the current value in the given column is less than or equal to splitPoint
# and drop to the falseNode otherwise
# If not numeric, we drop to the trueNode if the given attribute is 1, falseNode if it is 0

# Helper functions for nodes: Constructors
makeLeaf = function(class){
  list(class)
}

makeNumericSplit = function(column, splitPoint, trueNode, falseNode){
  list(TRUE, column, splitPoint, trueNode, falseNode)
}

makeCategorySplit = function (column, trueNode, falseNode){
  list(FALSE, column, -9999, trueNode, falseNode)
}

# Basic test functions for nodes
isLeaf = function(node){
  length(node) < 2
}

isInterior = function(node){
  length(node) > 1
}

isNumeric = function(node){
  node[[1]]
}

isCategorical = function(node){
  node[[1]] == FALSE
}





# Measure the impurity of a list of binary values using the GINI metric
# We use the trick that summing counts how many 1 instances there are
impurity = function(x) {
  p1 <- sum(x) / length(x)
  p1 * (1-p1) }


# Inputs:
# x: a list of numeric values
# y: a list of classifications for each entry in x
# Outputs: the split point which reduces the impurity the most
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

