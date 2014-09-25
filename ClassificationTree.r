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
  list(class = class)
}

makeNumericSplit = function(column, splitPoint, trueNode, falseNode){
  list(isNumeric = TRUE, col = column, splitPoint = splitPoint,
       left = trueNode, right = falseNode)
}

#Category variables are always true or false, so we don't need
#a numeric value to split on
#we use -9999 as a placeholder
makeCategorySplit = function (column, trueNode, falseNode){
  return(
    list(isNumeric = FALSE, col = column, splitPoint = -9999,
         left = trueNode, right = falseNode)
  )
}

# Basic test functions for nodes
isLeaf = function(node){
  return (length(node) < 2)
}

isInterior = function(node){
  return (length(node) > 1)
}

isNumeric = function(node){
  return( node[[1]] )
}

isCategorical = function(node){
  return(node[[1]] == FALSE)
}

# Data type tree
# 




# Measure the impurity of a list of binary values using the GINI metric
# Since y is a list of 0's and 1's, adding all elements in y is
# the same as counting the number of 1's in y
impurity = function(y) {
  if (length(y) > 0)
  {
    #TODO check this
    p1 <- sum(y) / length(y)
  }
  else
  {
    p1 <- sum(y)
  }
  
  return (p1 * (1-p1))
  }

# Given numeric inputs, find the best splitting point 
# Inputs:
# x: a list of numeric values
# y: a list of classifications for each entry in x
# Outputs: the split point which reduces the impurity the most
# And the impurity reduction
bestNumericSplit <- function(x,y) {
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
  #find index of diff with max element
  resultIndex = which.max(diffs)
  
  #return both the split, and the delta diff, for comparing with other attrs
  return (c(splitPoints[resultIndex], diffs[resultIndex]))

}

binarySplitDelta <- function(x,y) {
  
  n <- length(y)

  
  leftImp <- impurity(y[x == 0])
  rightImp <- impurity(y[x == 1])
  
  rootImp <- impurity(y)

  
  p0 = length(y[y == 0])/n
  p1 = length(y[y == 1])/n
  

  
  diff <- rootImp - (p0*leftImp + p1*rightImp)
  

  
  #return both the split, and the delta diff, for comparing with other attrs
  return (diff)
  
}

#
#bestAttribute <- function(tree)

tree.grow <- function(x,y,nmin,minleaf)
{
  
  #number of columns
  numAttributes <- (length(x))
  
  n <- length(x[,1])
  
  #Find out if each column is numeric
  isNumeric <- FALSE
  
  rowNumbers <- (1:n)
  
  for (i in 1:numAttributes)
  {
    #Find the index of each element not zero or 1
    nonZeroOneElements = rowNumbers[x[,i] != 0 || x[,i] != 1]
    #If we found at least one, then this is numeric
    isNumeric[i] <- (length(nonZeroOneElements) > 0)
  }
  
  
  
  
  tree.growHelper(x,y,nmin, minleaf, numAttributes, isNumeric)
}
  
# x : attributes
# y : class values
tree.growHelper <- function (x, y, nmin, minleaf, numAttributes, isNumeric) 
{
  #number of data points
  print("Initial data")
  print(x)
  print(y)

  n <- length(x[,1])
  
  
  
  if (length(y) != n)
  {
    #Impossible
  }
  
  #Find the majority class
  majorityClass <- 0
  #Check if more than half are 1's
  if (sum(y) > (length(y) / 2.0))
  {
    majorityClass <- 1
  }
  
  
  if (length(x) < nmin)
  {
    print("Making leaf")
    #Return a new leaf that classifies everything to the majority class
    return (makeLeaf(majorityClass)) #leaf
  }
  else
  {
    #Find best split where each child has at least minleaf data points
    #Then recursively grow each side of the split
    bestColumnForSplit <- 0
    bestSplitPoint <- -9999
    bestSplitDelta <- -9999
    for (i in 1:numAttributes)
    {
      if (isNumeric[i])
      {
        result <- bestNumericSplit(x[,i], y)
        splitPoint <- result[1]
        deltaDiff <- result[2]

        
        
        if (bestSplitDelta < deltaDiff)
        {
          bestColumnForSplit <- i
          bestSplitPoint <- splitPoint
          bestSplitDelta <- deltaDiff
        }
      }
      else
      {
        deltaDiff <- binarySplitDelta(x[,i], y)
        
        
        if (bestSplitDelta < deltaDiff)
        {
          bestColumnForSplit <- i
          bestSplitDelta <- deltaDiff
        }
        
      }
      
    }
    
    #Now that we have the best split, we divide our data
    if (isNumeric[bestColumnForSplit])
    {
      xLeft <- x[x[bestColumnForSplit] <= bestSplitPoint,]
      yLeft <- y[x[bestColumnForSplit] <= bestSplitPoint]
      xRight <- x[x[bestColumnForSplit] > bestSplitPoint,]
      yRight <- y[x[bestColumnForSplit] > bestSplitPoint]
      
      print("xLeft")
      print(length(xLeft[,1]))
      print(xLeft)
      
      print("xRight")
      print(length(xRight[,1]))
      print(xRight)
      
      
      #Recursively build the trees for each data set
      
      if ( (length(xLeft[,1]) == 0) || (length(xRight[,1]) == 0) )
      {
        print("Making leaf 1, length 0")
        return (makeLeaf(majorityClass))
      }
      
      leftTree <- tree.growHelper(xLeft, yLeft, nmin, minleaf, numAttributes, isNumeric)
      rightTree <- tree.growHelper(xRight, yRight, nmin, minleaf, numAttributes, isNumeric)
      
      return(
        makeNumericSplit(bestColumnForSplit, bestSplitPoint, leftTree, rightTree)
        )
    }
    else
    {
      xLeft <- x[x[bestColumnForSplit] == 0,]
      yLeft <- y[x[bestColumnForSplit] == 0]
      xRight <- x[x[bestColumnForSplit] == 1,]
      yRight <- y[x[bestColumnForSplit] == 1]
      
      print("xLeft")
      print(length(xLeft[,1]))
      print(xLeft)
      
      print("xRight")
      print(length(xRight[,1]))
      print(xRight)
      
      
      if ((length(xLeft[,1]) == 0) || (length(xRight[,1]) == 0))
      {
        print("Making leaf, length 0")
        return (makeLeaf(majorityClass))
      }
      
      leftTree <- tree.growHelper(xLeft, yLeft, nmin, minleaf, numAttributes, isNumeric)
      rightTree <- tree.growHelper(xLeft, yLeft, nmin, minleaf, numAttributes, isNumeric)
      
      return(
        makeCategoricalcSplit(bestColumnForSplit, leftTree, rightTree)
      )
    }
    
    
    
  }
}

tree.classify <- function(x, tr)
{
  0
}

