#Skeleton script for testing our algorithm

nminValues = 2^(0:12)

minleafValues 2^(0:12)

classifySpam = function(x)
{
  spam.dat <- read.csv('spam.dat')
  
  trainRows = 0 #TODO
  testRows = 0 #TODO
  
  for (nmin in nminValues)
  {
    for (minleaf in minleafValues)
    {
      print(paste("nmin: ", nmin)
      print(paste("nmin: ", minleaf))
      
      tr <- tree.grow(x, y, nmin, minleaf)
      
      results <- tree.classify(testData, tr)
      
      confusion = c(0,0,0,0) #TODO
      
      print("Confusion matrix")
      print(confusion)
    }
  }
}
