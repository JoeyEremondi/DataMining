#Skeleton script for testing our algorithm

n = length(Spambase[,1])

#Adjust this to adjust different nmin and minleaf values
#We test on all powers of 2 up to our data set length
#But this takes several hours to run

nminValues = 2^(0:12)

minleafValues = 2^(0:12)


#Run the tests
classifySpam = function()
{
  all <- c(1:n)
  seventyPercentOfAll <- sort(unique(sample(n, (0.7*n))))
  restOfAll <- all[-seventyPercentOfAll]
  
  TrainingSpambase <- Spambase[seventyPercentOfAll,]
  TestSpambase <- Spambase[restOfAll,]
  
  
    
  for (nmin in nminValues)
  {
    for (minleaf in minleafValues)
    {

      
            x <- TrainingSpambase[,(1:57)]
            y <- TrainingSpambase[,58]
            
            tr <- tree.grow(x, y, nmin, minleaf)
            
            results <- tree.classify(TestSpambase[,(1:57)], tr)
      
            realResults = TestSpambase[,58]
            
            successRate = sum(results == realResults)/length(TestSpambase[,58])
      
            errorRate = 1 - successRate
            
            print(paste("nmin ", nmin, "minleaf ", minleaf, "error ", errorRate ))
    }
  }
}
