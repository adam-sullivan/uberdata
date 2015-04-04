naiveBayesModel <- function(testTrip){
  nbModel <- naiveBayes(timeOfDay  ~ uid + day + numUniqueClusters + startCluster, data = testTrip,laplace = 10)
  return(nbModel)
}


naiveBayesTimeAnalysis <- function(model, testData){
  require(e1071)
  nbPreds <- predict(model,testData)
  cfMatrix <- confusionMatrix(table(nbPreds, testData$timeOfDay))
  return(cfMatrix)
}
  


multinomialHierBayesModel <- function(testTrip){
  
  listOfUids <- unique(testTrip$uid)
  regdata <- NULL 
  for (i in 1:100) { 
    filter <- testTrip$uid==listOfUids[i] 
    y <- tabulate(testTrip$endCluster[filter],32)
    X <- cbind( 
      as.matrix(testTrip[filter,names(testTrip) %in% c("timeOfDay",
                                                       "day","startCluster","numUniqueClusters")]))
    regdata[[i]] <- list(y=y, X=X) 
  }
  
  mcmc <- list(R=1000,use=10)
  out = rhierMnlRwMixture(Data=list(p=32,lgtdata = regdata), Mcmc = mcmc)
  
  table(mapPreds,testTrip$endCluster)
  sum(mapPreds == testTrip$endCluster)
  colnames(myPreds)
  
  
}
mlogitModel <- function(testTrip){
  testTrip <- trainFeatures

  library("nnet")
  summary(multinom(endCluster ~ startCluster, data = testTrip))
  
tD <- mlogit.data(testTrip,choice="endCluster", shape='wide',id.var='uid')
cModel <- mlogit(endCluster ~ timeOfDay | 1| startCluster, tD)
#cModel <- mlogit(endCluster ~ startCluster | startCluster, tD)
myPreds <- predict(cModel,tD)
mapPreds <- colnames(myPreds)[apply(myPreds,1,which.max)]
}



