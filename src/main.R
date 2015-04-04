# main.R 
# Script: Main API call for the Uber-Nashville package
# Author: Adam Sullivan
# Date: 4/3/15
# Rev 1

################
# Load external dependencies
library(plyr)
library(geosphere)
require(e1071)
require(caret)
require(mlogit)


# Load functions
#setwd('Documents/uberproject')
source('src/dataPreprocess.R')
source('src/featureEngineering.R')
source('src/Prediction.R')


# External arguments
trainMode <- 1
apiMode <- 0
saveResults <- 1
testFile <- '/data/hw1_test.csv'
resourceModel <- 'outputs//bestModel.Rda'



####### 
# Load in the data files

if (trainMode){
  trainData <- preprocessData('data/hw1_train.csv')
  testData <- preprocessData('data/hw1_test.csv')
  trainFeatures <- featureEngineering(trainData,truncatedData = TRUE)
  testFeatures <- featureEngineering(testData,truncatedData = TRUE)
  bestModel <- naiveBayesModel(trainFeatures)
  if(saveResults){
    save(bestModel,file='outputs/bestModel.Rda')
  }
}



if (apiMode){
  testData <- preprocessData(testFile)
  testFeatures <- featureEngineering(testData,truncatedData = TRUE)
  load(resourceModel)
  
}

confusionResults <- naiveBayesTimeAnalysis(bestModel, testFeatures)



# Summary statistics
nUsers <- length(unique(trainData$uid))
tripFreq <- count(trainData, .(uid))
hist(tripFreq$freq)

