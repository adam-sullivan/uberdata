NUMCENTERS = 32

# Feature Ideas
# - Day of week *
# - weekend 
# - common start place (do other people frequent where you're being picked up) (clustering)*
# - common end place (do other people frequent where you're starting from) *
# - Common time for you (is it an abnormal time to ask for pickup?)
# - is it a "common occurence"? Starts, ends, and times near a common place
# - partial part of a trip
# - probable home location
# - probable work location
# - cluster *
# - type of trip *
# - distance *

findDayOfWeek <- function(dataFrame){
  return(weekdays(as.Date(dataFrame$begintrip_at)))
}


findClusteredLocations <- function(dataFrame){
  locClusters <-kmeans(rbind(cbind(dataFrame$begintrip_lat, dataFrame$begintrip_lng),
                             cbind(dataFrame$dropoff_lat, dataFrame$dropoff_lng)),
                       centers=NUMCENTERS)
  
  return(list(startClusters = locClusters$cluster[1:(length(locClusters$cluster)/2)],
              endClusters = locClusters$cluster[(length(locClusters$cluster)/2 + 1):
                                                  length(locClusters$cluster)]))
}

calcTripDistance <- function(distFrame,type){
  if(type == 'haversine'){
  library(geosphere)
  dist <- distHaversine(cbind(distFrame$dropoff_lat, distFrame$dropoff_lng),
                        cbind(distFrame$begintrip_lat, distFrame$begintrip_lng))
  }
  else if(type == 'euclid'){
    dist <- sqrt((distFrame$dropoff_lat - distFrame$begintrip_lat)^2 + 
                   (distFrame$dropoff_lng - distFrame$begintrip_lng)^2)
  }
  return(dist) 
}

timeOfDayFnc <- function(tripFrame){
  catHour <- {}
  candidateHour <- as.numeric(tripFrame)  
  if(candidateHour > 6 & candidateHour <= 12){
    catHour <- "morning"
  }
  if (candidateHour > 12  & candidateHour <= 17){
    catHour <- "afternoon"
  }
  if (candidateHour > 17 & candidateHour <= 20){
    catHour <- "evening"
  }
 if (candidateHour > 20 | candidateHour <= 6){
    catHour <- "overnight"
  }
  return(catHour)
}


#' Take a given data frame and produce a feature vector for each unique 
#' row.
#' 
#' @param tripData A data frame with dateTime, startLat/Long, stopLat/Long, and uid.
#' @return featureFrame A data frame with the equivalent features calcualted
#' @examples
#' featureEngineering(tripData)
featureEngineering <- function(tripData){
  # Calculate the trip distance
  tripData$distance <- calcTripDistance(tripData,'haversine')
  tripData$day <- as.factor(findDayOfWeek(tripData))
  tripData$timeOfDay <- sapply(strftime(tripData$begintrip_at, format="%H"),timeOfDayFnc)
  tripData$startCluster <- as.factor(findClusteredLocations(tripData)$startClusters)
  tripData$endCluster <- as.factor(findClusteredLocations(tripData)$endClusters)
  tripHistory <- ddply(.data=tripData,.(uid),summarise, numUniqueClusters= length(unique(startCluster)),
                      mostTripsToSingleCluster = max(table(startCluster)),
                      totalTrips = length(endCluster))
  tripData <- merge(tripData,tripHistory,by='uid')
  hclustData <- tripData[,names(tripData) %in% c("day","startCluster", "day","endCluster")]
  hclustData$day <- as.factor(hclustData$day)
  hclustData$startCluster <- as.factor(hclustData$startCluster)
  hclustData$endCluster <- as.factor(hclustData$endCluster)
  startClusterProps <- as.data.frame.matrix(prop.table(table(tripData$uid, tripData$startCluster),1))
  endClusterProps<- as.data.frame.matrix(prop.table(table(tripData$uid, tripData$endCluster),1))
  dayProps <- as.data.frame.matrix(prop.table(table(tripData$uid, tripData$day),1))
  dayProps$uid <- rownames(dayProps)
  timeProps <- as.data.frame.matrix(prop.table(table(tripData$uid, tripData$timeOfDay),1))
  timeProps$uid <- rownames(timeProps)
  startClusterProps$uid <- rownames(startClusterProps)
  endClusterProps$uid <- rownames(endClusterProps)
  mergedClusterStarts <- merge(startClusterProps,tripData)
  tripData <- merge(endClusterProps,mergedClusterStarts, by='uid')
  tripData <- merge(tripData, dayProps,by='uid')
  tripData <- merge(tripData, timeProps,by='uid')
  globalPriors <- {}
  
  
 
}
regdata <- {}
keepList <- unique(tripData$uid)
testTrip <- tripData[tripData$uid %in% keepList,]
testTrip <- testTrip[,names(testTrip) %in% c("uid","timeOfDay",
                                               "day","startCluster","endCluster","numUniqueClusters")]
head(testTrip)
testTrip$timeOfDay <- as.factor(testTrip$timeOfDay)
testTrip$day <- as.factor(testTrip$day)
testTrip$startCluster <- as.factor(testTrip$startCluster)
testTrip$endCluster <- as.factor(testTrip$endCluster)
testTrip$uid <- as.factor(testTrip$uid)
testTrip$numUniqueClusters <- as.factor(testTrip$numUniqueClusters)

nb <- naiveBayes(timeOfDay  ~ uid + day + numUniqueClusters + startCluster, data = testTrip,laplace = 10)
nb <- naiveBayes(endCluster  ~ timeOfDay + uid + day + numUniqueClusters, data = testTrip,laplace = 10)
myPreds <- predict(nb,testTrip[,c(1,2,3,4,6)])
sum(myPreds==testTrip$endCluster)
myPreds <- predict(nb,testTrip)
sum(myPreds==testTrip$timeOfDay)


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
constraints = vector("list", 2)

Prior=list(ncomp=1)
out <- rhierLinearModel(Data=list(regdata=regdata),Mcmc=mcmc)

#regdata <- list(p=32,regdata)
xcoding = c(0,0)
options = list(none=FALSE, save=TRUE, keep=1)
out = rhierMnlRwMixture(Data=list(p=32,lgtdata = regdata), Mcmc = mcmc)

priorEndLocation <- testTrip[1,(34:65)]
priorTime <- testTrip[1,names(testTrip) %in% c("afternoon","evening","morning","overnight")]
priorDay <- testTrip[1,names(testTrip) %in% c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")]

tD <- mlogit.data(testTrip,choice="endCluster", shape='long',id.var='uid')
cModel <- mlogit(endCluster ~ startCluster|day + timeOfDay + startCluster + numUniqueClusters, tD,probit=TRUE)
cModel <- mlogit(endCluster ~ startCluster | startCluster, tD)
myPreds <- predict(cModel,tD)
mapPreds <- colnames(myPreds)[apply(myPreds,1,which.max)]

table(mapPreds,testTrip$endCluster)
sum(mapPreds == testTrip$endCluster)
colnames(myPreds)




ypooled = c(ypooled, lgtdata[[i]]$y)
nrowX = nrow(lgtdata[[i]]$X)
if ((nrowX/p) != length(lgtdata[[i]]$y)) {
  pandterm(paste("nrow(X) ne p*length(yi); exception at unit", 
                 i))
}
mapPreds <- colnames(myPreds)[apply(myPreds,1,which.max)]

table(mapPreds,testTrip$endCluster)
sum(mapPreds == testTrip$endCluster)
colnames(myPreds)

names(which.max(myPreds))
sapply(myPreds,which.max)


