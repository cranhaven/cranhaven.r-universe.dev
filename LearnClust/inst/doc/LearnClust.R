## ----setup, include=FALSE-----------------------------------------------------
library(LearnClust)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------

cluster1 <- matrix(c(1,2),ncol=2)

cluster2 <- matrix(c(2,4),ncol=2)

weight <- c(0.2,0.8)

vectorData <- c(1,1,2,3,4,7,8,8,8,10)
# vectorData <- c(1:10)

matrixData <- matrix(vectorData,ncol=2,byrow=TRUE)
print(matrixData)

dfData <- data.frame(matrixData)
print(dfData)
plot(dfData)

cMatrix <- matrix(c(2,4,4,2,3,5,1,1,2,2,5,5,1,0,1,1,2,1,2,4,5,1,2,1), ncol=3, byrow=TRUE)

cDataFrame <- data.frame(cMatrix)


## -----------------------------------------------------------------------------
edistance(cluster1,cluster2)

## -----------------------------------------------------------------------------
mdistance(cluster1,cluster2)

## -----------------------------------------------------------------------------
canberradistance(cluster1,cluster2)

## -----------------------------------------------------------------------------
chebyshevDistance(cluster1,cluster2)

## -----------------------------------------------------------------------------
octileDistance(cluster1,cluster2)

## -----------------------------------------------------------------------------
edistanceW(cluster1,cluster2,weight)

## -----------------------------------------------------------------------------
mdistanceW(cluster1,cluster2,weight)

## -----------------------------------------------------------------------------
canberradistanceW(cluster1,cluster2,weight)

## -----------------------------------------------------------------------------
chebyshevDistanceW(cluster1,cluster2,weight)

## -----------------------------------------------------------------------------
octileDistanceW(cluster1,cluster2,weight)

## -----------------------------------------------------------------------------
list <- toList(vectorData)

# list <- toList(matrixData)

# list <- toList(dfData)

print(list)

## -----------------------------------------------------------------------------
matrixDistance <- mdAgglomerative(list,'MAN','AVG')
print(matrixDistance)

## -----------------------------------------------------------------------------
minDistance <- minDistance(matrixDistance)
print(minDistance)

## -----------------------------------------------------------------------------
groupedClusters <- getCluster(minDistance, matrixDistance)
print(groupedClusters)

## -----------------------------------------------------------------------------
updatedClusters <- newCluster(list, groupedClusters)
print(updatedClusters)

## -----------------------------------------------------------------------------
agglomerativeExample <- agglomerativeHC(dfData,'EUC','MAX')

plot(agglomerativeExample$dendrogram)
print(agglomerativeExample$clusters)
print(agglomerativeExample$groupedClusters)

## -----------------------------------------------------------------------------
cleanClusters <- usefulClusters(updatedClusters)
print(cleanClusters)

## -----------------------------------------------------------------------------
distances <- c(2,4,6,8)

clusterDistanceByApproach <- clusterDistanceByApproach(distances,'AVG')
print(clusterDistanceByApproach)

## -----------------------------------------------------------------------------
clusterDistance <- clusterDistance(cluster1,cluster2,'MAX','MAN')
print(clusterDistance)

## -----------------------------------------------------------------------------
list <- toList.details(vectorData)

# list <- toList(matrixData)

# list <- toList(dfData)

print(list)

## -----------------------------------------------------------------------------
matrixDistance <- mdAgglomerative.details(list,'MAN','AVG')

## -----------------------------------------------------------------------------
minDistance <- minDistance.details(matrixDistance)

## -----------------------------------------------------------------------------
groupedClusters <- getCluster.details(minDistance, matrixDistance)

## -----------------------------------------------------------------------------
updatedClusters <- newCluster.details(list, groupedClusters)

## -----------------------------------------------------------------------------
agglomerativeExample <- agglomerativeHC.details(vectorData,'EUC','MAX')

## -----------------------------------------------------------------------------
 # list <- toListDivisive(vectorData)

# list <- toListDivisive(matrixData)

 list <- toListDivisive(dfData[1:4,])

print(list)

## -----------------------------------------------------------------------------
clustersList <- initClusters(list)
print(clustersList)

## -----------------------------------------------------------------------------
matrixDistance <- mdDivisive(clustersList,'MAN','AVG',list)
print(matrixDistance)

## -----------------------------------------------------------------------------
maxDistance <- maxDistance(matrixDistance)
print(maxDistance)

## -----------------------------------------------------------------------------
dividedClusters <- getClusterDivisive(maxDistance, matrixDistance)
print(dividedClusters)

## -----------------------------------------------------------------------------
divisiveExample <- divisiveHC(dfData[1:4,],'MAN','AVG')
print(divisiveExample)

## -----------------------------------------------------------------------------
data <- c(1,2,1,3,1,4,1,5)

components <- toListDivisive(data)

cluster1 <- matrix(c(1,2,1,3),ncol=2,byrow=TRUE)
cluster2 <- matrix(c(1,4,1,5),ncol=2,byrow=TRUE)
cluster3 <- matrix(c(1,6,1,7),ncol=2,byrow=TRUE)

complementaryClusters(components,cluster1,cluster2)

complementaryClusters(components,cluster1,cluster3)


## -----------------------------------------------------------------------------
complementaryClusters.details(components,cluster1,cluster2)

## -----------------------------------------------------------------------------
# list <- toListDivisive.details(vectorData)

# list <- toListDivisive(matrixData)

 list <- toListDivisive(dfData[1:4,])

print(list)

## -----------------------------------------------------------------------------
clustersList <- initClusters.details(list)

## -----------------------------------------------------------------------------
matrixDistance <- mdDivisive.details(clustersList,'MAN','AVG',list)

## -----------------------------------------------------------------------------
maxDistance <- maxDistance.details(matrixDistance)

## -----------------------------------------------------------------------------
dividedClusters <- getClusterDivisive.details(maxDistance, matrixDistance)

## -----------------------------------------------------------------------------
divisiveExample <- divisiveHC.details(dfData[1:4,],'MAN','AVG')
print(divisiveExample)


## -----------------------------------------------------------------------------
initData <- initData(cDataFrame)
print(initData)

## -----------------------------------------------------------------------------
target <- c(1,2,3)

initTarget <- initTarget(target,cDataFrame)
print(initTarget)

## -----------------------------------------------------------------------------
weight <- c(5,7,6)

weights <- normalizeWeight(TRUE,weight,cDataFrame)
print(weights)

## -----------------------------------------------------------------------------
cluster1 <- matrix(c(1,2,3),ncol=3)
cluster2 <- matrix(c(2,5,8),ncol=3)

weight <- c(3,7,4)

distance <- distances(cluster1,cluster2,'CHE',weight)
print(distance)

## -----------------------------------------------------------------------------
target <- c(5,5,1)

weight <- c(3,7,5)

correlation <- correlationHC(cDataFrame, target,  weight)

print(correlation$sortedValues)

print(correlation$distances)

plot(correlation$dendrogram)

## -----------------------------------------------------------------------------
initData <- initData.details(cDataFrame)

## -----------------------------------------------------------------------------
targetValid <- c(1,2,3)

targetInvalid <- c(1,2)

initTarget <- initTarget.details(targetValid,cDataFrame)

initTarget <- initTarget.details(targetInvalid,cDataFrame)

## -----------------------------------------------------------------------------
weight <- c(5,7,6)

weights <- normalizeWeight.details(TRUE,weight,cDataFrame)

weights <- normalizeWeight.details(FALSE,weight,cDataFrame)

weights <- normalizeWeight.details(FALSE,NULL,cDataFrame)

## -----------------------------------------------------------------------------
cluster1 <- matrix(c(1,2,3),ncol=3)
cluster2 <- matrix(c(2,5,8),ncol=3)

weight <- c(3,7,4)

distance <- distances.details(cluster1,cluster2,'CHE',weight)

## -----------------------------------------------------------------------------
target <- c(5,5,1)

weight <- c(3,7,5)

correlation <- correlationHC.details(cDataFrame, target,  weight)

print(correlation$sortedValues)

print(correlation$distances)

plot(correlation$dendrogram)

