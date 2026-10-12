################################################################################
# Copyright 2016 Indiana University
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
################################################################################

#' Generates clusters from multivariate normal distributions
#'
#' \code{GenerateClusterData} generates clusters of feature vectors drawn
#' from multivariate normal (MVN) distributions.  The mean values of the normal
#' distribution corresponding to the first cluster is always at the origin. 
#' The remaining clusters are generated from MVN distributions with mean values
#' at v_i and -v_i where v_i is the i-th unit vector.  The clusters are
#' generated in the following order by mean value of the MVN for each cluster:
#' origin, v_1, -v_1, v_2, -v_2, v_3, -v_3,..., v_{(numberOfClusters-1)/2},
#' -v_{(numberOfClusters-1)/2} (if \code{numberOfClusters} is odd)
#' origin, v_1, -v_1, v_2, -v_2, v_3, -v_3,..., v_{(numberOfClusters-1)/2}
#' (if \code{numberOfClusters} is even). 
#'
#' @param numberOfFeatures the number of features, the dimension of the feature
#'   space
#' @param numberOfVectorsPerCluster the number of vectors to randomly generate
#'   for each cluster
#' @param numberOfClusters the number of clusters to be generated.  The value
#'   of this parameter must be in the interval [1,2*\code{numberOfFeatures}+1]
#' @return a list containing a matrix of feature vectors \code{featureVectors}
#'   as rows of feature vectors,
#'   number of features \code{numberOfFeatures}, number of feature vectors
#'   \code{numberOfFeatureVectors}, and number of clusters
#'   \code{numberOfClusters}.
GenerateClusterData <- function(numberOfFeatures, numberOfVectorsPerCluster,
   numberOfClusters=2*numberOfFeatures+1) {

   if (numberOfClusters < 1) {
      stop("numberOfClusters must be greater than zero")
   }

   if (numberOfClusters > 2*numberOfFeatures+1) {
      stop("numberOfClusters must be less than or equal to 2*numberOfFeatures+1")
   }

   inflationFactor <- 3
   numberOfFeatureVectors <- numberOfClusters*numberOfVectorsPerCluster
   featureVectors <- rep(0.0, numberOfFeatureVectors*numberOfFeatures)
   dim(featureVectors) <- c(numberOfFeatureVectors, numberOfFeatures)

   # Generate cluster at origin
   mu <- rep(0.0, numberOfFeatures)
   dim(mu) <- c(1, numberOfFeatures)
   S <- diag(inflationFactor*stats::runif(numberOfFeatures), numberOfFeatures, numberOfFeatures)
#  cat(sprintf("Populating feature vectors %d--%d\n", 1, numberOfVectorsPerCluster))
   featureVectors[1:numberOfVectorsPerCluster, ] <- mvtnorm::rmvnorm(numberOfVectorsPerCluster, mu, S)

   if (numberOfClusters > 1) {
      for (i in 1:(numberOfClusters-1)) {
         if (i %% 2 == 0) {
            d <- i/2
            v <- -1
         } else {
            d <- (i+1)/2
            v <- 1
         }

         mu <- rep(0.0, numberOfFeatures)
         mu[d] <- v

         S <- diag(inflationFactor*stats::runif(numberOfFeatures), numberOfFeatures, numberOfFeatures)

         rangeStart <- i*numberOfVectorsPerCluster+1
         rangeEnd <- rangeStart + numberOfVectorsPerCluster - 1
#         cat(sprintf("Populating feature vectors %d--%d\n", rangeStart, rangeEnd))
         featureVectors[rangeStart:rangeEnd, ] <- mvtnorm::rmvnorm(numberOfVectorsPerCluster, mu, S)
      }
   }

#  cat(sprintf("featureVectors        : %d x %d\n", dim(featureVectors)[1], dim(featureVectors)[2]))
#  cat(sprintf("numberOfFeatures      : %d\n", numberOfFeatures))
#  cat(sprintf("numberOfFeatureVectors: %d\n", numberOfFeatureVectors))
#  cat(sprintf("numberOfClusters      : %d\n", numberOfClusters))

   return (list("featureVectors" = featureVectors, "numberOfFeatures" = numberOfFeatures, "numberOfFeatureVectors" = numberOfFeatureVectors, "numberOfClusters" = numberOfClusters))
}


#' Allocates and initializes input to the clustering for machine learning
#' microbenchmarks
#'
#' \code{ClusteringAllocator} allocates and initializes the data sets that are
#' input to the clustering microbenchmarks for the purposes of conducting a
#' single performance trial with one of the clustering microbenchmark functions.
#'
#' @param benchmarkParameters an object of type
#'   \code{\link{ClusteringMicrobenchmark}} specifying various parameters
#'   needed to generate input for the clustering microbenchmarks.
#' @return a list containing the data objects to be input to the
#'   clustering microbenchmark
#' @seealso \code{\link{PamClusteringMicrobenchmark}} \code{\link{ClaraClusteringMicrobenchmark}}
#'
#' @export
ClusteringAllocator <- function(benchmarkParameters) {
   # Create list of kernel parameters
   kernelParameters <- GenerateClusterData(benchmarkParameters$numberOfFeatures,
      benchmarkParameters$numberOfFeatureVectorsPerCluster,
      benchmarkParameters$numberOfClusters)

   if (dim(kernelParameters$featureVectors)[1] %% kernelParameters$numberOfClusters != 0) {
      stop("Number of feature vectors must be a multiple of the number of clusters")
   }

   return (kernelParameters)
}


#' Conducts a single performance trial with the cluster::pam function
#'
#' \code{ClusteringMicrobenchmark} conducts a single performance trial
#' of the cluster::pam function with the data given in the
#' \code{kernelParameters} parameter.
#'
#' @param benchmarkParameters an object of type
#'   \code{\link{ClusteringMicrobenchmark}} specifying various parameters
#'   for microbenchmarking the cluster::pam function
#' @param kernelParameters a list of data objects to be used as input to
#'   the clustering function
#' @return a vector containing the user, system, and elapsed performance
#'   timings in that order
#'
#' @examples
#' \dontrun{
#' # Allocate input to the pam clustering microbenchmark
#' microbenchmarks <- GetClusteringExampleMicrobenchmarks()
#' kernelParameters <- ClusteringAllocator(microbenchmarks[["pam_cluster_3_3_1000"]])
#' # Execute the microbenchmark
#' timings <- PamClusteringMicrobenchmark(
#'    microbenchmarks[["pam_cluster_3_3_1000"]], kernelParameters)
#' }
#'
#' @export
PamClusteringMicrobenchmark <- function(benchmarkParameters, kernelParameters) {
   timings <- system.time({clusterObj <- cluster::pam(kernelParameters$featureVectors,
      kernelParameters$numberOfClusters, cluster.only=TRUE, trace.lev=1)})
   return (timings)
}


#' Conducts a single performance trial with the cluster::clara function
#'
#' \code{ClusteringMicrobenchmark} conducts a single performance trial
#' of the cluster::clara function with the data given in the
#' \code{kernelParameters} parameter.
#'
#' @param benchmarkParameters an object of type
#'   \code{\link{ClusteringMicrobenchmark}} specifying various parameters
#'   for microbenchmarking the cluster::clara function
#' @param kernelParameters a list of data objects to be used as input to
#'   the clustering function
#' @return a vector containing the user, system, and elapsed performance
#'   timings in that order
#'
#' @examples
#' \dontrun{
#' # Allocate input to the pam clustering microbenchmark
#' microbenchmarks <- GetClusteringExampleMicrobenchmarks()
#' kernelParameters <- ClusteringAllocator(microbenchmarks[["clara_cluster_3_3_1000"]])
#' # Execute the microbenchmark
#' timings <- ClaraClusteringMicrobenchmark(
#'    microbenchmarks[["clara_cluster_3_3_1000"]], kernelParameters)
#' }
#'
#' @export
ClaraClusteringMicrobenchmark <- function(benchmarkParameters, kernelParameters) {
   timings <- system.time({clusterObj <- cluster::clara(kernelParameters$featureVectors,
      kernelParameters$numberOfClusters, samples=50, trace=0, keep.data=FALSE,
      rngR=TRUE, pamLike=TRUE)})
   return (timings)
}

