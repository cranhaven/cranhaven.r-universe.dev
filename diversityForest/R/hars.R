##' Data on human activity recognition using smartphones
##' 
##' This data set contains sensor data from 30 volunteers aged 19-48 years, performing 
##' six activities while wearing Samsung Galaxy S II smartphones on their waists. 
##' The sensors recorded 3-axial linear acceleration and angular velocity at 50Hz. 
##' The experiments were video-recorded to label the data manually. The outcome 
##' \code{Activity} is categorical with six classes that differentiate the six 
##' activities.\cr
##' This is an updated version of the Human Activity Recognition Using Smartphones 
##' data set published in the UC Irvine Machine Learning Repository. This updated 
##' version published on OpenML includes both raw sensor signals and updated 
##' activity labels, with aggregated measurements for each individual and activity.
##'
##' The classes of the outcome \code{Activity} are as follows: \code{LAYING}, 
##' \code{SITTING}, \code{STANDING}, \code{WALKING}, \code{WALKING_DOWNSTAIRS}, 
##' \code{WALKING_UPSTAIRS}.\cr
##' The OpenML data set contained one additional variable \code{Person} 
##' that was removed because it has too many factors to use it as a covariate 
##' in prediction.
##' 
##' @format A data frame with 180 observations (activities), 66 covariates and one 
##' 6-class outcome variable
##' @source 
##' \itemize{
##'   \item Updated version: OpenML: data.name: Smartphone-Based_Recognition_of_Human_Activities, data.id: 4153, link: \url{https://www.openml.org/d/4153/} (Accessed: 29/08/2024)
##'   \item Original version: UC Irvine Machine Learning Repository, link: \url{https://archive.ics.uci.edu/dataset/240/human+activity+recognition+using+smartphones/} (Accessed: 29/08/2024)
##' }
##'
##' @examples
##' 
##' # Load data:
##' data(hars)
##' 
##' # Numbers of observations per outcome class:
##' table(hars$Activity)
##' 
##' # Dimension of data:
##' dim(hars)
##'
##' # First rows of (subset) data:
##' head(hars[,1:5])
##' 
##' @references
##' \itemize{
##'   \item Reyes-Ortiz, J.-L., Oneto, L., Samà, A., Parra, X., Anguita, D. (2016). Transition-aware human activity recognition using smartphones. Neurocomputing, 171:754-767, <\doi{10.1016/j.neucom.2015.07.085}>.
##'   \item Vanschoren, J., van Rijn, J. N., Bischl, B., Torgo, L. (2013). OpenML: networked science in machine learning. SIGKDD Explorations 15(2):49-60, <\doi{10.1145/2641190.2641198}>.
##'   \item Dua, D., Graff, C. (2019). UCI Machine Learning Repository. Irvine, CA: University of California, School of Information and Computer Science. \url{https://archive.ics.uci.edu/ml/}.
##' }
##'
##' @name hars
NULL