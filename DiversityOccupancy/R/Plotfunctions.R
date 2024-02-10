#' plot the response of an occupancy model to the change of aparticular variable
#'
#' This function takes a batchoccupancy object and one of the variables used to
#' predict occupacy, and makes a plot showing the response of occupancyt against
#' the selected variable. This function automatically limits the values of that
#' variable to the maximum and minimum values of the dataset.
#' @param batch A result from the batchoccu function.
#' @param spp The species number of which response is going to be ploted.
#' @param variable The variable of which the response is to be ploted.
#' @return a ggplot object plotting the alpha diversity response to the selected
#' variable.
#' @examples
#' \dontrun{
#' data("IslandBirds")
#' data("Daily_Cov")
#' data("siteCov")
#' BirdOccupancy <-batchoccu(pres = IslandBirds, sitecov = siteCov, obscov =
#' Daily_Cov, spp = 5, form = ~ Day + Wind + Rime + Noise + Clouds ~
#' Elev + AgroFo + SecVec + Wetland)
#' #plot the response of occupancy to individual variables for species 4 and 5
#'
#' responseplot.occu(batch = BirdOccupancy, spp = 4, variable = Elev)
#'
#'
#' responseplot.occu(batch = BirdOccupancy, spp = 5, variable = Elev)
#' }
#' @seealso \code{\link[DiversityOccupancy]{batchoccu}}
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ylim
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export

responseplot.occu <- function(batch, spp, variable){
  upper <- lower <- NULL # Setting the variables to NULL first
  A<-data.frame(matrix(rep(colMeans(batch$Covs), each=length(batch$Covs[,1])), nrow = length(batch$Covs[,1]), ncol = ncol(batch$Covs)))
  colnames(A)<-colnames(batch$Covs)
  maxval<-apply(batch$Covs,2,max)
  minval<-apply(batch$Covs,2,min)
  newdata<- seq(from = minval[colnames(A)== as.character(substitute(variable))], to = maxval[colnames(A)== as.character(substitute(variable))], along.with = batch$Covs[,1])
  A[colnames(A)== as.character(substitute(variable))] <- newdata
  B<-predict(batch$models[[spp]], type = "state", newdata = A)
  DF<- data.frame(preditction = B$Predicted, upper = (B$Predicted + B$SE), lower = (B$Predicted - B$SE), dependent = A[colnames(A)== as.character(substitute(variable))])
  result <- ggplot(DF, aes(x= DF[,4], y = DF[,1])) + geom_ribbon(aes(ymax= upper, ymin = lower), fill = "grey") + geom_line() + theme_bw() + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + labs(x = as.character(substitute(variable)), y = "Occupancy") + ylim(c(min(c(DF$lower, 0)),max(c(DF$upper, 1))))
  return(result)
}

#' plot the response of an abundance model to the change of aparticular variable
#'
#' This function takes a diversityoccupancy object and one of the variables used
#' to predict abundance, and makes a plot showing the response of occupancyt
#' against the selected variable. This function automatically limits the values
#' of that variable to the maximum and minimum values of the dataset.
#' @param batch A result from the diversityoccu function.
#' @param spp The species number of which response is going to be ploted.
#' @param variable The variable of which the response is to be ploted.
#' @return a ggplot object plotting the alpha diversity response to the selected
#' variable.
#' @examples
#' \dontrun{
#' data("IslandBirds")
#' data("Daily_Cov")
#' data("siteCov")
#'
#' #Model the abundance for 5 bird species and calculate alpha diversity from that
#'
#' BirdDiversity <-diversityoccu(pres = IslandBirds, sitecov = siteCov,
#' obscov = Daily_Cov,spp = 5, form = ~ Day + Wind + Time + Rain +
#' Noise ~ Elev + AgroFo + SecVec + Wetland + Upland)
#'
#' #plot the response of abundance to individual variables for species 4, 11
#'
#' responseplot.abund(batch = BirdDiversity, spp = 4, variable = Elev)
#'
#' responseplot.abund(batch = BirdDiversity, spp = 11, variable = Elev)
#' }
#' @export
#' @seealso \code{\link[DiversityOccupancy]{batchoccu}}
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ylim
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>

responseplot.abund <- function(batch, spp, variable){
  upper <- lower <- NULL # Setting the variables to NULL first
  A<-data.frame(matrix(rep(colMeans(batch$Covs), each=length(batch$Covs[,1])), nrow = length(batch$Covs[,1]), ncol = ncol(batch$Covs)))
  colnames(A)<-colnames(batch$Covs)
  maxval<-apply(batch$Covs,2,max)
  minval<-apply(batch$Covs,2,min)
  newdata<- seq(from = minval[colnames(A)== as.character(substitute(variable))], to = maxval[colnames(A)== as.character(substitute(variable))], along.with = batch$Covs[,1])
  A[colnames(A)== as.character(substitute(variable))] <- newdata
  B<-predict(batch$models[[spp]], type = "state", newdata = A)
  DF<- data.frame(preditction = B$Predicted, upper = (B$Predicted + B$SE), lower = (B$Predicted - B$SE), dependent = A[colnames(A)== as.character(substitute(variable))])
  result <- ggplot(DF, aes(x= DF[,4], y = DF[,1])) + geom_ribbon(aes(ymax= upper, ymin = lower), fill = "grey") + geom_line() + theme_bw() + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + labs(x = as.character(substitute(variable)), y = "Abundance") + ylim(c(min(c(DF$lower, 0)),max(DF$upper)))
  return(result)
}

#' plot the response of the calculated alpha diversity to the change of a
#' particular variable
#'
#' This function takes a modeldiversity object and one of the variables used to
#' predict the alpha diversity index, and makes a plot showing the response of
#' the diversity index against the selected variable. This function automatically
#' limits the values of that variable to the maximum and minimum values of the
#' dataset.
#' @param model A result from the model.diversity function.
#' @param variable The variable of which the response is to be ploted.
#' @return a ggplot object plotting the alpha diversity response to the selected
#' variable.
#' @examples
#' \dontrun{
#' #Load the data
#' data("IslandBirds")
#' data("Daily_Cov")
#' data("siteCov")
#'
#' #Model the abundance for 5 bird species and calculate alpha diversity from that
#'
#' BirdDiversity <-diversityoccu(pres = IslandBirds, sitecov = siteCov,
#' obscov = Daily_Cov,spp = 5, form = ~ Day + Wind + Time + Rain +
#' Noise ~ Elev + AgroFo + SecVec + Wetland + Upland)
#'
#' #Select the best model that explains diversity using genetic algorithms
#' set.seed(123)
#' glm.Birdiversity <- model.diversity(BirdDiversity, method = "g")
#'
#' #see the best models
#'
#' glm.Birdiversity$Best.model
#'
#' #plot the response of diversity to individual variables
#'
#' plot(glm.Birdiversity, elev)
#' }
#' @export
#' @seealso \code{\link[DiversityOccupancy]{diversityoccu}}
#' @seealso \code{\link[DiversityOccupancy]{model.diversity}}
#' @importFrom stats glm
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 labs
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>

responseplot.diver<- function(model, variable){
  A<-data.frame(matrix(rep(colMeans(model$dataset), each=length(model$dataset[,1])), nrow = length(model$dataset[,1]), ncol = ncol(model$dataset)))
  colnames(A)<-colnames(model$dataset)
  maxval<-apply(model$dataset,2,max)
  minval<-apply(model$dataset,2,min)
  newdata<- seq(from = minval[colnames(A)== as.character(substitute(variable))], to = maxval[colnames(A)== as.character(substitute(variable))], along.with = model$dataset[,1])
  A[colnames(A)== as.character(substitute(variable))] <- newdata
  B<-predict(glm(model$Best_model, data= model$dataset), newdata = A, se.fit = TRUE)
  DF<- data.frame(preditction = B$fit, upper = (B$fit + B$se), lower = (B$fit - B$se), dependent = A[colnames(A)== as.character(substitute(variable))])
  result <- ggplot(DF, aes(x= DF[,4], y = DF[,1])) + geom_ribbon(aes(ymax= DF[,2], ymin = DF[,3]), fill = "grey") + geom_line() + theme_bw() + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + labs(x = as.character(substitute(variable)), y = "Diversity")
  return(result)
}
