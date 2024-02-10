#' Fits occupancy models for multiple species detection history
#'
#' This function takes a data.frame with multiple detection history from
#' various species in different sites, covariates of each site to calculate
#' occupancy, variables specific to sampling days to calculate probability of
#' detection. It features an automatic model selection when dredge = TRUE.
#'
#' @param pres a data.frame where rows are the sites and columns are a series of
#' presence-absence observation from multiple species, every species needs to
#' have the same number of observations.
#' @param sitecov a data.frame where every row is a site, and every column is a
#' measurement of that site, such as elevation or slope, this covariates are
#' usually more constant.
#' @param obscov a list where every element is a data frame with the daily
#' covariates for each site, that is a measurement for each day, such as average
#' temperature of a day, this covariates are usually very .
#' @param spp the number of species in the pres data.frame
#' @param form a formula in the format ~ obscov ~ sitcov, the first arguments
#' will be used to calculate probability of detection and the second part the
#' occupancy.
#' @param dredge default = FALSE, if TRUE, for each species, the best occupancy
#' model will be determined by fitting all possible models and ranking by AICc.
#' @return A list with the fitted models for each species and the calculated
#' Alpha diversity for each site.
#' @details
#' This function fits the single season occupancy model of MacKenzie et al (2002),
#' for multiple species and it can automatically select the best model for each
#' specie based on AICc.
#' @examples
#' \dontrun{
#' data("IslandBirds")
#' data("Daily_Cov")
#' data("siteCov")
#' BirdOccupancy <-batchoccu(pres = IslandBirds, sitecov = siteCov, obscov =
#' Daily_Cov, spp =  5, form = ~ Day + Wind + Rain + Noise + Clouds ~
#' Elev + AgroFo + SecVec + Wetland)
#' #plot the response of occupancy to individual variables for species 4 and  5
#'
#' responseplot.occu(batch = BirdOccupancy, spp = 4, variable = Elev)
#'
#' responseplot.occu(batch = BirdOccupancy, spp =  5, variable = Elev)
#' }
#' #Dredge for all species
#' BirdOccupancy2 <- batchoccu(pres = IslandBirds, sitecov = siteCov, obscov =
#' Daily_Cov, spp = 5, form = ~ 1 ~
#' Elev + AgroFo, dredge = TRUE)
#' @seealso \code{\link[DiversityOccupancy]{diversityoccu}}
#' @export
#' @importFrom unmarked occu
#' @importFrom unmarked unmarkedFrameOccu
#' @importMethodsFrom unmarked predict
#' @importFrom MuMIn dredge
#' @importFrom MuMIn AICc
#' @importFrom stats getCall
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>

batchoccu<- function(pres, sitecov, obscov, spp, form, dredge = FALSE) {
  secuencia <- c(1:spp)*(ncol(pres)/spp)
  secuencia2<-secuencia-(secuencia[1]-1)
  models <- vector('list', spp)
  fit <- matrix(NA, nrow(pres), spp)
  colnames(fit) <- paste("species", 1:spp, sep =".")
  if (dredge == FALSE) {
    for(i in 1:length(secuencia)) {
      data <- pres[, secuencia2[i]:secuencia[i]]
      data2 <- unmarkedFrameOccu(y = data, siteCovs = sitecov, obsCovs = obscov)
      models[[i]] <- occu(form, data2)
      fit[, i] <- suppressWarnings(predict(models[[i]], type = "state", newdata = sitecov))$Predicted
      print(paste("Species", as.character(i), "ready!"))
    }
  }
  else {
    for(i in 1:length(secuencia)) {
      data <- pres[, secuencia2[i]:secuencia[i]]
      data2 <- unmarkedFrameOccu(y = data, siteCovs = sitecov, obsCovs = obscov)
      dredged <- suppressWarnings(dredge(occu(form, data2)))
      # select the first model and evaluate
      models[[i]] <- eval(getCall(dredged, 1))
      #predictions for the best model
      fit[, i] <- predict(models[[i]], type = "state", newdata = sitecov)$Predicted
      print(paste("Species", as.character(i), "ready!"))
    }
  }
  result <- list(Covs = sitecov, models = models, fit = fit)
  class(result)<- "batchoccupancy"
  return(result)
}

#' Fits occupancy models for multiple species detection history and calculated model average
#'
#' This function takes a data.frame with multiple detection history from
#' various species in different sites, covariates of each site to calculate
#' occupancy, variables specific to sampling days to calculate probability of
#' detection. It features an automatic model selection when dredge = TRUE.
#'
#' @param pres a data.frame where rows are the sites and columns are a series of
#' presence-absence observation from multiple species, every species needs to
#' have the same number of observations.
#' @param sitecov a data.frame where every row is a site, and every column is a
#' measurement of that site, such as elevation or slope, this covariates are
#' usually more constant.
#' @param obscov a list where every element is a data frame with the daily
#' covariates for each site, that is a measurement for each day, such as average
#' temperature of a day, this covariates are usually very .
#' @param spp the number of species in the pres data.frame
#' @param form a formula in the format ~ obscov ~ sitcov, the first arguments
#' will be used to calculate probability of detection and the second part the
#' occupancy.
#' @param dredge default = FALSE, if TRUE, for each species, the best occupancy
#' model will be determined by fitting all possible models and ranking by AICc.
#' @param delta default = 2, AICc difference used for model averaging
#' @return A list with the average models for each species.
#' @details
#' This function fits the single season occupancy model of MacKenzie et al (2002),
#' for multiple species and it can automatically select the best model for each
#' specie based on AICc, ther result of this function is the model average.
#' @examples
#' \dontrun{
#' data("IslandBirds")
#' data("Daily_Cov")
#' data("siteCov")
#' BirdOccupancy <-batchoccuavg(pres = IslandBirds, sitecov = siteCov, obscov =
#' Daily_Cov, spp =  5, form = ~ Day + Wind + Rain + Noise + Clouds ~
#' Elev + AgroFo + SecVec + Wetland)
#' #Summary of averaged model for species 2
#' summary(BirdOccupancy$models[[2]])
#' }
#' #Dredge for all species
#' @seealso \code{\link[DiversityOccupancy]{diversityoccu}}
#' @export
#' @importFrom unmarked occu
#' @importFrom unmarked unmarkedFrameOccu
#' @importMethodsFrom unmarked predict
#' @importFrom MuMIn dredge
#' @importFrom MuMIn AICc
#' @importFrom MuMIn model.avg
#' @importFrom stats getCall
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>

batchoccuavg <- function (pres, sitecov, obscov, spp, form, dredge = FALSE, delta = 2)
{
  secuencia <- c(1:spp) * (ncol(pres)/spp)
  secuencia2 <- secuencia - (secuencia[1] - 1)
  models <- vector("list", spp)
  fit <- matrix(NA, nrow(pres), spp)
  colnames(fit) <- paste("species", 1:spp, sep = ".")
  if (dredge == FALSE) {
    for (i in 1:length(secuencia)) {
      data <- pres[, secuencia2[i]:secuencia[i]]
      data2 <- unmarkedFrameOccu(y = data, siteCovs = sitecov,
                                 obsCovs = obscov)
      models[[i]] <- occu(form, data2)
      print(paste("Species", as.character(i), "ready!"))
    }
  }
  else {
    for (i in 1:length(secuencia)) {
      data <- pres[, secuencia2[i]:secuencia[i]]
      data2 <- unmarkedFrameOccu(y = data, siteCovs = sitecov,
                                 obsCovs = obscov)
      models[[i]] <- model.avg(suppressWarnings(dredge(occu(form, data2))), delta < delta)
      print(paste("Species", as.character(i), "ready!"))
    }
  }
  result <- list(Covs = sitecov, models = models)
  class(result) <- "batchoccupancyavg"
  return(result)
}

#' Calculates alpha diversity from multiple species occupancy data
#'
#' This function takes a data.frame with multiple presence absence-data from
#' various species in different sites, covariates of each site to calculate
#' occupancy, variables specific to sampling days to calculate probability of
#' detection, and it calculates the alpha diversity for each site.
#'
#' @param pres a data.frame where rows are the sites and columns are a series of
#' presence-absence observation from multiple species, every species needs to
#' have the same number of observations.
#' @param sitecov a data.frame where every row is a site, and every column is a
#' measurement of that site, such as elevation or slope, this covariates are
#' usually more constant.
#' @param obscov a list where every element is a data frame with the daily
#' covariates for each site, that is a measurement for each day, such as average
#' temperature of a day, this covariates are usually very .
#' @param spp the number of species in the pres data.frame
#' @param form a formula in the format ~ obscov ~ sitcov, the first arguments
#' will be used to calculate probability of detection and the second part the
#' occupancy.
#' @param index Diversity index, one of "shannon", "simpson" or "invsimpson".
#' @param dredge default = FALSE, if TRUE, for each species, the best occupancy
#' model will be determined by fitting all possible models and ranking by AICc.
#' @return A list with the fitted models for each species, the calculated
#' Alpha diversity for each site, and a dataframe with the abundance of each
#' species and diversity.
#' @details
#' This function fits the latent abundance mixture model described in Royle and
#' Nichols (2003), to calculate the abundance of every species in each site, the
#' using that abundance it calculates the alpha diversity index for each site
#' based on that abundance.
#' @examples
#' \dontrun{
#' #Load the data
#' data("IslandBirds")
#' data("Daily_Cov")
#' data("siteCov")
#'
#' #Model the abundance for  5 bird species and calculate alpha diversity from that
#'
#' BirdDiversity <-diversityoccu(pres = IslandBirds, sitecov = siteCov,
#' obscov = Daily_Cov,spp =  5, form = ~ Day + Wind + Time + Rain +
#' Noise ~ Elev + AgroFo + SecVec + Wetland + Upland)
#'
#' #To see the estimates and p values for each model:
#'
#' BirdDiversity$models
#' }
#' @seealso \code{\link[vegan]{diversity}}
#' @seealso \code{\link[DiversityOccupancy]{model.diversity}}
#' @export
#' @importFrom vegan diversity
#' @importFrom unmarked occuRN
#' @importFrom unmarked unmarkedFrameOccu
#' @importMethodsFrom unmarked predict
#' @importFrom MuMIn dredge
#' @importFrom MuMIn AICc
#' @importFrom stats getCall


#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @author Nicole L. Michel
#' @author Mike Meredith

diversityoccu <- function(pres, sitecov, obscov, spp, form, index = "shannon", dredge = FALSE) {

  secuencia <- c(1:spp) * (ncol(pres)/spp)
  secuencia2 <- secuencia - (secuencia[1]-1)

  models <- vector('list', spp)
  div <- matrix(NA, nrow(pres), spp)
  colnames(div) <- paste("species", 1:spp, sep =".")

  if (dredge == FALSE) {
    for(i in 1:length(secuencia)) {
      data <- pres[, secuencia2[i]:secuencia[i]]
      data2 <- unmarkedFrameOccu(y = data, siteCovs = sitecov, obsCovs = obscov)
      models[[i]] <- occuRN(form, data2)
      div[, i] <- suppressWarnings(predict(models[[i]], type = "state", newdata = sitecov))$Predicted
      print(paste("Species", as.character(i), "ready!"))
    }
  } else {
    for(i in 1:length(secuencia)) {
      data <- pres[, secuencia2[i]:secuencia[i]]
      data2 <- unmarkedFrameOccu(y = data, siteCovs = sitecov, obsCovs = obscov)
      dredged <- suppressWarnings(dredge(occuRN(form, data2)))
      # select the first model and evaluate
      models[[i]] <- eval(getCall(dredged, 1))
      #predictions for the best model
      div[, i] <- predict(models[[i]], type = "state", newdata = sitecov)$Predicted
      print(paste("Species", as.character(i), "ready!"))
    }
  }
  h <- diversity(div, index)
  DF <- as.data.frame(cbind(h, div))

  result <- list(Covs = sitecov, models = models, Diversity = h, species = DF)
  class(result) <- "diversityoccupancy"

  return(result)
}


#' Find the best GLM model explaining the alpha divesity of the species
#'
#' This function takes a diversityoccu object and heuristically searches for the
#' glm that best explains the alpha diversity of the modelled species.
#'
#' @param DivOcc is an object returned by the divesityoccu function of this
#' package
#' @param method The method to be used to explore the candidate set of models.
#' If "h" an exhaustive screening is undertaken. If "g" the genetic algorithm is
#' employed (recommended for large candidate sets). If "l", a very fast
#' exhaustive branch-and-bound algorithm is used. Package leaps must then be
#' loaded, and this can only be applied to linear models with covariates and no
#' interactions.
#' @param delta	The number of models that will be returned will be the ones that
#' have a maximum AICc difference with the top model equal to delta.
#' @param squared, if FALSE (Default), only GLMs with linear components will be
#' evaluated; If TRUE, GLMs with both linear and quadratic components will be evaluated.
#' WARNING if squared is TRUE, the number of parameters duplicates and the models
#' grow exponentially, this may result in to many variables for a CPU to compute.
#' @return An object with the best fitted model, the coefficients of that model,
#' a table with the top 5 fitted models ranked by AICc and the data used for the
#' model
#' @details
#' This function fits every first order glm possible and ranks them by AICc.
#' @examples
#' \dontrun{
#' #Load the data
#' data("IslandBirds")
#' data("Daily_Cov")
#' data("siteCov")
#'
#' #Model the abundance for  5 bat species and calculate alpha diversity from that
#'
#' BirdDiversity <-diversityoccu(pres = IslandBirds, sitecov = siteCov,
#' obscov = Daily_Cov,spp =  5, form = ~ Day + Wind + Time + Rain +
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
#'
#' #To add the quadratic components of models
#'
#' glm.birdiversity <- model.diversity(BirdDiversity , method = "g", squared = TRUE)
#'
#' responseplot.diver(glm.birdiversity, Elev)
#' }
#' @seealso \code{\link[DiversityOccupancy]{diversityoccu}}
#' @export
#' @importFrom stats glm
#' @importFrom stats as.formula
#' @importFrom glmulti glmulti
#' @importFrom glmulti weightable
#' @importFrom dplyr filter
#' @importFrom qpcR akaike.weights
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>


model.diversity <- function(DivOcc, method = "h", delta = 2, squared = FALSE){
  Delta.AICc <- NULL
  A <- cbind(DivOcc$Diversity, DivOcc$Covs)
  colnames(A)[1]<-"Diversity"
  B <- paste(names(DivOcc$Covs), "+")
  B <- toString(B)
  B <- gsub(",", " ", B)
  if (squared == TRUE) {
    C <- paste("I(", names(DivOcc$Covs), "^2) +")
    C <- toString(C)
    C <- gsub (",", " ", C)
    B <- paste("Diversity ~", B, C, collapse = " ")
  }
  else if (squared == FALSE) {
    B <- paste("Diversity ~", B, collapse = " ")
  }

  B <- as.formula(substr(B, 1, nchar(B)-1))
  B <- glm(B, data = A)
  D <- glmulti(B, level = 1, crit = "aicc", plotty = FALSE, method = method)
  Best.model <- D@formulas[[1]]
  Table <- weightable(D)
  Table$Delta.AICc <- Table[,2]-Table[1,2]
  Table <- filter(Table, Delta.AICc < delta)
  Table$weights <- akaike.weights(Table$aicc)$weights
  d<-summary(glm(Best.model, data = A))
  result <- list(Best_model = Best.model, Table = Table, coeff = d, dataset= A)
  class(result) <- "modeldiversity"
  return(result)
}

#' Makes a spacially explicit prediction of the occupancy of multiple species
#' and alpha diversity, and select the area where
#'
#' This function takes an deiversityoccu object and predicts occupancy for all species
#' in new data, either a data.frame or a rasterstack. It can also return a subset
#' of the total area of a rasterstack, where diversity and occupancy/abundance are
#' higher than the nth quantile.
#' @param model A result from diversityoccu
#' @param diverse A result from the model.diversity function.
#' @param new.data a rasterstack, or a dataframe containing the same variables as
#' the siteCovs variable in diversityoccu or batchoccu
#' @param quantile.nth the nth quantile, over which is a goal to keep both diversity
#' and selected species. default = NULL
#' @param species a boolean vector of the species to take into acount
#' @param kml if TRUE builds a kml file of the selected area and saves it in your
#' working directry
#' @param name the name of the kml file if kml is TRUE
#' @return a data frame with predicted values, or a raster stack with predictions
#' for each species, a raster for diversity and a raster with the area meeting the
#' quantile criteria.
#' @examples
#' \dontrun{
#' #Load the data
#' data("IslandBirds")
#' data("Daily_Cov")
#' data("siteCov")
#' data("Birdstack")
#'
#' #Model the abundance for  5 bat species and calculate alpha diversity from that
#'
#' #Model the abundance for  5 bat species and calculate alpha diversity from that
#'
#' BirdDiversity <-diversityoccu(pres = IslandBirds, sitecov = siteCov,
#' obscov = Daily_Cov,spp =  5, form = ~ Day + Wind + Time ~ Elev + Wetland + Upland)
#'
#' #Select the best model that explains diversity using genetic algorithms
#' set.seed(123)
#' glm.Birdiversity <- model.diversity(BirdDiversity, method = "g")
#'
#' # get the area where the first two bird species are most abundant
#' # and the diversity is high
#'
#' library(rgdal)
#' Selected.area <- diversity.predict(model = BirdDiversity, diverse = glm.Birdiversity,
#' new.data = Birdstack, quantile.nth = 0.65, species =
#' c(TRUE, TRUE, FALSE, FALSE, FALSE))
#'
#' Selected.area
#' }
#' @export
#' @seealso \code{\link[DiversityOccupancy]{diversityoccu}}
#' @seealso \code{\link[DiversityOccupancy]{batchoccu}}
#' @seealso \code{\link[DiversityOccupancy]{model.diversity}}
#' @importFrom graphics plot
#' @importFrom stats glm
#' @importFrom raster addLayer
#' @importFrom raster KML
#' @importFrom raster quantile
#' @importFrom raster reclassify
#' @importFrom raster stack
#' @importFrom raster subset
#' @importFrom raster unstack
#' @importFrom raster writeRaster
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>

diversity.predict<- function(model, diverse, new.data, quantile.nth = 0.8 , species, kml = TRUE, name = "Priority_Area.kml") {
  models <- model$models[species]
  layers <- list()
  for (i in 1:length(models)){
    layers [[i]] <- predict(models[[i]], new.data, type = "state")$Predicted
  }
  glm.model <- glm(diverse$Best_model, data = diverse$dataset)
  diversity.raster<- predict(object = new.data, model = glm.model)
  layers <- stack(unlist(layers))
  desition <- addLayer(layers, diversity.raster)
  nths <- quantile(desition, quantile.nth)
  desition <- unstack(desition)
  rc<-list()
  for (i in 1:length(nths)){
    m <- c(-Inf, nths[i], NA,  nths[i], Inf, 1)
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    rc[[i]]<-reclassify(desition[[i]] , rcl = rclmat)
  }
  rc<- stack(unlist(rc))
  priority.area <- prod(rc)
  plot(priority.area, colNA="black", legend = FALSE)
  if (kml == TRUE) {
    KML(priority.area, file= name, overwrite = TRUE, col = "red")
  }
  result <- list(species = layers, diversity.raster = diversity.raster, priority.area = priority.area)
  return(result)
}

#' Predicts occupancy for all the species in a batchoccupancy class object
#'
#' This function takes an batchoccupancy object and predicts occupancy for all species
#' in new data, either a data.frame or a rasterstack.
#' @param batch A result from the batchoccu
#' @param new.data a rasterstack, or a dataframe containing the same variables as
#' the siteCovs variable in batchoccu
#' @return a raster stack with predictions
#' for each species.
#' @examples
#' \dontrun{
#' #Load the data
#' data("IslandBirds")
#' data("Daily_Cov")
#' data("siteCov")
#' data("Birdstack")
#' BirdOccupancy <-batchoccu(pres = IslandBirds, sitecov = siteCov, obscov =
#' Daily_Cov, spp =  5, form = ~ Day + Wind + Rime + Noise + Clouds ~
#' Elev + AgroFo + SecVec + Wetland)
#'
#' Occupancy.stack <- occupancy.predict(batch = BirdOccupancy, new.data =
#' Birdstack)
#' }
#' @export
#' @seealso \code{\link[DiversityOccupancy]{batchoccu}}
#' @importFrom raster plot
#' @importFrom raster addLayer
#' @importFrom raster stack
#' @importFrom raster subset
#' @importFrom graphics plot
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>


occupancy.predict<- function(batch, new.data) {
  models <- batch$models
  layers <- list()
  for (i in 1:length(models)){
    layers [[i]] <- predict(models[[i]], new.data, type = "state")
    layers [[i]] <- subset(layers[[i]], 1)
  }
  layers <- stack (unlist(layers))
  return(layers)
}
