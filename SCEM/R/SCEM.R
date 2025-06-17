#' @title Splitting-Coalescence-Estimation Method (SCEM) for archaeological time series.
#'
#' @description This function performs the clustering algorithm SCEM on the bivariate time series data –
#' where one series is the distance from the cementum-enamel junction, and the other series is the value of
#' the oxygen-18 isotope at that distance. It returns the class assignments and birth seasonality estimates
#' for all the individuals.
#'
#' The SCEM assumes that the oxygen isotope values \eqn{(z(t))} can be expressed as a function of \eqn{x(t)},
#' the natural logarithm of the distance from the CEJ for each of the incremental samples, scaled down by
#' the period (\eqn{X}, the length of the tooth crown). In other words,
#'
#' \loadmathjax
#' \mjdeqn{z(t) = f(x(t)/X) + e(t)}{ASCII representation}
#'
#' where the form of \eqn{f} is unknown and \eqn{e(t)} is an error process.
#' Also, following our definition, it assumes that the value of \eqn{x(t)/X} that maximizes \eqn{z(t)} is the estimated birth seasonality.
#'
#' Birth seasonality is estimated using the combined data from all individuals in a single cluster,
#' but birth seasonality estimates for individuals in a cluster are based on individual estimates of the length of the tooth crown (\mjeqn{X_{k}}{ASCII representation}).
#'
#' For a detailed description of the algorithm, please see Chazin et al. 2019, Supplemental Materials 1.
#'
#' @param paths A list of data frames, where each frame contains the data for one individual. There
#' should be two columns with names 'distance' and 'oxygen'.
#'
#' @param bandwidth Denotes the order of the bandwidth that should be used in the splitting-coalescence
#' (SC) clustering algorithm. A value k will mean that the bandwidth used in the algorithm is n^k.
#'
#' @export
#'
#' @returns
#'
#' A list containing the following components:
#'
#' \item{results}{A data frame that has the individual information (ID, species, number of observations in the time series), cluster assignment, estimated period, delay and the birth seasonality estimate for every individual.}
#' \item{groups}{The groups formed by the clustering algorithm}
#'
#' @references Chazin, Hannah, Soudeep Deb, Joshua Falk, and Arun Srinivasan. 2019. “New Statistical Approaches to Intra-Individual Isotopic Analysis and Modeling Birth Seasonality in Studies of Herd Animals.” Archaeometry 61 (2): 478–93.
#'
#' @examples
#' \dontrun{
#' armenia_split = split(armenia,f = armenia$ID)
#' results = SCEM(armenia_split,bandwidth = -0.33)
#' }


SCEM <- function(paths,
                 bandwidth){

  for(i in 1:length(paths)){
    if (!any(colnames(paths[[i]])==c("distance","oxygen"))) {stop('data frame does not contain columns named distance and oxygen')}
  }
  if (! is.atomic(bandwidth) || !length(bandwidth)==1) {stop('bandwidth needs to be a single value')}
  for(i in 1:length(paths)){if (any(is.na(paths[[i]]))) {stop('Data has NAs')}}

  cluster = SCalgo(paths,bandwidth = bandwidth)
  groups = cluster
  cosine = makeFits(paths,method="OLS")
  period = cosine$X
  gnum = length(groups)
  birthseason = numeric(gnum)
  for (k in 1:gnum){
    S = groups[[k]]
    for (jj in 1:length(S)){

      paths[[S[jj]]]$zval = paths[[S[jj]]]$distance/period[S[jj]]
    }
    fulldata = do.call(rbind,paths[S])
    fulldata = fulldata[order(fulldata$zval),]
    ddd = stats::aggregate(oxygen ~ zval,fulldata,FUN = mean)
    xx = (ddd$zval)
    yy = ddd$oxygen
    zz = xx[sort.int(yy,decreasing = T,index.return = T)$ix[1:5]]
    if (max(dist(zz))>0.5){
      zz[zz<0.5] = 1+zz[zz<0.5]
    }
    birthseason[k] = mean(zz)%%1
  }

  index <- numeric(length(paths))
  season <- numeric(length(paths))
  species <- unique(do.call(rbind,paths)[,"ID"])
  results <- data.frame(ID = species)
  results$Length = as.numeric(lapply(paths,nrow))
  dd = matrix(nrow = length(paths),ncol = 2)
  for (i in 1:length(groups)){
    dd[groups[[i]],1] = as.numeric(i)
    dd[groups[[i]],2] = as.numeric(round(birthseason[i],3))
  }

  results$Cluster = dd[,1]
  results$x0 = dd[,2]*period
  results$X = period
  results$Season = dd[,2]

  return(list(results = results,groups = groups))
}
