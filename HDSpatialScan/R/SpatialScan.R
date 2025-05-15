################################################################
##' @title Spatial scan procedure
##'
##' @description This function computes the different scan procedures available in the package.
##'
##' @param method character vector. The scan procedures to apply on the data. Possible values are:
##' \itemize{
##' \item Univariate scan procedures: "UG" (univariate gaussian, see \code{\link{UG}}), "UNP" (univariate nonparametric, see \code{\link{UNP}})
##' \item Multivariate scan procedures: "MG" (multivariate gaussian, see \code{\link{MG}}), "MNP" (multivariate nonparametric, see \code{\link{MNP}})
##' \item Univariate functional scan procedures: "NPFSS" (nonparametric functional scan statistic, see \code{\link{NPFSS}}), "PFSS" (parametric functional scan statistic, see \code{\link{PFSS}}), "DFFSS" (distribution-free functional scan statistic, see \code{\link{DFFSS}}), "URBFSS" (univariate rank-based functional scan statistic, see \code{\link{URBFSS}})
##' \item Multivariate functional scan procedures: "NPFSS" (nonparametric functional scan statistic, see \code{\link{NPFSS}}), "MDFFSS" (multivariate distribution-free functional scan statistic, see \code{\link{MDFFSS}}), "MRBFSS" (multivariate rank-based functional scan statistic, see \code{\link{MRBFSS}}), "MPFSS", "MPFSS-LH", "MPFSS-W", "MPFSS-P" and "MPFSS-R" (parametric multivariate functional scan statistic ; "LH", "W", "P", "R" correspond respectively to the Lawley-Hotelling trace test statistic, The Wilks lambda test statistic, the Pillai trace test statistic and the Roy's maximum root test statistic, see \code{\link{MPFSS}}). Note that "MPFSS" computes "MPFSS-LH", "MPFSS-W", "MPFSS-P" and "MPFSS-R".
##' }
##' @param data list of numeric matrices or a matrix or a vector:
##' \itemize{
##' \item Univariate case: Vector of the data, each element corresponds to a site (or an individual if the observations are by individuals and not by sites).
##' \item Multivariate case: Matrix of the data, the rows correspond to the sites (or the individuals if the observations are by individuals and not by sites) and each column represents a variable.
##' \item Univariate functional case: Matrix of the data, the rows correspond to the sites (or to the individuals if the observations are by individuals and not by sites) and each column represents an observation time. The times must be the same for each site/individual. Depending on the scan procedure they also need to be equally-spaced.
##' \item Multivariate functional case: List of nb_sites (or nb_individuals if the observations are by individuals and not by sites) matrices of the data, the rows correspond to the variables and each column represents an observation time. The times must be the same for each site/individual. Depending on the scan procedure they also need to be equally-spaced.
##' }
##' @param sites_coord numeric matrix. Coordinates of the sites (or the individuals, in that case there can be many individuals with the same coordinates).
##' @param system character. System in which the coordinates are expressed: "Euclidean" or "WGS84".
##' @param mini numeric. A minimum for the clusters (see type_minimaxi). Changing the default value may bias the inference.
##' @param maxi numeric. A Maximum for the clusters (see type_minimaxi). Changing the default value may bias the inference.
##' @param type_minimaxi character. Type of minimum and maximum: by default "sites/indiv": the mini and maxi are on the number of sites or individuals in the potential clusters. Other possible values are "area": the minimum and maximum area of the clusters, or "radius": the minimum and maximum radius.
##' @param mini_post numeric. A minimum to filter the significant clusters a posteriori (see type_minimaxi_post). The default NULL is for no filtering with a a posteriori minimum.
##' @param maxi_post numeric. A maximum to filter the significant clusters a posteriori (see type_minimaxi_post). The default NULL is for no filtering with a a posteriori maximum.
##' @param type_minimaxi_post character. Type of minimum and maximum a posteriori: by default "sites/indiv": the mini_post and maxi_post are on the number of sites or individuals in the significant clusters. Other possible values are "area": the minimum and maximum area of the clusters, or "radius": the minimum and maximum radius.
##' @param sites_areas numeric vector. Areas of the sites. It must contain the same number of elements than the rows of sites_coord. If the data is on individuals and not on sites, there can be duplicated values. By default: NULL
##' @param MC numeric. Number of Monte-Carlo permutations to evaluate the statistical significance of the clusters. By default: 999.
##' @param typeI numeric. The desired type I error. A cluster will be evaluated as significant if its associated p-value is less than typeI. By default 0.05.
##' @param nbCPU numeric. Number of CPU. If nbCPU > 1 parallelization is done. By default: 1. Ignored for "UG" and "UNP"
##' @param variable_names character. Names of the variables. By default NULL. Ignored for the univariate and univariate functional scan procedures.
##' @param times numeric. Times of observation of the data. By default NULL. Ignored for the univariate and multivariate scan procedures.
##'
##' @examples
##' # Univariate scan statistics
##' \donttest{
##' library(sp)
##' data("map_sites")
##' data("multi_data")
##' uni_data <- multi_data[,1]
##' coords <- coordinates(map_sites)
##' res <- SpatialScan(method = c("UG", "UNP"), data = uni_data, sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2)}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("multi_data")
##' uni_data <- multi_data[,1]
##' coords <- coordinates(map_sites)
##' res <- SpatialScan(method = c("UG", "UNP"), data = uni_data, sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2, MC = 9)}
##' # Multivariate scan statistics
##' \donttest{
##' library(sp)
##' data("map_sites")
##' data("multi_data")
##' coords <- coordinates(map_sites)
##' res <- SpatialScan(method = c("MG", "MNP"), data = multi_data, sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2)}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("multi_data")
##' indices <- c(51:75)
##' coords <- coordinates(map_sites[indices,])
##' res <- SpatialScan(method = c("MG", "MNP"), data = multi_data[indices,], sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2, MC = 9)
##' }
##' # Univariate functional scan statistics
##' \donttest{
##' library(sp)
##' data("map_sites")
##' data("funi_data")
##' coords <- coordinates(map_sites)
##' res <- SpatialScan(method = c("NPFSS", "PFSS", "DFFSS", "URBFSS"), data = funi_data,
##' sites_coord = coords, system = "WGS84", mini = 1, maxi = nrow(coords)/2)}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("funi_data")
##' indices <- c(51:75)
##' coords <- coordinates(map_sites[indices,])
##' res <- SpatialScan(method = c("NPFSS", "PFSS", "DFFSS", "URBFSS"), data = funi_data[indices,],
##' sites_coord = coords, system = "WGS84", mini = 1, maxi = 1, MC = 9)
##' }
##' # Multivariate functional
##' \donttest{
##' library(sp)
##' data("map_sites")
##' data("fmulti_data")
##' coords <- coordinates(map_sites)
##' res <- SpatialScan(method = c("NPFSS", "MPFSS", "MDFFSS", "MRBFSS"), data = fmulti_data,
##' sites_coord = coords, system = "WGS84", mini = 1, maxi = nrow(coords)/2)}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("fmulti_data")
##' indices <- c(51:75)
##' coords <- coordinates(map_sites[indices,])
##' res <- SpatialScan(method = c("NPFSS", "MPFSS", "MDFFSS", "MRBFSS"), data = fmulti_data[indices],
##' sites_coord = coords, system = "WGS84", mini = 1, maxi = 1, MC = 9)
##' }
##'
##'
##' @return A list of objects of class ResScanOutput:
##' \itemize{
##' \item Univariate case (UG, UNP): A list of objects of class ResScanOutputUni
##' \item Multivariate case (MG, MNP): A list of objects of class ResScanOutputMulti
##' \item Univariate functional case (NPFSS, PFSS, DFFSS, URBFSS): A list of objects of class ResScanOutputUniFunct
##' \item Multivariate functional case (NPFSS, MPFSS, MDFFSS, MRBFSS): A list of objects of class ResScanOutputMultiFunct
##' }
##' @export
##'
##' @seealso \code{\link{ResScanOutput}}, \code{\link{ResScanOutputUni}}, \code{\link{ResScanOutputMulti}}, \code{\link{ResScanOutputUniFunct}} and \code{\link{ResScanOutputMultiFunct}}
##'
##' @references For univariate scan statistics:
##' \itemize{
##' \item Inkyung Jung and Ho Jin Cho (2015). A Nonparametric Spatial Scan Statistic for Continuous Data. International Journal of Health Geographics, 14.
##' \item Martin Kulldorff and Lan Huang and Kevin Konty (2009). A Scan Statistic for Continuous Data Based on the Normal Probability Model. International Journal of Health Geographics, 8 (58).
##' }
##' For multivariate scan statistics:
##' \itemize{
##' \item Lionel Cucala and Michaël Genin and Florent Occelli and Julien Soula (2019). A Multivariate Nonparametric Scan Statistic for Spatial Data. Spatial statistics, 29, 1-14.
##' \item Lionel Cucala and Michaël Genin and Caroline Lanier and Florent Occelli (2017). A Multivariate Gaussian Scan Statistic for Spatial Data. Spatial Statistics, 21, 66-74.
##' }
##' For functional scan statistics:
##' \itemize{
##' \item Zaineb Smida and Lionel Cucala and Ali Gannoun. A Nonparametric Spatial Scan Statistic for Functional Data. Pre-print <https://hal.archives-ouvertes.fr/hal-02908496>.
##' \item Camille Frévent and Mohamed-Salem Ahmed and Matthieu Marbac and Michaël Genin. Detecting Spatial Clusters in Functional Data: New Scan Statistic Approaches. Pre-print <arXiv:2011.03482>.
##' \item Camille Frévent and Mohamed-Salem Ahmed and Sophie Dabo-Niang and Michaël Genin. Investigating Spatial Scan Statistics for Multivariate Functional Data. Pre-print <arXiv:2103.14401>.
##' }
##'
SpatialScan <- function(method, data, sites_coord = NULL, system = NULL, mini = 1, maxi = nrow(sites_coord)/2, type_minimaxi = "sites/indiv", mini_post = NULL, maxi_post = NULL, type_minimaxi_post = "sites/indiv", sites_areas = NULL, MC = 999, typeI = 0.05, nbCPU = 1, variable_names = NULL, times = NULL){

  if(is.null(sites_areas) == FALSE){
    if(is.numeric(sites_areas) == FALSE & is.integer(sites_areas)== FALSE){
      stop("sites_areas must be NULL or numeric")
    }
  }

  if(is.numeric(MC)==FALSE){
    stop("MC must be integer")
  }
  if(round(MC)!=MC){
    stop("MC must be integer")
  }

  if(is.numeric(typeI)==FALSE){
    stop("typeI must be numeric")
  }


  # method wanted : MPFSS compute MPFSS-LH, MPFSS-W, MPFSS-P and MPFSS-R
  if(sum(!(method %in% c("UG", "UNP", "MG", "MNP", "NPFSS", "PFSS", "DFFSS", "URBFSS", "MPFSS", "MPFSS-LH","MPFSS-W","MPFSS-P","MPFSS-R", "MDFFSS", "MRBFSS")))>0){
    stop("The methods must be among UG, UNP, MG, MNP, NPFSS, PFSS, DFFSS, URBFSS, MPFSS, MPFSS-LH,MPFSS-W,MPFSS-P,MPFSS-R, MDFFSS, MRBFSS")
  }

  # type of the data
  if("UG" %in% method | "UNP" %in% method){
    if(is.vector(data)==FALSE){
      stop("The data must be a vector for univariate scan procedures (UG and UNP)")
    }
  }

  if("MG" %in% method | "MNP" %in% method){
    if(is(data, "matrix") == FALSE){
      stop("The data must be a matrix for multivariate scan procedures (MG and MNP)")
    }

    if(is.numeric(nbCPU)==FALSE){
      stop("nbCPU must be integer")
    }
    if(round(nbCPU)!=nbCPU){
      stop("nbCPU must be integer")
    }
  }

  if("PFSS" %in% method | "DFFSS" %in% method | "URBFSS" %in% method){
    if(is(data, "matrix") == FALSE){
      stop("The data must be a matrix for PFSS, DFFSS and URBFSS")
    }

    if(is.numeric(nbCPU)==FALSE){
      stop("nbCPU must be integer")
    }
    if(round(nbCPU)!=nbCPU){
      stop("nbCPU must be integer")
    }
  }

  if("MPFSS" %in% method | "MDFFSS" %in% method | "MRBFSS" %in% method | "MPFSS-LH" %in% method | "MPFSS-W" %in% method | "MPFSS-P" %in% method | "MPFSS-R" %in% method){
    if(is(data, "list") == FALSE){
      stop("The data must be a list of matrices for MPFSS, MDFFSS and MRBFSS")
    }

    if(is.numeric(nbCPU)==FALSE){
      stop("nbCPU must be integer")
    }
    if(round(nbCPU)!=nbCPU){
      stop("nbCPU must be integer")
    }
  }

  if("NPFSS" %in% method){
    if(is(data, "list") == FALSE & is(data, "matrix") == FALSE){
      stop("The data must be a list of matrices or a matrix for NPFSS")
    }

    if(is.numeric(nbCPU)==FALSE){
      stop("nbCPU must be integer")
    }
    if(round(nbCPU)!=nbCPU){
      stop("nbCPU must be integer")
    }
  }

  # passed the data type check


  initialization <- InitScan(mini_post, maxi_post, type_minimaxi_post, sites_areas, sites_coord, system, mini, maxi, type_minimaxi)

  nb_sites <- nrow(initialization$matrix_clusters)

  # same permutations for all the scan procedures
  permutations <- permutate(1:nb_sites, MC)

  output <- list()

  if("UG" %in% method){
    cat("##### Univariate Gaussian scan procedure ##### \n")
    output[["UG"]] <- UG(data = data, MC = MC, typeI = typeI, initialization = initialization, permutations = permutations)
    cat("====================================================== \n")
  }

  if("UNP" %in% method){
    cat("##### Univariate nonparametric scan statistic ##### \n")
    output[["UNP"]] <- UNP(data = data, MC = MC, typeI = typeI, initialization = initialization, permutations = permutations)
    cat("====================================================== \n")
  }

  if("MG" %in% method){
    cat("##### Multivariate Gaussian scan statistic ##### \n")
    output[["MG"]] <- MG(data = data, MC = MC, typeI = typeI, nbCPU = nbCPU, variable_names = variable_names, initialization = initialization, permutations = permutations)
    cat("====================================================== \n")
  }

  if("MNP" %in% method){
    cat("##### Multivariate nonparametric scan statistic ##### \n")
    output[["MNP"]] <- MNP(data = data, MC = MC, typeI = typeI, nbCPU = nbCPU, variable_names = variable_names, initialization = initialization, permutations = permutations)
    cat("====================================================== \n")
  }

  if("NPFSS" %in% method){
    cat("##### Nonparametric functional scan statistic ##### \n")
    output[["NPFSS"]] <- NPFSS(data = data, MC = MC, typeI = typeI, nbCPU = nbCPU, variable_names = variable_names, times = times, initialization = initialization, permutations = permutations)
    cat("====================================================== \n")
  }

  if("PFSS" %in% method){
    cat("##### Parametric functional scan statistic ##### \n")
    output[["PFSS"]] <- PFSS(data = data, MC = MC, typeI = typeI, nbCPU = nbCPU, times = times, initialization = initialization, permutations = permutations)
    cat("====================================================== \n")
  }

  if("DFFSS" %in% method){
    cat("##### Distribution-free functional scan statistic ##### \n")
    output[["DFFSS"]] <- DFFSS(data = data, MC = MC, typeI = typeI, nbCPU = nbCPU, times = times, initialization = initialization, permutations = permutations)
    cat("====================================================== \n")
  }

  if("URBFSS" %in% method){
    cat("##### Univariate rank-based functional scan statistic ##### \n")
    output[["URBFSS"]] <- URBFSS(data = data, MC = MC, typeI = typeI, nbCPU = nbCPU, times = times, initialization = initialization, permutations = permutations)
    cat("====================================================== \n")
  }

  if("MDFFSS" %in% method){
    cat("##### Multivariate distribution-free functional scan statistic ##### \n")
    output[["MDFFSS"]] <- MDFFSS(data = data, MC = MC, typeI = typeI, nbCPU = nbCPU, variable_names = variable_names, times = times, initialization = initialization, permutations = permutations)
    cat("====================================================== \n")
  }

  if("MRBFSS" %in% method){
    cat("##### Multivariate rank-based functional scan statistic ##### \n")
    output[["MRBFSS"]] <- MRBFSS(data = data, MC = MC, typeI = typeI, nbCPU = nbCPU, variable_names = variable_names, times = times, initialization = initialization, permutations = permutations)
    cat("====================================================== \n")
  }

  if("MPFSS" %in% method){
    cat("##### Parametric multivariate functional scan statistic ##### \n")
    temp <- MPFSS(data = data, MC = MC, typeI = typeI, method = c("LH","W","P","R"), nbCPU = nbCPU, variable_names = variable_names, times = times, initialization = initialization, permutations = permutations)
    output[["MPFSS-LH"]] <- temp$LH
    output[["MPFSS-W"]] <- temp$W
    output[["MPFSS-P"]] <- temp$P
    output[["MPFSS-R"]] <- temp$R
    cat("====================================================== \n")
  }else{
    methods_mpfss <- c()
    if("MPFSS-LH" %in% method){
      methods_mpfss <- c(methods_mpfss, "LH")
    }
    if("MPFSS-W" %in% method){
      methods_mpfss <- c(methods_mpfss, "W")
    }
    if("MPFSS-P" %in% method){
      methods_mpfss <- c(methods_mpfss, "P")
    }
    if("MPFSS-R" %in% method){
      methods_mpfss <- c(methods_mpfss, "R")
    }

    if(length(methods_mpfss)>0){
      cat("##### Parametric multivariate functional scan statistic ##### \n")
      temp <- MPFSS(data = data, MC = MC, typeI = typeI, method = methods_mpfss, nbCPU = nbCPU, variable_names = variable_names, times = times, initialization = initialization, permutations = permutations)
      if("MPFSS-LH" %in% method){
        output[["MPFSS-LH"]] <- temp$LH
      }
      if("MPFSS-W" %in% method){
        output[["MPFSS-W"]] <- temp$W
      }
      if("MPFSS-P" %in% method){
        output[["MPFSS-P"]] <- temp$P
      }
      if("MPFSS-R" %in% method){
        output[["MPFSS-R"]] <- temp$R
      }
      cat("====================================================== \n")
    }
  }

  return(output)
}
