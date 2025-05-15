################################################################
##' @title Constructor function for objects of the ResScanOutput class
##'
##' @description This is the constructor function for objects of the ResScanOutput class.
##'
##' @param sites_clusters list. List of the indices of the sites of the selected clusters.
##' @param pval_clusters numeric vector. The pvalues of the selected clusters.
##' @param centres_clusters numeric matrix. Coordinates of the centres of the selected clusters.
##' @param radius_clusters numeric vector. Radius of the selected clusters.
##' @param areas_clusters numeric vector. Areas of the selected clusters.
##' @param system character. System in which the coordinates are expressed: "Euclidean" or "WGS84".
##' @param sites_coord numeric matrix. Coordinates of the sites (or the individuals, in that case there can be many individuals with the same coordinates).
##' @param data list of numeric matrices or a matrix or a vector. List of nb_sites (or nb_individuals if the observations are by individuals and not by site) matrices of the data, the rows correspond to the variables and each column represents an observation time (multivariate functional case) ; or Matrix of the data, the rows correspond to the sites (or to the individuals) and each column represents an observation time (univariate functional case) or a variable (multivariate case) ; or Vector of the data, the elements correspond to the sites (or to the individuals) (univariate case).
##' @param method character. The scan procedure used.
##'
##' @return An object of class ResScanOutput which is a list of the following elements:
##' \itemize{
##' \item sites_clusters: List of the indices of the sites of the selected clusters.
##' \item pval_clusters: The pvalues of the selected clusters.
##' \item centres_clusters: Coordinates of the centres of the selected clusters.
##' \item radius_clusters: Radius of the selected clusters.
##' \item areas_clusters: Areas of the selected clusters.
##' \item system: System in which the coordinates are expressed: "Euclidean" or "WGS84".
##' \item sites_coord: Coordinates of the sites (or the individuals, in that case there can be many individuals with the same coordinates).
##' \item data: List of numeric matrices or a matrix or a vector.
##' \item method: The scan procedure used.
##' }
##'
##'
##'
ResScanOutput <- function(sites_clusters, pval_clusters, centres_clusters, radius_clusters, areas_clusters, system, sites_coord, data, method){

  res <- list(sites_clusters = sites_clusters, pval_clusters = pval_clusters,
              centres_clusters = centres_clusters, radius_clusters = radius_clusters,
              areas_clusters = areas_clusters, system = system, sites_coord = sites_coord, data = data, method = method)

  oldClass(res) <- "ResScanOutput"

  return(res)

}

################################################################
##' @title Constructor function for objects of the ResScanOutputUni class
##'
##' @description This is the constructor function for objects of the ResScanOutputUni class which inherits from class ResScanOutput.
##'
##' @param sites_clusters list. List of the indices of the sites of the selected clusters.
##' @param pval_clusters numeric vector. The pvalues of the selected clusters.
##' @param centres_clusters numeric matrix. Coordinates of the centres of the selected clusters.
##' @param radius_clusters numeric vector. Radius of the selected clusters.
##' @param areas_clusters numeric vector. Areas of the selected clusters.
##' @param system character. System in which the coordinates are expressed: "Euclidean" or "WGS84".
##' @param sites_coord numeric matrix. Coordinates of the sites (or the individuals, in that case there can be many individuals with the same coordinates).
##' @param data vector. Vector of the data, the elements correspond to the sites (or to the individuals).
##' @param method character. The scan procedure used.
##'
##' @return An object of class ResScanOutputUni which is a list of the following elements:
##' \itemize{
##' \item sites_clusters: List of the indices of the sites of the selected clusters.
##' \item pval_clusters: The pvalues of the selected clusters.
##' \item centres_clusters: Coordinates of the centres of the selected clusters.
##' \item radius_clusters: Radius of the selected clusters.
##' \item areas_clusters: Areas of the selected clusters.
##' \item system: System in which the coordinates are expressed: "Euclidean" or "WGS84".
##' \item sites_coord: Coordinates of the sites (or the individuals, in that case there can be many individuals with the same coordinates).
##' \item data: Vector.
##' \item method: The scan procedure used.
##' }
##'
##'
ResScanOutputUni <- function(sites_clusters, pval_clusters, centres_clusters, radius_clusters, areas_clusters, system, sites_coord, data, method){

  res <- ResScanOutput(sites_clusters, pval_clusters, centres_clusters, radius_clusters, areas_clusters, system, sites_coord, data, method)
  oldClass(res) <- append(oldClass(res), "ResScanOutputUni")

  return(res)

}

################################################################
##' @title Constructor function for objects of the ResScanOutputMulti class
##'
##' @description This is the constructor function for objects of the ResScanOutputMulti class which inherits from class ResScanOutput.
##'
##' @param sites_clusters list. List of the indices of the sites of the selected clusters.
##' @param pval_clusters numeric vector. The pvalues of the selected clusters.
##' @param centres_clusters numeric matrix. Coordinates of the centres of the selected clusters.
##' @param radius_clusters numeric vector. Radius of the selected clusters.
##' @param areas_clusters numeric vector. Areas of the selected clusters.
##' @param system character. System in which the coordinates are expressed: "Euclidean" or "WGS84".
##' @param variable_names character. Names of the variables. By default NULL.
##' @param sites_coord numeric matrix. Coordinates of the sites (or the individuals, in that case there can be many individuals with the same coordinates).
##' @param data matrix. Matrix of the data, the rows correspond to the sites (or to the individuals) and each column represents a variable.
##' @param method character. The scan procedure used.
##'
##' @return An object of class ResScanOutputMulti which is a list of the following elements:
##' \itemize{
##' \item sites_clusters: List of the indices of the sites of the selected clusters.
##' \item pval_clusters: The pvalues of the selected clusters.
##' \item centres_clusters: Coordinates of the centres of the selected clusters.
##' \item radius_clusters: Radius of the selected clusters.
##' \item areas_clusters: Areas of the selected clusters.
##' \item system: System in which the coordinates are expressed: "Euclidean" or "WGS84".
##' \item sites_coord: Coordinates of the sites (or the individuals, in that case there can be many individuals with the same coordinates).
##' \item data: Matrix.
##' \item variable_names: names of the variables.
##' \item method: The scan procedure used.
##' }
##'
##'
##'
ResScanOutputMulti <- function(sites_clusters, pval_clusters, centres_clusters, radius_clusters, areas_clusters, system, variable_names = NULL, sites_coord, data, method){

  res <- ResScanOutput(sites_clusters, pval_clusters, centres_clusters, radius_clusters, areas_clusters, system, sites_coord, data, method)

  if(is.null(variable_names)){
    res["variable_names"] <- list(NULL)
  }else{
    res[["variable_names"]] <- variable_names
  }

  oldClass(res) <- append(oldClass(res), "ResScanOutputMulti")

  return(res)

}

################################################################
##' @title Constructor function for objects of the ResScanOutputUniFunct class
##'
##' @description This is the constructor function for objects of the ResScanOutputUniFunct class which inherits from class ResScanOutput.
##'
##' @param sites_clusters list. List of the indices of the sites of the selected clusters.
##' @param pval_clusters numeric vector. The pvalues of the selected clusters.
##' @param centres_clusters numeric matrix. Coordinates of the centres of the selected clusters.
##' @param radius_clusters numeric vector. Radius of the selected clusters.
##' @param areas_clusters numeric vector. Areas of the selected clusters.
##' @param system character. System in which the coordinates are expressed: "Euclidean" or "WGS84".
##' @param times numeric. Times of observation of the data. By default NULL.
##' @param sites_coord numeric matrix. Coordinates of the sites (or the individuals, in that case there can be many individuals with the same coordinates).
##' @param data matrix. Matrix of the data, the rows correspond to the sites (or to the individuals) and each column represents an observation time.
##' @param method character. The scan procedure used.
##'
##' @return An object of class ResScanOutputUniFunct which is a list of the following elements:
##' \itemize{
##' \item sites_clusters: List of the indices of the sites of the selected clusters.
##' \item pval_clusters: The pvalues of the selected clusters.
##' \item centres_clusters: Coordinates of the centres of the selected clusters.
##' \item radius_clusters: Radius of the selected clusters.
##' \item areas_clusters: Areas of the selected clusters.
##' \item system: System in which the coordinates are expressed: "Euclidean" or "WGS84".
##' \item sites_coord: Coordinates of the sites (or the individuals, in that case there can be many individuals with the same coordinates).
##' \item data: Matrix.
##' \item times: times of observation of the data.
##' \item method : the scan procedure used
##' }
##'
##'
##'
ResScanOutputUniFunct <- function(sites_clusters, pval_clusters, centres_clusters, radius_clusters, areas_clusters, system, times = NULL, sites_coord, data, method){

  res <- ResScanOutput(sites_clusters, pval_clusters, centres_clusters, radius_clusters, areas_clusters, system, sites_coord, data, method)

  if(is.null(times)){
    res["times"] <- list(NULL)
  }else{
    res[["times"]] <- times
  }

  oldClass(res) <- append(oldClass(res), "ResScanOutputUniFunct")

  return(res)

}

################################################################
##' @title Constructor function for objects of the ResScanOutputMultiFunct class
##'
##' @description This is the constructor function for objects of the ResScanOutputMultiFunct class which inherits from class ResScanOutput.
##'
##' @param sites_clusters list. List of the indices of the sites of the selected clusters.
##' @param pval_clusters numeric vector. The pvalues of the selected clusters.
##' @param centres_clusters numeric matrix. Coordinates of the centres of the selected clusters.
##' @param radius_clusters numeric vector. Radius of the selected clusters.
##' @param areas_clusters numeric vector. Areas of the selected clusters.
##' @param system character. System in which the coordinates are expressed: "Euclidean" or "WGS84".
##' @param times numeric. Times of observation of the data. By default NULL.
##' @param variable_names character. Names of the variables. By default NULL.
##' @param sites_coord numeric matrix. Coordinates of the sites (or the individuals, in that case there can be many individuals with the same coordinates).
##' @param data list of numeric matrices. List of nb_sites (or nb_individuals if the observations are by individuals and not by site) matrices of the data, the rows correspond to the variables and each column represents an observation time.
##' @param method character. The scan procedure used.
##'
##' @return An object of class ResScanOutputMultiFunct which is a list of the following elements:
##' \itemize{
##' \item sites_clusters: List of the indices of the sites of the selected clusters.
##' \item pval_clusters: The pvalues of the selected clusters.
##' \item centres_clusters: Coordinates of the centres of the selected clusters.
##' \item radius_clusters: Radius of the selected clusters.
##' \item areas_clusters: Areas of the selected clusters.
##' \item system: System in which the coordinates are expressed: "Euclidean" or "WGS84".
##' \item sites_coord: Coordinates of the sites (or the individuals, in that case there can be many individuals with the same coordinates).
##' \item data: list of numeric matrices.
##' \item times: times of observation of the data.
##' \item variable_names: names of the variables.
##' \item method: the scan procedure used.
##' }
##'
##'
##'
ResScanOutputMultiFunct <- function(sites_clusters, pval_clusters, centres_clusters, radius_clusters, areas_clusters, system, times = NULL, variable_names = NULL, sites_coord, data, method){

  res <- ResScanOutput(sites_clusters, pval_clusters, centres_clusters, radius_clusters, areas_clusters, system, sites_coord, data, method)

  if(is.null(times)){
    res["times"] <- list(NULL)
  }else{
    res[["times"]] <- times
  }

  if(is.null(variable_names)){
    res["variable_names"] <- list(NULL)
  }else{
    res[["variable_names"]] <- variable_names
  }


  oldClass(res) <- append(oldClass(res), "ResScanOutputMultiFunct")

  return(res)

}
