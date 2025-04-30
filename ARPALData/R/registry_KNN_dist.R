#' Identifies the K-nearest-neighbors (stations) to all the monitoring sites included in a given ARPALdf
#' registry data.frame. The neighbors are identified computing the Euclidean distance among the sites'
#' coordinates.
#'
#' @description For each element included in reg_X, it identifies the k-nearest neighbors locations
#' (among those included in reg_Y) according to an Euclidean distance metric. reg_X and reg_Y
#' must be two 'ARPALdf' objects obtained using get_ARPA_Lombardia_xxx_registry'.
#'
#' @param reg_X Dataset of class 'ARPALdf' containing the stations list obtained as registry
#' (from 'get_ARPA_Lombardia_xxx_registry' command). The object must contain the following
#' colums: 'IDStation','NameStation','Longitude' and 'Latitude'.
#' @param reg_Y Dataset of class 'ARPALdf' containing the stations list obtained as registry
#' (from 'get_ARPA_Lombardia_xxx_registry' command). The object must contain the following
#' colums: 'IDStation','NameStation','Longitude' and 'Latitude'.
#' @param k Integer value. Represents the number of neighbors the user wants to identify.
#'
#' @return A data.frame object having the same length of reg_X. For each row (stations in reg_X)
#' it contains the name and the IDStation code for the k-nearest neighbors.
#'
#' @examples
#' if (require("tidyverse")) {
#'     regAQ <- get_ARPA_Lombardia_AQ_registry()
#'     regAQ <- regAQ %>% filter(Pollutant %in% c("PM10","Ammonia"))
#'     regW <- get_ARPA_Lombardia_W_registry()
#'     registry_KNN_dist(regAQ,regW,k=2)
#' }
#'
#' @export

registry_KNN_dist <- function(reg_X,reg_Y,k=1){

  # For each element included in reg_X, it dentifies the k-nearest neighbours (among those included in reg_Y)
  # according to an Euclidean distance metric

  reg_X <- reg_X %>%
    dplyr::distinct(.data$IDStation,.data$NameStation,.data$Longitude,.data$Latitude) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) %>%
    dplyr::arrange(.data$IDStation)
  reg_Y <- reg_Y %>%
    dplyr::distinct(.data$IDStation,.data$NameStation,.data$Longitude,.data$Latitude) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) %>%
    dplyr::arrange(.data$IDStation)

  ### Computing distance
  dist_matrix <- sf::st_distance(reg_X,reg_Y)
  ### Identifying the k-NN IDStation of reg_Y
  min_dist_idx <- apply(dist_matrix,MARGIN = 1, FUN = k_min_vals_vec_idx, k=k)
  min_dist <- apply(dist_matrix,MARGIN = 1, FUN = k_min_vals_vec, k=k)
  if (k == 1) {
    min_dist_idx <- t(as.matrix(min_dist_idx))
    min_dist <- t(as.matrix(min_dist))
  }
  ### Extracting for each k the corresponding ID and NameStation of reg_Y
  knn_list <- list(length = k)
  for (j in 1:k) {
    knn_prog <- data.frame(cbind(reg_Y[min_dist_idx[j,],]$NameStation,
                                 reg_Y[min_dist_idx[j,],]$IDStation,
                                 round(min_dist[j,],2)))
    colnames(knn_prog) <- c(paste0("reg_Y_nn",j,"_name"),
                            paste0("reg_Y_nn",j,"_ID"),
                            paste0("reg_Y_nn",j,"_dist"))
    knn_list[[j]] <- knn_prog
  }
  ### Selecting columns from reg_X
  reg_X_name <- reg_X %>%
    dplyr::select(reg_X_name = .data$NameStation, reg_X_ID = .data$IDStation)
  ### Combining dataframes
  output_tab <- dplyr::bind_cols(reg_X_name,knn_list)

  return(list(output_tab))
}
