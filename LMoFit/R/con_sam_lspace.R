#' Condition of sample lpoints, as inside/outside of specific L-spaces on the L-moments ratio diagram, using sample.
#'
#' @param sample for a single site, sample is a vector of observations, e.x. FLOW_AMAX. For multiple sites, sample is a dataframe consisting of multiple columns where each column has the data observed at one site; this dataframe should have column names as station names, e.x. FLOW_AMAX_MULT.
#' @param type the type of the sample. It can be "s" for single site, the default, or "m" for multiple sites.
#' @param Dist select the distribution to plot its L-space in the background. This can be "BrIII" for Burr Typr-III distribution, "BrXII" for Burr Typr-XII distribution, or "GG" for Generalized Gamma distribution. The default Dist is "BrIII".
#'
#' @return The condition of the L-points in regards to the selected L-space as inside or outside.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom sf st_point
#' @importFrom sf st_polygon
#' @importFrom sf st_intersection
#' @importFrom utils data
#' 
#'
#' @examples
#' 
#' con_sam_lspace(LMoFit::FLOW_AMAX, type = "s", Dist = "BrIII")
#' con_sam_lspace(LMoFit::FLOW_AMAX, type = "s", Dist = "BrXII")
#' con_sam_lspace(LMoFit::FLOW_AMAX, type = "s", Dist = "GG")
#' con_sam_lspace(LMoFit::FLOW_AMAX_MULT, type = "m", Dist = "BrIII")
#' con_sam_lspace(LMoFit::FLOW_AMAX_MULT, type = "m", Dist = "BrXII")
#' con_sam_lspace(LMoFit::FLOW_AMAX_MULT, type = "m", Dist = "GG")
#' 
con_sam_lspace <- function(sample, type = "s", Dist = "BrIII") {
  if (Dist == "BrIII" | Dist == "BrXII" | Dist == "GG") {
  } else {
    stop(paste0("make sure to select one of the three distributions that are ", "'BrIII'" , " 'BrXII'", " or 'GG'"))
  }
  if (type == "s" | type == "m") {
  } else {
    stop(paste0("make sure to select one of the two types of samples ", "'s'" , " or 'm'"))
  }
  
  if (type == "s") { #condition of single site
    #sample <- FLOW_AMAX
    slmom <- get_sample_lmom(x = sample)
    point <- st_point(x = c(slmom$st2, slmom$st3), dim = "XY")
    
    if (Dist == "BrIII") {
      x <- LMoFit::lspace_BrIII.xy
      pol_pts <- list(as.matrix(x))
      polygon <- st_polygon(x = pol_pts, dim = "XY")
      
    } else if (Dist == "BrXII") {
      x <- LMoFit::lspace_BrXII.xy
      pol_pts <- list(as.matrix(x))
      polygon <- st_polygon(x = pol_pts, dim = "XY")
      
    } else if (Dist == "GG") {
      x <- LMoFit::lspace_GG.xy
      pol_pts <- list(as.matrix(x))
      polygon <- st_polygon(x = pol_pts, dim = "XY")
      
    }
    
    if (length(st_intersection(x = point, y = polygon)) == 0) {
      conditionx <- "lpoint_outside_lspace"
    } else {conditionx <- "lpoint_inside_lspace"}
    conditionx #this is a condition that defines if the lpoing lies inside or outside the lspace
    condition_all <- conditionx

  } else if (type == "m") { #condition of multiple sites
    
    #sample <- FLOW_AMAX_MULT
    if (Dist == "BrIII") {
      condition_all <- NA
      for (i in 1:ncol(sample)) { #iteration per site
        #i <- 1
        st_i <- get_sample_lmom(x = sample[,i])
        x <- LMoFit::lspace_BrIII.xy
        pol_pts <- list(as.matrix(x))
        point <- st_point(x = c(st_i$st2, st_i$st3), dim = "XY")
        polygon <- st_polygon(x = pol_pts, dim = "XY")
        if (length(st_intersection(x = point, y = polygon)) == 0) {
          conditionx <- "lpoint_outside_lspace"
        } else {conditionx <- "lpoint_inside_lspace"}
        conditionx
        condition_all <- cbind(condition_all, conditionx)
      }
      condition_all <- condition_all[,2:ncol(condition_all)]
      condition_all <- data.frame(sites = colnames(sample[,1:ncol(sample)]), condition = condition_all)
      
    } else if (Dist == "BrXII") {
      condition_all <- NA
      for (i in 1:ncol(sample)) { #iteration per site
        #i <- 1
        st_i <- get_sample_lmom(x = sample[,i])
        x <- LMoFit::lspace_BrXII.xy
        pol_pts <- list(as.matrix(x))
        point <- st_point(x = c(st_i$st2, st_i$st3), dim = "XY")
        polygon <- st_polygon(x = pol_pts, dim = "XY")
        if (length(st_intersection(x = point, y = polygon)) == 0) {
          conditionx <- "lpoint_outside_lspace"
        } else {conditionx <- "lpoint_inside_lspace"}
        conditionx
        condition_all <- cbind(condition_all, conditionx)
      }
      condition_all <- condition_all[,2:ncol(condition_all)]
      condition_all <- data.frame(sites = colnames(sample[,1:ncol(sample)]), condition = condition_all)
      
    } else if (Dist == "GG") {
      condition_all <- NA
      for (i in 1:ncol(sample)) { #iteration per site
        #i <- 1
        st_i <- get_sample_lmom(x = sample[,i])
        x <- LMoFit::lspace_GG.xy
        pol_pts <- list(as.matrix(x))
        point <- st_point(x = c(st_i$st2, st_i$st3), dim = "XY")
        polygon <- st_polygon(x = pol_pts, dim = "XY")
        if (length(st_intersection(x = point, y = polygon)) == 0) {
          conditionx <- "lpoint_outside_lspace"
        } else {conditionx <- "lpoint_inside_lspace"}
        conditionx
        condition_all <- cbind(condition_all, conditionx)
      }
      condition_all <- condition_all[,2:ncol(condition_all)]
      condition_all <- data.frame(sites = colnames(sample[,1:ncol(sample)]), condition = condition_all)
    }
  }
  return(condition_all)
}
