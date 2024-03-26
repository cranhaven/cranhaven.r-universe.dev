#' Condition of sample lpoints, as inside/outside of specific L-spaces on the L-moments ratio diagram, using sample lmoments.
#'
#' @param samplelmom L-moments as c(l1, l2, l3, l4, t2, t3, t4). Use get_sample_lmom() to obtain these lmoments.
#' @param Dist select the distribution to plot its L-space in the background. This can be "BrIII" for Burr Typr-III distribution, "BrXII" for Burr Typr-XII distribution, or "GG" for Generalized Gamma distribution. The default Dist is "BrIII". The default is set to BrIII.
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
#' sample <- LMoFit::FLOW_AMAX
#' samplelmom <- get_sample_lmom(x = sample)
#' con_samlmom_lspace(samplelmom, Dist = "BrIII")
#' con_samlmom_lspace(samplelmom, Dist = "BrXII")
#' con_samlmom_lspace(samplelmom, Dist = "GG")
#' 
con_samlmom_lspace <- function(samplelmom, Dist = "BrIII") {
  samplelmom <- as.numeric(samplelmom)
  point <- st_point(x = c(samplelmom[5], samplelmom[6]), dim = "XY") #point as (st2, st3)
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
  } else {
    stop(paste0("make sure to select one of the three distributions that are ", "'BrIII'" , " 'BrXII'", " or 'GG'"))
  }
  
  if (length(st_intersection(x = point, y = polygon)) == 0) {
    conditionx <- "lpoint_outside_lspace"
  } else {conditionx <- "lpoint_inside_lspace"}
  conditionx #this is a condition that defines if the lpoing lies inside or outside the lspace
  
  return(conditionx)
}
