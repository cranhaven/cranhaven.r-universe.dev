#' @name headwidthsize
#' 
#' @title Converts pollinator (Diptera, Hymenoptera and Lepidoptera) head width (mm) to body size (dry weight (mg)).
#' 
#' @description Calculates dry body weight (mg) using head width (mm) from Hodar (1997).  
#' 
#' @param HW A vector of head width measurements (mm).
#'
#' @param Eq a vector of predictive allometries for insect taxa from Hodar (1997). 
#' Options implemented are: H97DB (Brachycera), H97DN (Nematocera), H97HA (Hymenoptera), H97LH (Heterocera) and H97LR (Rhopalocera).
#'
#' @return A dataframe with pollinator body size as dry weight (mg) is returned for each specimen from selected equation.
#' 
#' @examples
#' headwidthsize(HW=c(10,5,2), Eq = c("H97DB"))
#' @references Kendall et al. (2018)  Pollinator size and its consequences: Predictive allometry for pollinating insects. <doi:10.1101/397604>
#' 
#' Hodar, J. A. (1997). The use of regresion equations for the estimation of prey length and biomass in diet studies of insectivore vertebrates. Miscellania Zoologica, 20(2), 1-10. 
#' 
#' @export
headwidthsize <- function(HW, Eq = "H97DB"){
  if(!Eq %in% c("H97DB", 
                "H97DN", 
                "H97DA","H97LH","H97LR")) {
    stop("Eq should be one of 'H97DB', 'H97DN', 'H97DA' , 'H97LH' or'H97LR'")
  } else {
    H97DB <- 0.655*(HW)^2.526
    H97DN <- 3.942*(HW)^3.106
    H97DA <- 1.999*(HW)^2.09
    H97LH <- 2.053*(HW)^2.804
    H97LR <- 1.634*(HW)^2.793
    if (Eq == "H97DB") out <- H97DB
    if (Eq == "H97DN") out <- H97DN
    if (Eq == "H97DA") out <- H97DA
    if (Eq == "H97LH") out <- H97LH
    if (Eq == "H97LR") out <- H97LR
    out
  }
}
              