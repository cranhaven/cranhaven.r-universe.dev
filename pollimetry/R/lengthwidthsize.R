#' @name lengthwidthsize
#' 
#' @title Converts pollinator body length*body width to body size (dry weight (mg)).
#' 
#' @description Calculates body size as dry weight (mg) from the equations described by Sample (1993) and others using body length*body width values (mm).  
#' 
#' @param BLW A vector of fly body length*body width measurments (mm).
#'
#' @param Eq a vector of a predictive allometry for Diptera, Hymenoptera or Lepidoptera. Options implemented are:
#'
#' @return A dataframe with body size, as dry weight (mg) is returned for each specimen from selected equation.
#' 
#' @examples
#' lengthwidthsize(BLW=c(3,5,2), Eq = c("Brachycera"))
#' @references Kendall et al. (2018) Pollinator size and its consequences: Predictive allometry for pollinating insects. <doi:10.1101/397604>
#' 
#' Sample et al. (1993) Estimation of insect biomass by length and width. American Midland Naturalist, 234-240.
#' 
#' @export
lengthwidthsize <- function(BLW, Eq = "DIP"){
  if(!Eq %in% c("Brachycera", 
                "Cyclorrapha", 
                "DIP",
                "Nematocera","S93HBLW", 
                "S93HFLW", 
                "S93HHLW",
                "S93HILW",
                "S93HALW",
                "S93HPLW","S93HVLW","S93LGLW", 
                "S93LMLW", 
                "S93LALW",
                "W13LLW",
                "S93LCLW",
                "S93LNLW","LEP")) {
    stop("Eq should be one of 'Brachycera', 'Cyclorrapha', 'Nematocera' or 'DIP','S93HBLW', 'S93HFLW', 'S93HHLW' , 'S93HILW','S93HALW','S93HPLW','S93HVLW',
'S93LGLW', 'S93LMLW', 'S93LALW' , 'W13LLW','S93LCLW','S93LNLW','LEP'")
  } else {
    #DIP
    Brachycera <- exp(-2.2+1.259*log(BLW))
    Cyclorrapha <- exp(-2.02+1.298*log(BLW))  
    Sage_All <- exp(-2.197+1.309*log(BLW))
    Nematocera <- exp(-2.217+1.288*log(BLW))
    #HYM
    S93HBLW <- exp(-2.19+1.445*log(BLW))
    S93HFLW <- exp(-2.378+1.473*log(BLW))
    S93HHLW <- exp(-1.946+1.444*log(BLW))
    S93HILW <- exp(-2.497+1.445*log(BLW))
    S93HALW <- exp(-2.375+1.456*log(BLW))
    S93HPLW <-  exp(-1.946+1.444*log(BLW))
    S93HVLW <-exp(-1.537+1.319*log(BLW))
    #LEP
    S93LGLW <- exp(-2.343+1.387*log(BLW))
    S93LMLW <- exp(-2.715+1.395*log(BLW))
    S93LALW <- exp(-2.607+1.457*log(BLW))
    W13LLW <- exp(-2.1+1.37*log(BLW))
    S93LCLW <- exp(-1.658+1.222*log(BLW))
    S93LNLW <-  exp(-1.607+1.214*log(BLW))
   
    if (Eq == "Brachycera") out <- Brachycera
    if (Eq == "Cyclorrapha") out <- Cyclorrapha
    if (Eq == "All") out <- Sage_All
    if (Eq == "Nematocera") out <- Nematocera
    
    if (Eq == "S93HBLW") out <- S93HBLW
    if (Eq == "S93HFLW") out <- S93HFLW
    if (Eq == "S93HHLW") out <- S93HHLW
    if (Eq == "S93HILW") out <- S93HILW
    if (Eq == "S93HALW") out <- S93HALW
    if (Eq == "S93HPLW") out <- S93HPLW
    if (Eq == "S93HVLW") out <- S93HVLW
    
   
   if (Eq == "S93LGLW") out <- Brachycera
   if (Eq == "S93LMLW") out <- Cyclorrapha
   if (Eq == "S93LALW") out <- Sage_All
   if (Eq == "W13LLW") out <- Nematocera
   if (Eq == "All") out <- cbind(W13LLW,S93LALW)
    out
  }
}

