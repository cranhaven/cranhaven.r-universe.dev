#' @name nectarload
#' 
#' @title Converts bee body length (mm) to total field nectar load (ul).
#' 
#' @description Calculates total field nectar load (ul) using body length (mm) from Henry & Rodet (2008).  
#' 
#' @param  BL vector of body length measurements (mm).
#'
#' @return A dataframe with bee total field nectar load (ul) is returned for each specimen.
#' 
#' @examples
#' nectarload(BL=c(10,5,2))
#' @references Kendall et al. (2018)  Pollinator size and its consequences: Predictive allometry for pollinating insects. <doi:10.1101/397604>
#' 
#' Henry, M., & Rodet, G. (2018). Controlling the impact of the managed honeybee on wild bees in protected areas. Scientific reports, 8(1), 9308.
#' @export
nectarload <- function(BL){
    out <- 0.005 * BL^3.0618
    out
  }
