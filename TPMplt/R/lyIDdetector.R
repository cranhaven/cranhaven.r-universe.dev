#' Detecting locations for Strain and Stress
#'
#' @description Function for detecting the locations for Strain and Stress in data frame. It is an key component for automatic
#' completion in the function of \code{\link[TPMplt:epsExtract]{epsExtract}}.
#' @param data A data frame with \code{\link[VBTree:VBTree-package]{VBTree}} style. Pay attention, all factors in column names
#' should be separated by "-" symbol, and factors for temperatures and strain rates should be saved in pure numeric style.
#' @param patterns A regex object to determine layer of Strain and Stress. The default pattern uses "[Ss][Tt][Rr]".
#'
#' @return A list consisted of the layer, and the levels in this layer for Strain and Stress respectively.
#' @import VBTree
#' @export lyIDdetector
#' @seealso \code{\link[VBTree:VBTree-package]{VBTree}}, \code{\link[TPMplt:epsExtract]{TPMplt}}
#'
#' @examples
#' require(VBTree)
#' chrvec2dl(colnames(TPMdata))
#' lyIDdetector(TPMdata)
#' @keywords lyIDdetector epsExtract
lyIDdetector <- function(data, patterns="[Ss][Tt][Rr]"){

  # input data diagnose:
  if(!is.data.frame(data)){
    warning("input data will be convert to data.frame.", call. = FALSE)
    data <- as.data.frame(data)
  }

  name_vec <- colnames(data)
  dl <- chrvec2dl(name_vec)
  layers <- length(dl)
  patterns <- patterns

  # Specify layer for Stress and Strain through pattern"[Ss][Tt][Rr]"
  i <- 1
  for (i in 1:layers) {
    if (sum(as.vector(regexpr(patterns, dl[[i]]))) >= 0){
      break
    }
  }

  # Specify levels for Stress and Strain through pattern "[Aa][Ii]"
  recheck <- length(dl[[i]])
  if (recheck!=2){
    stop("Please check the varaible name; which should only contain stress and strain.", call. = FALSE)
  }

  j <- 1
  for (j in 1:recheck) {
    if(sum(as.vector(regexpr("[Aa][Ii]", dl[[i]][j]))) >= 0){
      break
    }
  }

  if(j==1){
    k <- 2
  } else {
    k <- 1
  }
  result <- list("layer"=i, "strainID"=j, "stressID"=k)
  class(result) <- "pointer"
  return(result)
}
