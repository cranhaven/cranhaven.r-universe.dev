#'Convert colors (Converter cores).
#'
#'@description This function converts RGB colors to LAB, HEX and XYZ
#'(Esta funcao converte cores RGB para LAB, HEX e XYZ).
#'@usage convert_color(color)
#'@param color A vector with 3 values (r, g and b) or a data.frame with 3 columns (r, g and b)
#' (Um vetor com 3 valores(r, g e b) ou um data.frame com 3 colunas (r, g e b)).

#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Displays colors in RGB for LAB, HEX and XYZ

#' (Apresenta as cores em RGB para LAB, HEX e XYZ)
#'@seealso  \code{\link{pick_color}}


#'@importFrom grDevices dev.off  jpeg
#'@export
#' @examples
#' #Valores de RGB
#' color=c(0.15,0.16,0.17)
#' convert_color(color)

#' color=data.frame(R=runif(10),G=runif(10),B=runif(10))
#' color
#' convert_color(color)


convert_color=function(color){


if(is.vector(color)){
  color=as.data.frame(matrix(color,ncol=3))


  }
if(is.data.frame(color)){
  if(ncol(color)!=3){stop("The number of columns must be equal to 3  (O numero de colunas deve ser igual a 3).")}

  color=as.data.frame(as.matrix(color*255,ncol=3))



  res=schemr::rgb_to_xyz(color, transformation = "sRGB", linear_func = NULL)
  Xyz=as.matrix(res)

  res=schemr::rgb_to_hex(color)
  HEX=c(res)

  res=schemr::rgb_to_lab(color)
  Lab=as.matrix(res)


  }

list(RGB=color,HEX=HEX,LAB=Lab,XYZ=Xyz)

}
