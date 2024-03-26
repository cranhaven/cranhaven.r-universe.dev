#' Function to get a grayscale image from a color image (Funcao para a obtencao
#' de uma imagem em escala de cinza a partir de uma imagem colorida)
#'
#' @description Function to get a grayscale image from a color image (Esta
#'   funcao permite a obtencao de uma imagem em escala de cinza a partir de uma
#'   imagem colorida).
#' @usage gray_scale(im,method="r",normalize=TRUE,plot=FALSE)
#' @param im    :This object must contain an image in EBImage format (Este
#'   objeto deve conter uma imagem no formato do EBImage).
#' @param method    : Indicates the method for obtaining the gray scale (Este
#'   objeto indica o metodo para a obtencao da escala de cinza).:\cr
#'    "r" = extrair a banda de vermelho\cr
#'    "g" = extrair a banda de verde\cr
#'     "b" = extrair a banda de azul\cr
#'    "rg" = considera a media da banda de vermelho e verde: (r+g)/2\cr
#'    "rb" = considera a media da banda de vermelho e azul: (r+b)/2\cr
#'    "gb" = considera a media da banda de verde e azul: (g+b)/2\cr
#'   "rgb" = considera a media das 3 bandas: (r+g+b)/3\cr
#'   "r/g"=r/g\cr
#'   "r/b"=r/b\cr
#'   "g/r"=g/r\cr
#'   "g/b"=g/b\cr
#'   "b/r"=b/r\cr
#'   "b/g"=b/g\cr
#'   "S"=((R+G+B)-3*B)/(R+G+B)
#'    "BI"=sqrt((r^2+g^2+b^2)/3)\cr
#'    "BIM"=sqrt((2r+2g+2b)/3)\cr
#'    "SCI"=(r-g)/(r+g)\cr
#'    "GLI"=(2g-r-b)/(2g+r+b)\cr
#'    "HI"=(2r-g-b)/(g-b)\cr
#'    "NGRDI"=(g-r)/(g+r)\cr
#'    "SI"=(r-b)/(r+b)\cr
#'    "VARI"=(g-r)/(g+r-b)\cr
#'    "HUE"=atan(2(b-g-r)/30.5(g-r))\cr
#'    "MGVRI"=(g^2-r^2)/(g^2+r^2)\cr
#'    "GLI"=(2g-r-b)/(2g+r+b)\cr
#'    "MPRI"=(g-r)/(g+r)\cr
#'    "RGVBI"=(g-(br))/(g^2(br))\cr
#'    "ExG"=(2*g-r-b)\cr
#'    "VEG"=(g/(g^0.66667*b^0.66667))\cr
#' @param normalize Logic value, if true, the pixel values will be corrected to vary between 0 and 1
#' (Valor logico, se for verdadeiro os valores dos pixels sera corrigido para variar entre 0 e 1).
#' @param plot    :This object must contain an image in EBImage format (Indica
#'   se sera apresentada (TRUE) ou nao (FALSE) (default) a imagem segmentada).

#' @return Returns an image with text over each object in the image (Retorna uma
#'   imagem com um texto sobreposto a cada objeto na imagem)
#' @seealso  \code{\link{segmentation_logit}}

#' @examples
# \donttest{
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2))
#'##mostrar imagem
#'plot_image(im)
#'

#'
#'#Extraindo as camadas R, G e B
#'r=gray_scale(im,method = "r",plot=TRUE)
#'g=gray_scale(im,method = "g",plot=TRUE)
#'b=gray_scale(im,method = "b",plot=TRUE)
#}
#'@export
#' @exportS3Method print gray_scale


gray_scale=function(im,method="r",normalize=TRUE,plot=FALSE){
  #Separar a imagem em bandas


  normatizar=function(MAT){
    MAT=MAT-min(c(MAT),na.rm=T)
    id=1/max(c(MAT),na.rm=T)
    MAT=MAT*id
    return(MAT)
  }

  r=im@.Data[,,1]
  g=im@.Data[,,2]
  b=im@.Data[,,3]

  if(method=="gray"){imm=0.299*r + 0.587*g + 0.114*b}#

  if(method=="r"){imm=r}
  if(method=="g"){imm=g}
  if(method=="b"){imm=b}
  if(method=="rg"){imm=(r+g)/2}
  if(method=="rb"){imm=(r+b)/2}
  if(method=="gb"){imm=(g+b)/2}

  if(method=="r/g"){imm=r/g}#
  if(method=="r/b"){imm=r/b}#
  if(method=="g/r"){imm=g/r}#
  if(method=="g/b"){imm=g/b}#
  if(method=="b/r"){imm=b/r}#
  if(method=="b/g"){imm=b/g}#

  if(method=="rgb"){imm=(r+g+b)/3}
  if(method=="r/rgb"){imm=(r/((c(r))+(c(g))+(c(b))))}
  if(method=="g/rgb"){imm=(g/((c(r))+(c(g))+(c(b))))}
  if(method=="b/rgb"){imm=(b/((c(r))+(c(g))+(c(b))))}
  if(method=="BI"){imm=sqrt((r^2+g^2+b^2)/3)}
  if(method=="BIM"){imm=sqrt((r*2+g*2+b*2)/3)}
  if(method=="SCI"){imm=(r-g)/(r+g)}
  if(method=="GLI"){imm=(2*g-r-b)/(2*g+r+b)}
  if(method=="HI"){imm=(2*r-g-b)/(g-b)}
  if(method=="NGRDI"){imm=(g-r)/(g+r)}
  if(method=="SI"){imm=(r-b)/(r+b)}
  if(method=="S"){imm=((r+g+b)-3*b)/(r+g+b)}#
  if(method=="VARI"){imm=(g-r)/(g+r-b)}
  if(method=="HUE"){imm=atan(2*(b-g-r)/30.5*(g-r))}
  if(method=="MGVRI"){imm=(g^2-r^2)/(g^2+r^2)}
  if(method=="GLI"){imm=(2*g-r-b)/(2*g+r+b)}
  if(method=="MPRI"){imm=(g-r)/(g+r)}
  if(method=="RGVBI"){imm=(g-(b*r))/(g^2*(b*r))}
  if(method=="ExG"){imm=(2*g-r-b)}
  if(method=="VEG"){imm=(g/(g^0.66667*b^0.66667))}

#  if(plot==T){plot_image(EBImage::as.Image( EBImage::normalize(imm)),)}
  if(normalize==TRUE){
  if(plot==T){plot_image(normalize_image(as_image(imm)))}
  return(normalize_image(as_image(imm)))
  }

  if(normalize==FALSE){
    if(plot==T){plot(EBImage::as.Image( (imm)),)}
    return((imm))
  }


}



print.gray_scale=function(x,...){
  if(EBImage::is.Image(x)){cat("Is an image object","\n")}
  if(is.matrix(x)){cat("Is an matrix object","\n")}
  cat("Dimensions of Object:",dim(x),"\n")
}
