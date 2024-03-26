#' Create a png image with a transparent background
#' (Criar uma imagem png com o fundo transparente)
#'
#' @description Create a png image with a transparent background
#' (Criar uma imagem png com o fundo transparente)
#'
#' @usage create_png(im,segmentation,file=NULL,crop=FALSE)
#' @param im    :This object must contain an image in EBImage format (Este
#'   objeto deve conter uma imagem no formato do EBImage).
#' @param segmentation Binary matrix obtained of a segmentation(matrix binaria obtida por uma segmentacao).
#' @param file Name of the file that will be saved with the `.png` extension
#' (Nome do arquivo que sera salvo com a extensao `.png`).
#' @param crop Logical variable, if it is TRUE the image will be cut considering the segmentation matrix
#' (Variavel logica, se for TRUE a imagem sera cortada considerando a matriz segmentation).

#' @return    : returns an image of type `.png` (retorna uma imagem do tipo `.png`).
#' @seealso  \code{\link{segmentation} , \link{crop_image} }
#' @export

#' @examples
#'\donttest{
#' end=example_image(2)
#' im=read_image(end,plot=TRUE)
#' m=gray_scale(im,method = "GLI",plot=TRUE)
#' plot_image(m,col=3)
#' seg=segmentation(m,treshold = 0.2,fillHull = TRUE,plot=TRUE)
#'
#' info_image(im)
#'
#' im2=create_png(im,seg)
#' info_image(im2)
#' #write_image(im2,files = "imagem.png")
#'
#' im2=create_png(im,seg,crop=TRUE)
#' info_image(im2)
#' #write_image(im2,files = "imagem.png")
#' }

create_png=function(im,segmentation,file=NULL,crop=FALSE){

im2=im
seg=segmentation

comp=info_image(im2)$Length
if(length(comp)==3){
  comp[3]=comp[3]+1
mat=array(NA,dim=comp)
mat[,,1:(comp[3]-1)]=im2@.Data
mat[,,comp[3]]=seg*1
im2@.Data=mat

if(crop==TRUE) {
  im2=crop_image(im2,segmentation = seg)

  }

if(!is.null(file)){
  write_image(im2,files = file,type = "png")
}
return(im2)
}



if(length(comp)==2){
    mat=array(NA,dim=c(comp,2))
  mat[,,1]=im2@.Data
  mat[,,2]=seg*1
  im2@.Data=mat

  if(crop==TRUE) {
    im2=crop_image(im2,segmentation = seg)

  }

  if(!is.null(file)){
    write_image(im2,files = file,type = "png")
  }
  return(im2)
}

}
