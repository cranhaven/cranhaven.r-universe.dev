#' Join images(Juntar imagens)
#'
#' @description This function joins images by placing them side by side (Esta funcao junta imagens colocando uma do lado da outra).
#' @usage join_image(im1=NULL,im2=NULL,im3=NULL,im4=NULL,im5=NULL,bk=c(1,1,1),
#' ncol=NULL,normalize=FALSE,plot=TRUE)

#' @param im1    :Object containing an array, image in EBImage format or list of images
#'  (Objeto contendo um array, imagem, ou lista de imagens no formato do EBImage).
#' @param im2    :Object containing an array, image in EBImage format or list of images
#'  (Objeto contendo um array, imagem, ou lista de imagens no formato do EBImage).
#' @param im3   :Object containing an array, image in EBImage format or list of images
#'  (Objeto contendo um array, imagem, ou lista de imagens no formato do EBImage).
#' @param im4    :Object containing an array, image in EBImage format or list of images
#'  (Objeto contendo um array, imagem, ou lista de imagens no formato do EBImage).
#' @param im5    :Object containing an array, image in EBImage format or list of images
#'  (Objeto contendo um array, imagem, ou lista de imagens no formato do EBImage).
#' @param bk :Vector white rgb values for background(Vetor contendo os valores de rgb
#'  que serao considerados no background)
#' @param ncol    :Number of columns where images will appear in the chart (Numero de colunas em que as imagens aparecerao no grafico)
#' @param normalize    :Logical value indicating whether the image needs to be normalized
#'  (Valor logico indicando se a imagem precisa ser nomalizada)

#' @param plot    :Logical value. Indicates whether the image will be displayed (TRUE) or not
#'   (FALSE) (default) (Indica se sera apresentada (TRUE) ou nao (FALSE)
#'   (default) a imagem segmentada).

#' @return Return multiple images joined (Retorna a uniao de varias imagens).
#' @seealso  \code{\link{crop_image}}, \code{\link{edit_image}}
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @examples
#'\donttest{
#' im1=read_image(example_image(2),plot=TRUE)
#'
#' ##Aumentar brilho
#' im2=edit_image(im1,brightness = 0.1)
#'
#' #Aumentar contraste
#' im3=edit_image(im1,contrast = 1.2)
#'
#' #Aumentar gamma
#' im4=edit_image(im1,gamma  = 1.1)
#'
#' #Alterando brilho, contraste e gamma
#' im5=edit_image(im3,brightness = 0.1,contrast = 1.7,gamma  = 1.2)
#'
#' #Juntando imagens em um objeto raster e plotando
#' join_image(im1,im2,im3,im4,im5)
#'
#' #Juntando imagens em um objeto raster e plotando com uma lista
#' List1=list(im1,im2,im3,im4,im5)
#' join_image(List1)
#'
#'
#'
#' im1b=read_image(example_image(7),plot=TRUE)
#'
#' ##Aumentar brilho
#' im2b=edit_image(im1b,brightness = 0.1)
#'
#' #Aumentar contraste
#' im3b=edit_image(im1b,contrast = 1.2)
#'
#' #Aumentar gamma
#' im4b=edit_image(im1b,gamma  = 1.1)
#'
#' #Alterando brilho, contraste e gamma
#' im5b=edit_image(im3b,brightness = 0.1,contrast = 1.7,gamma  = 1.2)
#'
#' #Juntando imagens em um objeto raster e plotando
#' join_image(im1b,im2b,im3b,im4b,im5b)
#'
#' #Juntando imagens em um objeto raster e plotando com uma lista
#' List2=list(im1b,im2b,im3b,im4b,im5b)
#' join_image(List2)
#'
#' #Juntando imagens presentes em duas listas
#' join_image(List1,List2)
#'}
#' @export
# @exportS3Method print join_image
#' @importFrom stats na.omit


join_image=function(im1=NULL,im2=NULL,im3=NULL,im4=NULL,im5=NULL,bk=c(1,1,1), ncol=NULL,normalize=FALSE,plot=TRUE){

  if(EBImage::is.Image(im1)) im1=list(im1)
  if(EBImage::is.Image(im2)) im2=list(im2)
  if(EBImage::is.Image(im3)) im3=list(im3)
  if(EBImage::is.Image(im4)) im4=list(im4)
  if(EBImage::is.Image(im5)) im5=list(im5)
  list1=list()
  list1=c(im1,im2,im3,im4,im5)



  nrow=NULL


  nn=length(list1)
  n=NULL
  for(i in 1:nn){

    ii=dim(list1[[i]])[1:2]
    n=rbind(n,ii)
  }

  nn=nrow(n)



  for(i in 1:length(list1)){
   if(length(dim(list1[[i]]@.Data))==2){
     list1[[i]]@.Data=array(c(list1[[i]]@.Data,list1[[i]]@.Data,list1[[i]]@.Data),dim =c(dim(list1[[i]]@.Data)[1],
           dim(list1[[i]]@.Data)[2],3) )


   }

  }


  mat=array(grDevices::rgb(bk[1],bk[2],bk[3]),dim = c(apply(n,2,max),nrow(n)))
  for(i in 1:nn){
    if(!is.null(list1[[i]])){
      EBImage::colorMode(list1[[i]])=as.integer(2)
      list1[[i]]=(list1[[i]])
      if(normalize==TRUE) list1[[i]]=EBImage::normalize(list1[[i]])
      mat[1:n[i,1],1:n[i,2],i]=grDevices::rgb(red = list1[[i]]@.Data[,,1],green = list1[[i]]@.Data[,,2],blue = list1[[i]]@.Data[,,3])
    }
}


  if(plot==TRUE){
    if(is.null(ncol)&is.null(nrow)){
      ncol=ceiling(sqrt(nn))
      nrow=ceiling(nn/ncol)
    }
if(!is.null(ncol)){nrow=ceiling(nn/ncol)}
    op <- par(mfrow = c(nrow, ncol))
   # op
    for(i in 1:nn){
      plot(EBImage::as.Image(mat[,,i]))
    }
    op <- par(mfrow = c(1, 1))
  }


  return(mat)
}

# print.join_image=function(x,...){
#   if(EBImage::EBImage::is.Image(x)){cat("Is an image object","\n")}
#   if(is.matrix(x)){cat("Is an matrix object","\n")}
#   cat("Dimensions of Object:",dim(x),"\n")
# }
