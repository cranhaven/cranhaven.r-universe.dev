#' Image linearization  (Linearizacao da imagem).
#'
#' @description This function linearize matrix image
#' (Esta funcao lineariza a imagem).
#' @usage linearize_image(im,exclude=FALSE)
#' @param im   :Image object (Imagem com o objeto).
#' @param exclude    :Logical value, if TRUE, pixels with zero will be excluded
#'  (Valor logico, se for TRUE o valor zero sera excluido da matriz).

#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Return images size (Retorna o tamanho das imagens).
#' @seealso  \code{\link{gray_scale}} , \code{\link{read_image}}
#' @examples
#'\donttest{
#' im1=read_image(example_image(2),plot=TRUE)
#' B=gray_scale(im = im1,method = "b",plot=TRUE)
#' plot_image(B,col=3)
#' plot_image(B,col=2,axis = TRUE)
#' m=segmentation(B,treshold =0.38,selectHigher = FALSE,fillHull =TRUE,plot = TRUE )
#'
#' Lin1=linearize_image(im =im1)
#' head(Lin1)
#' info_image(Lin1)

#' Lin2=linearize_image(im=m)
#' head(Lin2)
#' info_image(Lin2)
#'
#' Lin3=linearize_image(im=m,exclude = TRUE)
#' head(Lin3)
#' info_image(Lin3)
#' }

#' @export





linearize_image=function(im,exclude=FALSE){
  if(EBImage::is.Image(im)){
    DD=im@.Data
  }
DD=im
    if(length(dim(DD))==2){

      mat=cbind(expand.grid(Row=1:nrow(DD),Col=1:ncol(DD)),i=c(DD))
    }

    #################################
    if(length(dim(DD))==3){

      mat=cbind(expand.grid(Row=1:dim(DD)[1],Col=1:dim(im)[2]))
      for(i in 1:dim(im)[3]){
        mat=cbind(mat,c(DD[,,i]))
      }
      colnames(mat)=c("Row","Col",paste0("B",1:dim(im)[3]))
    }



  if(!isFALSE(exclude)){
    if(length(exclude)==1){
      id=mat[,3]!=exclude
      mat=mat[id,]
    }

    if(length(exclude)>1){

      id=paste0(mat[,3:5])==paste0(exclude)
    }
  }




  return(mat)

}
