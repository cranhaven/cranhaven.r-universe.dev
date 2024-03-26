#'Esta funcao rotaciona a imagem. (This function rotates the image .)
#'@description Esta funcao permite rotacionar a imagem (This function allows
#'rotate the image).
#'@usage rotate_image(im,angle=NULL,BGcolor=c(0,0,0),plot=TRUE)
#'@param im Este objeto deve conter uma imagem no formato do EBImage (This
#'  object must contain an image in EBImage format ).
#'@param angle Valor em graus (Degree value).
#'@param BGcolor Vetor com os valores que preencherao o background (Vector with
#' the values that will fill the background).
#'@param plot Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'  imagem segmentada (Indicates whether the segmented image will be
#'  displayed (TRUE) or not (FALSE) (default)).

#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Retorna uma imagem rotacionada (Returns a rotated image).
#'@seealso  \code{\link{edit_image}}
#'@importFrom stats binomial glm predict
#'@importFrom grDevices dev.off  jpeg
#'@export
#' @examples

#'#library(ExpImage)
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2),plot=TRUE)
#'
#'##rotacionar a imagem
#'im2=rotate_image(im,angle=80)
#'im2=rotate_image(im,angle=60,BGcolor=c(1,1,1))
#' \dontrun{
#'im2=rotate_image(im)
#'
#'}

#' @exportS3Method print rotate_image
#'




rotate_image=function(im,angle=NULL,BGcolor=c(0,0,0),plot=TRUE){


      if( EBImage::is.Image(im)){
        if(is.null(angle)==FALSE){
          #print("xxxx")
        im2=EBImage::rotate(im,angle = angle)
        id=(im2@.Data[,,1]==0)&(im2@.Data[,,2]==0)&(im2@.Data[,,3]==0)
        im2@.Data[,,1][id]=BGcolor[1]
        im2@.Data[,,2][id]=BGcolor[2]
        im2@.Data[,,3][id]=BGcolor[3]
        }

        if(is.null(angle)){
       message("Clique em dois ponto sobre a imagem \n
Click on two points on the image ")
          plot_image(im)
        coord.a <- locator(type="p",n = 1, col="red",pch=19)
        coord.b <- locator(type="p",n = 1, col="red",pch=19)

        coord<-as.data.frame(mapply(c, coord.a, coord.b))
        colnames(coord)<-c("x","y")
        lines(coord, col= "red")
        if((coord$y[1]>=coord$y[2])&(coord$x[2]>=coord$x[1])){theta = (atan2((coord$y[1] - coord$y[2]), (coord$x[2] - coord$x[1])))*(180/pi)}
        if((coord$y[2]>=coord$y[1])&(coord$x[2]>=coord$x[1])){theta = (atan2((coord$y[2] - coord$y[1]), (coord$x[2] - coord$x[1])))*(180/pi)}
        if((coord$y[1]>=coord$y[2])&(coord$x[1]>=coord$x[2])){theta = (atan2((coord$y[1] - coord$y[2]), (coord$x[1] - coord$x[2])))*(180/pi)}
        if((coord$y[2]>=coord$y[1])&(coord$x[1]>=coord$x[2])){theta = (atan2((coord$y[2] - coord$y[1]), (coord$x[1] - coord$x[2])))*(180/pi)}
        print(paste("Theta = ",theta-90))
        message("Espere (Wait) ...")
       im2= rotate_image(im,angle = theta-90)

        }


        if(plot==TRUE){plot_image(im2)}
        return(im2)
      }



      if(is.matrix(im)){

        if(is.null(angle)!=TRUE){
        im0=EBImage::as.Image(im)
        im2=EBImage::rotate(im0,angle = angle)
        id=(im2@.Data[,]==0)
        im2@.Data[id]=BGcolor[1]
        im3=im2@.Data
        }


        if(is.null(angle)){
          plot_image(im)
          coord.a <- locator(type="p",n = 1, col="red",pch=19)
          coord.b <- locator(type="p",n = 1, col="red",pch=19)
          coord<-as.data.frame(mapply(c, coord.a, coord.b))
          colnames(coord)<-c("x","y")
          lines(coord, col= "red")
          if((coord$y[1]>=coord$y[2])&(coord$x[2]>=coord$x[1])){theta = (atan2((coord$y[1] - coord$y[2]), (coord$x[2] - coord$x[1])))*(180/pi)}
          if((coord$y[2]>=coord$y[1])&(coord$x[2]>=coord$x[1])){theta = (atan2((coord$y[2] - coord$y[1]), (coord$x[2] - coord$x[1])))*(180/pi)}
          if((coord$y[1]>=coord$y[2])&(coord$x[1]>=coord$x[2])){theta = (atan2((coord$y[1] - coord$y[2]), (coord$x[1] - coord$x[2])))*(180/pi)}
          if((coord$y[2]>=coord$y[1])&(coord$x[1]>=coord$x[2])){theta = (atan2((coord$y[2] - coord$y[1]), (coord$x[1] - coord$x[2])))*(180/pi)}

          im2= rotate_image(im,angle = theta-90,BGcolor =BGcolor )

        }



        if(plot==TRUE){plot_image(EBImage::as.Image((im3)))}
        return(im3)
      }

}





print.rotate_image=function(x,...){
  if(EBImage::is.Image(x)){cat("Is an image object","\n")}
  if(is.matrix(x)){cat("Is an matrix object","\n")}
  cat("Dimensions of Object:",dim(x@.Data),"\n")
}
