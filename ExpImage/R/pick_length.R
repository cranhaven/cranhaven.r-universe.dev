#'Measure objects in the image on click (Medir objetos na imagem ao clicar).
#'
#' @description Measure objects in the image on click (Medir objetos na imagem ao clicar).

#' @usage pick_length(im,dpi=NULL,NumPoints=2,col="red")

#' @param im    :Image in ExpImage format (Imagem no formato do ExpImage).
#' @param dpi    :Dots Per Inch - Number of pixels per inch in the image  (Numero de pixels por polegadas na imagem)
#' @param NumPoints    : Number of points on the segment to be measured (Numero de pontos no segmento que se deseja medir).
#' @param col    : Font color (Cor da fonte).

#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Returns the length of a segment in an image on click (Retorna o comprimento de um segmento em uma imagem ao clicar).
#' @seealso  \code{\link{pick_color}} , \code{\link{pick_resolution}}

#' @examples
#'\dontrun{
#'im=read_image(example_image(3),plot=TRUE)
#'dpi=pick_resolution(im,centimeters=8.5)
#'dpi$dpi
#'pick_length(im,dpi = dpi$dpi)
#'
#'}
#'
#'@export
#'

pick_length=function(im,dpi=NULL,NumPoints=2,col="red"){

  plot_image(im)
  stop=F
  a=0
print("Click on the edges of objects (Clique sobre as extremidades dos objetos)")
  if(NumPoints==2){
    coordinates=NULL
    while(isFALSE(stop)){
      a=a+1
      if(a>1){
        lines(c(c1$x,c2$x),c(c1$y,c2$y),col=col)
        coordinates=rbind(coordinates,c(unlist(c1),unlist(c2)))
      }
      c1=locator(1) ; if(!is.null(c1)) {c2=locator(1)}

      if(is.null(c1)){ stop = TRUE}
    }


    if(is.null(dpi)){dpi=1}
    mat=apply(coordinates,1,function(x) c(length_pixels=sqrt((x[1]-x[3])^2+(x[2]-x[4])^2),
                                          length_inch=sqrt((x[1]-x[3])^2+(x[2]-x[4])^2)/dpi,
                                          length_cm= sqrt((x[1]-x[3])^2+(x[2]-x[4])^2)*2.54/(dpi)))

    coordinates=cbind(coordinates,t(mat))
    colnames(coordinates)=c("x1","y1","x2","y2","length_pixels" ,"length_inch" ,"length_cm")

    if(is.null(dpi)){
      coordinates=coordinates[,-c(5:7)]
    }
    rownames(coordinates)=1:nrow(coordinates)
    list(Number=nrow(coordinates),coordinates=coordinates)
  }

  if(NumPoints==3){
    coordinates=NULL
    while(isFALSE(stop)){
      a=a+1
      if(a>1){
        lines(c(c1$x,c2$x),c(c1$y,c2$y),col=col)
        lines(c(c2$x,c3$x),c(c2$y,c3$y),col=col)
        coordinates=rbind(coordinates,c(unlist(c1),unlist(c2),unlist(c3)))
      }
      c1=locator(1) ; if(!is.null(c1)) {c2=locator(1)} ; if(!is.null(c1)) {c3=locator(1)}

      if(is.null(c1)){ stop = TRUE}
    }


    if(is.null(dpi)){dpi=1}
    mat=apply(coordinates,1,function(x) c(length_pixels=sqrt((x[1]-x[3])^2+(x[2]-x[4])^2)+sqrt((x[3]-x[5])^2+(x[4]-x[6])^2),
                                          length_inch=(sqrt((x[1]-x[3])^2+(x[2]-x[4])^2)+sqrt((x[3]-x[5])^2+(x[4]-x[6])^2))/dpi,
                                          length_cm= (sqrt((x[1]-x[3])^2+(x[2]-x[4])^2)+sqrt((x[3]-x[5])^2+(x[4]-x[6])^2))*2.54/(dpi)))

    coordinates=cbind(coordinates,t(mat))
    colnames(coordinates)=c("x1","y1","x2","y2","x3","y3","length_pixels" ,"length_inch" ,"length_cm")

    if(is.null(dpi)){
      coordinates=coordinates[,-c(7:9)]
    }
    rownames(coordinates)=1:nrow(coordinates)
    list(Number=nrow(coordinates),coordinates=coordinates)
  }
  list(Number=nrow(coordinates),coordinates=coordinates)
}
