#' Get the dpi resolution on an image (Obter a resolução em dpi em uma imagem).
#'
#' @description Obter o numero de pixels por centimetros ou polegadas em uma imagem
#' (Get the number of pixels per centimeters or inches in an image).

#' @usage pick_resolution(im,inches=NULL,centimeters=NULL,several=TRUE,col="red")

#' @param im    :Image in ExpImage format (Imagem no formato do ExpImage).
#' @param inches    :reference object size in inches (tamanho do objeto de referencia em polegadas).
#' @param centimeters    :reference object size in centimeters (tamanho do objeto de referencia em centimetros).
#' @param several    :TRUE: If you want to get the DPI from the average of several measurements (TRUE: Se deseja obter o DPI a partir da média de várias medidas ).
#' @param col    : Font color (Cor da fonte).

#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Returns the resolution in DPI of an image. (Retorna a resolução em uma imagem).
#' @seealso  \code{\link{pick_color}} , \code{\link{pick_length}}

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

pick_resolution=function(im,inches=NULL,centimeters=NULL,several=TRUE,col="red"){

  if(is.null(inches)&is.null(centimeters)) {stop("The inches or centimeters arguments must have values (Os argumentos inches ou centimeters devem ter valores).")}
  if(!is.null(inches)&!is.null(centimeters)) {stop("The inches or centimeters argument must be NULL (O argumento inches ou centimeters deve ser NULL).")}
  if(!is.null(inches)|!is.null(centimeters)) {
    if(!is.null(inches)){centimeters=inches*2.54}
    if(!is.null(centimeters)){inches=centimeters*0.393701}
  }


  plot_image(im)
  stop=F
  a=0

  print("Click on the edges of objects (Clique sobre as extremidades dos objetos)")

  coordinates=NULL
  while(isFALSE(stop)){
    a=a+1
    if(a>1){
      lines(c(c1$x,c2$x),c(c1$y,c2$y),col=col)
      coordinates=rbind(coordinates,c(unlist(c1),unlist(c2)))
    }
    c1=locator(1) ; if(!is.null(c1)) {c2=locator(1)}
    if(!isTRUE(several)){stop=TRUE}
    if(is.null(c1)){ stop = TRUE}

  }



  mat=apply(coordinates,1,function(x) c(length_pixels=sqrt((x[1]-x[3])^2+(x[2]-x[4])^2)))

  coordinates=cbind(coordinates,length_pixels=(mat))
  colnames(coordinates)=c("x1","y1","x2","y2","length_pixels" )

  med=mean(coordinates[,5])

  dpi=med/inches
  dpc=med/centimeters

  list(coordinates=coordinates,dpi=dpi,dpc=dpc)
}
