#'Count objects (Contar objetos).
#'
#' @description Count objects by clicking on the image (Contar objetos clicando na imagem)

#' @usage pick_count(im,cex=1,col="red")

#' @param im    :Image in ExpImage format (Imagem no formato do ExpImage).
#' @param cex    :Font size (Tamanho da fonte)
#' @param col    : Font color (Cor da fonte).
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Returns the number of objects in an image (Retorna o numero de objetos em uma imagem).
#' @seealso  \code{\link{pick_length}} , \code{\link{pick_resolution}}

#' @examples
#'\dontrun{
#'im=read_image(example_image(3),plot=TRUE)
#'pick_count(im)
#'}
#'
#'@export
#'

pick_count=function(im,cex=1,col="red"){
  plot_image(im)
  stop=F
  a=0
  coordinates=NULL
  print("click on objects (Clique sobre os objetos)")
  while(isFALSE(stop)){
    a=a+1
    if(a>1){
      text(c$x,c$y,a-1,cex=cex,col=col)
      coordinates=rbind(coordinates,unlist(c))
    }
    c=locator(1)

    if(is.null(c)){ stop = TRUE}
  }

  rownames(coordinates)=1:nrow(coordinates)
  list(Number=nrow(coordinates),coordinates=coordinates)
}
