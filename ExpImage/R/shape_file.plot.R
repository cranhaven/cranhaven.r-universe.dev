#' Plotar os objetos/parcelas do shapefile na imagem
#'
#' @description Esta funcao desenha as linhas dos objetos/parcelas selecionadas pela
#' funcao shape_file.create.
#' @usage shape_file.plot(im,ShapeFile,colorLines="red",colorNames="red")
#' @param im    :Este objeto deve conter uma imagem no formato do EBImage/ExpImage.
#' @param ShapeFile Objeto criado pela funcao shape_file.create
#' @param colorLines Nome da cor das linhas do grid que serao apresentadas. Default e "red".
#' @param colorNames Nome da cor das nomes plotados sobre cada grid. Default e "red".
#' @seealso  \code{\link{shape_file.create}}, \code{\link{shape_file.split}}
#' @importFrom graphics par
#' @examples
#'\dontrun{
#' end=example_image(13)
#' im=read_image(end,plot=TRUE)
#' A=shape_file.create(im,rows=5,cols=5,rectangular=F,Matrix=NULL,SelectSeveral = F)
#' B=shape_file.BorderExtract(im,A,p.rows = .9,p.cols = .9)
#' shape_file.plot(im,ShapeFile = B)
#' shape_file.split(im =im,shapefile = B,path = getwd(),namesFile = "TEST",type = ".jpg")
#'}
#'@export


shape_file.plot=function(im,ShapeFile,colorLines="red",colorNames="red"){
  A=ShapeFile
  col2=colorLines
  im2=resize_image(im,w=400)
  plot_image(im2)
  info1=info_image(im)$Length
  info2=info_image(im2)$Length
  A[,3]=info2[1]*A[,3]/info1[1]
  A[,4]=info2[2]*A[,4]/info1[2]
  ShapeFile=A
  for(i in unique(ShapeFile[,1])){


    id=ShapeFile[,1]==i
    BB=ShapeFile[id,]

    lines(BB[1:2,3:4],col=col2)
    lines(BB[3:2,3:4],col=col2)
    lines(BB[4:3,3:4],col=col2)
    lines(BB[c(1,4),3:4],col=col2)

    med=colMeans(BB)
    text(med[3],med[4],i,col=colorNames)
  }

}
