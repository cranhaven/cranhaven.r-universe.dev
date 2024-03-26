#' Excluir as bordas do objetos/parcelas do shapefile
#'
#' @description Esta funcao exclui as bordas (bordadura) dos objetos/parcelas selecionadas pela
#' funcao shape_file.create.
#' @usage shape_file.BorderExtract(im, ShapeFile,p.rows=0.5,p.cols=0.5,col1="red",col2="white")
#' @param im    :Este objeto deve conter uma imagem no formato do EBImage/ExpImage.
#' @param ShapeFile Objeto criado pela funcao shape_file.create
#' @param p.rows E a porcentagem de interesse das parcela no sentido das linhas. Deve variar entre 0 e 1.
#' @param p.cols E a porcentagem de interesse das parcela no sentido das colunas. Deve variar entre 0 e 1.
#' @param col1 Nome da cor das linhas do grid que serao apresentadas. Default e "red".
#' @param col2 Nome da cor das linhas da parcela correspondente a area de interesse. Default e "white".
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


shape_file.BorderExtract=function(im, ShapeFile,
                        p.rows=0.5,
                        p.cols=0.5,
                        col1="red",
                        col2="white"){
  A=ShapeFile
prows=(p.rows)
pcols=(p.cols)
for(i in unique(ShapeFile[,1])){
  id=A[,1]==i
  AA=A[id,]
  med=colMeans(AA)


  A[id,3]=(AA[,3]-med[3])*prows+med[3]
  A[id,4]=(AA[,4]-med[4])*pcols+med[4]


  }
A2=A
if(!is.null(im)){
  im2=resize_image(im,w=400)
plot_image(im2)
info1=info_image(im)$Length
info2=info_image(im2)$Length
A[,3]=info2[1]*A[,3]/info1[1]
A[,4]=info2[2]*A[,4]/info1[2]
ShapeFile=A
for(i in unique(ShapeFile[,1])){
    id=A[,1]==i
    AA=A[id,]

    lines(AA[1:2,3:4],col=col1)
    lines(AA[3:2,3:4],col=col1)
    lines(AA[4:3,3:4],col=col1)
    lines(AA[c(1,4),3:4],col=col1)

    id=ShapeFile[,1]==i
    BB=ShapeFile[id,]

    lines(BB[1:2,3:4],col=col2)
    lines(BB[3:2,3:4],col=col2)
    lines(BB[4:3,3:4],col=col2)
    lines(BB[c(1,4),3:4],col=col2)
  }
}

return(A2)
}
