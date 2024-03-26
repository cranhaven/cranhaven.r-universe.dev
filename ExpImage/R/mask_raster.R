#' Creates a mask over the background or foreground in a raster image (Cria uma mascara sobre o
#' background ou foreground em uma imagem no formato raster)
#'
#' @description Function to create a mask above the background or foreground.
#' (Funcao para criar uma mascara sobre o background ou foreground).
#' @usage mask_raster(Class,legend,col,main,plot=TRUE)
#' @param Class Raster object with pixels grouped into classes
#' (Objeto do tipo raster com os pixels agrupados em classes).
#' @param legend Vector with the names corresponding to each class (Vetor os nomes correspondentes a cada classe).
#' @param col Vector with the color names corresponding to each class (Vetor os nomes das cores correspondentes a cada classe).
#' @param main Titulo da figura (Figure title)
#' @param plot Logic value, if TRUE the figure will be shown (Valor logico, se TRUE a figura sera apresentada)
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#' @return Returns an array (retorna uma matriz).

#'
#'@export


mask_raster=function(Class,legend,col,main,plot=TRUE){

  res2000=Class
  res.val=raster::values(res2000)
  for(i in unique(res.val)){
    res.val[res.val==i]=legend[i]
  }

  res.val=as.factor(res.val)
  res.class=raster::setValues(x=res2000,res.val)

  ncluster=length(unique(legend))
  if(is.null(col))
  {col=grDevices::rgb(red =runif(n =ncluster,min = 0,max = 1),green = runif(n =ncluster,min = 0,max = 1),blue = runif(n =ncluster,min = 0,max = 1))}

  coord=raster::extent(res.class)
  if(plot==TRUE){
    raster::plot(res.class,col=col,legend=F,main=main,
         xlim=c(coord[1], coord[2]),
         ylim=c(coord[3], coord[4]))
    legend(x="right", levels(as.factor(legend)), fill=col,bg="white",box.lty=0)
  }

  return(res.class)
}
