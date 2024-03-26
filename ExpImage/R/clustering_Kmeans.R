#'Image segmentation by clustering using the Kmeans method (Segmentacao de imagens pela clusterização pelo metodo Kmeans).
#'
#' @description This function segments images through clustering by the Kmeans method
#' (Esta funcao segmenta imagens por meio da clusterização pelo metodo Kmeans).
#' @usage clustering_Kmeans(im,bands="all",ncluster=2,iter.max=10,
#'           nstart=1,mask=NULL,col="rand",raster=FALSE,seed=NULL,norma=1,plot=TRUE)

#' @param im    :Image that will be segmented (Imagem que sera segmentada).
#' @param bands    :Number indicating the bands that will be used in the segmentation, default and "all"
#'  (Numero indicando as bandas que serao utilizadas na segmentacao, default e "all").
#' @param ncluster    : Desired number of classes (Numero de classes desejado).
#' @param iter.max : 	The maximum number of iterations allowed (número maximo de iteracoes permitido).
#' @param nstart : If centers is a number, how many random sets should be chosen?
#' @param mask    : Mask obtained by the segmentation process, default=NULL (Mascara obtida pelo processo de segmentacao).
#' @param col    : Vector with the desired colors in the segmentation. If it's "rand" it will be random colors
#' (Vetor com as cores desejadas na segmentacao. Se for "rand" serao cores aleatorias).
#'@param raster Caso a imagem seja do tipo TIF pode-se deve-se considerar `raster=TRUE`
#' (If the image is of type TIF, you can consider `raster=TRUE`)
#' @param seed is an integer vector, containing the random number generator (RNG) state for random number generation in R
#' (E um vetor com numero inteiros para a geração de valores aleatorios)
#' @param norma Number by which pixel values should be divided to vary between 0 and 1 (Numero pelo qual os valores dos pixels deverao ser divididos para variar entre 0 e 1)
#' @param plot    : Logical value, if TRUE, the image will be displayed
#' (Valor logico, se for TRUE a imagem sera apresentada).

#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Returns the segmented image (Retorna a imagem segmentada).
#' @seealso  \code{\link{segmentation}} , \code{\link{segmentation_logit}}

#' @examples
#' #Carregando imagens de exemplo
#' end=example_image(13)
#' im=read_image(end,plot=TRUE)
#' seg1=clustering_Kmeans(im,bands = "all",ncluster = 2,col = c("green","red"))
#' seg2=clustering_Kmeans(im,bands = c(1,2,3),ncluster = 3,col = c("green","red","blue"))
#' seg3=clustering_Kmeans(im,bands = c(1,2,3),ncluster = 4,col = "rand")
#'@export
#'





clustering_Kmeans=function(im,bands="all",ncluster=2,iter.max=10,nstart=1,mask=NULL,col="rand",raster=FALSE,seed=NULL,norma=1,plot=TRUE){

  if(!is.null(seed)){base::set.seed(seed)}
  if(norma!=1){im@.Data=im@.Data/norma}

  if(raster==FALSE){

  COL=col
if(length(col)==1){
if(col=="rand"){COL=grDevices::rgb(red =runif(n =ncluster,min = 0,max = 1),green = runif(n =ncluster,min = 0,max = 1),blue = runif(n =ncluster,min = 0,max = 1))}
}

   BANDS=bands
   if(length(bands)==1){
if(bands=="all"){BANDS=1:dim(im)[3]}
   }


imm=im@.Data[,,BANDS]

imb=ima=linearize_image(imm)[,-c(1,2)]
if(!is.null(mask)){
id=linearize_image(im =as_image(mask))[,3]!=0
imb=imb[id,]
}


x=suppressWarnings(stats::kmeans(imb, ncluster,iter.max=iter.max,nstart=nstart))
x2=x3=x$cluster

nm=names(sort(table(x2)))
for(i in 1:length(nm)){
x3[x2==nm[i]]=i
}

if(!is.null(mask)){
  ima[id,]=x3
  ima[!id,]=NA
  x3=ima[,3]
}

m=matrix(x3,nrow=dim(im)[1])


LIST=list()
for(i in 1:length(nm)){
  list=list(m==i)
LIST=c(LIST,list)
}


if(plot==TRUE){
  if(length(dim(im@.Data))==3){
  im@.Data=im@.Data[,,1:3]
  EBImage::colorMode(im)=2
  }
x=mask_pixels(im,TargetPixels = LIST,col.TargetPixels = COL,plot=TRUE)
}

return(LIST)
  }


  if(raster==TRUE){
    im.values=raster::values(im)
    km=suppressWarnings(stats::kmeans(im.values, ncluster,iter.max=iter.max,nstart=nstart))
    res=im[[1]]
    res=raster::setValues(res,km$cluster)
    if(col[1]=="rand"){COL=grDevices::rgb(red =runif(n =ncluster,min = 0,max = 1),green = runif(n =ncluster,min = 0,max = 1),blue = runif(n =ncluster,min = 0,max = 1))}

    if(plot==TRUE){
       raster::plot(res,col=col)

    }
    return(res)

  }
}
