#'Image segmentation by Random Forest (Segmentacao de imagens por Random Forest).
#'
#' @description This function fits a Random Forest model for image segmentation
#' (Esta funcao ajusta um modelo Random Forest para a segmentacao de imagens).
#'
#' @usage segmentation_RF(im=NULL,palette,return="model",NumMax=1000,
#'                       col="rand",seed=NULL,norma=1,plot=TRUE)
#' @param im :Image that will be segmented (Imagem que sera segmentada).
#' @param palette :Object of type dataframe or list. If it is a dataframe, the first column must be the class and the other
#'  columns the values of the bands corresponding to each pixel. If it is list,
#'  there must be an image in each item corresponding to each of the classes.
#'  (Objeto do tipo dataframe ou list. Se for um dataframe a primeira coluna deve ser a classe e as demais
#'  colunas os valores das bandas correspondentes a cada pixel. Se for list deve ter uma imagem em cada item correspondente
#'   a cada uma das classes).
#' @param return : Object indicating what will be returned by the function,
#' if it is "image" the segmented image will be returned,
#'  if it is "model" it will be the model adjusted by the Random Forest methodology
#'  (Objeto indicando o que sera retornado pela funcao, se for "image" sera retornado a imagem segmentada,
#'   se for "model" será o modelo ajustado pela metodologia Random Forest).
#' @param NumMax : Maximum number of pixels of each class to be considered when adjusting
#' the Random Forest methodology (Número máximo de pixels de cada classe a serem
#' considerados no ajuste da metodologia Random Forest).
#' @param col    : Vector with the desired colors in the segmentation. If it's "rand" it will be random colors
#' (Vetor com as cores desejadas na segmentacao. Se for "rand" serao cores aleatorias).
#' @param seed is an integer vector, containing the random number generator (RNG) state for random number generation in R
#' (E um vetor com numero inteiros para a geração de valores aleatorios)
#' @param norma Number by which pixel values should be divided to vary between 0 and 1 (Numero pelo qual os valores dos pixels deverao ser divididos para variar entre 0 e 1)
#' @param plot    : Logical value, if TRUE, the image will be displayed
#' (Valor logico, se for TRUE a imagem sera apresentada).

#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Returns the segmented image (Retorna a imagem segmentada).
#' @seealso  \code{\link{predict_RF}} , \code{\link{segmentation_logit}}
#' @importFrom grDevices col2rgb
#'@export
#' @examples
#'
#' \donttest{
#' #Carregando imagens de exemplo
#' end=example_image(13)
#' im=read_image(end,plot=TRUE)
#' seg1=clustering_Kmeans(im,bands = "all",ncluster = 2,col = c("green","red"))
#' seg2=clustering_Kmeans(im,bands = c(1,2,3),ncluster = 3,col = c("green","red","blue"))
#' seg3=clustering_Kmeans(im,bands = c(1,2,3),ncluster = 4,col = "rand")
#' im=read_image(example_image(3))
#' Fundo=read_image(example_image(4))
#' Folha=read_image(example_image(5))
#' Ref=read_image(example_image(6))
#' paleta=list(Fundo=Fundo,Folha=Folha,Referencia=Ref)
#' col=c("black","green","red")
#'
#' #### Criando imagem
#' Image=segmentation_RF(im=im,
#'                       palette=paleta,
#'                       return="image",
#'                       NumMax=1000,
#'                       col=col,
#'                       seed=NULL,
#'                       norma=1,
#'                       plot=TRUE)
#'
#'
#' #Criando um modelo
#' model=segmentation_RF(im=im,
#' palette=paleta,
#' return="model",
#' NumMax=1000,
#' col=col,
#' seed=NULL,
#' norma=1,
#' plot=FALSE)
#'
#'
#' image=predict_RF(im,model,col="rand",plot=TRUE)
#'
#' # Outra forma de criar a paleta de cores
#' pallete2=rbind(
#' cbind(Class="Fundo",linearize_image(Fundo)[,-c(1,2)]),
#' cbind(Class="Folha",linearize_image(Folha)[,-c(1,2)]),
#' cbind(Class="Referencia",linearize_image(Ref)[,-c(1,2)]))
#' Image=segmentation_RF(im=im,
#'                       palette=pallete2,
#'                       return="image",
#'                       NumMax=1000,
#'                       col="rand",
#'                       seed=NULL,
#'                       norma=1,
#'                       plot=TRUE)
#'
#'
#'
#'}




segmentation_RF=function(im=NULL,
                         palette,
                         return="model",
                         NumMax=1000,
                         col="rand",
                         seed=NULL,
                         norma=1,
                         plot=TRUE){

paleta=palette
  if(is.data.frame(paleta)){
    mat=NULL
    for(i in unique(paleta[,1])){
      m1=paleta[paleta[,1]==i,]
      m1=m1[sample(1:nrow(m1)),]
      m1=m1[(1:min(nrow(m1),NumMax)),]
      mat=rbind(mat,m1)

    }
    D=mat

    }


  if(!is.data.frame(paleta)){
    mat=NULL
    for(i in 1:length(paleta)){
      m=paleta[[i]]
      m1=linearize_image(m)[,-c(1,2)]
      m1=m1[sample(1:nrow(m1)),]
      m1=m1[(1:min(nrow(m1),NumMax)),]
      mat=rbind(mat,cbind(names(paleta)[i],m1))

    }
    D=mat
  }

    classes=unique(D[,1])
    ncluster=length(classes)
  if (col[1] == "rand") {
    COL = grDevices::rgb(red = runif(n = ncluster, min = 0,
                                     max = 1), green = runif(n = ncluster, min = 0,
                                                             max = 1), blue = runif(n = ncluster, min = 0,
                                                                                    max = 1))}
  if (col[1] != "rand") { COL = col}



  if (!is.null(seed)) {
    base::set.seed(seed)
  }

  if (norma != 1){D[,-1]=D[,-1]/norma}

    message(crayon::black("Ajustando o modelo ..."))

  ncluster=length(unique(D[,1]))
  model=randomForest::randomForest(x=D[,-1],y=as.factor(D[,1]))
  raster=((class(im)=="RasterBrick")|(class(im)=="raster"))
  if(return=="model"){return(model)}
  if(return=="image"){
    message(crayon::black("Obtendo os valores preditos..."))
    if ((norma != 1)&(raster==TRUE)) {im = im/norma}
    if ((norma != 1)&(raster==FALSE)) {im@.Data = im@.Data/norma}


    if (raster == TRUE) {
    im.values = raster::values(im)
    im.values=data.frame(im.values)
    colnames(im.values)=colnames(D[,-1])
    RF=predict(model,newdata=im.values)
    res = im[[1]]
    res = raster::setValues(res, RF)
    if (plot == TRUE) {
      raster::plot(res, col = COL)
    }
    return(res)
    }
    if (raster == FALSE) {


      ima = linearize_image(im)[, -c(1, 2)]
      RF=predict(model,newdata=ima)
      RF2=RF1=as.character(RF)

      for(i in 1:ncluster){
        RF2[RF1==classes[i]]=COL[i]
      }

      RF3=col2rgb(c(RF2))
      r=matrix(RF3[1,],ncol=ncol(im@.Data[,,1]))
      g=matrix(RF3[2,],ncol=ncol(im@.Data[,,2]))
      b=matrix(RF3[3,],ncol=ncol(im@.Data[,,3]))

      im2=im
      im2@.Data[,,1]=r
      im2@.Data[,,2]=g
      im2@.Data[,,3]=b

      if(mean(im2@.Data)>1){im2@.Data=im2@.Data/255}

      if (plot == TRUE) {
        plot_image(im2)
      }
      return(im2)
    }
  }

}
