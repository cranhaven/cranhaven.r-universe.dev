#' Function to obtain measurements associated with objects in binary images
#' (Funcao para obter medidas associadas aos objetos em imagens binarias).
#'
#' @description Esta funcao possibilita a obtencao de medidas associadas aos
#'   objetos em imagens binarias.
#' @usage measure_image(img,noise=0,id=NULL,length= NULL,width =NULL, splitConnected=FALSE,
#' tolerance = 1, ext = 1,imOut=FALSE,  plot= TRUE)
#' @param img    :Este objeto deve ser obrigatoriamente uma matriz binaria,
#'   contendo os valores 0 (pixels do background) e 1 (pixels do foreground)).
#' @param noise    : E o numero de pixeis a partir do qual a funcao nao
#'   considerara como ruido.
#' @param id    :Se igual a NULL (default) nao sera feita a conversao de pixels
#'   para centimetros. Se houver algum objeto de referencia na imagem com area
#'   conhecida, deve-se colocar o numero referente a este objeto. Se o tamanho
#'   da imagem em centimetros for conhecida, pode-se colocar para este argumento
#'   a palavra "all".
#' @param length    :Comprimento do objeto de referencia ou da imagem em
#'   centimetros.
#' @param width    :Altura do objeto de referencia ou da imagem em centimetros.
#' @param splitConnected :Variavel do tipo logico. Se TRUE objetos encostados
#' serao considerados diferentes.
#' @param tolerance The minimum height of the object in the units of image
#'   intensity between its highest point (seed) and the point where it contacts
#'   another object (checked for every contact pixel). If the height is smaller
#'   than the tolerance, the object will be combined with one of its neighbors,
#'   which is the highest. Tolerance should be chosen according to the range of
#'   x. Default value is 1, which is a reasonable value if x comes from distmap.
#' @param ext Radius of the neighborhood in pixels for the detection of
#'   neighboring objects. Higher value smoothes out small objects.
#' @param imOut Logical variable, if TRUE, the segmentation image will be exported in the results.
#' (Variavel logica, se for TRUE sera exportada a imagem da segmentacao nos resultados).
#' @param  plot Indicates whether the segmented image will be displayed (TRUE) or not (FALSE) (default)
#' (Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'   imagem segmentada)
#' @return Returns the coordinates of each object, its area, perimeter, ...(Retorna as cordendas de cada objeto, sua area, perimetro, ...)
#' @seealso  \code{\link{segmentation_logit} , \link{segmentation}}

#' @examples
#'\donttest{
#'
#'############################################################################
#'#Obtendo o numero de ovos em uma folha
#'############################################################################
#'
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2))
#'##mostrar imagem
#'plot_image(im)

#'
#'#Selecionando o melhor indice para a segmentacao da folha
#'r=gray_scale(im,method = "r",plot=TRUE)
#'g=gray_scale(im,method = "g",plot=TRUE)
#'b=gray_scale(im,method = "b",plot=TRUE)
#'
#'
#'#O canal azul possibilita maior contraste
#'#O limiar pode ser um valor escolhido aleatoriamente (por exemplo: 0.6)
#'MatrizSegmentada=segmentation(b,threshold = 0.40,fillHull = TRUE,
#'selectHigher = FALSE,plot=TRUE)
#'
#'im2=extract_pixels(im,target =MatrizSegmentada,valueTarget =1,
#'valueSelect = c(0,0,0),plot=TRUE )
#'
#'
#'#Selecionando o melhor indice para a segmentacao dos ovos
#'r=gray_scale(im2,method = "r",plot=TRUE)
#'g=gray_scale(im2,method = "g",plot=TRUE)
#'b=gray_scale(im2,method = "b",plot=TRUE)
#'
#'#O canal Azul proporciona melhor segmentacao
#'#O limiar pode ser um valor escolhido aleatoriamente (por exemplo: 0.6)
#'MatrizSegmentada2=segmentation(b,threshold = 0.60,fillHull = TRUE,
#'selectHigher = TRUE,plot = TRUE)
#'
#'Medidas=measure_image(MatrizSegmentada2)
#'Medidas$ObjectNumber
#'
#'#Ver a mascara sobre os ovos na foto
#'im3=mask_pixels(im,MatrizSegmentada2==1,plot=TRUE)


#'
#'#############################################################
#' #Obtendo a area de folhas de acerola
#'##############################################################
#'
#'
#'#ativar pacote
#' library(ExpImage)
#' #Abrir imagem
#'im=read_image(example_image(3))
#'
#'
#'#Selecionando o melhor indice para a segmentacao
#' r=gray_scale(im,method ="r",plot=TRUE)
#' g=gray_scale(im,method = "g",plot=TRUE)
#' b=gray_scale(im,method ="b",plot=TRUE)
#'
#'
#'#A banda de azul foi a que melhor discriminou #O limiar pode ser um valor
#'#escolhido aleatoriamente (por exemplo: 0.6)
#'MatrizSegmentada=segmentation(b,threshold = 0.6,fillHull = FALSE,
#'selectHigher =FALSE,plot=TRUE)
#'
#'#O limiar tambem pode ser estabelecido pelo metodo de otsu
#'MatrizSegmentada2=segmentation(b,threshold = "otsu",fillHull = TRUE,
#'selectHigher =FALSE,plot=TRUE)
#'
#'#Obter medidas de cada objeto
#'medidas=measure_image(MatrizSegmentada2)
#'#ver o numero de objetos e medias medidas
#' medidas$ObjectNumber
#'
#'#Obter medidas de cada objeto excluindo o ruido
#'medidas=measure_image(MatrizSegmentada2,noise = 1000) #numero de objetos
#'medidas$ObjectNumber
#'Estimativas=medidas$measures
#'
#'#Plotar resultados das areas em pixel e salvar em um arquivo chamado "teste.jpg"
#'#plot_meansures(im,medidas$measures[,1],coordy=medidas$measures[,2],
#'#text=round(medidas$measures[,3],1),cex= 0.9,pathSave ="teste.jpg",
#'#col="blue" ,plot = TRUE)
#'
#'
#' plot_meansures(im,medidas$measures[,1],coordy=medidas$measures[,2],
#' text=round(medidas$measures[,3],2),cex = 0.9,col="blue" ,plot=TRUE)
#'##############################################################################
#'#Convertendo a area dos objetos para cm2
#'
#'#Conhecendo o identificador do objeto de referencia
#'
#' plot_meansures(im,medidas$measures[,1],coordy=medidas$measures[,2],
#' text=rownames(medidas$measures),cex= 0.9,
#' col="blue",plot=TRUE )

#'#como pode-se ver, o objeto de referencia e o de numero 30
#'# A area conhecida do objeto de referencia tem 8.5 x 5.5 cm.
#'#Isso nos leva a 46.75
#'medidas2=measure_image(MatrizSegmentada2,noise = 1000,id=30,
#'length= 8.5,width =5.5)
#'medidas2
#'#Apresentando a area foliar em cm2 de sobre cada uma das folhas
#'plot_meansures(im,medidas2$measures[,1],coordy=medidas2$measures[,2],
#'text=round(medidas2$measures[,3],2),cex = 0.9,col="blue")
#'
#'
#'################################################################
#'#Obs.: O uso do objeto de referencia e util para a conversao em cm2 em
#'#situacoes que nao se conhece a area fotografada.
#'#Se soubermos exatamente qual e o tamanho da area escaneada (fotografada)
#'#podemos dispensar o uso do objeto de referencia.
#'
#'#Convertendo a area em pixel para cm2 considerando a dimensao superficie
#' #escaneada.
#'# A dimensao da superficie escaneada tem 21*29.7 cm (dimensao de uma folha a4).
#'#Isso nos leva a  623.7 cm2
#'
#'medidas3=measure_image(MatrizSegmentada2,noise = 1000,id="all",
#'length= 21,width =29.7)
#'medidas3
#'#Apresentando a area foliar de sobre cada uma das folhas
#'plot_meansures(im,medidas3$measures[,1],coordy=medidas3$measures[,2],
#'text=round(medidas3$measures[,3],2),cex = 0.9,col="blue",plot=TRUE)
#'}
#'@export
# @exportS3Method print measure_image

measure_image=function(img,noise=0,id=NULL,length= NULL,
                       width =NULL,splitConnected=FALSE,tolerance=1, ext=1,
                       imOut=FALSE,
                       plot=TRUE){
  #ebimage()
  doParalell=FALSE
  NumberCores=3
  MatrizSegentada2=img
  MatrizSegentada3=EBImage::bwlabel(MatrizSegentada2)



  if(isTRUE(splitConnected)){
  MatrizSegentada4=EBImage::watershed(EBImage::distmap(MatrizSegentada3), tolerance=tolerance, ext=ext)
  }

  if(isFALSE(splitConnected)){
    MatrizSegentada4=MatrizSegentada3
  }


  Ta=table((MatrizSegentada4))
  M=max(MatrizSegentada4)
  res=table(MatrizSegentada4)[-1]

  ID=res>noise
  iddd=(0:M)[Ta>=noise]
  `%ni%` <- Negate(`%in%`)
  MatrizSegentada4[MatrizSegentada4 %ni% iddd]=0
  Output22=MatrizSegentada4




  r=runif(M,0,1) ; g=runif(M,0,1) ; b=runif(M,0,1)
  r=c(0,r); g=c(0,g); b=c(0,b)
  arr=array(c(r[c(MatrizSegentada4)+1],g[c(MatrizSegentada4)+1],b[c(MatrizSegentada4)+1]),
            dim = c(nrow(MatrizSegentada4),ncol(MatrizSegentada4),3))
  arr=EBImage::as.Image(arr)
  EBImage::colorMode(arr)=2


  if(plot==TRUE){plot_image(arr)}





  res=as.numeric(dimnames(res)$MatrizSegentada4)[ID]

  nn=length(unique(c(MatrizSegentada4)))-1
  RES=NULL
   if(isTRUE(splitConnected)){

     pb <- progress::progress_bar$new(total = nn)
     FFF=function(MatrizSegentada4,i,pb){
       pb$tick()
       coord=EBImage::computeFeatures.moment(MatrizSegentada4==i)
       Medidas=EBImage::computeFeatures.shape(MatrizSegentada4==i)
       r=c(coord[1:2],Medidas,coord[-c(1:2)])
       r
     }




     if(doParalell==T){

       # NumberCores=min(parallel::detectCores(),NumberCores)
       # cl <- parallel::makePSOCKcluster(NumberCores) # 6 cpu cores out of 8
       #
       # doParallel::registerDoParallel(cl)
       if(NumberCores=="all"){NumberCores=parallel::detectCores()}

       if (NumberCores > parallel::detectCores()) {
         message(paste0(" O numero de cores maximo (Maximum cores number): ", parallel::detectCores()))
         NumberCores = parallel::detectCores()
       }
       cl <- parallel::makeCluster(NumberCores)
       parallel::clusterExport(cl,
                     varlist = c("FFF","MatrizSegentada4","pb"),
                     envir=environment())
       on.exit(stopCluster(cl))

       r <- parallel::parLapply(cl = cl,res, function(i){FFF(MatrizSegentada4,i,pb)})


     }

     if(doParalell==F){


       coord=EBImage::computeFeatures.moment(MatrizSegentada4)
       Medidas=EBImage::computeFeatures.shape(MatrizSegentada4)

     RES=cbind(coord[,1:2],Medidas,coord[,3:5])

      # colnames(RES)=c( "x"    ,  "y" ,"area" ,"perimeter", "radius.mean"
           #             ,"radius.sd", "radius.min", "radius.max","majoraxis" ,"eccentricity"  ,   "theta")



}


     }




  if(isFALSE(splitConnected)){
    coord=EBImage::computeFeatures.moment( EBImage::bwlabel(MatrizSegentada4!=0))
    Medidas=EBImage::computeFeatures.shape( EBImage::bwlabel(MatrizSegentada4!=0))
    ID=Medidas[,1]>noise
    if(nrow(coord)==1){RES=(c(coord[,1:2],Medidas,coord[,-c(1:2)]))}
    if(nrow(coord)>1){RES=(cbind(coord[,1:2],Medidas,coord[,-c(1:2)]))
    RES=as.matrix(RES[ID,])}




  }


     if(is.matrix(RES)){
  colnames(RES)=c( "x"    ,  "y" ,"area" ,"perimeter", "radius.mean"
                   ,"radius.sd", "radius.min", "radius.max","majoraxis" ,"eccentricity"  ,   "theta")
  rownames(RES)=1:nrow(RES)


  }

  if(!is.matrix(RES)){
    names(RES)=c( "x"    ,  "y" ,"area" ,"perimeter", "radius.mean"
                  ,"radius.sd", "radius.min", "radius.max","majoraxis" ,"eccentricity"  ,   "theta")



  }



RES=RES2=round(RES,3)


  if(is.matrix(id)){
    id2=id
    Area=length*width
    RES=RES2
    if(nrow(RES)>1){RES[,3]= RES[,3]*Area/sum(id2)}
    if(nrow(RES)==1){RES[3]= RES[3]*Area/sum(id2)}
    perim=EBImage::bwlabel(id)
    perim2=EBImage::computeFeatures.shape(perim)

    if(nrow(perim2)>1){
      perim3=perim2[perim[,1]==max(perim[,1]),2]
    }

    if(nrow(perim2)==1){
      perim3=perim2[2]
    }

    RES[,4:9]= RES[,4:9]*((length+width)*2 )/ perim3


  }
  if( EBImage::is.Image(id)){
    id2=id@.Data
    Area=length*width
    RES=RES2
    RES[,3]= RES[,3]*Area/sum(id2)

    perim=EBImage::bwlabel(perim)
    perim=EBImage::computeFeatures.shape(perim)
    perim2=perim[perim[,1]==max(perim[,1]),4]

    RES[,4:9]= RES[,4:9]*((length+width)*2 )/ perim2


  }
  if((is.matrix(id)+ EBImage::is.Image(id))==0){

    if(isFALSE(is.null(id))){

      if(id!="all"){
        # length= 8.5
        #width =5.5
        Area=length*width

        RES[,3]= RES[,3]*Area/RES[id,3]



        RES[,4:9]= RES[,4:9]*((length+width)*2 )/ RES[id,4]
      }


      if(id=="all"){

 Area=length*width
   RES[,3]= RES[,3]*Area/(nrow(MatrizSegentada2)*ncol(MatrizSegentada2))
  # RES[id,4:9]* (nrow(MatrizSegentada2)*ncol(MatrizSegentada2))/ ((length+width)*2 )
RES[,4:9]= RES[,4:9]*((length+width)*2 )/ ((nrow(MatrizSegentada2)+ncol(MatrizSegentada2)) *2)


      }

    }
  }


  #Verificando numero de objetos
  ObjectNumber=nrow(RES)
  if(imOut==TRUE){ LIST=list(ObjectNumber=ObjectNumber,measures=RES,imOut=Output22)}
  if(imOut==FALSE){ LIST=list(ObjectNumber=ObjectNumber,measures=RES)}
class(LIST)="measurements"
  return(LIST)

}


