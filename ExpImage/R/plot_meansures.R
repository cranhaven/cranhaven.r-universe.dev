#' Funcao para sobrepor informacoes sobre os objetos da imagem
#'
#' @description Esta funcao possibilita sobrepor informacoes sobre os objetos da
#'   imagem
#' @usage
#' plot_meansures(img,coordx=NULL,coordy=NULL,text=NULL,measurements=NULL,
#' variable=NULL,pch=NULL,col="red",cex=1,pathSave="none",plot=F)
#'
#' @param img    :Este objeto deve conter uma imagem no formato do EBImage.
#' @param coordx    : deve ser um vetor com as coordenadas do eixo x dos
#'   objetos.
#' @param coordy    : deve ser um vetor com as coordenadas do eixo y dos
#'   objetos.
#' @param text    : deve ser um vetor com as informacoes (numeros ou texto) a
#'   serem sobrepostos em cada objeto.
#' @param measurements : Objeto obtido pela funcao `measure_image`.
#' @param variable : Pode ser um nome associado a uma das variaveis estimadas pela
#' funcao `measure_image`:
#' \itemize{ \item "id" = Identificacao dos objetos.
#'  \item "area" = Area dos objetos.
#'   \item "perimeter" = Perimetro dos objetos.
#'    \item "radius.mean" = Raio medio.
#'     \item "radius.sd" = Desvio padrao dos objetos.
#'      \item "radius.min" = Raio minimo dos objetos.
#'       \item "radius.max" = Raio maximo dos objetos.
#'        \item "major.axis" = Maior eixo dos objetos.
#'         \item "eccentricity" = Excentrecidade dos objetos.
#'          \item "theta" = Angulo theta dos objetos.
#'          }

#' @param pch : Podem ser valores numericos indicando diferentes simbolos.
#' @param col    : E a cor do texto que pretende-se colocar sobre a imagem
#' @param cex    : E o tamanho do texto que pretende-se colocar sobre a imagem
#' @param pathSave    : Se tiver preenchido por "none" nao sera salva a imagem
#'
#'
#'   resultante (default). Alternativamente, basta colocar o nome de um objeto
#'   (com extensao .jpg) que a imagem sera salva na pasta de trabalho.
#' @param plot    :Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'   imagem segmentada.

#' @return Retorna uma imagem com um texto sobreposto a cada objeto na imagem
#' @seealso  \code{\link{segmentation_logit}}

#' @examples
#' \donttest{
#' ####################################################################################
#' #Estimar a area foliar usando um objeto de referencia.
#' ###################################################################################
#'   #ativar pacote
#'   #library(EBImage)
#'   #library(ExpImage)
#'   #######################################################
#'   #Abrir imagem das folhas
#'   im=read_image(example_image(3))
#'   plot_image(im)
#'   #Abrir paleta de cores do fundo
#'   fundo=read_image(example_image(4))
#'   plot_image(fundo)
#'   #Abrir paleta de cores das folhas
#'   folhas=read_image(example_image(5))
#'   plot_image(folhas)
#'   #Abrir paleta de cores referencia
#'   ref=read_image(example_image(6))
#'   #Ver a imagem
#'   plot_image(ref)
#'
#'   #################################################################
#'   #Segmentacao para separar as folhas do restante
#'   folhas.seg=segmentation_logit(im,foreground=folhas,background=list(fundo,ref),
#'   sample=2000,fillHull=TRUE,plot=TRUE)
#'
#'   #Segmentacao para separar o objeto de referencia do restante
#'   ref.seg=segmentation_logit(im,foreground=ref,background=list(fundo,folhas),
#'   sample=2000,fillHull=TRUE,plot=TRUE)
#'
#'   #Identificar area de cada folha
#'
#'   medidas=measure_image(folhas.seg,noise = 1000)
#'   #numero de objetos e medias
#'   medidas
#'
#'   #Plotar resultados das areas em pixel e salvar em imagem jpg
#'   #plot_meansures(im,medidas$measures[,1],coordy=medidas$measures[,2],
#'   #text=round(medidas$measures[,3],1),col="blue",cex = 0.9,
#'   #pathSave ="teste.jpg" ,plot=TRUE)
#'

#'
#'  ##############################################################################
#'  ######################################################################
#'  #Convertendo a area dos objetos para cm2

#'  #Identificando a area do objeto de referencia (maior area)
#'  # A area conhecida do objeto de referencia tem 8.5 x 5.5 cm e
#'  #sua area segmentada esta no objeto ref.seg
#'
#'  medidasref=measure_image(img = folhas.seg,noise =1000,id=ref.seg,length =8.5,width =5.5 )
#' #numero de objetos e medias
#'  medidasref
#'

#'  #Apresentando a area foliar de sobre cada uma das folhas
#'  plot_meansures(im,medidasref$measures[,1],coordy=medidasref$measures[,2],
#'  text=round(medidasref$measures[,3],2),cex = 0.9,col="blue")
#'  }
#'@export



plot_meansures=function(img,
                        coordx=NULL,coordy=NULL,text=NULL,measurements=NULL,
                        variable=NULL,pch=NULL,
                        col="red",cex=1,pathSave="none",plot=F){

if(!is.null(measurements)){
   class(measurements)="measurements"
  coordx=matrix(measurements$measures,ncol=11)[,1]
  coordy=matrix(measurements$measures,ncol=11)[,2]
  if(!is.null(variable)){
  if(variable!="id"){text2=measurements$measures[,variable]}
  if(variable=="id"){text2=rownames(measurements$measures)}
}
}

  if(!is.null(text)){text2=text}


  plot_image(EBImage::as.Image(img))
  if(is.null(text)&is.null(variable)){
    points(coordx,coordy,pch=pch,col=col,cex=cex)
  }

  if(!is.null(text)|!is.null(variable)){text(coordx,coordy,text2,col=col,cex=cex)}

  if(pathSave!="none"){
    jpeg(pathSave)
    plot( EBImage::as.Image(img))
    text(coordx,coordy,text,col=col,cex=cex)
    dev.off()

    if(plot==T){
    plot_image(EBImage::as.Image(img))
    text(coordx,coordy,text,col=col,cex=cex)
}


  }


}
