#'Esta funcao plota imagens. (This function plot the images.)
#'@description Esta funcao permite plotar a imagem (This function allows you to
#'  view the image).
#' @usage plot_image(im,r=1,g=2,b=3,band=NULL,col=0,normalize=FALSE,axis=FALSE,
#'        lim=NULL,title=NULL,flip=TRUE,flop=FALSE,rotate=0,norm=1,
#'        rasterRGB=FALSE)
#' @param im Este objeto deve conter uma imagem no formato do EBImage ou na forma
#'de uma matriz, no caso de imagem em escala de cinza (This
#'  object must contain an image in EBImage format).
#'@param r Indica o canal correspondente a cor vermelha para imagens com
#'   extensao '.tif'. O defaut e 1. (Indicates the channel corresponding to
#'   red color for images with the extension '.tif'. The default is 1.)
#'@param g Indica o canal correspondente a cor verde para imagens com
#'   extensao '.tif'. O defaut e 2. (Indicates the channel corresponding to
#'   green color for images with the extension '.tif'. The default is 2.)
#'@param b Indica o canal correspondente a cor azul para imagens com
#'   extensao '.tif'. O defaut e 3. (Indicates the channel corresponding to
#'   blue color for images with the extension '.tif'. The default is 3.)
#'@param band Indica a banda que se deseja plotar. Neste caso nao se precisa
#'considerar as bandas de R, G e B simultaneamente.
#'@param col Pode ser um valor numerico variando entre 0 e 8 ou uma paleta de cores
#'obtida pela funcao `colorRampPalette`. Se for 0 sera considerada a representacao
#'da imagem monocromatica em escala de cinza. Valores entre 1 e 6 indicam outras
#'paletas de cores para a representacao. (It can be a numerical value ranging from
#'0 to 6 or a color palette 'obtained by the `colorRampPalette` function. If it is
#'0, the representation of the monochromatic image in gray scale will be considered.
#' Values between 1 and 6 indicate other color palettes for the representation. )
#'@param normalize Se for TRUE a imagem sera normalizada para que os valores dos pixels
#' variem entre 0 e 1 (If TRUE, the image will be normalized so that the pixel values
#'  vary between 0 and 1).
#'@param axis Se for FALSE nao aparecera os eixos no grafico (If it is FALSE, the axes will
#'not appear in the graph)
#'@param lim Vetor contendo quatro valores correspondentes aos limites que apareceral nos eixos do grafico
#' (Vector containing four values corresponding to the limits that will appear on the graph axes)
#'@param title Titulo do grafico (chart title).
#'@param flip Inverter a imagem (invert the image)
#'@param flop Inverter a imagem (invert the image)
#'@param rotate Valor numerico com o angulo para a rotacao da imagem
#'(Numeric value with angle for image rotation)
#'@param norm Numero pelo qual os valores dos pixels deverao ser divididos para variar entre 0 e 1
#'(Number by which pixel values should be divided to vary between 0 and 1).
#'@param rasterRGB Se a imagem for em TIF com uma unica banda deve ser FALSE.
#' Se houver 3 ou mais bandas deve ser TRUE (If the image is in TIF with a
#' single band it must be FALSE. If there are 3 or more bands it must be TRUE).
#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Apresenta a imagem contida no objeto im.
#'@seealso  \code{\link{read_image}}
#'@importFrom stats binomial glm predict
#'@importFrom grDevices dev.off  jpeg recordPlot
#'@importFrom graphics points
#'@importFrom raster plotRGB  stack raster
#'@importFrom parallel detectCores
#'@importFrom doParallel registerDoParallel
#'@importFrom foreach %dopar% foreach
#'@export
#' @examples
#'
#'#library(ExpImage)
#'#library(EBImage)
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2),plot=FALSE)
#' im=resize_image(im,w = 300,plot = FALSE)
#'plot_image(im)
#'
#'#Representado imagens em escalas de cinza
#'im2 = gray_scale(im,"r")
#'plot_image(im2)
#'#Utilizando uma paleta de cores para a melhor visualizacao
#'plot_image(im2,col=2)
#'plot_image(im2,col=3,axis=TRUE)
#'
#'
#'
#'
#'\donttest{
#'
#'#########################################################
#'#########################################################
#'#' #' ########################################################
#' ###' Abrindo o endereco de bandas de imagens de satelite
#' ########################################################
#' end1=example_image(14) #Banda de azul
#' end2=example_image(15) #Banda de verde
#' end3=example_image(16) #Banda de vermelho
#' end4=example_image(17) #Banda de IR
#' end5=example_image(18) #Banda de SWIR
#'
#' ########################################################
#' ###' Abrindo bandas de imagens de satelite
#' ########################################################
#' B1=read_image(end1,plot=TRUE)
#' B2=read_image(end2,plot=TRUE)
#' B3=read_image(end3,plot=TRUE)
#' B4=read_image(end4,plot=TRUE)
#' B5=read_image(end5,plot=TRUE)
#'
#' ########################################################
#' ###' Juntando as bandas em uma imagem
#' ########################################################
#' im=join_bands(imgs = list(B1,B2,B3,B4,B5))
#' tif_info(im)

#' plot_image(im,r=3,g=2,b=1,rasterRGB=TRUE)
#' }





plot_image=function(im,r=1,g=2,b=3,band=NULL,col=0,normalize=FALSE,axis=FALSE,lim=NULL,title=NULL,flip=TRUE,flop=FALSE,rotate=0,norm=1,rasterRGB=FALSE){
classe=base::class(im)
titulo=NULL
raster=FALSE
  if(length(classe)>1) classe=classe[1]

if((classe=="RasterStack")|(classe=="RasterBrick")|(classe=="RasterLayer")){ raster=TRUE
  if(col==0) col=3
  }


  if(rasterRGB==FALSE){
if(raster==FALSE){
  im=EBImage::as.Image(im)
  Rows=Cols=Value=0

  info=info_image(im)
  n=info$Length[1]
 if(n>700) {im=resize_image(im,w=700)}
  if(normalize==TRUE) {normalize_image(im)}

  if(is.null(class(im)[1])){
  class(im)="aaa"
}

  if(class(im)[1]=="RasterStack"){
    if(is.null(band)){plotRGB(im,r=r,g=g,b=b)}
    if(!is.null(band)){im=EBImage::as.Image(im@.Data[,,band])}
    }

  if(class(im)[1]!="RasterStack"){
    if(!is.null(band)){im=EBImage::as.Image(im@.Data[,,band])
    im@colormode=as.integer(0)}

  if(EBImage::is.Image(im)|is.matrix(im)){
    #if(EBImage::is.Image(im)){im=im@.Data}


   if(is.matrix(im)) {im=EBImage::as.Image(im)}

    if( (im@colormode)>0){
      im2=EBImage::as.Image(im@.Data[,,c(r,g,b)])
      im2@colormode<-as.integer(2)
#raster::stack(im2)

      if((axis==FALSE)&(is.null(title))){plot(im2)}
      if((axis!=FALSE)|(!is.null(title))){

       R=raster::raster(im2@.Data[,,r])
       G=raster::raster(im2@.Data[,,g])
       B=raster::raster(im2@.Data[,,b])

       im3=raster::stack(R,G,B)


       plot(im3)

      }





    }

    if((im@colormode)==0){
      im2=EBImage::as.Image(im)
      if(col==0){
        plot(im2)
      }
      if(col>0){
im=EBImage::as.Image(im)

if(flip==TRUE) im=EBImage::flip(im)
if(flop==TRUE) im=EBImage::flop(im)
if(rotate>0) im=EBImage::rotate(im,angle=rotate)
im=as.matrix(im@.Data)

paleta=colorRamp_Palette(col)

imm=linearize_image(im)

if(is.list(lim)){lim=lim$Extent}

        IM=data.frame(Rows=imm[,1],Cols=imm[,2],Value=imm[,3])

        if(!is.null(lim)){
          IM[,1]=Normatiza(matrix(IM[,1],ncol=1),LimiteInferior = lim[1],LimiteSuperior   = lim[2],Metodo = 2)
          IM[,2]=Normatiza(matrix(IM[,2],ncol=1),LimiteInferior = lim[3],LimiteSuperior = lim[4],Metodo = 2)
        }




        if(axis==FALSE){

       PLOT= ggplot2::ggplot( data=IM,
                         ggplot2::aes( x=Rows, y=Cols, fill=Value)) +
          ggplot2::geom_raster(interpolate= TRUE)  +
          ggplot2::scale_fill_gradientn(colours = paleta(100),na.value = "white")    +
          ggplot2::theme_void()
       print(PLOT)
        }

        if(axis==TRUE){
          PLOT= ggplot2::ggplot( data=IM,
                           ggplot2::aes( x=Rows, y=Cols, fill=Value)) +
            ggplot2::geom_raster(interpolate= TRUE)  +
            ggplot2::scale_fill_gradientn(colours = paleta(100),na.value = "white")    +
            ggplot2::ggtitle(title)+
            ggplot2::xlab("")+ggplot2::ylab("")+
          ggplot2::theme_classic()+
           ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
          print(PLOT)
        }

      }
    }

  }
  }
}
if(raster==TRUE){
  bands=dim(im)[3]
  if(bands==1){ raster::plot(im,col=colorRamp_Palette(col)(50),main=title) }
  if(bands>1){
    if(is.null(band)) {band=1:bands}
    if(length(title[band])>1) titulo=title[band]
    if(length(title[band])==1) titulo=title
    raster::plot(im[[band]],col=colorRamp_Palette(col)(50),main=titulo) }
  }
}


  if(rasterRGB==TRUE){
    im2=im
    if(norm!=1) im2=im/norm
    raster::plotRGB(im2,stretch="lin",r=r,g=g,b=b,main=title,axes=axis)
  }
}



colorRamp_Palette=function(m=1){
  if(m==1){ col = colorRampPalette(c('white', 'cyan', '#007FFF', 'blue','#00007F'))}

  if(m==2){col = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', 'white',
                                    'cyan', '#007FFF', 'blue','#00007F'))}


  if(m==3){col= colorRampPalette(c('#67001F', '#B2182B', '#D6604D', '#F4A582',
                                   '#FDDBC7', '#FFFFFF', '#D1E5F0', '#92C5DE',
                                   '#4393C3', '#2166AC', '#053061'))}

  if(m==4){col = colorRampPalette(c('red', 'white', 'blue')) }

  if(m==5){col = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', '#7FFF7F',
                                    'cyan', '#007FFF', 'blue', '#00007F')) }

  if(m==6){col = colorRampPalette(c("#669900","yellow","#FF5500")) }
  if(m==7){col = colorRampPalette(c("black","white")) }
  if(m==8){col = colorRampPalette(c("brown","red","orange","yellow","#5E715B","#455A52","#254620")) }

  return(col)

}

