#Este fichero se utilizará para hacer representaciones de mapas
#if (!require("sf")) install.packages("sf");require("sf")
#if (!require("ggplot2")) install.packages("ggplot2"); require("ggplot2")
globalVariables("variable")

#' @title Graphs. Representation on maps. Choropleth map
#'
#' @description With this function you can make choropleth maps, to carry out
#' representations of the data on a map. By default, the map used is that of Spain,
#' but any map of another country could be used too, as long as it is of shapefile type.
#' **NOTE:** The shapefile to be used can be downloaded from the following web page [https://github.com/Miguelro/Electoral/tree/master/Mapa](https://github.com/Miguelro/Electoral/tree/master/Mapa)
#' and all the files will be placed in a folder, which will serve to point out the
#' path to be indicated in the 'camino' parameter.
#'
#' @param dat It's a data.frame with two columns. The first contains, in the case
#' of Spain, the INE (Spanish National Institute of Statistics) code of the provinces,
#' and the second the value of the variable to be represented. **IMPORTANT NOTE:** It must
#' be taken special care with the fact that the first column is not of the factor type.
#' It is recommended to be character type, never use a factor.
#' @param camino It will be a character value to indicate where are located the files downloaded from
#' github containing the map data. The path must end with the character "/".
#' @param titulo The title to be displayed on the map will be specified here.
#' @param size_letra This is the font size that will be used to display the numerical
#' values on the map.
#' @param color_text It serves to indicate the color of the text that will be used to
#' display the data on the map.
#' @param ver_text It must be a Boolean value. By default it is TRUE, to
#' indicate that the data will be shown on the map. On the other hand, if this
#' parameter is set to FALSE the data values will not be displayed.
#'
#' @return Returns a ggplot2 object, containing the map to be represented
#'
#' @import sf
#' @import ggplot2
#'
#' @examples
#'
#' d <-data.frame(
#'cod=c(1,2,5,9,15,23,43,50),
#'da = c(3,5,7,10,1,3,80,4)
#')
#'suppressWarnings(mapa(d,system.file("maps/", package="Relectoral"),
#'                      titulo="An example map and some NA values",ver_text = TRUE))
#'
#'
#'@export
#'@md

mapa <-function(dat,camino,titulo,size_letra=3,color_text="brown",ver_text=TRUE){
  if(class(dat) != "data.frame") stop("The data provided in the parameter 'dat' must be contained in a data.frame")
  if(ncol(dat) != 2) stop("The data.frame 'dat' must have 2 columns")
  colnames(dat) <- c("Codigo","variable")
  # Lo formateo para tener un cero a la izquierda si hay sólo un dígito
  dat$Codigo <- sprintf("%02d",as.integer(dat$Codigo))

  nc<-st_read(paste0(camino,"/Provincias_ETRS89_30N.shx"))
  #Entresacamos los datos de las Islas Canarias
  canarias<-nc[as.character(nc$Cod_CCAA)=='05',]
  #Entresacamos los datos de la península
  Peninsula<-nc[!as.character(nc$Cod_CCAA)=='05',]

  nccanarias<-st_geometry(canarias)
  nccanarias<-nccanarias+c(1700000,700000)
  canarias$geometry<-nccanarias
  st_crs(canarias)<-st_crs(Peninsula)
  esp<-rbind(Peninsula,canarias)
  # Codigo es un factor lo paso a tipo character
  esp$Codigo <- as.character(esp$Codigo)
  esp <-merge(esp,dat,by=c("Codigo"),all.x=TRUE)


  p <- ggplot(data=esp)+geom_sf(aes(fill = variable))+
    ggtitle(titulo)+
    annotate(geom = "text", x = 950000, y = 4050000, label = "Islas Canarias",
             fontface = "italic", color = "grey22", size = size_letra)+
    coord_sf(xlim = c(-14094.75, 1184136.44),
             ylim = c(3832131.07, 4859239.69), expand = FALSE)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
        )+
    theme_void()+
    scale_fill_gradient(low = "#56B1F7", high = "#132B43",na.value="white")+ labs(fill = "")+
    theme(plot.title = element_text(hjust = 0.5),
          plot.margin = unit(c(0.3,0.3,0.5,0.3), "cm"))
  if(ver_text){
    p<-p+geom_sf_text(data=st_centroid(esp),aes(label=variable),size=size_letra,col=color_text)
  }
  #imprimo
  return(p)
}

# d <-data.frame(
#   cod=c(1,2,5,9,15,23,43,50),
#   da = c(3,5,7,10,1,3,80,4)
# )
#
# suppressWarnings(mapa(d,"F:/Elecciones/TFM/Mapa/",
#                       titulo="Un mapa de ejemplo, y valores NA",ver_text = TRUE))

