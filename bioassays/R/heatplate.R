#' @name heatplate
#'
#' @aliases heatplate
#'
#' @title Heatmap of multi well plate
#'
#' @description This function generate a heatmap (for numeric vector) or categorical plot (character vector) of multi well plate
#'
#' @usage heatplate(datamatrix, name, size = 7.5)
#'
#' @param datamatrix data in matrix format. An easy way to create this is by calling \code{\link{matrix96}}
#'
#' @param name name to be given for the heatmap
#'
#' @param size plot size for each well in the heatmap (default is 7.5)
#'
#' @details Heat map can be generated for any multi well plate data in matrix format (datamatrix). The columns and rows of datamatrix should be labelled appropriately using \code{\link{matrix96}}. A heatplot is generated if datamatrix is numeric, but a categorical plot is generated if datamatrix is a character matrix.
#'
#' @return A graphical plot.
#'
#' @author A.A Palakkan
#'
#' @examples
#' ## loading data
#' data(metafile96, rawdata96,rawdata384)
#' rawdata96 <- data2plateformat(rawdata96,platetype = 96)
#' rawdata384 <- data2plateformat(rawdata384,platetype = 384)
#'
#' ## eg:1 heat map of rawdata96
#' data<-matrix96(plate2df(rawdata96),"value")
#' heatplate(data,"Plate 1", size=5)
#'
#' ## eg:2 heat map of rawdata96 can also be called as
#' heatplate(as.matrix(rawdata96),"Plate 1", size=5)
#'
#' ## eg:3 heat map of rawdata384
#' heatplate(as.matrix(rawdata384),"Plate 1", size=2)
#'
#' ## eg:4 catagorical map of metafile96 (column:id)
#' data<-matrix96(metafile96,"id")
#' heatplate(data,"Plate 1", size=5)
#'
#' @keywords hplot
#'
#'@importFrom ggplot2 ggplot aes geom_point theme coord_fixed element_blank scale_color_distiller guides scale_y_reverse scale_x_continuous ggtitle
#'@importFrom reshape2 melt
#'@importFrom rlang sym
#'@export
#'
#'
#'

heatplate <- function(datamatrix,name,size=7.5){

## Initial setup ##
Column <- Row <- Var1 <- Var2 <- value<- NULL
data<-reshape2::melt(datamatrix)
names(data)[names(data)=="Var1"|names(data)=="X1"]<-"Row"
names(data)[names(data)=="Var2"|names(data)=="X2"]<-"Column"
data$Row<-as.numeric(match(toupper(data$Row),LETTERS))
n1<-size+1
n2<-size

qnorm <- function (x){x/max(x)}

## For heatmap ##

if(is.numeric(data[,"value"])=="TRUE"){

  r1<-(ncol(datamatrix)+1)/ncol(datamatrix)
  r2<-(nrow(datamatrix)+1)/nrow(datamatrix)

  data$value<-round(qnorm(data$value),2)

  plot<-ggplot2::ggplot(data=data, ggplot2::aes(x=Column, y=Row))+
        ggplot2::geom_point(data=expand.grid(seq(1,ncol(datamatrix)),
                                             seq(1,nrow(datamatrix))),
        ggplot2::aes(x=Var1, y=Var2),color="black",shape=21, size=n1)+
        ggplot2::coord_fixed(ratio= r1/r2,
                             xlim=c(0.8, (ncol(datamatrix)+0.2)),
                             ylim=c((nrow(datamatrix)+0.2),0.8))+
        ggplot2::scale_y_reverse(breaks=seq(1,nrow(datamatrix)),
                                 labels = LETTERS[seq(1,nrow(datamatrix),1)])+
        ggplot2::scale_x_continuous(breaks=seq(1,ncol(datamatrix)))+
        ggplot2::ggtitle(paste0(rlang::sym(name)))+
        ggplot2::theme(axis.title = ggplot2::element_blank())+
        ggplot2::theme(legend.title = ggplot2::element_blank())
}

if(is.numeric(data[,"value"])=="TRUE"){
  new <- plot + ggplot2::geom_point(ggplot2::aes(colour=value),size =n2)

  new <- new + ggplot2::scale_color_distiller(type = "seq",palette = 8, direction = 1)+
               ggplot2::guides("coloursteps",aesthetics = "colour")
}

## For Catagorical plot ##

if(is.numeric(data[,"value"])=="FALSE"){

  r3 <- (ncol(datamatrix)+1)/ncol(datamatrix)
  r4 <- (nrow(datamatrix)+1)/nrow(datamatrix)


  plot<-ggplot2::ggplot(data=data, ggplot2::aes(x=Column, y=Row))+
        ggplot2::geom_point(data=expand.grid(seq(1,ncol(datamatrix)),
                            seq(1,nrow(datamatrix))),
                            ggplot2::aes(x=Var1, y=Var2),
                            color="black",
                            shape=21,
                            size=n1-1)+
        ggplot2::coord_fixed(ratio= r3/r4,
                             xlim=c(0.8, (ncol(datamatrix)+0.2)),
                             ylim=c((nrow(datamatrix)+0.4),0.8))+
        ggplot2::scale_y_reverse(breaks=seq(1,nrow(datamatrix)),
                                 labels = LETTERS[seq(1,nrow(datamatrix),1)])+
        ggplot2::scale_x_continuous(breaks=seq(1,ncol(datamatrix)))+
        ggplot2::ggtitle(paste0(rlang::sym(name)))+
        ggplot2::theme(axis.title = ggplot2::element_blank())+
        ggplot2::theme(legend.title = ggplot2::element_blank())

  new <-plot + ggplot2::geom_point(ggplot2::aes(colour=factor(value)),size =(n1-2))
}


return(new)

}

