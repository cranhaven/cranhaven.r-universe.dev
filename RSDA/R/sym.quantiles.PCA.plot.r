#' sym.quantiles.PCA.plot
#'
#' @author Jorge Arce Garro
#' @name sym.quantiles.PCA.plot
#' @aliases sym.quantiles.PCA.plot
#'
#' @param histogram.PCA.r A quantil matrix
#' @param concept.names Concept Name
#' @param var.names     Variables to plot
#' @param Title         Plot title
#' @param axes.x.label Label of axis X
#' @param axes.y.label Label of axis Y
#' @param label.name   Concept Variable
#' @importFrom plotly add_surface plot_ly layout
#' @return 3D plot
#' @export
#'
#' @examples
#' \dontrun{
#' data("hardwoodBrito")
#' Hardwood.histogram<-hardwoodBrito
#' Hardwood.cols<-colnames(Hardwood.histogram)
#' Hardwood.names<-row.names(Hardwood.histogram)
#'  M<-length(Hardwood.cols)
#'  N<-length(Hardwood.names)
#'  BIN.Matrix<-matrix(rep(3,N*M),nrow = N)
#' pca.hist<-sym.histogram.pca(Hardwood.histogram,BIN.Matrix)
#' Hardwood.quantiles.PCA<-quantiles.RSDA(pca.hist$sym.hist.matrix.PCA,3)
#' label.name<-"Hard Wood"
#' Title<-"First Principal Plane"
#' axes.x.label<- "PC 1 (84.83%)"
#' axes.y.label<- "PC 2 (9.70%)"
#' concept.names<-c("ACER")
#' var.names<-c("PC.1","PC.2")

#' plot.3D.HW<-sym.quantiles.PCA.plot(Hardwood.quantiles.PCA,
#'                                      concept.names,
#'                                      var.names,
#'                                      Title,
#'                                      axes.x.label,
#'                                      axes.y.label,
#'                                      label.name)
#'
#' plot.3D.HW
#' }
#' @rdname sym.quantiles.PCA.plot
sym.quantiles.PCA.plot<-function(histogram.PCA.r,
                             concept.names,
                             var.names,
                             Title,
                             axes.x.label,
                             axes.y.label,
                             label.name){
  indx.concepts<-which(row.names(histogram.PCA.r) %in% concept.names)
  indx.vars<-which(colnames(histogram.PCA.r) %in% var.names)
  histogram.PCA.r.tmp<-histogram.PCA.r[indx.concepts,indx.vars]
  dim.tmp<-dim(histogram.PCA.r.tmp)

  matrix.freq<-matrix(0,nrow = 0, ncol = dim.tmp[1])
  df.matrix.freq<-as.data.frame(matrix(0,nrow = 0, ncol = dim.tmp[1]))

  for(i in 1:dim.tmp[1]){
    concept.act<-concept.names[i]
    data.tmp.1<-data.frame(
                dim.1 = histogram.PCA.r.tmp[[1]][[i]]$breaks,
                props.1 = histogram.PCA.r.tmp[[1]][[i]]$props
    )

    data.tmp.2<-data.frame(
      dim.2 = histogram.PCA.r.tmp[[2]][[i]]$breaks,
      props.2 = histogram.PCA.r.tmp[[2]][[i]]$props
    )

    dim.tmp.2<-dim(data.tmp.1)
    matrix.freq<-matrix(0,nrow = dim.tmp.2[1], ncol = dim.tmp.2[1])
    for(j in 1:dim.tmp.2[1]){
      for(k in 1:dim.tmp.2[1]){
        matrix.freq[j,k]<-data.tmp.1[j,2]*data.tmp.2[k,2]
      }
    }

    df.matrix.freq<-rbind(df.matrix.freq,as.data.frame(matrix.freq))
  }

  fig <- plot_ly(x = data.tmp.1$dim.1,
                 y = data.tmp.2$dim.2,
                 z = as.matrix(df.matrix.freq)) %>% add_surface()

  axx <- list(
    title = axes.x.label
  )

  axy <- list(
    title = axes.y.label
  )

  axz <- list(
    title = "Frequency"
  )

  fig<- fig %>% layout(scene = list(xaxis= axx,
                                    yaxis= axy,
                                    zaxis= axz ))
  return(fig)
}
