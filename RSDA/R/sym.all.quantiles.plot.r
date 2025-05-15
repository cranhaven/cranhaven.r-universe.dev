#' sym.all.quantiles.plot
#'
#' @author Jorge Arce Garro
#' @param quantiles.sym A quantile matrix
#' @param concept.names Concept Names
#' @param var.names Variables to plot
#' @param Title     Plot title
#' @param axes.x.label Label of axis X
#' @param axes.y.label Label of axis Y
#' @param label.name Concept Variable
#'
#' @importFrom plotly add_surface plot_ly layout add_markers
#' @return 3D Scatter Plot
#' @export
#'
#' @examples
#' \dontrun{
#'  data("hardwoodBrito")
#'  Hardwood.histogram<-hardwoodBrito
#'  Hardwood.cols<-colnames(Hardwood.histogram)
#'  Hardwood.names<-row.names(Hardwood.histogram)
#'  M<-length(Hardwood.cols)
#'  N<-length(Hardwood.names)
#'  BIN.Matrix<-matrix(rep(3,N*M),nrow = N)
#'  pca.hist<-sym.histogram.pca(Hardwood.histogram,BIN.Matrix)
#'  Hardwood.quantiles.PCA<-quantiles.RSDA(pca.hist$sym.hist.matrix.PCA,3)
#'  label.name<-"Hard Wood"
#'  Title<-"First Principal Plane"
#'  axes.x.label<- "First Principal Component (84.83%)"
#'  axes.y.label<- "Second Principal Component (9.70%)"
#'  concept.names<-c("ACER")
#'  var.names<-c("PC.1","PC.2")
#'
#'  concept.names<-row.names(Hardwood.quantiles.PCA)
#'  sym.all.quantiles.plot(Hardwood.quantiles.PCA,
#'                            concept.names,
#'                            var.names,
#'                            Title,
#'                            axes.x.label,
#'                            axes.y.label,
#'                            label.name)
#' }
sym.all.quantiles.plot<-function(quantiles.sym,
                            concept.names,
                            var.names,
                            Title,
                            axes.x.label,
                            axes.y.label,
                            label.name){
  indx.concepts<-which(row.names(quantiles.sym) %in% concept.names)
  indx.vars<-which(colnames(quantiles.sym) %in% var.names)
  quantiles.tmp<-quantiles.sym[indx.concepts,indx.vars]
  dim.tmp<-dim(quantiles.tmp)

  columns<-c("Concept.Name","Coord.Var.1","Coord.Var.2","Frequency")
  df.plot<- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(df.plot)<-columns
  for(i in 1:dim.tmp[1]){
    concept.act<-concept.names[i]
    breaks.dim.1<-quantiles.tmp[[1]][[i]]$breaks
    breaks.dim.2<-quantiles.tmp[[2]][[i]]$breaks
    props.dim.1<-quantiles.tmp[[1]][[i]]$props
    props.dim.2<-quantiles.tmp[[2]][[i]]$props
    max.dim.1<-max(breaks.dim.1)
    max.dim.2<-max(breaks.dim.2)
    length.data<-length(breaks.dim.2)

    for(k in 1:length.data){
      for(j in 1:length.data){
        df.plot.tmp<-data.frame(
          Concept.Name = concept.act,
          Coord.Var.1 = breaks.dim.1[k],
          Coord.Var.2 = breaks.dim.2[j],
          Frequency = props.dim.1[k]*props.dim.2[j]
        )
        df.plot<-rbind(df.plot,df.plot.tmp)
      }
    }

  }


  fig <- plot_ly(df.plot,
                  x = ~Coord.Var.1,
                  y = ~Coord.Var.2,
                 z = ~Frequency,
                 color = ~Concept.Name) %>% add_markers()

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


