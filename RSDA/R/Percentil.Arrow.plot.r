#' Percentil.Arrow.plot
#' @author Jorge Arce Garro
#' @param quantiles.sym Matrix of Quantiles
#' @param concept.names Concept Names
#' @param var.names Variables to plot the arrows
#' @param Title Plot title
#' @param axes.x.label  Label of axis X
#' @param axes.y.label  Label of axis Y
#' @param label.name  Label
#' @importFrom ggplot2 ggplot geom_point geom_segment
#' @return Arrow Plot
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
#'  M<-length(Hardwood.cols)
#'  N<-length(Hardwood.names)
#'  BIN.Matrix<-matrix(rep(3,N*M),nrow = N)
#' label.name<-"Hard Wood"
#' Title<-"First Principal Plane"
#' axes.x.label<- "First Principal Component (84.83%)"
#' axes.y.label<- "Second Principal Component (9.70%)"
#' concept.names<-c("ACER")
#' var.names<-c("PC.1","PC.2")
#' quantile.ACER.plot<-Percentil.Arrow.plot(Hardwood.quantiles.PCA,
#'                                         concept.names,
#'                                         var.names,
#'                                         Title,
#'                                         axes.x.label,
#'                                         axes.y.label,
#'                                         label.name
#'                                         )
#' quantile.ACER.plot
#' }
Percentil.Arrow.plot<-function(quantiles.sym,
                               concept.names,
                               var.names,
                               Title,
                               axes.x.label,
                               axes.y.label,
                               label.name
                               ){
  indx.concepts<-which(row.names(quantiles.sym) %in% concept.names)
  indx.vars<-which(colnames(quantiles.sym) %in% var.names)
  quantiles.tmp<-quantiles.sym[indx.concepts,indx.vars]
  dim.tmp<-dim(quantiles.tmp)

  columns<-c("Concept.Name","Coord.Var.1","Coord.Var.2","Coord.Var.Next.1","Coord.Var.Next.2")
  df.plot<- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(df.plot)<-columns
  for(i in 1:dim.tmp[1]){
    concept.act<-concept.names[i]
    breaks.dim.1<-quantiles.tmp[[1]][[i]]$breaks
    breaks.dim.2<-quantiles.tmp[[2]][[i]]$breaks
    max.dim.1<-max(breaks.dim.1)
    max.dim.2<-max(breaks.dim.2)
    length.data<-length(breaks.dim.2)
    df.plot.tmp<-data.frame(
      Concept.Name = concept.act,
      Coord.Var.1 = breaks.dim.1,
      Coord.Var.2 = breaks.dim.2,
      Coord.Var.Next.1 = c(breaks.dim.1[-1],max.dim.1),
      Coord.Var.Next.2 = c(breaks.dim.2[-1],max.dim.2)
    )
    df.plot<-rbind(df.plot,df.plot.tmp)
  }

  with.out<-seq(length.data,length.data*dim.tmp[1],by = length.data)

  p <- ggplot(df.plot, aes(Coord.Var.1, Coord.Var.2)) + geom_point(aes(colour = Concept.Name))

  p<- p + geom_segment(aes(  x = Coord.Var.1,
                             y = Coord.Var.2,
                             xend = Coord.Var.Next.1,
                             yend = Coord.Var.Next.2,
                             color = Concept.Name),
                       data = df.plot[-with.out,],
                       arrow = arrow(length = unit(0.03, "npc"))
  )

  p<- p + labs(title = Title, y = axes.y.label, x = axes.x.label,colour = label.name)

  p<- p + geom_hline(yintercept=0,linetype="dashed", size=0.5)
  p<- p + geom_vline(xintercept =0,linetype="dashed", size=0.5)

  return(p)

}
