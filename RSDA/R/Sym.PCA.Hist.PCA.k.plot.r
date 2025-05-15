#' Sym.PCA.Hist.PCA.k.plot
#' @author Jorge Arce Garro
#' @param data.sym.df Bins's projections
#' @param title.graph Plot title
#' @param concepts.name Concepts names
#' @param title.x Label of axis X
#' @param title.y Label of axis Y
#' @param pca.axes Principal Component
#'
#' @importFrom ggrepel geom_text_repel
#'
#' @return Concepts projected onto the Principal component chosen
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
#' ACER.p1<-Sym.PCA.Hist.PCA.k.plot(data.sym.df = pca.hist$Bins.df,
#'                                     title.graph = " ",
#'                                     concepts.name = c("ACER"),
#'                                     title.x = "First Principal Component (84.83%)",
#'                                     title.y = "Frequency",
#'                                     pca.axes = 1)
#'
#' ACER.p1
#' }
Sym.PCA.Hist.PCA.k.plot<-function(data.sym.df,
                            title.graph,
                            concepts.name,
                            title.x,
                            title.y,
                            pca.axes){
  df<-data.sym.df[data.sym.df$Principal.Component == pca.axes & data.sym.df$Object.Name %in% concepts.name, ]
  dim.df<-dim(df)
  df$Frequency.min<-rep(0,dim.df[1])
  colnames(df)
  sp <- ggplot(df, aes(x=Coord.Min, y=Frequency.min))+ geom_point()
  sp<- sp + labs(title = title.graph,x = title.x, y = title.y)
  sp <-sp +  geom_text_repel(aes(x=Coord.Min, y=Frequency.min,label = Id ,color = Object.Name))
  #sp <-sp +  geom_text_repel(aes(x=Coord.Max, y=Frequency.min,label = Id ,color = Id))

  sp<-sp + geom_rect(aes(xmin = Coord.Min, xmax = Coord.Max,
                         ymin = Frequency.min, ymax = Frequency,fill = Object.Name),
                     colour = "grey50",alpha = 1/5)

  sp<-sp+theme(legend.position="bottom",
               axis.text=element_text(size=9),
               axis.title=element_text(size=11)
  )
  return(sp)
}
