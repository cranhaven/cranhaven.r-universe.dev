
#' @title plotting hscore of each bi-cluster on bicluster dimension
#' @description funcc_show_bicluster_hscore graphically shows the hscore vs the dimension (i.e. number of rows and columns) of each bi-cluster 
#' @export
#' @param fun_mat The data array (n x m x T) where each entry corresponds to the measure of one observation i, i=1,...,n, for a functional variable m, m=1,...,p, at point t, t=1,...,T
#' @param res_input An object produced by the funcc_biclust function
#' @return a figure representing the dimensions of each bi-cluster (i.e. number of rows and columns)
#' @examples  
#' data("funCCdata")
#' res <- funcc_biclust(funCCdata,delta=10,theta=1,alpha=1,beta=0,const_alpha=TRUE)
#' funcc_show_bicluster_hscore(funCCdata,res)
#' 
funcc_show_bicluster_hscore <- function(fun_mat,res_input){
  
  biclust_n <- NULL
  
  col_palette = c(RColorBrewer::brewer.pal(9, 'Set1'),
                  RColorBrewer::brewer.pal(12, 'Set3'),
                  RColorBrewer::brewer.pal(8, 'Set2'),
                  RColorBrewer::brewer.pal(8, 'Accent'),
                  
                  RColorBrewer::brewer.pal(8, 'Dark2'),
                  RColorBrewer::brewer.pal(9, 'PiYG'),
                  RColorBrewer::brewer.pal(9, 'PuOr'),
                  RColorBrewer::brewer.pal(9, 'RdBu'),
                  RColorBrewer::brewer.pal(9, 'PRGn'),
                  RColorBrewer::brewer.pal(9, 'Pastel1'),
                  RColorBrewer::brewer.pal(8, 'Pastel2'),
                  RColorBrewer::brewer.pal(9, 'BrBG'),
                  RColorBrewer::brewer.pal(11, 'Spectral'),
                  RColorBrewer::brewer.pal(8,'RdGy'),
                  RColorBrewer::brewer.pal(8,'RdBu'),
                  RColorBrewer::brewer.pal(8,'PRGn'),
                  RColorBrewer::brewer.pal(8,'PiYG'),
                  RColorBrewer::brewer.pal(8,'Oranges'),
                  RColorBrewer::brewer.pal(8,'Blues'),
                  RColorBrewer::brewer.pal(8,'RdGy'),
                  RColorBrewer::brewer.pal(8,'Greens'),
                  RColorBrewer::brewer.pal(6,'BuPu'),
                  RColorBrewer::brewer.pal(6,'BuGn'),
                  RColorBrewer::brewer.pal(9,'Paired'),
                  RColorBrewer::brewer.pal(9,'Paired'),
                  RColorBrewer::brewer.pal(9,'Paired'),
                  RColorBrewer::brewer.pal(9,'Paired'),
                  RColorBrewer::brewer.pal(9,'Paired'))
      
    
    res <- res_input[[1]]
    param <- res_input[[2]]
    biclust <- data.frame(biclust_n=numeric(),h_score=numeric(),dim=numeric())
    for(i in 1:res@Number){
      logr <- res@RowxNumber[,i]
      logc <- res@NumberxCol[i,]
      fun_mat_prova <- array(fun_mat[logr,logc,],dim = c(sum(logr),sum(logc),dim(fun_mat)[3]))
      dist_mat <- evaluate_mat_dist(fun_mat_prova,param$template.type, param$alpha, param$beta, param$const_alpha, param$const_beta, param$shift.alignement, param$shift.max, param$max.iter)
      h_score <- ccscore_fun(dist_mat)
      dim <- sum(res@RowxNumber[,i])*sum(res@NumberxCol[i,])
      biclust_i <- data.frame(biclust_n=i,h_score=h_score,dim=dim)
      biclust <- rbind(biclust,biclust_i)
      
    }
    
    grDevices::dev.new()
    g <- ggplot2::ggplot(biclust,ggplot2::aes(x=dim,y=h_score,color=factor(biclust_n)))+
      ggplot2::geom_point(size=3) + ggplot2::scale_color_manual(values=col_palette) +
      ggplot2::xlab('Dimension') + ggplot2::ylab('H score') + ggplot2::labs(color='Bi-Cluster')
    
    print(g)


}