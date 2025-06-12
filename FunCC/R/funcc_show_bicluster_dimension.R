#' @title plotting dimensions of each bi-cluster
#' @description funcc_show_bicluster_dimension graphically shows the dimensions of each bi-cluster (i.e. number of rows and columns)
#' @export
#' @param fun_mat The data array (n x m x T) where each entry corresponds to the measure of one observation i, i=1,...,n, for a functional variable m, m=1,...,p, at point t, t=1,...,T
#' @param res_input An object produced by the funcc_biclust function
#' @return a figure representing the dimensions of each bi-cluster (i.e. number of rows and columns)
#' @examples  
#' data("funCCdata")
#' res <- funcc_biclust(funCCdata,delta=10,theta=1,alpha=1,beta=0,const_alpha=TRUE)
#' funcc_show_bicluster_dimension(funCCdata,res)
#'
funcc_show_bicluster_dimension <- function(fun_mat,res_input){
  
  cl <- n_element <- NULL

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


  res = res_input[[1]]

  count_null <- apply(fun_mat, c(1,2), function(x) sum(is.na(x)))
  not_null <-count_null < dim(fun_mat)[3]


  count = data.frame(cl=character(),nrow=numeric(),ncol=numeric(),n_element=numeric())
  if(res@Number==1){
    count = rbind(count,data.frame(cl=1,nrow=sum(c(res@RowxNumber)),ncol=sum(c(res@NumberxCol)),n_element=sum(not_null[c(res@NumberxCol),c(res@RowxNumber)])))
    # sum(c(res@NumberxCol))*sum(c(res@RowxNumber))))

  }
  if(res@Number>1){
    for( i in 1:res@Number){
      count = rbind(count,data.frame(cl=i,nrow=sum(res@RowxNumber[,i]),ncol=sum(res@NumberxCol[i,]),n_element=sum(not_null[res@RowxNumber[,i],res@NumberxCol[i,]])))
    }
  }


  grDevices::dev.new()
  g <- ggplot2::ggplot(count, ggplot2::aes(x=ncol,y=nrow,col=factor(cl))) + ggplot2::geom_point(ggplot2::aes(size = n_element)) +
    ggplot2::scale_color_manual(values=col_palette) +
    ggplot2::xlab('Number of Columns') + ggplot2::ylab('Number of Rows') + ggplot2::labs(color='Bi-Cluster')

  print(g)

  # grDevices::dev.new()
  # #g <- ggplot(count, aes(y=n_element,group=1)) + geom_boxplot()
  # g <- ggplot2::ggplot(count, ggplot2::aes(x=n_element)) + ggplot2::geom_histogram() +ggplot2::labs(title = 'Bi-clusters dimension distribution')
  # print(g)


}
