#' @title Plotting co-clustering results of funCC on the data matrix
#' @description funcc_show_block_matrix graphically shows the bi-clusters positions in the original data matrix
#' @export
#' @param fun_mat The data array (n x m x T) where each entry corresponds to the measure of one observation i, i=1,...,n, for a functional variable m, m=1,...,p, at point t, t=1,...,T
#' @param res_input An object produced by the funcc_biclust function
#' @return a figure representing the bi-clusters positions in the original data matrix
#' @examples  
#' data("funCCdata")
#' res <- funcc_biclust(funCCdata,delta=10,theta=1,alpha=1,beta=0,const_alpha=TRUE)
#' funcc_show_block_matrix(funCCdata,res)

funcc_show_block_matrix <- function(fun_mat,res_input){
  
  variable <- id <- value <- NULL

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
  not_null <- count_null < dim(fun_mat)[3]

  x_y=matrix(0,nrow=dim(fun_mat)[1],ncol=dim(fun_mat)[2])
  i=1
  if(res@Number==1){#x_y=t(rep.row(c(res@RowxNumber),ncol(fun_mat)))*rep.row(c(res@NumberxCol),nrow(fun_mat))}
    x_y=t(matrix(base::rep(c(res@RowxNumber),each=ncol(fun_mat)),nrow=ncol(fun_mat)))*matrix(base::rep(c(res@NumberxCol),each=nrow(fun_mat)),nrow=nrow(fun_mat))}
  if(res@Number>1){
    for( i in 1:res@Number){
      #xy=t(rep.row(res@RowxNumber[,i],ncol(fun_mat)))*rep.row(res@NumberxCol[i,],nrow(fun_mat))
      xy=t(matrix(base::rep(res@RowxNumber[,i],each=ncol(fun_mat)),nrow=ncol(fun_mat)))*matrix(base::rep(res@NumberxCol[i,],each=nrow(fun_mat)),nrow=nrow(fun_mat))
      xy =xy*i
      x_y=x_y+xy

    }
  }

  if(sum(x_y==0)>0){col_palette <- c('grey',col_palette)}

  x_y=data.frame(x_y)
  x_y[!not_null] = NA

  x_y$id=paste0("ROW ",seq(1:nrow(fun_mat)))
  names(x_y)=c(paste0("COL ",seq(1:ncol(fun_mat))),"id")
  x_y=reshape::melt(x_y,id.vars=c("id"))

  #### plot di x_y
  x_y$id <- factor(x_y$id,levels=paste0("ROW ",seq(1:nrow(fun_mat)))[nrow(fun_mat):1])
  grDevices::dev.new()
  g <- ggplot2::ggplot(x_y, ggplot2::aes(variable,id )) + ggplot2::geom_tile(ggplot2::aes(fill = factor(value)),
                                                  colour = "white") + ggplot2::scale_fill_manual(values=col_palette) +
    ggplot2::xlab('Columns') + ggplot2::ylab('Rows') + ggplot2::labs(fill='Bi-cluster') + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)) +ggplot2::theme_bw()

  print(g)

  # dev.new()
  # g <- ggplot(x_y, aes(variable,id )) + geom_tile(aes(fill = factor(value)),
  #                                                 colour = "white") + scale_fill_manual(values=col_palette) +
  #   xlab('Columns') + ylab('Rows') + labs(fill='Cluster') + theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position='none')+theme_bw()
  #
  # print(g)
}
