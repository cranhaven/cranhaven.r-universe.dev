#' @title plotting coverage of each bi-cluster
#' @description funcc_show_bicluster_coverage graphically shows the coverage of each bi-cluster in terms of percentage of included functions
#' @export
#' @param fun_mat The data array (n x m x T) where each entry corresponds to the measure of one observation i, i=1,...,n, for a functional variable m, m=1,...,p, at point t, t=1,...,T
#' @param res_input An object produced by the funcc_biclust function
#' @param not_assigned logicol: if true also the cluster of not assigned elements is included
#' @param max_coverage scalar: percentage of maximum cumulative coverage to be shown
#' @return a figure representing for each bi-cluster the coverage in terms of percentage of included functions
#' @examples  
#' data("funCCdata")
#' res <- funcc_biclust(funCCdata,delta=10,theta=1,alpha=1,beta=0,const_alpha=TRUE)
#' funcc_show_bicluster_coverage(funCCdata,res)

funcc_show_bicluster_coverage <- function(fun_mat,res_input,not_assigned=TRUE,max_coverage=1){

  cl <- perc <- type <- cum_perc <- NULL
  
  res <- res_input[[1]]

  if(max_coverage>1){
    stop('Error: max_coverage should be a value between 0 and 1')
    
  }

  if(!not_assigned %in% c(TRUE,FALSE)){
    stop('Error: not_assigned should be a logicol value TRUE/FALSE')
    
  }

  n_tot <- dim(fun_mat)[1]*dim(fun_mat)[2]

  # ordino le sottomatrici per dimensione
  dim <- colSums(res@RowxNumber) * rowSums(res@NumberxCol)
  #dim <- dim[dim>0]

  coverage <- data.frame(cl=seq(1,res@Number),dim=dim)
  coverage$perc <- coverage$dim/n_tot
  coverage$color='grey'
  coverage$type='Bi-cluster'

  if(not_assigned){
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
    num_not_assagined = sum(x_y==0)
    coverage <- rbind(data.frame(cl=0,dim=num_not_assagined,perc=num_not_assagined/n_tot,color='red',type='Not assigned'),coverage)
  }

  coverage$cum_perc <- cumsum(coverage$perc)

  coverage <- coverage[coverage$cum_perc<=max_coverage,]

  grDevices::dev.new()
  #max_val <- max(coverage$perc)
  g <- ggplot2::ggplot() + ggplot2::geom_bar(data=coverage,ggplot2::aes(x=factor(cl),y=perc,fill=type),stat='identity') +
    ggplot2::scale_fill_manual(values=c('red','grey2')) +
    ggplot2::geom_point(data=coverage,ggplot2::aes(x=factor(cl),y=cum_perc*max(perc)),color='blue')+
    ggplot2::geom_line(data=coverage,ggplot2::aes(x=factor(cl),y=cum_perc*max(perc)),color='blue')+
    ggplot2::scale_y_continuous(limits=c(0, max(coverage$perc)),name = "Bi-clusters coverage",
                                sec.axis = ggplot2::sec_axis(~./max(coverage$perc), name = "Bi-clusters cumulative coverage")) +
    ggplot2::xlab('Bi-cluster')+ggplot2::theme(legend.title =  ggplot2::element_blank())

  g

}
