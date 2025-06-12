#' @title Functional Cheng and Church Algorithm varying the delta value
#' @description The find_best_delta function evaluate the results of FunCC algorithm in terms of total H-score value, the number of obtained bi-clusters and the number of not assigned elements when varying the delta value
#' @export
#' @param fun_mat The data array (n x m x T) where each entry corresponds to the measure of one observation i, i=1,...,n, for a functional variable m, m=1,...,p, at point t, t=1,...,T
#' @param delta_min scalar: Manimum value of the maximum of accepted score, should be a real value > 0
#' @param delta_max scalar: Maximum value of the maximum of accepted score, should be a real value > 0
#' @param num_delta integer: number of delta to be evaluated between delta_min and delta_max
#' @param theta scalar: Scaling factor should be a real value > 1
#' @param template.type character: type of template required. If template.type='mean' the template is evaluated as the average function, if template.type='medoid' the template is evaluated as the medoid function.
#' @param number integer: Maximum number of iterations
#' @param alpha binary: if alpha=1 row shift is allowed, if alpha=0 row shift is avoided
#' @param beta binary: if beta=1 row shift is allowed, if beta=0 row shift is avoided
#' @param const_alpha logicol: indicates if row shift is contrained as constant
#' @param const_beta logicol: indicates if col shift is contrained as constant
#' @param shift.alignement logicol: If shift.alignement=True the shift aligment is performed, if shift.alignement=False no alignment is performed
#' @param shift.max scalar: shift.max controls the maximal allowed shift, at each iteration, in the alignment procedure with respect to the range of curve domains. t.max must be such that 0<shift.max<1
#' @param max.iter.align integer: maximum number of iteration in the alignment procedure
#' @return a dataframe containing for each evaluated delta: Htot_sum (the sum of totale H-score), num_clust (the number of found Bi-clusters), not_assigned (the number of not assigned elements)
#' @examples 
#' \dontrun{ 
#' data("funCCdata")
#' find_best_delta(funCCdata,delta_min=0.1,delta_max=20,num_delta=20,alpha=1,beta=0,const_alpha=TRUE)
#' }

find_best_delta <- function(fun_mat, delta_min,delta_max,num_delta=10,template.type='mean',theta=1.5,number=100,alpha=0,beta=0,const_alpha=FALSE,const_beta=FALSE,shift.alignement=FALSE,shift.max = 0.1, max.iter.align=100){

  if(length(dim(fun_mat))!=3){
    stop('Error: fun_mat should be an array of three dimensions')
    }

  if(template.type=='medoid' & (alpha!=0 | beta!=0)){
    stop('Error: Medoid template is defined only for alpha and beta equal to 0')
    }

  if(shift.max>1 | shift.max<0){
    stop('Error: shift.max must be in [0,1]')
    }

  if(!(alpha %in% c(0,1)) | !(beta %in% c(0,1))){
    stop('Error: alpha and beta must be 0 or 1')
    }

  if(!(template.type %in% c('mean','medoid'))){
    stop(paste0('Error: template.type ',template.type,' is not defined'))
    }

  if(number<=0 | number%%1!=0){
    stop('Error: number must be an integer greater than 0')
    }

  if(!(shift.alignement %in% c(TRUE,FALSE))){
    stop(paste0('Error: shift.alignement should be a logicol variable'))
    }


  if(max.iter.align<=0 | max.iter.align%%1!=0){
    stop('Error: max.iter.align must be an integer greater than 0')
    }

  if(base::length(dim(fun_mat))!=3){
    stop('Error: fun_mat must an array of three dimensions')
    }

  if(!(const_alpha %in% c(FALSE,TRUE)) | !(const_beta %in% c(FALSE,TRUE))){
    stop('Error: const_alpha and const_beta must TRUE or FALSE')
    }

  if(delta_min<0 | !is.numeric(delta_min) | delta_max<0 | !is.numeric(delta_max)){
    stop('Error: delta_min and delta_max must be a number greater than 1')
    }

  if(delta_min>delta_max){
    stop('Error: delta_max must be a number greater than delta_min')
    }

  
  cl <- delta <- NULL
  delta_check <- seq(delta_min,delta_max,(delta_max-delta_min)/num_delta)

  Htot_best <- delta_max
  best_d <- delta_max

  Htot_sum <- numeric()
  Htot_all_mean <- numeric()
  num_clust <- numeric()
  not_assigned <- numeric()

  for(d in delta_check){
    #print(paste0('Iteration: ',which(delta_check==d),' - Delta: ',d))
    res_fun_list <- funcc_biclust(fun_mat,delta=d,template.type=template.type,theta=theta,number=number,alpha=alpha,beta=beta,const_alpha,const_beta,shift.alignement,shift.max, max.iter.align)
    res_fun <- res_fun_list[[1]]

    if(res_fun@Number==0){
      Htot_all_mean <- c(Htot_all_mean,NA)#ccscore_fun(fun_mat))
      Htot_sum <- c(Htot_sum,NA)
      num_clust <- c(num_clust,0)
      not_assigned <- c(not_assigned,nrow(fun_mat)*ncol(fun_mat))}
    
    if(res_fun@Number==1){
      fun_mat_cl <- array(fun_mat[c(res_fun@RowxNumber),c(res_fun@NumberxCol),],dim=c(sum(c(res_fun@RowxNumber)),sum(c(res_fun@NumberxCol)),dim(fun_mat)[3]))

      dist_mat <- evaluate_mat_dist(fun_mat_cl,template.type,alpha,beta,const_alpha,const_beta,shift.alignement,shift.max, max.iter.align)
      H_cl<-ccscore_fun(dist_mat)

      elements <- nrow(fun_mat)*ncol(fun_mat)
      elements <- elements-nrow(fun_mat_cl)*ncol(fun_mat_cl)
      not_assigned <- c(not_assigned,elements)
      Htot_d <- mean(H_cl)
      Htot_all_mean<- c(Htot_all_mean,Htot_d)
      Htot_sum <- c(Htot_sum,sum(H_cl))
      num_clust <- c(num_clust,1)
    }
    if(res_fun@Number>1){
      num_clust <- c(num_clust,res_fun@Number)

      H_cl <- numeric()
      elements <- nrow(fun_mat)*ncol(fun_mat)
      for(cl in 1:res_fun@Number){

        dist_mat <- evaluate_mat_dist(array(fun_mat[c(res_fun@RowxNumber[,cl]),c(res_fun@NumberxCol[cl,]),],dim=c(sum(c(res_fun@RowxNumber[,cl])),sum(c(res_fun@NumberxCol[cl,])),dim(fun_mat)[1])),template.type,alpha,beta,const_alpha,const_beta,shift.alignement,shift.max, max.iter.align)
        H_cl_temp<-ccscore_fun(dist_mat)
        
        H_cl <- c(H_cl,H_cl_temp)

        fun_mat_cl <- array(fun_mat[c(res_fun@RowxNumber[,cl]),c(res_fun@NumberxCol[cl,]),],dim=c(sum(c(res_fun@RowxNumber[,cl])),sum(c(res_fun@NumberxCol[cl,])),dim(fun_mat)[3]))
        
        elements <- elements-nrow(fun_mat_cl)*ncol(fun_mat_cl)
      }
      not_assigned <- c(not_assigned,elements)
      Htot_d <- mean(H_cl)
      Htot_all_mean<- c(Htot_all_mean,Htot_d)
      Htot_sum <- c(Htot_sum,sum(H_cl))
      if(Htot_d<Htot_best){
        Htot_best <- Htot_d
        best_d <- d
      }
    }

  }
  h <- data.frame(Htot_sum=Htot_sum,Htot_all_mean=Htot_all_mean,num_clust=num_clust,delta=delta_check,not_assigned=not_assigned)
  
  grDevices::dev.new()
  g <- ggplot2::ggplot(h,ggplot2::aes(x=delta,y=Htot_sum))+ggplot2::geom_point()+ggplot2::geom_line() + ggplot2::ggtitle('Delta vs H tot')
  print(g)

  grDevices::dev.new()
  g <- ggplot2::ggplot(h,ggplot2::aes(x=delta,y=num_clust))+ggplot2::geom_point()+ggplot2::geom_line() + ggplot2::ggtitle('Delta vs num clusters')
  print(g)

  grDevices::dev.new()
  g <- ggplot2::ggplot(h,ggplot2::aes(x=delta,y=not_assigned))+ggplot2::geom_point()+ggplot2::geom_line() + ggplot2::ggtitle('Delta vs not assigned')
  print(g)

  return(h)
  
}
