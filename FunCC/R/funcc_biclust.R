#' @title Functional Cheng and Church algorithm
#' @description The funCC algorithm allows to simultaneously cluster the rows and the columns of a data matrix where each entry of the matrix is a function or a time series
#' @export
#' @param fun_mat The data array (n x m x T) where each entry corresponds to the measure of one observation i, i=1,...,n, for a functional variable m, m=1,...,p, at point t, t=1,...,T
#' @param delta scalar: Maximum of accepted score, should be a real value > 0
#' @param theta scalar: Scaling factor should be a real value > 1
#' @param template.type character: type of template required. If template.type='mean' the template is evaluated as the average function, if template.type='medoid' the template is evaluated as the medoid function.
#' @param number integer: Maximum number of iteration
#' @param alpha binary: if alpha=1 row shift is allowed, if alpha=0 row shift is avoided
#' @param beta binary: if beta=1 row shift is allowed, if beta=0 row shift is avoided
#' @param const_alpha logicol: Indicates if row shift is contrained as constant.
#' @param const_beta logicol: Indicates if col shift is contrained as constant.
#' @param shift.alignement logicol: If shift.alignement=True the shift aligment is performed, if shift.alignement=False no alignment is performed
#' @param shift.max scalar: shift.max controls the maximal allowed shift, at each iteration, in the alignment procedure with respect to the range of curve domains. t.max must be such that 0<shift.max<1
#' @param max.iter.align integer: maximum number of iteration in the alignment procedure
#' @return a list of two elements containing respectively the Biclustresults and a dataframe containing the parameters setting of the algorithm
#'  @examples  
#' data("funCCdata")
#' res <- funcc_biclust(funCCdata,delta=10,theta=1,alpha=1,beta=0,const_alpha=TRUE)
#' res

funcc_biclust<-function(fun_mat,delta,theta=1,template.type='mean',number=100,alpha=0,beta=0,const_alpha=FALSE,const_beta=FALSE,shift.alignement=FALSE,shift.max = 0.1, max.iter.align=100){

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

  if(delta<0 | !is.numeric(delta)){
    stop('Error: delta must be a number greater than 1')
    }

  count_null <- apply(fun_mat, c(1,2), function(x) sum(is.na(x)))
  not_null <- count_null < dim(fun_mat)[3]

  parameter_input <- data.frame(delta=delta,theta=theta,template.type=template.type,alpha=alpha,beta=beta,const_alpha=const_alpha,const_beta=const_beta,shift.alignement=shift.alignement,shift.max=shift.max,max.iter.align=max.iter.align)

  only.one <- ifelse(alpha==0 & beta==0,'True',
                     ifelse(alpha==0 & beta!=0, 'True_beta',
                            ifelse(alpha!=0 & beta==0,'True_alpha',
                                   ifelse(alpha!=0 & beta!=0,'False',NA))))
  
  
  n=dim(fun_mat)[1]
  m=dim(fun_mat)[2]
  p=dim(fun_mat)[3]


  MYCALL <- match.call()
  ma=c()
  mi=c()
  for(tt in 1:p){
    ma<-c(ma,max(fun_mat[,,tt]))
    mi<-c(mi,min(fun_mat[,,tt]))
  }

  x<-matrix(FALSE,nrow=n,ncol=number)
  y<-matrix(FALSE,nrow=number,ncol=m)
  xy <- matrix(FALSE,nrow=nrow(fun_mat),ncol=ncol(fun_mat)) #new
  logr<-rep(TRUE,nrow(fun_mat))
  logc<-rep(TRUE,ncol(fun_mat))
  Stop <- FALSE
  n_clust <- 2 # per la prima iterazione
  k <- 0 # numero di bicluster trovati
  for(i in 1:number)
  {
    #print(i)
    # tolgo righe e colonne interamente a null
    # qui
    logr[logr==TRUE] <- ifelse(rowSums(matrix(is.na(matrix(fun_mat[logr,logc,1],nrow=sum(logr),ncol=sum(logc))),nrow=sum(logr),ncol=sum(logc)))==sum(logc),FALSE,TRUE)
    
    if(only.one=='False' & (sum(1-xy)<2 | sum(logr)<=1 | sum(logc)<=1)) # non voglio che ci sia una sola riga o una sola colonna
    {
      Stop <- TRUE
      break
    }
    
    if(only.one=='True' & (sum(1-xy)<2 | sum(logr)<1 | sum(logc)<1)) # non voglio che non ci siano più righe e colonne
    {
      Stop <- TRUE
      break
    }
    
    if(only.one=='True_alpha' & (sum(1-xy)<2 | sum(logr)<1 | sum(logc)<=1)) # ok singole righe ma non singole colonne
    {
      Stop <- TRUE
      break
    }
    if(only.one=='True_beta' & (sum(1-xy)<2 | sum(logr)<=1 | sum(logc)<1)) # ok singole colonne ma non singole righe
    {
      Stop <- TRUE
      break
    }
    
    
    logc[logc==TRUE] <- ifelse(colSums(matrix(is.na(matrix(fun_mat[logr,logc,1],nrow=sum(logr),ncol=sum(logc))),nrow=sum(logr),ncol=sum(logc)))==sum(logr),FALSE,TRUE)

    if(only.one=='False' & (sum(1-xy)<2 | sum(logr)<=1 | sum(logc)<=1)) # non voglio che ci sia una sola riga o una sola colonna
    {
      Stop <- TRUE
      break
    }
    
    if(only.one=='True' & (sum(1-xy)<2 | sum(logr)<1 | sum(logc)<1)) # non voglio che non ci siano più righe e colonne
    {
      Stop <- TRUE
      break
    }
    
    if(only.one=='True_alpha' & (sum(1-xy)<2 | sum(logr)<1 | sum(logc)<=1)) # ok singole righe ma non singole colonne
    {
      Stop <- TRUE
      break
    }
    if(only.one=='True_beta' & (sum(1-xy)<2 | sum(logr)<=1 | sum(logc)<1)) # ok singole colonne ma non singole righe
    {
      Stop <- TRUE
      break
    }
    
    fun_mat_temp <- fun_mat
    fun_mat_temp <- array(fun_mat_temp[logr,logc,],dim = c(sum(logr),sum(logc),dim(fun_mat)[3]))
    erg<-bigcc_fun(fun_mat_temp,delta,theta,template.type,alpha,beta,const_alpha,const_beta,shift.alignement,shift.max, max.iter.align,only.one) # lista che ritorna assegnazione righe e colonne
    
    #erg<-bigcc_fun(fun_mat[logr,logc,],delta,theta,template.type,alpha,beta,const_alpha,const_beta,shift.alignement,shift.max, max.iter.align) # lista che ritorna assegnazione righe e colonne
    n_clust <- n_clust-1
    if(sum(erg[[1]])==0 & n_clust==0 | sum(erg[[1]])==0 & i==1)
    {
      # non ho trovato niente e non ho più sottomatrici in cui cercare
      Stop <- TRUE
      break
    }
    else if (sum(erg[[1]])==0 & n_clust>=1){
      # non ho trovato nella sottomatrice dove cercavo, cerco nelle altre
      cl <- cl+1
      logr <- rep(F,nrow(xy))
      logr[clus_row[,cl]] <- T
      logc <- rep(F,ncol(xy))
      logc[clus_col[cl,]] <- T

    }
    else
    {
      k = k+1 # aggiorno numero di cluster
      # mi indicano cosa è stato assegnato all'iterazione i-esima
      x[logr,k]<-erg[[1]]
      y[k,logc]<-erg[[2]] #26019

      xy <-  xy + t(matrix(base::rep(x[,k],each=ncol(y)),nrow=ncol(y)))* matrix(base::rep(y[k,],each=nrow(x)),nrow=nrow(x))

      #xy <-  xy + t(rep.row(x[,k],ncol(y)))*rep.row(y[k,],nrow(x)) # new aggiorno l'assegnazione elementi


      #trovo le sottomatrici
      if(only.one=='False'){ res <- biclust::biclust(1-xy, method=biclust::BCBimax(), minr=2, minc=2, number=100)}
      if(only.one=='True'){res <- biclust::biclust(1-xy, method=biclust::BCBimax(), minr=1, minc=1, number=100)}
      if(only.one=='True_alpha'){res <- biclust::biclust(1-xy, method=biclust::BCBimax(), minr=1, minc=2, number=100)}
      if(only.one=='True_beta'){res <- biclust::biclust(1-xy, method=biclust::BCBimax(), minr=2, minc=1, number=100)}
      
      
      n_clust <- res@Number
      cl <- 1
      
      if(n_clust==0){#i <- i+1
        Stop <- TRUE
        break}

      clus_row <- res@RowxNumber
      clus_col <- res@NumberxCol
      
      # valuto le dimensioni dei cluster
      dimensioni <- colSums(clus_row) * rowSums(clus_col)
      
      # trovo quelli a dimensione 1 e li tolgo
      if_oneone <- which(dimensioni>1) # cluster con dimensioni > 1
      
      if(length(if_oneone)==0)
      {Stop <- TRUE
      break}
      
      if(length(if_oneone)!=n_clust){
        n_clust <- length(if_oneone) 
        clus_row <- matrix(clus_row[,if_oneone],nrow=nrow(clus_row),ncol=n_clust)
        clus_col <- matrix(clus_col[if_oneone,],nrow=n_clust,ncol=ncol(clus_col))
      }
      

      if(n_clust==0){#i <- i+1
        Stop <- TRUE
        break}

      # ordino le sottomatrici per dimensione
      if(n_clust>1){
        dim <- colSums(clus_row) * rowSums(clus_col)
        clus_row <- clus_row[,order(dim,decreasing=TRUE)]
        clus_col <- clus_col[order(dim,decreasing=TRUE),]
      }

      logr <- rep(F,nrow(xy))
      logr[clus_row[,1]] <- T
      logc <- rep(F,ncol(xy))
      logc[clus_col[1,]] <- T




    }
  }

  if(Stop)
  {
    clus_row <- x[,1:k]
    clus_col <- y[1:k,]

    # ordino le sottomatrici per dimensione
    if(k>1){
      dim <- colSums(clus_row) * rowSums(clus_col)
      clus_row <- clus_row[,order(dim,decreasing=TRUE)]
      clus_col <- clus_col[order(dim,decreasing=TRUE),]
    }

    return(list(biclust::BiclustResult(as.list(MYCALL),as.matrix(clus_row),as.matrix(clus_col),(k),list(0)),parameter=parameter_input))
  }
  else
  {
    clus_row <- x[,1:k]
    clus_col <- y[1:k,]

    # ordino le sottomatrici per dimensione
    if(k>1){
      dim <- colSums(clus_row) * rowSums(clus_col)
      clus_row <- clus_row[,order(dim,decreasing=TRUE)]
      clus_col <- clus_col[order(dim,decreasing=TRUE),]
    }
    return(list(biclust::BiclustResult(as.list(MYCALL),as.matrix(clus_row),as.matrix(clus_col),k,list(0)),parameter=parameter_input))
  }


}
