#' Lower bounds on the number of studies with replicated effect
#'
#' @description lower bounds on the number of studies with increased and\\ or decreased effect. 
#' @param x Object of class 'meta'
#' @param alternative 'less', 'greater' or 'two-sided'
#' @param t truncation threshold for truncated-Pearsons' test (`t=0.05` by default). t is ignored if `common.effect  = TRUE`.
#' @param confidence Confidence level used in the computaion of the lower bound(s) \eqn{u_{max}^L} and\\or \eqn{u_{max}^R}. 
#' @param common.effect Use common.effect = FALSE (default) for replicability-analysis combining with no assumptions (Pearson or truncated-Pearson test).
#' @importFrom stats pnorm
#' @importFrom utils combn
#' @return
#' An object of class list reporting the bounds on the number of
#' studies with a positive or negative effect, as follows:
#' \item{worst.case}{A charachter vector of the names of
#' \code{n-u_{max}+1} studies at which the the \code{r(u_{max})-}value
#' is computed.}
#' \item{side}{The direction of the replicated signal in the
#' 'worst.case' studies. 'less' if the effect is negative, 'greater'
#' if positive.}
#' \item{u_max}{The bound on the number of studies with either a
#' positive or a negative effect.}
#' \item{r-value}{The 'u-out-of-n' \code{r(u)-}-value calculated with
#' u=u_max.}
#' \item{Replicability_Analysis}{Report of the replicability lower
#' bounds on the number of studies with negative effect and with
#' positive effect.}
#' 
#' @export
#'
#' @examples n.i.1 <- c( 20, 208, 24, 190, 58, 36, 51)
#' a.i <- c( 2,79,0,98,15,34,9) 
#' n.i.2 <- c( 20, 119, 22, 185, 29, 51, 47)
#' c.i <- c(9,106,14,98,12,49,9) 
#' m1 <- metabin( event.e = a.i,n.e = n.i.1,
#'                event.c = c.i,n.c = n.i.2,
#'                studlab = paste('Study',1:7), sm = 'OR',
#'                common = FALSE, random = TRUE )
#' find_umax(m1 , common.effect = FALSE, alternative = 'two-sided',
#'           t = 0.05 , confidence = 0.95 )        
find_umax <- function(x , alternative = 'two-sided' ,
                      t = 0.05 , confidence = 0.95, common.effect = FALSE ){


  if(is.numeric(confidence)){
    if ( (confidence >= 1) | (confidence <= 0) ) stop("confidence must be a number between 0 - 1.")
  }else{
    stop("confidence must be a number between 0 - 1.")
  }
  
  if (common.effect){
    t <- NULL
    message( "Performing Replicability analysis with the common-effect assumption" )
  }else{
    if(is.null(t)){
      stop("Error: Must specify truncation threshold t <= 1 .
           For replicability-analysis with common-effect assumption, set common.effect = TRUE ")
    }
    if( (t<= 0)|(t>1) ){
      stop("Error: Truncation threshold t must be a positive velue < = 1")
    }
    if(t == 1 ) message( "Performing Replicability analysis via original-Pearson's test" )
  }  
  
  na.pvs <- all(is.na(x$pval))
  na.zvs <- (sum(!is.na(x$statistic))==0 )
  if ( na.zvs & na.pvs ){
    warning('Please supply valid p-values or z-values.')
    return( list ( u_max = NULL , worst.case = NULL , side = NULL , r.value = NULL )[c(2,1,3,4)] )
  }
  
  alpha = (1 - confidence)
  Do.truncated.umax = ifelse(is.null(t) , F , t < 1 )
  Alpha.tilde = ifelse(is.null(t) , 1 , t )
  random = !common.effect
  
  # do.random.u_max = Fisher's \ karl pearson . do.truncated.umax  = truncated fisher
  # Or else - alternative = 'greater' , 'less'
  chkclass(x, "meta")
  twoSided <- (alternative == 'two-sided')
  
  nstudlab <- sum(!is.na(x$pval))
  nstudlab <- ifelse( na.pvs , sum(!is.na(x$statistic)) , nstudlab )
  
  pv.greater <- ifelse(common.effect , x$statistic.common , x$statistic.random ) 
  
  if(!is.na(pv.greater)){ 
    pv.greater <- pnorm( pv.greater , lower.tail = FALSE )
  }else{
    pv.greater <- ifelse(common.effect , x$pval.common , x$pval.random ) 
    TE.sign <- ifelse(common.effect , x$TE.common , x$TE.random ) 
    pv.greater <- ifelse(TE.sign > 0 , pv.greater/2 , 1-pv.greater/2) 
  }
  
  pv.less <- 1 - pv.greater
  
  rvl <- rvg <- 1
  ul <- ug <- u_max <-  0
  meta_ug <- meta_ul <- meta_ul_last_sig <- meta_ug_last_sig <- NULL
  
  if(is.null(t)){
    stop.n.at.less <- 
      stop.n.at.greatr <- nstudlab
  }else{
    pvs.all.less <- pnorm(x$statistic, lower.tail = TRUE )
    pvs.all.less <- pvs.all.less[!is.na(pvs.all.less)]
    stop.n.at.less <- sum(pvs.all.less<=t)
    
    pvs.all.greater <- pnorm(x$statistic, lower.tail = FALSE )
    pvs.all.greater <- pvs.all.greater[!is.na(pvs.all.greater)]
    stop.n.at.greatr <- sum(pvs.all.greater<=t)
  }
  
  # perform random replicability analysis  
  if( !common.effect ){
    meta_ul_prev <-  meta_ug_prev <- NULL
    if ( alternative != 'greater' ){
      u1 <-  1  
      meta_ul <-  metaRvalue.onesided.U(x,u = u1 ,common = FALSE , random = TRUE,
                                      alternative = 'less',
                                      do.truncated.umax = Do.truncated.umax ,
                                      alpha.tilde = Alpha.tilde )
      rvl <- meta_ul$pvalue.onesided
      while( (u1 < stop.n.at.less ) & ( rvl <=  alpha / (1 + twoSided )) ){
        meta_ul_prev <- meta_ul
        u1 <- u1 + 1
        meta_ul <- metaRvalue.onesided.U(x,u = u1 ,common = FALSE , random = TRUE,
                                         alternative = 'less',
                                         do.truncated.umax = Do.truncated.umax  ,
                                         alpha.tilde = Alpha.tilde )
        rvl <- meta_ul$pvalue.onesided
      }
      
      ul <- u1
      
      if ( (rvl >  alpha / (1 + twoSided )) ){
        if(is.null(meta_ul_prev)){
          ul <- 0
        }else{
          rvl <- meta_ul_prev$pvalue.onesided
          meta_ul <- meta_ul_prev
          ul <- u1-1 
        }

      }
      
      u_max <- ul ;  names( u_max) <- 'u^L'
      rvalue <- rvl
      worst.case.meta <- meta_ul 
      side = 'less'
      rep.text <- paste0('out of ' , nstudlab , ' studies, ', ul ,
                         ' with decreased effect.')
    }
    
    
    if ( alternative != 'less' ){
      u1 <-  1  
      meta_ug = metaRvalue.onesided.U(x,u = u1 , common = FALSE , random = TRUE,
                                      alternative = 'greater',
                                      do.truncated.umax = Do.truncated.umax ,
                                      alpha.tilde = Alpha.tilde )
      rvg <- meta_ug$pvalue.onesided
      while((u1 < stop.n.at.greatr )&( rvg <=  alpha / (1 + twoSided )) ){
        meta_ug_prev <- meta_ug
        u1 <- u1 + 1
        meta_ug <- metaRvalue.onesided.U(x,u = u1 ,common = FALSE , random = TRUE,
                                         alternative = 'greater',
                                         do.truncated.umax = Do.truncated.umax,
                                         alpha.tilde = Alpha.tilde )
        rvg <- meta_ug$pvalue.onesided
      }
      ug <- u1
      if (rvg >  alpha / (1 + twoSided ) ){ 
        if ( is.null(meta_ug_prev) ){
          ug <- 0
          }else{
          rvg <- meta_ug_prev$pvalue.onesided
          meta_ug <- meta_ug_prev
          ug <- u1-1 
        }
      }
      u_max <- ug ;  names( u_max) <- 'u^R'
      rvalue <- rvg
      worst.case.meta <- meta_ug 
      side <- 'greater'
      rep.text <- paste0('out of ' , nstudlab , ' studies, ' , ug , ' with increased effect.')
    }
    
    if(is.null(rvl)) { rvl <- 1 ; ul <- 0 }
    if(is.null(rvg)) { rvg <- 1 ; ug <- 0 }
    
    names(rvalue) <- 'r^R'
    
    if ( alternative == 'two-sided' ){
      if( (ul > ug) | ((ul == ug)&(rvl<rvg)) ){
        u_max <- ul
        worst.case.meta <- meta_ul 
        side <- 'less'
        names(rvalue) <- 'r^L'
        names( u_max) <- 'u^L'
      }
      u_max <- c(u_max , ul , ug )
      names(u_max) <- c('u_max' , 'u^L', 'u^R')
      
      rvalue <- 2*min( c( rvl, rvg, 0.5 ))
      rvalue <- c(  rvalue , rvl , rvg )
      names( rvalue ) <- c( 'r.value' , 'r^L' , 'r^R')
      
      rep.text <- paste0('out of ' , nstudlab , ' studies: ', ul ,
                         ' with decreased effect, and ', ug , ' with increased effect.')
      
      worst.case.meta$pvalue.onesided <- min( c( 0.5 , rvl , rvg) )*2 
    }

    names(side) <- 'Direction of the stronger signal'
    return(list(worst.case =  (worst.case.meta$worst.case)$studlab,
                side = side , u_max = u_max , r.value = round(rvalue,digits = 4) ,
                Replicability_Analysis = unname(rep.text)))
  }
  
  # if reaches here, means that common-effect replicability analysis is performed. 
  
  
  
  ul <- 0 ; meta_ul <- NULL
  if ( alternative != 'greater' ){
    u1 <- 1 ; u2 <- stop.n.at.less
    final_ul <- NULL
    # eleminate the radical cases: 
    meta_ul_last_sig <- meta_ul <- 
      metaRvalue.onesided.U(x,u = 1 ,common = TRUE , random = FALSE,
                            alternative = 'less',  do.truncated.umax = FALSE ,
                            alpha.tilde = Alpha.tilde )
    if( pv.less >  alpha / (1 + twoSided )) {
      meta_ul_last_sig <- NULL
      final_ul <- 
        list(u_max = 0 , worst.case =  (meta_ul$worst.case)$studlab,
             side = 'less' , r.value = meta_ul$pvalue.onesided )
      
    }
    
    # u1 <- u1 + 1
    
    # model with one study only, replicability at all: 
    meta_ul <-  metaRvalue.onesided.U(x,u = u2 ,common =T , random = FALSE,
                                      alternative = 'less',
                                      do.truncated.umax = FALSE ,
                                      alpha.tilde = Alpha.tilde)
    rvl <- meta_ul$pvalue.onesided
    
    if(rvl <=  alpha / (1 + twoSided )) {
      final_ul <- 
        list(u_max = u2 , worst.case =  (meta_ul$worst.case)$studlab,
             side = 'less' , r.value = round( rvl ,digits = 4) )
    }
    
    # u2 <- u2 - 1 
    # if 1 < u_max < n.studlab , search for it by devide-and-concur:
    if ( is.null(final_ul)){
      u_mid <- ceiling( (u1 + u2)/2 )
      
      while ( u_mid != u2 ){
        meta_ul <- metaRvalue.onesided.U(x,u = u_mid ,common = TRUE , random = FALSE,
                                         alternative = 'less',
                                         do.truncated.umax = FALSE ,
                                         alpha.tilde = Alpha.tilde)
        
        if ( meta_ul$pvalue.onesided < alpha / (1 + twoSided ) ){
          u1 <- u_mid 
          meta_ul_last_sig <- meta_ul
        }else{
          u2 <- u_mid 
        }
        u_mid <- ceiling( (u1 + u2)/2 )
      }
      ul <- u1
      meta_ul <- meta_ul_last_sig 
      rvl <- meta_ul$pvalue.onesided
      side <- 'less'
      
      rep.text <- paste0('out of ' , nstudlab , ' studies, ', ul ,
                         ' with decreased effect.')
      

      final_ul <- 
        list(u_max = ul , worst.case =  (meta_ul$worst.case)$studlab,
             side = 'less' ,
             r.value = round( rvl ,digits = 4) , 
             Replicability_Analysis = unname(rep.text) )
    }
    
    
    if( alternative == 'less'){
      return(final_ul[c(2,1,3:5)])
    }
    
  }  
  
  ug <- 0 ; meta_ug <- NULL
  if ( alternative != 'less' ){
    u1 <- 1 ; u2 <- stop.n.at.greatr
    final_ug <- NULL
    
    # eleminate the radical cases: 
    meta_ug <- meta_ug_last_sig <-
      metaRvalue.onesided.U(x,u = 1 ,common = TRUE , random = FALSE,
                            alternative = 'greater', do.truncated.umax = FALSE ,
                            alpha.tilde = Alpha.tilde )
    
    if( pv.greater >  alpha / (1 + twoSided )) {
      meta_ug_last_sig <- NULL
      final_ug <-
        list(u_max = 0 , worst.case =  (meta_ug$worst.case)$studlab,
             side = 'greater' , r.value = meta_ug$pvalue.onesided )
    }
    
    # the original model
    
    # u1 <- u1 + 1
    
    # model with one study only, replicability at all: 
    meta_ug = metaRvalue.onesided.U(x,u = u2 ,common = TRUE , random = FALSE,
                                    alternative = 'greater',
                                    do.truncated.umax = FALSE ,
                                    alpha.tilde = Alpha.tilde )
    rvg <- meta_ug$pvalue.onesided
    
    if(rvg <=  alpha / (1 + twoSided )) {
      final_ug <-
        list(u_max = u2 , worst.case =  (meta_ug$worst.case)$studlab,
             side = 'greater' , r.value = round( rvg ,digits = 4)  )
      
    }
    
    # u2 <- u2 - 1 
    
    # if 1 < u_max < n.studlab , search for it by devide-and-concur:
    if (is.null(final_ug)) {
      u_mid <- ceiling( (u1 + u2)/2 )
      
      while ( u_mid != u2 ){
        meta_ug <- metaRvalue.onesided.U(x,u = u_mid ,common = TRUE , random = FALSE,
                                         alternative = 'greater',
                                         do.truncated.umax = FALSE ,
                                         alpha.tilde = Alpha.tilde )
        
        if ( meta_ug$pvalue.onesided < alpha / (1 + twoSided ) ){
          u1 <- u_mid 
          meta_ug_last_sig <- meta_ug 
        }else{
          u2 <- u_mid 
        }
        u_mid <- ceiling( (u1 + u2)/2 )
      }
      ug <- u1
      meta_ug <- meta_ug_last_sig 
      rvg <- meta_ug$pvalue.onesided
      
      rep.text <- paste0('out of ' , nstudlab , ' studies, ', ug ,
                         ' with increased effect.')
      
      final_ug <-
        list(u_max = ug , worst.case =  (meta_ug$worst.case)$studlab,
             side = 'greater' ,
             r.value = round( rvg ,digits = 4) ,
             Replicability_Analysis = unname(rep.text))
    }
    final <- final_ug
    side = 'greater'
    if( alternative == 'greater'){
      return(final_ug[c(2,1,3:5)])
    }
    
  }
  
  if(is.null(rvl)) { rvl <- 1 ; ul <- 0 }
  if(is.null(rvg)) { rvg <- 1 ; ug <- 0 }
  
  if ( alternative == 'two-sided' ){
    if( (ul > ug) | ((ul == ug)&(rvl<rvg)) ){
      final <- final_ul
      side <- 'less'
    }
    final$r.value <- round( min( min(rvl,rvg)*2 ,1 ) ,digits = 4)  
    final <- final[c(2,1,3,4)]
    return(final)
  }
  
}
