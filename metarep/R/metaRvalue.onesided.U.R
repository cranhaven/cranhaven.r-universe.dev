#' One-sided replicability analysis
#'
#' @param x object of class 'meta'
#' @param u integer between 2-\code{n}
#' @param common logical
#' @param random logical 
#' @param alternative 'less' or 'greater' only. 
#' @param do.truncated.umax  logical. 
#' @param alpha.tilde between (0,1)
#' @importFrom stats update
#' 
#' @return No return value, called for internal use only. 
metaRvalue.onesided.U <- function (x,u = 2 , common = FALSE , random = TRUE ,
                                   alternative = 'less',
                                   do.truncated.umax = TRUE ,
                                   alpha.tilde = .05 ){
  chkclass(x, "meta")
  metaInf <- inherits(x,'metainf')
  x.original = x
  if( is.null(x$statistic) | all( is.na(x$statistic) ) ){
    stop('Please supply model with z-values')
  }
  x.common.metainf <- NULL
  x.common.w <- NULL
  nstudlab <- length(x$studlab)
  if(!is.numeric(alpha.tilde)){ 
    alpha.tilde <- 1
  }else{ 
      alpha.tilde <- replace(alpha.tilde, alpha.tilde>1, 1 )
      }
  
  # available.pvs.after.truncaion <- pnorm(x$statistic, lower.tail = (alternative == 'less'))
  # nstudlab.truncation <- sum(available.pvs.after.truncaion<=alpha.tilde)
  
  if (( u > nstudlab ) | (u < 1) ){
    stop ( 'invalid tuning parameter u ' )
  }# else{
    # if ( u > nstudlab.truncation ){
    #   
    #   return(list(worst.case = x ,
    #               Side = alternative,
    #               pvalue.onesided =  1 ))
    #   message ( paste('Invalid tuning parameter u: less than u studies left after truncation at' , alpha.tilde ))
    # }
  # }
  
  
  if ( (! common )&( ! random ) ) stop('Desired replicability model must be supplied')
  if(!(alternative %in% c('less','greater'))) stop('Supply informative alternative ( "less" or "greater" )')
  
  ## common-effects replicability analysis. 
  worst.case.common <- studies_subsets <- NULL
  if ( common ){
    
    if( u == 1 ){
      return(list(worst.case = x ,
                  Side = alternative,
                  pvalue.onesided = pnorm(x$statistic.common , lower.tail = ( alternative == 'less'))  ))
    }
    if( u == nstudlab ){
      studies_subsets <- rbind( 1:nstudlab , rep(NA , nstudlab) )
      rownames(studies_subsets) <- c('s1' , 'rvalue' )
      studies_subsets['rvalue' , ] <- pnorm( x$statistic , lower.tail = (alternative=='less'))
      
      # derive the line of worst case studies 
      k = max(which.max(studies_subsets['rvalue',])) # studies column pointer. 
      
      worst.case.studies = x$studlab[ k ]
      worst.case.studies = which( x$data$.studlab %in% worst.case.studies )
      worst.case <- update(x ,subset = worst.case.studies )
      
      return(list(worst.case = worst.case ,
                  Side = alternative,
                  pvalue.onesided = studies_subsets['rvalue' , k] ))
      
      
    }
    
    studies_subsets <- combn(x = 1:nstudlab , m = nstudlab-u+1 ,replace=F)
    studies_subsets <- rbind(studies_subsets , rep(NA , ncol(studies_subsets)))
    rownames(studies_subsets) <- c( paste0('s' , 1:(nstudlab-u+1)) , 'rvalue' )
    for ( k in 1:ncol(studies_subsets)){
      worst.studies.fisher = x$studlab[ studies_subsets[-c(nrow(studies_subsets)) ,k] ]
      worst.studies.fisher = which( x$data$.studlab %in% worst.studies.fisher)
      x.sub <- update(x,subset =  worst.studies.fisher )
      z.val <- x.sub$statistic.common
      studies_subsets['rvalue' , k] <-  x.sub$pval.common / 2
      if ( ((z.val < 0) & (alternative == 'greater')) | ( (z.val > 0) & (alternative == 'less') ) ){
        studies_subsets['rvalue' , k] = 1 - studies_subsets['rvalue' , k]  
      }
    }
    # derive the line of worst case study 
    k = max(which.max(studies_subsets['rvalue',])) # studies column pointer. 
    
    worst.case.studies = x$studlab[ studies_subsets[-c(nrow(studies_subsets)) ,k] ]
    worst.case.studies = which( x$data$.studlab %in% worst.case.studies )
    
    worst.case <- update(x ,subset = worst.case.studies )
    return(list(worst.case = worst.case ,
                Side = alternative,
                pvalue.onesided = studies_subsets['rvalue' , k] ))
  }
  # so far so good 
  
  ## random-effects replicability analysis:
  
  statistic.all <- x$statistic[!is.na(x$statistic)]
  pvs.all <- pnorm( statistic.all ,lower.tail = (alternative == 'less'))
  
  if( u == 1 ){
    pvo <- truncatedPearson( p = pvs.all , alpha.tilde =  alpha.tilde)
    return(list(worst.case = x ,
                Side = alternative,
                pvalue.onesided =  pvo$p.value ))
  }
  if( u > sum(pvs.all<=alpha.tilde) ){
    return(list(worst.case = x ,
                Side = alternative,
                pvalue.onesided =  1 ))
  }
  
  # if( u == sum(pvs.all<=alpha.tilde) ){
  #   pvs.all <- replace(pvs.all,pvs.all>alpha.tilde, NA)
  #   pvo <- max(pvs.all, na.rm = TRUE)
  #   worst.case.studies <- x$studlab[which.max(pvs.all)]
  #   worst.case.studies <- which( x$data$.studlab %in% worst.case.studies )
  #   worst.case <- update(x ,subset = worst.case.studies )
  #   
  #   if ( pvo < alpha.tilde){
  #     return(list(worst.case = worst.case,
  #                 Side = alternative,
  #                 pvalue.onesided =  pvo ))
  #   }else{
  #     return(list(worst.case = worst.case,
  #                 Side = alternative,
  #                 pvalue.onesided =  1 ))
  #   }
  #   
  # }
  # The previous lines should be removed because (1) they only consider u=2, and (2) they truncate prior to subsetting. 
  
  
  wsf <- which( rank(pvs.all) >= u ) # order replaced with rank, as it was incorrect. 
  worst.studies.fisher <- x$studlab[ wsf ]
  worst.studies.fisher <- which( (x$data$.studlab %in% worst.studies.fisher) )
  worst.case <- update(x ,subset = worst.studies.fisher )
  if( length(wsf) <= 1){
    worst.pvs.fisher <-  list(p.value  = ifelse( length(wsf) == 0 , 1, pvs.all[ wsf ]) )
  }else{
    worst.pvs.fisher <- truncatedPearson( p = pvs.all[ wsf ] ,alpha.tilde =  alpha.tilde)
  }
  return(list(worst.case = worst.case ,
              Side = alternative,
              pvalue.onesided = worst.pvs.fisher$p.value ))
}
