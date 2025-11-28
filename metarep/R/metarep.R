#' Replicability-analysis of a meta-analysis 
#' @description Add results of replicability-analysis to a meta-analysis, whether common- or random-effects.
#' @param x object of class 'meta'
#' @param u replicability requirement. \code{u} must be an intiger between 2 and \code{n} (nmber of studies in the meta-analysis). 
#' @param t truncation threshold for truncated-Pearsons' test (`t=0.05` by default). t is ignored if `common.effect  = TRUE`.
#' @param alternative use 'less', 'greater' or 'two-sided'

#' @param report.u.max  use TREU to report the lower bounds on number of studies with replicated effect. 
#' @param confidence Confidence level used in the computaion of the lower bound(s) \eqn{u_{max}^L} and\\or \eqn{u_{max}^R}. 
#' @param common.effect Use common.effect = FALSE (default) for replicability-analysis combining with no assumptions (Pearson or truncated-Pearson test).
#' Replicability-analysis based on the test-statistic of common-effects model can be applied using common.effect = TRUE. 
#'
#' @return An object of class list containing meta-analysis and replicability analysis results, as follows:
#' \item{worst.case.studies}{A charachter vector of the names of \code{n-u+1} studies at which the the \code{r(u)-}value is computed.}
#' \item{r.value}{ \code{r(u)-}value for the specied u. }
#' \item{side}{The direction of the effect with the lower one-sided \code{r(u)-}value }
#' \item{u_L , u_R }{ Lower bounds of the number of studies with decreased or increased effect, respectively. Both bounds are reported simultinualsly only when performing replicability analysis for two-sided alternative with no assumptions  }
#' 
#' @importFrom utils combn
#' @export 
#'
#' @examples  n.i.1 <- c( 20, 208, 24, 190, 58, 36, 51)
#' a.i <- c( 2,79,0,98,15,34,9) 
#' n.i.2 <- c( 20, 119, 22, 185, 29, 51, 47)
#' c.i <- c(9,106,14,98,12,49,9) 
#' m1 <- metabin( event.e = a.i,n.e = n.i.1,event.c = c.i,n.c = n.i.2,
#'                studlab = paste0('Study ' , 1:7) , sm = 'OR' ,
#'                common = FALSE, random = TRUE )
#' mr1 <- metarep(  m1 , u = 2, common.effect = FALSE , t = 0.05 , 
#'                alternative = 'two-sided', report.u.max = TRUE)
#' forest(mr1, layout='revman5',digits.pval = 4 , test.overall = TRUE )
metarep <- function(x, u = 2 , t = 0.05 , alternative = 'two-sided',
                    report.u.max = FALSE , confidence = 0.95 , common.effect = FALSE ) {
  chkclass(x, "meta")
  if(is.numeric(confidence)){
    if ( (confidence >= 1) | (confidence <= 0) ) stop("confidence must be a number between 0 - 1.")
  }else{
    stop("confidence must be a number between 0 - 1.")
  }
  alpha = 1-confidence
  Do.truncated.umax = ifelse(is.null(t) , F , t < 1 )
  Alpha.tilde = ifelse(is.null(t) , 1 , t )
  random = !common.effect
  
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
  
  res <- x
  ##
  ## Do replicability analysis
  ##
  
  
  
  # compute r-value
  rvalue <- rvalue.less <- rvalue.greater <- NULL 
  if ( alternative != 'two-sided' ){
    rvalue.results <- metaRvalue.onesided.U( x = x , u = u , alternative = alternative  , 
                                             common = common.effect,
                                             random = random,
                                             do.truncated.umax = Do.truncated.umax, 
                                             alpha.tilde = Alpha.tilde )
    rvalue <- rvalue.results$pvalue.onesided
    side <- alternative
  }else{
    rvalue.results.less <- metaRvalue.onesided.U( x = x , u = u , alternative = 'less', 
                                                  common = common.effect,
                                                  random = random,
                                                  do.truncated.umax = Do.truncated.umax, 
                                                  alpha.tilde = Alpha.tilde )
    rvalue.less <- rvalue.results.less$pvalue.onesided
    
    rvalue.results.greater <- metaRvalue.onesided.U( x = x , u = u , alternative = 'greater', 
                                                     common = common.effect,
                                                     random = random,
                                                     do.truncated.umax = Do.truncated.umax, 
                                                     alpha.tilde = Alpha.tilde  )
    
    rvalue.greater <- rvalue.results.greater$pvalue.onesided
    
    
    
    if(all(is.null(c(rvalue.greater, rvalue.less)))){
      rvalue.results <- rvalue.results.less
      side <- 'None'
      rvalue <- NULL
    }else{
      if( is.null(rvalue.greater) ) {
        rvalue.results <- rvalue.results.less
        side <- 'less'
        rvalue <- NULL
      }else{
        if( is.null(rvalue.less) ){
          rvalue.results <- rvalue.results.greater
          side <- 'greater'
          rvalue <- NULL
        }
        if ( rvalue.less < rvalue.greater ){
          rvalue.results <- rvalue.results.less
          side <- 'less'
        }else{
          rvalue.results <- rvalue.results.greater
          side <- 'greater'
        }
        rvalue <- min(1 , 2*rvalue.results$pvalue.onesided )
      }
    }
  }
  
  # find u_max
  if(report.u.max){
    Umax_right  <- Umax_left <- Umax <- Side <-  NULL
    
    if( common.effect ){
      
      Umax = find_umax( x , alternative = alternative, confidence = confidence ,
                        common.effect = TRUE , t = Alpha.tilde)
      
      if( Umax$side == 'less '){
        res$u_L <-  Umax$u_max 
        res$u_R <- NULL
      }else{
        res$u_L <-  NULL 
        res$u_R <- Umax$u_max 
      }
      
      
    }else{
      
      if( ! alternative %in% c('less','None') ){
        Umax_right = find_umax( x , alternative = 'greater',
                                confidence = 1 - alpha/(1+(alternative == 'two-sided')) ,
                                common.effect  = FALSE , t = Alpha.tilde)
        res$u_R <- Umax_right$u_max
      }
      
      if( ! alternative %in% c('greater','None')  ){
        Umax_left = find_umax( x , alternative = 'less', 
                               confidence = 1 - alpha/(1+(alternative == 'two-sided')) ,
                               common.effect = FALSE , t = Alpha.tilde)
        
        res$u_L <- Umax_left$u_max
      }
      
    }
    
  }
  ## 
  ## Replicability analysis results
  ##
  res$r.value <- rvalue 
  res$side <- side
  res$worst.case.studies <- (rvalue.results$worst.case)$studlab
  if( common.effect ){
    res$repl.method <- 'Common-effect' 
  }else{
    res$repl.method <- ifelse( t == 1,  'Truncated-Pearson' , 'Pearson')
  }
  
  
  ##
  class(res) <- c("metarep", class(res))
  res
  
}
