#'@rdname Predictability
#'@title Computing scores of predictability using two dynamical proxies 
#'based on dynamical systems theory.
#'
#'@author Carmen Alvarez-Castro, \email{carmen.alvarez-castro@cmcc.it}
#'@author Maria M. Chaves-Montero, \email{mdm.chaves-montero@cmcc.it}
#'@author Veronica Torralba, \email{veronica.torralba@cmcc.it}
#'@author Davide Faranda, \email{davide.faranda@lsce.ipsl.fr}
#'
#'@description This function divides in terciles the two dynamical proxies 
#'computed with CST_ProxiesAttractor or ProxiesAttractor. These terciles will
#'be used to measure the predictability of the system in dyn_scores. When the
#'local dimension 'dim' is small and the inverse of persistence 'theta' is 
#'small the predictability is high, and viceversa. 
#'
#'@references Faranda, D., Alvarez-Castro, M.C., Messori, G., Rodriguez, D., 
#'and Yiou, P. (2019). The hammam effect or how a warm ocean enhances large 
#'scale atmospheric predictability.Nature Communications, 10(1), 1316. 
#'\doi{10.1038/s41467-019-09305-8}"
#'@references Faranda, D., Gabriele Messori and Pascal Yiou. (2017).
#'Dynamical proxies of North Atlantic predictability and extremes. 
#'Scientific Reports, 7-41278, 2017.
#'
#'@param dim An array of N named dimensions containing the local dimension as 
#'  the output of CST_ProxiesAttractor or ProxiesAttractor. 
#'@param theta An array of N named dimensions containing the inverse of the 
#'  persistence 'theta' as the output of CST_ProxiesAttractor or 
#'  ProxiesAttractor.
#'@param ncores The number of cores to use in parallel computation.
#'
#'@return A list of length 2:
#'\itemize{
#'  \item{'pred.dim', a list of two lists 'qdim' and 'pos.d'. The 'qdim' list
#'        contains values of local dimension 'dim' divided by terciles: 
#'        d1: lower tercile (high predictability), 
#'        d2: middle tercile, 
#'        d3: higher tercile (low predictability)
#'        The 'pos.d' list contains the position of each tercile in parameter 
#'        'dim'.}
#'\item{'pred.theta', a list of two lists 'qtheta' and 'pos.t'. 
#'      The 'qtheta' list contains values of the inverse of persistence 'theta' 
#'      divided by terciles: 
#'      th1: lower tercile (high predictability), 
#'      th2: middle tercile, 
#'      th3: higher tercile (low predictability)
#'      The 'pos.t' list contains the position of each tercile in parameter 
#'      'theta'.}
#'}
#'@return dyn_scores values from 0 to 1. A dyn_score of 1 indicates the highest
#'predictability.
#'
#'@examples
#'# Creating an example of matrix dat(time,grids):
#'m <- matrix(rnorm(2000) * 10, nrow = 50, ncol = 40)
#'names(dim(m)) <- c('time', 'grid')
#'# imposing a threshold
#'  quanti <-  0.90
#'# computing dyn_scores from parameters dim and theta of the attractor
#' attractor <- ProxiesAttractor(dat = m, quanti = 0.60)
#' predyn <- Predictability(dim = attractor$dim, theta = attractor$theta)
#'@export 
Predictability <- function(dim, theta, ncores = NULL) {
  
  if (any(names(dim(dim)) %in% 'sdate')) {
    if (any(names(dim(dim)) %in% 'ftime')) {
      dim <- MergeDims(dim, merge_dims = c('ftime', 'sdate'),
                        rename_dim = 'time')
    }
  }
  if (!(any(names(dim(dim)) %in% 'time'))){
    stop("Parameter 'dim' must have a temporal dimension named 'time'.")
  }
  
  if (any(names(dim(theta)) %in% 'sdate')) {
    if (any(names(dim(theta)) %in% 'ftime')) {
      theta <- MergeDims(theta, merge_dims = c('ftime', 'sdate'),
                        rename_dim = 'time')
    }
  }
  if (!(any(names(dim(theta)) %in% 'time'))){
    stop("Parameter 'data' must have a temporal dimension named 'time'.")
  }
  
  pred <- Apply(list(dim, theta), target_dims = 'time',
                                 fun = .predictability,  
                                 ncores = ncores)
  dim(pred$dyn_scores) <- dim(theta)
  return(list(pred.dim = list(qdim = list(pred$qdim.d1,pred$qdim.d2,pred$qdim.d3),
             pos.d = list(pred$pos.d1,pred$pos.d2,pred$pos.d3)),
  pred.theta = list(qtheta = list(pred$qtheta.th1,pred$qtheta.th2,pred$qtheta.th3),
            pos.t = list(pred$pos.th1,pred$pos.th2,pred$pos.th3)),
              dyn_scores = pred$dyn_scores))
}
.predictability <- function(dim, theta) {
  if (is.null(dim)) {
    stop("Parameter 'dim' cannot be NULL.")
  }  
  if (is.null(theta)) {
    stop("Parameter 'theta' cannot be NULL.")
  }    
  if (length(dim) != length(theta)) {
    stop("Parameters 'dim' and 'theta' must have the same length")
  }    
  pos <- c(1:length(dim))
  
  # dim
  qd1 <- quantile(dim, 0.33, na.rm = TRUE)
  qd3 <- quantile(dim, 0.66, na.rm = TRUE)
  d3 <- which(dim >= qd3)
  d1 <- which(dim <= qd1)
  d2 <- which(dim > qd1 & dim < qd3)
  qdim <- list(d1 = dim[d1], d2 = dim[d2], d3 = dim[d3])
  pos.d <- list(d1=d1,d2=d2,d3=d3)

  #theta
  qt1 <- quantile(theta, 0.33, na.rm = TRUE)
  qt3 <- quantile(theta, 0.66, na.rm = TRUE)
  th3 <- which(theta >= qt3)
  th1 <- which(theta <= qt1)
  th2 <- which(theta > qt1 & theta < qt3)
  qtheta <- list(th1 = theta[th1], th2 = theta[th2], th3 = theta[th3])
  pos.t <- list(th1 = th1, th2 = th2, th3 = th3)
  
  #scores position
  d1th1 <- pos.d$d1[which(pos.d$d1 %in% pos.t$th1)]
  d1th2 <- pos.d$d1[which(pos.d$d1 %in% pos.t$th2)]
  d2th1 <- pos.d$d2[which(pos.d$d2 %in% pos.t$th1)]
  d1th3 <- pos.d$d1[which(pos.d$d1 %in% pos.t$th3)]
  d3th1 <- pos.d$d3[which(pos.d$d3 %in% pos.t$th1)]
  d2th2 <- pos.d$d2[which(pos.d$d2 %in% pos.t$th2)]
  d2th3 <- pos.d$d2[which(pos.d$d2 %in% pos.t$th3)]
  d3th2 <- pos.d$d3[which(pos.d$d3 %in% pos.t$th2)]
  d3th3 <- pos.d$d3[which(pos.d$d3 %in% pos.t$th3)]
  #scores values
  dyn_scores <- c(1:length(dim))
  dyn_scores[which(pos %in% d1th1)]<- 9/9
  dyn_scores[which(pos %in% d1th2)]<- 8/9
  dyn_scores[which(pos %in% d2th1)]<- 7/9
  dyn_scores[which(pos %in% d1th3)]<- 6/9
  dyn_scores[which(pos %in% d3th1)]<- 5/9
  dyn_scores[which(pos %in% d2th2)]<- 4/9
  dyn_scores[which(pos %in% d2th3)]<- 3/9
  dyn_scores[which(pos %in% d3th2)]<- 2/9
  dyn_scores[which(pos %in% d3th3)]<- 1/9
  
return(list(qdim.d1 = dim[d1], qdim.d2 = dim[d2], qdim.d3 = dim[d3],
            pos.d1 = d1, pos.d2 = d2, pos.d3 =d3,
            qtheta.th1 = theta[th1], qtheta.th2 = theta[th2], qtheta.th3 = theta[th3], 
            pos.th1 = th1, pos.th2 = th2, pos.th3 = th3,
            dyn_scores = dyn_scores))
}
  
 

 
