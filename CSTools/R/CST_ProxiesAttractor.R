#'@rdname CST_ProxiesAttractor
#'@title Computing two dinamical proxies of the attractor in s2dv_cube.
#'
#'@author Carmen Alvarez-Castro, \email{carmen.alvarez-castro@cmcc.it}
#'@author Maria M. Chaves-Montero, \email{mdm.chaves-montero@cmcc.it}
#'@author Veronica Torralba, \email{veronica.torralba@cmcc.it}
#'@author Davide Faranda, \email{davide.faranda@lsce.ipsl.fr}
#'
#'@description This function computes two dinamical proxies of the attractor: 
#'The local dimension (d) and the inverse of the persistence (theta) for an
#''s2dv_cube' object.
#'These two parameters will be used as a condition for the computation of 
#'dynamical scores to measure predictability and to compute bias correction 
#'conditioned  by the dynamics with the function DynBiasCorrection Function 
#'based on the matlab code (davide.faranda@lsce.ipsl.fr) used in 
#'@references Faranda, D., Alvarez-Castro, M.C., Messori, G., Rodriguez, D., 
#'and Yiou, P. (2019). The hammam effect or how a warm ocean enhances large 
#'scale atmospheric predictability. Nature Communications, 10(1), 1316. 
#'\doi{10.1038/s41467-019-09305-8}"
#'@references Faranda, D., Gabriele Messori and Pascal Yiou. (2017).
#'Dynamical proxies of North Atlantic predictability and extremes. 
#'Scientific Reports, 7-41278, 2017.
#'
#'@param data An s2dv_cube object with the data to create the attractor. Must be 
#'  a matrix with the timesteps in nrow and the grids in ncol(dat(time,grids)
#'@param quanti A number lower than 1 indicating the quantile to perform the 
#'  computation of local dimension and theta.
#'@param ncores The number of cores to use in parallel computation.
#'@return dim and theta
#'@examples
#'# Example 1: Computing the attractor using simple s2dv data
#'obs <- rnorm(2 * 3 * 4 * 8 * 8)
#'dim(obs) <- c(dataset = 1, member = 2, sdate = 3, ftime = 4, lat = 8, lon = 8)
#'lon <- seq(10, 13.5, 0.5)
#'lat <- seq(40, 43.5, 0.5)
#'coords <- list(lon = lon, lat = lat)
#'data <- list(data = obs, coords = coords)
#'class(data) <- "s2dv_cube"
#'attractor <- CST_ProxiesAttractor(data = data, quanti = 0.6)
#'@import multiApply
#'@export
CST_ProxiesAttractor <- function(data, quanti, ncores = NULL) {
  # Check 's2dv_cube'
  if (!inherits(data, 's2dv_cube')) {
    stop("Parameter 'data' must be of the class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  # Check quanti
  if (is.null(quanti)) {
    stop("Parameter 'quanti' cannot be NULL.")
  }        
  
  data$data <- ProxiesAttractor(data = data$data, quanti = quanti, 
                                ncores = ncores)

  return(data)
}
#'@rdname ProxiesAttractor
#'
#'@title Computing two dinamical proxies of the attractor.
#'@author Carmen Alvarez-Castro, \email{carmen.alvarez-castro@cmcc.it}
#'@author Maria M. Chaves-Montero, \email{mdm.chaves-montero@cmcc.it}
#'@author Veronica Torralba, \email{veronica.torralba@cmcc.it}
#'@author Davide Faranda, \email{davide.faranda@lsce.ipsl.fr}
#'
#'@description This function computes two dinamical proxies of the attractor: 
#'The local dimension (d) and the inverse of the persistence (theta). 
#'These two parameters will be used as a condition for the computation of dynamical 
#'scores to measure predictability and to compute bias correction conditioned by
#'the dynamics with the function DynBiasCorrection.  
#'Funtion based on the matlab code (davide.faranda@lsce.ipsl.fr) used in:
#'@references Faranda, D., Alvarez-Castro, M.C., Messori, G., Rodriguez, D., and 
#'Yiou, P. (2019). The hammam effect or how a warm ocean enhances large scale 
#'atmospheric predictability. Nature Communications, 10(1), 1316. 
#'\doi{10.1038/s41467-019-09305-8}"
#'@references Faranda, D., Gabriele Messori and Pascal Yiou. (2017).
#' Dynamical proxies of North Atlantic predictability and extremes. 
#' Scientific Reports, 7-41278, 2017. 
#'
#'@param data A multidimensional array with named dimensions to create the 
#'  attractor. It requires a temporal dimension named 'time' and spatial 
#'  dimensions called 'lat' and 'lon', or 'latitude' and 'longitude' or 'grid'. 
#'@param quanti A number lower than 1 indicating the quantile to perform the 
#'  computation of local dimension and theta
#'@param ncores The number of cores to use in parallel computation.
#'
#'@return dim and theta
#' 
#'@examples
#'# Example 1: Computing the attractor using simple data
#'# Creating an example of matrix data(time,grids):
#'mat <- array(rnorm(36 * 40), c(time = 36, grid = 40)) 
#'qm <- 0.90 # imposing a threshold
#'Attractor <- ProxiesAttractor(data = mat, quanti = qm)
#'# to plot the result
#'time = c(1:length(Attractor$theta))
#'plot(time, Attractor$dim, xlab = 'time', ylab = 'd',
#'     main = 'local dimension', type = 'l')
#'@import multiApply
#'@export
ProxiesAttractor <- function(data, quanti, ncores = NULL){
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }  
  if (is.null(quanti)) {
    stop("Parameter 'quanti' is mandatory")
  }    
   if (any(names(dim(data)) %in% 'sdate')) {
    if (any(names(dim(data)) %in% 'ftime')) {
      data <- MergeDims(data, merge_dims = c('ftime', 'sdate'),
                        rename_dim = 'time')
    }
  }
  if (!(any(names(dim(data)) %in% 'time'))){
    stop("Parameter 'data' must have a temporal dimension named 'time'.")
  }
  if (any(names(dim(data)) %in% 'lat')) {
    if (any(names(dim(data)) %in% 'lon')) {
      data <- MergeDims(data, merge_dims = c('lon', 'lat'),
                             rename_dim = 'grid')
    }
  }
  if (any(names(dim(data)) %in% 'latitude')) {
    if (any(names(dim(data)) %in% 'longitude')) {
      data <- MergeDims(data, merge_dims = c('longitude', 'latitude'),
                             rename_dim = 'grid')
    }
  }
  if(!(any(names(dim(data)) %in% 'grid'))){
    stop("Parameter 'data' must have a spatial dimension named 'grid'.")
  }
  attractor <- Apply(data, target_dims = c('time', 'grid'),
                     fun = .proxiesattractor,
                     quanti = quanti , ncores = ncores)
  # rename dimensions
  attractor <- lapply(attractor, 
                      FUN = function(x, dimname){ 
                        names(dim(x))[dimname] <- 'time'
                        return(x)}, 
                      dimname = which(names(dim(attractor[[1]])) == 'dim2'))
   return(list(dim = attractor$dim, theta = attractor$theta))
}

.proxiesattractor <- function(data, quanti) {
  # expected dimensions data: time and grid
  logdista <- Apply(data, target_dims =  'grid',
                    fun = function(x, y){
                      -log(colMeans((y - as.vector(x))^2))},
                    y = t(data))[[1]]
  
  #Computation of theta
  Theta <- function(logdista, quanti){
    #Compute the thheshold corresponding to the quantile
    thresh  <- quantile(logdista, quanti, na.rm = TRUE)
    logdista[which(logdista == 'Inf')] <-  NaN
    Li <- which(as.vector(logdista) > as.numeric(thresh))
    #Length of each cluster
    Ti <- diff(Li)
    N <-  length(Ti)
    q <-  1 - quanti
    Si <- Ti - 1
    Nc <- length(which(Si > 0))
    N <-  length(Ti)
    theta <- (sum(q * Si) + N + Nc - sqrt(((sum(q * Si) + N + Nc)^2) - 
             8 * Nc * sum(q * Si))) / (2 * sum(q * Si))
    #Sort the exceedances  
    logdista <- sort(logdista)
    #Find all the Peaks over Thresholds.
    findidx <- which(as.vector(logdista) > as.numeric(thresh))
    if(length(findidx) < 1) {
      stop("Parameter 'quanti' is too high for the length of the data provided.")
    }
    logextr <- logdista[findidx[[1]]:(length(logdista) - 1)]
    #The inverse of the dimension is just the average of the exceedances
    dim <- 1 /mean(as.numeric(logextr) - as.numeric(thresh))
    return(list(dim = dim, theta = theta))
  }
  names(dim(logdista)) <- c('dim1', 'dim2')
  proxies <- Apply(data = list(logdista = logdista),
                  target_dims = list('dim1'), fun = Theta, quanti = quanti)
  
  return(list(dim = proxies$dim, theta = proxies$theta))
}

