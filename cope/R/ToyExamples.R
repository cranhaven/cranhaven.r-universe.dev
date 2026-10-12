#' Return the Toy Signal.
#'
#' @param ImRange A vector with two components giving the range of the region on
#'               which the Toy Signal is to be computed.
#' @param NPixel Number of pixels of the result in one direction. The resulting
#'               picture will have NPixel x NPixel pixels.           
#' @return A list with components "x", "y" and "z". Here, x and y are the 
#'         coordinates of the grid and z is matrix of dimensions 
#'         c(NPixel,NPixel) giving the Toy Signal.
#' @export
ToySignal = function(ImRange = c(0,1), NPixel = 64){
  
  s = seq(0, 10, length.out = NPixel)
  ds = s[2] - s[1]
  
  #Create single peak test signal.
  single.peak = function(sx,sy,x, y, b) {
    matrix(mvtnorm::dmvnorm(expand.grid(sx,sy),mean=c(x,y),sigma=diag(c(b,b))),
           nrow=length(sy))
  }
  
  #Defining the signal.
  mu1 = 50*single.peak(s, s, 2.6, 5.7, 1.5) + 
    100*single.peak(s, s, 6.8, 7.8, 2.5) +
    50*single.peak(s, s, 7.8, 2.1, 1.3) 
  mu = mu1/max(mu1) * 3
  
  s = seq(from = ImRange[1], to = ImRange[2], length.out = NPixel)
  
  list(x=s,y=s,z=mu)
} 

#' Return the Toy Signal with discontinuities.
#'
#' @param ImRange A vector with two components giving the range of the region on
#'               which the Toy Signal is to be computed.
#' @param NPixel Number of pixels of the result in one direction. The resulting
#'               picture will have NPixel x NPixel pixels.           
#' @return A list with components "x", "y" and "z". Here, x and y are the 
#'         coordinates of the grid and z is matrix of dimensions 
#'         c(NPixel,NPixel) giving the Toy Signal.
#' @export
ToySignalDC = function(ImRange = c(0,1), NPixel = 64){
  
  N2 <- round(NPixel / 2)
  
  mu <- matrix(rep(seq(from = 0, to = 1, length.out = NPixel), NPixel), 
                NPixel, NPixel)
  mu2 <- matrix(rep(seq(from = 1/2, to = 3/4, length.out = N2), N2), N2, N2)
  
  startInd <- round(NPixel / 4)
  mu[(startInd + 1):(startInd + N2), 
      (startInd + 1):(startInd + N2)] <- mu2
 
  s <- seq(from = ImRange[1], to = ImRange[2], length.out = NPixel)
  list(x=s,y=s,z=mu)
}

#' The toy slope.
#' 
#' @param ImRange A vector with two components giving the range of the region on
#'               which the Toy Slope is to be computed.
#' @param NPixel Number of pixels of the result in one direction. The resulting
#'               picture will have NPixel x NPixel pixels.           
#' @return A list with components "x", "y" and "z". Here, x and y are the 
#'         coordinates of the grid and z is matrix of dimensions 
#'         c(NPixel,NPixel) giving the Toy Signal.
#' @export
ToySlope <- function(ImRange = c(0, 1), NPixel = 64){
 m <- matrix(rep(1:NPixel, NPixel), NPixel, NPixel) 
 m <- (m - mean(m))
 m <- t(m / max(m))
 
 s = seq(from = ImRange[1], to = ImRange[2], length.out = NPixel)
 list(x=s,y=s,z=m)
}

#' Generate a realization of the Toy Noise 1.
#'
#' @param n The number of realizations to produce.
#' @param Ns Number of pixels of the result in one direction. The resulting
#'               picture will have Ns x Ns pixels. 
#' @param model The correlation structure of the noise, as used by arima.sim.
#'              Default is list() which gives i.i.d. noise.
#' @param theta Bandwidth of kernel used to smooth the noise.
#' @param l1,l2 Pixel size of the noise blocks in either side of the domain.
#'               See main reference for details.
#' @param tau Scaling factor with which noise is multiplied after generation.
#' @importFrom stats arima.sim
#' @return A list containing x and y, the coordinates of the grid and
#'        z and array of dimensions c(64,64,n) giving n reallizations of the 
#'        Toy Noise 1.
#' @export
ToyNoise1 <- function(n = 1, Ns = 64, model = list(), theta = 0.1,
                      l1 = 1, l2 = 4, tau = 12){
  
  s <- seq(0, 1, length.out = Ns) # Grid coordinates.
  ds <- s[2] - s[1]
  
  Z1 <- matrix(n, Ns / (2 * l1), Ns / l1)
  Z1 <- apply(Z1, 1:2, function(n){ arima.sim(n, model = model)})
  if(n > 1) Z1 <- aperm(Z1, c(2, 3, 1))
  Z1 <- kronecker(Z1, matrix(1, l1, l1))
  
  Z2 <- matrix(n, Ns / (2 * l2), Ns / l2)
  Z2 <- apply(Z2, 1:2, function(n){arima.sim(n, model = model)})
  if(n > 1) Z2 <- aperm(Z2, c(2, 3, 1))
  Z2 <- kronecker(Z2, matrix(1, l2, l2))
  
  Z <- abind::abind(Z1, Z2, along = 1)
  Z <- array(Z, c(Ns, Ns, n))
  
  for(i in 1:n){
    Z[, , i] <- fields::image.smooth(Z[, , i], theta = theta, 
                                     dx = ds, dy = ds)$z
  }
  
  if(n == 1) Z <- matrix(Z, Ns, Ns)
  list(x = s, y = s, z = tau * Z)
}


#' Generate a realization of the Toy Noise 1 before smoothing.
#'
#' @param n The number of realizations to produce.
#' @param Ns Number of pixels of the result in one direction. The resulting
#'               picture will have Ns x Ns pixels. 
#' @param model The correlation structure of the noise, as used by arima.sim.
#'              Default is list() which gives i.i.d. noise.
#' @param theta Bandwidth of kernel used to smooth the noise.
#' @param l1,l2 Pixel size of the noise blocks in either side of the domain.
#'               See main reference for details.
#' @param tau Scaling factor with which noise is multiplied after generation.
#' @return A list containing x and y, the coordinates of the grid and
#'        z and array of dimensions c(64,64,n) giving n reallizations of the 
#'        Toy Noise 1 before smoothing.
#' @export
ToyNoise1Presmooth <- function(n = 1, Ns = 64, model = list(), theta = 0.1,
                      l1 = 1, l2 = 4, tau = 12){
  
  s <- seq(0, 1, length.out = Ns) # Grid coordinates.
  ds <- s[2] - s[1]
  
  Z1 <- matrix(n, Ns / (2 * l1), Ns / l1)
  Z1 <- apply(Z1, 1:2, function(n){ arima.sim(n, model = model)})
  if(n > 1) Z1 <- aperm(Z1, c(2, 3, 1))
  Z1 <- kronecker(Z1, matrix(1, l1, l1))
  
  Z2 <- matrix(n, Ns / (2 * l2), Ns / l2)
  Z2 <- apply(Z2, 1:2, function(n){arima.sim(n, model = model)})
  if(n > 1) Z2 <- aperm(Z2, c(2, 3, 1))
  Z2 <- kronecker(Z2, matrix(1, l2, l2))
  
  Z <- abind::abind(Z1, Z2, along = 1)
  Z <- array(Z, c(Ns, Ns, n))
  
  
  if(n == 1) Z <- matrix(Z, Ns, Ns)
  list(x = s, y = s, z = Z)
}



#' Generate a realization of the Toy Noise 2.
#'
#' @param n The number of realizations to produce.
#' @param Ns Number of pixels of the result in one direction. The resulting
#'               picture will have Ns x Ns pixels. 
#' @param model The correlation structure of the noise, as used by arima.sim.
#'              Default is list() which gives i.i.d. noise.
#' @param theta Bandwidth of kernel used to smooth the noise.
#' @param l1,l2 Pixel size of the noise blocks in either side of the domain.
#'               See main reference for details.
#' @param tau Scaling factor with which noise is multiplied after generation.
#' @importFrom stats arima.sim
#' @return A list containing x and y, the coordinates of the grid and
#'        z and array of dimensions c(64,64,n) giving n reallizations of the 
#'        Toy Noise 2.
#' @export
ToyNoise2 <- function(n = 1, Ns = 64, model = list(), theta = 0.1,
                      l1 = 1, l2 = 4, tau = 40){
  
  s <- seq(0, 1, length.out = Ns) # Grid coordinates.
  ds <- s[2] - s[1]
  
  Z1 <- matrix(n, Ns / (2 * l1), Ns / l1)
  Z1 <- apply(Z1, 1:2, function(n){ arima.sim(n, model = model)})
  if(n > 1) Z1 <- aperm(Z1, c(2, 3, 1))
  Z1 <- kronecker(Z1, matrix(1, l1, l1))
  
  Z2 <- matrix(n, Ns / (2 * l2), Ns / l2)
  Z2 <- apply(Z2, 1:2, function(n){arima.sim(n, model = model)})
  if(n > 1) Z2 <- aperm(Z2, c(2, 3, 1))
  Z2 <- kronecker(Z2, matrix(1, l2, l2))
  
  Z <- abind::abind(Z1, Z2, along = 1)
  Z <- array(Z, c(Ns, Ns, n))
  
  laplaceker = function(x) fields::double.exp(sqrt(x))
  for(i in 1:n){
    Z[, , i] <- fields::image.smooth(Z[, , i], theta = theta, 
                                     dx = ds, dy = ds, 
                                     kernel.function = laplaceker)$z
  }
  
  
  if(n == 1) Z <- matrix(Z, Ns, Ns)
  list(x = s, y = s, z = tau * Z)
}

#' Generate a realization of the Toy Noise 3.
#'
#' @param n The number of realizations to produce.
#' @param Ns Number of pixels of the result in one direction. The resulting
#'               picture will have Ns x Ns pixels. 
#' @param model The correlation structure of the noise, as used by arima.sim.
#'              Default is list() which gives i.i.d. noise.
#' @param theta Bandwidth of kernel used to smooth the noise.
#' @param l1,l2 Pixel size of the noise blocks in either side of the domain.
#'               See main reference for details.
#' @param tau Scaling factor with which noise is multiplied after generation.
#' @importFrom stats rbinom arima.sim rexp
#' @return A list containing x and y, the coordinates of the grid and
#'        z and array of dimensions c(64,64,n) giving n reallizations of the 
#'        Toy Noise 3.
#' @export
ToyNoise3 <- function(n = 1, Ns = 64, model = list(), theta = 0.05,
                      l1 = 1, l2 = 4, tau = 10){
  
  s <- seq(0, 1, length.out = Ns) # Grid coordinates.
  ds <- s[2] - s[1]
  
  rlaplace = function(n,b=1){
    (2*rbinom(n,size=1,prob=0.5)-1)*rexp(n,1/b)
  }
  
  Z1 <- matrix(n, Ns / (2 * l1), Ns / l1)
  Z1 <- apply(Z1, 1:2, function(n){ arima.sim(n, rand.gen = rlaplace,
                                              model = model)})
  if(n > 1) Z1 <- aperm(Z1, c(2, 3, 1))
  Z1 <- kronecker(Z1, matrix(1, l1, l1))
  
  Z2 <- matrix(n, Ns / (2 * l2), Ns / l2)
  Z2 <- apply(Z2, 1:2, function(n){arima.sim(n, rand.gen = stats::rt, model = model, 
                                             df = 10)})
  if(n > 1) Z2 <- aperm(Z2, c(2, 3, 1))
  Z2 <- kronecker(Z2, matrix(1, l2, l2))
  
  Z <- abind::abind(Z1, Z2, along = 1)
  Z <- array(Z, c(Ns, Ns, n))
  
  laplaceker = function(x) fields::double.exp(sqrt(x))
  for(i in 1:n){
    Z[, , i] <- fields::image.smooth(Z[, , i], theta = theta, 
                                     dx = ds, dy = ds, 
                                     kernel.function = laplaceker)$z
  }
  
  if(n == 1) Z <- matrix(Z, Ns, Ns)
  list(x = s, y = s, z = tau * Z)
}

#' Generate the AR coefficient map.
#'
#' @param Ns Number of pixels of the result in one direction. The resulting
#'               picture will have Ns x Ns pixels. 
#' @return A list containing x and y, the coordinates of the grid and
#'        z, a matrix of dimensions Ns x Ns giving the AR coefficients map.
#' @export
ARCoeffMap <- function(Ns = 64){
  
  s <- seq(0, 1, length.out = Ns) # Grid coordinates.

  # Parameters.
  Z <- outer(s, s, 
             FUN = function(x, y) stats::qnorm(0.99 - 0.98 * (x + y) / 2, mean = 0.1, sd = 0.125))
  
  list(x = s, y = s, z = Z)
}

#' Generate an AR sequence with the ToyFUN as base noise and the AR coefficients
#' given by the ARCoeffMap.
#' 
#' @param n Length of sequence.
#' @param ToyFUN Base noise to build the sequence from.
#' @param ... Additional parameters passed to ToyFUN.
#' @return A list containing x and y, the coordinates of the grid and
#'        z and array of dimensions c(64,64,n) giving n realizations of the 
#'        Toy Noise.
#' @export
ToyNoiseMap <- function(n = 1, ToyFUN, ...){
  Z <- get(ToyFUN)(n = n, ...)$z
  Ns <- dim(Z)[1]
  s <- seq(0, 1, length.out = Ns) # Grid coordinates.
  
  ARMap <- ARCoeffMap(Ns = Ns)$z
  for(i in 1:Ns){
    for(j in 1:Ns){
      Z[i, j, ] <- arima.sim(n = n, model = list(ar = ARMap[i, j]),  
                             innov = Z[i, j, ])
    }
  }
  
  list(x = s, y = s, z = Z)
  }
