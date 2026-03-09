#' Gillespie algorithm for mRNA generating processes
#'
#' Gillespie algorithms allow synthetic data simulation via three different
#' underlying mRNA generating processes: the basic process consists of a
#' simple death-birth model of mRNA transcription and degradation; the
#' switching process considers additionally gene activation and deactivation,
#' with mRNA transcription only happening in active gene states; the
#' bursting process, transcribes mRNA in bursts with geometrically distributed burst sizes.
#' The basic_burst model combines both the basic and the burst model.
#' The IGbasic burst model describes the basic model with non-constant transcription rates, but
#' transcription rates follow an inverse Gaussian distribution governed by one parameter, the mean parameter
#' of the inverse Gaussian distribution. Additionally a burst transcription occures (with NB distributed
#' burst sizes), the whole burst (rate and burst sizes) are determined by the rate parameter.
#'

#' @param n Number of observations
#' @param r.degr mRNA degradation rate (all models)
#' @param r.act DNA activation rate (Switching Model)
#' @param r.deact DNA deactivation rate (Switching Model)
#' @param r.on Transcription rate during gene activation (Switching model)
#' @param r.burst Bursty transcription rate (Bursting model, Basic Burst model and IG Basic Burst model)
#' @param s.burst Mean burst size (Bursting Model and  Basic Burst model)
#' @param r.mu Mean parameter for the inverse Gaussian distribution (IG Basic Burst model)
#' @name gmRNA
#' @rdname gmRNA
#' @export
#' @examples
#' x <- gmRNA_basic(100, 0.75, 0.001)
#' plot(density(x))
gmRNA_basic <- function(n, r.on, r.degr) {
  cpp_gmRNA_basic(n, r.on, r.degr)
}


#' @rdname gmRNA
#' @export
#' @examples
#' x <- gmRNA_switch(100, 0.23, 0.15, 0.75, 0.001)
#' plot(density(x))
gmRNA_switch <- function(n, r.act, r.deact, r.on, r.degr) {
  cpp_gmRNA_switch(n, r.act, r.deact, r.on, r.degr)
}


#' @rdname gmRNA
#' @export
#' @examples
#' x <- gmRNA_burst(10, 0.15, 0.75, 0.001)
#' plot(density(x))
gmRNA_burst <- function(n, r.burst, s.burst, r.degr) {
  cpp_gmRNA_burst(n, r.burst, s.burst, r.degr)
}


#' @rdname gmRNA
#' @export
#' @examples
#' x <- gmRNA_basic_burst(10, 0.75, 0.15, 0.5, 0.001)
#' plot(density(x))
gmRNA_basic_burst <- function(n, r.on, r.burst, s.burst, r.degr) {
    cpp_gmRNA_basic_burst(n, r.on, r.burst, s.burst, r.degr)
}

#' #' @rdname gmRNA
#' #' @export
#' #' @examples
#' #' x <- gmRNA_IGbasic_burst(10, 2, 0.5, 0.001)
#' gmRNA_IGbasic_burst <- function(n, r.mu, r.burst, r.degr) {
#'     cpp_gmRNA_IGbasic_burst(n, r.mu, r.burst, r.degr)
#' }


#' @rdname gmRNA
#' @export
#' @importFrom stats rexp
#' @examples
#' x <- gmRNA_IGbasic_burst(10, 2, 0.5, 0.1)
#' plot(density(x))
gmRNA_IGbasic_burst <- function( n, r.mu, r.burst, r.degr) {
  res <- c()
  t0 = 0
  x0 = 0
  tmax = 20/r.degr
  i = 1

  while( i <= n) {
    x = x0
    tx = t0

    lambda_draw = rInvGaus(1, r.mu, r.mu * r.burst)
    lambda1 = lambda_draw[1]
    lambda2 = r.burst
    lambda3 = r.degr * x
    lambdax = lambda1 + lambda2 + lambda3

    tau_vec = rexp(1, lambdax)
    tau = tau_vec[1]
    tau_stern = min(tau, tmax - tx)
    tx = tx+ tau_stern
    while(tx < tmax) {
      u_vec = runif(1)
      u = u_vec[1]
      if(u <= lambda1/lambdax){
        k = 1
      } else if(u <= (lambda1 + lambda2)/lambdax){
        k = 2} else k = 3


      if(tau <= tau_stern) {
        if(k == 1){
          x = x + 1
        } else if(k == 2) {
          r_vec = rnbinom(1, 1/2, r.burst/(r.burst + 2 * r.mu))
          x = x + r_vec[1]
        } else {
          x= x - 1
        }

      }


      lambda_draw = rInvGaus(1, r.mu, r.mu * r.burst)
      lambda1 = lambda_draw[1]
      lambda2 = r.burst
      lambda3 = r.degr * x
      lambdax = lambda1 + lambda2 + lambda3


      tau_vec = rexp(1, lambdax)
      tau = tau_vec[1]
      tau_stern = min(tau, tmax - tx)
      tx =  tx + tau_stern
    }
    res[i] = x
    i <- i + 1
  }
  res
}


