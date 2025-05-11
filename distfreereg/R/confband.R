confband <- function(x, w, func, m, batch_len, N, conf.level, buffer, matsqrt_tol){
  n <- length(x); b <- batch_len; a <- n/b
  if(!isInteger(a)) warning("Batch length, ", b, ", does not evenly divide data length, ", n)
  buffer <- diff(range(x))*buffer
  if(is.null(w)) w <- seq(from = min(x) + buffer, to = max(x) - buffer, length.out = m)
  fnw <- func(x = x)(w)
  Sigma_hat <- tryCatch(confband_Sigma_hat(x, fnw = fnw, func = func, w = w, a = a, b = b),
                        error = function(e) stop("An confband error occurred when trying to calculate Sigma_hat: ", e))
  radii <- tryCatch(confband_radii(N = N, Sigma_hat = Sigma_hat, df = a - 1,
                                   conf.level = conf.level, n = n, matsqrt_tol = matsqrt_tol),
                    error = function(e) stop("An confband error occurred when trying to calculate radii: ", e))
  output <- list(call = match.call(), Sigma_hat = Sigma_hat, radii = radii, w = w,
                 fnw = fnw, cb_lower = fnw - radii, cb_upper = fnw + radii)
  return(output)
}
