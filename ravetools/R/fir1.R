sineint <- function(x) {
  neg <- x < 0
  x[neg] <- -x[neg]

  re <- Im( pracma::expint( 1i * x ) ) + pi/2
  re[neg] <- -re[neg]
  re[x == 0] <- 0
  re
}


scale_filter <- function(b, fband, freq, len) {

  if( fband ) {
    b <- b / sum(b)
  } else {
    if( freq[[4]] == 1 ) {
      # length(w) == 1
      f0 <- 1
    } else {
      f0 <- mean(freq[c(3,4)])
    }
    # b = b / abs( exp(-1i*2*pi*(0:L-1)*(f0/2))*(b.') );
    b <- b / Mod(sum(
      exp(-1i*2*pi*seq(0, len - 1)*(f0/2)) * b
    ))
  }

  b

}

fir_validate <- function(n, max_freq, magnitude, odd_allowed = FALSE) {

  # order
  if(missing(n) || length(n) != 1 || !is.numeric(n) || n - round(n) != 0 || n <= 0 ) {
    stop("Filter order `n` must be a positive integer")
  }

  n_is_odd <- n %% 2 == 1

  if( magnitude[[length(magnitude)]] != 0 && max_freq == 1 && n_is_odd && !odd_allowed ) {
    warning("IR filters must have a gain of zero at the Nyquist frequency. Increasing the order by 1")
    n <- n + 1
  }

  n
}


fir1 <- function(
    n, w, type = c("low", "high", "stop", "pass", "DC-0", "DC-1"),
    window = hamming, scale = TRUE, hilbert = FALSE
) {
  type <- match.arg(type)

  nw <- length(w)
  if(!nw || any(w < 0 | w > 1)) {
    stop("`fir1`: w must be a real vector from range 0.0 to 1.0")
  }
  if(nw > 1 && any(diff(w) < 0)) {
    stop("`fir1`: w must be non-decreasing")
  }

  if(w[[length(w)]] >= 1) {
    w <- w[-length(w)]
  }

  if(missing(type)) {
    if(nw == 1) {
      type <- "low"
    } else if (nw == 2) {
      type <- 'pass'
    } else {
      type <- "DC-0"
    }
  } else {
    type <- match.arg(type)
  }
  nw <- length(w)

  nbands <- nw + 1

  # make sure default 3 band filter is bandpass
  if( type == "pass" ) {
    if( nbands > 2 ) {
      type <- "DC-0"
    } else if ( nbands == 2 ) {
      type <- "high"
    }
  }

  # frequency vector
  freq <- c(0, rep(w, each = 2), 1)

  # magnitude vector
  use_first_band <- !type %in% c("DC-0", "high")
  magnitude <- rep( (use_first_band + seq(0, nbands - 1)) %% 2, each = 2)

  n <- fir_validate(n, freq[[length(freq)]], magnitude, odd_allowed = !hilbert)

  # filter length is order+1
  filter_len <- n + 1

  if(is.function(window)) {
    wind <- window(filter_len)
    if( length(wind) != filter_len ) {
      stop("`fir1`: window length is invalid given requested length: (n+1)")
    }
  } else {
    if(length(window) != filter_len) {
      stop("`fir1`: window must be either a function or a vector of length (n+1)")
    }
    wind <- window
  }

  if(hilbert) {
    stop("`fir1`: Hilbert option has not been implemented yet")
    hh <- firls(n, freq, magnitude, ftype = "hilbert")$h
  } else {
    hh <- firls(n, freq, magnitude)$h
  }

  b <- wind * hh

  if(scale) {
    b <- scale_filter(b, use_first_band, freq, filter_len)
  }

  return(b)
}

sinc <- function(x) {
  v <- pi * x
  re <- sin(v) / v
  if(length(v)) {
    re[ v == 0 ] <- 1
  }
  re
}

firls <- function(N, freq, A, W = NULL, ftype = "") {

  if (!is.numeric(N) || length(N) != 1 || !is.finite(N) || N <= 0) {
    stop("Invalid input for N.")
  }
  N <- as.integer(N)

  if (!is.numeric(freq) || !is.numeric(A) || length(freq) != length(A) || any(freq < 0) || any(freq > 1)) {
    stop("Invalid input for freq or A.")
  }
  freq <- as.double(freq)
  A <- as.double(A)

  if (!is.null(W)) {
    if (length(W) == 0) {
      W <- rep(1, floor(length(freq) / 2))
    } else {
      W <- as.double(W)
    }
  } else {
    W <- rep(1, floor(length(freq) / 2))
  }

  if (identical(ftype, "h") || identical(ftype, "hilbert")) {
    filtype <- 1
    differ <- 0
  } else if (identical(ftype, "d") || identical(ftype, "differentiator")) {
    filtype <- 1
    differ <- 1
  } else {
    filtype <- 0
    differ <- 0
  }

  if (length(W) == 0) {
    weight <- rep(1, floor(length(freq) / 2))
  } else {
    weight <- W
  }

  freq_length <- length(freq)
  wt <- abs(sqrt(weight))

  N <- N + 1

  Fn <- freq / 2

  dF <- diff(Fn)

  if (any(dF < 0)) {
    stop("Invalid frequency vector.")
  }

  lendF <- freq_length - 1
  fullband <- FALSE

  if (lendF > 1) {
    fullband <- all(dF == 0)
  }

  tempW <- wt - wt[1]
  constant_weights <- sum(tempW) == 0

  L <- (N - 1) / 2
  Nodd <- (N %% 2 == 1)

  b0 <- 0

  if (filtype == 0) {
    if (!Nodd) {
      m <- (0:L) + 0.5
    } else {
      m <- 0:L
    }

    k <- m

    need_matrix <- !fullband || !constant_weights

    if (need_matrix) {
      result <- initMatrices(m)
      I1 <- result$I1
      I2 <- result$I2
      G <- result$G
    }

    if (Nodd) {
      k <- k[-1]
      b0 <- 0
    }

    b <- numeric(length(k))

    for (s in seq(1, length(Fn), by = 2)) {
      m_s <- (A[s + 1] - A[s]) / (Fn[s + 1] - Fn[s])
      b1 <- A[s] - m_s * Fn[s]

      if (Nodd) {
        b0 <- b0 + (b1 * (Fn[s + 1] - Fn[s]) + m_s / 2 * (Fn[s + 1]^2 - Fn[s]^2)) * wt[(s + 1) / 2]^2
      }

      b <- b + (m_s / (4 * pi^2) * (cos(2 * pi * k * Fn[s + 1]) - cos(2 * pi * k * Fn[s])) / (k^2)) * wt[(s + 1) / 2]^2
      b <- b + (Fn[s + 1] * (m_s * Fn[s + 1] + b1) * sinc(2 * k * Fn[s + 1]) -
                  Fn[s] * (m_s * Fn[s] + b1) * sinc(2 * k * Fn[s])) * wt[(s + 1) / 2]^2

      if (need_matrix) {
        G <- G + (0.5 * Fn[s + 1] * (sinc(2 * I1 * Fn[s + 1]) + sinc(2 * I2 * Fn[s + 1])) -
                    0.5 * Fn[s] * (sinc(2 * I1 * Fn[s]) + sinc(2 * I2 * Fn[s]))) * wt[(s + 1) / 2]^2
      }
    }

    if (Nodd) {
      b <- c(b0, b)
    }

    if (need_matrix) {
      a <- solve(G, b)
    } else {
      a <- (wt[1]^2) * 4 * b
      if (Nodd) {
        a[1] <- a[1] / 2
      }
    }

    if (Nodd) {
      h <- c(a[seq(L+1,2, by = -1)] / 2, a[1], a[2:(L + 1)] / 2)
    } else {
      h <- 0.5 * c(rev(a), a)
    }

  } else if (filtype == 1) {
    if (differ) {
      do_weight <- as.double(abs(A[seq(2, length(A), by = 2)]) + abs(A[seq(1, length(A), by = 2)]) > 0)
    } else {
      do_weight <- numeric(length(Fn))
    }

    need_matrix <- !fullband || any(do_weight) || !constant_weights

    if (Nodd) {
      m <- 1:L
    } else {
      m <- 0:L + 0.5
    }

    k <- m
    b <- numeric(length(k))

    if (need_matrix) {
      result <- initMatrices(m)
      I1 <- result$I1
      I2 <- result$I2
      G <- result$G
    } else {
      G <- matrix(0, nrow = 0, ncol = 0)
    }

    for (s in seq(1, length(Fn), by = 2)) {
      if (do_weight[(s + 1) / 2]) {
        if (Fn[s] == 0) {
          Fn[s] <- 1e-5
        }

        m_s <- (A[s + 1] - A[s]) / (Fn[s + 1] - Fn[s])
        b1 <- A[s] - m_s * Fn[s]

        snint1 <- sineint(2 * pi * k * Fn[s + 1]) - sineint(2 * pi * k * Fn[s])
        csint1 <- Re((-1/2) * (pracma::expint(1i * 2 * pi * k * Fn[s + 1]) +
                                 pracma::expint(-1i * 2 * pi * k * Fn[s + 1]) -
                                 pracma::expint(1i * 2 * pi * k * Fn[s]) -
                                 pracma::expint(-1i * 2 * pi * k * Fn[s])))

        b <- b + (m_s * snint1 +
                    b1 * 2 * pi * k * (-sinc(2 * k * Fn[s + 1]) + sinc(2 * k * Fn[s]) + csint1)) *
          wt[(s + 1) / 2]^2

        snint1 <- sineint(2 * pi * Fn[s + 1] * (-I2))
        snint2 <- sineint(2 * pi * Fn[s + 1] * I1)
        snint3 <- sineint(2 * pi * Fn[s] * (-I2))
        snint4 <- sineint(2 * pi * Fn[s] * I1)

        G <- G - ((-1/2) * (cos(2 * pi * Fn[s + 1] * (-I2)) / Fn[s + 1] -
                              2 * snint1 * pi * I2 -
                              cos(2 * pi * Fn[s + 1] * I1) / Fn[s + 1] +
                              2 * snint2 * pi * I1) -
                    (-1/2) * (cos(2 * pi * Fn[s] * (-I2)) / Fn[s] -
                                2 * snint3 * pi * I2 -
                                cos(2 * pi * Fn[s] * I1) / Fn[s] +
                                2 * snint4 * pi * I1)) * wt[(s + 1) / 2]^2
      } else {
        m_s <- (A[s + 1] - A[s]) / (Fn[s + 1] - Fn[s])
        b1 <- A[s] - m_s * Fn[s]

        b <- b + (m_s / (4 * pi^2) * (sin(2 * pi * k * Fn[s + 1]) - sin(2 * pi * k * Fn[s])) / (k^2)) * wt[(s + 1) / 2]^2
        b <- b + ((m_s * Fn[s] + b1) * cos(2 * pi * k * Fn[s]) -
                    (m_s * Fn[s + 1] + b1) * cos(2 * pi * k * Fn[s + 1])) / (2 * pi * k) * wt[(s + 1) / 2]^2

        if (need_matrix) {
          G <- G + (0.5 * Fn[s + 1] * (sinc(2 * I1 * Fn[s + 1]) - sinc(2 * I2 * Fn[s + 1])) -
                      0.5 * Fn[s] * (sinc(2 * I1 * Fn[s]) - sinc(2 * I2 * Fn[s]))) * wt[(s + 1) / 2]^2
        }
      }
    }

    if (need_matrix) {
      a <- solve(G, b)
    } else {
      a <- -4 * b * wt[1]^2
    }

    if (Nodd) {
      h <- 0.5 * c(rev(a), 0, -a)
    } else {
      h <- 0.5 * c(rev(a), -a)
    }

    if (differ) {
      h <- -h
    }

  } else {
    h <- 0
  }

  return(list(h = h, a = 1))
}


initMatrices <- function(m) {
  k <- m
  x <- matrix(k, ncol = length(m), nrow = length(m), byrow = TRUE)
  y <- matrix(m, ncol = length(k), nrow = length(k), byrow = FALSE)

  I1 <- x + y
  I2 <- x - y
  G <- matrix(0, ncol = length(m), nrow = length(m))

  return(list(I1 = I1, I2 = I2, G = G))
}

# firls <- function(n, freq, magnitude, weight, type = c("default", "hilbert", "differentiator")) {
#
#   type <- match.arg(type)
#   freq_length <- length(freq)
#
#   if(freq_length %% 2 != 0) {
#     stop("`firls`: `filter` size must be even")
#   }
#   if(length(freq) != length(magnitude)) {
#     stop("`firls`: `filter` size not equals to `magnitude` size")
#   }
#
#   if(missing(weight)) {
#     weight <- rep(1, floor(freq_length / 2))
#   }
#
#   min_freq <- min(freq)
#   max_freq <- max(freq)
#
#   n <- fir_validate(n, freq[[length(freq)]], magnitude, type != "default")
#
#   filter_length <- n + 1
#   half_freq <- freq / 2
#   amp <- as.double(magnitude)
#   wt <- Mod(sqrt( as.complex(weight) ))
#
#   # difference of half-frequency
#   diff_hf <- diff(half_freq)
#   len_hf <- freq_length - 1
#
#   fullband <- FALSE
#   if( len_hf > 1 ){
#     if(all(diff_hf[seq(2, len_hf, by = 2)] == 0)) {
#       fullband <- TRUE
#     }
#   }
#
#   # validate weight
#   if(all(wt == wt[[1]])) {
#     constant_weights <- TRUE
#   } else {
#     constant_weights <- FALSE
#   }
#
#   l <- n / 2
#   b0 <- 0
#   a <- 1
#   h <- 0
#
#   fl_is_even <- filter_length %% 2 == 0
#   need_matrix <- !(fullband && constant_weights)
#
#   half_freq2 <- matrix(half_freq, nrow = 2, byrow = FALSE)
#
#   if( type == "default" ) {
#
#     if( fl_is_even ) {
#       # type II filter
#       m <- seq(0, l) + 0.5
#     } else {
#       m <- seq(0, l)
#     }
#
#     if( need_matrix ) {
#       # TODO init matrix
#       I1 <- outer(m, m, "+")
#       I2 <- outer(m, m, "-")
#       G <- array(0.0, c(l, l))
#     } else {
#       I1 <- 0
#       I2 <- 0
#       G <- 0
#     }
#
#     if( !fl_is_even ) {
#       # the first element is 0, need to handle differently
#       m <- m[-1]
#     }
#
#     b <- rep(0.0, length(m))
#
#     for(ii in seq_len(ncol(half_freq2))) {
#       s <- 2 * ii - 1
#
#       slope <- (amp[s + 1] - amp[s]) / (half_freq2[s + 1] - half_freq2[s])
#       intercept <- amp[s] - slope * half_freq2[s]
#
#       if( !fl_is_even ) {
#         b0 <- b0 + (
#           intercept * (half_freq2[s + 1] - half_freq2[s]) +
#             slope / 2 * (half_freq2[s + 1]^2 - half_freq2[s]^2)
#         ) * (wt[(s + 1) / 2])^2
#       }
#
#       b <- b +
#         (
#           slope * ( cos(2 * pi * m * half_freq2[s+1]) - cos(2 * pi * m * half_freq2[s]) ) /
#             (2*pi*m)^2
#         ) * (wt[(s+1) / 2]) ^ 2 +
#         (
#           half_freq2[s+1] * ( slope * half_freq2[s+1] + intercept ) * sinc( 2*m*half_freq2[s+1] ) -
#             half_freq2[s] * ( slope * half_freq2[s] + intercept ) * sinc( 2*m*half_freq2[s] )
#         ) * (wt[(s+1) / 2]) ^ 2
#
#       if( need_matrix ) {
#         G <- G + (
#           half_freq2[s+1] * ( sinc(2 * I1 * half_freq2[s+1]) + sinc(2 * I2 * half_freq2[s+1]) ) -
#             half_freq2[s] * ( sinc(2 * I1 * half_freq2[s]) + sinc(2 * I2 * half_freq2[s]) )
#         ) * (wt[(s+1) / 2]) ^ 2 / 2
#       }
#
#     }
#
#     if(!fl_is_even) {
#       b <- c(b0, b)
#     }
#
#     if( need_matrix ) {
#       a <- solve(G, b)
#     } else {
#       a <- (wt[1]^2) * 4 * b
#       if(!fl_is_even) {
#         a[[1]] <- a[[1]] / 2
#       }
#     }
#
#     if( !fl_is_even ) {
#       sub <- a[seq(2, l + 1)] / 2
#       h <- c(rev(sub), a[[1]], sub)
#     } else {
#       h <- c(rev(a), a) / 2
#     }
#
#
#   } else {
#     # Type 3/4 FIR
#
#     amp2 <- matrix(amp, nrow = 2, byrow = FALSE)
#     if( type == "differentiator" ) {
#       do_weight <- abs(amp2[1,]) > abs(amp2[2,])
#     } else {
#       do_weight <- rep(FALSE, ncol(amp2))
#     }
#
#     need_matrix <- need_matrix || any(do_weight)
#     if( !fl_is_even ) {
#       m <- seq_len(l)
#     } else {
#       m <- seq(0, l) + 0.5
#     }
#
#     b <- 0
#
#     if( need_matrix ) {
#       I1 <- outer(m, m, "+")
#       I2 <- outer(m, m, "-")
#       G <- array(0.0, c(l, l))
#     } else {
#       I1 <- 0
#       I2 <- 0
#       G <- 0
#     }
#
#
#     for(ii in seq_len(length(half_freq) / 2)) {
#       s <- 2 * ii - 1
#
#       if(do_weight[[ii]]) {
#         if(half_freq2[[s]] == 0) {
#           half_freq2[[s]] <- 1e-5
#         }
#
#         slope <- (amp[s + 1] - amp[s]) / (half_freq2[s + 1] - half_freq2[s])
#         intercept <- amp[s] - slope * half_freq2[s]
#
#         tmp1 <- 2*pi * m * half_freq2[s + 1]
#         tmp0 <- 2*pi * m * half_freq2[s]
#         snint1 <- sineint( tmp1 ) - sineint( tmp0 )
#         csint1 <- -0.5 * Re(
#           pracma::expint( 1i * tmp1 ) + pracma::expint( -1i * tmp1 ) -
#             pracma::expint( 1i * tmp0 ) - pracma::expint( -1i * tmp0 )
#         )
#
#         slope * snint1 + intercept * 2*pi * m * (
#           sinc( 2 * m * half_freq2[s] ) + csint1 - sinc( 2 * m * half_freq2[s + 1] )
#         ) * wt[[ii]]^2
#
#
#         tmp12 <- 2*pi * half_freq2[s + 1] * (-I2)
#         tmp11 <- 2*pi * half_freq2[s + 1] * I1
#         tmp02 <- 2*pi * half_freq2[s] * (-I2)
#         tmp01 <- 2*pi * half_freq2[s] * I1
#         snint1 <- sineint( tmp12 )
#         snint2 <- sineint( tmp11 )
#         snint3 <- sineint( tmp02 )
#         snint4 <- sineint( tmp01 )
#
#         G <- G + 0.5 * (
#           (
#             cos( tmp12 ) / half_freq2[s + 1] -
#               2 * snint1 * pi * I2 -
#               cos( tmp11 ) / half_freq2[s + 1] -
#               2 * snint2 * pi * I1 ) -
#           (
#             cos( tmp02 ) / half_freq2[s] -
#               2 * snint3 * pi * I2 -
#               cos( tmp01 ) / half_freq2[s] -
#               2 * snint4 * pi * I1
#           )
#         ) * wt[[ii]]^2
#
#       } else {
#
#         slope <- (amp[s + 1] - amp[s]) / (half_freq2[s + 1] - half_freq2[s])
#         intercept <- amp[s] - slope * half_freq2[s]
#
#         if( !fl_is_even ) {
#           b0 <- b0 + (
#             intercept * (half_freq2[s + 1] - half_freq2[s]) +
#               slope / 2 * (half_freq2[s + 1]^2 - half_freq2[s]^2)
#           ) * (wt[(s + 1) / 2])^2
#         }
#
#         b <- b +
#           (
#             slope * ( sin(2 * pi * m * half_freq2[s+1]) - sin(2 * pi * m * half_freq2[s]) ) /
#               (2*pi*m)^2
#           ) * (wt[(s+1) / 2]) ^ 2 +
#           (
#             ( slope * half_freq2[s] + intercept ) * cos( 2*m*half_freq2[s] ) -
#             ( slope * half_freq2[s+1] + intercept ) * cos( 2*m*half_freq2[s+1] )
#           ) / (2 * pi * m) * (wt[(s+1) / 2]) ^ 2
#
#         if( need_matrix ) {
#           G <- G + (
#             half_freq2[s+1] * ( sinc(2 * I1 * half_freq2[s+1]) - sinc(2 * I2 * half_freq2[s+1]) ) -
#               half_freq2[s] * ( sinc(2 * I1 * half_freq2[s]) - sinc(2 * I2 * half_freq2[s]) )
#           ) * (wt[(s+1) / 2]) ^ 2 / 2
#         }
#
#       }
#     }
#
#
#     if( need_matrix ) {
#       a <- solve(G, b)
#     } else {
#       a <- -4 * b * wt[[1]]^2
#     }
#
#     if( !fl_is_even ) {
#       h <- c(rev(a), 0, -a) / 2
#     } else {
#       h <- c(rev(a), -a) / 2
#     }
#
#     if ( type == "differentiator" ) {
#       h <- -h
#     }
#
#   }
#
#   return(list(h = h, b = b, a = a))
#
# }
