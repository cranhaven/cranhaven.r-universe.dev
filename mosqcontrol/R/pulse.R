#' sum_one_pulse
#'
#' \code{sum_one_pulse} returns output pulse.
#'
#' This function computes the fourier sum for 1 pulse.
#'
#' @param z Numeric
#' @param rho Numeric
#' @param mu Numeric
#' @param tau Numeric
#' @param kmax Numeric
#' @param modes Numeric
#' @keywords internal
sum_one_pulse <- function(z, rho, mu, tau, kmax, modes) {
  jmax <- length(modes) # modes is the list of fourier modes, jmax = number of list elements
  out_val <- 0 # running total for output value

  P_func <- function(x) {
    rho * 1 / (log(1 / (1 - rho)) - 2 * pi * complex(real = 0, imaginary = 1) * x)
  }
  Q_func <- function(x) {
    (rho / (1 - rho)) * 1 / (log(1 / (1 - rho)) + 2 * pi * complex(real = 0, imaginary = 1) * x)
  }


  for (k in -kmax:kmax) {
    pre_factor <- P_func(k)

    post_factor <- exp(-2 * pi * complex(real = 0, imaginary = 1) * k * z / tau)
    P_val <- pre_factor * post_factor

    Q_val <- 0

    for (j in (-jmax + 1):(jmax - 1)) {
      pre_factor <- Q_func(-k - j)
      post_factor <- exp(-2 * pi * complex(real = 0, imaginary = 1) * (-k - j) * z / tau)

      if (j == 0) {
        Q_val <- Q_val + pre_factor * post_factor * modes[1] / mu
      } else if (j < 0) {
        Q_val <- Q_val + pre_factor * post_factor * Conj(modes[-(j - 1)]) / mu
      } else if (j > 0) {
        Q_val <- Q_val + pre_factor * post_factor * modes[j + 1] / mu
      }
    }


    out_val <- out_val + (P_val) * (Q_val) / (1 - log(1 - rho) / (mu * tau) - 2 * pi * complex(real = 0, imaginary = 1) * k / (mu * tau))
  }

  return(Re(out_val))
}

#' sum_n_pulse
#'
#' \code{sum_n_pulse} returns output pulse.
#'
#' This function computes the Fourier sum for N pulses, where the "P" and "Q"
#' products and exponentials are computeded by the "P_vals" and "q_vals"
#' functions.
#'
#' @param z Numeric
#' @param rho Numeric
#' @param Npulse Numeric
#' @param mu Numeric
#' @param tau Numeric
#' @param kmax Numeric
#' @param modes Numeric
#' @keywords internal
sum_n_pulse <- function(z, rho, Npulse, mu, tau, kmax, modes) {
  jmax <- length(modes)
  out_val <- 0 # running total for output


  for (k in -kmax:kmax) {
    P_val <- P_vals(z, rho, Npulse, tau, k) # compute "P" product and exponentials at this k value

    Q_val <- 0 # running total for "Q" product and exponentials
    # add together "Q" products and exponentials at (k + j) value for all j emergence fourier modes
    for (j in (-jmax + 1):(jmax - 1)) {

      # if (abs(k+j) < kmax){

      if (j == 0) {
        Q_val <- Q_val + Q_vals(z, rho, Npulse, tau, k, j) * modes[1] / mu # zero fourier mode
      } else if (j < 0) {
        Q_val <- Q_val + Q_vals(z, rho, Npulse, tau, k, j) * Conj(modes[-(j - 1)]) / mu # negative fourier modes
      } else if (j > 0) {
        Q_val <- Q_val + Q_vals(z, rho, Npulse, tau, k, j) * modes[j + 1] / mu # positive fourier modes
      }
      # }
    }

    # add contribution from this k value to running output total
    out_val <- out_val + (P_val) * (Q_val) / (1 - Npulse * log(1 - rho) / (mu * tau) - 2 * pi * complex(real = 0, imaginary = 1) * k / (mu * tau))
  }
  out <- Re(out_val)
  return(out)
}