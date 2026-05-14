#' P_vals
#'
#' \code{P_vals} returns "P" product.
#'
#' This function computes the "P" product and exponentials for a given k.
#'
#' @param z Numeric
#' @param rho Numeric
#' @param Npulse Numeric
#' @param tau Numeric
#' @param k Numeric
#' @keywords internal
P_vals <- function(z, rho, Npulse, tau, k) {
  P_func <- function(x) {
    rho * 1 / (log(1 / (1 - rho)) - 2 * pi * complex(real = 0, imaginary = 1) * x)
  }

  z_vec <- matrix(0, nrow = Npulse, ncol = 1)

  for (i in 1:Npulse) {
    z_vec[i, 1] <- z[i]
  }

  # determine number of integer vectors to permute (will be smaller for
  # only two and three pulses). number of combos = N_terms

  if (k == 0) {
    if (Npulse == 2) {
      N_terms <- 5
    } else if (Npulse == 3) {
      N_terms <- 7
    } else {
      N_terms <- 8
    }


    # create a matrix to hold the integer vectors to permute, and
    # initialize to zero. Each column will give an integer vector
    int_vecs <- matrix(0, nrow = Npulse, ncol = N_terms)

    # the first 5 integer vectors have at most two non-zero elements.
    # This loop replaces the first two zeros in the first 5 columns of int_vecs
    # with the appropriate integers.
    for (i in 1:5) {
      int_vecs[1:2, i] <- c((i - 1), -(i - 1))
    }

    # if there are are three or more pulses, we need to include
    # integer vectors with three non-zero terms
    if (Npulse >= 3) {
      int_vecs[1:3, 6] <- c(2, -1, -1)
      int_vecs[1:3, 7] <- c(-2, 1, 1)
    }

    # if there are four or more pulses, then we need to include an
    # integer vector with four non-zero terms
    if (Npulse >= 4) {
      int_vecs[1:4, 8] <- c(1, 1, -1, -1)
    }


    # calculates the "P" functions for each column of integer combos in
    # int_vec.  "P" function values are independet of integer
    # permutations. pre_factors is a list which holds the "P"-function
    # values for each integer combo
    pre_factors <- matrix(1, ncol = N_terms, nrow = 1)
    for (i in 1:N_terms) {
      for (m in 1:Npulse) {
        pre_factors[1, i] <- pre_factors[1, i] * P_func(int_vecs[m, i])
      }
    }


    # Now calculate the exponential terms which will multiply the "P"functions in the pre_factors list
    # post_factors = list of the exponential terms to multiply the
    #               corresponding "P" functions in the pre_factors list
    post_factors <- numeric(N_terms) # initialize to zero

    for (i in 1:N_terms) { # for each integer combo given by the columns of int_vecs...

      # calculates the unique permutations of the ith integer combo
      # int_vecs(:, i) = ith column of int_vecs matrix
      # perm_list = Nperms x Npulse matrix, where Nperms is the number
      #           of unique permutations of the ith integer combo
      # Each row of perm_list gives a unique permutaion of the ith integer combo
      perm_list <- uperm(int_vecs[, i])

      if (i == 1) {
        Nperms <- 1
      } else {
        Nperms <- length(perm_list[, 1]) # number of unique permutations of the ith integer combo
      }


      for (m in 1:Nperms) { # for each uniqe permutation of the ith integer combo, add the corresponding      exponential to the running total of the corresponding post_factor value

        if (i == 1) {
          post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * (perm_list %*% z) / tau)
        } else {
          post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * (perm_list[m, ] %*% z) / tau)
        }
      } # perm_list[m, ] = mth row of perm_list = mth unique permutaton of ith integer combo
    }



    val <- 0
    for (i in 1:N_terms) { # add together the (pre_factor * post_factor) for each integer combo
      val <- val + pre_factors[1, i] * post_factors[i]
    }

    return(val)
  } else if (abs(k) == 1) {
    kk <- abs(k)

    if (Npulse == 2) {
      N_terms <- 5
    } else {
      N_terms <- 8
    }


    int_vecs <- matrix(0, nrow = Npulse, ncol = N_terms)
    for (i in 1:5) {
      int_vecs[1:2, i] <- sign(k) * c(kk + (i - 1), -(i - 1))
    }

    if (Npulse >= 3) {
      int_vecs[1:3, 6] <- sign(k) * c(1, 1, -1)
      int_vecs[1:3, 7] <- sign(k) * c(3, -1, -1)
      int_vecs[1:3, 8] <- sign(k) * c(2, -2, 1)
    }

    pre_factors <- matrix(1, ncol = N_terms, nrow = 1)
    for (i in 1:N_terms) {
      for (m in 1:Npulse) {
        pre_factors[1, i] <- pre_factors[1, i] * P_func(int_vecs[m, i])
      }
    }

    post_factors <- numeric(N_terms)
    for (i in 1:N_terms) {
      perm_list <- uperm(int_vecs[, i])
      Nperms <- dim(perm_list)[1]

      for (m in 1:Nperms) {
        post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }


    val <- 0
    for (i in 1:N_terms) {
      val <- val + pre_factors[1, i] * post_factors[i]
    }

    return(val)
  }
  else if (abs(k) == 2) {
    kk <- abs(k)

    if (Npulse == 2) {
      N_terms <- 5
    } else if (Npulse == 3) {
      N_terms <- 6
    } else {
      N_terms <- 7
    }
    int_vecs <- matrix(0, Npulse, N_terms)

    int_vecs[1:2, 1] <- sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] <- sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] <- sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] <- sign(k) * c(kk + 2, -2)
    int_vecs[1:2, 5] <- sign(k) * c(kk + 3, -3)

    if (Npulse >= 3) {
      int_vecs[1:3, 6] <- sign(k) * c(2, -1, 1)
    }
    if (Npulse >= 4) {
      int_vecs[1:4, 7] <- sign(k) * c(1, 1, 1, -1)
    }


    pre_factors <- matrix(1, ncol = N_terms, nrow = 1)
    for (i in 1:N_terms) {
      for (m in 1:Npulse) {
        pre_factors[1, i] <- pre_factors[1, i] * P_func(int_vecs[m, i])
      }
    }

    post_factors <- numeric(N_terms)
    for (i in 1:N_terms) {
      perm_list <- uperm(int_vecs[, i])

      if (Npulse == 2 && i == 2) {
        Nperms <- 1
        post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      } else {
        Nperms <- dim(perm_list)[1]

        for (m in 1:Nperms) {
          post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }


    val <- 0
    for (i in 1:N_terms) {
      val <- val + pre_factors[1, i] * post_factors[i]
    }

    return(val)
  } else if (abs(k) == 3) {
    kk <- abs(k)

    if (Npulse == 2) {
      N_terms <- 5
    } else {
      N_terms <- 8
    }

    int_vecs <- matrix(0, Npulse, N_terms)

    int_vecs[1:2, 1] <- sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] <- sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] <- sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] <- sign(k) * c(kk + 2, -2)
    int_vecs[1:2, 5] <- sign(k) * c(kk + 3, -3)


    if (Npulse >= 3) {
      int_vecs[1:3, 6] <- sign(k) * c(1, 1, 1)
      int_vecs[1:3, 7] <- sign(k) * c(3, 1, -1)
      int_vecs[1:3, 8] <- sign(k) * c(2, 2, -1)
    }

    pre_factors <- matrix(1, ncol = N_terms, nrow = 1)
    for (i in 1:N_terms) {
      for (m in 1:Npulse) {
        pre_factors[1, i] <- pre_factors[1, i] * P_func(int_vecs[m, i])
      }
    }
    post_factors <- numeric(N_terms)
    for (i in 1:N_terms) {
      perm_list <- uperm(int_vecs[, i])

      if (Npulse == 3 && i == 6) {
        Nperms <- 1
        post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      } else {
        Nperms <- dim(perm_list)[1]

        for (m in 1:Nperms) {
          post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }


    val <- 0

    for (i in 1:N_terms) {
      val <- val + pre_factors[1, i] * post_factors[i]
    }

    return(val)
  } else if (abs(k) == 4) {
    kk <- abs(k)

    if (Npulse == 2) {
      N_terms <- 5
    } else if (Npulse == 3) {
      N_terms <- 7
    } else {
      N_terms <- 8
    }


    int_vecs <- matrix(0, Npulse, N_terms)

    int_vecs[1:2, 1] <- sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] <- sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] <- sign(k) * c(kk - 2, 2)
    int_vecs[1:2, 4] <- sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 5] <- sign(k) * c(kk + 2, -2)

    if (Npulse >= 3) {
      int_vecs[1:3, 6] <- sign(k) * c(4, -1, 1)
      int_vecs[1:3, 7] <- sign(k) * c(2, 1, 1)
    }
    if (Npulse >= 4) {
      int_vecs[1:4, 8] <- sign(k) * c(1, 1, 1, 1)
    }

    pre_factors <- matrix(1, ncol = N_terms, nrow = 1)

    for (i in 1:N_terms) {
      for (m in 1:Npulse) {
        pre_factors[1, i] <- pre_factors[1, i] * P_func(int_vecs[m, i])
      }
    }
    post_factors <- numeric(N_terms)
    for (i in 1:N_terms) {
      perm_list <- uperm(int_vecs[, i])

      if (Npulse == 4 && i == 8) {
        Nperms <- 1
        post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      } else if (Npulse == 2 && i == 3) {
        Nperms <- 1
        post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      } else {
        Nperms <- dim(perm_list)[1]


        for (m in 1:Nperms) {
          post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }
    val <- 0
    for (i in 1:N_terms) {
      val <- val + pre_factors[1, i] * post_factors[i]
    }

    return(val)
  } else if (abs(k) == 5) {
    kk <- abs(k)

    if (Npulse == 2) {
      N_terms <- 5
    } else {
      N_terms <- 7
    }

    int_vecs <- matrix(0, Npulse, N_terms)

    int_vecs[1:2, 1] <- sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] <- sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] <- sign(k) * c(kk - 2, 2)
    int_vecs[1:2, 4] <- sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 5] <- sign(k) * c(kk + 2, -2)


    if (Npulse >= 3) {
      int_vecs[1:3, 6] <- sign(k) * c(2, 2, 1)
      int_vecs[1:3, 7] <- sign(k) * c(3, 1, 1)
    }

    pre_factors <- matrix(1, ncol = N_terms, nrow = 1)

    for (i in 1:N_terms) {
      for (m in 1:Npulse) {
        pre_factors[1, i] <- pre_factors[1, i] * P_func(int_vecs[m, i])
      }
    }
    post_factors <- numeric(N_terms)
    for (i in 1:N_terms) {
      perm_list <- uperm(int_vecs[, i])
      Nperms <- dim(perm_list)[1]


      for (m in 1:Nperms) {
        post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }

    val <- 0
    for (i in 1:N_terms) {
      val <- val + pre_factors[1, i] * post_factors[i]
    }

    return(val)
  } else if (abs(k) == 6) {
    kk <- abs(k)

    if (Npulse == 2) {
      N_terms <- 6
    } else {
      N_terms <- 7
    }

    int_vecs <- matrix(0, Npulse, N_terms)

    int_vecs[1:2, 1] <- sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] <- sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] <- sign(k) * c(kk - 2, 2)
    int_vecs[1:2, 4] <- sign(k) * c(kk - 3, 3)
    int_vecs[1:2, 5] <- sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 6] <- sign(k) * c(kk + 2, -2)


    if (Npulse >= 3) {
      int_vecs[1:3, 7] <- sign(k) * c(4, 1, 1)
    }

    pre_factors <- matrix(1, ncol = N_terms, nrow = 1)

    for (i in 1:N_terms) {
      for (m in 1:Npulse) {
        pre_factors[1, i] <- pre_factors[1, i] * P_func(int_vecs[m, i])
      }
    }
    post_factors <- numeric(N_terms)
    for (i in 1:N_terms) {
      perm_list <- uperm(int_vecs[, i])

      if (Npulse == 2 && i == 4) {
        Nperms <- 1
        post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      } else {
        Nperms <- dim(perm_list)[1]


        for (m in 1:Nperms) {
          post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }


    val <- 0
    for (i in 1:N_terms) {
      val <- val + pre_factors[1, i] * post_factors[i]
    }

    return(val)
  } else if (abs(k) == 7) {
    kk <- abs(k)

    N_terms <- 6
    int_vecs <- matrix(0, Npulse, N_terms)

    int_vecs[1:2, 1] <- sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] <- sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] <- sign(k) * c(kk - 2, 2)
    int_vecs[1:2, 4] <- sign(k) * c(kk - 3, 3)
    int_vecs[1:2, 5] <- sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 6] <- sign(k) * c(kk + 2, -2)


    pre_factors <- matrix(1, ncol = N_terms, nrow = 1)

    for (i in 1:N_terms) {
      for (m in 1:Npulse) {
        pre_factors[1, i] <- pre_factors[1, i] * P_func(int_vecs[m, i])
      }
    }
    post_factors <- numeric(N_terms)
    for (i in 1:N_terms) {
      perm_list <- uperm(int_vecs[, i])
      Nperms <- dim(perm_list)[1]


      for (m in 1:Nperms) {
        post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }

    val <- 0
    for (i in 1:N_terms) {
      val <- val + pre_factors[1, i] * post_factors[i]
    }

    return(val)
  } else if (abs(k) == 8) {
    kk <- abs(k)

    N_terms <- 6
    int_vecs <- matrix(0, Npulse, N_terms)

    int_vecs[1:2, 1] <- sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] <- sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] <- sign(k) * c(kk - 2, 2)
    int_vecs[1:2, 4] <- sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 5] <- sign(k) * c(kk - 3, +3)
    int_vecs[1:2, 6] <- sign(k) * c(kk - 4, +4)



    pre_factors <- matrix(1, ncol = N_terms, nrow = 1)

    for (i in 1:N_terms) {
      for (m in 1:Npulse) {
        pre_factors[1, i] <- pre_factors[1, i] * P_func(int_vecs[m, i])
      }
    }
    post_factors <- numeric(N_terms)
    for (i in 1:N_terms) {
      perm_list <- uperm(int_vecs[, i])

      if (Npulse == 2 && i == 6) {
        Nperm <- 1
        post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      } else {
        Nperms <- dim(perm_list)[1]


        for (m in 1:Nperms) {
          post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }

    val <- 0
    for (i in 1:N_terms) {
      val <- val + pre_factors[1, i] * post_factors[i]
    }

    return(val)
  } else if (abs(k) == 9) {
    kk <- abs(k)

    N_terms <- 5
    int_vecs <- matrix(0, Npulse, N_terms)

    int_vecs[1:2, 1] <- sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] <- sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] <- sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] <- sign(k) * c(kk - 2, 2)
    int_vecs[1:2, 5] <- sign(k) * c(kk - 3, +3)


    pre_factors <- matrix(1, ncol = N_terms, nrow = 1)

    for (i in 1:N_terms) {
      for (m in 1:Npulse) {
        pre_factors[1, i] <- pre_factors[1, i] * P_func(int_vecs[m, i])
      }
    }
    post_factors <- numeric(N_terms)
    for (i in 1:N_terms) {
      perm_list <- uperm(int_vecs[, i])
      Nperms <- dim(perm_list)[1]


      for (m in 1:Nperms) {
        post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }

    val <- 0
    for (i in 1:N_terms) {
      val <- val + pre_factors[i] * post_factors[i]
    }

    return(val)
  } else if (abs(k) == 10) {
    kk <- abs(k)

    N_terms <- 4
    int_vecs <- matrix(0, Npulse, N_terms)

    int_vecs[1:2, 1] <- sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] <- sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] <- sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] <- sign(k) * c(kk - 2, 2)


    pre_factors <- matrix(1, ncol = N_terms, nrow = 1)

    for (i in 1:N_terms) {
      for (m in 1:Npulse) {
        pre_factors[1, i] <- pre_factors[1, i] * P_func(int_vecs[m, i])
      }
    }
    post_factors <- numeric(N_terms)
    for (i in 1:N_terms) {
      perm_list <- uperm(int_vecs[, i])
      Nperms <- dim(perm_list)[1]


      for (m in 1:Nperms) {
        post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }

    val <- 0
    for (i in 1:N_terms) {
      val <- val + pre_factors[1, i] * post_factors[i]
    }

    return(val)
  } else if (abs(k) == 11) {
    kk <- abs(k)

    N_terms <- 4

    int_vecs <- matrix(0, Npulse, N_terms)

    int_vecs[1:2, 1] <- sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] <- sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] <- sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] <- sign(k) * c(kk - 2, 2)


    pre_factors <- matrix(1, ncol = N_terms, nrow = 1)

    for (i in 1:N_terms) {
      for (m in 1:Npulse) {
        pre_factors[1, i] <- pre_factors[1, i] * P_func(int_vecs[m, i])
      }
    }
    post_factors <- numeric(N_terms)
    for (i in 1:N_terms) {
      perm_list <- uperm(int_vecs[, i])
      Nperms <- dim(perm_list)[1]


      for (m in 1:Nperms) {
        post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }

    val <- 0
    for (i in 1:N_terms) {
      val <- val + pre_factors[1, i] * post_factors[i]
    }

    out <- val
  } else {
    kk <- abs(k)

    N_terms <- 3
    int_vecs <- matrix(0, Npulse, N_terms)

    int_vecs[1:2, 1] <- sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] <- sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] <- sign(k) * c(kk + 1, -1)


    pre_factors <- matrix(1, ncol = N_terms, nrow = 1)

    for (i in 1:N_terms) {
      for (m in 1:Npulse) {
        pre_factors[1, i] <- pre_factors[1, i] * P_func(int_vecs[m, i])
      }
    }
    post_factors <- numeric(N_terms)
    for (i in 1:N_terms) {
      perm_list <- uperm(int_vecs[, i])
      Nperms <- dim(perm_list)[1]


      for (m in 1:Nperms) {
        post_factors[i] <- post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }

    val <- 0
    for (i in 1:N_terms) {
      val <- val + pre_factors[i] * post_factors[i]
    }

    return(val)
  }
}
