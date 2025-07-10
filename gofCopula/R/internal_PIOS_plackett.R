# Internal functions for the derivation of the plackett copula for the PIOSRn 
# and PIOSTn test statistics. Called by .Rn and .Tn, the internal functions for
# their test statistics.

# 2d density
.plackett.12.density <- function(theta, u) { 
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
  }
  theta <- theta - 1

  -(((1 + theta) * (-1 - theta * u2 + theta * u1 * (-1 + 2 * u2))) / 
      ((-4 * theta * (1 + theta) * u1 * u2 + ((1 + theta * (u1 + u2))^2))^1.5))
}

# 2d first derivative wrt theta of the log-density
.plackett.12.V <- function(theta, u1, u2) { 
  theta <- theta - 1
  (-((2 + theta) / (1 + theta)) + 1 / (-1 - theta * u1 + theta * 
      (-1 + 2 * u1) * u2) + (3 + 3 * theta * (u1 + u2 - 2 * u1 * u2)) / 
      (1 + (theta^2) * ((u1 - u2)^2) + 2 * theta * 
         (u1 + u2 - 2 * u1 * u2))) / theta
}

# 2d second derivative wrt theta of the log-density
.plackett.12.S <- function(theta, u1, u2) { 
  theta <- theta - 1

  ((2 + theta * (4 + theta)) / ((1 + theta)^2) - ((1 + theta * (u1 + u2 - 2 * 
    u1 * u2))^(-2)) + 2 / (1 + theta * (u1 + u2 - 2 * u1 * u2)) +
    (24 * (-1 + u1) * u1 * (-((1 + theta * u1)^2) + theta * (2 + theta) * 
    (-1 + 2 * u1) * u2)) / ((1 + (theta^2) * ((u1 - u2)^2) + 2 * theta * 
    (u1 + u2 - 2 * u1 * u2))^2) + (3 * (-1 + 8 * (u1^2) - 2 * theta * u2 + 
    u1 * (-8 + theta * (-2 + 4 * u2)))) / (1 + (theta^2) * ((u1 - u2)^2) + 
    2 * theta * (u1 + u2 - 2 * u1 * u2))) / (theta^2)
}

