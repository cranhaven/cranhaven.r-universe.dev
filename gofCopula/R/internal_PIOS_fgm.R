# 2d density
.fgm.12.density <- function(theta, u) { 
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
  }

  1 + theta * (-1 + 2 * u1) * (-1 + 2 * u2)
}

# 2d first derivative wrt theta of the log-density
.fgm.12.V <- function(theta, u1, u2) { 

  ((-1 + 2 * u1) * (-1 + 2 * u2)) / (1 + theta * (-1 + 2 * u1) * (-1 + 2 * u2))
}

# 2d second derivative wrt theta of the log-density
.fgm.12.S <- function(theta, u1, u2) { 

  -((((1 - 2 * u1)^2) * ((1 - 2 * u2)^2)) / ((1 + theta * (-1 + 2 * u1) * 
                                                (-1 + 2 * u2))^2))
}
