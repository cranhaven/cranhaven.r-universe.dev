# Internat functions for the Rosenblatt transformation for Galambos, FGM and
# Plackett Copula
.rosenblatt.galambos <- function(u, theta) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
  }
  .deriv.1.galambos <- function(u1, u2, theta) {
    exp(((((-log(u1))^(-theta)) + ((-log(u2))^(-theta)))^(-1 / theta))) * u2 *
      (1 + 1 / (((-log(u1))^theta) * log(u1) *
        ((((-log(u1))^(-theta)) + ((-log(u2))^(-theta)))^((1 + theta) / 
                                                            theta))))
  }
  cbind(u1, .deriv.1.galambos(u1, u2, theta) / .deriv.1.galambos(u1, 1, theta))
}

.rosenblatt.fgm <- function(u, theta) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
  }
  .deriv.1.fgm <- function(u1, u2, theta) {
    u2 + theta * (-1 + 2 * u1) * (-1 + u2) * u2
  }
  cbind(u1, .deriv.1.fgm(u1, u2, theta) / .deriv.1.fgm(u1, 1, theta))
}

.rosenblatt.plackett <- function(u, theta) {
  theta <- theta - 1
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
  }
  .deriv.1.plackett <- function(u1, u2, theta) {
    (1 + (-1 - theta * u1 + (2 + theta) * u2) / sqrt(-4 * theta * 
      (1 + theta) * u1 * u2 + ((1 + theta * (u1 + u2))^2))) / 2
  }
  cbind(u1, .deriv.1.plackett(u1, u2, theta) / .deriv.1.plackett(u1, 1, theta))
}
