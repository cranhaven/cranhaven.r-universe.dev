# Internal functions for the derivation of the joe copula for the PIOSRn and
# PIOSTn test statistics. Called by .Rn and .Tn, the internal functions for
# their test statistics.

# 2d density
.joe.12.density <- function(theta, u) { 
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
  }

  u1t <- (-1 + ((1 - u1)^theta))
  u2t <- (-1 + ((1 - u2)^theta))

  (((1 - u1)^(-1 + theta)) * ((1 - u1t * u2t)^(1 / theta)) * 
      (theta - u1t * u2t) * ((1 - u2)^(-1 + theta))) / 
    ((((1 - u1)^theta) - u1t * ((1 - u2)^theta))^2)
}

# 2d first derivative wrt theta of the log-density
.joe.12.V <- function(theta, u1, u2) { 

  u1tt <- (1 - u1)^theta
  u2tt <- (1 - u2)^theta
  u1t <- (-1 + u1tt)
  u2t <- (-1 + u2tt)

  -((theta * ((-((-1 + theta)^2) + u1tt) * u1tt + ((-1 + theta) * theta + 
      (2 + (-1 + theta) * theta) * u1tt - 2 * ((1 - u1)^(2 * theta))) * 
      u2tt + u1t * (-theta + u1tt) * ((1 - u2)^(2 * theta))) * log(1 - u1) - 
      (-theta + u1t * u2t) * (-u1tt + u1t * u2tt) * log(1 - u1t * u2t) + 
      theta * (((-1 + u1tt)^2) * ((1 - u2)^(2 * theta)) * log(1 - u2) + 
      theta * u1tt * (1 + (-1 + theta + u1tt) * log(1 - u2)) - u1t * u2tt * 
      (theta + (-((-1 + theta)^2) + (1 + theta) * u1tt) * log(1 - u2)))) /
      ((theta^2) * (-theta + u1t * u2t) * (u1tt - u1t * u2tt)))
}

# 2d second derivative wrt theta of the log-density
.joe.12.S <- function(theta, u1, u2) { 
  u1tt <- (1 - u1)^theta
  u2tt <- (1 - u2)^theta
  u1t <- (-1 + u1tt)
  u2t <- (-1 + u2tt)

  (-((theta^3) * ((u1tt - u1t * u2tt)^2)) + (-1 + theta) * (theta^2) * u1tt * 
      u2t * (-(theta * u2t * (-((1 - u1)^(2 * theta)) + u1t * (3 + u1tt) * 
      u2tt)) + 2 * (theta^2) * u2tt + (u1t^2) * (u2t^2) * u2tt) * 
      ((log(1 - u1))^2) + 2 * ((theta - u1t * u2t)^2) * ((u1tt - u1t * 
      u2tt)^2) * log(1 - u1t * u2t) - 2 * theta * u1tt * log(1 - u1) * 
      ((-2 * theta * u1t * u2t + (u1t^2) * (u2t^2) + (theta^2) * (1 + 
      u1tt - u1t * u2tt)) * u2t * (-u1tt + u1t * u2tt) + (-1 + theta) * 
      theta * (-2 * (theta^2) - (u1t^2) * (u2t^2) + theta * u1t * u2t * 
      (3 - u1tt + u1t * u2tt)) * u2tt * log(1 - u2)) + theta * u1t * u2tt * 
      log(1 - u2) * (-2 * (u1t^3) * ((1 - u2)^(3 * theta)) + (u1t^2) * 
      ((1 - u2)^(2 * theta)) * (-4 + 2 * theta * (2 + theta) + 6 * u1tt + 
      (-1 + theta) * theta * (-theta + u1tt) * log(1 - u2)) + u1tt * (2 * 
      (((-1 + theta)^2) + (-2 + theta * (2 + theta)) * u1tt + ((1 - u1)^(2 * 
      theta))) + (-1 + theta) * theta * (-1 + theta + u1tt) * (-1 + 2 * 
      theta + u1tt) * log(1 - u2)) - 2 * u1t * u2tt * (((-1 + theta)^2) + 
      ((1 - u1)^(2 * theta)) * (3 + (-1 + theta) * theta * log(1 - u2)) +
      u1tt * (-4 + 2 * theta * (2 + theta) + ((-1 + theta)^2) * theta * 
      log(1 - u2))))) / ((theta^3) * ((theta - u1t * u2t)^2) * 
      ((u1tt - u1t * u2tt)^2))
}

# 3d density
.joe.123.density <- function(theta, u) { 
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
    u3 <- u[3]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
    u3 <- u[, 3]
  }

  u1tt <- (1 - u1)^theta
  u2tt <- (1 - u2)^theta
  u3tt <- (1 - u3)^theta
  u1t <- -1 + u1tt
  u2t <- -1 + u2tt
  u3t <- -1 + u3tt

  (theta^2) * ((1 - u1)^(-1 + theta)) * ((1 - u2)^(-1 + theta)) * 
    ((1 + (u1t) * (u2t) * (u3t))^(-3 + 1 / theta)) * (2 - u1tt - u2tt + 
    u1tt * u2tt + (3 * (u1t) * (u2t) * (u3t)) / theta + ((u1t^2) * (u2t^2) *
    (u3t^2)) / (theta^2) - (u1t) * (u2t) * u3tt) * ((1 - u3)^(-1 + theta))
}

# 3d first derivative wrt theta of the log-density
.joe.123.V <- function(theta, u) { 
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
    u3 <- u[3]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
    u3 <- u[, 3]
  }

  u1tt <- (1 - u1)^theta
  u2tt <- (1 - u2)^theta
  u3tt <- (1 - u3)^theta
  u1t <- -1 + u1tt
  u2t <- -1 + u2tt
  u3t <- -1 + u3tt
  u1t2 <- u1t^2
  u2t2 <- u2t^2
  u3t2 <- u3t^2
  u123t <- u1t * u2t * u3t
  t2 <- theta^2
  t.i <- 1 / theta

  (2 * theta * ((1 + u123t)^2) + 3 * (1 - u1tt) * (1 - u2tt) * (1 + u123t) * 
    (1 - u3tt) - 6 * (-1 + t.i) * theta * (1 - u1tt) * (1 - u2tt) * 
    (1 + u123t) * (1 - u3tt) + ((-1 + theta) * u1t2 * u2t2 * u3t2) / 
    theta + ((-1 + 2 * theta) * u1t2 * u2t2 * u3t2) / theta + (2 * 
    (-1 + theta) * (-1 + 2 * theta) * u1t2 * u2t2 * u3t2) / theta + 
    ((theta + theta * u123t)^2) * log(1 - u1) - 3 * (-1 + t.i) * t2 * 
    (1 - u1tt) * (1 - u2tt) * (1 + u123t) * (1 - u3tt) * log(1 - u1) +
    3 * (-1 + t.i) * t2 * u1tt * (1 - u2tt) * (1 + u123t) * (1 - u3tt) * 
    log(1 - u1) + (-1 + theta) * (-1 + 2 * theta) * u1t2 * u2t2 * u3t2 *
    log(1 - u1) + 2 * (-1 + theta) * (-1 + 2 * theta) * (u1t) * u1tt * u2t2 * 
    u3t2 * log(1 - u1) + ((theta + theta * u123t)^2) * log(1 - u2) -
    3 * (-1 + t.i) * t2 * (1 - u1tt) * (1 - u2tt) * (1 + u123t) * (1 - u3tt) * 
    log(1 - u2) + 3 * (-1 + t.i) * t2 * (1 - u1tt) * u2tt * (1 + u123t) * 
    (1 - u3tt) * log(1 - u2) + (-1 + theta) * (-1 + 2 * theta) * u1t2 * u2t2 * 
    u3t2 * log(1 - u2) + 2 * (-1 + theta) * (-1 + 2 * theta) * u1t2 * u2t * 
    u2tt * u3t2 * log(1 - u2) + ((theta + theta * u123t)^2) * log(1 - u3) - 
    3 * (-1 + t.i) * t2 * (1 - u1tt) * (1 - u2tt) * (1 + u123t) * (1 - u3tt) * 
    log(1 - u3) + (-1 + theta) * (-1 + 2 * theta) * u1t2 * u2t2 * u3t2 * 
    log(1 - u3) + 3 * (-1 + t.i) * t2 * (1 - u1tt) * (1 - u2tt) * 
    (1 + u123t) * u3tt * log(1 - u3) + 2 * (-1 + theta) * (-1 + 2 * theta) * 
    u1t2 * u2t2 * u3t * u3tt * log(1 - u3) + (1 + u123t) * ((-1 - u123t) * 
    log(1 + u123t) + (1 - theta) * theta * (u1tt * u2t * u3t * log(1 - u1) + 
    u1t * (u2tt * u3t * log(1 - u2) + u2t * u3tt * log(1 - u3)))) + 
    (-2 + t.i) * (-1 + t.i) * t2 * u1t2 * u2t2 * u3t2 * 
    (-(log(1 + u123t) / t2) + ((-3 + t.i) * (u1tt * u2t * u3t * log(1 - u1) + 
    u1t * (u2tt * u3t * log(1 - u2) + u2t * u3tt * log(1 - u3)))) / 
    (1 + u123t)) - 3 * (-1 + t.i) * t2 * (1 - u1tt) * (1 - u2tt) * 
    (1 + u123t) * (1 - u3tt) * (-(log(1 + u123t) / t2) + ((-2 + t.i) * 
    (u1tt * u2t * u3t * log(1 - u1) + u1t * (u2tt * u3t * log(1 - u2) + u2t * 
    u3tt * log(1 - u3)))) / (1 + u123t))) / (t2 * (2 - u1tt - u2tt + u1tt * 
    u2tt + (3 * u123t) / theta + (u1t2 * u2t2 * u3t2) / t2 -
    u1t * u2t * u3tt))
}

# 3d second derivative wrt theta of the log-density
.joe.123.S <- function(theta, u) { 
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
    u3 <- u[3]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
    u3 <- u[, 3]
  }

  u1tt <- (1 - u1)^theta
  u2tt <- (1 - u2)^theta
  u3tt <- (1 - u3)^theta
  u1t <- -1 + u1tt
  u2t <- -1 + u2tt
  u3t <- -1 + u3tt

  u1t2 <- u1t^2
  u2t2 <- u2t^2
  u3t2 <- u3t^2

  u123t <- u1t * u2t * u3t

  u123t2 <- u123t^2
  u123tt <- u1t * u2t * u3tt

  t2 <- theta^2
  it <- 1 / theta
  tm1 <- (-1 + theta)
  t2m1 <- (-1 + 2 * theta)

  lu1 <- log(1 - u1)
  lu2 <- log(1 - u2)
  lu3 <- log(1 - u3)

  tim1 <- (-1 + it)
  u1ttm1 <- 1 - u1tt
  u2ttm1 <- 1 - u2tt
  u3ttm1 <- 1 - u3tt

  aaa <- (u1tt * u2t * u3t * lu1 + u1t * (u2tt * u3t * lu2 + u2t * u3tt * lu3))
  bbb <- (u1tt * u2t * u3t * lu1 + u1t * u2tt * u3t * lu2 + u123tt * lu3)
  ccc <- tim1 * t2 * u1ttm1
  u123tp1 <- 1 + u123t
  l1u123t <- log(u123tp1)

  (-2 * (2 - u1tt - u2tt + u1tt * u2tt + (3 * u123t) / theta + u123t2 / 
    t2 - u123tt) * (2 * theta * (u123tp1^2) + 3 * u1ttm1 * u2ttm1 * u123tp1 * 
    u3ttm1 - 6 * tim1 * theta * u1ttm1 * u2ttm1 * u123tp1 * u3ttm1 + 
    (tm1 * u123t2) / theta + (t2m1 * u123t2) / theta + (2 * tm1 * t2m1 * 
    u123t2) / theta + ((theta + theta * u123t)^2) * lu1 - 3 * ccc * u2ttm1 * 
    u123tp1 * u3ttm1 * lu1 + 3 * tim1 * t2 * u1tt * u2ttm1 * u123tp1 * 
    u3ttm1 * lu1 + tm1 * t2m1 * u123t2 * lu1 + 2 * tm1 * t2m1 * u1t * u1tt * 
    u2t2 * u3t2 * lu1 + ((theta + theta * u123t)^2) * lu2 - 3 * ccc * u2ttm1 * 
    u123tp1 * u3ttm1 * lu2 + 3 * ccc * u2tt * u123tp1 * u3ttm1 * lu2 + tm1 * 
    t2m1 * u123t2 * lu2 + 2 * tm1 * t2m1 * u1t2 * u2t * u2tt * u3t2 * lu2 + 
    ((theta + theta * u123t)^2) * lu3 - 3 * ccc * u2ttm1 * u123tp1 * u3ttm1 * 
    lu3 + tm1 * t2m1 * u123t2 * lu3 + 3 * ccc * u2ttm1 * u123tp1 * u3tt * lu3 + 
    2 * tm1 * t2m1 * u1t2 * u2t2 * u3t * u3tt * lu3 + u123tp1 * ((-1 - u123t) * 
    l1u123t + (1 - theta) * theta * aaa) + (-2 + it) * tim1 * t2 * u123t2 * 
    (-(l1u123t / t2) + ((-3 + it) * aaa) / u123tp1) - 3 * ccc * u2ttm1 * 
    u123tp1 * u3ttm1 * (-(l1u123t / t2) + ((-2 + it) * aaa) / u123tp1)) - 
    theta * ((-3 * u123t) / t2 - (2 * u123t2) / (theta^3) - u1tt * lu1 + 
    u1tt * u2tt * lu1 + (3 * u1tt * u2t * u3t * lu1) / theta + (2 * u1t * 
    u1tt * u2t2 * u3t2 * lu1) / t2 - u1tt * u2t * u3tt * lu1 - u2tt * lu2 + 
    u1tt * u2tt * lu2 + (3 * u1t * u2tt * u3t * lu2) / theta + (2 * u1t2 * 
    u2t * u2tt * u3t2 * lu2) / t2 - u1t * u2tt * u3tt * lu2 - u123tt * lu3 + 
    (3 * u123tt * lu3) / theta + (2 * u1t2 * u2t2 * u3t * u3tt * lu3) / t2) * 
    (2 * theta * (u123tp1^2) + 3 * u1ttm1 * u2ttm1 * u123tp1 * u3ttm1 -
    6 * tim1 * theta * u1ttm1 * u2ttm1 * u123tp1 * u3ttm1 + (tm1 * u123t2) / 
    theta + (t2m1 * u123t2) / theta + (2 * tm1 * t2m1 * u123t2) / theta + 
    ((theta + theta * u123t)^2) * lu1 - 3 * ccc * u2ttm1 * u123tp1 * u3ttm1 * 
    lu1 + 3 * tim1 * t2 * u1tt * u2ttm1 * u123tp1 * u3ttm1 * lu1 + tm1 * 
    t2m1 * u123t2 * lu1 + 2 * tm1 * t2m1 * u1t * u1tt * u2t2 * u3t2 * lu1 + 
    ((theta + theta * u123t)^2) * lu2 - 3 * ccc * u2ttm1 * u123tp1 * u3ttm1 * 
    lu2 + 3 * ccc * u2tt * u123tp1 * u3ttm1 * lu2 + tm1 * t2m1 * u123t2 * 
    lu2 + 2 * tm1 * t2m1 * u1t2 * u2t * u2tt * u3t2 * lu2 + 
    ((theta + theta * u123t)^2) * lu3 - 3 * ccc * u2ttm1 * u123tp1 * 
    u3ttm1 * lu3 + tm1 * t2m1 * u123t2 * lu3 + 3 * ccc * u2ttm1 * u123tp1 * 
    u3tt * lu3 + 2 * tm1 * t2m1 * u1t2 * u2t2 * u3t * u3tt * lu3 + u123tp1 * 
    ((-1 - u123t) * l1u123t + (1 - theta) * theta * aaa) + (-2 + it) * tim1 * 
    t2 * u123t2 * (-(l1u123t / t2) + ((-3 + it) * aaa) / u123tp1) - 3 * ccc * 
    u2ttm1 * u123tp1 * u3ttm1 * (-(l1u123t / t2) + ((-2 + it) * aaa) / 
    u123tp1)) + theta * (2 - u1tt - u2tt + u1tt * u2tt + (3 * u123t) / theta + 
    u123t2 / t2 - u123tt) * (2 * (u123tp1^2) - 6 * tim1 * u1ttm1 * u2ttm1 * 
    u123tp1 * u3ttm1 + (6 * u1ttm1 * u2ttm1 * u123tp1 * u3ttm1) / theta -
    (tm1 * u123t2) / t2 + (3 * u123t2) / theta + (4 * tm1 * u123t2) / theta - 
    (t2m1 * u123t2) / t2 - (2 * tm1 * t2m1 * u123t2) / t2 + (2 * t2m1 * 
    u123t2) / theta + 3 * u1ttm1 * u2ttm1 * u123tp1 * u3ttm1 * lu1 - 6 * 
    tim1 * theta * u1ttm1 * u2ttm1 * u123tp1 * u3ttm1 * lu1 - 6 * u1tt * 
    u2ttm1 * u123tp1 * u3ttm1 * lu1 + 12 * tim1 * theta * u1tt * u2ttm1 * 
    u123tp1 * u3ttm1 * lu1 + 2 * tm1 * u123t2 * lu1 + t2m1 * u123t2 * lu1 + 
    4 * tm1 * u1t * u1tt * u2t2 * u3t2 * lu1 + (2 * tm1 * u1t * u1tt * u2t2 * 
    u3t2 * lu1) / theta + 2 * t2m1 * u1t * u1tt * u2t2 * u3t2 * lu1 + (2 * 
    t2m1 * u1t * u1tt * u2t2 * u3t2 * lu1) / theta + (4 * tm1 * t2m1 * u1t * 
    u1tt * u2t2 * u3t2 * lu1) / theta + 6 * tim1 * t2 * u1tt * u2ttm1 * 
    u123tp1 * u3ttm1 * (lu1^2) + 4 * tm1 * t2m1 * u1t * u1tt * u2t2 * u3t2 * 
    (lu1^2) + 2 * tm1 * t2m1 * ((1 - u1)^(2 * theta)) * u2t2 * u3t2 * (lu1^2) +
    3 * u1ttm1 * u2ttm1 * u123tp1 * u3ttm1 * lu2 - 6 * tim1 * theta * u1ttm1 * 
    u2ttm1 * u123tp1 * u3ttm1 * lu2 - 6 * u1ttm1 * u2tt * u123tp1 * u3ttm1 * 
    lu2 + 12 * tim1 * theta * u1ttm1 * u2tt * u123tp1 * u3ttm1 * lu2 +
    2 * tm1 * u123t2 * lu2 + t2m1 * u123t2 * lu2 + 4 * tm1 * u1t2 * u2t * 
    u2tt * u3t2 * lu2 + (2 * tm1 * u1t2 * u2t * u2tt * u3t2 * lu2) / theta + 
    2 * t2m1 * u1t2 * u2t * u2tt * u3t2 * lu2 + (2 * t2m1 * u1t2 * u2t * 
    u2tt * u3t2 * lu2) / theta + (4 * tm1 * t2m1 * u1t2 * u2t * u2tt * u3t2 * 
    lu2) / theta + 3 * tim1 * t2 * u1tt * u2ttm1 * u123tp1 * u3ttm1 * lu1 * 
    lu2 + 3 * ccc * u2tt * u123tp1 * u3ttm1 * lu1 * lu2 - 6 * tim1 * t2 * 
    u1tt * u2tt * u123tp1 * u3ttm1 * lu1 * lu2 + 2 * tm1 * t2m1 * u1t * u1tt * 
    u2t2 * u3t2 * lu1 * lu2 + 2 * tm1 * t2m1 * u1t2 * u2t * u2tt * u3t2 * 
    lu1 * lu2 + 8 * tm1 * t2m1 * u1t * u1tt * u2t * u2tt * u3t2 * lu1 * lu2 + 
    6 * ccc * u2tt * u123tp1 * u3ttm1 * (lu2^2) + 4 * tm1 * t2m1 * u1t2 * 
    u2t * u2tt * u3t2 * (lu2^2) + 2 * tm1 * t2m1 * u1t2 * 
    ((1 - u2)^(2 * theta)) * u3t2 * (lu2^2) + 3 * u1ttm1 * u2ttm1 * u123tp1 * 
    u3ttm1 * lu3 - 6 * tim1 * theta * u1ttm1 * u2ttm1 * u123tp1 * u3ttm1 * 
    lu3 + 2 * tm1 * u123t2 * lu3 + t2m1 * u123t2 * lu3 - 6 * u1ttm1 * u2ttm1 * 
    u123tp1 * u3tt * lu3 + 12 * tim1 * theta * u1ttm1 * u2ttm1 * u123tp1 * 
    u3tt * lu3 + 4 * tm1 * u1t2 * u2t2 * u3t * u3tt * lu3 + (2 * tm1 * u1t2 * 
    u2t2 * u3t * u3tt * lu3) / theta + 2 * t2m1 * u1t2 * u2t2 * u3t * u3tt * 
    lu3 + (2 * t2m1 * u1t2 * u2t2 * u3t * u3tt * lu3) / theta + (4 * tm1 * 
    t2m1 * u1t2 * u2t2 * u3t * u3tt * lu3) / theta + 3 * tim1 * t2 * u1tt * 
    u2ttm1 * u123tp1 * u3ttm1 * lu1 * lu3 + 2 * tm1 * t2m1 * u1t * u1tt * 
    u2t2 * u3t2 * lu1 * lu3 + 3 * ccc * u2ttm1 * u123tp1 * u3tt * lu1 * lu3 - 
    6 * tim1 * t2 * u1tt * u2ttm1 * u123tp1 * u3tt * lu1 * lu3 + 2 * tm1 * 
    t2m1 * u1t2 * u2t2 * u3t * u3tt * lu1 * lu3 + 8 * tm1 * t2m1 * u1t * 
    u1tt * u2t2 * u3t * u3tt * lu1 * lu3 + 3 * ccc * u2tt * u123tp1 * u3ttm1 * 
    lu2 * lu3 + 2 * tm1 * t2m1 * u1t2 * u2t * u2tt * u3t2 * lu2 * lu3 + 3 * 
    ccc * u2ttm1 * u123tp1 * u3tt * lu2 * lu3 - 6 * ccc * u2tt * u123tp1 * 
    u3tt * lu2 * lu3 + 2 * tm1 * t2m1 * u1t2 * u2t2 * u3t * u3tt * lu2 * lu3 + 
    8 * tm1 * t2m1 * u1t2 * u2t * u2tt * u3t * u3tt * lu2 * lu3 + 6 * ccc * 
    u2ttm1 * u123tp1 * u3tt * (lu3^2) + 4 * tm1 * t2m1 * u1t2 * u2t2 * u3t * 
    u3tt * (lu3^2) + 2 * tm1 * t2m1 * u1t2 * u2t2 * ((1 - u3)^(2 * theta)) * 
    (lu3^2) + 4 * theta * u123tp1 * (u1tt * u2t * u3t * lu1 + u1t * u2tt * 
    u3t * lu2 + u123tt * lu3) + 3 * u1ttm1 * u2ttm1 * u3ttm1 * bbb - 6 * tim1 * 
    theta * u1ttm1 * u2ttm1 * u3ttm1 * (u1tt * u2t * u3t * lu1 + u1t * u2tt * 
    u3t * lu2 + u123tt * lu3) - 3 * ccc * u2ttm1 * u3ttm1 * lu1 * bbb + 3 * 
    tim1 * t2 * u1tt * u2ttm1 * u3ttm1 * lu1 * bbb - 3 * ccc * u2ttm1 * 
    u3ttm1 * lu2 * bbb + 3 * ccc * u2tt * u3ttm1 * lu2 * bbb - 3 * ccc * 
    u2ttm1 * u3ttm1 * lu3 * bbb + 3 * ccc * u2ttm1 * u3tt * lu3 * bbb + 2 * 
    (theta + theta * u123t) * lu1 * (u123tp1 + theta * u1tt * u2t * u3t * 
    lu1 + theta * u1t * u2tt * u3t * lu2 + theta * u123tt * lu3) + 2 * 
    (theta + theta * u123t) * lu2 * (u123tp1 + theta * u1tt * u2t * u3t * 
    lu1 + theta * u1t * u2tt * u3t * lu2 + theta * u123tt * lu3) + 2 * 
    (theta + theta * u123t) * lu3 * (u123tp1 + theta * u1tt * u2t * u3t * 
    lu1 + theta * u1t * u2tt * u3t * lu2 + theta * u123tt * lu3) + (u1tt * 
    u2t * u3t * lu1 + u1t * u2tt * u3t * lu2 + u123tt * lu3) * ((-1 - u123t) * 
    l1u123t + (1 - theta) * theta * aaa) - (-2 + it) * u123t2 * 
    (-(l1u123t / t2) + ((-3 + it) * aaa) / u123tp1) - tim1 * u123t2 * 
    (-(l1u123t / t2) + ((-3 + it) * aaa) / u123tp1) + 2 * (-2 + it) * tim1 * 
    theta * u123t2 * (-(l1u123t / t2) + ((-3 + it) * aaa) / u123tp1) + 2 * 
    (-2 + it) * tim1 * t2 * u1t * u1tt * u2t2 * u3t2 * lu1 * (-(l1u123t / t2) +
    ((-3 + it) * aaa) / u123tp1) + 2 * (-2 + it) * tim1 * t2 * u1t2 * u2t * 
    u2tt * u3t2 * lu2 * (-(l1u123t / t2) + ((-3 + it) * aaa) / u123tp1) + 2 * 
    (-2 + it) * tim1 * t2 * u1t2 * u2t2 * u3t * u3tt * lu3 * (-(l1u123t / t2) + 
    ((-3 + it) * aaa) / u123tp1) + 3 * u1ttm1 * u2ttm1 * u123tp1 * u3ttm1 * 
    (-(l1u123t / t2) + ((-2 + it) * aaa) / u123tp1) - 6 * tim1 * theta * 
    u1ttm1 * u2ttm1 * u123tp1 * u3ttm1 * (-(l1u123t / t2) + ((-2 + it) * aaa) /
    u123tp1) + 3 * tim1 * t2 * u1tt * u2ttm1 * u123tp1 * u3ttm1 * lu1 * 
    (-(l1u123t / t2) + ((-2 + it) * aaa) / u123tp1) + 3 * ccc * u2tt * 
    u123tp1 * u3ttm1 * lu2 * (-(l1u123t / t2) + ((-2 + it) * aaa) / u123tp1) + 
    3 * ccc * u2ttm1 * u123tp1 * u3tt * lu3 * (-(l1u123t / t2) + ((-2 + it) * 
    aaa) / u123tp1) - 3 * ccc * u2ttm1 * u3ttm1 * bbb * (-(l1u123t / t2) + 
    ((-2 + it) * aaa) / u123tp1) + u123tp1 * (-(u1tt * u2t * u3t * lu1) - 
    u1t * u2tt * u3t * lu2 - u123tt * lu3 - l1u123t * bbb + (1 - theta) * 
    (u1tt * u2t * u3t * lu1 + u1t * (u2tt * u3t * lu2 + u2t * u3tt * lu3)) - 
    theta * aaa + (1 - theta) * theta * (u1tt * u2t * u3t * (lu1^2) + 2 * 
    u1tt * lu1 * (u2tt * u3t * lu2 + u2t * u3tt * lu3) + u1t * (u2tt * u3t * 
    (lu2^2) + 2 * u2tt * u3tt * lu2 * lu3 + u2t * u3tt * (lu3^2)))) + (3 * 
    tim1 * u1ttm1 * u2ttm1 * u123tp1 * u3ttm1 * (-2 * l1u123t + (theta * bbb) / 
    u123tp1 + (theta * aaa) / u123tp1 - (t2 * t2m1 * ((u1tt * u2t * u3t * 
    lu1 + u1t * (u2tt * u3t * lu2 + u2t * u3tt * lu3))^2)) / ((u1tt + u2tt - 
    u1tt * u2tt + u3tt - u1tt * u3tt - u2tt * u3tt + u1tt * u2tt * u3tt)^2) +
    (t2 * t2m1 * (u1tt * u2t * u3t * (lu1^2) + 2 * u1tt * lu1 * (u2tt * u3t * 
    lu2 + u2t * u3tt * lu3) + u1t * (u2tt * u3t * (lu2^2) + 2 * u2tt * u3tt * 
    lu2 * lu3 + u2t * u3tt * (lu3^2)))) / (u1tt + u2tt - u1tt * u2tt + u3tt - 
    u1tt * u3tt - u2tt * u3tt + u1tt * u2tt * u3tt))) / theta - ((-2 + it) * 
    tim1 * u123t2 * (-2 * l1u123t + (theta * bbb) / u123tp1 + (theta * aaa) / 
    u123tp1 - (t2 * (-1 + 3 * theta) * ((u1tt * u2t * u3t * lu1 + u1t * 
    (u2tt * u3t * lu2 + u2t * u3tt * lu3))^2)) / ((u1tt + u2tt - u1tt * 
    u2tt + u3tt - u1tt * u3tt - u2tt * u3tt + u1tt * u2tt * u3tt)^2) + (t2 * 
    (-1 + 3 * theta) * (u1tt * u2t * u3t * (lu1^2) + 2 * u1tt * lu1 * (u2tt * 
    u3t * lu2 + u2t * u3tt * lu3) + u1t * (u2tt * u3t * (lu2^2) + 2 * u2tt * 
    u3tt * lu2 * lu3 + u2t * u3tt * (lu3^2)))) / (u1tt + u2tt - u1tt * u2tt + 
    u3tt - u1tt * u3tt - u2tt * u3tt + u1tt * u2tt * u3tt))) / theta)) / 
    ((theta^3) * ((-2 + u1tt + u2tt - u1tt * u2tt - (3 * u123t) / theta - 
    u123t2 / t2 + u123tt)^2))
}
