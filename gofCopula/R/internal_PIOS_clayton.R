# Internal functions for the derivation of the clayton copula for the PIOSRn and 
# PIOSTn test statistics. Called by .Rn and .Tn, the internal functions for
# their test statistics.

# 2d first derivative wrt theta of the log-density
.V.clay.12 <- function(theta, u1, u2) {
  res.f <- -((theta * (1 + theta) * ((u2^theta) + theta * 
            (-(u1^theta) + (u2^theta) + .Power(u1 * u2, theta))) * log(u1) + 
            theta * (1 + theta) * ((1 + theta) * (u1^theta) + theta * 
            (-1 + (u1^theta)) * (u2^theta)) * log(u2) + ((u1^theta) + 
            (u2^theta) - .Power(u1 * u2, theta)) * ((theta^2) + (1 + theta) * 
            log(-1 + .Power(u1, -theta) + .Power(u2, -theta)))) / 
            ((theta^2) * (1 + theta) * (-(u2^theta) + (u1^theta) * 
            (-1 + (u2^theta)))))
  return(res.f)
}

# 2d second derivative wrt theta of the log-density
.S.clay.12 <- function(theta, u1, u2) {
  res.f <- ((theta^2) * .Power(1 + theta, 2) * (1 + 2 * theta) * 
            ((u1^theta) * .Power(u2, 2 * theta) - .Power(u1 * u2, theta)) * 
            .Power(log(u1), 2) + 2 * theta * .Power(1 + theta, 2) * 
            (-.Power(u1 * u2, theta) + .Power(u1, 2 * theta) * (-1 + 
            (u2^theta))) * log(u2) + (theta^2) * .Power(1 + theta, 2) * 
            (1 + 2 * theta) * (.Power(u1, 2 * theta) * (u2^theta) - 
            .Power(u1 * u2, theta)) * .Power(log(u2), 2) + 2 * theta * 
            .Power(1 + theta, 2) * log(u1) * ((-1 + (u1^theta)) * 
            .Power(u2, 2 * theta) - .Power(u1 * u2, theta) + theta * 
            (1 + 2 * theta) * .Power(u1 * u2, theta) * log(u2)) + 
            (-.Power(u2, 2 * theta) + 2 * (u1^theta) * .Power(u2, 2 * theta) + 
            .Power(u1, 2 * theta) * (-1 + 2 * (u2^theta)) - 
            .Power(u1 * u2, theta) * (2 + .Power(u1 * u2, theta))) * 
            (.Power(theta, 3) + 2 * .Power(1 + theta, 2) * log(-1 + 
            .Power(u1, -theta) + .Power(u2, -theta)))) / (.Power(theta, 3) * 
            .Power(1 + theta, 2) * .Power((u1^theta) + (u2^theta) - 
            .Power(u1 * u2, theta), 2))
  return(res.f)
}

# 2d density
.clay.12.density <- function(x, u) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
  }
  u1pt <- u1^(-x)
  u2pt <- u2^(-x)
  res.f <- (1 + x) * u1pt * u2pt * ((u1pt + u2pt - 1)^(-1 / x - 2)) / (u1 * u2)
  return(res.f)
}

# 3d density
.clay.123.density <- function(x, u) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
    u3 <- u[3]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
    u3 <- u[, 3]
  }
  u1pt <- u1^(-x)
  u2pt <- u2^(-x)
  u3pt <- u3^(-x)
  u123 <- u1 * u2 * u3
  res.f <- ((1 + x) * (1 + 2 * x) * ((u123)^(2 * x - 1))) / 
    (((u1pt + u2pt + u3pt - 2)^(1 / x)) * ((u1 * u2)^x + (u1 * u3)^x + 
    (u2 * u3)^x - 2 * (u123)^x)^3)
  return(res.f)
}

# 3d second derivative wrt theta of the log-density
.clay.S.123 <- function(theta, u) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
    u3 <- u[3]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
    u3 <- u[, 3]
  }
  
  t2 <- theta^2
  t3 <- theta^3
  t4 <- theta^4
  t5 <- theta^5
  t6 <- theta^6
  u12 <- u1 * u2
  u13 <- u1 * u3
  u23 <- u2 * u3
  u123 <- u1 * u2 * u3
  
  u1t <- .Power(u1, -theta)
  u2t <- .Power(u2, -theta)
  u3t <- .Power(u3, -theta)
  u1tt <- .Power(u1, theta)
  u2tt <- .Power(u2, theta)
  u3tt <- .Power(u3, theta)
  u1tt2 <- u1tt^2
  u2tt2 <- u2tt^2
  u3tt2 <- u3tt^2
  l3 <- log(u3)
  l3p2 <- .Power(l3, 2)
  lu123t <- log(-2 + u1t + u2t + u3t)
  
  a12t <- .Power(u12, theta)
  a13t <- .Power(u13, theta)
  a23t <- .Power(u23, theta)
  
  a12t2 <- a12t^2
  a13t2 <- a13t^2
  a23t2 <- a23t^2
  
  res.f <- -((5 * t3 * a12t2 + 12 * t4 * a12t2 + 8 * t5 * a12t2 - 20 * t3 * 
            a12t2 * u3tt - 48 * t4 * a12t2 * u3tt - 32 * t5 * a12t2 * u3tt + 
            10 * t3 * a12t * u3tt2 + 24 * t4 * a12t * u3tt2 + 16 * t5 * a12t * 
            u3tt2 + 10 * t3 * u2tt2 * a13t + 24 * t4 * u2tt2 * a13t + 16 * t5 * 
            u2tt2 * a13t + 5 * t3 * a13t2 + 12 * t4 * a13t2 + 8 * t5 * a13t2 - 
            20 * t3 * u2tt * a13t2 - 48 * t4 * u2tt * a13t2 - 32 * t5 * u2tt * 
            a13t2 + 10 * t3 * u1tt2 * a23t + 24 * t4 * u1tt2 * a23t + 16 * 
            t5 * u1tt2 * a23t + 5 * t3 * a23t2 + 12 * t4 * a23t2 + 8 * t5 * 
            a23t2 - 20 * t3 * u1tt * a23t2 - 48 * t4 * u1tt * a23t2 - 32 * 
            t5 * u1tt * a23t2 + 20 * t3 * .Power(u123, 2 * theta) + 48 * 
            t4 * .Power(u123, 2 * theta) + 32 * t5 * .Power(u123, 2 * theta) + 
            t2 * (1 + 3 * theta) * .Power(1 + 3 * theta + 2 * t2, 2) * 
            (a12t * u3tt2 + u2tt2 * a13t - 2 * u1tt * a23t2) * 
            .Power(log(u1), 2) + t2 * (1 + 3 * theta) * .Power(1 + 3 * 
            theta + 2 * t2, 2) * (a12t * u3tt2 - 2 * u2tt * a13t2 + 
            u1tt2 * a23t) * .Power(log(u2), 2) + 2 * theta * a12t2 * l3 + 
            12 * t2 * a12t2 * l3 + 26 * t3 * a12t2 * l3 + 24 * t4 * a12t2 * 
            l3 + 8 * t5 * a12t2 * l3 - 4 * theta * a12t2 * u3tt * l3 - 24 * 
            t2 * a12t2 * u3tt * l3 - 52 * t3 * a12t2 * u3tt * l3 - 48 * t4 * 
            a12t2 * u3tt * l3 - 16 * t5 * a12t2 * u3tt * l3 + 2 * theta * 
            u2tt2 * a13t * l3 + 12 * t2 * u2tt2 * a13t * l3 + 26 * t3 * 
            u2tt2 * a13t * l3 + 24 * t4 * u2tt2 * a13t * l3 + 8 * t5 * u2tt2 * 
            a13t * l3 + 2 * theta * u1tt2 * a23t * l3 + 12 * t2 * u1tt2 * 
            a23t * l3 + 26 * t3 * u1tt2 * a23t * l3 + 24 * t4 * u1tt2 * a23t * 
            l3 + 8 * t5 * u1tt2 * a23t * l3 - 2 * t2 * a12t2 * u3tt * l3p2 - 
            18 * t3 * a12t2 * u3tt * l3p2 - 62 * t4 * a12t2 * u3tt * l3p2 - 
            102 * t5 * a12t2 * u3tt * l3p2 - 80 * t6 * a12t2 * u3tt * l3p2 - 
            24 * .Power(theta, 7) * a12t2 * u3tt * l3p2 + t2 * u2tt2 * a13t * 
            l3p2 + 9 * t3 * u2tt2 * a13t * l3p2 + 31 * t4 * u2tt2 * a13t * 
            l3p2 + 51 * t5 * u2tt2 * a13t * l3p2 + 40 * t6 * u2tt2 * a13t * 
            l3p2 + 12 * .Power(theta, 7) * u2tt2 * a13t * l3p2 + t2 * u1tt2 * 
            a23t * l3p2 + 9 * t3 * u1tt2 * a23t * l3p2 + 31 * t4 * u1tt2 * 
            a23t * l3p2 + 51 * t5 * u1tt2 * a23t * l3p2 + 40 * t6 * u1tt2 * 
            a23t * l3p2 + 12 * .Power(theta, 7) * u1tt2 * a23t * l3p2 - 2 * 
            theta * .Power(1 + 3 * theta + 2 * t2, 2) * log(u1) * 
            (-(a12t * u3tt2) - u2tt2 * a13t - a23t2 + 2 * u1tt * a23t2 + 
            theta * (1 + 3 * theta) * a12t * u3tt2 * log(u2) + theta * 
            (1 + 3 * theta) * u2tt2 * a13t * l3) + 2 * theta * 
            .Power(1 + 3 * theta + 2 * t2, 2) * log(u2) * (a12t * u3tt2 + 
            a13t2 - 2 * u2tt * a13t2 + u1tt2 * a23t - theta * (1 + 3 * theta) * 
            u1tt2 * a23t * l3) + 2 * a12t2 * lu123t + 12 * theta * a12t2 * 
            lu123t + 26 * t2 * a12t2 * lu123t + 24 * t3 * a12t2 * lu123t + 8 * 
            t4 * a12t2 * lu123t - 8 * a12t2 * u3tt * lu123t - 48 * theta * 
            a12t2 * u3tt * lu123t - 104 * t2 * a12t2 * u3tt * lu123t - 96 * 
            t3 * a12t2 * u3tt * lu123t - 32 * t4 * a12t2 * u3tt * lu123t + 
            4 * a12t * u3tt2 * lu123t + 24 * theta * a12t * u3tt2 * lu123t + 
            52 * t2 * a12t * u3tt2 * lu123t + 48 * t3 * a12t * u3tt2 * 
            lu123t + 16 * t4 * a12t * u3tt2 * lu123t + 4 * u2tt2 * a13t * 
            lu123t + 24 * theta * u2tt2 * a13t * lu123t + 52 * t2 * u2tt2 * 
            a13t * lu123t + 48 * t3 * u2tt2 * a13t * lu123t + 16 * t4 * 
            u2tt2 * a13t * lu123t + 2 * a13t2 * lu123t + 12 * theta * a13t2 * 
            lu123t + 26 * t2 * a13t2 * lu123t + 24 * t3 * a13t2 * lu123t + 8 * 
            t4 * a13t2 * lu123t - 8 * u2tt * a13t2 * lu123t - 48 * theta * 
            u2tt * a13t2 * lu123t - 104 * t2 * u2tt * a13t2 * lu123t - 96 * 
            t3 * u2tt * a13t2 * lu123t - 32 * t4 * u2tt * a13t2 * lu123t + 4 * 
            u1tt2 * a23t * lu123t + 24 * theta * u1tt2 * a23t * lu123t + 52 * 
            t2 * u1tt2 * a23t * lu123t + 48 * t3 * u1tt2 * a23t * lu123t + 16 * 
            t4 * u1tt2 * a23t * lu123t + 2 * a23t2 * lu123t + 12 * theta * 
            a23t2 * lu123t + 26 * t2 * a23t2 * lu123t + 24 * t3 * a23t2 * 
            lu123t + 8 * t4 * a23t2 * lu123t - 8 * u1tt * a23t2 * lu123t - 
            48 * theta * u1tt * a23t2 * lu123t - 104 * t2 * u1tt * a23t2 * 
            lu123t - 96 * t3 * u1tt * a23t2 * lu123t - 32 * t4 * u1tt * 
            a23t2 * lu123t + 8 * .Power(u123, 2 * theta) * lu123t + 48 * 
            theta * .Power(u123, 2 * theta) * lu123t + 104 * t2 * 
            .Power(u123, 2 * theta) * lu123t + 96 * t3 * .Power(u123, 2 * 
            theta) * lu123t + 32 * t4 * .Power(u123, 2 * theta) * lu123t) / 
            (t3 * .Power(1 + theta, 2) * .Power(1 + 2 * theta, 2) * 
            .Power(a12t + a13t + a23t - 2 * .Power(u123, theta), 2)))
              return(res.f)
}

# 3d first derivative wrt theta of the log-density
.clay.V.123 <- function(theta, u) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
    u3 <- u[3]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
    u3 <- u[, 3]
  }
  
  t2 <- theta^2
  t3 <- theta^3
  t4 <- theta^4
  t5 <- theta^5
  t6 <- theta^6
  u12 <- u1 * u2
  u13 <- u1 * u3
  u23 <- u2 * u3
  u123 <- u1 * u2 * u3
  l3 <- log(u3)
  u12t <- u12^theta
  u13t <- u13^theta
  u23t <- u23^theta
  u1t <- u1^(-theta)
  u2t <- u2^(-theta)
  u3t <- u3^(-theta)
  a <- log(-2 + u1t + u2t + u3t)
  p123 <- u123^theta
  
  res.f <- (3 * t2 * u12t + 4 * t3 * u12t + 3 * t2 * u13t + 4 * t3 * u13t + 
              3 * t2 * u23t + 4 * t3 * u23t - 6 * t2 * p123 - 8 * t3 * p123 + 
              theta * (1 + 3 * theta + 2 * t2) * (u23t - theta * (u12t + u13t - 
              2 * u23t - 2 * p123)) * log(u1) + theta * (1 + 3 * theta + 2 * 
              t2) * (u13t - theta * (u12t - 2 * u13t + u23t - 2 * p123)) * 
              log(u2) + theta * u12t * l3 + 5 * t2 * u12t * l3 + 8 * t3 * 
              u12t * l3 + 4 * t4 * u12t * l3 - t2 * u13t * l3 - 3 * t3 * 
              u13t * l3 - 2 * t4 * u13t * l3 - t2 * u23t * l3 - 3 * t3 * 
              u23t * l3 - 2 * t4 * u23t * l3 + 2 * t2 * p123 * l3 + 6 * t3 * 
              p123 * l3 + 4 * t4 * p123 * l3 + u12t * a + 3 * theta * u12t * 
              a + 2 * t2 * u12t * a + u13t * a + 3 * theta * u13t * a + 2 * 
              t2 * u13t * a + u23t * a + 3 * theta * u23t * a + 2 * t2 * 
              u23t * a - 2 * p123 * a - 6 * theta * p123 * a - 4 * t2 * 
              p123 * a) / (t2 * (1 + theta) * (1 + 2 * theta) * (u12t + 
              u13t + u23t - 2 * p123))
  return(res.f)
}

