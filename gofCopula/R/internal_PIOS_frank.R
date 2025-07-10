# Internal functions for the derivation of the frank copula for the PIOSRn and 
# PIOSTn test statistics. Called by .Rn and .Tn, the internal functions for
# their test statistics.

# 2d first derivative wrt theta of the log-density
.V.fran.12 <- function(theta, u1, u2) {
  res.f <- (exp(theta * (2 + u2)) * (1 + theta * (u1 - u2)) + 
              exp(theta + theta * u1) * (-1 + theta * (1 + u1 - u2)) + 
              exp(theta * (2 + u1)) * (1 + theta * (-u1 + u2)) + 
              exp(theta + theta * u2) * (-1 + theta * (1 - u1 + u2)) + 
              exp(theta * (1 + u1 + u2)) * (-1 + theta * (-2 + u1 + u2)) + 
              exp(theta * (u1 + u2)) * (1 - theta * (-1 + u1 + u2)) + 
              exp(theta) * (1 + theta * (-1 + u1 + u2)) - 
              exp(2 * theta) * (1 + theta * (u1 + u2))) / 
              ((-1 + exp(theta)) * (-exp(theta) + exp(theta + theta * u1) - 
              exp(theta * (u1 + u2)) + exp(theta + theta * u2)) * theta)
  return(res.f)
}

# 2d second derivative wrt theta of the log-density
.S.fran.12 <- function(theta, u1, u2) {
  res.f <- -((exp(2 * theta) + exp(4 * theta) + exp(2 * theta * (1 + u1)) + 
                exp(2 * theta * (2 + u1)) + exp(2 * theta * (1 + u2)) + 
                exp(2 * theta * (2 + u2)) + exp(2 * theta * (u1 + u2)) + 
                exp(2 * theta * (1 + u1 + u2)) + exp(3 * theta) * 
                (-2 + .Power(theta, 2)) + exp(theta * (3 + 2 * u1)) * 
                (-2 + .Power(theta, 2)) + exp(theta * (3 + 2 * u2)) * 
                (-2 + .Power(theta, 2)) + exp(theta + 2 * theta * u1 + 2 * 
                theta * u2) * (-2 + .Power(theta, 2)) - 2 * 
                exp(theta * (1 + u1 + 2 * u2)) * (1 + .Power(theta, 2) * 
                .Power(-1 + u1, 2)) - 2 * exp(theta * (3 + u1 + 2 * u2)) * 
                (1 + .Power(theta, 2) * .Power(-1 + u1, 2)) - 2 * 
                exp(theta * (2 + u1)) * (1 + .Power(theta, 2) * 
                .Power(u1, 2)) - 2 * exp(theta * (4 + u1)) * 
                (1 + .Power(theta, 2) * .Power(u1, 2)) + 2 * 
                exp(theta * (3 + u1)) * (2 + .Power(theta, 2) * 
                (-1 + 2 * .Power(u1, 2))) + 2 * 
                exp(theta * (2 + u1 + 2 * u2)) * (2 + .Power(theta, 2) * 
                (1 - 4 * u1 + 2 * .Power(u1, 2))) + 2 * 
                exp(theta * (4 + u1 + u2)) * (1 + .Power(theta, 2) * 
                .Power(u1 - u2, 2)) - 2 * exp(theta * (1 + 2 * u1 + u2)) * 
                (1 + .Power(theta, 2) * .Power(-1 + u2, 2)) - 2 * 
                exp(theta * (3 + 2 * u1 + u2)) * (1 + .Power(theta, 2) * 
                .Power(-1 + u2, 2)) - 2 * exp(theta * (2 + u2)) * 
                (1 + .Power(theta, 2) * .Power(u2, 2)) - 2 * 
                exp(theta * (4 + u2)) * (1 + .Power(theta, 2) * 
                .Power(u2, 2)) + 2 * exp(theta * (1 + u1 + u2)) * 
                (1 + .Power(theta, 2) * .Power(-1 + u1 + u2, 2)) - 2 * 
                exp(theta * (3 + u1 + u2)) * (1 + .Power(theta, 2) * 
                (-2 + .Power(u1, 2) + u1 * (2 - 6 * u2) + 2 * u2 + 
                .Power(u2, 2))) + 2 * exp(theta * (3 + u2)) * 
                (2 + .Power(theta, 2) * (-1 + 2 * .Power(u2, 2))) + 2 * 
                exp(theta * (2 + 2 * u1 + u2)) * (2 + .Power(theta, 2) * 
                (1 - 4 * u2 + 2 * .Power(u2, 2))) - 2 * 
                exp(theta * (2 + u1 + u2)) * (1 + .Power(theta, 2) * 
                (1 + .Power(u1, 2) - 4 * u2 + .Power(u2, 2) + u1 * 
                (-4 + 6 * u2)))) / (.Power(-1 + exp(theta), 2) * 
                .Power(exp(theta) - exp(theta + theta * u1) + 
                exp(theta * (u1 + u2)) - exp(theta + theta * u2), 2) * 
                .Power(theta, 2)))
  return(res.f)
}

# 2d density
.fran.12.density <- function(x, u) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
  }
  
  au1 <- -1 + exp(-x * u1)
  au2 <- -1 + exp(-x * u2)
  au12 <- au1 * au2
  au <- -1 + exp(-x)
  a <- au * (1 + au12 / au)
  e12 <- exp(-x * (u1 + u2))
  
  res.f <- (e12 * au12 * x) / (a^2) - (e12 * x) / a
  return(res.f)
}

# 3d second derivative wrt theta of the log-density
.fran.S.123 <- function(theta, u) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
    u3 <- u[3]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
    u3 <- u[, 3]
  }
  et <- exp(theta)
  et2 <- exp(2 * theta)
  t2 <- theta^2
  u12 <- u1 + u2
  u13 <- u1 + u3
  u23 <- u2 + u3
  et12 <- exp(theta * (2 + u12))
  et13 <- exp(theta * (2 + u13))
  et23 <- exp(theta * (2 + u23))
  
  u123 <- u12 + u3
  t2u1 <- exp(theta * (2 + u1))
  t2u2 <- exp(theta * (2 + u2))
  t2u3 <- exp(theta * (2 + u3))
  e123 <- exp(theta * (u123))
  e1123 <- exp(theta * (1 + u123))
  e2123 <- exp(theta * (2 + u123))
  a <- -et2 + t2u1 + t2u2 - et12 + t2u3 - et13 - et23 + e123 - 2 * 
    e1123 + 2 * e2123
  c1 <- et2 - t2u1 - t2u2 + et12 - t2u3 + et13 + et23 + e123 - 2 * e1123
  b <- c1^2
  
  res.f <- ((-2 * (-1 + et) * c1 * (2 * (-1 + et) * c1 * a + 2 * et * c1 * a * 
            theta + (-1 + et) * c1 * a * theta * (2 + u123) - 3 * (-1 + et) * 
            a * theta * (2 * et2 - t2u1 * (2 + u1) - t2u2 * (2 + u2) + et12 * 
            (2 + u12) - t2u3 * (2 + u3) + et13 * (2 + u13) + et23 * (2 + u23) + 
            e123 * (u123) - 2 * e1123 * (1 + u123)) + (-1 + et) * c1 * theta * 
            (-2 * et2 + t2u1 * (2 + u1) + t2u2 * (2 + u2) - et12 * (2 + u12) + 
            t2u3 * (2 + u3) - et13 * (2 + u13) - et23 * (2 + u23) + e123 * 
            (u123) - 2 * e1123 * (1 + u123) + 2 * e2123 * (2 + u123)))) / 
            a - (2 * et * c1 * theta * (2 * (-1 + et) * c1 * a + 2 * et * c1 * 
            a * theta + (-1 + et) * c1 * a * theta * (2 + u123) - 3 * 
            (-1 + et) * a * theta * (2 * et2 - t2u1 * (2 + u1) - t2u2 * 
            (2 + u2) + et12 * (2 + u12) - t2u3 * (2 + u3) + et13 * (2 + u13) + 
            et23 * (2 + u23) + e123 * (u123) - 2 * e1123 * (1 + u123)) + 
            (-1 + et) * c1 * theta * (-2 * et2 + t2u1 * (2 + u1) + t2u2 * 
            (2 + u2) - et12 * (2 + u12) + t2u3 * (2 + u3) - et13 * 
            (2 + u13) - et23 * (2 + u23) + e123 * (u123) - 2 * e1123 * 
            (1 + u123) + 2 * e2123 * (2 + u123)))) / a + ((-1 + et) * 
            c1 * theta * (-2 - u1 - u2 - u3) * (2 * (-1 + et) * c1 * a + 2 * 
            et * c1 * a * theta + (-1 + et) * c1 * a * theta * (2 + u123) - 
            3 * (-1 + et) * a * theta * (2 * et2 - t2u1 * (2 + u1) - t2u2 * 
            (2 + u2) + et12 * (2 + u12) - t2u3 * (2 + u3) + et13 * (2 + u13) + 
            et23 * (2 + u23) + e123 * (u123) - 2 * e1123 * (1 + u123)) + 
            (-1 + et) * c1 * theta * (-2 * et2 + t2u1 * (2 + u1) + t2u2 * 
            (2 + u2) - et12 * (2 + u12) + t2u3 * (2 + u3) - et13 * (2 + u13) - 
            et23 * (2 + u23) + e123 * (u123) - 2 * e1123 * (1 + u123) + 2 * 
            e2123 * (2 + u123)))) / a + (3 * (-1 + et) * theta * (2 * et2 - 
            t2u1 * (2 + u1) - t2u2 * (2 + u2) + et12 * (2 + u12) - t2u3 * 
            (2 + u3) + et13 * (2 + u13) + et23 * (2 + u23) + e123 * (u123) - 
            2 * e1123 * (1 + u123)) * (2 * (-1 + et) * c1 * a + 2 * et * c1 * 
            a * theta + (-1 + et) * c1 * a * theta * (2 + u123) - 3 * 
            (-1 + et) * a * theta * (2 * et2 - t2u1 * (2 + u1) - t2u2 * 
            (2 + u2) + et12 * (2 + u12) - t2u3 * (2 + u3) + et13 * 
            (2 + u13) + et23 * (2 + u23) + e123 * (u123) - 2 * e1123 * 
            (1 + u123)) + (-1 + et) * c1 * theta * (-2 * et2 + t2u1 * 
            (2 + u1) + t2u2 * (2 + u2) - et12 * (2 + u12) + t2u3 * (2 + u3) - 
            et13 * (2 + u13) - et23 * (2 + u23) + e123 * (u123) - 2 * e1123 * 
            (1 + u123) + 2 * e2123 * (2 + u123)))) / a - ((-1 + et) * c1 * 
            theta * (-2 * et2 + t2u1 * (2 + u1) + t2u2 * (2 + u2) - et12 * 
            (2 + u12) + t2u3 * (2 + u3) - et13 * (2 + u13) - et23 * 
            (2 + u23) + e123 * (u123) - 2 * e1123 * (1 + u123) + 2 * e2123 * 
            (2 + u123)) * (2 * (-1 + et) * c1 * a + 2 * et * c1 * a * theta + 
            (-1 + et) * c1 * a * theta * (2 + u123) - 3 * (-1 + et) * a * 
            theta * (2 * et2 - t2u1 * (2 + u1) - t2u2 * (2 + u2) + et12 * 
            (2 + u12) - t2u3 * (2 + u3) + et13 * (2 + u13) + et23 * 
            (2 + u23) + e123 * (u123) - 2 * e1123 * (1 + u123)) + (-1 + et) * 
            c1 * theta * (-2 * et2 + t2u1 * (2 + u1) + t2u2 * (2 + u2) - 
            et12 * (2 + u12) + t2u3 * (2 + u3) - et13 * (2 + u13) - et23 * 
            (2 + u23) + e123 * (u123) - 2 * e1123 * (1 + u123) + 2 * e2123 * 
            (2 + u123)))) / .Power(et2 - t2u1 - t2u2 + et12 - t2u3 + et13 + 
            et23 - e123 + 2 * e1123 - 2 * e2123, 2) + (2 * .Power(-1 + et, 2) * 
            b * a + 8 * et * (-1 + et) * b * a * theta + 2 * et2 * b * a * t2 + 
            4 * .Power(-1 + et, 2) * b * a * theta * (2 + u123) + 2 * et * 
            (-1 + et) * b * a * t2 * (2 + u123) + .Power(-1 + et, 2) * b * a * 
            t2 * .Power(2 + u123, 2) + 2 * et * (-1 + et) * b * a * t2 * 
            (3 + u123) - 12 * .Power(-1 + et, 2) * c1 * a * theta * 
            (2 * et2 - t2u1 * (2 + u1) - t2u2 * (2 + u2) + et12 * (2 + u12) - 
            t2u3 * (2 + u3) + et13 * (2 + u13) + et23 * (2 + u23) + e123 * 
            (u123) - 2 * e1123 * (1 + u123)) - 12 * et * (-1 + et) * c1 * a * 
            t2 * (2 * et2 - t2u1 * (2 + u1) - t2u2 * (2 + u2) + et12 * 
            (2 + u12) - t2u3 * (2 + u3) + et13 * (2 + u13) + et23 * 
            (2 + u23) + e123 * (u123) - 2 * e1123 * (1 + u123)) - 6 * 
            .Power(-1 + et, 2) * c1 * a * t2 * (2 + u123) * (2 * et2 - t2u1 * 
            (2 + u1) - t2u2 * (2 + u2) + et12 * (2 + u12) - t2u3 * (2 + u3) + 
            et13 * (2 + u13) + et23 * (2 + u23) + e123 * (u123) - 2 * e1123 * 
            (1 + u123)) + 12 * .Power(-1 + et, 2) * a * t2 * 
            .Power(2 * et2 - t2u1 * (2 + u1) - t2u2 * (2 + u2) + et12 * 
            (2 + u12) - t2u3 * (2 + u3) + et13 * (2 + u13) + et23 * 
            (2 + u23) + e123 * (u123) - 2 * e1123 * (1 + u123), 2) - 3 * 
            .Power(-1 + et, 2) * c1 * a * t2 * (4 * et2 - t2u1 * 
            .Power(2 + u1, 2) - t2u2 * .Power(2 + u2, 2) + et12 * 
            .Power(2 + u12, 2) - t2u3 * .Power(2 + u3, 2) + et13 * 
            .Power(2 + u13, 2) + et23 * .Power(2 + u23, 2) + e123 * 
            .Power(u123, 2) - 2 * e1123 * .Power(1 + u123, 2)) + 4 * 
            .Power(-1 + et, 2) * b * theta * (-2 * et2 + t2u1 * (2 + u1) + 
            t2u2 * (2 + u2) - et12 * (2 + u12) + t2u3 * (2 + u3) - et13 * 
            (2 + u13) - et23 * (2 + u23) + e123 * (u123) - 2 * e1123 * 
            (1 + u123) + 2 * e2123 * (2 + u123)) + 4 * et * (-1 + et) * b * 
            t2 * (-2 * et2 + t2u1 * (2 + u1) + t2u2 * (2 + u2) - et12 * 
            (2 + u12) + t2u3 * (2 + u3) - et13 * (2 + u13) - et23 * 
            (2 + u23) + e123 * (u123) - 2 * e1123 * (1 + u123) + 2 * e2123 * 
            (2 + u123)) + 2 * .Power(-1 + et, 2) * b * t2 * (2 + u123) * 
            (-2 * et2 + t2u1 * (2 + u1) + t2u2 * (2 + u2) - et12 * 
            (2 + u12) + t2u3 * (2 + u3) - et13 * (2 + u13) - et23 * 
            (2 + u23) + e123 * (u123) - 2 * e1123 * (1 + u123) + 2 * e2123 * 
            (2 + u123)) - 6 * .Power(-1 + et, 2) * c1 * t2 * (2 * et2 - t2u1 * 
            (2 + u1) - t2u2 * (2 + u2) + et12 * (2 + u12) - t2u3 * (2 + u3) + 
            et13 * (2 + u13) + et23 * (2 + u23) + e123 * (u123) - 2 * e1123 * 
            (1 + u123)) * (-2 * et2 + t2u1 * (2 + u1) + t2u2 * (2 + u2) - 
            et12 * (2 + u12) + t2u3 * (2 + u3) - et13 * (2 + u13) - et23 * 
            (2 + u23) + e123 * (u123) - 2 * e1123 * (1 + u123) + 2 * e2123 * 
            (2 + u123)) + .Power(-1 + et, 2) * b * t2 * (-4 * et2 + t2u1 * 
            .Power(2 + u1, 2) + t2u2 * .Power(2 + u2, 2) - et12 * 
            .Power(2 + u12, 2) + t2u3 * .Power(2 + u3, 2) - et13 * 
            .Power(2 + u13, 2) - et23 * .Power(2 + u23, 2) + e123 * 
            .Power(u123, 2) - 2 * e1123 * .Power(1 + u123, 2) + 2 * e2123 * 
            .Power(2 + u123, 2))) / a) / (.Power(-1 + et, 2) * b * t2)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            return(res.f)
}

# 3d first derivative wrt theta of the log-density
.fran.V.123 <- function(theta, u) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
    u3 <- u[3]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
    u3 <- u[, 3]
  }
  et <- .Power(E, theta)
  et2 <- .Power(E, 2 * theta)
  et12 <- .Power(E, theta * (2 + u1 + u2))
  et13 <- .Power(E, theta * (2 + u1 + u3))
  et23 <- .Power(E, theta * (2 + u2 + u3))
  
  u123 <- u1 + u2 + u3
  t2u1 <- .Power(E, theta * (2 + u1))
  t2u2 <- .Power(E, theta * (2 + u2))
  t2u3 <- .Power(E, theta * (2 + u3))
  e123 <- .Power(E, theta * (u123))
  e1123 <- .Power(E, theta * (1 + u123))
  e2123 <- .Power(E, theta * (2 + u123))
  a <- et2 - t2u1 - t2u2 + et12 - t2u3 + et13 + et23 + e123 - 2 * e1123
  b <- -et2 + t2u1 + t2u2 - et12 + t2u3 - et13 - et23 + e123 - 
    2 * e1123 + 2 * e2123
  
  res.f <- (2 * (-1 + et) * a * b + 2 * et * a * b * theta + (-1 + et) * a * 
              b * theta * (2 + u123) - 3 * (-1 + et) * b * theta * (2 * et2 - 
              t2u1 * (2 + u1) - t2u2 * (2 + u2) + et12 * (2 + u1 + u2) - 
              t2u3 * (2 + u3) + et13 * (2 + u1 + u3) + et23 * (2 + u2 + u3) + 
              e123 * u123 - 2 * e1123 * (1 + u123)) + (-1 + et) * a * theta * 
              (-2 * et2 + t2u1 * (2 + u1) + t2u2 * (2 + u2) - et12 * 
              (2 + u1 + u2) + t2u3 * (2 + u3) - et13 * (2 + u1 + u3) - 
              et23 * (2 + u2 + u3) + e123 * u123 - 2 * e1123 * (1 + u123) + 
              2 * e2123 * (2 + u123))) / ((-1 + et) * a * b * theta)
  return(res.f)
}

# 3d density
.fran.123.density <- function(x, u) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
    u3 <- u[3]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
    u3 <- u[, 3]
  }
  
  au1 <- -1 + exp(-x * u1)
  au2 <- -1 + exp(-x * u2)
  au3 <- -1 + exp(-x * u3)
  au123 <- au1 * au2 * au3
  au <- -1 + exp(-x)
  a <- (au^2) * (1 + au123 / (au^2))
  e123 <- exp(-x * (u1 + u2 + u3))
  
  res.f <- (2 * e123 * (au123^2) * x^2) / (a^3) - (3 * e123 * au123 * x^2) / 
    (a^2) + (e123 * x^2) / a
  return(res.f)
}

