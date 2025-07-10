# Internal functions for the derivation of the gumbel copula for the PIOSRn and 
# PIOSTn test statistics. Called by .Rn and .Tn, the internal functions for
# their test statistics.

# 2d first derivative wrt theta of the log-density
.V.gumb.12 <- function(theta, u1, u2) {
  a <- .Power(.Power(-log(u1), theta) + .Power(-log(u2), theta), 1 / theta)
  a2 <- a^2
  res.f <- (-(theta * (.Power(-log(u1), theta) * (1 - 3 * a + a2) + 
            .Power(theta, 2) * (.Power(-log(u1), theta) - 
            .Power(-log(u2), theta)) + theta * (-1 + a) * (2 * 
            .Power(-log(u1), theta) - .Power(-log(u2), theta))) * 
            log(-log(u1))) + (1 + theta * (-1 + a) - 3 * a + a2) * 
            (.Power(-log(u1), theta) + .Power(-log(u2), theta)) * 
            log(.Power(-log(u1), theta) + .Power(-log(u2), theta)) + 
            theta * (theta * (.Power(-log(u1), theta) + 
            .Power(-log(u2), theta)) - (-(theta * (-1 + a) * 
            (.Power(-log(u1), theta) - 2 * .Power(-log(u2), theta))) + 
            .Power(theta, 2) * (-.Power(-log(u1), theta) + 
            .Power(-log(u2), theta)) + (1 - 3 * a + a2) * 
            .Power(-log(u2), theta)) * log(-log(u2)))) / 
            (.Power(theta, 2) * (-1 + theta + a) * (.Power(-log(u1), theta) + 
            .Power(-log(u2), theta)))
  return(res.f)
}

# 2d second derivative wrt theta of the log-density
.S.gumb.12 <- function(theta, u1, u2) {
  a <- .Power(.Power(-log(u1), theta) + .Power(-log(u2), theta), 1 / theta)
  a2 <- a^2
  a3 <- a^3
  b <- .Power(log(u1) * log(u2), theta)
  d <- log(.Power(-log(u1), theta) + .Power(-log(u2), theta))
  res.f <- -((.Power(theta, 2) * (.Power(-log(u1), 2 * theta) * 
              (2 - 2 * a + a2) * a + 2 * .Power(theta, 4) * b + 5 * 
              .Power(theta, 3) * (-1 + a) * b + .Power(theta, 2) * 
              (.Power(-log(u1), 2 * theta) * a + 4 * b - 9 * a * b + 4 * 
              a2 * b) + theta * (-3 * .Power(-log(u1), 2 * theta) * a + 2 * 
              .Power(-log(u1), 2 * theta) * a2 - b + 4 * a * b - 4 * a2 * 
              b + a3 * b)) * .Power(log(-log(u1)), 2) + (2 + 
              .Power(theta, 2) + theta * (-3 + 2 * a) - 2 * a + a2) * a * 
              (.Power(-log(u1), 2 * theta) + .Power(-log(u2), 2 * theta) + 
              2 * b) * .Power(d, 2) + 2 * theta * d * ((-1 + 
              .Power(theta, 2) * (-1 + a) + 2 * theta * (1 - 3 * a + a2) + 
              4 * a - 4 * a2 + a3) * (.Power(-log(u1), 2 * theta) + 
              .Power(-log(u2), 2 * theta) + 2 * b) - (2 + .Power(theta, 2) + 
              theta * (-3 + 2 * a) - 2 * a + a2) * a * 
              (.Power(-log(u2), 2 * theta) + b) * log(-log(u2))) + 
              .Power(theta, 2) * (.Power(theta, 2) * (.Power(-log(u1), 2 * 
              theta) + .Power(-log(u2), 2 * theta) + 2 * b) - 2 * 
              (-1 + .Power(theta, 2) * (-1 + a) + 2 * theta * (1 - 3 * a + 
              a2) + 4 * a - 4 * a2 + a3) * (.Power(-log(u2), 2 * theta) + b) * 
              log(-log(u2)) + ((2 - 2 * a + a2) * a * .Power(-log(u2), 2 * 
              theta) + 2 * .Power(theta, 4) * b + 5 * .Power(theta, 3) * 
              (-1 + a) * b + .Power(theta, 2) * (a * .Power(-log(u2), 2 * 
              theta) + 4 * b - 9 * a * b + 4 * a2 * b) + theta * (-3 * a * 
              .Power(-log(u2), 2 * theta) + 2 * a2 * .Power(-log(u2), 
              2 * theta) - b + 4 * a * b - 4 * a2 * b + a3 * b)) * 
              .Power(log(-log(u2)), 2)) - 2 * theta * log(-log(u1)) * 
              ((2 + .Power(theta, 2) + theta * (-3 + 2 * a) - 2 * a + a2) * 
              a * (.Power(-log(u1), 2 * theta) + b) * d + theta * 
              ((-1 + .Power(theta, 2) * (-1 + a) + 2 * theta * 
              (1 - 3 * a + a2) + 4 * a - 4 * a2 + a3) * (.Power(-log(u1), 2 * 
              theta) + b) + (-1 + theta) * (2 * .Power(theta, 3) + 
              .Power(theta, 2) * (-3 + 5 * a) + theta * (1 - 5 * a + 4 * a2) + 
              (2 - 2 * a + a2) * a) * b * log(-log(u2))))) / 
              (.Power(theta, 4) * .Power(-1 + theta + a, 2) * 
              .Power(.Power(-log(u1), theta) + .Power(-log(u2), theta), 2)))
  return(res.f)
}

# 2d density
.gumb.12.density <- function(x, u) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
  }
  lu1 <- -log(u1)
  lu2 <- -log(u2)
  a <- lu1^x + lu2^x
  xm1 <- x - 1
  res.f <- (lu1^xm1 * (xm1 + a^(1 / x)) * a^(-2 + 1 / x) * lu2^xm1) / 
    (exp(a^(1 / x)) * u1 * u2)
  return(res.f)
}

# 3d density
.gumb.123.density <- function(x, u) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
    u3 <- u[3]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
    u3 <- u[, 3]
  }
  p123 <- (-log(u1))^x + (-log(u2))^x + (-log(u3))^x
  p123.x <- p123^(1 / x)
  res.f <- ((1 + 2 * (x^2) + 3 * x * (-1 + p123.x) - 3 * p123.x + p123.x^2) * 
              p123^(-3) * ((-(log(u1) * log(u2) * log(u3)))^(-1 + x))) / 
    (exp(p123.x) * u1 * u2 * u3)
  return(res.f)
}

# 3d second derivative wrt theta of the log-density
.gumb.S.123 <- function(theta, u) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
    u3 <- u[3]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
    u3 <- u[, 3]
  }
  
  l1 <- log(u1)
  l2 <- log(u2)
  l3 <- log(u3)
  ll1 <- log(-l1)
  ll2 <- log(-l2)
  ll3 <- log(-l3)
  
  ti <- 1 / theta
  
  pu1t <- (-l1)^theta
  pu2t <- (-l2)^theta
  pu3t <- (-l3)^theta
  
  pl13t <- (l1 * l3)^theta
  pl12t <- (l1 * l2)^theta
  pl23t <- (l2 * l3)^theta
  
  pu123t <- pu1t + pu2t + pu3t
  lt <- log(pu123t)
  b <- pu123t^ti
  b2 <- b^2
  b3 <- b^3
  b4 <- b^4
  ll32 <- ll3^2
  a <- b * ll32
  lpu123t <- lt^2
  
  
  t2 <- theta^2
  t3 <- theta^3
  t4 <- theta^4
  t5 <- theta^5
  t6 <- theta^6
  
  res.f <- (-.Power(-3 * t2 * pu1t + 4 * t3 * pu1t - 3 * t2 * pu2t + 4 * t3 * 
                      pu2t + 3 * t2 * pu1t * b + 3 * t2 * pu2t * b - 3 * t2 * 
                      pu3t + 4 * t3 * pu3t + 3 * t2 * b * pu3t - theta * 
                      (pu1t * (-1 + 7 * b - 6 * b2 + b3) + t2 * (-1 + b) * 
                      (8 * pu1t - 3 * pu2t - 3 * pu3t) + t3 * (4 * pu1t - 2 * 
                      pu2t - 2 * pu3t) + theta * (1 - 3 * b + b2) * (5 * 
                      pu1t - pu2t - pu3t)) * ll1 - theta * (pu2t * (-1 + 7 * 
                      b - 6 * b2 + b3) - theta * (1 - 3 * b + b2) * (pu1t - 
                      5 * pu2t + pu3t) - 2 * t3 * (pu1t - 2 * pu2t + pu3t) - 
                      t2 * (-1 + b) * (3 * pu1t - 8 * pu2t + 3 * pu3t)) * ll2 - 
                      pu1t * lt + 3 * theta * pu1t * lt - 2 * t2 * pu1t * lt - 
                      pu2t * lt + 3 * theta * pu2t * lt - 2 * t2 * pu2t * lt + 
                      7 * pu1t * b * lt - 9 * theta * pu1t * b * lt + 2 * t2 * 
                      pu1t * b * lt + 7 * pu2t * b * lt - 9 * theta * pu2t * 
                      b * lt + 2 * t2 * pu2t * b * lt - 6 * pu1t * b2 * lt + 
                      3 * theta * pu1t * b2 * lt - 6 * pu2t * b2 * lt + 3 * 
                      theta * pu2t * b2 * lt + pu1t * b3 * lt + pu2t * b3 * 
                      lt - pu3t * lt + 3 * theta * pu3t * lt - 2 * t2 * pu3t * 
                      lt + 7 * b * pu3t * lt - 9 * theta * b * pu3t * lt + 2 * 
                      t2 * b * pu3t * lt - 6 * b2 * pu3t * lt + 3 * theta * 
                      b2 * pu3t * lt + b3 * pu3t * lt + t2 * pu1t * ll3 - 3 * 
                      t3 * pu1t * ll3 + 2 * t4 * pu1t * ll3 + t2 * pu2t * 
                      ll3 - 3 * t3 * pu2t * ll3 + 2 * t4 * pu2t * ll3 - 3 * 
                      t2 * pu1t * b * ll3 + 3 * t3 * pu1t * b * ll3 - 3 * 
                      t2 * pu2t * b * ll3 + 3 * t3 * pu2t * b * ll3 + t2 * 
                      pu1t * b2 * ll3 + t2 * pu2t * b2 * ll3 + theta * pu3t * 
                      ll3 - 5 * t2 * pu3t * ll3 + 8 * t3 * pu3t * ll3 - 4 * 
                      t4 * pu3t * ll3 - 7 * theta * b * pu3t * ll3 + 15 * 
                      t2 * b * pu3t * ll3 - 8 * t3 * b * pu3t * ll3 + 6 * 
                      theta * b2 * pu3t * ll3 - 5 * t2 * b2 * pu3t * ll3 - 
                      theta * b3 * pu3t * ll3, 2) + (1 + 2 * t2 + 3 * theta * 
                      (-1 + b) - 3 * b + b2) * (4 * t4 * (pu1t^2) + 4 * t4 * 
                      (pu2t^2) + 8 * t4 * pl12t + 4 * t4 * (pu3t^2) + 8 * 
                      t4 * pl13t + 8 * t4 * pl23t + t2 * ((pu1t^2) * (1 - 15 * 
                      b + 25 * b2 - 10 * b3 + b4) + theta * (-1 + 7 * b - 6 * 
                      b2 + b3) * (7 * (pu1t^2) - 3 * pl12t - 3 * pl13t) + 
                      t2 * (1 - 3 * b + b2) * (18 * (pu1t^2) + (pu2t^2) - 
                      16 * pl12t + (pu3t^2) - 16 * pl13t + 2 * pl23t) + 2 * 
                      t4 * (4 * (pu1t^2) + (pu2t^2) - 7 * pl12t + (pu3t^2) - 
                      7 * pl13t + 2 * pl23t) + t3 * (-1 + b) * (20 * 
                      (pu1t^2) + 3 * (pu2t^2) - 27 * pl12t + 3 * (pu3t^2) - 
                      27 * pl13t + 6 * pl23t)) * (ll1^2) + t2 * ((pu2t^2) * 
                      (1 - 15 * b + 25 * b2 - 10 * b3 + b4) + t3 * (-1 + b) * 
                      (3 * (pu1t^2) + 20 * (pu2t^2) - 27 * pl12t + 3 * 
                      (pu3t^2) + 6 * pl13t - 27 * pl23t) + t2 * (1 - 3 * b + 
                      b2) * ((pu1t^2) + 18 * (pu2t^2) - 16 * pl12t + 
                      (pu3t^2) + 2 * pl13t - 16 * pl23t) + 2 * t4 * 
                      ((pu1t^2) + 4 * (pu2t^2) - 7 * pl12t + (pu3t^2) + 2 * 
                      pl13t - 7 * pl23t) + theta * (-1 + 7 * b - 6 * b2 + b3) * 
                      (7 * (pu2t^2) - 3 * pl12t - 3 * pl23t)) * (ll2^2) + 2 * 
                      theta * (pu1t^2) * lt - 4 * t3 * (pu1t^2) * lt + 2 * 
                      theta * (pu2t^2) * lt - 4 * t3 * (pu2t^2) * lt + 4 * 
                      theta * pl12t * lt - 8 * t3 * pl12t * lt - 14 * theta * 
                      (pu1t^2) * b * lt + 4 * t3 * (pu1t^2) * b * lt - 14 * 
                      theta * (pu2t^2) * b * lt + 4 * t3 * (pu2t^2) * b * 
                      lt - 28 * theta * pl12t * b * lt + 8 * t3 * pl12t * b * 
                      lt + 12 * theta * (pu1t^2) * b2 * lt + 12 * theta * 
                      (pu2t^2) * b2 * lt + 24 * theta * pl12t * b2 * lt - 2 * 
                      theta * (pu1t^2) * b3 * lt - 2 * theta * (pu2t^2) * b3 * 
                      lt - 4 * theta * pl12t * b3 * lt + 2 * theta * (pu3t^2) * 
                      lt - 4 * t3 * (pu3t^2) * lt - 14 * theta * b * (pu3t^2) * 
                      lt + 4 * t3 * b * (pu3t^2) * lt + 12 * theta * b2 * 
                      (pu3t^2) * lt - 2 * theta * b3 * (pu3t^2) * lt + 4 * 
                      theta * pl13t * lt - 8 * t3 * pl13t * lt - 28 * theta * 
                      b * pl13t * lt + 8 * t3 * b * pl13t * lt + 24 * theta * 
                      b2 * pl13t * lt - 4 * theta * b3 * pl13t * lt + 4 * 
                      theta * pl23t * lt - 8 * t3 * pl23t * lt - 28 * theta * 
                      b * pl23t * lt + 8 * t3 * b * pl23t * lt + 24 * theta * 
                      b2 * pl23t * lt - 4 * theta * b3 * pl23t * lt + 
                      (pu1t^2) * lpu123t - 3 * theta * (pu1t^2) * lpu123t + 
                      2 * t2 * (pu1t^2) * lpu123t + (pu2t^2) * lpu123t - 3 * 
                      theta * (pu2t^2) * lpu123t + 2 * t2 * (pu2t^2) * 
                      lpu123t + 2 * pl12t * lpu123t - 6 * theta * pl12t * 
                      lpu123t + 4 * t2 * pl12t * lpu123t - 15 * (pu1t^2) * 
                      b * lpu123t + 21 * theta * (pu1t^2) * b * lpu123t - 
                      6 * t2 * (pu1t^2) * b * lpu123t - 15 * (pu2t^2) * b * 
                      lpu123t + 21 * theta * (pu2t^2) * b * lpu123t - 6 * t2 * 
                      (pu2t^2) * b * lpu123t - 30 * pl12t * b * lpu123t + 42 * 
                      theta * pl12t * b * lpu123t - 12 * t2 * pl12t * b * 
                      lpu123t + 25 * (pu1t^2) * b2 * lpu123t - 18 * theta * 
                      (pu1t^2) * b2 * lpu123t + 2 * t2 * (pu1t^2) * b2 * 
                      lpu123t + 25 * (pu2t^2) * b2 * lpu123t - 18 * theta * 
                      (pu2t^2) * b2 * lpu123t + 2 * t2 * (pu2t^2) * b2 * 
                      lpu123t + 50 * pl12t * b2 * lpu123t - 36 * theta * 
                      pl12t * b2 * lpu123t + 4 * t2 * pl12t * b2 * lpu123t - 
                      10 * (pu1t^2) * b3 * lpu123t + 3 * theta * (pu1t^2) * 
                      b3 * lpu123t - 10 * (pu2t^2) * b3 * lpu123t + 3 * theta * 
                      (pu2t^2) * b3 * lpu123t - 20 * pl12t * b3 * lpu123t + 6 * 
                      theta * pl12t * b3 * lpu123t + (pu1t^2) * b4 * lpu123t + 
                      (pu2t^2) * b4 * lpu123t + 2 * pl12t * b4 * lpu123t + 
                      (pu3t^2) * lpu123t - 3 * theta * (pu3t^2) * lpu123t + 2 * 
                      t2 * (pu3t^2) * lpu123t - 15 * b * (pu3t^2) * lpu123t + 
                      21 * theta * b * (pu3t^2) * lpu123t - 6 * t2 * b * 
                      (pu3t^2) * lpu123t + 25 * b2 * (pu3t^2) * lpu123t - 18 * 
                      theta * b2 * (pu3t^2) * lpu123t + 2 * t2 * b2 * 
                      (pu3t^2) * lpu123t - 10 * b3 * (pu3t^2) * lpu123t + 3 * 
                      theta * b3 * (pu3t^2) * lpu123t + b4 * (pu3t^2) * 
                      lpu123t + 2 * pl13t * lpu123t - 6 * theta * pl13t * 
                      lpu123t + 4 * t2 * pl13t * lpu123t - 30 * b * pl13t * 
                      lpu123t + 42 * theta * b * pl13t * lpu123t - 12 * t2 * 
                      b * pl13t * lpu123t + 50 * b2 * pl13t * lpu123t - 36 * 
                      theta * b2 * pl13t * lpu123t + 4 * t2 * b2 * pl13t * 
                      lpu123t - 20 * b3 * pl13t * lpu123t + 6 * theta * b3 * 
                      pl13t * lpu123t + 2 * b4 * pl13t * lpu123t + 2 * pl23t * 
                      lpu123t - 6 * theta * pl23t * lpu123t + 4 * t2 * pl23t * 
                      lpu123t - 30 * b * pl23t * lpu123t + 42 * theta * b * 
                      pl23t * lpu123t - 12 * t2 * b * pl23t * lpu123t + 50 * 
                      b2 * pl23t * lpu123t - 36 * theta * b2 * pl23t * 
                      lpu123t + 4 * t2 * b2 * pl23t * lpu123t - 20 * b3 * 
                      pl23t * lpu123t + 6 * theta * b3 * pl23t * lpu123t + 2 * 
                      b4 * pl23t * lpu123t - 6 * t4 * (pu1t^2) * ll3 + 8 * t5 * 
                      (pu1t^2) * ll3 - 6 * t4 * (pu2t^2) * ll3 + 8 * t5 * 
                      (pu2t^2) * ll3 - 12 * t4 * pl12t * ll3 + 16 * t5 * 
                      pl12t * ll3 + 6 * t4 * (pu1t^2) * b * ll3 + 6 * t4 * 
                      (pu2t^2) * b * ll3 + 12 * t4 * pl12t * b * ll3 - 2 * t2 * 
                      (pu3t^2) * ll3 + 16 * t4 * (pu3t^2) * ll3 - 16 * t5 * 
                      (pu3t^2) * ll3 + 14 * t2 * b * (pu3t^2) * ll3 - 16 * t4 * 
                      b * (pu3t^2) * ll3 - 12 * t2 * b2 * (pu3t^2) * ll3 + 2 * 
                      t2 * b3 * (pu3t^2) * ll3 - 2 * t2 * pl13t * ll3 + 10 * 
                      t4 * pl13t * ll3 - 8 * t5 * pl13t * ll3 + 14 * t2 * b * 
                      pl13t * ll3 - 10 * t4 * b * pl13t * ll3 - 12 * t2 * b2 * 
                      pl13t * ll3 + 2 * t2 * b3 * pl13t * ll3 - 2 * t2 * 
                      pl23t * ll3 + 10 * t4 * pl23t * ll3 - 8 * t5 * pl23t * 
                      ll3 + 14 * t2 * b * pl23t * ll3 - 10 * t4 * b * pl23t * 
                      ll3 - 12 * t2 * b2 * pl23t * ll3 + 2 * t2 * b3 * pl23t * 
                      ll3 - 2 * t2 * (pu1t^2) * lt * ll3 + 6 * t3 * (pu1t^2) * 
                      lt * ll3 - 4 * t4 * (pu1t^2) * lt * ll3 - 2 * t2 * 
                      (pu2t^2) * lt * ll3 + 6 * t3 * (pu2t^2) * lt * ll3 - 4 * 
                      t4 * (pu2t^2) * lt * ll3 - 4 * t2 * pl12t * lt * ll3 + 
                      12 * t3 * pl12t * lt * ll3 - 8 * t4 * pl12t * lt * ll3 + 
                      14 * t2 * (pu1t^2) * b * lt * ll3 - 18 * t3 * (pu1t^2) * 
                      b * lt * ll3 + 4 * t4 * (pu1t^2) * b * lt * ll3 + 14 * 
                      t2 * (pu2t^2) * b * lt * ll3 - 18 * t3 * (pu2t^2) * b * 
                      lt * ll3 + 4 * t4 * (pu2t^2) * b * lt * ll3 + 28 * t2 * 
                      pl12t * b * lt * ll3 - 36 * t3 * pl12t * b * lt * ll3 + 
                      8 * t4 * pl12t * b * lt * ll3 - 12 * t2 * (pu1t^2) * b2 * 
                      lt * ll3 + 6 * t3 * (pu1t^2) * b2 * lt * ll3 - 12 * t2 * 
                      (pu2t^2) * b2 * lt * ll3 + 6 * t3 * (pu2t^2) * b2 * lt * 
                      ll3 - 24 * t2 * pl12t * b2 * lt * ll3 + 12 * t3 * pl12t * 
                      b2 * lt * ll3 + 2 * t2 * (pu1t^2) * b3 * lt * ll3 + 2 * 
                      t2 * (pu2t^2) * b3 * lt * ll3 + 4 * t2 * pl12t * b3 * 
                      lt * ll3 - 2 * theta * (pu3t^2) * lt * ll3 + 10 * t2 * 
                      (pu3t^2) * lt * ll3 - 16 * t3 * (pu3t^2) * lt * ll3 + 
                      8 * t4 * (pu3t^2) * lt * ll3 + 30 * theta * b * 
                      (pu3t^2) * lt * ll3 - 70 * t2 * b * (pu3t^2) * lt * 
                      ll3 + 48 * t3 * b * (pu3t^2) * lt * ll3 - 8 * t4 * b * 
                      (pu3t^2) * lt * ll3 - 50 * theta * b2 * (pu3t^2) * lt * 
                      ll3 + 60 * t2 * b2 * (pu3t^2) * lt * ll3 - 16 * t3 * b2 * 
                      (pu3t^2) * lt * ll3 + 20 * theta * b3 * (pu3t^2) * lt * 
                      ll3 - 10 * t2 * b3 * (pu3t^2) * lt * ll3 - 2 * theta * 
                      b4 * (pu3t^2) * lt * ll3 - 2 * theta * pl13t * lt * ll3 + 
                      8 * t2 * pl13t * lt * ll3 - 10 * t3 * pl13t * lt * ll3 + 
                      4 * t4 * pl13t * lt * ll3 + 30 * theta * b * pl13t * lt * 
                      ll3 - 56 * t2 * b * pl13t * lt * ll3 + 30 * t3 * b * 
                      pl13t * lt * ll3 - 4 * t4 * b * pl13t * lt * ll3 - 50 * 
                      theta * b2 * pl13t * lt * ll3 + 48 * t2 * b2 * pl13t * 
                      lt * ll3 - 10 * t3 * b2 * pl13t * lt * ll3 + 20 * theta * 
                      b3 * pl13t * lt * ll3 - 8 * t2 * b3 * pl13t * lt * ll3 - 
                      2 * theta * b4 * pl13t * lt * ll3 - 2 * theta * pl23t * 
                      lt * ll3 + 8 * t2 * pl23t * lt * ll3 - 10 * t3 * pl23t * 
                      lt * ll3 + 4 * t4 * pl23t * lt * ll3 + 30 * theta * b * 
                      pl23t * lt * ll3 - 56 * t2 * b * pl23t * lt * ll3 + 30 * 
                      t3 * b * pl23t * lt * ll3 - 4 * t4 * b * pl23t * lt * 
                      ll3 - 50 * theta * b2 * pl23t * lt * ll3 + 48 * t2 * 
                      b2 * pl23t * lt * ll3 - 10 * t3 * b2 * pl23t * lt * 
                      ll3 + 20 * theta * b3 * pl23t * lt * ll3 - 8 * t2 * 
                      b3 * pl23t * lt * ll3 - 2 * theta * b4 * pl23t * lt * 
                      ll3 + t4 * (pu1t^2) * ll32 - 3 * t5 * (pu1t^2) * ll32 + 
                      2 * t6 * (pu1t^2) * ll32 + t4 * (pu2t^2) * ll32 - 3 * 
                      t5 * (pu2t^2) * ll32 + 2 * t6 * (pu2t^2) * ll32 + 2 * 
                      t4 * pl12t * ll32 - 6 * t5 * pl12t * ll32 + 4 * t6 * 
                      pl12t * ll32 - 3 * t4 * (pu1t^2) * a + 3 * t5 * 
                      (pu1t^2) * a - 3 * t4 * (pu2t^2) * a + 3 * t5 * 
                      (pu2t^2) * a - 6 * t4 * pl12t * a + 6 * t5 * pl12t * a + 
                      t4 * (pu1t^2) * b2 * ll32 + t4 * (pu2t^2) * b2 * ll32 + 
                      2 * t4 * pl12t * b2 * ll32 + t2 * (pu3t^2) * ll32 - 7 * 
                      t3 * (pu3t^2) * ll32 + 18 * t4 * (pu3t^2) * ll32 - 20 * 
                      t5 * (pu3t^2) * ll32 + 8 * t6 * (pu3t^2) * ll32 - 15 * 
                      t2 * b * (pu3t^2) * ll32 + 49 * t3 * b * (pu3t^2) * 
                      ll32 - 54 * t4 * b * (pu3t^2) * ll32 + 20 * t5 * b * 
                      (pu3t^2) * ll32 + 25 * t2 * b2 * (pu3t^2) * ll32 - 42 * 
                      t3 * b2 * (pu3t^2) * ll32 + 18 * t4 * b2 * (pu3t^2) * 
                      ll32 - 10 * t2 * b3 * (pu3t^2) * ll32 + 7 * t3 * b3 * 
                      (pu3t^2) * ll32 + t2 * b4 * (pu3t^2) * ll32 + 3 * t3 * 
                      pl13t * ll32 - 16 * t4 * pl13t * ll32 + 27 * t5 * pl13t * 
                      ll32 - 14 * t6 * pl13t * ll32 - 21 * t3 * b * pl13t * 
                      ll32 + 48 * t4 * b * pl13t * ll32 - 27 * t5 * b * pl13t * 
                      ll32 + 18 * t3 * b2 * pl13t * ll32 - 16 * t4 * b2 * 
                      pl13t * ll32 - 3 * t3 * b3 * pl13t * ll32 + 3 * t3 * 
                      pl23t * ll32 - 16 * t4 * pl23t * ll32 + 27 * t5 * pl23t * 
                      ll32 - 14 * t6 * pl23t * ll32 - 21 * t3 * b * pl23t * 
                      ll32 + 48 * t4 * b * pl23t * ll32 - 27 * t5 * b * pl23t * 
                      ll32 + 18 * t3 * b2 * pl23t * ll32 - 16 * t4 * b2 * 
                      pl23t * ll32 - 3 * t3 * b3 * pl23t * ll32 - 2 * theta * 
                      ll1 * (-(theta * (pl12t * (1 - 15 * b + 25 * b2 - 10 * 
                      b3 + b4) - theta * (-1 + 7 * b - 6 * b2 + b3) * 
                      ((pu1t^2) + (pu2t^2) - 8 * pl12t + pl13t + pl23t) - 2 * 
                      t4 * (2 * (pu1t^2) + 2 * (pu2t^2) - 8 * pl12t - 
                      (pu3t^2) + pl13t + pl23t) - t2 * (1 - 3 * b + b2) * (5 * 
                      (pu1t^2) + 5 * (pu2t^2) - 25 * pl12t - (pu3t^2) + 4 * 
                      pl13t + 4 * pl23t) - t3 * (-1 + b) * (8 * (pu1t^2) + 8 * 
                      (pu2t^2) - 34 * pl12t - 3 * (pu3t^2) + 5 * pl13t + 5 * 
                      pl23t)) * ll2) + ((1 - 15 * b + 25 * b2 - 10 * b3 + b4) * 
                      ((pu1t^2) + pl12t + pl13t) + t2 * (1 - 3 * b + b2) * (8 * 
                      (pu1t^2) - 3 * (pu2t^2) + 5 * pl12t - 3 * (pu3t^2) + 5 * 
                      pl13t - 6 * pl23t) + 2 * t3 * (-1 + b) * (2 * (pu1t^2) - 
                      (pu2t^2) + pl12t - (pu3t^2) + pl13t - 2 * pl23t) + 
                      theta * (-1 + 7 * b - 6 * b2 + b3) * (5 * (pu1t^2) - 
                      (pu2t^2) + 4 * pl12t - (pu3t^2) + 4 * pl13t - 2 * 
                      pl23t)) * lt + theta * ((pu1t^2) - 8 * t2 * (pu1t^2) + 
                      8 * t3 * (pu1t^2) + 3 * t2 * (pu2t^2) - 4 * t3 * 
                      (pu2t^2) + pl12t - 5 * t2 * pl12t + 4 * t3 * pl12t - 7 * 
                      (pu1t^2) * b + 8 * t2 * (pu1t^2) * b - 3 * t2 * 
                      (pu2t^2) * b - 7 * pl12t * b + 5 * t2 * pl12t * b + 6 * 
                      (pu1t^2) * b2 + 6 * pl12t * b2 - (pu1t^2) * b3 - pl12t * 
                      b3 + 3 * t2 * (pu3t^2) - 4 * t3 * (pu3t^2) - 3 * t2 * b * 
                      (pu3t^2) + pl13t - 5 * t2 * pl13t + 4 * t3 * pl13t - 7 * 
                      b * pl13t + 5 * t2 * b * pl13t + 6 * b2 * pl13t - b3 * 
                      pl13t + 6 * t2 * pl23t - 8 * t3 * pl23t - 6 * t2 * b * 
                      pl23t - ((1 - 15 * b + 25 * b2 - 10 * b3 + b4) * 
                      pl13t - theta * (-1 + 7 * b - 6 * b2 + b3) * ((pu1t^2) + 
                      pl12t + (pu3t^2) - 8 * pl13t + pl23t) - 2 * t4 * (2 * 
                      (pu1t^2) - (pu2t^2) + pl12t + 2 * (pu3t^2) - 8 * pl13t + 
                      pl23t) - t2 * (1 - 3 * b + b2) * (5 * (pu1t^2) - 
                      (pu2t^2) + 4 * pl12t + 5 * (pu3t^2) - 25 * pl13t + 4 * 
                      pl23t) - t3 * (-1 + b) * (8 * (pu1t^2) - 3 * (pu2t^2) + 
                      5 * pl12t + 8 * (pu3t^2) - 34 * pl13t + 5 * pl23t)) * 
                      ll3)) - 2 * theta * ll2 * ((-(t2 * (1 - 3 * b + b2) * 
                      (3 * (pu1t^2) - 8 * (pu2t^2) - 5 * pl12t + 3 * (pu3t^2) + 
                      6 * pl13t - 5 * pl23t)) - theta * (-1 + 7 * b - 6 * b2 + 
                      b3) * ((pu1t^2) - 5 * (pu2t^2) - 4 * pl12t + (pu3t^2) + 
                      2 * pl13t - 4 * pl23t) - 2 * t3 * (-1 + b) * ((pu1t^2) - 
                      2 * (pu2t^2) - pl12t + (pu3t^2) + 2 * pl13t - pl23t) + 
                      (1 - 15 * b + 25 * b2 - 10 * b3 + b4) * ((pu2t^2) + 
                      pl12t + pl23t)) * lt + theta * (3 * t2 * (pu1t^2) - 4 * 
                      t3 * (pu1t^2) + (pu2t^2) - 8 * t2 * (pu2t^2) + 8 * t3 * 
                      (pu2t^2) + pl12t - 5 * t2 * pl12t + 4 * t3 * pl12t - 3 * 
                      t2 * (pu1t^2) * b - 7 * (pu2t^2) * b + 8 * t2 * 
                      (pu2t^2) * b - 7 * pl12t * b + 5 * t2 * pl12t * b + 
                      6 * (pu2t^2) * b2 + 6 * pl12t * b2 - (pu2t^2) * b3 - 
                      pl12t * b3 + 3 * t2 * (pu3t^2) - 4 * t3 * (pu3t^2) - 3 * 
                      t2 * b * (pu3t^2) + 6 * t2 * pl13t - 8 * t3 * pl13t - 6 * 
                      t2 * b * pl13t + pl23t - 5 * t2 * pl23t + 4 * t3 * 
                      pl23t - 7 * b * pl23t + 5 * t2 * b * pl23t + 6 * b2 * 
                      pl23t - b3 * pl23t - ((1 - 15 * b + 25 * b2 - 10 * b3 + 
                      b4) * pl23t - theta * (-1 + 7 * b - 6 * b2 + b3) * 
                      ((pu2t^2) + pl12t + (pu3t^2) + pl13t - 8 * pl23t) + 2 * 
                      t4 * ((pu1t^2) - 2 * (pu2t^2) - pl12t - 2 * (pu3t^2) - 
                      pl13t + 8 * pl23t) + t2 * (1 - 3 * b + b2) * ((pu1t^2) - 
                      5 * (pu2t^2) - 4 * pl12t - 5 * (pu3t^2) - 4 * pl13t + 
                      25 * pl23t) + t3 * (-1 + b) * (3 * (pu1t^2) - 8 * 
                      (pu2t^2) - 5 * pl12t - 8 * (pu3t^2) - 5 * pl13t + 34 * 
                      pl23t)) * ll3)))) / (t4 * ((1 + 2 * t2 + 3 * theta * 
                      (-1 + b) - 3 * b + b2)^(2)) * ((pu123t)^(2)))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              return(res.f)
}

# 3d first derivative wrt theta of the log-density
.gumb.V.123 <- function(theta, u) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
    u3 <- u[3]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
    u3 <- u[, 3]
  }
  
  l1 <- log(u1)
  l2 <- log(u2)
  l3 <- log(u3)
  ll1 <- log(-l1)
  ll2 <- log(-l2)
  ll3 <- log(-l3)
  ti <- 1 / theta
  
  pu1t <- (-l1)^theta
  pu2t <- (-l2)^theta
  pu3t <- (-l3)^theta
  pu123t <- pu1t + pu2t + pu3t
  lt <- log(pu123t)
  b <- .Power(pu123t, ti)
  b2 <- b^2
  b3 <- b^3
  
  t2 <- theta^2
  t3 <- theta^3
  t4 <- theta^4
  t5 <- theta^5
  
  res.f <- (-3 * t2 * pu1t + 4 * t3 * pu1t - 3 * t2 * pu2t + 4 * t3 * pu2t + 
            3 * t2 * pu1t * b + 3 * t2 * pu2t * b - 3 * t2 * pu3t + 4 * t3 * 
            pu3t + 3 * t2 * b * pu3t - theta * (pu1t * (-1 + 7 * b - 6 * b2 + 
            b3) + t2 * (-1 + b) * (8 * pu1t - 3 * pu2t - 3 * pu3t) + t3 * 
            (4 * pu1t - 2 * pu2t - 2 * pu3t) + theta * (1 - 3 * b + b2) * (5 * 
            pu1t - pu2t - pu3t)) * ll1 - theta * (pu2t * (-1 + 7 * b - 6 * 
            b2 + b3) - theta * (1 - 3 * b + b2) * (pu1t - 5 * pu2t + pu3t) - 
            2 * t3 * (pu1t - 2 * pu2t + pu3t) - t2 * (-1 + b) * (3 * pu1t - 
            8 * pu2t + 3 * pu3t)) * ll2 - pu1t * lt + 3 * theta * pu1t * lt - 
            2 * t2 * pu1t * lt - pu2t * lt + 3 * theta * pu2t * lt - 2 * t2 * 
            pu2t * lt + 7 * pu1t * b * lt - 9 * theta * pu1t * b * lt + 2 * 
            t2 * pu1t * b * lt + 7 * pu2t * b * lt - 9 * theta * pu2t * b * 
            lt + 2 * t2 * pu2t * b * lt - 6 * pu1t * b2 * lt + 3 * theta * 
            pu1t * b2 * lt - 6 * pu2t * b2 * lt + 3 * theta * pu2t * b2 * lt + 
            pu1t * b3 * lt + pu2t * b3 * lt - pu3t * lt + 3 * theta * 
            pu3t * lt - 2 * t2 * pu3t * lt + 7 * b * pu3t * lt - 9 * theta * 
            b * pu3t * lt + 2 * t2 * b * pu3t * lt - 6 * b2 * pu3t * lt + 3 * 
            theta * b2 * pu3t * lt + b3 * pu3t * lt + t2 * pu1t * ll3 - 3 * 
            t3 * pu1t * ll3 + 2 * t4 * pu1t * ll3 + t2 * pu2t * ll3 - 3 * t3 * 
            pu2t * ll3 + 2 * t4 * pu2t * ll3 - 3 * t2 * pu1t * b * ll3 + 3 * 
            t3 * pu1t * b * ll3 - 3 * t2 * pu2t * b * ll3 + 3 * t3 * pu2t * b * 
            ll3 + t2 * pu1t * b2 * ll3 + t2 * pu2t * b2 * ll3 + theta * pu3t * 
            ll3 - 5 * t2 * pu3t * ll3 + 8 * t3 * pu3t * ll3 - 4 * t4 * pu3t * 
            ll3 - 7 * theta * b * pu3t * ll3 + 15 * t2 * b * pu3t * ll3 - 8 * 
            t3 * b * pu3t * ll3 + 6 * theta * b2 * pu3t * ll3 - 5 * t2 * b2 * 
            pu3t * ll3 - theta * b3 * pu3t * ll3) / (t2 * (1 + 2 * t2 + 3 * 
            theta * (-1 + b) - 3 * b + b2) * (pu123t))
  return(res.f)
}

