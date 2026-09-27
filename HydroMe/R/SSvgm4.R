SSvgm4 <-
  structure(function (input, Thr, Ths, alp, nscal)
  {
  .expr1 <- Ths - Thr
  .expr3 <- ifelse((exp(alp)) * input <= 0, 1, (exp(alp)) * input)
  .expr4 <- .expr3^nscal
  .expr5 <- 1 + .expr4
  .expr7 <- 1 - (1/nscal)
  .expr8 <- .expr5^.expr7
  .expr11 <- 1/.expr8
  .expr14 <- .expr5^(.expr7 - 1)
  .expr22 <- .expr8^2
  .value <- Thr + (.expr1/.expr8)
  .grad <- array(0, c(length(.value), 4L), list(NULL, c("Thr","Ths", "alp", "nscal")))
  .grad[, "Thr"] <- 1 - .expr11
  .grad[, "Ths"] <- .expr11
  .grad[, "alp"] <- -((.expr1 * (.expr14 * (.expr7 * ((.expr3^(nscal - 1)) * (nscal * .expr3)))))/.expr22)
  .grad[, "nscal"] <- -((.expr1 * ((.expr14 * (.expr7 * (.expr4 * (log(.expr3))))) + (.expr8 * ((log(.expr5)) * (1/(nscal^2))))))/.expr22)
  attr(.value, "gradient") <- .grad
  .value
}, initial = function (mCall, data, LHS, ...)
{
  xy <- data.frame(sortedXyData(mCall[["input"]], LHS, data))
  if (nrow(xy) < 4) {
    stop("Too few distinct input values to fit a van Genuchten model")
  }
  ndistinct <- nrow(xy)
  nlast <- max(3, round(ndistinct/2))
  dfirst <- xy[1, ][["y"]][1]
  dlast <- xy[nrow(xy), ][["y"]][1]
  Thr1 <- ifelse(((xy[1, ][["y"]] - xy[ndistinct, ][["y"]]) < (xy[1, ][["y"]])/2), 0, xy[ndistinct, ][["y"]])
  Ths1 <- dfirst
  dmid <- xy[(ndistinct/2 - 2):(ndistinct/2 + 1), ]
  pars2 <- coef(lm(y ~ log(x), data = dmid))
  ymid <- xy[1:max(3, round(nrow(xy)/2)), ][["y"]][max(3, round(nrow(xy)/2))]
  ax <- (ymid - pars2[1])/pars2[2]
  slopep <- pars2[2]/(dfirst - dlast)
  m1 <- ifelse(abs(slopep) < 1, (1 - exp(-0.8 * (abs(slopep)))),(1 - (0.5755/(abs(slopep))) + (0.1/(abs(slopep))^2) + (0.025/(abs(slopep))^3)))
  nscal1 <- 1/(1 - m1)
  alp1 <- (((2^(1/m1)) - 1)^(1 - m1))/exp(ax)
  pars <-as.numeric(c(Thr = Thr1, Ths = Ths1, alp = alp1, nscal = nscal1))
  setNames(c(pars[1], pars[2], pars[3], pars[4]), mCall[c("Thr", "Ths", "alp", "nscal")])
}, pnames = c("Thr", "Ths", "alp", "nscal"), class = "selfStart")
