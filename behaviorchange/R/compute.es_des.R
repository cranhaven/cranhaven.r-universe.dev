### This function was copied from compute.es::des
compute.es_des <- function (d, n.1, n.2, level = 95,
                            cer = 0.2, dig = 2, verbose = TRUE,
                            id = NULL, data = NULL)
{
  if (!is.null(data)) {
    mf <- match.call()
    args <- match(c("d", "n.1", "n.2",
                    "level", "dig", "id", "data"),
                  names(mf), 0)
    mf <- mf[c(1, args)]
    mf$drop.unused.levels <- TRUE
    mf.d <- mf[[match("d", names(mf))]]
    d <- eval(mf.d, data, enclos = sys.frame(sys.parent()))
    mf.n.1 <- mf[[match("n.1", names(mf))]]
    n.1 <- eval(mf.n.1, data, enclos = sys.frame(sys.parent()))
    mf.n.2 <- mf[[match("n.2", names(mf))]]
    n.2 <- eval(mf.n.2, data, enclos = sys.frame(sys.parent()))
    mf.id <- mf[[match("id", names(mf))]]
    id <- eval(mf.id, data, enclos = sys.frame(sys.parent()))
  }
  var.d <- (n.1 + n.2)/(n.1 * n.2) + (d^2)/(2 * (n.1 + n.2))
  df <- (n.1 + n.2) - 2
  j <- 1 - (3/(4 * df - 1))
  g <- j * d
  var.g <- j^2 * var.d
  a <- ((n.1 + n.2)^2)/(n.1 * n.2)
  n <- n.1 + n.2
  r <- d/sqrt((d^2) + a)
  var.r <- (a^2 * var.d)/(d^2 + a)^3
  lor <- d * (pi/sqrt(3))
  var.lor <- var.d * (pi^2/3)
  z <- 0.5 * log((1 + r)/(1 - r))
  var.z <- 1/(n - 3)
  alpha <- (100 - level)/100
  crit <- stats::qnorm(alpha/2, lower.tail = FALSE)
  zval.d <- d/sqrt(var.d)
  pval.d <- 2 * stats::pt(abs(zval.d), df, lower.tail = FALSE)
  lower.d <- d - crit * sqrt(var.d)
  upper.d <- d + crit * sqrt(var.d)
  U3.d <- stats::pnorm(d) * 100
  cl.d <- (stats::pnorm((d)/sqrt(2))) * 100
  cliffs.d <- 2 * stats::pnorm(d/sqrt(2)) - 1
  zval.g <- g/sqrt(var.g)
  pval.g <- 2 * stats::pt(abs(zval.g), df, lower.tail = FALSE)
  lower.g <- g - crit * sqrt(var.g)
  upper.g <- g + crit * sqrt(var.g)
  U3.g <- stats::pnorm(g) * 100
  cl.g <- (stats::pnorm((g)/sqrt(2))) * 100
  zval.z <- z/sqrt(var.z)
  pval.z <- 2 * stats::pt(abs(zval.z), df, lower.tail = FALSE)
  lower.z <- z - crit * sqrt(var.z)
  upper.z <- z + crit * sqrt(var.z)
  pval.r <- pval.z
  lower.r <- (exp(2 * lower.z) - 1)/(1 + exp(2 * lower.z))
  upper.r <- (exp(2 * upper.z) - 1)/(1 + exp(2 * upper.z))
  zval.lor <- lor/sqrt(var.lor)
  pval.lor <- 2 * stats::pt(abs(zval.lor), df, lower.tail = FALSE)
  lower.lor <- lor - crit * sqrt(var.lor)
  upper.lor <- lor + crit * sqrt(var.lor)
  nnt <- 1/(stats::pnorm(d - stats::qnorm(1 - cer)) - cer)
  if (!is.null(data)) {
    out <- round(data.frame(id = id, N.total = n, n.1 = n.1,
                            n.2 = n.2, d = d, var.d = var.d, l.d = lower.d, u.d = upper.d,
                            U3.d = U3.d, cl.d = cl.d, cliffs.d = cliffs.d, pval.d = pval.d,
                            g = g, var.g = var.g, l.g = lower.g, u.g = upper.g,
                            U3.g = U3.g, cl.g = cl.g, pval.g = pval.g, r = r,
                            var.r = var.r, l.r = lower.r, u.r = upper.r, pval.r = pval.r,
                            fisher.z = z, var.z = var.z, l.z = lower.z, u.z = upper.z,
                            OR = exp(lor), l.or = exp(lower.lor), u.or = exp(upper.lor),
                            pval.or = pval.lor, lOR = lor, l.lor = lower.lor,
                            u.lor = upper.lor, pval.lor = pval.lor, NNT = nnt),
                 dig)
    return(out)
  }
  else {
    if (verbose) {
      cat("Mean Differences ES:", "\n", "\n",
          "d [", level, "%CI] =", round(d,
                                        dig), "[", round(lower.d, dig), ",",
          round(upper.d, dig), "]", "\n", " var(d) =",
          round(var.d, dig), "\n", " p-value(d) =",
          round(pval.d, dig), "\n", " U3(d) =",
          round(U3.d, dig), "%", "\n", " CLES(d) =",
          round(cl.d, dig), "%", "\n", " Cliff's Delta =",
          round(cliffs.d, dig), "\n", "\n",
          "g [", level, "%CI] =", round(g,
                                        dig), "[", round(lower.g, dig), ",",
          round(upper.g, dig), "]", "\n", " var(g) =",
          round(var.g, dig), "\n", " p-value(g) =",
          round(pval.g, dig), "\n", " U3(g) =",
          round(U3.g, dig), "%", "\n", " CLES(g) =",
          round(cl.g, dig), "%", "\n", "\n",
          "Correlation ES:", "\n", "\n",
          "r [", level, "%CI] =", round(r,
                                        dig), "[", round(lower.r, dig), ",",
          round(upper.r, dig), "]", "\n", " var(r) =",
          round(var.r, dig), "\n", " p-value(r) =",
          round(pval.r, dig), "\n", "\n", "z [",
          level, "%CI] =", round(z, dig), "[",
          round(lower.z, dig), ",", round(upper.z,
                                          dig), "]", "\n", " var(z) =",
          round(var.z, dig), "\n", " p-value(z) =",
          round(pval.z, dig), "\n", "\n", "Odds Ratio ES:",
          "\n", "\n", "OR [", level,
          "%CI] =", round(exp(lor), dig), "[",
          round(exp(lower.lor), dig), ",", round(exp(upper.lor),
                                                 dig), "]", "\n", " p-value(OR) =",
          round(pval.lor, dig), "\n", "\n",
          "Log OR [", level, "%CI] =", round(lor,
                                             dig), "[", round(lower.lor, dig), ",",
          round(upper.lor, dig), "]", "\n",
          " var(lOR) =", round(var.lor, dig), "\n",
          " p-value(Log OR) =", round(pval.lor, dig),
          "\n", "\n", "Other:", "\n",
          "\n", "NNT =", round(nnt, dig), "\n",
          "Total N =", n)
    }
    out <- round(data.frame(N.total = n, n.1 = n.1, n.2 = n.2,
                            d = d, var.d = var.d, l.d = lower.d, u.d = upper.d,
                            U3.d = U3.d, cl.d = cl.d, cliffs.d = cliffs.d, pval.d = pval.d,
                            g = g, var.g = var.g, l.g = lower.g, u.g = upper.g,
                            U3.g = U3.g, cl.g = cl.g, pval.g = pval.g, r = r,
                            var.r = var.r, l.r = lower.r, u.r = upper.r, pval.r = pval.r,
                            fisher.z = z, var.z = var.z, l.z = lower.z, u.z = upper.z,
                            OR = exp(lor), l.or = exp(lower.lor), u.or = exp(upper.lor),
                            pval.or = pval.lor, lOR = lor, l.lor = lower.lor,
                            u.lor = upper.lor, pval.lor = pval.lor, NNT = nnt),
                 dig)
    invisible(out)
  }
}
