
EPS = sqrt(.Machine$double.eps) # "epsilon" for very small numbers

AIC <- function(loglike, gl) {
	return(2*loglike+2*gl)
}

BIC <- function(loglike, n, gl) {
	return(2*loglike+log(n)*gl)
}


llik.weibull <- function(shape, scale, thres, x)
{
	sum(dweibull(x - thres, shape, scale, log=TRUE))

}


thetahat.weibull <- function(x)
{
    if(any(x <= 0)) stop("x values must be positive")

    toptim <- function(theta) -llik.weibull(theta[1], theta[2], theta[3], x)

    mu = mean(log(x))
    sigma2 = var(log(x))
    shape.guess = 1.2 / sqrt(sigma2)
    scale.guess = exp(mu + (0.572 / shape.guess))
    thres.guess = 1

    res = nlminb(c(shape.guess, scale.guess, thres.guess), toptim, lower=EPS)

    c(shape=res$par[1], scale=res$par[2], thres=res$par[3], res)
}

consultaKS <- function (significancia, tamanho) {
	#1%, 2%, 5%, 10%, 20%
	tabelaKS = list("0.01"=1.63, "0.02" = 1.52, "0.05"=1.36,"0.1"=1.22,"0.2"=1.22)

	d = as.numeric(tabelaKS[toString(significancia)])/sqrt(tamanho)

	return(d)
}

consultaQui5 <- function (gl, cauda = 0.95) {
	return(qchisq(cauda, gl))
}


#consultaAD5 <- function (tamanho) {
#
#	if (tamanho > 1000) {
#		stop("Interpolacao valida at? 1000 observacoes")
#	}
#
#	valoresCriticosAD = data.frame(n=c(10, 15, 20, 25, 30, 35, 40, 50, 1000), vc = c(0.712, 0.720, 0.725, 0.728, 0.730, 0.732, 0.734, 0.736, 0.757))
#	vc = approx(x = valoresCriticosAD$n, y=valoresCriticosAD$vc, xout=tamanho)
#	return(vc$y)
#}

convertePValor <- function (pvalor) {

	if (is.numeric(pvalor)) {
		return(pvalor)
	}

	if (all.equal(pvalor,"> 0.10") == TRUE) {
		return (0.11)
	} else if (all.equal(pvalor, "< 0.01") == TRUE) {
		return (0.01)
	}
}

#http://pic.dhe.ibm.com/infocenter/spssstat/v21r0m0/index.jsp?topic=%2Fcom.ibm.spss.statistics.help%2Falg_simplan_goodness_continuous_ks_weibull.htm
consultaKSWeibull <- function (D) {
	tabela = data.frame(p=c(0.10,	0.05,	0.025,	0.01), d = c(1.372,	1.477,	1.557,	1.671))

  if (D < min(tabela$d))  return (0.11)
  if (D > max(tabela$d))  return (0.001)

	vc = approx(x = tabela$d, y=tabela$p, xout=D)
	return(vc$y)
	#lm(valoresCriticosAD$p~valoresCriticosAD$d)
}




defineClasses = function(dados, amplitude) {

	(pCentro = seq(floor(min(dados)),ceiling(max(dados)), amplitude))
#	pCentro = pCentro[1:length(pCentro)-1]

	(pClasse = cbind(as.matrix(pCentro)-(amplitude/2),as.matrix(pCentro+amplitude)-(amplitude/2)))
	#pClasse[dim(pClasse)[1], 2] =  max(dados)+0.00000001


	if (pClasse[dim(pClasse)[1], 2] == max(dados)) {
		pClasse[dim(pClasse)[1], 2] =  max(dados)+0.00000001
	}

	return(list(centro=pCentro, classe=pClasse))
}

add.col <- function(dataf, vec, namevec) {
  if (nrow(dataf) < length(vec) ){ dataf <-  # pads rows if needed
                                     rbind(dataf, matrix(NA, length(vec)-nrow(dataf), ncol(dataf),
                                                         dimnames=list( NULL, names(dataf) ) ) )
  }
  length(vec) <- nrow(dataf) # pads with NA's
  dataf[, namevec] <- vec; # names new col properly
  return(dataf)
}





dtrunc <- function(x, spec, a = -Inf, b = Inf, ...) {
  tt <- rep(0, length(x))
  g <- get(paste("d", spec, sep = ""), mode = "function")
  G <- get(paste("p", spec, sep = ""), mode = "function")
  tt[x>=a & x<=b] <- g(x[x>=a&x<=b], ...)/(G(b, ...) - G(a, ...))
  return(tt)
}

extrunc <- function(spec, a = -Inf, b = Inf,...) {
  f <- function(x) x * dtrunc(x, spec, a = a, b = b, ...)
  return(integrate(f, lower = a, upper = b)$value)
}

vartrunc <- function(spec, a = -Inf, b = Inf, ...) {
  ex <- extrunc(spec, a = a, b = b, ...)
  f <- function(x) (x - ex)^2 * dtrunc(x, spec, a = a, b = b, ...)
  tt <- integrate(f, lower = a, upper = b)$value
  return(tt)
}

ptrunc <- function(x, spec, a = -Inf, b = Inf, ...) {
  tt <- x
  aa <- rep(a, length(x))
  bb <- rep(b, length(x))
  G <- get(paste("p", spec, sep = ""), mode = "function")
  tt <- G(apply(cbind(apply(cbind(x, bb), 1, min), aa), 1, max), ...)
  tt <- tt - G(aa, ...)
  tt <- tt/(G(bb, ...) - G(aa, ...))
  return(tt)
}

qtrunc <- function(p, spec, a = -Inf, b = Inf, ...) {
  tt <- p
  G <- get(paste("p", spec, sep = ""), mode = "function")
  Gin <- get(paste("q", spec, sep = ""), mode = "function")
  tt <- Gin(G(a, ...) + p*(G(b, ...) - G(a, ...)), ...)
  return(tt)
}

rtrunc <- function(n, spec, a = -Inf, b = Inf, ...) {
  x <- u <- runif(n, min = 0, max = 1)
  x <- qtrunc(u, spec, a = a, b = b,...)
  return(x)
}

dirCreate <- function(dir_str) {
	# print(dir_str)
	dir.create(file.path(getwd(), dir_str), showWarnings = TRUE, recursive = TRUE)
}
