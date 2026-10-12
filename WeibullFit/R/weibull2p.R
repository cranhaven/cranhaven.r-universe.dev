#' @import R.oo
#' @import FAdist
#' @importFrom sqldf sqldf
#' @import mixdist
#' @import optimx
#' @importFrom kSamples ad.test
#' @import e1071
#' @importFrom R.methodsS3 setGenericS3
weibull = function(dados, amplitude = 2, te = NA, td = NA, maxIT=20, verbose) {
  if (!is.numeric(dados) || !all(is.finite(dados)))	stop("Invalid data")
  if (!is.numeric(amplitude) || !all(is.finite(amplitude)))	stop("Invalid amplitude")

  if (is.numeric(te) && !is.numeric(td)) {
    nome = "Weibull 2P trunc. a esquerda (forma, escala)"
  } else if (!is.numeric(te) && is.numeric(td)) {
    nome = "Weibull 2P trunc. a direita (forma, escala)"
  } else if (is.numeric(te) && is.numeric(td)) {
    nome = "Weibull 2P trunc. a esquerda e direita (forma, escala)"
  } else if (!is.numeric(te) && !is.numeric(td)) {
    nome = "Weibull 2P (forma, escala)"
  }

  resultadoClasses = defineClasses(dados, amplitude)

  weibull = list(te = te, td = td, nome = nome,amplitude = amplitude, dados = dados, estimado = NA, observado = NA, densidade = NA, centro = resultadoClasses$centro, classe = resultadoClasses$classe, forma = NA, escala = NA, metodo = NA, parametros = NA, cdfResultado = NA, freqAcumuladaEstimada = NA, freqAcumuladaObservada = NA, resultadoOptimx = NA, chutesIniciais = NA,
                 get = function(x) weibull[[x]], set = function(x, value) weibull[[x]] <- value, maxIT = maxIT)

  weibull$cdf = function(dados, forma, escala) {

    te = weibull$te
    td = weibull$td

    if (exists("fcdf")) remove(fcdf)

    if (is.numeric(te) && !is.numeric(td)) {
      fcdf = 1 - exp(((te/escala)^forma)-((dados/escala)^forma))
    } else if (!is.numeric(te) && is.numeric(td)) {
      fcdf = (1-(exp(-(dados/escala)^forma)))/(1-exp(-(td/escala)^forma))
    } else if (is.numeric(te) && is.numeric(td)) {
      fcdf = 1-(exp(((te/escala)^forma)-((dados/escala)^forma))/(1-exp(-(td/escala)^forma)))
    } else if (!is.numeric(te) && !is.numeric(td)) {
      fcdf = 1 - (exp(-(dados/escala)^forma))
    }

    return(fcdf)
  }

  weibull$fdp = function(dados, forma, escala) {

    te = weibull$te
    td = weibull$td

    if(is.numeric(forma) && is.numeric(escala)) {
      if (is.numeric(te) && !is.numeric(td)) {
        fpdf = ((forma/escala)*((dados/escala)^(forma-1))*exp(((te/escala)^forma)-((dados/escala)^forma)))
      } else if (!is.numeric(te) && is.numeric(td)) {
        fpdf = (((forma/escala)*((dados/escala)^(forma-1))*exp(-(dados/escala)^forma))/(1-exp(-(td/escala)^forma)))
      } else if (is.numeric(te) && is.numeric(td)) {
        fpdf = (((forma/escala)*((dados/escala)^(forma-1))*exp(((te/escala)^forma)-((dados/escala)^forma)))/(1-exp(-(td/escala)^forma)))
      } else if (!is.numeric(te) && !is.numeric(td)) {
        fpdf = ((forma/escala)*((dados/escala)^(forma-1))*exp(-(dados/escala)^forma))
      }
      return(fpdf)
    } else {
      stop("Enter the correct values for the function's parameters")
    }
  }

  weibull$loglik = function(params) {
    te = weibull$te
    td = weibull$td

    if(exists("b")) remove(b)
    if(exists("x")) remove(x)
    if(exists("n")) remove(n)
    if(exists("loglik")) remove(loglik)

    c = params[1] # forma
    b = params[2] # escala

    x = weibull$dados
    n = length(x)

    if (is.numeric(te) && !is.numeric(td)) {
      # Truncada a esquerda
      loglik = (n*te^c)/b^c+log(c)*n-log(b)*c*n+(sum((c-1)*log(x)))-sum(x^c)/b^c
    } else if (!is.numeric(te) && is.numeric(td)) {
      #maxima Truncada a direita
      loglik = -n*log(1-exp(1)^(-((td)/b)^c))+log(c)*n-log(b)*c*n+(sum((c-1)*log(x)))-(sum((x)^c)/b^c)
    } else if (is.numeric(te) && is.numeric(td)) {
      # Truncada a esquerda e direita
      loglik = ((n*(te)^c)/(b^c))-log(1-(exp(1)^(-((td)/b)^c)))+log(c)*n-log(b)*c*n+(sum((c-1)*log(x)))-(sum((x)^c)/b^c)
    } else if (!is.numeric(te) && !is.numeric(td)) {
      # Completa
      loglik = log(c)*n-log(b)*c*n+(sum((c-1)*log(x)))-sum(x^c)/b^c
    }

    return(-loglik)
  }

  weibull <- list2env(weibull)
  class(weibull) = "weibull"

  if(verbose)
	  cat(paste("\n\nComputando ", weibull$nome, "..."))

  weibull$observado = obtemObservado(weibull)
  if(verbose)
	  cat(paste("\n"))

  weibull = estimaParametros(weibull)


  return(weibull)
}

avaliaAderencia.weibull <- function(obj, parametros, significancia = 0.05) {

  dfParametros = as.data.frame(parametros[,1:(length(parametros)-1)])

  names(dfParametros)[2] = "forma"
  names(dfParametros)[3] = "escala"
  names(dfParametros)[4] = "locacao"

  loglike = parametros[,length(parametros)]

  nColunas = dim(dfParametros)[2]+1

  for(i in 1:nrow(dfParametros)) {

    if (!is.na(dfParametros$forma[i])) {

      forma =  as.double(dfParametros$forma[i])
      escala = as.double(dfParametros$escala[i])

      #Kolmogorov-smirnov
      resultado=testeKS(obj, forma=forma, escala=escala)


      #D
      dfParametros[i,nColunas] = as.numeric(resultado$D)
      #p-value
      dfParametros[i,nColunas+1] = resultado$pvalue

      #Rejeita?
      dfParametros[i,nColunas+2] = (as.numeric(resultado$pvalor) < significancia)

      remove(resultado)

      #Anderson Darling
      teorica = obtemTeorica(obj, obj$dados, forma, escala)

      resultado = tryCatch({
					kSamples::ad.test(obj$dados, teorica)
				  }, error = function(err) {
					return(NULL)
				  })

	  if (!is.null(resultado)) {
		  #A?
		  dfParametros[i,nColunas+3] = as.numeric(resultado$ad[1])
		  #p-value
		  dfParametros[i,nColunas+4] = as.numeric(resultado$ad[5])
		  #Rejeita?
		  dfParametros[i,nColunas+5] = (dfParametros[i,nColunas+4] <  significancia)

		  remove(resultado)
	  }

      #Qui-quadrado
      resultado = qui2(obj, pforma=forma, pescala=escala)
	  if (!is.null(resultado)) {
		  #X?
		  dfParametros[i,nColunas+6] = as.numeric(resultado$statistic)
		  #p-value
		  dfParametros[i,nColunas+7] = as.numeric(resultado$p.value)
		  #Rejeita?
		  dfParametros[i,nColunas+8] = (as.numeric(resultado$p.value) < significancia)
	  }
      remove(resultado)
    }

  }

  if(ncol(dfParametros) < 13) {
    for(j in ncol(dfParametros):13) {
      dfParametros[,j] <- 0
    }
  }
  names(dfParametros)[nColunas]="KS"
  names(dfParametros)[nColunas+1]="KS_p_value"
  names(dfParametros)[nColunas+2]="KS_rejeita"
  names(dfParametros)[nColunas+3]="AD"
  names(dfParametros)[nColunas+4]="AD_p_value"
  names(dfParametros)[nColunas+5]="AD_rejeita"
  names(dfParametros)[nColunas+6]="Qui2"
  names(dfParametros)[nColunas+7]="Qui2_p_value"
  names(dfParametros)[nColunas+8]="Qui2_rejeita"

	#AIC
	dfParametros = add.col(dfParametros, (round(AIC(loglike, 1), 4)), "AIC")


	#BIC
	dfParametros = add.col(dfParametros, (round(BIC(loglike, length(obj$dados), 1), 4)), "BIC")



  if (nrow(dfParametros) >= 1) {
	dfParametros = sqldf("SELECT * FROM dfParametros where forma IS NOT NULL")

    sqldfParametros = "SELECT * FROM dfParametros where forma != pChute1 and escala != pChute2 and locacao != pChute3"

    sqldfParametros = gsub("pChute1", as.numeric(obj$chutesIniciais[1]), sqldfParametros)
    sqldfParametros = gsub("pChute2", as.numeric(obj$chutesIniciais[2]), sqldfParametros)
    sqldfParametros = gsub("pChute3", as.numeric(obj$chutesIniciais[3]), sqldfParametros)
    dfParametros = sqldf(sqldfParametros)

	  if (nrow(dfParametros)>0) {
	    dfParametros = sqldf("SELECT * FROM dfParametros
  				  WHERE forma IS NOT NULL
					  AND (forma <> escala AND escala <> locacao)
					  ORDER BY KS_p_value DESC")
	    dfParametros$locacao = 0

	   }
  }



  return(dfParametros)

}
setGenericS3("avaliaAderencia")

estimaParametros.weibull <- function (obj, significancia = 0.05, considerar=1) {


  dfParametros = data.frame()

  maxIT = obj$maxIT


  iteracao = 1

  chutes = data.frame()
  chute = list()
  finalizado = FALSE

  while (TRUE) {
	if (finalizado && nrow(chutes)==1) {
		chute = list(forma = chutes[1,1], escala = chutes[1,2], locacao = 0)

			# cat(paste("\n\nPega o menor KS apos 20 iteracoes, \ncaso nao tenha encontrado KS <= 0.10:\n"))
			# print(chutes)


	} else if (iteracao == 1) {
		mu = mean(log(obj$dados))
		sigma2 = var(log(obj$dados))
		shape.guess = 1.2 / sqrt(sigma2)
		scale.guess = exp(mu + (0.572 / shape.guess))
		chute = list(forma = shape.guess, escala = scale.guess, locacao = 0)

	} else if (iteracao > 1){
		aleatorio = runif(1, 0.1, 1.4)
		mu = mean(log(obj$dados))
		sigma2 = var(log(obj$dados))
		shape.guess = aleatorio / sqrt(sigma2)
		scale.guess = exp(mu + (aleatorio / shape.guess))

		chute = list(forma = shape.guess, escala = scale.guess, locacao = 0)
	}

   obj$chutesIniciais = chute

    obj$resultadoOptimx = obtemParametros.weibull(obj)

	if (!is.null(obj$resultadoOptimx)) {
		parametrosIniciais = data.frame(metodo=attr(coef(obj$resultadoOptimx), "dimnames")[[1]], obj$resultadoOptimx[,1:4])
		parametrosIniciais = sqldf("SELECT * from parametrosIniciais where p1 >= 0 and p2 >= 0 and p3 >= 0")

		if(nrow(parametrosIniciais)>0) {
		  dfParametros = avaliaAderencia.weibull(obj, parametrosIniciais, significancia)
		}


		if (nrow(dfParametros) == 0) {

				# cat(paste("\nNao encontrou parametros, iteracao:", iteracao))
		} else {

			obj$set("forma", dfParametros$forma[considerar])
			obj$set("escala", dfParametros$escala[considerar])

			cdfResultado = obtemEstimado.weibull(obj, forma=obj$get("forma"), escala=obj$get("escala"))
			obj$cdfResultado = cdfResultado
			obj$densidade = cdfResultado$probabilidade
			obj$estimado=cdfResultado$estimado

			if (min(obj$estimado, na.rm=TRUE) < 5) {
				dfParametros$Qui2 = ""
				dfParametros$Qui2_p_value = ""
				dfParametros$Qui2_rejeita = ""
			}

			obj$set("metodo", dfParametros$metodo[considerar])
			obj$set("parametros", dfParametros)


			obj$freqAcumuladaObservada <- cumsum(obj$observado)/sum(obj$observado)
			obj$freqAcumuladaEstimada <- obj$cdf(obj$classe[,2], forma=obj$get("forma"), escala=obj$get("escala"))

		}


		# cat(paste("\n---------------------------------------------------------------------------------------------------------------------------------------------------"))
		# cat(paste("\nIteracao: ", iteracao))
		# cat(paste("\n---------------------------------------------------------------------------------------------------------------------------------------------------"))
		# cat(paste( "\n"))
		# print(dfParametros[considerar,])


		if (nrow(dfParametros)>0)  {
			# Termina caso encontre um KS menor que 0.10
			if ((!dfParametros$KS_rejeita[considerar]) && dfParametros$KS[considerar] < 0.10) {
				obj$iteracoes = iteracao + 1
				break;
			}
			if (!finalizado) {
				chutes = rbind(chutes, data.frame(chute, ks=dfParametros$KS[considerar], ksrejeita=dfParametros$KS_rejeita[considerar]))
			}
		}

		if (iteracao == (maxIT)) {
		  if (nrow(chutes)>1) {
			  chutes = sqldf("SELECT * from chutes where ksrejeita=0 order by ks asc limit 1")
			  iteracao = iteracao - 1
			  finalizado = TRUE
		  } else if (finalizado && nrow(chutes)==1) {
			obj$iteracoes = iteracao + 1
			remove(dfParametros)
			break;
		  } else {

				# cat(paste("\n\nNao convergiu!"))
		    obj$iteracoes = -999999
			break;
		  }
		}

	}

    iteracao = iteracao + 1

  }

  return(obj)

}
setGenericS3("estimaParametros")

print.weibull.default <- function(obj) {
  cat(paste("\n"))
  cat(paste(obj$get("nome")))

  te = obj$get("te")
  td = obj$get("td")

  cat(paste("\n\nForma: ", obj$get("forma"), ", escala:", obj$get("escala")))
  if (is.numeric(te) && !is.numeric(td)) {
    cat(paste("\nTruncamento a esquerda: ", obj$get("te")))
  } else if (!is.numeric(te) && is.numeric(td)) {
    cat(paste("\nTruncamento a direita: ", obj$get("td")))
  } else if (is.numeric(te) && is.numeric(td)) {
    cat(paste("\nTruncamento a esquerda e a direita: ", obj$get("te"), ", ", obj$get("td")))
  }

  cat(paste("\nMetodo: Maxima verossimilhanca, algoritmo: ", toString(obj$get("metodo"))))
  cat(paste("\n\nDados: ", toString(obj$get("dados"))))
  cat(paste("\n\nValor m?nimo: ", toString(min(obj$get("dados")))," Valor m?ximo:", toString(max(obj$get("dados")))))
  cat(paste("\n\nParametros estimados:\n"))
  print(obj$get("parametros"))
  cat(paste("\nClasses:\n "))
  print(obj$get("classe"))
  cat(paste("\nCentro: "))
  cat(paste(obj$get("centro")))
  cat(paste("\nEstimado por classes:\n "))
  print(obj$get("cdfResultado"))
  cat(paste("\n\nObservado: "))
  cat(paste(obj$get("observado")))
  cat(paste("\n\nEstimado: "))
  cat(paste(obj$get("estimado")))
  cat(paste("\n\nProbabilidade: "))
  cat(paste(obj$get("densidade")))
  cat(paste("\n\nFrequencia acumulada estimada: "))
  cat(paste(obj$get("freqAcumuladaEstimada")))
  cat(paste("\n\nFrequencia acumulada observada: "))
  cat(paste(obj$get("freqAcumuladaObservada")))

}
setGenericS3("print.weibull")

testeKS.weibull = function(obj, forma, escala) {
  ksresult = NA

  if (is.numeric(obj$te) && !is.numeric(obj$td)) {
	ksresult = ks.test(obj$dados, "ptrunc", spec="weibull", shape=forma, scale=escala, a=obj$te)
  } else if (!is.numeric(obj$te) && is.numeric(obj$td)) {
	ksresult = ks.test(obj$dados, "ptrunc", spec="weibull", shape=forma, scale=escala, b=obj$td)
  } else if (is.numeric(obj$te) && is.numeric(obj$td)) {
	ksresult = ks.test(obj$dados, "ptrunc", spec="weibull", shape=forma, scale=escala, a=obj$te, b=obj$td)
  } else if (!is.numeric(obj$te) && !is.numeric(obj$td)) {
     ksresult = ks.test(obj$dados, "pweibull", shape=forma, scale=escala)
  }

  resultado = data.frame(D = as.numeric(ksresult$statistic), pvalor = as.numeric(ksresult$p.value), pvalue = as.numeric(ksresult$p.value))
  return(resultado)

}
setGenericS3("testeKS")

parametros.weibull = function(obj) {

  if (is.null(obj$parametros)) {stop("It is necessary to estimate the parameters and its adherences.\n Use the function avaliaAderencia.")}
  p = as.list(c(nome = obj$nome, obj$parametros[1,]))
  return(p)
}
setGenericS3("parametros")

obtemObservado.weibull = function(obj) {
  fos = c()
  for(i in 1:length(obj$centro)) {
    inferior = obj$classe[i,1]
    superior = obj$classe[i,2]
    fo = length(obj$dados[obj$dados >= inferior & obj$dados < superior])
    fos = c(fos, fo)
  }
  return(fos)
}
setGenericS3("obtemObservado")

obtemEstimado.weibull <- function(obj, forma, escala) {
  estimad = c()
  probabilidad = c()

  classe = obj$classe
  n = length(obj$dados)

  for(i in 1:length(obj$centro)) {
    superior = obj$cdf(classe[i,2], forma, escala)
    inferior = obj$cdf(classe[i,1], forma, escala)

    prob = superior - inferior

    est = prob * n
    estimad = c(estimad, est)
    probabilidad = c(probabilidad, prob)
  }

  dfCDF = data.frame(inferior = obj$classe[,1], superior = obj$classe[,2], probabilidade = probabilidad, estimado = estimad)

  return (dfCDF)

}
setGenericS3("obtemEstimado")


obtemParametros.weibull = function(obj) {
	resultadoOtimizacao = tryCatch({
							optimx(as.double(obj$chutesIniciais), obj$loglik, control=list(all.methods=TRUE, trace=0))
						}, error = function(err) {
							return(NULL)
						})
	return(resultadoOtimizacao)
}
setGenericS3("obtemParametros")

qui2.weibull = function(obj, pescala, pforma) {
  # Compara??o de propor??es observadas com propor??es esperadas - uma das aplica??es do qui-quadrado ? comparar a freq??ncia observada (n?mero de ocorr?ncia, distribui??o de freq??ncia) de um certo evento, em geral, em amostra, tendo uma freq??ncia esperada segundo uma determinada teoria.
  cdfEsperado = obtemEstimado(obj, forma=pforma, escala=pescala)
  if (!is.null(cdfEsperado$probabilidade)) {
	  if (sum(cdfEsperado$probabilidade) > 0)   {
		xi = chisq.test(obj$observado, p = cdfEsperado$probabilidade, rescale.p=TRUE)
		return(xi)
	  } else {
		return(NULL)
	  }

  } else {
  		return(NULL)
  }

}
setGenericS3("qui2")


obtemTeorica.weibull = function(obj, dados, forma, escala) {
  #executar ao final da estimativa

  n = length(dados)

  te = obj$te
  td = obj$td

  if (is.numeric(te) & !is.numeric(td)) {
    teorico = rtrunc(n, "weibull", a=te, shape=forma, scale=escala)
  } else if (!is.numeric(te) & is.numeric(td)) {
    teorico = rtrunc(n, "weibull", b=td, shape=forma, scale=escala)
  } else if (is.numeric(te) & is.numeric(td)) {
    teorico = rtrunc(n, "weibull", a=te, b=td, shape=forma, scale=escala)
  } else if (!is.numeric(te) & !is.numeric(td)) {
    teorico = rweibull(n, shape=forma, scale=escala)
  }

  return(teorico)
}
setGenericS3("obtemTeorica")



plot.weibull <- function(obj, prob=FALSE, main = "Histograma", acumulado=FALSE, teorico=FALSE) {
  te = obj$te
  td = obj$td
  n = length(obj$dados)
  forma  = obj$forma
  escala = obj$escala
  if (acumulado == TRUE) {
    if (teorico == TRUE) {

      if (is.numeric(te) & !is.numeric(td)) {
        teorico = rtrunc(n, "weibull", a=te, shape=forma, scale=escala)
      } else if (!is.numeric(te) & is.numeric(td)) {
        teorico = rtrunc(n, "weibull", b=td, shape=forma, scale=escala)
      } else if (is.numeric(te) & is.numeric(td)) {
        teorico = rtrunc(n, "weibull", a=te, b=td, shape=forma, scale=escala)
      } else if (!is.numeric(te) & !is.numeric(td)) {
        teorico = rweibull(n, shape=forma, scale=escala)
      }

      x<-seq(min(teorico)-3, max(teorico)+3, 0.01) # Criar as coordenadas te?ricas para o eixo x

      if (is.numeric(te) & !is.numeric(td)) {
        y = ptrunc(x, "weibull", a=te, shape=forma, scale=escala)
      } else if (!is.numeric(te) & is.numeric(td)) {
        y = ptrunc(x, "weibull", b=td, shape=forma, scale=escala)
      } else if (is.numeric(te) & is.numeric(td)) {
        y = ptrunc(x, "weibull", a=te, b=td, shape=forma, scale=escala)
      } else if (!is.numeric(te) & !is.numeric(td)) {
        y = pweibull(x, shape=forma, scale=escala)
      }

      plot(ecdf(obj$dados), lwd=3, verticals = TRUE, col="blue", main=main)
      lines(x, y, lwd=2, lty=4, col="red") # Criar a curva te?rica
    } else {
      plot(obj$freqAcumuladaObservada~obj$centro, type="l", col="blue", main=main, lwd=2, ylab="Acumulado", xlab="Centro de Classe")
      lines(obj$freqAcumuladaEstimada~obj$centro, type="l", col="red", lwd=2, lty=5)
      legend("topleft",  c("Observada","Estimada"), lty=c(1,5), lwd=c(2.5,2.5),col=c("blue","red"))
    }
  } else {
    if (prob==FALSE) {
      bp = barplot(height=obj$observado, col="white", space=0, main=main, ylab="Frequencia", xlab="Centro de Classe")
      axis(1, at=bp, labels=obj$centro)

      par(new=TRUE)
      plot(obj$estimado, col="red", type="b", pch=18, axes = FALSE, xlab = "", ylab = "")
    } else {
      hist(obj$dados, xlab="DAP", ylab="Densidade",  main=main, breaks=length(obj$centro), prob = prob)
      lines(obj$densidade~obj$centro,col="red",type="l")


    }
  }
}
setGenericS3("plot")
