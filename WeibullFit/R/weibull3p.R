#' @import R.oo
#' @import FAdist
#' @importFrom sqldf sqldf
#' @import mixdist
#' @import optimx
#' @importFrom kSamples ad.test
#' @import e1071
#' @importFrom R.methodsS3 setGenericS3
weibull3 = function(dados, amplitude = 2, te = NA, td = NA, maxIT=20, verbose) {

  options(scipen=10)
  if (!is.numeric(dados) || !all(is.finite(dados)))	stop("Invalid Data")
  if (!is.numeric(amplitude) || !all(is.finite(amplitude)))	stop("Invalid Amplitude")

  if (is.numeric(te) && !is.numeric(td)) {
    nome = "Weibull 3P trunc. a esquerda (forma, escala, locacao)"
  } else if (!is.numeric(te) && is.numeric(td)) {
    nome = "Weibull 3P trunc. a direita (forma, escala, locacao)"
  } else if (is.numeric(te) && is.numeric(td)) {
    nome = "Weibull 3P trunc. a esquerda e direita (forma, escala, locacao)"
  } else if (!is.numeric(te) && !is.numeric(td)) {
    nome = "Weibull 3P (forma, escala, locacao)"
  }

  resultadoClasses = defineClasses(dados, amplitude)

  weibull3 = list(te = te, td = td, nome = nome,amplitude = amplitude, dados = dados, estimado = NA, observado = NA, densidade = NA, centro = resultadoClasses$centro, classe = resultadoClasses$classe, forma = NA, escala = NA, locacao = NA, metodo = NA, parametros = NA, cdfResultado = NA, freqAcumuladaEstimada = NA, freqAcumuladaObservada = NA, resultadoOptimx = NA, chutesIniciais = NA, iteracoes = NA,
                  get = function(x) weibull3[[x]], set = function(x, value) weibull3[[x]] <- value, maxIT=maxIT)

  weibull3$cdf = function(dados, forma, escala, locacao) {

    te = weibull3$te
    td = weibull3$td

    if (exists("fcdf")) remove(fcdf)

    if (is.numeric(te) && !is.numeric(td)) {
      fcdf = 1 - exp((((te-locacao)/escala)^forma)-(((dados-locacao)/escala)^forma))
    } else if (!is.numeric(te) && is.numeric(td)) {
      fcdf = (1- exp(-((dados-locacao)/escala)^forma))/(1-exp(-((td-locacao)/escala)^forma))
    } else if (is.numeric(te) && is.numeric(td)) {
      fcdf = (1-exp((((te-locacao)/escala)^forma)-(((dados-locacao)/escala)^forma)))/(1-exp(-((td-locacao)/escala)^forma))
    } else if (!is.numeric(te) && !is.numeric(td)) {
      fcdf = 1 - exp(-((dados-locacao)/escala)^forma)
    }

    return(fcdf)
  }

  weibull3$fdp = function(dados, forma, escala, locacao) {

    te = weibull3$te
    td = weibull3$td

    if(is.numeric(forma) && is.numeric(escala)) {
      if (is.numeric(te) && !is.numeric(td)) {
        fpdf = ((forma/escala)*(((dados-locacao)/escala)^(forma-1))*exp((((te-locacao)/escala)^forma)-(((dados-locacao)/escala)^forma)))
      } else if (!is.numeric(te) && is.numeric(td)) {
        fpdf = (((forma/escala)*(((dados-locacao)/escala)^(forma-1))*exp(-((dados-locacao)/escala)^forma))/(1-exp(-((td-locacao)/escala)^forma)))
      } else if (is.numeric(te) && is.numeric(td)) {
        fpdf = (((forma/escala)*(((dados-locacao)/escala)^(forma-1))*exp((((te-locacao)/escala)^forma)-(((dados-locacao)/escala)^forma)))/(1-exp(-((td-locacao)/escala)^forma)))
      } else if (!is.numeric(te) && !is.numeric(td)) {
        fpdf = ((forma/escala)*(((dados-locacao)/escala)^(forma-1))*exp(-((dados-locacao)/escala)^forma))
      }
      return(fpdf)
    } else {
      stop("Enter the correct values for the function's parameters")
    }
  }

  weibull3$loglik = function(params) {

    te = weibull3$te
    td = weibull3$td

    c = params[1] # forma
    b = params[2] # escala
    a = params[3] # locacao

    x = weibull3$dados
    n = length(x)

    if (is.numeric(te) && !is.numeric(td)) {

      #correta máxima
      loglik3p = n*((te-a)/b)^c + log(c) * n - log(b) * c * n + (sum((c-1)*log(x-a)))-(sum((x-a)^c)/(b^c))

    } else if (!is.numeric(te) && is.numeric(td)) {
      # Truncada à direita
      #maxima
      loglik3p = -n*log(1-exp(1)^(-((td-a)/b)^c))+log(c)*n-log(b)*c*n+(sum((c-1)*log(x-a)))-(sum((x-a)^c)/b^c)
    } else if (is.numeric(te) && is.numeric(td)) {
      # Truncada à direita e à esquerda.
      #maxima  Luciane
      loglik3p = ((n*(te-a)^c)/(b^c))-log(1-(exp(1)^(-((td-a)/b)^c)))+log(c)*n-log(b)*c*n+(sum((c-1)*log(x-a)))-(sum((x-a)^c)/b^c)
    } else if (!is.numeric(te) && !is.numeric(td)) {
      #Completa
      loglik3p = n*log(c)-n*c*log(b)+(c-1)*(sum(log(x-a)))-(1/(b^c))*(sum((x-a)^c))
    }

    return(-loglik3p)
  }

  weibull3 <- list2env(weibull3)
  class(weibull3) = c("weibull3","weibull")


  # cat(paste("\n\nComputando ", weibull3$nome, "..."))

  weibull3$observado = obtemObservado.weibull3(weibull3)

  weibull3 = estimaParametros.weibull3(weibull3)


  return(weibull3)
}

avaliaAderencia.weibull3 <- function(obj, parametros, significancia = 0.05) {

	dfParametros = as.data.frame(parametros[,1:(length(parametros)-1)])

	names(dfParametros)[2] = "forma"
	names(dfParametros)[3] = "escala"
	names(dfParametros)[4] = "locacao"

	loglike = parametros[,length(parametros)]

	nColunas = dim(dfParametros)[2]+1


	for(i in 1:nrow(dfParametros)) {
		if (is.na(dfParametros$forma[i]) == FALSE) {

		  forma =  as.double(dfParametros$forma[i])
		  escala = as.double(dfParametros$escala[i])
		  locacao = as.double(dfParametros$locacao[i])

		  #Kolmogorov-smirnov
		  resultado=testeKS.weibull3(obj, forma=forma, escala=escala,locacao=locacao)

		  #D
		  dfParametros[i,nColunas] = as.numeric(resultado$D)

		  #p-value
		  dfParametros[i,nColunas+1] = resultado$pvalue

		  #Rejeita?
		  dfParametros[i,nColunas+2] = (as.numeric(resultado$pvalor) < significancia)

		  remove(resultado)

		  #Anderson Darling
		  teorica = obtemTeorica.weibull3(obj, obj$dados, forma, escala, locacao)


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
		  resultado = qui2.weibull3(obj, pforma=forma, pescala=escala, plocacao=locacao)

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


	if (nrow(dfParametros) >= 1) {
		dfParametros = sqldf("SELECT * FROM dfParametros where forma IS NOT NULL and forma >= 0 and escala >= 0 and locacao >= 0 and forma <> escala and escala <> locacao")
	}

	if (nrow(dfParametros) >= 1) {
		sqldfParametros = "SELECT * FROM dfParametros where forma != pChute1 and escala != pChute2 and locacao != pChute3"

		sqldfParametros = gsub("pChute1", as.numeric(obj$chutesIniciais[1]), sqldfParametros)
		sqldfParametros = gsub("pChute2", as.numeric(obj$chutesIniciais[2]), sqldfParametros)
		sqldfParametros = gsub("pChute3", as.numeric(obj$chutesIniciais[3]), sqldfParametros)
		dfParametros = sqldf(sqldfParametros)
	}


	if (nrow(dfParametros) >= 1) {
		dfParametros = sqldf("SELECT * FROM dfParametros
								  WHERE forma IS NOT NULL
								  AND (escala <> locacao and escala <> 1)
								  ORDER BY KS ASC")

	}

	#AIC
	dfParametros = add.col(dfParametros, (round(AIC(loglike, 2), 4)), "AIC")


	#BIC
	dfParametros = add.col(dfParametros, (round(BIC(loglike, length(obj$dados), 2), 4)), "BIC")

	return(dfParametros)

}
setGenericS3("avaliaAderencia")

estimaParametros.weibull3 <- function (obj, significancia = 0.05, considerar=1) {

  dfParametros = data.frame()

  maxIT = obj$maxIT

  iteracao = 1

  chutes = data.frame()
  chute = list()
  finalizado = FALSE

  while (TRUE) {
	if (finalizado && nrow(chutes)==1) {
		chute = list(forma = chutes[1,1], escala = chutes[1,2], locacao = chutes[1,3])
		# cat(paste("\n\nPega o menor KS apos 20 iteracoes, \ncaso nao tenha encontrado KS <= 0.10:\n"))
		# print(chutes)
	} else if (iteracao == 1) {
		mu = mean(log(obj$dados))
		sigma2 = var(log(obj$dados))
		shape.guess = 1.2 / sqrt(sigma2)
		scale.guess = exp(mu + (0.572 / shape.guess))
		thres.guess = 1

		chute = list(forma = shape.guess, escala = scale.guess, locacao = thres.guess)

	} else if (iteracao > 1){
		aleatorio = runif(1, 0.1, 1.4)
		mu = mean(log(obj$dados))
		sigma2 = var(log(obj$dados))
		shape.guess = aleatorio / sqrt(sigma2)
		scale.guess = exp(mu + (aleatorio / shape.guess))
		thres.guess = aleatorio

		chute = list(forma = shape.guess, escala = scale.guess, locacao = thres.guess)
	}

   obj$chutesIniciais = chute

    obj$resultadoOptimx = obtemParametros.weibull3(obj)


	if (!is.null(obj$resultadoOptimx)) {

		obj$resultadoOptimx = obj$resultadoOptimx[obj$resultadoOptimx$convcode==0,]

		parametrosIniciais = data.frame(metodo=attr(coef(obj$resultadoOptimx), "dimnames")[[1]], obj$resultadoOptimx[,1:4])

		if(nrow(parametrosIniciais)>0){
			dfParametros = avaliaAderencia.weibull3(obj, parametrosIniciais, significancia)
		}


		if (nrow(dfParametros) == 0) {
			# cat(paste("\nNao encontrou parametros, iteracao:", iteracao))
		} else {
			obj$set("forma", dfParametros$forma[considerar])
			obj$set("escala", dfParametros$escala[considerar])
			obj$set("locacao", dfParametros$locacao[considerar])


			cdfResultado = obtemEstimado.weibull3(obj, forma=obj$get("forma"), escala=obj$get("escala"), locacao=obj$get("locacao"))
			obj$cdfResultado = cdfResultado
			obj$densidade = cdfResultado$probabilidade
			obj$estimado=cdfResultado$estimado


			if (min(obj$estimado,na.rm = TRUE) < 5) {
				dfParametros$Qui2 = ""
				dfParametros$Qui2_p_value = ""
				dfParametros$Qui2_rejeita = ""
			}

			obj$set("metodo", dfParametros$metodo[considerar])
			obj$set("parametros", dfParametros)


			obj$freqAcumuladaObservada <- cumsum(obj$observado)/sum(obj$observado)
			obj$freqAcumuladaEstimada <- obj$cdf(obj$classe[,2], forma=obj$get("forma"), escala=obj$get("escala"), locacao=obj$get("locacao"))

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

print.weibull3.default <- function(obj) {
  cat(paste(obj$get("nome")))
  cat(paste("\nlocacao: ", obj$get("locacao")), ", ")
  UseMethod("print.weibull")
}
setGenericS3("print.weibull3")


testeKS.weibull3 = function(obj, forma, escala, locacao) {

  ksresult = NA

  if (is.numeric(obj$te) && !is.numeric(obj$td)) {
	ksresult = ks.test(obj$dados, "ptrunc", spec="weibull3", shape=forma, scale=escala, thres=locacao, a=obj$te)
  } else if (!is.numeric(obj$te) && is.numeric(obj$td)) {
	ksresult = ks.test(obj$dados, "ptrunc", spec="weibull3", shape=forma, scale=escala, thres=locacao, b=obj$td)
  } else if (is.numeric(obj$te) && is.numeric(obj$td)) {
	ksresult = ks.test(obj$dados, "ptrunc", spec="weibull3", shape=forma, scale=escala, thres=locacao, a=obj$te, b=obj$td)
  } else if (!is.numeric(obj$te) && !is.numeric(obj$td)) {
     ksresult = ks.test(obj$dados, "pweibull3", shape=forma, scale=escala, thres=locacao)
  }

  resultado = data.frame(D = as.numeric(ksresult$statistic), pvalor = as.numeric(ksresult$p.value), pvalue = as.numeric(ksresult$p.value))

  return(resultado)
}
setGenericS3("testeKS")

parametros.weibull3 = function(obj) {
  if (is.null(obj$parametros)) {stop("It is necessary to estimate the parameters and its adherences.\n Use the function avaliaAderencia")}
  p = as.list(c(nome = obj$nome, obj$parametros[1,]))
  return(p)
}
setGenericS3("parametros")

obtemObservado.weibull3 = function(obj) {
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

obtemParametros.weibull3 = function(obj) {
  resultadoOtimizacao = tryCatch({
	 quiet(optimx(as.double(obj$chutesIniciais), obj$loglik , lower=EPS, control=list(all.methods=TRUE, trace=0)))

  }, error = function(err) {
		return(NULL)
  })
  return(resultadoOtimizacao)
}
setGenericS3("obtemParametros")

qui2.weibull3 = function(obj, pforma, pescala, plocacao) {
  # Compara??o de propor??es observadas com propor??es esperadas - uma das aplica??es do qui-quadrado ? comparar a freq??ncia observada (n?mero de ocorr?ncia, distribui??o de freq??ncia) de um certo evento, em geral, em amostra, tendo uma freq??ncia esperada segundo uma determinada teoria.

 cdfEsperado = obtemEstimado(obj, forma=pforma, escala=pescala, locacao=plocacao)
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

obtemEstimado.weibull3 <- function(obj, forma, escala, locacao) {
  estimad = c()
  probabilidad = c()

  classe = obj$classe
  n = length(obj$dados)


  for(i in 1:length(obj$centro)) {
    superior = obj$cdf(classe[i,2], forma, escala, locacao)
    inferior = obj$cdf(classe[i,1], forma, escala, locacao)

    prob = superior - inferior

    est = prob * n

    probabilidad = c(probabilidad, prob)
    estimad = c(estimad, est)
  }

  dfCDF = data.frame(inferior = obj$classe[,1], superior = obj$classe[,2], probabilidade = probabilidad, estimado = estimad)


  dfCDF$probabilidade[is.nan(dfCDF$probabilidade)] <- 0
  dfCDF$estimado[is.nan(dfCDF$estimado)] <- 0

  return (dfCDF)

}
setGenericS3("obtemEstimado")

obtemTeorica.weibull3 = function(obj, dados, forma, escala, locacao) {
  #executar ao final da estimativa

  n = length(dados)

  te = obj$te
  td = obj$td

  funcao = "weibull3"

  if (is.numeric(te) & !is.numeric(td)) {
    teorico = rtrunc(n, funcao, a=te, shape=forma, scale=escala, thres=locacao)
  } else if (!is.numeric(te) & is.numeric(td)) {
    teorico = rtrunc(n, funcao, b=td, shape=forma, scale=escala, thres=locacao)
  } else if (is.numeric(te) & is.numeric(td)) {
    teorico = rtrunc(n, funcao, a=te, b=td, shape=forma, scale=escala, thres=locacao)
  } else if (!is.numeric(te) & !is.numeric(td)) {
    teorico = rweibull3(n, shape=forma, scale=escala, thres=locacao)
  }

  return(teorico)
}
setGenericS3("obtemTeorica")



plot.weibull3 <- function(obj, prob=FALSE, main = "", acumulado=FALSE, teorico = FALSE) {

  # cat(paste("\nPlotando ", obj$nome))

  if (!all(is.na(obj$estimado)) & !all(is.na(obj$observado))) {
    te = obj$te
    td = obj$td
    n = length(obj$dados)
    forma  = obj$forma
    escala = obj$escala
    locacao = obj$locacao
    funcao = "weibull3"
    if (acumulado == TRUE) {
      if (teorico == TRUE) {

        if (is.numeric(te) & !is.numeric(td)) {
          teorico = rtrunc(n, funcao, a=te, shape=forma, scale=escala, thres=locacao)
        } else if (!is.numeric(te) & is.numeric(td)) {
          teorico = rtrunc(n, funcao, b=td, shape=forma, scale=escala, thres=locacao)
        } else if (is.numeric(te) & is.numeric(td)) {
          teorico = rtrunc(n, funcao, a=te, b=td, shape=forma, scale=escala, thres=locacao)
        } else if (!is.numeric(te) & !is.numeric(td)) {
          teorico = rweibull3(n, shape=forma, scale=escala, thres=locacao)
        }

        x<-seq(min(teorico)-3, max(teorico)+3, 0.01) # Criar as coordenadas teoricas para o eixo x

        if (is.numeric(te) & !is.numeric(td)) {
          y = ptrunc(x, funcao, a=te, shape=forma, scale=escala, thres=locacao)
        } else if (!is.numeric(te) & is.numeric(td)) {
          y = ptrunc(x, funcao, b=td, shape=forma, scale=escala, thres=locacao)
        } else if (is.numeric(te) & is.numeric(td)) {
          y = ptrunc(x, funcao, a=te, b=td, shape=forma, scale=escala, thres=locacao)
        } else if (!is.numeric(te) & !is.numeric(td)) {
          y = pweibull3(x, shape=forma, scale=escala, thres=locacao)
        }

        plot(ecdf(obj$dados), lwd=3, verticals = TRUE, col="blue", main=main)
        lines(x, y, lwd=2, lty=4, col="red") # Criar a curva teorica
      } else {
        plot(obj$freqAcumuladaObservada~obj$centro, type="l", col="blue", main=main, lwd=2, ylab="Acumulado", xlab="Centro de Classe")
        lines(obj$freqAcumuladaEstimada~obj$centro,  col="red", lwd=2, lty=5)
        legend("topleft",  c("Observada","Estimada"), lty=c(1,5), lwd=c(2,2),col=c("blue","red"))
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
  } else {
    # cat("Nao foi possivel realizar a estimativa")
  }
}
setGenericS3("plot")

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}
