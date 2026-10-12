#' @title Weibull-fitting function
#' @name weibullFit
#' @description This functions calculates the shape, scale and location parameters for the weibull distribution to the input data and save the plots.
#'
#' @param dataFrame the input data frame containing the independent, continuous variable.
#' @param primaryGroup the name(String) of the primary grouping column of the data frame.
#' @param secondaryGroup the name(String) of the secondary grouping column of the data frame.
#' @param restrValue the restriction value choosen to be applied to the secondary group column.
#' @param pValue the name(String) of the independent, continuos variable to be analyzed.
#' @param leftTrunc An integer, defining the value for the weibull's function truncation.
#' @param folder the pathname of the folder where the plots will be saved.
#' @param selectedFunctions A character vector determining which weibull function to be applied. Can be any of the following: w2, w2te, w2td, w2tetd, w3, w3te, w3td, w3tetd
#' @param limit A positive integer determining the maximum number of rows from the data frame (grouped by the primary group column) to be analyzed.
#' @param amp The continuous variable class width to be accounted for the calculations.
#' @param pmaxIT A positive integer, the maximum number of iterations used by the algorithm to try to get the weibull function parameters, for each primary group.
#' @param verbose Logical, determines if the function prints more detailed results on the console.
#'
#' @details This function first extracts a subset of the input data frame using the restrValue parameter applied
#'          to the secondary group column. Then, it calculates the weibull function scale, shape and location parameters
#'          using the maximum-likelyhood method. Finally, it plots the results (as .wmf, .csv and .jpeg) inside the folder
#'          given by the Folder parameter.
#'
#' @return A data frame object containing the best results for shape, location and scale parameters.
#'
#' @examples
#' functions <- c("w2", "w3")
#' best <- weibullFit(restrValue = 60, dataFrame = TreesDBH,
#' selectedFunctions = functions, amp = 2, pmaxIT = 1, limit = 1)
#'
#'
#' @importFrom sqldf sqldf
#' @importFrom xtable xtable
#' @importFrom glue glue
#' @importFrom grDevices dev.off
#' @import stats
#' @import graphics
#' @importFrom grDevices png
#' @importFrom utils capture.output write.table
#'
#' @export
weibullFit = function(dataFrame, primaryGroup="parcela", secondaryGroup="idadearred", restrValue, pValue="dap", leftTrunc=5, folder = NA, limit=100000, selectedFunctions = NULL, amp=2, pmaxIT=20, verbose=FALSE) {

	##Extração dos dados específicos
	newDF = data.frame(extractData(dataFrame=dataFrame, restrVal=restrValue, primaryGroup=primaryGroup, secondaryGroup=secondaryGroup, pValue = pValue))
	dataFrameName=as.character(quote(newDF))

	if(!is.na(folder)) {
	  dirCreate(folder)
	}

	if (!is.null(getOption("sqldf.connection"))) sqldf()

	if (is.na(pmaxIT) || is.null(pmaxIT) || !is.numeric(pmaxIT) || pmaxIT < 1 || !(pmaxIT%%1==0)) {
	stop("Insert a valid number of iterations (pmaxIT) >= 1")
	}

	##Escolha das funções por vetor de strings: c("w2","w2te", ...)
	if (is.null(selectedFunctions)) {
	functions = list(w2 = TRUE, w2te = TRUE, w2td = TRUE, w2tetd = TRUE, w3 = TRUE, w3te = TRUE, w3td = TRUE, w3tetd = TRUE)
	} else {
		functions = list(w2 = FALSE, w2te = FALSE, w2td = FALSE, w2tetd = FALSE, w3 = FALSE, w3te = FALSE, w3td = FALSE, w3tetd = FALSE)
		for(i in 1:length(selectedFunctions)) {
			functions[selectedFunctions[i]]=TRUE
		}
	}

  sqlPrimaryKey <- glue("SELECT DISTINCT {primaryGroup} FROM {dataFrameName} ORDER BY {primaryGroup} LIMIT {limit}")
	dfPrimaryKeys = sqldf(sqlPrimaryKey)
	if(nrow(dfPrimaryKeys)==0)
	  stop(glue("No {primaryGroup} found in {dataFrame} with restriction value of {restrValue}!"))

	#Verificando o tipo da variavel do Primary Group:
	classePrimaryGroup <- class(dfPrimaryKeys[1,1])

	if(verbose)
	  cat(paste("\n",primaryGroup,"\n"))

	if(verbose) cat(paste("\nGenerating result for ", nrow(dfPrimaryKeys)," instances of ", primaryGroup, sep=""))


	dfAll = data.frame()
	dfBest = data.frame()
	dfIter = data.frame()
	dfWeibulls3p = data.frame()

	if (!is.null(getOption("sqldf.connection"))) sqldf()




	for(i in 1:nrow(dfPrimaryKeys)) {
	pkInstance = dfPrimaryKeys[i, 1]
	if(verbose) cat(paste("\n......................................", sep=""))
	#    cat(paste("\n\nParcela ", parcela, sep=""))

	if(classePrimaryGroup == "character") {
	  sqlData <- glue("SELECT * FROM {dataFrameName} WHERE {primaryGroup} = '{pkInstance}'")
	} else {
	  sqlData <- glue("SELECT * FROM {dataFrameName} WHERE {primaryGroup} = {pkInstance}")
	}

	remove(values)
	values = sqldf(sqlData)
	values = values[,3]

	# ------ WEIBULL 2P-----------------------------------------------
	if(verbose) cat(glue("\n\n{primaryGroup} {pkInstance} ({i} out of {nrow(dfPrimaryKeys)})\n"))
	dfWeibulls2p = data.frame()

	#Corrigido até aqui
	w2p1 = NA
	if (functions[[1]]== TRUE) {
		w2p1 = weibull(values, amplitude=amp, maxIT=pmaxIT, verbose = verbose)
		if (w2p1$iteracoes > 0) {
			dfWeibulls2p = rbind(data.frame(parametros(w2p1), parcela=pkInstance))
			grava("_1-W2P", w2p1, pkInstance, restrValue, pasta=folder, primaryGroup = primaryGroup)
			dfIter = rbind(dfIter, data.frame(w2p1$chutesIniciais, parcela=pkInstance, tipo = "W2P"))
		}

	}
	w2pte1 = NA
	if (functions[[2]] == TRUE) {
		w2pte1 = weibull(values, te=leftTrunc, amplitude=amp, maxIT=pmaxIT, verbose = verbose)
		if (w2pte1$iteracoes > 0) {
			dfWeibulls2p = rbind(dfWeibulls2p, data.frame(parametros(w2pte1), parcela=pkInstance))
			grava("_2-W2P_TE", w2pte1, pkInstance, restrValue, pasta=folder, primaryGroup = primaryGroup)
			dfIter = rbind(dfIter, data.frame(w2pte1$chutesIniciais, parcela=pkInstance, tipo = "W2PTE"))
		}
	}

	w2ptd1 = NA
	if (functions[[3]] == TRUE) {
		w2ptd1 = weibull(values, td=max(defineClasses(values, 2)$classe[,amp]), amplitude=amp, maxIT=pmaxIT, verbose = verbose)
		if (w2ptd1$iteracoes > 0) {
			dfWeibulls2p = rbind(dfWeibulls2p, data.frame(parametros(w2ptd1), parcela=pkInstance))
			grava("_3-W2P_TD", w2ptd1, pkInstance, restrValue, pasta=folder, primaryGroup = primaryGroup)
			dfIter = rbind(dfIter, data.frame(w2ptd1$chutesIniciais, parcela=pkInstance, tipo = "W2PTD"))
		}
	}

	w2ptetd1 = NA
	if (functions[[4]] == TRUE) {
		w2ptetd1 = weibull(values, te=leftTrunc, td=max(defineClasses(values, 2)$classe[,amp]), amplitude=amp, maxIT=pmaxIT, verbose = verbose)
		if (w2ptetd1$iteracoes > 0) {
			dfWeibulls2p = rbind(dfWeibulls2p, data.frame(parametros(w2ptetd1), parcela=pkInstance))
			grava("_4-W2P_TETD", w2ptetd1, pkInstance, restrValue, pasta=folder, primaryGroup = primaryGroup)
			dfIter = rbind(dfIter, data.frame(w2ptetd1$chutesIniciais, parcela=pkInstance, tipo = "W2PTETD"))
		}
	}

	if (nrow(dfWeibulls2p)>0) {
		dfWeibulls2p$nome = lapply(dfWeibulls2p$nome, as.character)
		dfWeibulls2p$nome = unlist(dfWeibulls2p$nome)


		dfWeibulls2p = sqldf("SELECT * from dfWeibulls2p ORDER BY KS DESC")

		if(verbose)
			print(xtable(dfWeibulls2p,digits=rep(4, dim(dfWeibulls2p)[2]+1), include.rownames=FALSE), type="html", file=paste(folder,"Parcela_", primaryGroup, "_", gsub(".", "", restrValue, fixed = TRUE), "_", "_5-Resumo Weibull 2p.html", sep=""), html.table.attributes=list("border='1' bordercolor=black cellpadding='0' cellspacing='0'"))


	}

	# ------ WEIBULL 3P-----------------------------------------------
	if(verbose)
		cat(paste("\n\n"))
	dfWeibulls3p = data.frame()


	w3p1 = NA
	if (functions[[5]] == TRUE) {
		w3p1 = weibull3(values, amplitude=amp, maxIT=pmaxIT, verbose = verbose)
		if (nrow(w3p1$parametros)>0 && w3p1$iteracoes > 0) {
		  dfWeibulls3p = rbind(data.frame(parametros(w3p1), parcela=pkInstance))
		  if (w3p1$locacao > 1) {
			grava("+++_1-W3P", w3p1, pkInstance, restrValue, pasta=folder, primaryGroup = primaryGroup)
		  } else {
			grava("_1-W3P", w3p1, pkInstance, restrValue, pasta=folder, primaryGroup = primaryGroup)
		  }
		  dfIter = rbind(dfIter, data.frame(w3p1$chutesIniciais, parcela=pkInstance, tipo = "W3P"))
		}
	}

	w3pte1 = NA
	if (functions[[6]] == TRUE) {
		w3pte1 = weibull3(values, te=leftTrunc, amplitude=amp, maxIT=pmaxIT, verbose = verbose)
		if (nrow(w3pte1$parametros)>0 && w3pte1$iteracoes > 0) {
		  dfWeibulls3p = rbind(dfWeibulls3p, data.frame(parametros(w3pte1), parcela=pkInstance))
		  if (w3pte1$locacao > 1) {
			grava("+++_2-W3P_TE", w3pte1, pkInstance, restrValue, pasta=folder, primaryGroup = primaryGroup)
		  } else {
			grava("_2-W3P_TE", w3pte1, pkInstance, restrValue, pasta=folder, primaryGroup = primaryGroup)
		  }
		  dfIter = rbind(dfIter, data.frame(w3pte1$chutesIniciais, parcela=pkInstance, tipo = "W3PTE"))
		}
	}
	w3ptd1 = NA
	if (functions[[7]] == TRUE) {
		w3ptd1 = weibull3(values, td=max(defineClasses(values, 2)$classe[,amp]), amplitude=amp, maxIT=pmaxIT, verbose = verbose)
		if (nrow(w3ptd1$parametros)>0 && w3ptd1$iteracoes > 0) {
		  dfWeibulls3p = rbind(dfWeibulls3p, data.frame(parametros(w3ptd1), parcela=pkInstance))
		  if (w3ptd1$locacao > 1) {
			grava("+++_3-W3P_TD", w3ptd1, pkInstance, restrValue, pasta=folder, primaryGroup = primaryGroup)
		  } else {
			grava("_3-W3P_TD", w3ptd1, pkInstance, restrValue, pasta=folder, primaryGroup = primaryGroup)
		  }
		  dfIter = rbind(dfIter, data.frame(w3ptd1$chutesIniciais, parcela=pkInstance, tipo = "W3PTD"))
		}
	}

	w3ptetd1 = NA
	if (functions[[8]] == TRUE) {
		w3ptetd1 = weibull3(values, te=leftTrunc, td=max(defineClasses(values, 2)$classe[,amp]), amplitude=amp, maxIT=pmaxIT, verbose = verbose)
		if (nrow(w3ptetd1$parametros)>0 && w3ptetd1$iteracoes > 0) {
		  dfWeibulls3p = rbind(dfWeibulls3p, data.frame(parametros(w3ptetd1), parcela=pkInstance))
		  if (w3ptetd1$locacao > 1) {
			grava("+++_4-W3P_TETD", w3ptetd1, pkInstance, restrValue, pasta=folder, primaryGroup = primaryGroup)
		  } else {
			grava("_4-W3P_TETD", w3ptetd1, pkInstance, restrValue, pasta=folder, primaryGroup = primaryGroup)
		  }
		  dfIter = rbind(dfIter, data.frame(w3ptetd1$chutesIniciais, parcela=pkInstance, tipo = "W3PTETD"))
		}
	}

	# ------------------------------------------------------------

	if (nrow(dfWeibulls3p)>0) {
		dfWeibulls3p$nome = lapply(dfWeibulls3p$nome, as.character)
		dfWeibulls3p$nome = unlist(dfWeibulls3p$nome)

		dfWeibulls3p = sqldf("SELECT * from dfWeibulls3p ORDER BY KS DESC")

		if(verbose)
			print(xtable(dfWeibulls3p,digits=rep(4, dim(dfWeibulls3p)[2]+1), include.rownames=FALSE), type="html", file=paste(folder,"Parcela_", primaryGroup, "_", gsub(".", "", restrValue, fixed = TRUE), "_", "_5-Resumo Weibull 3p.html", sep=""), html.table.attributes=list("border='1' bordercolor=black cellpadding='0' cellspacing='0'"))


	}

	dfTemp = data.frame()

	#Parcela
	dfParcela = rbind(dfWeibulls2p, dfWeibulls3p)
	if (nrow(dfParcela)>0){
		dfParcela = sqldf("SELECT * from dfParcela order by KS ASC")

		if(verbose)
		  print(xtable(dfParcela,digits=rep(4, dim(dfParcela)[2]+1), include.rownames=FALSE), type="html", file=paste(folder,"Parcela_", primaryGroup, "_", gsub(".", "", restrValue, fixed = TRUE), "_", "_6-Resumo Weibull 2p e 3p.html", sep=""), html.table.attributes=list("border='1' bordercolor=black cellpadding='0' cellspacing='0'"))

		if (nrow(dfWeibulls2p) > 0) {
			dfTemp = rbind(dfTemp, sqldf("SELECT * from dfWeibulls2p ORDER by KS ASC LIMIT 1"))
		}
		if (nrow(dfWeibulls3p) > 0) {
			dfTemp = rbind(dfTemp, sqldf("SELECT * from dfWeibulls3p ORDER by KS ASC LIMIT 1"))
		}

		if (nrow(dfTemp) > 0) {
			dfBest = rbind(dfBest, sqldf("SELECT * from dfTemp ORDER by KS ASC LIMIT 1"))
		}

		dfAll = rbind(dfAll, dfParcela)
	}

	tryCatch({
	  if(verbose)
			cat("\n\n\nSaving Results")
	  if(is.na(folder)) {
	    if (nrow(dfAll)>0) write.table(dfAll, file=tempfile(fileext = ".csv"), sep=";", dec=",",row.names=FALSE)
	    if (nrow(dfBest)>0) write.table(dfBest, file=tempfile(fileext = ".csv"), sep=";", dec=",",row.names=FALSE)
	  } else {
	    if (nrow(dfAll)>0) write.table(dfAll, file=paste(folder,"result_", gsub(".", "", restrValue, fixed = TRUE), ".csv", sep=""), sep=";", dec=",",row.names=FALSE)
	    if (nrow(dfBest)>0) write.table(dfBest, file=paste(folder,"best_", gsub(".", "", restrValue, fixed = TRUE), ".csv", sep=""), sep=";", dec=",",row.names=FALSE)
	   }

	}, error = function(err) {
	   print(paste("Error:  ",err))
	})

	tryCatch({
    if(is.na(folder)) {
      if (nrow(dfIter)>0) write.table(dfIter, file=tempfile(fileext = ".csv"), sep=";", dec=",", row.names=FALSE)
    } else {
      if (nrow(dfIter)>0) write.table(dfIter, file=paste(folder,"tries_", gsub(".", "", restrValue, fixed = TRUE), ".csv", sep=""), sep=";", dec=",", row.names=FALSE)
    }


	}, error = function(err) {
	   print(paste("Error:  ",err))
	})

	}

	# if(updateDB) {
	# 	dfMDDTemp=sqldf(paste0("SELECT * from ",deparse(substitute(dataFrame))))
	#
	# 	melhoresResultados = paste0(folder,"melhores_",restrValue,".csv")
	# 	dfMelhoresWeibull = read.csv2(melhoresResultados)
	# 	dfMelhoresWeibull = cbind(dfMelhoresWeibull,idadearred=restrValue)
	#
	# 	camposAtualizar=c("forma","escala","locacao")
	# 	campochaves=c(primaryGroup,secondaryGroup)
	#
	# 	dfMDDTemp=atualizaCampoBase(camposAtualizar,dfMelhoresWeibull,dfMDDTemp,campochaves)
	# 	dfMDDTemp=data.frame(dfMDDTemp)
	# 	return(dfMDDTemp)
	# }

	# Definição de Retorno:
	return(dfBest)
}

grava = function(tipo, resultado, parcela, idade, pasta, primaryGroup) {

  if(is.na(pasta)) {
    arquivo = tempdir()
  } else {
	arquivo = paste(pasta,primaryGroup,"_", parcela, "_", gsub(".", "", idade, fixed = TRUE), "_", tipo, sep="")
	arquivoAcumulado = paste(pasta,primaryGroup,"Accumulated_", parcela, "_", gsub(".", "", idade, fixed = TRUE), "_", tipo, sep="")
  }

	if (!is.null(resultado)) {
	##win.metafile(paste(arquivo, ".wmf",sep=""))
	png(paste(arquivo, ".png",sep=""))
	#Plota o histograma
	plot(resultado, main ="")
	dev.off()
	##win.metafile(paste(arquivoAcumulado, ".wmf",sep=""))
	png(paste(arquivo, ".png",sep=""))
	#Plota o grafico da CDF
	plot(resultado, main ="", acumulado = TRUE)
	dev.off()

	capture.output(resultado, file=paste(arquivo, ".txt",sep=""))
	}
}


