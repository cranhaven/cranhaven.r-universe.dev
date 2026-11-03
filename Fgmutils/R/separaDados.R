## Copyright (C) 2016  Clayton Vieira Fraga Filho
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## as published by the Free Software Foundation; either version 2
## of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

##' @title Data Separates
##' @description divides the dataFrame as the percentage defined in percTraining enabling apply and measure the performance of the regression equation.
##' @param dataFrame source of data
##' @param fieldName column of dataFrame that will be applied regression
##' @param percTraining percentage that will be reserved for training (default 0.70)
##' @param seed integer that determines how the sample is randomly chosen (default NULL)
##' @import sqldf
##' @export
separaDados <- function(dataFrame, fieldName, percTraining=0.70, seed=NULL) {

  if (length(fieldName) > 1) {
    stop("fieldName nao pode ser um vetor, informe apenas um nome de campo!")
  }

  if (!is.null(seed)) set.seed(seed)

  if (percTraining<=0.01 || percTraining>1 || is.na(percTraining) || is.null(percTraining)) {
    stop("Informe um percTraining entre 0.01 e 1")
  }

  nomeDF = toString(substitute(dataFrame))
  #	print(nomeDF)

  str_sql_individuos = paste0("SELECT distinct ",fieldName," from dataFrame group by ",fieldName)
  dfIndividuos = sqldf(str_sql_individuos)

  indice <- 1:nrow(dfIndividuos)
  tamanho = floor(length(indice)*percTraining)
  indiceTreino <- sample(indice, size=as.integer(tamanho))

  dfIndividuos_treino = NULL;

  eval(parse(text=paste0("dfIndividuos_treino = data.frame(",fieldName,
                         "=dfIndividuos[indiceTreino,])")))

  dfIndividuos_validacao = NULL;
  eval(parse(text=paste0("dfIndividuos_validacao = data.frame(",fieldName,"
                         =dfIndividuos[-indiceTreino,])")))
  cat(paste0("\nTotal de individuos(",fieldName,"): ", nrow(dfIndividuos)))
  cat(paste0("\nTotal para Ajuste/Treino: ", nrow(dfIndividuos_treino), " (",round((nrow(dfIndividuos_treino)/nrow(dfIndividuos))*100, 2),"%)"))
  cat(paste0("\nTotal para Teste: ", nrow(dfIndividuos_validacao), " (",round((nrow(dfIndividuos_validacao)/nrow(dfIndividuos))*100, 2),"%)"))

  percentual = (nrow(dfIndividuos_validacao)/nrow(dfIndividuos))

  str_SQL_treino = paste0("SELECT *
						  from dataFrame
						  where ",fieldName,"  in (SELECT distinct ",fieldName,"
							  from dfIndividuos_treino)")

  dataFrame_treino = sqldf(str_SQL_treino)

  str_SQL_validacao = paste0("SELECT *
						 from dataFrame
						  where  ",fieldName,"   in (SELECT distinct  ",fieldName,"
						  from dfIndividuos_validacao)")

  dataFrame_validacao = sqldf(str_SQL_validacao)

  percentualResult = list()
  percentualResult$validacao = round(percentual, 2)
  percentualResult$treino = 1-percentualResult$validacao
  cat("\nConcluido\n")
  individuos = list()
  individuos$treino = dfIndividuos_treino
  individuos$validacao = dfIndividuos_validacao

  return(list(individuos = individuos,
              nroIndividuos=nrow(dfIndividuos),
              percentual =percentualResult,
              validacao=dataFrame_validacao,
              treino=dataFrame_treino
  )
  )
}
