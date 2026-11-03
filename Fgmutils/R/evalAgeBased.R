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

##' @title Evaluate Age Based
##' @description This function evaluates the volume of past data frames based on the parameter 'listOfdata'
##' @param listOfdata the list that contains the data frames predicts
##' @param mapper mapper from labels of fields volume, dap, ht
##' @param fnAvaliaEstimativas funcion to evaluate dataframes of listOfdata
##' @param paramEstatisticsDAP parameters to pass to function 'fnAvaliaEstimativas'
##' @param paramEstatisticsHT analogous to paramEstatisticsDAP
##' @param paramEstatisticsVolume analogous to paramEstatisticsDAP
##' @param titulos customize titles of grafics
##' @param ageER regex used to discover age in names from dataframe in listOfdata
##' @param nameModel name of model used to predict to generate listOfdata optional
##' @return will be returned a list of round ages
##' @export
evalAgeBased <- function(listOfdata,
                         mapper = list(volume2 = "volume2", volume2est = "volume2est", dap2 = "dap2", dap2est = "dap2est", ht2 = "ht2", ht2est = "ht2est"),
                         fnAvaliaEstimativas = avaliaEstimativas,
                         paramEstatisticsDAP,  paramEstatisticsHT,  paramEstatisticsVolume,
                         titulos = "paste(\"Idade\", idade)", ageER = "^.*_", nameModel = NULL){

  retorno = list();

  rankB0 = 0
  rankB1 = 0

  for (j in 1:3) {
    b0 = b1 = 0

    ret = list()
    args = "observado = tabela[, mapper$dap2], estimado = tabela[,mapper$dap2est]"
    param = paramEstatisticsDAP
    tipo = "dap"
    if (j == 2){
      args = "observado = tabela[, mapper$ht2], estimado = tabela[,mapper$ht2est]"
      param = paramEstatisticsHT
      tipo = "ht"
    }
    if (j == 3){
      args = "observado = tabela[, mapper$volume2], estimado = tabela[,mapper$volume2est]"
      param = paramEstatisticsVolume
      tipo = "volume"
    }

    for (i in 1:length(labels(param)))
      args = paste0(args, ", ", labels(param)[[i]], " = param$", labels(param)[[i]])

    titulo = titulos
    if (!is.null(param$graficos$titulo))
      titulo = param$graficos$titulo

    if (length(listOfdata) < 1)
      stop("Not exists data to eval!")

    for (i in 1:length(listOfdata)) {

      idade = names(listOfdata[i])[[1]]
      tabela = eval(parse(text = paste0("listOfdata$", idade)))
      idade = gsub(ageER, "", idade)

      if (j == 1){
        if (i == 1)
          base = tabela
        else
          base = rbind(base, tabela)
      }


      eval(parse(text = paste0("param$graficos$titulo = ", titulo)))
      if(is.null(param$graficos$nome))
        param$graficos$nome = paste("grafico age", idade)

      res = eval(parse(text = paste0("fnAvaliaEstimativas(", args, ", nome = paste0(\"age \", idade))")))
      res$estatisticas$estatisticas = cbind(res$estatisticas$estatisticas, tabela)
      eval(parse(text = paste0("ret$Estatistics_Age_", idade, " = res")))
      b0 = b0 +  res$ranking$rankingB0
      b1 = b1 + res$ranking$rankingB1
    }
    ret$ranking = data.frame(rankingB0 = b0, rankingB1 = b1)
    eval(parse(text = paste0("retorno$", tipo," = ret")))
    rankB0 = rankB0 + b0
    rankB1 = rankB1 + b1
  }
  retorno$base = base
  retorno$ranking = data.frame(rankingB0 = rankB0, rankingB1 = rankB1)
  return(retorno)
}
