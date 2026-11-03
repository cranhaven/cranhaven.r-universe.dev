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

##' @title avalia Volume Age Based
##' @description this function evaluate volume based on ages
##' @param base the data base
##' @param firstAge the first age to eval
##' @param lastAge the last age to eval
##' @param models list of exclusive for base models
##' @param mapper mapper from labels of fields volume, dap, ht
##' @param groupBy name field of base is group of individuals
##' @param save list of param to save the files
##' @param percTraining percentage that will be reserved for training (default 0.70)
##' @param paramEstatisticsDAP parameters to pass to function 'fnAvaliaEstimativas'
##' @param paramEstatisticsHT analogous to paramEstatisticsDAP
##' @param paramEstatisticsVolume analogous to paramEstatisticsDAP
##' @param plot is list of plots to function roundAges
##' @param ageER regex used to discover age in names from dataframe in listOfdata
##' @param ageRound synchronize begin of ages with an age? what age?
##' @param ageInYears  ages are in year?
##' @param forcePredict force the calculation without using predict?
##' @return will be returned a list of round ages
##' @export
avaliaVolumeAgeBased <- function(base, firstAge, lastAge, models,
                                 mapper= list(age1="idade1", age2="idade2", dap1="dap1", dap2="dap2", dap2est = "dap2est", ht1="ht1", ht2="ht2", ht2est = "ht2est", volume1="volume1", volume2="volume2", volume2est = "volume2est"),
                                 groupBy = "parcela", save = NULL, percTraining = 0.7, paramEstatisticsDAP, paramEstatisticsHT, paramEstatisticsVolume, plot = "parcela", ageER = "^.*_", ageRound = NaN, ageInYears = F, forcePredict = F){

  colunas = c(groupBy, plot)
  for (i in 1:length(models))
    colunas = union(colunas, getColumnsOfBase(base, models[[i]]()[[2]]))
  if (length((diff = intersect(colunas, c("b0", "b1", "b2",
                                          "b3", "b4", "b5", "b6", "b7", "b8", "b9")))) > 0)
    stop(paste0("this base contains fields with suspicious names: <",
                toString(diff), "> are coefficients from any model? sugest: rename then"))

  salvar = dirDAP = dirHT = NULL
  if (!is.null(save))
    salvar = save$diretorio
  if (!is.null(save) && !is.null(save$diretorioDAP))
    dirDAP = paste0(save$diretorio, save$diretorioDAP)
  if (!is.null(save) && !is.null(save$diretorioHT))
    dirHT = paste0(save$diretorio, save$diretorioHT)

  bw = list()
  eval(parse(text = paste0("bw$", union(colunas, mapper), " = base$", union(colunas, mapper))))
  baseW = as.data.frame(bw)
  dftv = separaDados(baseW, mapper$age1, percTraining = percTraining)
  baseTreino = dftv$treino
  baseValidacao = dftv$validacao
  baseValidacao[, "idadearred"] = roundAge(plots = baseValidacao[, plot], ages = baseValidacao[, mapper$age1],
                                           firstAge = ageRound, inYears = ageInYears)
  mapper2 = mapper
  mapper2$age1 = "idadearred"
  paramEstatisticsVolume$estatisticas$baseDoAjuste = baseTreino
  retorno = list(base = base, baseTreino = dftv$treino, baseValidacao = dftv$validacao)

  ranking = data.frame(model  = character(), rankingB0 = double(), rankingB1 = double())

  for (i in 1:length(models)) {
    modelo = models[[i]]
    paramEstatisticsVolume$estatisticas$formulaDoAjuste = modelo()[[2]]
    nomeModelo = gsub("\\s", "", modelo()[[1]], perl = T)
    salvarEm = salvarDapEm = salvarHtEm = NULL
    if (!is.null(salvar))
      paramEstatisticsDAP$graficos$save = paramEstatisticsDAP$salvarEm = paste0(salvar, nomeModelo, "/")
    if (!is.null(dirDAP))
      paramEstatisticsHT$graficos$save = paramEstatisticsHT$salvarEm = paste0(dirDAP, nomeModelo, "/")
    if (!is.null(dirHT))
      paramEstatisticsVolume$graficos$save = paramEstatisticsVolume$salvarEm = paste0(dirHT, nomeModelo, "/")

    ajusteDAP = modelo(y1 = mapper$dap1, y2 = mapper$dap2, base = baseTreino)
    ajusteHT = modelo(y1 = mapper$ht1, y2 = mapper$ht2, base = baseTreino)

    paramEstatisticsDAP$ajuste = ajusteDAP
    paramEstatisticsHT$ajuste = ajusteHT

    avaliacao = evalAgeBased(
      listOfdata = projectBaseOriented(
        fitDAP=ajusteDAP,
        fitHT=ajusteHT,
        base = baseValidacao,
        mapper = mapper2,
        forcePredict = forcePredict,
        firstAge = firstAge,
        lastAge = lastAge),
      mapper = mapper, ageER = ageER,
      paramEstatisticsDAP = paramEstatisticsDAP,
      paramEstatisticsHT = paramEstatisticsHT,
      paramEstatisticsVolume = paramEstatisticsVolume,
      nameModel = modelo()[[1]]
    )

    avaliacao$dap$fitDAP = ajusteDAP
    avaliacao$ht$fitHT = ajusteHT

    eval(parse(text = paste0("retorno$", nomeModelo, " = avaliacao")))
    ranking = rbind(ranking, data.frame(model = nomeModelo, rankingB0 = avaliacao$ranking$rankingB0, rankingB1 = avaliacao$ranking$rankingB1))
  }

  retorno$ranking = ranking[order(ranking$rankingB0, ranking$rankingB1), ]
  rownames(retorno$ranking) = seq(1:length(models))
  print(retorno$ranking)
  return (retorno)
}
