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

##' @title evaluates Volume Advanced
##' @description this function performs an assessment of estimates of a variable as the forcefulness with expected
##' @param base data.frame with data
##' @param mapeamento name of field eight and diameter
##' @param modelos list of exclusive for base models
##' @param salvar list of param to save the files
##' @param graficos list of param to plot graphics
##' @param estatisticas list of param to caclc estatistics
##' @param forcePredict force the calculation without using predict?
##' @param dividirEm how divide the base in training and validation
##' @param percentualDeTreino how many percent will stay in the training group?
##' @param agruparPor name field of base is group of individuals
##' @param fnCalculaVolume list of estatistics results
##' @return will be returned a result of statistics and ranking of volume
##' @import gridExtra
##' @export
avaliaVolumeAvancado <- function(base, mapeamento= list(dap1="dap1", dap2="dap2", ht1="ht1", ht2="ht2"), modelos=NULL, salvar = NULL, graficos = NULL, estatisticas = NULL, forcePredict = F, dividirEm = "parcela", percentualDeTreino = 0.7, agruparPor="parcela", fnCalculaVolume = calculaVolumeDefault){
  if (is.null(modelos))
    stop("informe modelos a serem avaliados vide ITGM::getModelosLiteraturaExclusivos()")
  save = dirDAP = dirHT = NULL
  if (!is.null(salvar)) save = salvar$diretorio
  if (!is.null(salvar) && !is.null(salvar$diretorioDAP)) dirDAP= paste0(salvar$diretorio, salvar$diretorioDAP)
  if (!is.null(salvar) && !is.null(salvar$diretorioHT)) dirHT=paste0(salvar$diretorio, salvar$diretorioHT)

  ##pegar na base apenas as colunas que vamos usar
  colunas = c(dividirEm, agruparPor)
  for(i in 1:length(modelos))
    colunas = union(colunas, getColumnsOfBase(base, modelos[[i]]()[[2]]))
  if(length((diff = intersect(colunas, c("b0", "b1", "b2", "b3", "b4","b5","b6", "b7", "b8", "b9")))) > 0)
    stop(paste0("A base contem campos com nomes suspeitos: <", toString(diff), "> sao coeficientes de algum modelo? sugest: renomeie-os"))
  bw = list()
  eval(parse(text = paste0("bw$", union(colunas, mapeamento), " = base$", union(colunas, mapeamento))))
  baseW = as.data.frame(bw)

  ##separar a base em treino e validacao
  dftv = separaDados(baseW, dividirEm, percTraining = percentualDeTreino)
  baseTreino = dftv$treino
  baseValidacao = dftv$validacao

  ranking = data.frame(b0 = double(), b1 = double(), rankingB0  = double(), rankingB1 = double())
  dfestatisticas = list()
  nomes = list()
  volumesPreditos = data.frame(k = baseValidacao[, agruparPor])
  names(volumesPreditos)[[1]] = agruparPor

  ##amarrado a 2 funcoes de estatistica CV e R2 :(
  estatisticasV = estatisticas
  estatisticasV$baseDoAjuste=baseValidacao

  ###graficos
  if (!is.null(graficos)){
    rw = length(modelos)
    cl = 3 * length(graficos$funcoes)
    par(mfrow=c(rw, cl))
    nms = c("g1", "g2", "g3", "g4", "g5", "g6","g7", "g8", "g9", "g10","g11", "g12", "g13", "g14","g15")
    j=1
  }

  for (i in 1:length(modelos)) {
    modelo = modelos[[i]]
    estatisticasV$formula = modelo()[[2]]
    nomes[i] = gsub("\\s", "", modelo()[[1]], perl = T)

    grafic1 = grafic2 = grafic3 = graficos
    if (!is.null(graficos)){
      grafic1$save = paste0(dirDAP, nomes[i], "/" )
      grafic2$save = paste0(dirHT, nomes[i], "/" )
      grafic3$save = paste0(save, nomes[i], "/" )
      grafic1$titulo = grafic1$nome = paste0( nomes[i], " DAP Ajuste" )
      grafic2$titulo = grafic2$nome =  paste0( nomes[i], " HT Ajuste" )
      grafic3$titulo = grafic3$nome =  paste0( nomes[i], " Volume Validacao" )
      grafic1$nomeParaExibir = nms[[j]]
      grafic2$nomeParaExibir = nms[[j+1]]
      grafic3$nomeParaExibir = nms[[j+2]]
      j = j+3
    }

    estatistica = avaliaEstimativas(
      observado = fnCalculaVolume( dap = baseValidacao$dap2, ht = baseValidacao$ht2 ),
      estimado = fnCalculaVolume(
        dap = predizer( ajuste = avaliaEstimativas( observado =  baseTreino$dap2, ajuste = modelo(y1=mapeamento$dap1, y2=mapeamento$dap2, base = baseTreino), graficos = grafic1, estatisticas = estatisticas, salvarEm = paste0(dirDAP, nomes[i], "/" ), nome = paste0("ajuste DAP ", nomes[i]))$ajuste, newdata = baseValidacao, force = forcePredict),
        ht = predizer( ajuste = avaliaEstimativas( observado =  baseTreino$ht2, ajuste = modelo(y1=mapeamento$ht1, y2=mapeamento$ht2, base = baseTreino), graficos = grafic2, estatisticas = estatisticas, salvarEm = paste0(dirHT, nomes[i], "/" ), nome = paste0("ajuste HT ", nomes[i]))$ajuste, newdata = baseValidacao, force = forcePredict) ),
      graficos = grafic3,
      estatisticas = estatisticasV,
      salvarEm = paste0(save, nomes[i], "/" ),
      nome = paste0("validacao Volume ", nomes[i])
    )

    estatistica$estatisticas$estatisticas = cbind(baseValidacao, estatistica$estatisticas$estatisticas)
    eval(parse(text=paste0("dfestatisticas$", nomes[i], " = estatistica")))
    eval(parse(text=paste0("volumesPreditos$", nomes[i], " = estatistica$estimado")))
    ranking = rbind(ranking, estatistica$ranking)
  }

  rownames(ranking) = nomes
  ranking = ranking[ order( ranking$rankingB0, ranking$rankingB1),]
  ranking$rank = list(1:nrow(ranking))[[1]]
  rank = ranking[,c("rank", "b0", "b1")]
  if (!is.null(salvar))
    capture.output(rank, file = paste0(salvar$diretorio, " rank Test F.txt"))
  print(rank)

  ##totalizando
  str = paste0("SELECT ", agruparPor, ", SUM(", nomes[1], ") AS '", nomes[1], "'")
  if ( 2 <= length(nomes))
    for(i in 2:length(nomes))
      str = paste0(str, ", SUM(", nomes[i], ") AS '", nomes[i], "'")
  str = paste0(str, "from volumesPreditos GROUP BY ", agruparPor)
  print("agrupando...")
  volumesPreditos = sqldf(str)
  print("atribuindo...")
  data = atualizaCampoBase(camposAtualizar = setdiff(names(volumesPreditos), agruparPor), baseAgrupada = volumesPreditos, baseAtualizar = baseW, keys = agruparPor)


  ##graficos
  if (!is.null(graficos)){
    par(mfrow=c(1, 1))
    for(i in 1:length(graficos$funcoes))
      if(("getggplot2GraphicObservadoXEstimado" %in% ls(envir = as.environment(1))) && (all.equal(graficos$funcoes[[i]], getggplot2GraphicObservadoXEstimado) == T)){
        eval(parse(text="grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14, g15, ncol=3, nrow=5)"))
      }
  }

  return(list(estatisticas = dfestatisticas, base= data, ranking = rank))
}
