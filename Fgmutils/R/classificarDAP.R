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

##' @title classify field dap
##' @description classify field dap as specified amplitude and includes a few fields
##' @param inventario the database to update
##' @param amplitude it is amplitude of dap class
##' @param verbose use TRUE to show status of process
##' @return data.frame with classeDAP field and other
##' @import sqldf
##' @export
classificarDAP <- function(inventario, amplitude = 1, verbose = FALSE){

  if (verbose) print("buscando  os campos necessarios...")

  dfBrutos <- inventario[,c("projeto", "talhao", "parcela", "fila", "cova", "fuste", "idade",  "idadearred", "dap", "volume", "NHa")]

  if (verbose) if (verbose) print("gerando idetificador...")
  dfBrutos$cod_id = paste0(dfBrutos$projeto, "_", dfBrutos$talhao, "_",  dfBrutos$parcela, "_", dfBrutos$fila, "_", dfBrutos$cova, "_", dfBrutos$fuste)

  if (verbose) print("adicionando coluna classeDAP...")
  dfBrutos <- transform(dfBrutos, classeDAP = -999, N = -999, NCLASSES = -999, VolumeTotal = -999, classeDAPpriMed = -999, NhaClasse=-999)

  if (verbose) print("criando base de classes...")
  base <- sqldf("SELECT COUNT(*) AS N, MIN(dap) AS limiteMin, MAX(dap) AS limiteMax, sum(volume) AS VolumeTotal, idadearred, parcela FROM dfBrutos GROUP BY idadearred, parcela ORDER BY parcela")

  if (verbose) print("definindo classes possiveis...")
  classes <- getClasses(base, amplitude, verbose)

  if (verbose) print("classificando tuplas da base de dados")
  erros = 0;
  linhas = nrow(dfBrutos)

  for(i in 1:linhas){

    if (verbose) print("setando volume")
    #dfBrutos$VolumeTotal[i] = dfVolumes$VolumeTotal[dfBrutos$parcela[i] == dfVolumes$parcela]
    dfBrutos$VolumeTotal[i] = base$VolumeTotal[base$idadearred == dfBrutos$idadearred[i] & base$parcela == dfBrutos$parcela[i]]
    if (verbose) print("preparando index")
    index = -1;
    if (verbose) print ("gerando comando")
    cmd = paste0("index = ", rownames(base[base$idadearred == dfBrutos$idadearred[i] & base$parcela == dfBrutos$parcela[i],]))
    if (verbose) print("executando expressao")
    eval(parse(text = (cmd)))
    if (verbose) print("aplicando classe")
    if (index > 0){
      dfBrutos$N[i] = base[index,"N"]
      if (verbose) print("adquirindo dataframe")
      df = as.data.frame(classes[[index]])
      if (verbose) print(paste0("classificando dap para ", i))
      classe = classificaClasseDAP(df, dfBrutos$dap[i])
      if (classe >= 0)
      {
        if (verbose) print("setando classe")
        dfBrutos$classeDAP[i] = classe
        if (verbose) print("incrementando contador")
        classes[[index]]$NCLASSES[classes[[index]]$centro == classe] =  classes[[index]]$NCLASSES[classes[[index]]$centro == classe] + 1
        classes[[index]]$NhaClasse[classes[[index]]$centro == classe] =  classes[[index]]$NhaClasse[classes[[index]]$centro == classe] + dfBrutos$NHa[i]
        if (verbose) print(paste0("classificou ", i, " de ", linhas, " indice ", index, " como ", classe, ". ", erros, " erros."))
      }
      else
      {
        print(paste0("erro retorno ", classe))
        erros = erros + 1
      }
    }
    else{
      print(paste0("erro indice ", i, " nao encontrado"))
      erros = erros + 1
    }
  }

  if (verbose) print("obtendo classe dap da primeira medicao...")
  dfDAP = sqldf("SELECT cod_id, classeDAP, MIN(idade) FROM dfBrutos GROUP BY cod_id")
  #dfBrutos$classeDAPpriMed = dfDAP$classeDAP[dfBrutos$cod_id == dfDAP$cod_id]
  if (verbose) print("atribuindo dap da primeira medicao...")
  for(i in 1:linhas)
  {dfBrutos$classeDAPpriMed[i] = dfDAP$classeDAP[dfDAP$cod_id == dfBrutos$cod_id[i]]}

  if (erros < 1){
    print("base classificada com sucesso!")
  }
  else
    print(paste0("ocorreram ", erros, " durante a classificacao"))

  erros2 = 0
  if (verbose) print("quantificando NCLASSES & NhaClasse...")
  for(i in 1:linhas){
    index = -1;
    eval(parse(text = (paste0("index = ", rownames(base[base$idadearred == dfBrutos$idadearred[i] & base$parcela == dfBrutos$parcela[i],])))))

    if (index > 0
        && ((X = classificaClasseDAP(as.data.frame(classes[[index]]), dfBrutos$dap[i], getNCLASSES = TRUE)) > 0)
        && ((Y = classificaClasseDAP(as.data.frame(classes[[index]]), dfBrutos$dap[i], getNhaClasse = TRUE)) > 0)){
      dfBrutos$NCLASSES[i] = X
      dfBrutos$NhaClasse[i] = Y
      if (verbose) print(paste0("quantificou ", i, " em ", X, " e ", Y, " restando ", (linhas - i), ". ", erros2, " erros."))
    }
    else{
      print(paste0("erro indice ", i, " nao encontrado para ", nrow(classes)))
      erros2 = erros2 + 1
    }
  }

  if (erros2 < 1){
    print("classes quantificadas com sucesso!")
  }
  else
    print(paste0("ocorreram ", erros2, " durante a quantificacao"))

  if (verbose) print("calculando PROBABLILIDADE por classes...")
  dfBrutos$PROBABILIDADE = dfBrutos$NCLASSES / dfBrutos$N

  if (verbose) print("calculando Volume por classes...")
  dfBrutos$VolumeClasse =  dfBrutos$PROBABILIDADE * dfBrutos$VolumeTotal

  if (verbose) print(paste0("encerrou com ", erros + erros2, " erros."))

  return(dfBrutos)
}
