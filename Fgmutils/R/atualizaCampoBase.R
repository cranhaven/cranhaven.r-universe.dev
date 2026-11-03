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

##' @title updated base field
##' @description this function update certain fields in a dataframe, based on the provided key
##' @param camposAtualizar is the vector you want to update
##' @param baseAgrupada It is the database that contains the data you want to update on dataframe
##' @param baseAtualizar It is dataframe that you want to change fields
##' @param keys are the keys of the table that will be used in the compare
##' @param verbose default false
##' @return baseAtualizar with the updated fields according to baseAgrupada
##' @import data.table
##' @export
atualizaCampoBase <- function (camposAtualizar, baseAgrupada, baseAtualizar, keys, verbose=FALSE){
  ini = Sys.time()
  if(verbose) print("Picking up the columns of the database to be updated.")
  baseAtualizar = data.table(baseAtualizar)  # Base to be updated.
  baseAgrupada = data.table(baseAgrupada) # Base with grouped data

  if(verbose) print("check that the column already exists in the base, failing that, will create")
  if(verbose) print("Putting the fields of update of both tables in the same type")
  dtType = data.table(
    campos = camposAtualizar,
    tipo = (sapply(baseAgrupada[, camposAtualizar, with = FALSE], class)))
  tipos = dtType$tipo[ dtType$tipo %in% c("factor", "character", "integer", "logical", "numeric")]
  for(i in 1:length(tipos))
    eval(parse(text = paste0("baseAtualizar[, dtType[tipo == tipos[[i]]]$campos := as.",  tipos[[i]], "(-999)]")))

  if(verbose) print("Putting the keys in the same type")
  diferentes = unique(data.table(
    campos = keys,
    diferentes = (sapply(baseAgrupada[, keys, with = FALSE], class)
                  != sapply(baseAtualizar[, keys, with = FALSE], class))
  )[diferentes %in% c(TRUE, T)]$campos)
  if(length(diferentes) > 0){
    baseAgrupada[,diferentes] <- sapply(baseAgrupada[,diferentes, with = FALSE], as.character)
    baseAtualizar[,diferentes] <- sapply(baseAtualizar[,diferentes, with = FALSE], as.character)
  }

  setkeyv(baseAtualizar, keys)

  for (i in 1:nrow(baseAgrupada)) {
    if (verbose) cat(".")
    for (j in 1:length(camposAtualizar))
      eval(parse(text = paste0(
        "baseAtualizar[baseAgrupada[i,keys, with = FALSE], ", camposAtualizar[[j]],
        ":= (baseAgrupada[i, ", camposAtualizar[[j]], "])]")))
  }

  if (verbose) cat("\n")
  if (verbose) print(paste0("elapsed ", round(as.numeric(Sys.time() - ini), 2), " sgs"))
  remove(ini, dtType,tipos, diferentes)
  return (baseAtualizar[])
}
