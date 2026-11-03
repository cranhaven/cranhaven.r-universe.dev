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

#' @title classifica Classe DAP
#' @description the center of the class that the DAP belongs.
#' @param dfClassesDAP a frequency distribution with the attributes $classe and $centro
#' @param dap integer Diameter at breast height
#' @param  getNhaClasse get NhaClasse field of dfClassesDAP, default false
#' @param  getNCLASSES get NCLASSES field of dfClassesDAP, default false
#' @examples
#' dados = defineClasses(1, 10, 2, getDataFrame = TRUE)
#' classificaClasseDAP(dados,7)
#' @export
classificaClasseDAP <- function(dfClassesDAP, dap, getNhaClasse = FALSE, getNCLASSES = FALSE) {
  linhas = nrow(dfClassesDAP)
  if(linhas == 1 && ((dap >= dfClassesDAP[1,]$linf) && (dap <= dfClassesDAP[1,]$lsup))){
    if (getNCLASSES == TRUE){
      return (dfClassesDAP[1, ]$NCLASSES)
    }
    if (getNhaClasse == TRUE){
      return (dfClassesDAP[1, ]$NhaClasse)
    }
    return(dfClassesDAP[1,]$centro)
  }
  if (linhas > 1){
    for (i in 1:linhas) {
      if (dap >= dfClassesDAP[i,]$linf && (dap < dfClassesDAP[i,]$lsup || (i == linhas && dap <= dfClassesDAP[i,]$lsup))) {
        if (getNCLASSES == TRUE){
          return (dfClassesDAP[i, ]$NCLASSES)
        }
        if (getNhaClasse == TRUE){
          return (dfClassesDAP[i, ]$NhaClasse)
        }
        return(dfClassesDAP[i,]$centro)
      }
    }
  }
  print(paste0('impossivel classificar dap ', dap, ' para ', dfClassesDAP[1,]$linf, ' A ', dfClassesDAP[linhas,]$lsup))
  return(-1)
}
