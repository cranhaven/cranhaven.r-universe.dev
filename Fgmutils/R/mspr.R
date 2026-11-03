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

#' @title mspr
#' @description average square of the prediction errors .
#' @param observados vector of values observed.
#' @param estimados vector of regression model data.
#' @param nValidacao number of cases in the validation data set.
#' @references JESUS, S. C.; MIURA, A. K. Analise de regressao linear multipla para estimativa do indice de vegetacao melhorado (EVI) a partir das bandas 3 4 e 5 do sensor TM/Landsat 5. In: SIMPOSIO BRASILEIRO DE SENSORIAMENTO REMOTO, 14. (SBSR), 2009, Natal. Anais... Sao Jose dos Campos: INPE, 2009. p. 1103-1110. DVD, On-line. ISBN 978-85-17-00044-7. (INPE-15901-PRE/10511)
#' @export
mspr <- function(observados, estimados, nValidacao)
{
  sum((observados-estimados))^2/nValidacao
}
