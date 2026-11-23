#' Analysing the quality of the estimation
#'
#' This function calculates the  root-mean-square deviation (RMSD), the Nash Sutchiffe Coefficient, the residual sum of squares (RSS) and the mean absolute percentage error (MAPE) for the matrices introduces. This function also calculates the mean of the RMSD, the mean of the Nash Sutchiffe Coefficient  and the mean of the RSS.
#'
#'
#'@param matrixData Matrix that contains at row i the bacterial taxa of bacteria i at the time points that we want take into account to calculate the quality control values.
#'@param matrixExpected Matrix that contains at row i the expected value of the bacterial taxa i at the time points that we want take into account to calculate the quality control values. The bacteria must be placed in the same order than in \code{matrixData}
#'@param names.especie Vector with the names of the bacteria in the same order that are placed in the \code{matrixData} matrix.
#'
#'@return Returns a data.frame.
#'
#'
#' @examples
#'
#'
#' names.especie=c("Bact1", "Bact2", "Bact3")
#' matrixExpected=matrix(c(1:9),3,3)
#' matrixData=matrixExpected+0.1
#'
#' QualityControl(matrixData, matrixExpected,names.especie)
#'
#'
#' @export
#'
#'
#'


#    CoDaLoMic. Compositional Models to Longitudinal Microbiome Data.
#    Copyright (C) 2024  Irene Creus Martí
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


QualityControl<-function(matrixData, matrixExpected,names.especie){


  if(dim( matrixExpected)[1]!=dim(matrixData)[1]){stop("The matrices do not have the same dimensions")}
  if(dim( matrixExpected)[2]!=dim(matrixData)[2]){stop("The matrices do not have the same dimensions")}

  times.points=dim(matrixData)[2]
  E=dim(matrixData)[1]

  RMSD=rep(0,E)
  for (k in 1:E){
    RMSD[k]=sqrt( sum((matrixExpected[k,]-matrixData[k,])^2)/times.points)
  } #Returns the RMSD of all the families

  mediaRMSD=mean(RMSD)


  media=rep(0,E)
  EE=rep(0,E)
  for (k in 1:E){
    media[k]=(1/times.points)*sum(matrixData[k,])
  }
  for (k in 1:E){
    EE[k]=1-(sum((matrixData[k,]-matrixExpected[k,])^2)/(sum((matrixData[k,]-media[k])^2)))
  } #Returns the Nash Sutchiffe Coefficient of all the families

  mediaEE=mean(EE)


  RSS=rep(0,E)
  for (k in 1:E){
    RSS[k]=
      sum((matrixData[k,]-matrixExpected[k,])^2)

  } #Returns the RSS (residual sum of squares) of all families

  mediaRSS=mean(RSS)


  MAPE=rep(0,E)
  for (k in 1:E){
    MAPE[k]=sum(abs((matrixData[k,]-matrixExpected[k,]))/abs(matrixData[k,]))/times.points
  } #Returns  mean absolute percentage error (MAPE), also known as mean absolute percentage deviation (MAPD), of all families

  mediaMAPE=mean(MAPE)


QualityFinal<-cbind( c(RMSD,mediaRMSD),
         c(EE,mediaEE),
         c(RSS,mediaRSS),
         c(MAPE,mediaMAPE))
colnames(QualityFinal)<-c("RMSD", "Nash Sutchiffe Coefficient", "RSS","MAPE")
rownames(QualityFinal)<-c(names.especie,"Mean")

return(QualityFinal)

}
