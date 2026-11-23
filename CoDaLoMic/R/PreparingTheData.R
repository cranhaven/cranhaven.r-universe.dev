#' Preparing dataset
#'
#' Preparing the dataset to be introduce in the models' functions. In order to introduce the usage of the package there is a README file. You can find the link to the file using \code{base::system.file("extdata", "README.pdf", package = "CoDaLoMic")}. On windows you can open the file with \code{base::shell.exec(system.file("extdata", "README.pdf", package = "CoDaLoMic"))}.
#'
#'
#' @param DaTa data.frame. The first column contains the time point information (natural numbers 1,2,3...). The rest of the columns contain the relative abundance of each bacteria at the different time points. The values of each column must sum 1.
#' @param Pred Number. The data at t=1,...,Pred-1 will be used to estimate the model. The rest of the time points will be used to study the capacity of the model to predict. If \code{Pred==0} all the datatset will be used to estimate the model.
#'
#' @return If \code{Pred==0} returns a list with
#' \itemize{
#'   \item \code{Tt} - The number of time points available.
#'   \item \code{E} - Number of bacteria available
#'    \item \code{especieOriginal} - Matrix that contains at row i the bacterial taxa of bacteria i at all time points.
#'   \item \code{especiemodiOriginal} - Matrix that contains at row i the bacterial taxa of bacteria i at time points t=2,...,\code{Tt}.
#'}
#'
#' If \code{Pred!=0} returns a list with
#' \itemize{
#'   \item \code{Tt} - The number of time points available used to estimate the model (\code{Tt}=Pred-1).
#'   \item \code{E} - Number of bacteria available
#'    \item \code{especieOriginal} - Matrix that contains at row i the bacterial taxa of bacteria i at the time points t=1,2,...,Pred-1.
#'   \item \code{especiemodiOriginal} - Matrix that contains at row i the bacterial taxa of bacteria i at time points t=2,...,Pred-1.
#'   \item \code{especieOriginal.All} - Matrix that contains at row i the bacterial taxa of bacteria i at the time points.
#'   \item \code{especiemodiOriginal.All} - Matrix that contains at row i the bacterial taxa of bacteria i at time points.
#'  \item \code{K} - Number of time points available at the dataset.
#'}
#'
#' @examples
#'
#'
#' df<-data.frame(cbind(c(1,2,3),
#'                      c(0.5,0.2,0.3),
#'                      c(0.2,0.1,0.6),
#'                      c(0.1,0.1,0.8),
#'                      c(0.3,0.3,0.4)))
#' PreparingTheData(df,Pred=0)
#'
#' df2<-data.frame(cbind(c(1,2,3,4,5),
#'                       c(0.1,0.1,0.1,0.2,0.5),
#'                       c(0.2,0.2,0.2,0.2,0.2),
#'                       c(0.2,0.3,0.1,0.2,0.2)))
#' PreparingTheData(df2,Pred=4)
#' @export
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


PreparingTheData <- function(DaTa,Pred) {

  if(Pred==0){

  E=(dim(DaTa)[2]-1) #Number of bacteria available at the data
  Tt=dim(DaTa)[1] #Number of time points available at the data


  especieOriginal=matrix(0,E,Tt)
  for (i in 1:E){
    especieOriginal[i,]=DaTa[,i+1]
  }#This matrix contains at row i the bacterial taxa of bacteria i at all time points
  Time=DaTa[,1]

  especiemodiOriginal=especieOriginal[,-1] #This matrix contains at row i the bacterial taxa of bacteria i at time points t=2,...,Tt.

  return(list("E" = E, "Tt" = Tt, "especieOriginal"=especieOriginal, "especiemodiOriginal"= especiemodiOriginal))

}

  if(Pred!=0){

    E=(dim(DaTa)[2]-1) #Number of bacteria available at the data
    K=dim(DaTa)[1] #Number of time points available at the data


    especieOriginal.All=matrix(0,E,K)
    for (i in 1:E){
      especieOriginal.All[i,]=DaTa[,i+1]
    }#This matrix contains at row i the bacterial taxa of bacteria i at all time points


    especiemodiOriginal.All=especieOriginal.All[,-1] #This matrix contains at row i the bacterial taxa of bacteria i at time points t=2,...,Tt.



    DaTanew=DaTa[-c(Pred:K),]


    E=(dim(DaTanew)[2]-1) #Number of bacteria available at the data
    Tt=dim(DaTanew)[1] #Number of time points available at the data


    especieOriginal=matrix(0,E,Tt)
    for (i in 1:E){
      especieOriginal[i,]=DaTanew[,i+1]
    }#This matrix contains at row i the bacterial taxa of bacteria i at all time points
    Time=DaTanew[,1]

    especiemodiOriginal=especieOriginal[,-1] #This matrix contains at row i the bacterial taxa of bacteria i at time points t=2,...,Tt.

    return(list("E" = E, "Tt" = Tt, "especieOriginal"=especieOriginal, "especiemodiOriginal"= especiemodiOriginal,"K"=K, "especieOriginal.All"=especieOriginal.All,"especiemodiOriginal.All" =especiemodiOriginal.All))



  }


}
