#' Zero replacement
#'
#' In this function the zeros are removed or replaced using functions of "zCompositions" package that can be used with longitudinal data (because they do not use the information of other rows to make the replacement).
#'
#'
#' @param DaTa data.frame. The first column contains the time point information (natural numbers 1,2,3...). The rest of the columns contain the relative abundance of each bacteria at the different time points. The values of each column must sum 1.
#' @param seed Number. Set a seed. Default \code{seed=NULL}.
#' @param method Character.
#' \itemize{
#'   \item If \code{method="multKM"} - The replacement is carried out with the "multiplicative Kaplan-Meier smoothing spline replacement" (Palarea-Albaladejo and Martín-Fernandez, 2015). Default method. The zeros must be written with a 0.
#'   \item If \code{method="multRepl"} -  The replacement is carried out with the "multiplicative simple replacement" (Palarea-Albaladejo and Martín-Fernandez, 2015).  The zeros must be written with a 0.
#'   \item If \code{method="nozeros"} - The bacteria that contains zeros are removed. One column is added to the dataset called "Other".
#' }
#'
#'
#' @return The dataset without zeros.
#'
#'
#' @examples
#'
#'set.seed(2)
#'dat=gtools::rdirichlet(6,c(1,2,3,1,2,3))
#'dat2=dat
#'dat2[2,1]=0
#'dat2[2,2]=dat[2,1]+dat[2,2]
#'dat2[4,3]=0
#'dat2[4,4]=dat[4,3]+dat[4,4]
#'
#'X <- cbind( c(1:6) ,dat2)
#'
#'Final=ZeroData(X,"multKM",1)
#'Final2=ZeroData(X,"multRepl",1)
#'
#'@references Palarea-Albaladejo J. and Martín-Fernandez JA. zCompositions – R package for multivariate imputation of left-censored data under a compositional approach. Chemometrics and Intelligent Laboratory Systems 2015; 143: 85-96.
#'
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


ZeroData <- function(DaTa, method="multKM", seed=NULL) {

  X=DaTa[,-1]

  if(method=="multKM"){

    Y=X*100
    if(!is.null(seed)){
      set.seed(seed)
    }
    Data_replaced<-zCompositions::multKM(Y,label=0,dl=rep(1,ncol(Y)))
    DataFin=cbind(DaTa[,1],  Data_replaced/100)
    colnames(DataFin)<-c("Time", colnames( Data_replaced))

    return(DataFin)
  }


  if(method=="multRepl"){

    Y=X*100
    if(!is.null(seed)){
      set.seed(seed)
    }
    Data_replaced<-zCompositions::multRepl(Y,label=0,dl=rep(1,ncol(Y)))
    DataFin=cbind(DaTa[,1],  Data_replaced/100)
    colnames(DataFin)<-c("Time", colnames( Data_replaced))

    return(DataFin)
  }


  if(method=="nozeros"){

    vec=rep(0,2)
    for(i in 1:dim(DaTa)[2]   ){
    if(length(which(DaTa[,i]==0))!=0){vec=c(vec,i)}
    }
    vect=vec[-c(1,2)]
    SinOther=DaTa[,-vect]
    SinOtherTime=SinOther[,-1]

    Other=matrix(0, dim(SinOtherTime)[1],1)
    for (i in 1:dim(SinOtherTime)[1]){Other[i,]=1-sum(SinOtherTime[i,])}
    colnames(Other)<-"Other"

    DataFin=cbind(DaTa[,-vect], Other)

    return(DataFin)

  }
}
