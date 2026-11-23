#' Obtaining the graphic of the SPBal at all time points
#'
#' The SPBal (of BPBM model) are ordered from highest to lowest variance percentage. The zero is highlight because the closer the value of the balance is to zero, the more similar (in terms of relative abundance) the numerator and denominator groups will be. The farther away from zero, the more different.
#'
#'@param MatrizPBmodelo Matrix. Output of "ObtainigValueSPBal" function.  MatrixSPBal is the matrix that contains the covariates of the model. The first line es equal to 1 for all columns. The other rows contain the value of one SPBal at all time points. The selected principal balance of the row i+1 has at its numerator the bacteria placed in the rows \code{NumSPBal[[i]]} of the "especie". The selected principal balance of the row i+1 has at its denominator the bacteria placed in the rows \code{DemSPBal[[i]]} of the "especie".
#'
#'@return Returns a graphic.
#'
#'
#'
#'
#' @examples
#'
#' MatrizPBmodelo=rbind(c(1,1,1,1,1,1),c(-0.3,0.4,0.3,-0.7,-0.4,-0.6),c(0.3,0.5,-0.3,0.1,0.4,0.1))
#' GraphicsSPBal(MatrizPBmodelo)
#'
#'@references Creus-Martí, I., Moya, A., Santonja, F. J. (2022). Bayesian hierarchical compositional models for analysing longitudinal abundance data from microbiome studies. Complexity, 2022.
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
GraphicsSPBal<-function(MatrizPBmodelo){

  if(dim(MatrizPBmodelo)[1]==2){

    SPBal=MatrizPBmodelo[-1,]
    Time=c(1:dim(MatrizPBmodelo)[2])
    Names.SPBal=rep("SPBal1",dim(MatrizPBmodelo)[2])


    df=data.frame(Time,SPBal,Names.SPBal)
    df$Time=as.numeric(df$Time)
    df$SPBal=as.numeric(df$SPBal)

    gr<-ggplot2::ggplot(df, ggplot2::aes(x=Time, y=SPBal))+ ggplot2::geom_line(ggplot2::aes(color=Names.SPBal))+ ggplot2::geom_hline(yintercept = 0)

    return(gr)

  }else{

  mmm=MatrizPBmodelo[-1,]
  mgg=data.frame(t(mmm))
  colnames(mgg)<-paste0("SPBal", c(1:dim(mgg)[2]))
  p=paste0("SPBal", c(1:dim(mgg)[2]))
  time=c(1:dim(mgg)[1])

  Time=rep(time, dim(mgg)[2])
  SPBal=as.vector(as.matrix(mgg))
  Names.SPBal=as.vector(sapply(p, function (x) rep(x,dim(mgg)[1])))

  df=data.frame(Time,SPBal,Names.SPBal)
  df$Time=as.numeric(df$Time)
  df$SPBal=as.numeric(df$SPBal)

  gr<-ggplot2::ggplot(df, ggplot2::aes(x=Time, y=SPBal))+ ggplot2::geom_line(ggplot2::aes(color=Names.SPBal))+ ggplot2::geom_hline(yintercept = 0)

  return(gr)

  }

  }
