
#' Obtaining the selected principal balances values
#'
#' Calculates the value of the selected principal balances (SPBal) of the BPBM model at all time points.
#'
#'@param especie  Matrix that contains at row i the bacterial taxa of bacteria i at all time points.
#'@param Tt  Number of time points available
#'@param Num List. The component i of the list has the number of the row of the matrix \code{especie} where the bacteria in the numerator of the principal balance i are placed.
#'@param Dem List. The component i of the list has the number of the row of the matrix \code{especie} where the bacteria in the denominator of the principal balance i are placed.
#'
#'
#'@return Returns a list with:
#'\itemize{
#'   \item NumSPBal:  List. The component i of the list has the number of the row of the matrix \code{especie} where the bacteria in the numerator of the selected  principal balance i are placed.
#'   \item DemSPBal:  List. The component i of the list has the number of the row of the matrix \code{especie} where the bacteria in the denominator of the selected  principal balance i are placed.
#'    \item MatrixSPBal:  MatrixSPBal is the matrix that contains the covariates of the model. The first line es equal to 1 for all columns. The other rows contain the value of one SPBal at all time points. The selected principal balance of the row i+1 has at its numerator the bacteria placed in the rows \code{NumSPBal[[i]]} of the "especie". The selected principal balance of the row i+1 has at its denominator the bacteria placed in the rows \code{DemSPBal[[i]]} of the "especie".
#'   \item PercenVarianceSPBal: Vector. The component of the vector i contains the percentage of variance of the SPBal with numerator \code{NumSPBal[[i]]} and denominator \code{DemSPBal[[i]]}.
#'   }
#'
#'
#'
#'
#'
#' @examples
#'
#'set.seed(314)
#'esp2=t(gtools::rdirichlet(n=6, c(1,1,5,1,1)))
#'
#'Num2<-list(3,c(3,5),1,c(3,5,4))
#'Dem2<-list(5,4,2,c(1,2))
#'
#'ObtainigValueSPBal(Num2,Dem2,esp2,6)
#'
#' @references Creus-Martí, I., Moya, A., Santonja, F. J. (2022). Bayesian hierarchical compositional models for analysing longitudinal abundance data from microbiome studies. Complexity, 2022.
#' @export
#'
#'
#'

ObtainigValueSPBal<-function(Num, Dem, especie, Tt){

#Obtaining the value of the principal balances at all time points
PB=ObtainigValuePB(Num, Dem, especie, Tt)

#Obtaining the percentage of variance of each PB (in the same order that are placed in the rows of PB)
PercenVar=Percen_Variance(PB)


OrdenMaxToMin=sort(PercenVar,decreasing=TRUE, index.return = TRUE)$x
IndexInPB=sort(PercenVar,decreasing=TRUE, index.return = TRUE)$ix

ss=0
pp=0
while(ss<80){
  pp=pp+1
  ss=sum(OrdenMaxToMin[c(1:pp)])
}

rows.position=IndexInPB[c(1:pp)]
MatrizPBmodelo=BPBM_Matrix(rows.position, PB, Tt)

NumSPBal=Num[rows.position]
DemSPBal=Dem[rows.position]
PercenVarSPBal=PercenVar[rows.position]
rturn<-list(NumSPBal,DemSPBal,MatrizPBmodelo,PercenVarSPBal)
names(rturn)<-c("NumSPBal","DemSPBal", "MatrixSPBal","PercenVarianceSPBal")
return(rturn)
}
