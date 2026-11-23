#' Obtainig a table with the interpretable parameters
#'
#' This function returns a table with the interpretable parameters of the Dirich-gLV model.
#'
#'In an example with three bacteria, the regression of this model is defined by
#'
#' \deqn{r_{1}\cdot log(x_{1}(t)/x_{3}(t))+log(x_{1}(t)/x_{3}(t))\cdot [a_{11}\cdot log(x_{1}(t)/x_{3}(t))(t)+a_{12}\cdot log(x_{2}(t)/x_{3}(t))] }
#' \deqn{r_{2}\cdot log(x_{2}(t)/x_{3}(t))+log(x_{2}(t)/x_{3}(t))\cdot [a_{21}\cdot log(x_{1}(t)/x_{3}(t))(t)+a_{22}\cdot log(x_{2}(t)/x_{3}(t))] }
#'
#' @param especie Matrix that contains at row i the bacterial taxa of bacteria i at all time points. The bacteria placed in the last row of this matrix is the one used as reference in the alr transformation that the model applies.
#' @param names Vector with the name of the bacteria in the same order than are present in the \code{especie} matrix.
#' @param E Number of bacteria available.
#' @param Param.Estimates Vector with the estimates parameters. It is equal to \code{c(tau,as.vector( pam))} where:
#'
#'\itemize{
#'   \item pam  Matrix. Each row has the parameters of each bacteria. Following our example, pam has the parameters placed as follows:
#'   \tabular{rrrr}{
#'  r1 \tab  a11  \tab  a12\cr
#'  r2 \tab  a21 \tab   a22 }
#'   \item tau  Number. Value of the tau parameter in the model
#'   }
#'
#'@return Returns the table as formatted text. By using the \code{cat} function, you can generate a LaTeX-ready table that can be easily copied and pasted.
#'
#' @examples
#'
#'
#'pam.ini=rbind(c(0.1,0.2,0.3),c(0.4,0.5,0.6))
#'paramEstimadosFinal=c(5, as.vector(pam.ini))
#'E=3
#'especie=cbind(c(0.2,0.4,0.4),c(0.1,0.1,0.8),c(0.5,0.1,0.4))
#'names=c("a","b","c")
#'
#'tat=Table_alr_Dirich_glv(paramEstimadosFinal,especie,names,E)
#'cat(tat, sep = "\n")
#'
#'@references Creus-Martí, I. and Moya, A. and Santonja, F. J. (2018). A Statistical Model with a Lotka-Volterra Structure for Microbiota Data. Lucas Jodar, Juan Carlos Cortes and Luis Acedo,  Modelling or engineering and human behavior 2018, Instituto Universitario de Matematica Multidisciplinar. ISBN: 978-84-09-07541-6
#' @export
#'
#
#

Table_alr_Dirich_glv<-function(Param.Estimates,especie,names,E){

  tau=Param.Estimates[1]
  parms.vector.m=Param.Estimates[-1]
  parms=matrix(0,dim(especie)[1]-1,dim(especie)[1])
  m=length(parms.vector.m)/(dim(especie)[1]-1)
  #parms[,1]=parms.vector.m[c(1:(1+dim(especie)[2]-1))]
  parms[,1]=parms.vector.m[c(1:dim(parms)[1])]
  for(i in 1:(m-1)){
    parms[,i+1]=parms.vector.m[c((1+i*dim(parms)[1]):(1+i*dim(parms)[1]+dim(parms)[1]-1))]
  }

  parms.new=cbind(names[-length(names)],round(parms,2))


  mm=matrix(0,8,E+1)
  mm[1,]=c("Names" ,"Weight that", "Weight that", rep(NA,E-2))
  mm[2,]=c(NA ,"the bacteria has","the interaction of" , rep(NA,E-2))
  mm[3,]=c(NA ,"in defining", "both bacteria", rep(NA,E-2))
  mm[4,]=c(NA ,"herself in the","has in defining", rep(NA,E-2))
  mm[5,]=c(NA ,"next time point","the bacteria in", rep(NA,E-2))
  mm[6,]=c(NA ,NA, "in the row in the", rep(NA,E-2))
  mm[7,]=c(NA ,NA, "next time point", rep(NA,E-2))
  mm[8,]=c(NA ,NA, names[-length(names)])



  Final=rbind(mm,parms.new)

  xt<-xtable::xtable(Final)
  hlines<-c(-1,7,8,dim(Final)[1])


  salida <- utils::capture.output(print(xt, booktabs = TRUE, hline.after = hlines,include.rownames=FALSE, include.colnames=FALSE))

  return(salida)

}
