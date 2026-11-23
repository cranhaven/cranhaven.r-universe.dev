#' Obtainig a table with the interpretable parameters
#'
#' This function returns a table with the interpretable parameters of the FBM model.
#'
#'
#' \deqn{\mu_{it}=a_{i1}+a_{i2}\cdot\text{alr}(x_{i,(t-1)})+a_{i3}\cdot\text{Balance}(x_{i,(t-1)})\text{ for }i=1,\dots, D-1\text{ where } D \text{ is the number of bacteria}}
#'
#' @param paramEstimadosFinal The estimate parameters, in the following order: a11,a12,a13, a21, a22,a23, ...a(D-1)1,a(D-1)2,a(D-1)3,tau. Where D is the number of bacterial species present in the matrix \code{especie}.
#' @param names Vector of length D. The component i has the name of the bacteria i.
#' @param E Number of bacteria available.
#'
#'@return Returns the table as formatted text. By using the \code{cat} function, you can generate a LaTeX-ready table that can be easily copied and pasted.
#'
#' @examples
#'paramEstimadosFinal=c(1,2,3,1,2,3,1,2,3)
#'names=c("Bact1", "Bact2","Bact3")
#'E=3
#'
#'tat=TableFBM(paramEstimadosFinal,names,E)
#'cat(tat, sep = "\n")
#'
#'@references Creus-Martí, I., Moya, A., Santonja, F. J. (2021). A Dirichlet autoregressive model for the analysis of microbiota time-series data. Complexity, 2021, 1-16.
#' @export
#'
#

TableFBM<-function(paramEstimadosFinal,names,E){

  H=length(paramEstimadosFinal)

  params=paramEstimadosFinal[-H]
  MatrizParametros=matrix(0,E-1,3)
  MatrizParametros[1,]=params[1:3]
  for (i in 1:(E-2)){
    MatrizParametros[i+1,]=params[(1+3*i):(3+3*i)]
  }


  mm=matrix(0,6,4)
  mm[1,]=c("Name","Intercept ", "Weight", "Weight")
  mm[2,]=c(NA,NA, "of the bacteria" ,"of the rest" )
  mm[3,]=c(NA,NA,"in defining herself ","of the community ")
  mm[4,]=c(NA,NA,"in the next", "in defining the")
  mm[5,]=c(NA,NA,"time point", "bacteria in the")
  mm[6,]=c(NA,NA,NA, "next time point")

  Final=rbind(mm,cbind(names[-length(names)],MatrizParametros))

  xt<-xtable::xtable(Final)
  hlines<-c(-1,6,dim(Final)[1])



  salida <- utils::capture.output(print(xt, booktabs = TRUE, hline.after = hlines,include.rownames=FALSE, include.colnames=FALSE))

  return(salida)
}

