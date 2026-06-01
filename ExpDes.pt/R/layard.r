#' Teste de homogeneidade de variancias de Layard
#'
#' \code{layard} Executa o teste de homogeneidade de variancias
#' de Layard (1973) por Jackknife para um delineamento
#' inteiramente casualizado.
#' @param trat Vetor numerico ou complexo contendo os
#' tratamentos.
#' @param resp Vetor numerico ou complexo contendo a variavel
#' resposta.
#' @param t Escalar. Numero de tratamentos.
#' @param r Vetor numerico ou complexo contendo o numero de
#' repeticoes de cada tratamento.
#' @return Retorna o valor-p do teste de homogeneidade de
#' variancias de Layard para uma hipotese nula de que as
#' variancias sao homogeneas.
#' @references LAYARD, M. N. J. Robust large-sample tests for
#' homogeneity of variances. \emph{Journal of the American
#' Statistical Association}, v.68, n.341, p.195-198, 1973.
#'
#' NOGUEIRA, D, P.; PEREIRA, G, M. Desempenho de testes para
#' homogeneidade de variancias em delineamentos inteiramente
#' casualizados. \emph{Sigmae}, Alfenas, v.2, n.1, p. 7-22.
#' 2013.
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#'  @author Denismar Alves Nogueira
#'  @author Marcos Costa de Paula
#'  @author Mateus Pimenta Siqueira Lima
#' @seealso \code{\link{bartlett}}, \code{\link{samiuddin}},
#' \code{\link{levene}}, \code{\link{oneillmathews}}.
#' @examples
#' data(ex1)
#' attach(ex1)
#' dic(trat, ig, quali = FALSE, hvar = "layard")
#' @export

layard<-function(trat,resp,t,r)
 {
 vari<-matrix(0,t,1)
 varij<-matrix(0,max(r),t)
 U<-matrix(0,max(r),t)
 rp<-0
 for(i in 1:t) {
   vv<-resp[(rp+1):(rp+r[i])]
   vari[i]<-var(vv)
   for(j in 1:r[i]) {
     varij[j,i]<-var(vv[-j])
     U[j,i]<-(r[i]*log(vari[i]))-((r[i]-1)*log(varij[j,i]))
   }
   rp<-sum(r[1:i])
  }
 Uij<-as.vector(U)
 dadosUij<-cbind(trat,Uij)
 dadosUij<-as.data.frame(dadosUij)
 pvalor<-summary(aov(dadosUij$Uij ~ trat))[[1]][1,5]
 output <- pvalor
 return(output)
}
