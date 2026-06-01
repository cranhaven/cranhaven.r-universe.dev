#' Teste de homogeneidade de variancias de Bartlett
#'
#' \code{bartlett} Executa o teste de homogeneidade de
#' variancias de Bartlett (1937) para um delineamento
#' inteiramente casualizado.
#' @param trat Vetor numerico ou complexo contendo os
#' tratamentos.
#' @param resp Vetor numerico ou complexo contendo a variavel
#' resposta.
#' @param t Escalar. Numero de tratamentos.
#' @param r Vetor numerico ou complexo contendo o numero de
#' repeticoes de cada tratamento.
#' @return Retorna o valor-p do teste de homogeneidade de
#' variancias de Bartlett para a hipotese nula de que as
#' variancias sao homogeneas.
#' @references BARTLETT, M. S. Properties of sufficiency and
#' statistical tests. \emph{Proceedings of the Royal
#' Statistical Society - Serie A}, 60:268-282, 1937.
#'
#' NOGUEIRA, D, P.; PEREIRA, G, M. Desempenho de testes para
#' homogeneidade de vari?ncias em delineamentos inteiramente
#' casualizados. \emph{Sigmae}, Alfenas, v.2, n.1, p. 7-22.
#' 2013.
#' @author Eric B Ferreira, \email{eric.ferreira@@unifal-mg.edu.br}
#' @author Denismar Alves Nogueira
#' @author Marcos Costa de Paula
#' @author Mateus Pimenta Siqueira Lima
#' @seealso \code{\link{levene}}, \code{\link{layard}},
#' \code{\link{oneillmathews}} e \code{\link{samiuddin}}.
#' @examples
#' data(ex1)
#' attach(ex1)
#' dic(trat, ig, quali = FALSE, hvar='bartlett', sigF = 0.05)
#' @export

bartlett<-function(trat, resp, t, r)
{
  vari<-matrix(0,t,1)
  rp<-0
  for(i in 1:t) {
    vari[i]<-var(resp[(rp+1):(rp+r[i])])
    rp<-sum(r[1:i])
  }
  S2p<-sum((r-1)*vari)/(length(resp)-t)
  A<-(length(resp)-t)*log(S2p)-sum((r-1)*log(vari))
  B<-(1/(3*(t-1)))*(sum(1/(r-1))-(1/(length(resp)-t)))
  Xc1<-A/(1+B)
  pvalor<-1-pchisq(Xc1, t-1)
  output <- pvalor
  return(output)
}
