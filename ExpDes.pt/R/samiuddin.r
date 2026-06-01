#' Teste de homogeneidade de variancias de Samiuddin.
#'
#' \code{samiuddin} Executa o teste de homogeneidade de
#' variancias de Samiuddin (1976) para um delineamento
#' inteiramente casualizado.
#' @param trat Vetor numerico ou complexo contendo os
#' tratamentos.
#' @param resp Vetor numerico ou complexo contendo a
#' variavel resposta.
#' @param t Escalar.Numero de tratamentos.
#' @param r Vetor numerico ou complexo contendo o numero de
#' repeticoes de cada tratamento.
#' @return Retorna o valor-p do teste de homogeneidade de
#' variancias de Samiuddin para uma hipotese nula de que as
#' variancias sao homogenenas.
#' @references SAMIUDDIN, M. Bayesian test of homogeneity of
#' variance. \emph{Journal of the American Statistical
#' Association}, 71(354):515-517, Jun. 1976.
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
#' @seealso \code{\link{bartlett}}, \code{\link{layard}},
#' \code{\link{levene}}, \code{\link{oneillmathews}}.
#' @examples
#' data(ex1)
#' attach(ex1)
#' dic(trat, ig, quali = FALSE, hvar = "samiuddin", sigF = 0.05)
#' @export

samiuddin<-function(trat, resp, t, r)
{
  Trat<-factor(trat)
  #Samiuddin
  t=length(levels(Trat))
  m<-matrix(0,t,1)
  somma<-matrix(0,t,1)
  a2<-matrix(0,t,1)
  rp<-0
  for(i in 1:t) {
    dife<-0
    soma<-0
    for(j in 1:r[i]) {
      dife<-(resp[rp+j]-mean(resp[(rp+1):(rp+r[i])]))^2
      soma<-soma+dife
    }
    somma[i]<-soma
    rp<-sum(r[1:i])
    m[i]<-(((r[i]-1)/somma[i])^(1/3))*(1-(2/(9*(r[i]-1))))
    a2[i]<-2/(9*(somma[i]^(2/3))*(r[i]-1)^(1/3))
  }
  mm<-sum(m/a2)/sum(1/a2)
  pvalor<-pchisq(sum(((m-mm)^2)/a2), (t-1))
  output<-pvalor
  return(output)
}
