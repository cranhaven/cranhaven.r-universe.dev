#' Teste de homogeneidade de variancias de Levene
#'
#' \code{levene} Executa o teste de homogeneidade de variancias
#' de Levene (1960) para um delineamento inteiramente
#' casualizado.
#' @param trat Vetor numerico ou complexo contendo os
#' tratamentos.
#' @param resp Vetor numerico ou complexo contendo a variavel
#' resposta.
#' @param t Escalar. Numero de tratamentos.
#' @param r Vetor numerico ou complexo contendo o numero de
#' repeticoes de cada tratamento.
#' @return Retorna o valor-p do teste de homogeneidade de
#' variancias de Levene para uma hipotese nula de que as
#' variancias sao homogeneas.
#' @references LEVENE, H. Robust tests for equality of
#' variances. In: Olkin, I.; Ghurye, S.G.; Hoeffding, W.;
#' Madow, W.G.; Mann, H.B. (eds.). \emph{Contribution to
#' Probability and Statistics. Stanford}, CA: Stanford
#' University Press, pages 278-292, 1960.
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
#' \code{\link{layard}}, \code{\link{oneillmathews}}.
#' @examples
#' data(ex1)
#' attach(ex1)
#' dic(trat, ig, quali = FALSE, hvar = "levene")
#' @export

levene<-function(trat,resp,t,r)
{
   Trat<-factor(trat)
   zdados1<-matrix(0,length(resp),1)
   rp<-0
   for(k in 1:length(resp)) {
    zdados1[k]<-abs(resp[k]-mean(resp[(rp+1):(rp+r[Trat[k]])]))
    if(k<length(resp)){if(trat[k]!=trat[k+1]){rp<-sum(r[1:Trat[k]])}}
     }
   pvalor<-summary(aov(zdados1 ~ Trat))[[1]][1,5]
   output <- pvalor
   return(output)
}
