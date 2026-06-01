#' Teste de Homogeneidade de Variancia de Han
#'
#' \code{han} Realiza o teste de Han (1969) para homogeneidade
#' de variancias em um delineamento em blocos casualizados.
#' @param resp Vetor numerico ou complexo contendo a variavel
#' resposta.
#' @param trat Vetor numerico ou complexo contendo os
#' tratamentos.
#' @param bloco Vetor numerico ou complexo contendo os blocos.
#' @return Retorna o valor-p do teste de Han de homogeneidade
#' de variancias para a hipotese nula de que as variancias sao
#' homogeneas.
#' @references HAN, C. P. Testing the homogeneity of variances
#' in a two-way classification. \emph{Biometrics}, 25:153-158,
#' Mar. 1969.
#'
#' RIBEIRO, R. \emph{Proposta e comparacao do desempenho de
#' testes para homogeneidade de variancia de modelos de
#' classicacao one-way e two-way}. Iniciacao Cientifica.
#' (Iniciacao Cientifica) - Universidade Federal de Alfenas.
#' 2012.
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#'  @author Denismar Alves Nogueira
#'  @author Marcos Costa de Paula
#'  @author Mateus Pimenta Siqueira Lima
#'  @author Renata Ribeiro
#' @seealso \code{\link{anscombetukey}},
#' \code{\link{oneilldbc}}.
#' @examples
#' data(ex2)
#' attach(ex2)
#' dbc(trat, provador, aparencia, hvar = "han")
#' @export

han<-function(resp,trat,bloco)
{
  Trat<-length(levels(trat))
  Block<-length(levels(bloco))
  if (Block>Trat){
   dife<-matrix(0,Block,Trat)
   ymedia<-matrix(0,Block,1)
   rp<-0
   for(j in 1:Block) {
     for(i in 1:Trat) {
       ymedia[j]<-mean(resp[(rp+1):(rp+Trat)])
       dife[j,i]<-(resp[rp+i]-ymedia[j])
     }
     rp<-Trat*j
    }
   modelohan<-lm(ymedia ~ dife[,2:Trat])
   pvalor.hvar<-1-pf(summary(modelohan)[[10]][1],summary(modelohan)[[10]][2],summary(modelohan)[[10]][3])
   output <- pvalor.hvar
   return(output)
  }
}
