#' Teste de homogeneidade de variancias de Anscombe e Tukey
#'
#' \code{anscombetukey} Realiza o teste de Anscombe e Tukey
#' (1963) para o teste de homegenidade de variancias em um
#' delineamento em blocos casualizados.
#' @param resp Vetor numerico ou complexo contendo a variavel
#' resposta.
#' @param trat Vetor numerico ou complexo contendo os
#' tratamentos.
#' @param bloco Vetor numerico ou complexo contendo os blocos.
#' @param glres Escalar. Graus de liberdade do residuo.
#' @param qmres Escalar. Quadrado medio do residuo.
#' @param sqtrat Escalar. Soma de quadrados do tratamento.
#' @param sqbloco Escalar. Soma de quadrados de bloco.
#' @param residuos Vetor numerico ou complexo contendo os
#' residuos.
#' @param valores.ajustados Vetor numerico ou complexo contendo os
#' valores ajustados.
#' @return Retorna o valor-p do teste de Anscombe e Tukey de
#' homogeneidade de variancias para uma hipotese nula de que as
#' variancias sao homogeneas.
#' @author Eric B Ferreira, \email{eric.ferreira@@unifal-mg.edu.br}
#' @author Denismar Alves Nogueira
#' @author Marcos Costa de Paula
#' @author Mateus Pimenta Siqueira Lima
#' @seealso \code{\link{han}}, \code{\link{oneilldbc}}.
#' @references ANSCOMBE, F. J.; TUKEY, J. W. \emph{The
#' examination and analysis of residuals.} Technometrics,
#' 5:141-160, 1963.
#'
#' RIBEIRO, R. \emph{Proposta e comparacao do desempenho de
#' testes para homogeneidade de variancia de modelos de
#' classificacao one-way e two-way}. Iniciacao Cientifica.
#' (Iniciacao Cientifica) - Universidade Federal de Alfenas.
#' 2012.
#' @examples
#' data(ex2)
#' attach(ex2)
#' dbc(trat, provador, aparencia, quali = TRUE, mcomp = "tukey",
#' hvar='anscombetukey', sigT = 0.05, sigF = 0.05)
#' @importFrom "stats" "anova" "aov" "kmeans" "ks.test" "lm"
#' "model.tables" "na.action" "na.omit" "pchisq" "pf"
#' "ptukey" "qtukey" "rnorm" "runif" "shapiro.test" "var"
#' @export

anscombetukey<-function(resp, trat, bloco, glres, qmres, sqtrat,
                        sqbloco, residuos, valores.ajustados)
{
 Trat<-length(trat)
 Bloco<-length(bloco)
 div1<-(2*glres*(qmres)^2)/glres+2
 div2<-((((Trat-2)*(Bloco-1))/Trat*Bloco)*sqtrat)+((((Trat-1)*(Bloco-2))/Trat*Bloco)*sqbloco)
 Fc17<-((sum((residuos^2)*(valores.ajustados-mean(resp))))^2)/(div1*div2)
 pvalor.hvar<-1-pf(Fc17,1,((Trat-1)*(Bloco-1)))
 output <- pvalor.hvar
 return(output)
}
