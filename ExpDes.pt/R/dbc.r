#' Delineamento em Blocos Casualizados
#'
#' \code{dbc} Analisa experimentos em Delineamento em Blocos
#' Casualizados balanceado com um so fator, considerando o
#' modelo fixo.
#' @param trat Vetor numerico ou complexo contendo os
#' tratamentos.
#' @param bloco Vetor numerico ou complexo contendo os blocos.
#' @param resp Vetor numerico ou complexo contendo a variavel
#' resposta.
#' @param quali Logico. Se TRUE (default), os tratamentos sao
#' entendido como qualitativos, se FALSE, quantitativos.
#' @param mcomp Permite escolher o teste de comparacao
#' multipla; o \emph{default} e o teste de Tukey, contudo
#' tem-se como outras opcoes: o teste LSD ("lsd"), o teste LSDB
#' ("lsdb"), o teste de Duncan ("duncan"), o teste de SNK
#' ("snk"), o teste de Scott-Knott ("sk"), o teste de
#' comparacoes multiplas bootstrap ("ccboot") e o teste de
#' Calinski e Corsten baseado na distribuicao F ("ccf").
#' @param nl Logico. Escolhe se modelos nao lineares devem ser
#' ajustados aos fatores quantitativos. O default e FALSE.
#' @param hvar Permite escolher o teste de homogeneidade de
#' variancias; o \emph{default} e o teste de ONeill e Mathews
#' ('oneilmathews'), contudo tem-se como outras  opcoes: o
#' teste de Han ('han') e o teste de Ascombe e Tukey
#' ('ascombetukey').
#' @param sigT Significancia a ser adotada pelo teste de
#' comparacao multipla de medias; o default e 5\%.
#' @param sigF Significancia a ser adotada pelo teste F da
#' ANAVA; o default e 5\%.
#' @param unfold Orienta os desdobramentos apos a analise de
#' variancia. Se NULL (\emph{default}), sao feitas as analises
#' recomendadas; se '0', e feita apenas a analise de variancia;
#' se '1', os efeitos simples sao estudados.
#' @details Os argumentos sigT e mcomp so serao utilizados
#' quando os tratamentos forem qualitativos.
#' @return Sao retornados os valores da analise de variancia do
#' DBC em questao, o teste de normalidade de Shapiro-Wilk para
#' os residuos do modelo, o ajuste de modelos de regressao
#' (caso de tratamentos quantitativos) ou os testes de
#' comparacao de medias (caso de tratamentos qualitativos):
#' teste de Tukey, teste de Duncan, teste t de Student (LSD),
#' teste t de Bonferroni, teste de Student-Newman-Keuls(SNK),
#' teste de Scott-Knott  e teste de comparacoes multiplas
#' bootstrap.
#' @references BANZATTO, D. A.; KRONKA, S. N. Experimentacao
#' Agricola. 4 ed. Jaboticabal: Funep. 2006. 237 p.
#'
#' FERREIRA, E. B.; CAVALCANTI, P. P.; NOGUEIRA D. A. Funcao
#' em codigo R para analisar experimentos em DBC simples, em
#' uma so rodada. In: JORNADA CIENTIFICA DA UNIVERSIDADE
#' FEDERAL DE ALFENAS-MG, 2., 2009, Alfenas. Annals...
#' ALfenas: Unifal-MG, 2009.
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#' @author Denismar Alves Nogueira
#' @author Portya Piscitelli Cavalcanti
#' @note O \code{\link{graficos}} pode ser usado para
#' construir os graficos da regressao e o
#' \code{\link{plotres}} para analise do residuo da anava.
#' @seealso \code{\link{fat2.dbc}}, \code{\link{fat3.dbc}},
#' \code{\link{psub2.dbc}}, \code{\link{faixas}},
#' \code{\link{fat2.ad.dbc}} e \code{\link{fat3.ad.dbc}}.
#' @examples
#' data(ex2)
#' attach(ex2)
#' dbc(trat, provador, aparencia, quali = TRUE, mcomp = "lsd",
#' hvar = "oneillmathews", sigT = 0.05, sigF = 0.05, unfold=NULL)
#' @export

dbc<-function (trat,
               bloco,
               resp,
               quali = TRUE,
               mcomp = "tukey",
               nl=FALSE,
               hvar='oneillmathews',
               sigT = 0.05,
               sigF = 0.05,
               unfold=NULL){

Trat <- factor(trat)
Bloco <- factor(bloco)
anava <- aov(resp ~ Trat + Bloco)
tab <- summary(anava)

colnames(tab[[1]]) <- c("GL", "SQ", "QM", "Fc", "Pr>Fc")
tab[[1]] <- rbind(tab[[1]], c(apply(tab[[1]], 2, sum)))
rownames(tab[[1]]) <- c("Tratamento", "Bloco", "Residuo", "Total")
cv <- round(sqrt(tab[[1]][3, 3])/mean(resp) * 100, 2)
tab[[1]][4, 3] = NA
cat("------------------------------------------------------------------------\nQuadro da analise de variancia\n------------------------------------------------------------------------\n")
print(tab[[1]])
cat("------------------------------------------------------------------------\nCV =",
    cv, "%\n")

#Teste de normalidade
pvalor.shapiro <- shapiro.test(anava$residuals)$p.value
cat("\n------------------------------------------------------------------------\nTeste de normalidade dos residuos \n")
cat("valor-p: ", pvalor.shapiro, "\n")
if (pvalor.shapiro < 0.05) {
  cat("ATENCAO: a 5% de significancia, os residuos nao podem ser considerados normais!\n------------------------------------------------------------------------\n")
}
else {
  cat("De acordo com o teste de Shapiro-Wilk a 5% de significancia, os residuos podem ser considerados normais.\n------------------------------------------------------------------------\n")
}

#Teste de homogeneidade de vari?ncia
if(hvar=='oneillmathews') pvalor.hvar<-oneilldbc(resp, Trat, Bloco)
if(hvar=='han') pvalor.hvar<-han(resp, Trat, Bloco)
if(hvar=='anscombetukey') pvalor.hvar<-anscombetukey(resp, Trat, Bloco, tab[[1]][3,1], as.numeric(tab[[1]][3,3]), tab[[1]][1,2], tab[[1]][2,2], anava$residuals, anava$fitted.values)

cat('\n------------------------------------------------------------------------\nTeste de homogeneidade de variancia \n')
cat('valor-p: ',pvalor.hvar, '\n')
if(pvalor.hvar<0.05){cat('ATENCAO: a 5% de significancia, as variancias nao podem ser consideradas homogeneas!
------------------------------------------------------------------------\n')}
else{cat('De acordo com o teste de',hvar,'a 5% de significancia, as variancias podem ser consideradas homogeneas.
------------------------------------------------------------------------\n')}

# Creating unfold #########################################
if(is.null(unfold)){
  if(tab[[1]][1,5]<=sigF) {unfold<-c(unfold,1)}
}

#Para fator significativo, fazer...
if(any(unfold==1)) {

if (quali == TRUE) {

  if(mcomp=='tukey') tukey(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  if(mcomp=='duncan')duncan(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  if(mcomp=='lsd')   lsd(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  if(mcomp=='lsdb')  lsdb(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  if(mcomp=='sk')    scottknott(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  if(mcomp=='snk')   snk(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  if(mcomp=="ccboot")ccboot(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)
  if(mcomp=="ccF")   ccF(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigT)

                  }

else if(nl==FALSE) reg<-reg.poly(resp, trat, tab[[1]][3,1],
                                 tab[[1]][3,2], tab[[1]][1,1], tab[[1]][1,2])
else if(nl==TRUE)  reg<-reg.nl(resp, trat)
                            }
else {
cat("\nDe acordo com o teste F, as medias nao podem ser consideradas diferentes.\n")
mean.table <- tapply.stat(resp, trat, mean)
colnames(mean.table) <- c("Niveis", "Medias")
print(mean.table)
cat("------------------------------------------------------------------------\n")
     }

#Saida
out<-list()
out$residuos<-anava$residuals
out$gl.residual<-anava$df.residual
out$coeficientes<-anava$coefficients
out$efeitos<-anava$effects
out$valores.ajustados<-anava$fitted.values
out$medias<-tapply.stat(resp,trat,mean)
if(quali==FALSE && tab[[1]][1,5]<sigF) {out$reg<-reg}
invisible(out)

}

