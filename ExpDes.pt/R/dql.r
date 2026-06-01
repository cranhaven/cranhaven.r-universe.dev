#' Delineamento em Quadrado Latino
#'
#' \code{dql} Analisa experimentos em Delineamento em Quadrado
#' Latino balanceado com um so fator, considerando o modelo
#' fixo.
#' @param trat Vetor numerico ou complexo contendo os
#' tratamentos.
#' @param linha Vetor numerico ou complexo contendo as linhas.
#' @param coluna Vetor numerico ou complexo contendo as colunas.
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
#' DQL em questao, o teste de normalidade de Shapiro-Wilk para
#' os residuos do modelo, o ajuste de modelos de regressao
#' (caso de tratamentos quantitativos) ou os testes de
#' comparacao de medias (caso de tratamentos qualitativos):
#' teste de Tukey, teste de Duncan, teste t de Student (LSD),
#' teste t de Bonferroni, teste de Student-Newman-Keuls(SNK),
#' teste de Scott-Knott  e teste de comparacoes multiplas
#' bootstrap.
#' @references GOMES, F. P. Curso de Estatistica Experimental.
#' 10a ed. Piracicaba: ESALQ/USP. 1982. 430.
#'
#' FERREIRA, E. B.; CAVALCANTI, P. P.; NOGUEIRA D. A. Funcao
#' em codigo R para analisar experimentos em DQL simples, em
#' uma so rodada. In: CONGRESSO DE POS-GRADUACAO DA
#' UNIVERSIDADE FEDERAL DE LAVRAS, 18., 2009, Lavras.
#' Annals... Lavras: UFLA, 2009.
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#' @author Denismar Alves Nogueira
#' @author Portya Piscitelli Cavalcanti
#' @note O \code{\link{graficos}} pode ser usado para construir
#' os graficos da regressao e o \code{\link{plotres}} para
#' analise do residuo da anava.
#' @seealso \code{\link{dic}}, \code{\link{dbc}}.
#' @examples
#' data(ex3)
#' attach(ex3)
#' dql(trat, linha, coluna, resp, quali=TRUE, mcomp = "snk",
#' sigT = 0.05, sigF = 0.05, unfold=NULL)
#' @export

dql<-function(trat,
              linha,
              coluna,
              resp,
              quali=TRUE,
              mcomp="tukey",
              sigT=0.05,
              sigF=0.05,
              unfold=NULL) {

Trat<-factor(trat)
Linha<-factor(linha)
Coluna<-factor(coluna)
anava<-aov(resp~Trat+Linha+Coluna)
tab<-summary(anava)

colnames(tab[[1]])<-c('GL','SQ','QM','Fc','Pr>Fc')
tab[[1]]<-rbind(tab[[1]],c(apply(tab[[1]],2,sum)))
rownames(tab[[1]])<-c('Tratamento','Linha','Coluna','Residuo',
                      'Total')
cv<-round(sqrt(tab[[1]][4,3])/mean(resp)*100, 2)
tab[[1]][5,3]=NA
cat('------------------------------------------------------------------------\nQuadro da analise de variancia
------------------------------------------------------------------------\n')
print(tab[[1]])
cat('------------------------------------------------------------------------\nCV =',cv,'%\n')

#Teste de normalidade
pvalor.shapiro<-shapiro.test(anava$residuals)$p.value
cat('\n------------------------------------------------------------------------\nTeste de normalidade dos residuos (Shapiro-Wilk)\n')
cat('valor-p: ',pvalor.shapiro, '\n')
if(pvalor.shapiro<0.05){cat('ATENCAO: a 5% de significancia, os residuos nao podem ser considerados normais!
------------------------------------------------------------------------\n')}
else{cat('De acordo com o teste de Shapiro-Wilk a 5% de significancia, os residuos podem ser considerados normais.
------------------------------------------------------------------------\n')}

# Creating unfold #########################################
if(is.null(unfold)){
  if(tab[[1]][1,5]<=sigF) {unfold<-c(unfold,1)}
}

#Para fator significativo, fazer...
if(any(unfold==1)){

if(quali==TRUE) {

  if(mcomp=='tukey') tukey(resp,Trat,tab[[1]][4,1],tab[[1]][4,2],sigT)
  if(mcomp=='duncan')duncan(resp,Trat,tab[[1]][4,1],tab[[1]][4,2],sigT)
  if(mcomp=='lsd')   lsd(resp,Trat,tab[[1]][4,1],tab[[1]][4,2],sigT)
  if(mcomp=='lsdb')  lsdb(resp,Trat,tab[[1]][4,1],tab[[1]][4,2],sigT)
  if(mcomp=='sk')    scottknott(resp,Trat,tab[[1]][4,1],tab[[1]][4,2],sigT)
  if(mcomp=='snk')   snk(resp,Trat,tab[[1]][4,1],tab[[1]][4,2],sigT)
  if(mcomp=='ccboot')ccboot(resp,Trat,tab[[1]][4,1],tab[[1]][4,2],sigT)
  if(mcomp=='ccF')   ccF(resp,Trat,tab[[1]][4,1],tab[[1]][4,2],sigT)
#  if(mcomp=="dnt")  {if(length(cont)==0) stop('Informe o nome do tratamento controle!')
#                     else dunnett(resp,Trat,tab[[1]][4,1],tab[[1]][4,2],cont=cont,proc="dnt",alpha=sigT)}
#  if(mcomp=="sddnt"){if(length(cont)==0) stop('Informe o nome do tratamento controle!')
#                     else dunnett(resp,Trat,tab[[1]][4,1],tab[[1]][4,2],cont=cont,proc="sddnt",alpha=sigT)}
                     }

else reg<-reg.poly(resp, trat, tab[[1]][4,1], tab[[1]][4,2], tab[[1]][1,1], tab[[1]][1,2])

                       }
else {
    cat('\nDe acordo com o teste F, as medias nao podem ser consideradas diferentes.\n')
mean.table<-tapply.stat(resp,trat,mean)
colnames(mean.table)<-c('Niveis','Medias')
print(mean.table)
cat('------------------------------------------------------------------------\n')
}
#residuals<-anava$residuals
#coefficients<-anava$coefficients
#effects<-anava$effects
#fitted.values<-anava$fitted.values
#means.trat<-tapply.stat(resp,trat,mean)
#if(quali==FALSE) {
#invisible(list(residuals=residuals, means.trat=means.trat,
#coefficients=coefficients, effects=effects,
#fitted.values=fitted.values,Regressao=reg)) }

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
