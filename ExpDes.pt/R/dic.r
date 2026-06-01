#' Delineamento Inteiramente Casualizado Simples
#'
#' \code{dic} Analisa experimentos em Delineamento Inteiramente
#' Casualizado balanceado com um so fator, considerando o
#' modelo fixo.
#' @param trat Vetor numerico ou complexo contendo os
#' tratamentos.
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
#' variancias; o \emph{default} e o teste de Bartlett
#' ("bartlett"), contudo tem-se como outras opcoes: o
#' teste de Levene ("levene"), Samiuddin ("samiuddin"),
#' ONeill e Mathews ("oneilmathews") e Layard ("layard").
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
#' DIC em questao, o teste de normalidade de Shapiro-Wilk para
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
#' FERREIRA, E. B.; CAVALCANTI, P. P. Funcao em codigo R para
#' analisar experimentos em DIC simples, em uma so rodada. In:
#' REUNIAO ANUAL DA REGIAO BRASILEIRA DA SOCIEDADE
#' INTERNACIONAL DE BIOMETRIA, 54./SIMPOSIO DE ESTATISTICA
#' APLICADA A EXPERIMENTACAO AGRONOMICA, 13., 2009, Sao Carlos.
#' Programas e resumos... Sao Carlos, SP: UFSCar, 2009. p. 1-5.
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#' @author Denismar Alves Nogueira
#' @author Portya Piscitelli Cavalcanti
#' @note O \code{\link{graficos}} pode ser usado para
#' construir os graficos da regressao e o
#' \code{\link{plotres}} para analise do residuo da anava.
#' @seealso \code{\link{fat2.dic}}, \code{\link{fat3.dic}},
#' \code{\link{psub2.dic}}, \code{\link{fat2.ad.dic}} and
#' \code{\link{fat3.ad.dic}}.
#' @examples
#' data(ex1)
#' attach(ex1)
#' dic(trat, ig, quali = FALSE, sigF = 0.05, unfold=NULL)
#' @export

dic<-function(trat,
              resp,
              quali=TRUE,
              mcomp='tukey',
              nl=FALSE,
              hvar='bartlett',
              sigT=0.05,
              sigF=0.05,
              unfold=NULL) {

Trat<-factor(trat)
anava<-aov(resp~Trat)
tab<-summary(anava)

#### Encontrar o vetor com o numero de repeti??es ####
#i<-0
#ii<-1
#rr<-1
t<-length(levels(Trat))
#r<-matrix(0,t,1)
#for(i in 1:(length(Trat)-1)) {
#  if (Trat[i]==Trat[i+1]) {rr<-rr+1} else {r[ii]<-rr}
#  if (Trat[i]!=Trat[i+1]) rr<-1
#  if (Trat[i]!=Trat[i+1]) ii<-ii+1
#  if ((i+1)==length(Trat)){r[ii]<-rr}
#}
r<-as.numeric(table(Trat))
#########################################

colnames(tab[[1]])<-c('GL','SQ','QM','Fc','Pr>Fc')
tab[[1]]<-rbind(tab[[1]],c(apply(tab[[1]],2,sum)))
rownames(tab[[1]])<-c('Tratamento','Residuo','Total')
cv<-round(sqrt(tab[[1]][2,3])/mean(resp)*100, 2)
tab[[1]][3,3]=NA
cat('------------------------------------------------------------------------
Quadro da analise de variancia\n------------------------------------------------------------------------\n')
print(tab[[1]])
cat('------------------------------------------------------------------------\nCV =',cv,'%\n')

#Teste de normalidade
#pvalor.shapiro<-shapiro.test(anava$residuals)$p.value
#cat('\n------------------------------------------------------------------------\nTeste de normalidade dos residuos \n')
#cat('Valor-p: ',pvalor.shapiro, '\n')
#if(pvalor.shapiro<0.05){cat('ATENCAO: a 5% de significancia, os residuos nao podem ser considerados normais!
#------------------------------------------------------------------------\n')}
#else{cat('De acordo com o teste de Shapiro-Wilk a 5% de significancia, os residuos podem ser considerados normais.
#------------------------------------------------------------------------\n')}


if (length(anava$residuals)<5000)
  {
    pvalor.normal<-shapiro.test(anava$residuals)$p.value
    nome.teste<-"Shapiro-Wilk"
  }
else
  {
    pvalor.normal<-ks.test(anava$residuals,rnorm(length(anava$residuals),0,sqrt(as.numeric(tab[[1]][2,3]))))$p.value
    nome.teste<-"Kolmogorov-Smirnov"
  }
cat('\n------------------------------------------------------------------------\nTeste de normalidade dos residuos (',nome.teste,') \n')
cat('Valor-p: ',pvalor.normal, '\n')
if(pvalor.normal<0.05){cat('ATENCAO: a 5% de significancia, os residuos nao podem ser considerados normais!\n')
cat('------------------------------------------------------------------------\n')}
else{cat('De acordo com o teste de',nome.teste,'a 5% de significancia, os residuos podem ser considerados normais.\n')
cat('------------------------------------------------------------------------\n')}

#Teste de homogeneidade de vari?ncia
if(hvar=='bartlett') pvalor.hvar<-bartlett(trat,resp,t,r)
if(hvar=='levene') pvalor.hvar<-levene(trat,resp,t,r)
if(hvar=='samiuddin') pvalor.hvar<-samiuddin(trat,resp,t,r)
if(hvar=='oneillmathews') pvalor.hvar<-oneillmathews(trat,resp,t,r)
if(hvar=='layard') pvalor.hvar<-layard(trat,resp,t,r)

cat('\n------------------------------------------------------------------------\nTeste de homogeneidade de variancia \n')
cat('valor-p: ',pvalor.hvar, '\n')
if(pvalor.hvar<0.05){cat('ATENCAO: a 5% de significancia, as variancias nao podem ser consideradas homogeneas!\n')
cat('------------------------------------------------------------------------\n')}
else{cat('De acordo com o teste de',hvar,'a 5% de significancia, as variancias podem ser consideradas homogeneas.\n')
cat('------------------------------------------------------------------------\n')}

# Creating unfold #########################################
if(is.null(unfold)){
  if(tab[[1]][1,5]<=sigF) {unfold<-c(unfold,1)}
}

#Para fator significativo, fazer...
if(any(unfold==1)) {

if(quali==TRUE) {

  if(mcomp=='tukey') tukey(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  if(mcomp=='duncan')duncan(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  if(mcomp=='lsd')   lsd(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  if(mcomp=='lsdb')  lsdb(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  if(mcomp=='sk')    scottknott(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  if(mcomp=='snk')   snk(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  if(mcomp=="ccboot")ccboot(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)
  if(mcomp=="ccF")   ccF(resp,Trat,tab[[1]][2,1],tab[[1]][2,2],sigT)

                }

else if(nl==FALSE) reg<-reg.poly(resp, trat, tab[[1]][2,1],
      tab[[1]][2,2], tab[[1]][1,1], tab[[1]][1,2])
else if(nl==TRUE)  reg<-reg.nl(resp, trat)

                       }

else {
    cat('\nDe acordo com o teste F, as medias nao podem ser consideradas diferentes.\n')
    cat('------------------------------------------------------------------------\n')
mean.table<-tapply.stat(resp,trat,mean)
colnames(mean.table)<-c('Niveis','Medias')
print(mean.table)
cat('------------------------------------------------------------------------\n')
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

