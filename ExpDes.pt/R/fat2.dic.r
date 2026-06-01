#' Fatorial duplo em DIC
#'
#' \code{fat2.dic} Analisa experimentos em fatorial duplo
#' em Delineamento Inteiramente
#' Casualizado balanceado, considerando o modelo fixo.
#' @param fator1 Vetor numerico ou complexo contendo os niveis
#' do fator 1.
#' @param fator2 Vetor numerico ou complexo contendo os niveis
#' do fator 2.
#' @param resp Vetor numerico ou complexo contendo a variavel
#' resposta.
#' @param quali Logico, se TRUE (default) na primeira posicao,
#' os niveis do fator 1 sao entendidos como qualitativos, se
#' FALSE, quantitativos; da mesma forma, a segunda posicao e
#' referente aos niveis do fator 2.
#' @param mcomp Permite escolher o teste de comparacao multipla;
#' o \emph{default} e o teste de Tukey, contudo tem-se como
#' outras opcoes: o teste LSD ('lsd'), o teste LSDB ('lsdb'),
#' o teste de Duncan ('duncan'), o teste de SNK ('snk'), o
#' teste de Scott-Knott ('sk'), o teste de comparacoes
#' multiplas bootstrap ('ccboot') e o teste de Calinski e
#' Corsten baseado na distribuicao F ('ccf').
#' @param fac.names Permite nomear os fatores 1 e 2.
#' @param sigT Significancia a ser adotada pelo teste de
#' comparacao multipla de medias; o default e 5\%.
#' @param sigF Significancia a ser adotada pelo teste F da
#' ANAVA; o default e 5\%.
#' @param unfold Orienta os desdobramentos apos a analise de
#' variancia. Se NULL (\emph{default}), sao feitas as analises
#' recomendadas; se '0', e feita apenas a analise de variancia;
#' se '1', os efeitos simples sao estudados; se '2', a interacao
#' dupla e estudada.
#' @details Os argumentos sigT e mcomp so serao utilizados
#' quando os tratamentos forem qualitativos.
#' @return Sao retornados os valores da analise de variancia
#' do DIC em questao, o teste de
#' normalidade de Shapiro-Wilk para os residuos do modelo, o
#' ajuste de modelos de regressao (caso de tratamentos
#' quantitativos) ou os testes de comparacao de medias (caso de
#' tratamentos qualitativos): teste de Tukey, teste de Duncan,
#' teste t de Student (LSD), teste t de Bonferroni, teste de
#' Student-Newman-Keuls (SNK), teste de Scott-Knott e teste de
#' comparacoes multiplas bootstrap; com o desdobramento da
#' interacao, caso esta seja significativa.
#' @references HEALY, M. J. R. The analysis of a factorial
#' experiment with additional treatments. Journal of
#' Agricultural Science, Cambridge, v. 47, p. 205-206.
#' 1956.
#' @author Eric B Ferreira,
#'\email{eric.ferreira@@unifal-mg.edu.br}
#' @author Denismar Alves Nogueira
#' @author Portya Piscitelli Cavalcanti
#' @note O \code{\link{graficos}} pode ser usado para
#' construir os graficos da regressao e o
#' \code{\link{plotres}} para analise do residuo da anava.
#' @seealso \code{\link{fat2.dbc}},
#' \code{\link{fat3.dic}}, \code{\link{fat3.dbc}},
#' \code{\link{fat2.ad.dic}}, \code{\link{fat2.ad.dbc}},
#' \code{\link{fat3.ad.dic}} and \code{\link{fat3.ad.dbc}}.
#' @examples
#' data(ex4)
#' attach(ex4)
#' fat2.dic(revol,esterco,zn,quali=c(FALSE,TRUE),mcomp="tukey",
#' fac.names=c("Revolvimento","Esterco"),sigT = 0.05,
#' sigF = 0.05, unfold=NULL)
#' @export

fat2.dic<-function(fator1,
 fator2,
 resp,
 quali=c(TRUE,TRUE),
 mcomp='tukey',
 fac.names=c('F1','F2'),
 sigT=0.05,
 sigF=0.05,
 unfold=NULL) {

cat('------------------------------------------------------------------------\nLegenda:\n')
cat('FATOR 1: ',fac.names[1],'\n')
cat('FATOR 2: ',fac.names[2],'\n------------------------------------------------------------------------\n\n')

fatores<-cbind(fator1,fator2)
Fator1<-factor(fator1)
Fator2<-factor(fator2)
nv1<-length(summary(Fator1)) #Diz quantos niveis tem o fator 1.
nv2<-length(summary(Fator2)) #Diz quantos niveis tem o fator 2.
lf1<-levels(Fator1)
lf2<-levels(Fator2)
anava<-aov(resp~Fator1*Fator2)
tab<-summary(anava)
colnames(tab[[1]])<-c('GL','SQ','QM','Fc','Pr>Fc')
tab[[1]]<-rbind(tab[[1]],c(apply(tab[[1]],2,sum)))
rownames(tab[[1]])<-c(fac.names[1],fac.names[2],paste(fac.names[1],'*',fac.names[2],sep=''),'Residuo','Total')
cv<-round(sqrt(tab[[1]][4,3])/mean(resp)*100, 2)
tab[[1]][5,3]=' '
cat('\nQuadro da analise de variancia\n------------------------------------------------------------------------\n')
print(tab[[1]])
cat('------------------------------------------------------------------------\nCV =',cv,'%\n')

#Teste de normalidade
pvalor.shapiro<-shapiro.test(anava$residuals)$p.value
cat('\n------------------------------------------------------------------------
Teste de normalidade dos residuos (Shapiro-Wilk)\n')
cat('valor-p: ',pvalor.shapiro, '\n')
if(pvalor.shapiro<0.05){cat('ATENCAO: a 5% de significancia, os residuos nao podem ser considerados normais!
------------------------------------------------------------------------\n')}
else{cat('De acordo com o teste de Shapiro-Wilk a 5% de significancia, os residuos podem ser considerados normais.
------------------------------------------------------------------------\n')}

# Creating unfold #########################################
if(is.null(unfold)){
if(tab[[1]][3,5]>sigF){unfold<-c(unfold,1)}
if(tab[[1]][3,5]<=sigF) {unfold<-c(unfold,2)}
}

#Para interacao nao significativa, fazer...
if(any(unfold==1)) {
cat('\nInteracao nao significativa: analisando os efeitos simples
------------------------------------------------------------------------\n')
fatores<-data.frame('fator 1'=fator1,'fator 2' = fator2)

for(i in 1:2){

#Para os fatores QUALITATIVOS, teste de Tukey
if(quali[i]==TRUE && tab[[1]][i,5]<=sigF) {
cat(fac.names[i])
if(mcomp=='tukey'){
tukey(resp,fatores[,i],tab[[1]][4,1],tab[[1]][4,2],sigT)
}
if(mcomp=='duncan'){
duncan(resp,fatores[,i],tab[[1]][4,1],tab[[1]][4,2],sigT)
}
if(mcomp=='lsd'){
lsd(resp,fatores[,i],tab[[1]][4,1],tab[[1]][4,2],sigT)
}
if(mcomp=='lsdb'){
lsdb(resp,fatores[,i],tab[[1]][4,1],tab[[1]][4,2],sigT)
}
if(mcomp=='sk'){
scottknott(resp,fatores[,i],tab[[1]][4,1],tab[[1]][4,2],sigT)
}
if(mcomp=='snk'){
snk(resp,fatores[,i],tab[[1]][4,1],tab[[1]][4,2],sigT)
}
if(mcomp=="ccboot"){
ccboot(resp,fatores[,i],tab[[1]][4,1],tab[[1]][4,2],sigT)
 }
if(mcomp=="ccF"){
ccF(resp,fatores[,i],tab[[1]][4,1],tab[[1]][4,2],sigT)
 }
 }
if(quali[i]==TRUE && tab[[1]][i,5]>sigF) {
cat(fac.names[i])
cat('\nDe acordo com o teste F, as medias desse fator sao estatisticamente iguais.\n')
cat('------------------------------------------------------------------------\n')
mean.table<-tapply.stat(resp,fatores[,i],mean)
colnames(mean.table)<-c('Niveis','Medias')
print(mean.table)
cat('------------------------------------------------------------------------')
}

#Para os fatores QUANTITATIVOS, regressao
if(quali[i]==FALSE && tab[[1]][i,5]<=sigF){
cat(fac.names[i])
reg.poly(resp, fatores[,i], tab[[1]][4,1], tab[[1]][4,2], tab[[1]][i,1], tab[[1]][i,2])
}

if(quali[i]==FALSE && tab[[1]][i,5]>sigF) {
cat(fac.names[i])
cat('\nDe acordo com o teste F, as medias desse fator sao estatisticamente iguais.\n\n')
cat('------------------------------------------------------------------------\n')
mean.table<-tapply.stat(resp,fatores[,i],mean)
colnames(mean.table)<-c('Niveis','Medias')
print(mean.table)
cat('------------------------------------------------------------------------')
}
cat('\n')
}
}

#Se a interacao for significativa, desdobrar a interacao
if(any(unfold==2)){
cat("\n\n\nInteracao significativa: desdobrando a interacao
------------------------------------------------------------------------\n")

#Desdobramento de FATOR 1 dentro do niveis de FATOR 2
cat("\nDesdobrando ", fac.names[1], ' dentro de cada nivel de ', fac.names[2], '
------------------------------------------------------------------------\n')

des1<-aov(resp~Fator2/Fator1)

l1<-vector('list',nv2)
names(l1)<-names(summary(Fator2))
v<-numeric(0)
for(j in 1:nv2) {
for(i in 0:(nv1-2)) v<-cbind(v,i*nv2+j)
l1[[j]]<-v
v<-numeric(0)
}
des1.tab<-summary(des1,split=list('Fator2:Fator1'=l1))[[1]]

#Montando a tabela de ANAVA do des1
glb=nv2-1

glf1=c(as.numeric(des1.tab[3:(nv2+2),1]))
glE=tab[[1]][4,1]
glT=tab[[1]][5,1]

SQb=tab[[1]][2,2]
SQf1=c(as.numeric(des1.tab[3:(nv2+2),2]))
SQE=tab[[1]][4,2]
SQT=tab[[1]][5,2]

QMb=SQb/glb
QMf1=SQf1/glf1
QME=SQE/glE
QMT=SQT/glT

Fcb=QMb/QME
Fcf1=QMf1/QME

rn<-numeric(0)
for(j in 1:nv2){ rn<-c(rn, paste(paste(fac.names[1],':',fac.names[2],sep=''),lf2[j]))}

anavad1<-data.frame("GL"=c(round(c(glb, glf1, glE, glT))),
"SQ"=c(round(c(SQb,SQf1,SQE,SQT),5)),
"QM"=c(round(c(QMb,QMf1,QME,QMT),5)),
"Fc"=c(round(c(Fcb,Fcf1),4),' ',' '),
"Pr>Fc"=c(round(c(1-pf(Fcb,glb,glE),1-pf(Fcf1,glf1,glE)),4),'', ''))
rownames(anavad1)=c(fac.names[2],rn,"Residuo","Total")
cat('------------------------------------------------------------------------
Quadro da analise de variancia\n------------------------------------------------------------------------\n')
print(anavad1)
cat('------------------------------------------------------------------------\n\n')


for(i in 1:nv2) {
if(des1.tab[(i+2),5]<=sigF){
if(quali[1]==TRUE){
cat('\n\n',fac.names[1],' dentro do nivel ',lf2[i],' de ',fac.names[2],'
------------------------------------------------------------------------')
if(mcomp=='tukey'){
tukey(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
}
if(mcomp=='duncan'){
duncan(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
 }
if(mcomp=='lsd'){
lsd(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
}
if(mcomp=='lsdb'){
lsdb(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
 }
if(mcomp=='sk'){
scottknott(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
 }
if(mcomp=='snk'){
snk(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
}
if(mcomp=="ccboot"){
ccboot(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
 }
if(mcomp=="ccF"){
ccF(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
 }
}
else{#regressao
cat('\n\n',fac.names[1],' dentro do nivel ',lf2[i],' de ',fac.names[2],'
------------------------------------------------------------------------')
reg.poly(resp[Fator2==lf2[i]], fator1[Fator2==lf2[i]], tab[[1]][4,1], tab[[1]][4,2], des1.tab[i+2,1], des1.tab[i+2,2])
}
}
else{cat('\n\n',fac.names[1],' dentro do nivel ',lf2[i],' de ',fac.names[2],'\n')
cat('\nDe acordo com o teste F, as medias desse fator sao estatisticamente iguais.\n')
cat('------------------------------------------------------------------------\n')
mean.table<-tapply.stat(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],mean)
colnames(mean.table)<-c('Niveis','Medias')
print(mean.table)
cat('------------------------------------------------------------------------\n')
}
 }
cat('\n\n')

#Desdobramento de FATOR 2 dentro do niveis de FATOR 1
cat("\nDesdobrando ", fac.names[2], ' dentro de cada nivel de ', fac.names[1], '
------------------------------------------------------------------------\n')

des2<-aov(resp~Fator1/Fator2)

l2<-vector('list',nv1)
names(l2)<-names(summary(Fator1))
v<-numeric(0)
for(j in 1:nv1) {
for(i in 0:(nv2-2)) v<-cbind(v,i*nv1+j)
l2[[j]]<-v
v<-numeric(0)
}
des2.tab<-summary(des2,split=list('Fator1:Fator2'=l2))[[1]]

#Montando a tabela de ANAVA do des2
gla=nv1-1
glf2=c(as.numeric(des2.tab[3:(nv1+2),1]))

SQa=tab[[1]][1,2]
SQf2=c(as.numeric(des2.tab[3:(nv1+2),2]))

QMa=SQa/gla
QMf2=SQf2/glf2

Fca=QMa/QME
Fcf2=QMf2/QME

rn<-numeric(0)
for(i in 1:nv1){ rn<-c(rn, paste(paste(fac.names[2],':',fac.names[1],sep=''),lf1[i]))}

anavad2<-data.frame("GL"=c(round(c(gla, glf2, glE, glT))),
"SQ"=c(round(c(SQa,SQf2,SQE,SQT),5)),
"QM"=c(round(c(QMa,QMf2,QME,QMT),5)),
"Fc"=c(round(c(Fca,Fcf2),4),' ',' '),
"Pr>Fc"=c(round(c(1-pf(Fca,gla,glE),1-pf(Fcf2,glf2,glE)),4),'', ''))
rownames(anavad2)=c(fac.names[1],rn,"Residuo","Total")
cat('------------------------------------------------------------------------
Quadro da analise de variancia\n------------------------------------------------------------------------\n')
print(anavad2)
cat('------------------------------------------------------------------------\n\n')


for(i in 1:nv1) {
if(des2.tab[(i+2),5]<=sigF){
if(quali[2]==TRUE){
cat('\n\n',fac.names[2],' dentro do nivel ',lf1[i],' de ',fac.names[1],'
------------------------------------------------------------------------')
if(mcomp=='tukey'){
tukey(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
}
if(mcomp=='duncan'){
duncan(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
 }
if(mcomp=='lsd'){
lsd(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
}
if(mcomp=='lsdb'){
lsdb(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
 }
if(mcomp=='sk'){
scottknott(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
 }
if(mcomp=='snk'){
snk(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
}
if(mcomp=="ccboot"){
ccboot(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
 }
if(mcomp=="ccF"){
ccF(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],tab[[1]][4,1],tab[[1]][4,2],sigT)
 }
}
else{#regressao
cat('\n\n',fac.names[2],' dentro do nivel ',lf1[i],' de ',fac.names[1],'
------------------------------------------------------------------------')
reg.poly(resp[Fator1==lf1[i]], fator2[Fator1==lf1[i]], tab[[1]][4,1], tab[[1]][4,2], des2.tab[i+2,1], des2.tab[i+2,2])
}
 }
else{cat('\n\n',fac.names[2],' dentro do nivel ',lf1[i],' de ',fac.names[1],'\n')
cat('\nDe acordo com o teste F, as medias desse fator sao estatisticamente iguais.\n')
cat('------------------------------------------------------------------------\n')
mean.table<-tapply.stat(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],mean)
colnames(mean.table)<-c('Niveis','Medias')
print(mean.table)
cat('------------------------------------------------------------------------\n')
}

}
}
#Saida
out<-list()
out$residuos<-anava$residuals
out$gl.residual<-anava$df.residual
out$coeficientes<-anava$coefficients
out$efeitos<-anava$effects
out$valores.ajustados<-anava$fitted.values
out$medias.fator1<-tapply.stat(resp,fatores[,1],mean)
out$medias.fator2<-tapply.stat(resp,fatores[,2],mean)
tabmedia<-model.tables(anava, "means")
out$medias.dentro12<-tabmedia$tables$`Fator1:Fator2`
#if(quali==FALSE && tab[[1]][1,5]<sigF) {out$reg<-reg}
invisible(out)
}

