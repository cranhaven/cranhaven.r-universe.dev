#' Fatorial duplo com um tratamento adicional em DIC
#'
#' \code{fat2.ad.dic} Analisa experimentos em fatorial duplo
#' com um tratamento adicional em Delineamento Inteiramente
#' Casualizado balanceado, considerando o modelo fixo.
#' @param fator1 Vetor numerico ou complexo contendo os niveis
#' do fator 1.
#' @param fator2 Vetor numerico ou complexo contendo os niveis
#' do fator 2.
#' @param repet Vetor numerico ou complexo contendo as
#' repeticoes.
#' @param resp Vetor numerico ou complexo contendo a variavel
#' resposta.
#' @param respAd Vetor numerico ou complexo contendo a variavel
#' resposta do tratamento adicional.
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
#' do DIC em questao com um tratamento adicional,o teste de
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
#' @seealso \code{\link{fat2.dic}}, \code{\link{fat2.dbc}},
#' \code{\link{fat3.dic}}, \code{\link{fat3.dbc}},
#' \code{\link{fat2.ad.dbc}},
#' \code{\link{fat3.ad.dic}} and \code{\link{fat3.ad.dbc}}.
#' @examples
#' data(ex8)
#' attach(ex8)
#' data(secaAd)
#' fat2.ad.dic(inoculante, biodiesel, vaso, seca, secaAd,
#' quali = c(TRUE,FALSE), mcomp = "tukey", fac.names =
#' c("Inoculante", "Biodiesel"), sigT = 0.05, sigF = 0.05,
#' unfold=NULL)
#' @export

fat2.ad.dic<-function(fator1,
fator2,
repet,
resp,
respAd,
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
nv1<-length(summary(Fator1))
nv2<-length(summary(Fator2))
lf1<-levels(Fator1)
lf2<-levels(Fator2)
J=length(respAd)
n.trat2<-nv1*nv2

#ANAVA do fatorial 2
anavaF2<-summary(aov(resp~Fator1*Fator2))
(SQa<-anavaF2[[1]][1,2])
(SQb<-anavaF2[[1]][2,2])
(SQab<-anavaF2[[1]][3,2])

#Anava de todos os tratamentos do experimento (fatorial 2 + adicional)
col1<-numeric(0)
for(i in 1:n.trat2) {
col1<-c(col1, rep(i,J))
}
col1<-c(col1,rep('ad',J))
col2<-c(repet,rep(1:J))
col3<-c(resp,respAd)
tabF2ad<-data.frame("TRAT2"=col1, "REP"=col2, "RESP2"=col3)
TRAT2<-factor(tabF2ad[,1])
anava<-aov(tabF2ad[,3]~TRAT2)
anavaTr<-summary(anava)
SQad<-anavaTr[[1]][1,2] - (SQa+SQb+SQab)
SQE<-anavaTr[[1]][2,2]
SQT<-anavaTr[[1]][1,2]+anavaTr[[1]][2,2]

gla=nv1-1
glb=nv2-1
glab=(nv1-1)*(nv2-1)
glad=1
glE=(nv1*nv2+1)*(J-1)
glT=(nv1*nv2+1)*J-1
QMa=SQa/gla
QMb=SQb/glb
QMab=SQab/glab
QMad=SQad/glad
QME=SQE/glE
QMT=SQT/glT
Fca=QMa/QME
Fcb=QMb/QME
Fcab=QMab/QME
Fcad=QMad/QME
pv.fs=c(1-pf(Fca,gla,glE), 1-pf(Fcb,glb,glE))

#Montando a tabela da ANAVA
anavaT<-data.frame("GL"=c(gla, glb, glab, glad, glE, glT ),
"SQ"=c(round(c(SQa,SQb,SQab,SQad,SQE,SQT),5)),
"QM"=c(round(c(QMa,QMb,QMab,QMad,QME),5),''),
"Fc"=c(round(c(Fca,Fcb,Fcab,Fcad),4),'',''),
"Pr>Fc"=c(round(c(pv.fs, 1-pf(Fcab,glab,glE), 1-pf(Fcad,glad,glE)),4),'', ''))
colnames(anavaT)[5]="Pr>Fc"
rownames(anavaT)=c(fac.names[1],fac.names[2],paste(fac.names[1],'*',fac.names[2],sep=''),"Ad vs Fatorial","Residuo","Total")
cat('------------------------------------------------------------------------
Quadro da analise de variancia\n------------------------------------------------------------------------\n')
print(anavaT)
cat('------------------------------------------------------------------------\n')
#CV
cv<-round(sqrt(QME)/mean(col3)*100, 2)
cat('CV =',cv,'%\n')

#Teste de normalidade
pvalor.shapiro<-shapiro.test(anava$residuals)$p.value
cat('\n------------------------------------------------------------------------
Teste de normalidade dos residuos (Shapiro-Wilk)\n')
cat('valor-p: ',pvalor.shapiro, '\n')
if(pvalor.shapiro<0.05){cat('ATENCAO: a 5% de significancia, os residuos nao podem ser considerados normais!
------------------------------------------------------------------------\n')}
else{cat('De acordo com o teste de Shapiro-Wilk a 5% de significancia, os residuos podem ser considerados normais.
------------------------------------------------------------------------\n\n')}

#Contraste Ad vs Fatorial
cat('Contraste do tratamento adicional com o fatorial
------------------------------------------------------------------------\n')
x<-mean(respAd)
y<-mean(resp)

if(1-pf(Fcad,glad,glE)>sigF) { C1<-data.frame("Medias"=c(x,y))
rownames(C1)=c("Adicional","Fatorial")
colnames(C1)<-c("Medias")
cat('De acordo com o teste F, as medias dos dois grupos sao estatisticamente iguais.\n')
print(C1) }else{
C2<-data.frame("Media"=c(x,y),
" "=c(letters[1],letters[2]))
rownames(C2)=c("Adicional","Fatorial")
colnames(C2)<-c("Medias"," ")
print(C2)
}
cat('------------------------------------------------------------------------\n')

# Creating unfold #########################################
if(is.null(unfold)){
if(1-pf(Fcab,glab,glE)>sigF){unfold<-c(unfold,1)}
if(1-pf(Fcab,glab,glE)<=sigF) {unfold<-c(unfold,2)}
}

#Para interacao nao significativa, fazer...
if(any(unfold==1)) {
cat('\nInteracao nao significativa: analisando os efeitos simples
------------------------------------------------------------------------\n')
fatores<-data.frame('fator 1'=fator1,'fator 2' = fator2)

for(i in 1:2){

#Para os fatores QUALITATIVOS, teste de Tukey
if(quali[i]==TRUE && pv.fs[i]<=sigF) {
cat(fac.names[i])
if(mcomp=='tukey'){tukey(resp,fatores[,i],anavaT[5,1],anavaT[5,2],sigT)}
if(mcomp=='duncan'){duncan(resp,fatores[,i],anavaT[5,1],anavaT[5,2],sigT)}
if(mcomp=='lsd'){lsd(resp,fatores[,i],anavaT[5,1],anavaT[5,2],sigT)}
if(mcomp=='lsdb'){lsdb(resp,fatores[,i],anavaT[5,1],anavaT[5,2],sigT)}
if(mcomp=='sk'){scottknott(resp,fatores[,i],anavaT[5,1],anavaT[5,2],sigT)}
if(mcomp=='snk'){snk(resp,fatores[,i],anavaT[5,1],anavaT[5,2],sigT)}
if(mcomp=="ccboot"){ccboot(resp,fatores[,i],anavaT[5,1],anavaT[5,2],sigT)}
if(mcomp=="ccF"){ccF(resp,fatores[,i],anavaT[5,1],anavaT[5,2],sigT)}
}
if(quali[i]==TRUE && pv.fs[i]>sigF) {
cat(fac.names[i])
cat('\nDe acordo com o teste F, as medias desse fator sao estatisticamente iguais.\n')
cat('------------------------------------------------------------------------\n')
mean.table<-tapply.stat(resp,fatores[,i],mean)
colnames(mean.table)<-c('Niveis','Medias')
print(mean.table)
cat('------------------------------------------------------------------------')
}

#Para os fatores QUANTITATIVOS, regressao
if(quali[i]==FALSE && pv.fs[i]<=sigF){
cat(fac.names[i])
reg.poly(resp, fatores[,i], anavaT[5,1], anavaT[5,2], anavaT[i,1], anavaT[i,2])
}

if(quali[i]==FALSE && pv.fs[i]>sigF) {
cat(fac.names[i])
cat('\nDe acordo com o teste F, as medias desse fator sao estatisticamente iguais.\n')
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
glf1=c(as.numeric(des1.tab[3:(nv2+2),1]))
SQf1=c(as.numeric(des1.tab[3:(nv2+2),2]))
QMf1=SQf1/glf1
Fcf1=QMf1/QME
rn<-numeric(0)
for(j in 1:nv2){ rn<-c(rn, paste(paste(fac.names[1],':',
 fac.names[2],sep=''),
 lf2[j]))}

anavad1<-data.frame("GL"=c(glb, glf1, glad, glE, glT),
"SQ"=c(round(c(SQb,SQf1,SQad,SQE,SQT),5)),
"QM"=c(round(c(QMb,QMf1,QMad,QME),5),''),
"Fc"=c(round(c(Fcb,Fcf1,Fcad),4),'',''),
"Pr>Fc"=c(round(c(1-pf(Fcb,glb,glE),1-pf(Fcf1,glf1,glE), 1-pf(Fcad,glad,glE)),4),'', ''))
colnames(anavad1)[5]="Pr>Fc"
rownames(anavad1)=c(fac.names[2],rn,"Ad vs Fatorial","Residuo","Total")
cat('------------------------------------------------------------------------
Quadro da analise de variancia\n------------------------------------------------------------------------\n')
print(anavad1)
cat('------------------------------------------------------------------------\n\n')


ii<-0
for(i in 1:nv2) {
ii<-ii+1
if(1-pf(Fcf1,glf1,glE)[ii]<=sigF){
if(quali[1]==TRUE){
cat('\n\n',fac.names[1],' dentro do nivel ',lf2[i],' de ',fac.names[2],'
------------------------------------------------------------------------')
if(mcomp=='tukey'){
tukey(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],anavaT[5,1],anavaT[5,2],sigT)
}
if(mcomp=='duncan'){
duncan(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],anavaT[5,1],anavaT[5,2],sigT)
 }
if(mcomp=='lsd'){
lsd(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],anavaT[5,1],anavaT[5,2],sigT)
}
if(mcomp=='lsdb'){
lsdb(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],anavaT[5,1],anavaT[5,2],sigT)
 }
if(mcomp=='sk'){
scottknott(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],anavaT[5,1],anavaT[5,2],sigT)
 }
if(mcomp=='snk'){
snk(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],anavaT[5,1],anavaT[5,2],sigT)
}
if(mcomp=="ccboot"){
ccboot(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],anavaT[5,1],anavaT[5,2],sigT)
 }
if(mcomp=="ccF"){
ccF(resp[Fator2==lf2[i]],fatores[,1][Fator2==lf2[i]],anavaT[5,1],anavaT[5,2],sigT)
 }
}
else{#regressao
cat('\n\n',fac.names[1],' dentro do nivel ',lf2[i],' de ',fac.names[2],'
------------------------------------------------------------------------')
reg.poly(resp[Fator2==lf2[i]], fator1[Fator2==lf2[i]], anavaT[5,1], anavaT[5,2], anavad1[i+1,1], anavad1[i+1,2])
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
glf2=c(as.numeric(des2.tab[3:(nv1+2),1]))
SQf2=c(as.numeric(des2.tab[3:(nv1+2),2]))
QMf2=SQf2/glf2
Fcf2=QMf2/QME
rn<-numeric(0)
for(i in 1:nv1){ rn<-c(rn, paste(paste(fac.names[2],':',fac.names[1],sep=''),lf1[i]))}

anavad2<-data.frame("GL"=c(gla, glf2, glad, glE, glT),
"SQ"=c(round(c(SQa,SQf2,SQad,SQE,SQT),5)),
"QM"=c(round(c(QMa,QMf2,QMad,QME),5),''),
"Fc"=c(round(c(Fca,Fcf2,Fcad),4),'',''),
"Pr>Fc"=c(round(c(1-pf(Fca,gla,glE), 1-pf(Fcf2,glf2,glE), 1-pf(Fcad,glad,glE)),4),'', ''))
colnames(anavad2)[5]="Pr>Fc"
rownames(anavad2)=c(fac.names[1],rn,"Ad vs Fatorial","Residuo","Total")
cat('------------------------------------------------------------------------
Quadro da analise de variancia\n------------------------------------------------------------------------\n')
print(anavad2)
cat('------------------------------------------------------------------------\n\n')


ii<-0
for(i in 1:nv1) {
ii<-ii+1
if(1-pf(Fcf2,glf2,glE)[ii]<=sigF){
if(quali[2]==TRUE){
cat('\n\n',fac.names[2],' dentro do nivel ',lf1[i],' de ',fac.names[1],'
------------------------------------------------------------------------')
if(mcomp=='tukey'){
tukey(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],anavaT[5,1],anavaT[5,2],sigT)
}
if(mcomp=='duncan'){
duncan(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],anavaT[5,1],anavaT[5,2],sigT)
 }
if(mcomp=='lsd'){
lsd(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],anavaT[5,1],anavaT[5,2],sigT)
}
if(mcomp=='lsdb'){
lsdb(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],anavaT[5,1],anavaT[5,2],sigT)
 }
if(mcomp=='sk'){
scottknott(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],anavaT[5,1],anavaT[5,2],sigT)
 }
if(mcomp=='snk'){
snk(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],anavaT[5,1],anavaT[5,2],sigT)
}
if(mcomp=="ccboot"){
ccboot(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],anavaT[5,1],anavaT[5,2],sigT)
 }
if(mcomp=="ccF"){
ccF(resp[Fator1==lf1[i]],fatores[,2][Fator1==lf1[i]],anavaT[5,1],anavaT[5,2],sigT)
 }
}
else{#regressao
cat('\n\n',fac.names[2],' dentro do nivel ',lf1[i],' de ',fac.names[1],'
------------------------------------------------------------------------')
reg.poly(resp[Fator1==lf1[i]], fator2[Fator1==lf1[i]], anavaT[5,1], anavaT[5,2], anavad2[i+1,1], anavad2[i+1,2])
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
out$media.Ad<-x
out$medias.fator1<-tapply.stat(resp,fatores[,1],mean)
out$medias.fator2<-tapply.stat(resp,fatores[,2],mean)
tabmedia<-model.tables(aov(resp~Fator1*Fator2), "means")
out$medias.dentro12<-tabmedia$tables$`Fator1:Fator2`
#if(quali==FALSE && tab[[1]][1,5]<sigF) {out$reg<-reg}
invisible(out)
}
