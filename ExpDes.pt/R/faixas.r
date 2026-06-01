#' Experimentos em faixas
#'
#' \code{faixas} Analisa experimentos em faixas.
#' @param fator1 Vetor numerico ou complexo contendo os niveis
#' do fator 1.
#' @param fator2 Vetor numerico ou complexo contendo os niveis
#' do fator 2.
#' @param bloco Vetor numerico ou complexo contendo os blocos.
#' @param resp Vetor numerico ou complexo contendo a variavel
#' resposta.
#' @param quali Logico, se TRUE (default) na primeira posicao,
#' os niveis do fator 1 sao entendidos como qualitativos, se
#' FALSE, quantitativos; da mesma forma, a segunda posicao e
#' referente aos niveis do fator 2.
#' @param mcomp Permite escolher o teste de comparacao
#' multipla; o \emph{default} e o teste de Tukey, contudo
#' tem-se como outras opcoes: o teste LSD ('lsd'), o teste
#' LSDB ('lsdb'), o teste de Duncan ('duncan'), o teste de SNK
#' ('snk'), o teste de Scott-Knott ('sk'), o teste de
#' comparacoes multiplas bootstrap ('ccboot') e o teste de
#' Calinski e Corsten baseado na distribuicao F ('ccf').
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
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#' @author Laís Brambilla Storti Ferreira
#' @note O \code{\link{graficos}} pode ser usado para
#' construir os graficos da regressao e o
#' \code{\link{plotres}} para analise do residuo da anava.
#' @seealso \code{\link{dbc}}, \code{\link{fat2.dbc}},
#' \code{\link{fat3.dbc}}, \code{\link{psub2.dbc}},
#' \code{\link{fat2.ad.dbc}} e \code{\link{fat3.ad.dbc}}.
#' @examples
#' data(ex5)
#' attach(ex5)
#' faixas(trat, genero, bloco, sabor, quali = c(TRUE,TRUE),
#' mcomp = "tukey", fac.names = c("Amostras","Genero"),
#' sigT = 0.05, sigF = 0.05, unfold=NULL)
#' @export

faixas<-function(fator1,
                 fator2,
                 bloco,
                 resp,
                 quali=c(TRUE,TRUE),
                 mcomp='tukey',
                 fac.names=c('F1','F2'),
                 sigT=0.05,
                 sigF=0.05,
                 unfold=NULL) {

cat('------------------------------------------------------------------------\nLegenda:\n')
cat('FATOR 1 (parcela): ',fac.names[1],'\n')
cat('FATOR 2 (subparcela): ',fac.names[2],'\n------------------------------------------------------------------------\n\n')

cont<-c(2,4)                   #endereco das linhas dos fatores F1 e F2
Fator1<-factor(fator1)
Fator2<-factor(fator2)
bloco<-factor(bloco)
nv1<-length(summary(Fator1))   #Diz quantos niveis tem o fator 1.
nv2<-length(summary(Fator2))   #Diz quantos niveis tem o fator 2.

anava<-aov(resp ~ Fator1*Fator2 + Fator1*bloco + Fator2:bloco)
tab1<-summary(anava)
colnames(tab1[[1]])<-c('GL', 'SQ', 'QM', 'Fc', 'Pr(>Fc)')
tab<-rbind(tab1[[1]],apply(tab1[[1]],2,sum))
tab<-rbind(tab[3,],tab[1,],tab[5,],tab[2,],tab[6,],tab[4,],tab[7,],tab[8,])
rownames(tab)<-c('Bloco',fac.names[1],'Erro a',fac.names[2],'Erro b',paste(fac.names[1],'*',fac.names[2],sep=''),'Erro c','Total')
QMerrobloco<-tab[3,3] + tab[5,3] - tab[7,3]
tab[,4]<-c(tab[1,3]/QMerrobloco,tab[2,3]/tab[3,3],NA,tab[4,3]/tab[5,3],NA,tab[6,3]/tab[7,3],NA,NA)
tab[,5]<-c(1-pf(tab[1,4],tab[1,1],tab[3,1]),1-pf(tab[2,4],tab[2,1],tab[3,1]),NA,
           1-pf(tab[4,4],tab[4,1],tab[5,1]),NA,1-pf(tab[6,4],tab[6,1],tab[7,1]),NA,NA)

tab[8,3]<-NA
cv1=sqrt(tab[3,3])/mean(resp)*100
cv2=sqrt(tab[5,3])/mean(resp)*100
cv3=sqrt(tab[7,3])/mean(resp)*100
tab<-round(tab,6)


cat('------------------------------------------------------------------------
Quadro da analise de variancia\n------------------------------------------------------------------------\n')
print(tab)
cat('------------------------------------------------------------------------
CV 1 =',cv1,'%\nCV 2 =', cv2,'%\nCV 3 =', cv3,'%\n')

fatores<-data.frame('fator 1' = fator1,'fator 2' = fator2)

###############################################################################################################
#Teste de normalidade
#pvalor.shapiro<-shapiro.test(anava$residuals)$p.value
#cat('\n------------------------------------------------------------------------
#Teste de normalidade dos residuos (Shapiro-Wilk)\n')
#cat('p-valor: ',pvalor.shapiro, '\n')
#if(pvalor.shapiro<0.05){cat('ATENCAO: a 5% de significancia, os residuos nao podem ser considerados normais!
#------------------------------------------------------------------------\n')}
#else{cat('De acordo com o teste de Shapiro-Wilk a 5% de significancia, os residuos podem ser considerados normais.
#------------------------------------------------------------------------\n')}

# Creating unfold #########################################
if(is.null(unfold)){
  if(tab[6,5]>sigF)  {unfold<-c(unfold,1)}
  if(tab[6,5]<=sigF) {unfold<-c(unfold,2)}
}

#Para interacao nao significativa, fazer...
if(any(unfold==1)) {
cat('\nInteracao nao significativa: analisando os efeitos simples
------------------------------------------------------------------------\n')

for(i in 1:2){

#Para os fatores QUALITATIVOS, teste de medias
if(quali[i]==TRUE && as.numeric(tab[cont[i],5])<=sigF) {
    cat(fac.names[i])

  if(mcomp=='tukey'){
    tukey(resp,fatores[,i],as.numeric(tab[cont[i]+1,1]), as.numeric(tab[cont[i]+1,2]),sigT)
                    }
  if(mcomp=='duncan'){
    duncan(resp,fatores[,i],as.numeric(tab[cont[i]+1,1]), as.numeric(tab[cont[i]+1,2]),sigT)
                    }
  if(mcomp=='lsd'){
    lsd(resp,fatores[,i],as.numeric(tab[cont[i]+1,1]), as.numeric(tab[cont[i]+1,2]),sigT)
                    }
  if(mcomp=='lsdb'){
    lsdb(resp,fatores[,i],as.numeric(tab[cont[i]+1,1]), as.numeric(tab[cont[i]+1,2]),sigT)
                    }
  if(mcomp=='sk'){
    scottknott(resp,fatores[,i],as.numeric(tab[cont[i]+1,1]), as.numeric(tab[cont[i]+1,2]),sigT)
                    }
  if(mcomp=='snk'){
    snk(resp,fatores[,i],as.numeric(tab[cont[i]+1,1]), as.numeric(tab[cont[i]+1,2]),sigT)
                  }
  if(mcomp=="ccboot"){
  ccboot(resp,fatores[,i],as.numeric(tab[cont[i]+1,1]), as.numeric(tab[cont[i]+1,2]),sigT)
                     }
  if(mcomp=="ccF"){
  ccF(resp,fatores[,i],as.numeric(tab[cont[i]+1,1]), as.numeric(tab[cont[i]+1,2]),sigT)
                     }
          }

if(quali[i]==TRUE && as.numeric(tab[cont[i],5]>sigF)) {
    cat(fac.names[i])
    cat('\nDe acordo com o teste F, as medias desse fator sao estatisticamente iguais.\n')
    cat('------------------------------------------------------------------------\n')
    mean.table<-tapply.stat(resp,fatores[,i],mean)
    colnames(mean.table)<-c('Niveis','Medias')
    print(mean.table)
    cat('------------------------------------------------------------------------')
}

#Para os fatores QUANTITATIVOS, regressao
if(quali[i]==FALSE && as.numeric(tab[cont[i],5])<=sigF){
    cat(fac.names[i])
    reg.poly(resp, fatores[,i], tab[cont[i]+1,1], as.numeric(tab[cont[i]+1,2]), as.numeric(tab[cont[i],1]),
             as.numeric(tab[cont[i],2]))
}

if(quali[i]==FALSE && as.numeric(tab[cont[i],5])>sigF) {
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
if(any(unfold==2)) {
cat("\n\n\nInteracao significativa: desdobrando a interacao
------------------------------------------------------------------------\n")

#Desdobramento de FATOR 1 dentro dos niveis de FATOR 2
cat("\nDesdobrando ", fac.names[1], ' dentro de cada nivel de ', fac.names[2], '
------------------------------------------------------------------------\n')

#Somas de quadrados do fator 1 dentro dos niveis de fator 2
l2<-names(summary(Fator2))

sq<-numeric(0)

for(k in 1:nv2) {
soma<-numeric(0)
for(j in 1:nv1) {
sub<-resp[Fator1==levels(Fator1)[j] & Fator2==levels(Fator2)[k]]
q.som<-length(sub)
soma<-c(soma, sum(sub))
                 }
sq<-c(sq, sum(soma^2)/q.som - sum(soma)^2/(q.som*length(soma)))
                 }
gl.sattert<-(as.numeric(tab[3,3])+(nv2-1)*as.numeric(tab[7,3]))^2/((as.numeric(tab[3,3])^2/as.numeric(tab[3,1]))
             + (((nv2-1)*as.numeric(tab[7,3]))^2/as.numeric(tab[7,1])))
gl.f1f2<-c(rep(nv1-1,nv2),gl.sattert)
sq<-c(sq, NA)
qm.f1f2<-sq[1:nv2]/gl.f1f2[1:nv2]
qm.ecomb<-(as.numeric(tab[3,3])+(nv2-1)*as.numeric(tab[7,3]))/nv2
qm.f1f2<-c(qm.f1f2,qm.ecomb)
fc.f1f2<-c(qm.f1f2[1:nv2]/qm.f1f2[nv2+1],NA)
p.f1f2<-c(1-pf(fc.f1f2,gl.f1f2,gl.sattert))
tab.f1f2<-data.frame('GL'=gl.f1f2,'SQ'=sq,'QM'=qm.f1f2,'Fc'=fc.f1f2, 'valor-p'=p.f1f2)
nome.f1f2<-numeric(0)
for(j in 1:nv2){
nome.f1f2<-c(nome.f1f2, paste(fac.names[1], ' : ', fac.names[2],' ',l2[j],' ',sep=''))
                }
nome.f1f2<-c(nome.f1f2,'Erro combinado')
rownames(tab.f1f2)<-nome.f1f2
tab.f1f2<-round(tab.f1f2,6)
tab.f1f2[nv2+1,2]<-tab.f1f2[nv2+1,3]*tab.f1f2[nv2+1,1]
tab.f1f2[nv2+1,5]<-tab.f1f2[nv2+1,4]<-''
print(tab.f1f2)
    cat('------------------------------------------------------------------------\n\n')

for(i in 1:nv2) {

    cat('\n',fac.names[1], 'dentro de', fac.names[2], l2[i] )
    cat('\n------------------------------------------------------------------------')

  if(quali[1]==TRUE & as.numeric(tab.f1f2[i,5])<=sigF) {

    if(mcomp=='tukey'){
    tukey(resp[fatores[,2]==l2[i]], fatores[,1][fatores[,2]==l2[i]], as.numeric(tab.f1f2[nv2+1,1]),as.numeric(tab.f1f2[nv2+1,2]), sigT)
                      }

  if(mcomp=='duncan'){
    duncan(resp[fatores[,2]==l2[i]],fatores[,1][fatores[,2]==l2[i]],as.numeric(tab.f1f2[nv2+1,1]),as.numeric(tab.f1f2[nv2+1,2]),sigT)
                    }

  if(mcomp=='lsd'){
    lsd(resp[fatores[,2]==l2[i]],fatores[,1][fatores[,2]==l2[i]],as.numeric(tab.f1f2[nv2+1,1]),as.numeric(tab.f1f2[nv2+1,2]),sigT)
                    }

  if(mcomp=='lsdb'){
    lsdb(resp[fatores[,2]==l2[i]],fatores[,1][fatores[,2]==l2[i]],as.numeric(tab.f1f2[nv2+1,1]),as.numeric(tab.f1f2[nv2+1,2]),sigT)
                    }

  if(mcomp=='sk'){
    scottknott(resp[fatores[,2]==l2[i]],fatores[,1][fatores[,2]==l2[i]],as.numeric(tab.f1f2[nv2+1,1]),as.numeric(tab.f1f2[nv2+1,2]),sigT)
                    }

  if(mcomp=='snk'){
    snk(resp[fatores[,2]==l2[i]],fatores[,1][fatores[,2]==l2[i]],as.numeric(tab.f1f2[nv2+1,1]),as.numeric(tab.f1f2[nv2+1,2]),sigT)
                   }
  if(mcomp=="ccboot"){
  ccboot(resp[fatores[,2]==l2[i]],fatores[,1][fatores[,2]==l2[i]],as.numeric(tab.f1f2[nv2+1,1]),as.numeric(tab.f1f2[nv2+1,2]),sigT)
                     }
  if(mcomp=="ccF"){
  ccF(resp[fatores[,2]==l2[i]],fatores[,1][fatores[,2]==l2[i]],as.numeric(tab.f1f2[nv2+1,1]),as.numeric(tab.f1f2[nv2+1,2]),sigT)
                     }
                                                   }

if(quali[1]==FALSE & as.numeric(tab.f1f2[i,5])<sigF) {             #Fazer regressao
    reg.poly(resp[fatores[,2]==l2[i]], fatores[,1][fatores[,2]==l2[i]], as.numeric(tab.f1f2[nv2+1,1]),
    as.numeric(tab.f1f2[nv2+1,2]), as.numeric(tab.f1f2[i,1]), as.numeric(tab.f1f2[i,2]))
                                                   }

if(as.numeric(tab.f1f2[i,5])>sigF) {
    cat('\nDe acordo com o teste F, as medias desse fator sao estatisticamente iguais.\n')
    cat('------------------------------------------------------------------------\n')
    mean.table<-tapply.stat(resp[fatores[,2]==l2[i]],fatores[,1][fatores[,2]==l2[i]],mean)
    colnames(mean.table)<-c('Niveis','Medias')
    print(mean.table)
    cat('------------------------------------------------------------------------\n')
                                   }
                        }


#Desdobramento de FATOR 2 dentro dos niveis de FATOR 1
cat("\n\nDesdobrando ", fac.names[2], ' dentro de cada nivel de ', fac.names[1], '
------------------------------------------------------------------------\n')

#Somas de quadrados do fator 2 dentro dos niveis de fator 1
l1<-names(summary(Fator1))

sq<-numeric(0)

for(k in 1:nv1) {
soma<-numeric(0)
for(j in 1:nv2) {
parc<-resp[Fator1==levels(Fator1)[k] & Fator2==levels(Fator2)[j]]
q.som<-length(parc)
soma<-c(soma, sum(parc))
                 }
sq<-c(sq, sum(soma^2)/q.som - sum(soma)^2/(q.som*length(soma)))
                 }
gl.sattert<-(as.numeric(tab[5,3])+(nv2-1)*as.numeric(tab[7,3]))^2/((as.numeric(tab[5,3])^2/as.numeric(tab[5,1]))
            + (((nv2-1)*as.numeric(tab[7,3]))^2/as.numeric(tab[7,1])))
gl.f2f1<-c(rep(nv2-1,nv1),gl.sattert)
sq<-c(sq, NA)
qm.f2f1<-sq[1:nv1]/gl.f2f1[1:nv1]
qm.ecomb<-(as.numeric(tab[5,3])+(nv1-1)*as.numeric(tab[7,3]))/nv1
qm.f2f1<-c(qm.f2f1,qm.ecomb)
fc.f2f1<-c(qm.f2f1[1:nv1]/qm.f2f1[nv1+1],NA)
p.f2f1<-c(1-pf(fc.f2f1,gl.f2f1,gl.sattert))
tab.f2f1<-data.frame('GL'=gl.f2f1,'SQ'=sq,'QM'=qm.f2f1,'Fc'=fc.f2f1, 'valor-p'=p.f2f1)
nome.f2f1<-numeric(0)
for(j in 1:nv1){
nome.f2f1<-c(nome.f2f1, paste(fac.names[2], ' : ', fac.names[1],' ',l1[j],' ',sep=''))
                }
nome.f2f1<-c(nome.f2f1,'Erro combinado')
rownames(tab.f2f1)<-nome.f2f1
tab.f2f1<-round(tab.f2f1,6)
tab.f2f1[nv1+1,2]<-tab.f2f1[nv1+1,3]*tab.f2f1[nv1+1,1]
tab.f2f1[nv1+1,5]<-tab.f2f1[nv1+1,4]<-''
print(tab.f2f1)
    cat('------------------------------------------------------------------------\n\n')


for(i in 1:nv1) {

    cat('\n',fac.names[2], 'dentro de', fac.names[1], l1[i] )
    cat('\n------------------------------------------------------------------------')


  if(quali[2]==TRUE & as.numeric(tab.f2f1[i,5])<sigF) {             #Fazer teste de comparacao multipla

    if(mcomp=='tukey'){
    tukey(resp[fatores[,1]==l1[i]], fatores[,2][fatores[,1]==l1[i]], as.numeric(tab.f2f1[nv1+1,1]),as.numeric(tab.f2f1[nv1+1,2]),sigT)
                    }

  if(mcomp=='duncan'){
    duncan(resp[fatores[,1]==l1[i]],fatores[,2][fatores[,1]==l1[i]],as.numeric(tab.f2f1[nv1+1,1]),as.numeric(tab.f2f1[nv1+1,2]),sigT)
                    }

  if(mcomp=='lsd'){
    lsd(resp[fatores[,1]==l1[i]],fatores[,2][fatores[,1]==l1[i]],as.numeric(tab.f2f1[nv1+1,1]),as.numeric(tab.f2f1[nv1+1,2]),sigT)
                    }

  if(mcomp=='lsdb'){
    lsdb(resp[fatores[,1]==l1[i]],fatores[,2][fatores[,1]==l1[i]],as.numeric(tab.f2f1[nv1+1,1]),as.numeric(tab.f2f1[nv1+1,2]),sigT)
                    }

  if(mcomp=='sk'){
    scottknott(resp[fatores[,1]==l1[i]],fatores[,2][fatores[,1]==l1[i]],as.numeric(tab.f2f1[nv1+1,1]),as.numeric(tab.f2f1[nv1+1,2]),sigT)
                    }

  if(mcomp=='snk'){
    snk(resp[fatores[,1]==l1[i]],fatores[,2][fatores[,1]==l1[i]],as.numeric(tab.f2f1[nv1+1,1]),as.numeric(tab.f2f1[nv1+1,2]),sigT)
                  }
  if(mcomp=="ccboot"){
  ccboot(resp[fatores[,1]==l1[i]],fatores[,2][fatores[,1]==l1[i]],as.numeric(tab.f2f1[nv1+1,1]),as.numeric(tab.f2f1[nv1+1,2]),sigT)
                     }
  if(mcomp=="ccF"){
  ccF(resp[fatores[,1]==l1[i]],fatores[,2][fatores[,1]==l1[i]],as.numeric(tab.f2f1[nv1+1,1]),as.numeric(tab.f2f1[nv1+1,2]),sigT)
                     }
    cat('------------------------------------------------------------------------\n\n')
                                                      }


  if(quali[2]==FALSE & as.numeric(tab.f2f1[i,5])<sigF){            #Fazer regressao
    reg.poly(resp[fatores[,1]==l1[i]], fatores[,2][fatores[,1]==l1[i]], as.numeric(tab.f2f1[nv1+1,1]),
    as.numeric(tab.f2f1[nv1+1,2]), as.numeric(tab.f2f1[i,1]), as.numeric(tab.f2f1[i,2]))
                                                   }


if(as.numeric(tab.f2f1[i,5])>sigF) {
    cat('\nDe acordo com o teste F, as medias desse fator sao estatisticamente iguais.\n')
    cat('------------------------------------------------------------------------\n')
    mean.table<-tapply.stat(resp[fatores[,1]==l1[i]],fatores[,2][fatores[,1]==l1[i]],mean)
    colnames(mean.table)<-c('Niveis','Medias')
    print(mean.table)
    cat('------------------------------------------------------------------------\n')
                                                }

}

}
## error a ##
tabmedia<-model.tables(anava, "means")
#error.plot<-as.vector(t(as.matrix(tabmedia$tables$`Fator1:bloco`)-as.vector(tabmedia$tables$Fator1))-as.vector(tabmedia$tables$bloco))
#Saida
out<-list()
out$residuos<-anava$residuals
#out$residuals.a<-error.plot
out$gl.residual<-anava$df.residual
#out$df.residual.a<-as.numeric(tab[2,1])
out$coeficientes<-anava$coefficients
out$efeitos<-anava$effects
out$valores.ajustados<-anava$fitted.values
out$medias.fator1<-tapply.stat(resp,fatores[,1],mean)
out$medias.fator2<-tapply.stat(resp,fatores[,2],mean)
out$medias.dentro12<-tabmedia$tables$`Fator1:Fator2`
#if(quali==FALSE && tab[[1]][1,5]<sigF) {out$reg<-reg}
invisible(out)
}
