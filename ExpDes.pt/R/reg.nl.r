#' Regressao Nao-linear
#'
#' \code{reg.nl} Ajusta modelos de regressao nao-linear na ANAVA.
#' @param resp Vetor numerico ou complexo contendo a variavel
#' resposta.
#' @param trat Vetor numerico ou complexo contendo os
#' tratamentos.
#' @return Sao retornados os valores dos ajustes de modelos de
#' regressao.
#' @references DRAPER, N.R.; SMITH, H. \emph{Apllied regression
#' analysis}. 3ed. New York : John Wiley, 1998. 706p.
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#' @author Luiz Alberto Beijo
#' @seealso \code{\link{graficos}}.
#' @examples
#' data(exnl)
#' attach(exnl)
#' x<-dic(trat, resp, quali=FALSE, nl=TRUE)
#' par(mfrow=c(1,2))
#' graficos(x, grau='pot')
#' graficos(x, grau='exp')
#' @importFrom "stargazer" "stargazer"
#' @export

reg.nl<-function(resp, trat) {

###========================================================###
#        -  Mod Potencia
###========================================================###
#Obtencao de estimativas iniciais
#Protecao para o dominio da funcao log
 if(any(resp==0)) {resp1<-resp+1e-10; yli=log(resp1)}
  if(any(resp==0)==FALSE) yli=log(resp)
 if(any(trat==0)) {trat1<-trat+1e-10; xli=log(trat1)}
  if(any(trat==0)==FALSE) xli=log(trat)

modli=lm(yli~xli)

apinit=exp(coef(modli)[1]) # Estimativa inicial do parametro beta
bpinit=(coef(modli)[2])    # Estimativa inicial do parametro gama

api<-(apinit)[[1]][1:1]
bpi<-(bpinit)[[1]][1:1]

mod_pot<-numeric(0)
try( mod_pot <- nls(resp~alfa*trat^beta, start=c(alfa=api, beta=bpi)),
     silent=TRUE)
cat('------------------------------------------------------------------------\n')

if(length(mod_pot)==0) {cat('
------------------------------------------------------------------------\n
Modelo Potencia
------------------------------------------------------------------------\n
Erro no ajuste do modelo! (singularidade, convergencia, etc)
                            ')

tm1<-data.frame('Erro no ajuste do modelo!')
aic1 <- rs1 <- c('Erro no ajuste do modelo!')

}

if(length(mod_pot)!=0) {

b1<-summary(mod_pot)
tm1<-data.frame('Estimativa' = round(b1[[10]][,1],8),
                'Erro padrao' = round(b1[[10]][,2],5),
                'tc'=round(b1[[10]][,3],5),
                'p-valor' = round(b1[[10]][,4],5))
rownames(tm1)<-c('Alpha','Beta')

#AIC
aic1<-AIC(mod_pot)

# R2 Aproximado
f=fitted.values(mod_pot)
r=residuals(mod_pot)
dr=sum((r/resp)^2)
sr=sum(r^2)
qe=mean(resp)
d=(f-qe)
dq=d^2
rs1=sum(dq)/(sum(dq)+sr)

cat('------------------------------------------------------------------------\n')
output1<-list('Modelo Potencia
------------------------------------------------------------------------' = tm1,
              '
------------------------------------------------------------------------
AIC
------------------------------------------------------------------------' = aic1,
              '
------------------------------------------------------------------------
R2 aprox do Modelo Potencia
------------------------------------------------------------------------' = rs1)
#print(output1,right=TRUE)
stargazer(tm1, type = "text", title="Modelo Potencia", summary=F, digits=4)
stargazer(aic1, type = "text", title="AIC", digits=4, style="apsr")
stargazer(rs1, type = "text", title="R2 aprox do Modelo Potencia", digits=4, flip=FALSE, summary=F, style="apsr")
cat('------------------------------------------------------------------------\n')

}

###========================================================###
#        Modelo Exponencial
###========================================================###

#Obtencao de estimativas iniciais
yeli=log(resp)
xeli=(trat)
modlie=lm(yeli~xeli)

aeinit=exp(coef(modlie)[1]) # Estimativa inicial do parametro beta
beinit=(coef(modlie)[2])    # Estimativa inicial do parametro gama

aei<-(aeinit)[[1]][1:1]
bei<-(beinit)[[1]][1:1]

mod_exp<-numeric(0)
try( mod_exp<-nls(resp~alfa*exp(beta*trat), start=c(alfa=aei, beta=bei)),
     silent=TRUE)

if(length(mod_exp)==0) {cat('
------------------------------------------------------------------------
Modelo Exponencial
------------------------------------------------------------------------
Erro no ajuste do modelo! (singularidade, convergencia, etc)
                           ')

tm2<-data.frame('Erro no ajuste do modelo!')
aic2 <- rs2 <- c('Erro no ajuste do modelo!')

}

if(length(mod_exp)!=0) {

b2<-summary(mod_exp)

tm2<-data.frame('Estimativa' = round(b2[[10]][,1],8),
                'Erro padrao' = round(b2[[10]][,2],5),
                'tc'=round(b2[[10]][,3],5),
                'p-valor' = round(b2[[10]][,4],5))
rownames(tm2)<-c('Alpha','Beta')

aic2<-AIC(mod_exp)

# R2 Aproximado
f=fitted.values(mod_exp)
r=residuals(mod_exp)
dr=sum((r/resp)^2)
sr=sum(r^2)
qe=mean(resp)
d=(f-qe)
dq=d^2
rs2=sum(dq)/(sum(dq)+sr)

output2<-list('
------------------------------------------------------------------------
Modelo Exponencial
------------------------------------------------------------------------' = tm2,
              '
------------------------------------------------------------------------
AIC
------------------------------------------------------------------------' = aic2,
              '
------------------------------------------------------------------------
R2 aprox do Modelo Exponencial
------------------------------------------------------------------------' = rs2)
#print(output2,right=TRUE)
stargazer(tm2, type = "text", title="Modelo Exponencial", summary=F, digits=4)
stargazer(aic2, type = "text", title="AIC", digits=4, style="apsr")
stargazer(rs2, type = "text", title="R2 aprox do Modelo Exponencial", digits=4, flip=FALSE, summary=F, style="apsr")
cat('------------------------------------------------------------------------\n')
}

###========================================================###
#                      Modelo logistico
###========================================================###

#Obtencao de estimativas iniciais
ylinit=log(((max(resp)+2)/resp)-1)
modlil=lm(ylinit~trat)
summary(modlil)

b=coef(modlil)[1]
gm=coef(modlil)[2]

glinit=-1*gm             # Estimativa inicial do parametro gama
alinit=max(resp)+2       # Estimativa inicial do parametro alfa
blinit=(coef(modlil)[1]) # Estimativa inicial do parametro beta

ali<-(alinit)[[1]][1:1]
bli<-(blinit)[[1]][1:1]
gli<-(glinit)[[1]][1:1]

mod_logi<-numeric(0)
try( mod_logi<-nls(resp~alfa/(1+exp(beta-(gama*trat))),
                  start=c(alfa=ali,beta=bli,gama=gli)),
     silent=TRUE)

if(length(mod_logi)==0) {cat('
------------------------------------------------------------------------
Modelo Logistico
------------------------------------------------------------------------
Erro no ajuste do modelo! (singularidade, convergencia, etc)
                             ')

tm3<-data.frame('Erro no ajuste do modelo!')
aic3 <- rs3 <- c('Erro no ajuste do modelo!')

}

if(length(mod_logi)!=0) {

b3<-summary(mod_logi)

tm3<-data.frame('Estimativa' = round(b3[[10]][,1],8),
                'Erro padrao' = round(b3[[10]][,2],5),
                'tc'=round(b3[[10]][,3],5),
                'p-valor' = round(b3[[10]][,4],5))
rownames(tm3)<-c('Alpha','Beta','Gamma')

yest=fitted(mod_logi)
aic3<-AIC(mod_logi)

# Coeficiente de Determinacao Aproximado
fl=fitted.values(mod_logi)
rl=residuals(mod_logi)
drl=sum((rl/resp)^2)
srl=sum(rl^2)
qel=mean(resp)
dl=(fl-qel)
dql=dl^2
rs3=sum(dql)/(sum(dql)+srl)

output3<-list('
------------------------------------------------------------------------
Modelo Logistico
------------------------------------------------------------------------' = tm3,
              '
------------------------------------------------------------------------
AIC
------------------------------------------------------------------------' = aic3,
              '
------------------------------------------------------------------------
R2 aprox do Modelo Logistico
------------------------------------------------------------------------' = rs3)
#print(output3,right=TRUE)
stargazer(tm3, type = "text", title="Modelo Logistico", summary=F, digits=4)
stargazer(aic3, type = "text", title="AIC", digits=4, style="apsr")
stargazer(rs3, type = "text", title="R2 aprox do Modelo Logistico", digits=4, flip=FALSE, summary=F, style="apsr")
cat('------------------------------------------------------------------------\n')
}


###========================================================###
#         Modelo Gompertz
###========================================================###

#Obtencao de estimativas iniciais
ylig=log(-1*log(resp/(max(resp)+2)))
modlig=lm(ylig~trat)

bg=coef(modlig)[1]
gmg=coef(modlig)[2]

gginit=-1*gmg            # Estimativa inicial do parametro gama
aginit=max(resp)+2       # Estimativa inicial do parametro alfa
bginit=(coef(modlig)[1]) # Estimativa inicial do parametro beta

agi<-(aginit)[[1]][1:1]
bgi<-(bginit)[[1]][1:1]
ggi<-(gginit)[[1]][1:1]

#modelo gompertz
mod_gomp<-numeric(0)
try( mod_gomp<-nls(resp~(alfa*exp(-exp(beta-(gama*trat)))),
             start=c(alfa=22,beta=1.5,gama=0.4)),
     silent=TRUE)

if(length(mod_gomp)==0) {cat('
------------------------------------------------------------------------
Modelo Gompertz
------------------------------------------------------------------------
Erro no ajuste do modelo! (singularidade, convergencia, etc)
                            ')

tm4<-data.frame('Erro no ajuste do modelo!')
aic4 <- rs4 <- c('Erro no ajuste do modelo!')

}

if(length(mod_gomp)!=0) {

b4<-summary(mod_gomp)
tm4<-data.frame('Estimativa' = round(b4[[10]][,1],8),
                'Erro padrao' = round(b4[[10]][,2],5),
                'tc'=round(b4[[10]][,3],5),
                'p-valor' = round(b4[[10]][,4],5))
rownames(tm4)<-c('Alpha','Beta','Gamma')

yest_gomp=fitted(mod_gomp)
aic4<-AIC(mod_gomp)

# Coeficiente de Determinacao Aproximado
fgo=fitted.values(mod_gomp)
rg=residuals(mod_gomp)
drg=sum((rg/resp)^2)
srg=sum(rg^2)
qeg=mean(resp)
dg=(fgo-qeg)
dqg=dg^2
rs4=sum(dqg)/(sum(dqg)+srg)

output4<-list('
------------------------------------------------------------------------
Modelo Gompertz
------------------------------------------------------------------------' = tm4,
              '
------------------------------------------------------------------------
AIC
------------------------------------------------------------------------' = aic4,
              '
------------------------------------------------------------------------
R2 aprox do Modelo Gompertz
------------------------------------------------------------------------' = rs4)
#print(output4,right=TRUE)
stargazer(tm4, type = "text", title="Modelo Gompertz", summary=F, digits=4)
stargazer(aic4, type = "text", title="AIC", digits=4, style="apsr")
stargazer(rs4, type = "text", title="R2 aprox do Modelo Gompertz", digits=4, flip=FALSE, summary=F, style="apsr")
cat('------------------------------------------------------------------------\n')
}



###################### Output ####################################
mean.table<-tapply.stat(resp,trat,mean)
colnames(mean.table)<-c('  Niveis','   Medias Observadas')


regout<-list("Quadro de medias" = mean.table,
  "Coeficientes modelo potencia" = tm1[,1], "AIC modelo potencia" = aic1,
  "R2 aprox modelo potencia"= rs1,
  "Coeficientes modelo exponencial" = tm2[,1], "AIC modelo exponencial" = aic2,
  "R2 aprox modelo exponencial"= rs2,
  "Coeficientes modelo logistico" = tm3[,1], "AIC modelo logistico" = aic3,
  "R2 aprox modelo logistico"= rs3,
  "Coeficientes modelo Gompertz" = tm4[,1], "AIC modelo Gompertz" = aic4,
  "R2 aprox modelo Gompertz"= rs4)
invisible(regout)

}

