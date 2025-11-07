#####################################
######priliminary utilities##########
#####################################
bivcenindic <- function(Cen1,Cen2){
ar<-array(1:length(Cen1))
ar1=Cen1*Cen2*ar
ar11<-ar1[ar1>0]
ar1=Cen1*(1-Cen2)*ar
ar10<-ar1[ar1>0]
ar1=(1-Cen1)*Cen2*ar
ar01<-ar1[ar1>0]
ar1=(1-Cen1)*(1-Cen2)*ar
ar00<-ar1[ar1>0]
list(Ic11=ar11,Ic10=ar10,Ic01=ar01,Ic00=ar00)}
#
clusievent <- function(x,fx){
ar<-array(x[1]:x[2])
kr=sum(fx[ar])
kr}
#
clustopronmat <- function(M,x){
ar<-M
kr <- apply(x,1,clusievent,fx=ar)
kr}
#
clusiobj <- function(x,fx){
ar<-array(x[1]:x[2])
kr=fx[ar[1]]
kr}
#
#
feasirej<- function(tht){
kr=feasibleregforR(tht)
kr}
#
funbf <- function(RIU,u){
re0=RIU[u]
re0}
#
funfittypp <- function(clustind){
Tfr=table(clustind);Tfr=as.matrix(Tfr)
tfr=as.vector(Tfr[,1])
typ<-NULL
if(max(tfr)==1){typ=c("Univ")}
if((max(tfr)==2)&(min(tfr)==2)){typ=c("Biv")}
if((max(tfr)==2)&(min(tfr)==1)){typ=c("Shr")}
if(max(tfr)>2){typ=c("Shr")}
typ
}
#
feasibleregforR<- function(theta){
sr1=theta[1:2]
amin=min(sr1)
amax=max(sr1)
x=sqrt(amin)/sqrt(amax)
x}
#
#
interacmat <- function(X,u){X*u}
#
indicmin<-function(i1,i2,time){
i3=i1
for(k in 1:(length(time)/2)){
i3[k]=i1[k]
if(time[i1[k]]>time[i2[k]]){i3[k]=i2[k]}}
i3}
#

matcolcumsum <- function(x){
ar<-cumsum(x)
ar}
#
matcolopp <- function(x,MAT,nc){
ar<-array(x[1]:x[2])
ar=as.vector(ar)
nr=length(ar)
MAT=as.matrix(MAT)
macl=MAT[ar,]
macl=matrix(c(macl),nr,nc)
kr=colSums(macl)
kr}
#
risk_set_op <- function(x,y,fx){
ar<-ifelse(x[1]<=y,1,0)
kr=sum(fx*ar)
kr}
#
rissksetdr <- function(time,x) ifelse(time[1]>=x,1,0)
#
risskset <- function(uniq_tim,x) ifelse(uniq_tim[1]<=x,1,0)
#
rissksetord <- function(timeo,x) ifelse(timeo[1]>=x,1,0)
#
risskf <- function(RIU0,indic1,indic2){
re0=RIU0[indic1]+RIU0[indic2]
re0}
#
tofevent <- function(uniind,x,cen){
cde=ifelse(uniind[1]==x,1,0)
cdr=c(cen)*cde*array(1:length(x))
cdeb=as.vector(cdr[cdr>0])
if(length(cdeb)==0){re=c(0)}
if(length(cdeb)>0){re=c(sum(cen[cdeb]))}
re}
#

# to generate bivariate normal random effect
rbivnormdv<-function(psize,ssq1,ssq2,r){
w1=rnorm(psize,mean=0,sd=sqrt(ssq1))
w2=rnorm(psize,mean=r*w1,sd=sqrt((1-r^2)*ssq2))
cbind(w1,w2)}
#
# for derivatives in censoring cases (1,1),(1,0), and (0,1)
#
d2barderivP11cv= function (Ic11,newtht,Y1,Y2,H1,H2){
Y1=as.matrix(Y1);Y2=as.matrix(Y2);X1=Y1[Ic11,];X2=Y2[Ic11,]
if(length(Ic11)==1){X1=matrix(c(X1),1,ncol(Y1));X2=matrix(c(X2),1,ncol(Y1))}else{
X1=as.matrix(X1);X2=as.matrix(X2)}
a = abs(newtht[1]);R = abs(newtht[2])
H1=H1[Ic11];H2=H2[Ic11]
M1=(1+a*H1);M2=(1+a*H2);M12=(1+a*(H1+H2))
c1=(R+a)*R;c2=(R-R^2);c3=(R-R^2);c4=(1-R)^2
dac1=R;dRc1=(2*R+a);dRc2=dRc3=(1-2*R);dRc4=-2*(1-R)
a1<-a2<-NULL
P11=(c1*M1*M2+(c2*M1+c3*M2)*M12+c4*M12^2)
daP11=(1/P11)*( dac1*M1*M2+c1*(H1*M2+M1*H2)+(c2*M1+c3*M2)*(H1+H2)+(c2*H1+c3*H2)*M12+2*c4*M12*(H1+H2))
dRP11=(1/P11)*(dRc1*M1*M2+(dRc2*M1+dRc3*M2)*M12+dRc4*M12^2)
DBP11=(1/P11)*(c1*(a*H1*M2*X1+a*H2*M1*X2)+(c3*M2+c2*M1)*a*H1*X1+(c3*M2+c2*M1)*a*H2*X2+
(c3*a*H2*X2+c2*a*H1*X1)*M12+2*c4*M12*(a*H1*X1+a*H2*X2))
DBaP11=(1/P11)*( (dac1*a+c1)*(H1*M2*X1+H2*M1*X2)+c1*a*(H1*H2*X1+H2*H1*X2)+
(c3*H2+c2*H1)*a*H1*X1+(c3*M2+c2*M1)*H1*X1+(c3*M2+c2*M1)*H2*X2+(c3*H2+c2*H1)*a*H2*X2+
(c3*a*H2*X2+c2*a*H1*X1)*(H1+H2)+(c3*H2*X2+c2*H1*X1)*M12+ 2*c4*(M12+a*(H1+H2))*(H1*X1+H2*X2))
DBRP11=(1/P11)*(dRc1*(a*H1*M2*X1+a*H2*M1*X2)+(dRc3*M2+dRc2*M1)*a*H1*X1+(dRc3*M2+dRc2*M1)*a*H2*X2+
(dRc3*a*H2*X2+dRc2*a*H1*X1)*M12+2*dRc4*M12*(a*H1*X1+a*H2*X2))
Dbap11=c(colSums(DBaP11))-c(colSums(c(daP11)*DBP11))
DbRp11=c(colSums(DBRP11))-c(colSums(c(dRP11)*DBP11))
Dbthtp11=rbind(Dbap11,DbRp11)##########
n_cov_coef= ncol(X1);data.n= nrow(X1)
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=X2);X12<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=X1);X21<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=X1);X11<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=X2);X22<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
D2BP11=(1/P11)*( c1*((c(a*H1*M2)*X11+c(a*H1*a*H2)*X21)+(c(a*H2*M1)*X22+c(a*H1*a*H2)*X12))+
c3*(c(a*H1*a*H2)*X21+c(a*H1*M2)*X11)+c2*(c(a*H1*a*H1)*X11+c(a*H1*M1)*X11)+
c3*(c(a*H2*a*H2)*X22+c(a*H2*M2)*X22)+c2*(c(a*H1*a*H2)*X12+c(a*H2*M1)*X22)+
M12*(c3*a*H2*X22+c2*a*H1*X11)+c3*c(a*H1*a*H2)*X21+c3*c(a*H2*a*H2)*X22+c2*c(a*H1*a*H1)*X11+c2*c(a*H1*a*H2)*X12+
2*c4*M12*(c(a*H1)*X11+ c(a*H2)*X22)+
2*c4*(c((a*H1)^2)*X11+c(a*H1*a*H2)*X12+c(a*H1*a*H2)*X21+ c((a*H2)^2)*X22))
D2bp11=matrix(c(colSums(D2BP11)),n_cov_coef,n_cov_coef)-t(DBP11)%*%DBP11######
list(D2B=D2bp11,DBtht=Dbthtp11)
}
#
d2barderivP10cv= function (Ic10,newtht,Y1,Y2,H1,H2){
Y1=as.matrix(Y1);Y2=as.matrix(Y2)
X1=Y1[Ic10,];X2=Y2[Ic10,]
if(length(Ic10)==1){X1=matrix(c(X1),1,ncol(Y1));X2=matrix(c(X2),1,ncol(Y1))}else{
X1=as.matrix(X1);X2=as.matrix(X2)}
a = abs(newtht[1]);R = abs(newtht[2])
H1=H1[Ic10];H2=H2[Ic10]
M1=(1+a*H1);M2=(1+a*H2);M12=(1+a*H1+a*H2)
P10=(R*M1+(1-R)*M12)
DBP10=(1/P10)*(R*a*H1*X1+(1-R)*(a*H1*X1+a*H2*X2))
daP10=(1/P10)*(R*H1+(1-R)*(H1+H2));dRP10=(1/P10)*(M1-M12)
DBaP10=(1/P10)*(R*H1*X1+(1-R)*(H1*X1+H2*X2));DBRP10=(1/P10)*(a*H1*X1-(a*H1*X1+a*H2*X2))
Dbap10=c(colSums(DBaP10))-c(colSums(c(daP10)*DBP10))
DbRp10=c(colSums(DBRP10))-c(colSums(c(dRP10)*DBP10))
Dbthtp10=rbind(Dbap10,DbRp10)###########
data.n2= nrow(X1);n_cov_coef= ncol(X1)
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=X1);X11<-matrix(resinteracmatx,data.n2,n_cov_coef*n_cov_coef)
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=X2);X22<-matrix(resinteracmatx,data.n2,n_cov_coef*n_cov_coef)
D2BP10=(1/P10)*(R*a*H1*X11+(1-R)*(a*H1*X11+a*H2*X22))
D2bP10=matrix(c(colSums(D2BP10)),n_cov_coef,n_cov_coef)-t(DBP10)%*%DBP10######
list(D2B=D2bP10,DBtht=Dbthtp10)
}

d2barderivP01cv= function (Ic01,newtht,Y1,Y2,H1,H2){
Y1=as.matrix(Y1);Y2=as.matrix(Y2)
X1=Y1[Ic01,];X2=Y2[Ic01,]
if(length(Ic01)==1){X1=matrix(c(X1),1,ncol(Y1));X2=matrix(c(X2),1,ncol(Y1))}else{
X1=as.matrix(X1);X2=as.matrix(X2)}
a = abs(newtht[1]);R = abs(newtht[2])
H1=H1[Ic01];H2=H2[Ic01]
M1=(1+a*H1);M2=(1+a*H2);M12=(1+a*H1+a*H2)
P01=(R*M2+(1-R)*M12)
DBP01=(1/P01)*(R*a*H2*X2+(1-R)*(a*H1*X1+a*H2*X2))
daP01=(1/P01)*(R*H2+(1-R)*(H1+H2))
dRP01=(1/P01)*(M2-M12)
DBaP01=(1/P01)*(R*H2*X2+(1-R)*(H1*X1+H2*X2))
DBRP01=(1/P01)*(a*H2*X2-(a*H1*X1+a*H2*X2))
Dbap01=c(colSums(DBaP01))-c(colSums(c(daP01)*DBP01))
DbRp01=c(colSums(DBRP01))-c(colSums(c(dRP01)*DBP01))
Dbthtp01=rbind(Dbap01,DbRp01)###########
data.n3= nrow(X1);n_cov_coef= ncol(X1)
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=X1);X11<-matrix(resinteracmatx,data.n3,n_cov_coef*n_cov_coef)
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=X2);X22<-matrix(resinteracmatx,data.n3,n_cov_coef*n_cov_coef)
D2BP01=(1/P01)*(R*a*H2*X22+(1-R)*(a*H1*X11+a*H2*X22))
D2bp01=matrix(c(colSums(D2BP01)),n_cov_coef,n_cov_coef)-t(DBP01)%*%DBP01######
list(D2B=D2bp01,DBtht=Dbthtp01)
}

d2bderiv00cv= function (newtht,X1,X2,H1,H2,cen1,cen2){
X1=as.matrix(X1);X2=as.matrix(X2)
a = abs(newtht[1]);R = abs(newtht[2])
k=(1-R)*a^(-1);k1=(k+cen1);k2=(k+cen2);k0=(R*a^(-1)+cen1+cen2)
dak1=dak2=(-(1-R)*a^(-2));dRk1=dRk2=(-a^(-1));dak0=(-R*a^(-2));dRk0=a^(-1)
#-k1*log(1+a*H1)-k2*log(1+a*H2)-k0*log(1+a*H1+a*H2)#LL
a1=a2=a
DB00=-c(k1*(a*H1/(1+a*H1)))*X1-c(k2*(a*H2/(1+a*H2)))*X2-c(k0*(1/(1+a*H1+a*H2)))*(a*H1*X1+a*H2*X2)
n_cov_coef= ncol(X1);data.n= nrow(X1)
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=X2);X12<-matrix(resinteracmatx,data.n,(n_cov_coef*n_cov_coef))
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=X1);X21<-matrix(resinteracmatx,data.n,(n_cov_coef*n_cov_coef))
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=X1);X11<-matrix(resinteracmatx,data.n,(n_cov_coef*n_cov_coef))
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=X2);X22<-matrix(resinteracmatx,data.n,(n_cov_coef*n_cov_coef))
D2B00=( -c(k1*((a*H1/(1+a*H1))-(((a*H1)^2)/(1+a*H1)^2)))*X11-c(k2*( (a*H2/(1+a*H2))-((a*H2)^2)/(1+a*H2)^2 ))*X22-
c(k0*(-1/(1+a*H1+a*H2)^2))*((a*H1)^2*X11+a*H1*a*H2*X12+a*H1*a*H2*X21+(a*H2)^2*X22)-c(k0*(1/(1+a*H1+a*H2)))*(a*H1*X11+a*H2*X22))
D2b=matrix(c(colSums(D2B00)),n_cov_coef,n_cov_coef)###########
DBa00=(-dak1*(a*H1/(1+a*H1))*X1-k1*(H1/(1+a*H1)-a*H1^2/(1+a*H1)^2)*X1-
dak2*(a*H2/(1+a*H2))*X2-k2*(H2/(1+a*H2)-a*H2^2/(1+a*H2)^2)*X2-
dak0*(1/(1+a*H1+a*H2))*(a*H1*X1+a*H2*X2)-k0*(1/(1+a*H1+a*H2)-a*(H1+H2)/(1+a*H1+a*H2)^2)*(H1*X1+H2*X2))
DBR00=-dRk1*(a*H1/(1+a*H1))*X1-dRk2*(a*H2/(1+a*H2))*X2-dRk0*(1/(1+a*H1+a*H2))*(a*H1*X1+a*H2*X2)
Dba00=c(colSums(DBa00))
DbR00=c(colSums(DBR00))
Dbtht=rbind(Dba00,DbR00)###########
list(D2B=D2b,DBtht=Dbtht)
}
#
D2bnthtderivcv<-function(newtht,X1,X2,H1,H2,cen1,cen2){
bivi=bivcenindic(Cen1=cen1,Cen2=cen2)
Ic11=bivi$Ic11;Ic10=bivi$Ic10;Ic01=bivi$Ic01
comp1=d2bderiv00cv(newtht=newtht,X1=X1,X2=X2,H1=H1,H2=H2,cen1=cen1,cen2=cen2)
D2B0=comp1$D2B;DBtht0=comp1$DBtht
nulld2b=matrix(0,ncol(X1),ncol(X1));nulltht=matrix(0,2,ncol(X1))
if(length(Ic11)>0){
comp2=d2barderivP11cv(Ic11=Ic11,newtht=newtht,Y1=X1,Y2=X2,H1=H1,H2=H2)
D2B11=comp2$D2B;DBtht11=comp2$DBtht}else{D2B11=nulld2b;DBtht11=nulltht}
if(length(Ic10)>0){
comp3=d2barderivP10cv(Ic10=Ic10,newtht=newtht,Y1=X1,Y2=X2,H1=H1,H2=H2)
D2B10=comp3$D2B;DBtht10=comp3$DBtht}else{D2B10=nulld2b;DBtht10=nulltht}
if(length(Ic01)>0){
comp4=d2barderivP01cv(Ic01=Ic01,newtht=newtht,Y1=X1,Y2=X2,H1=H1,H2=H2)
D2B01=comp4$D2B;DBtht01=comp4$DBtht}else{D2B01=nulld2b;DBtht01=nulltht}
D2b=D2B0+D2B11+D2B10+D2B01
Dbtht=DBtht0+DBtht11+DBtht10+DBtht01
list(D2B=D2b,DBtht=Dbtht)
}
#

derivdbcv<-function (theta,cen1,cen2,X1,X2,H1,H2){
a=theta[1];R=theta[2]
k=(1-R)*a^(-1);k1=(k+cen1);k2=(k+cen2);k0=(R*a^(-1)+cen1+cen2)
M1=(1+a*H1);M2=(1+a*H2);M12=(1+a*(H1+H2))
DB00=-k1*(a*H1/M1)*X1-k2*(a*H2/M2)*X2-k0*(1/M12)*(a*H1*X1+a*H2*X2)
c1=(R+a)*R;c2=(R-R^2);c3=(R-R^2);c4=(1-R)^2
P11=(c1*M1*M2+(c2*M1+c3*M2)*M12+c4*M12^2)
DBP11=((cen1*cen2)/P11)*(c1*(a*H1*M2*X1+a*H2*M1*X2)+(c3*M2+c2*M1)*a*H1*X1+(c3*M2+c2*M1)*a*H2*X2+
(c3*a*H2*X2+c2*a*H1*X1)*M12+2*c4*M12*(a*H1*X1+a*H2*X2))
P10=(R*M1+(1-R)*M12)
DBP10=((cen1*(1-cen2))/P10)*(R*a*H1*X1+(1-R)*(a*H1*X1+a*H2*X2))
P01=(R*M2+(1-R)*M12)
DBP01=(((1-cen1)*cen2)/P01)*(R*a*H2*X2+(1-R)*(a*H1*X1+a*H2*X2))
DB0=DB00+DBP11+DBP10+DBP01
DB=c(colSums((cen1*X1+cen2*X2)))+c(colSums(DB0))###
DB
}

derivdbdxcv<-function (newtht,cen1,cen2,X1,X2,H1,H2,dx1H1,dx1H2){
a = abs(newtht[1]);R = abs(newtht[2])
X1=as.matrix(X1);X2=as.matrix(X2)
k=(1-R)*a^(-1);k1=(k+cen1);k2=(k+cen2);k0=(R*a^(-1)+cen1+cen2)
M1=(1+a*H1);M2=(1+a*H2);M12=(1+a*(H1+H2))
DB00=-k1*(a*H1/M1)*X1-k2*(a*H2/M2)*X2-k0*(1/M12)*(a*H1*X1+a*H2*X2)
DBx100=(-k1*((a*dx1H1/M1)-(a*H1*a*dx1H1/M1^2))*X1-k2*((a*dx1H2/M2)-(a*H2*a*dx1H2/M2^2))*X2-
k0*((-(a*dx1H1+a*dx1H2)/M12^2)*(a*H1*X1+a*H2*X2)+(1/M12)*(a*dx1H1*X1+a*dx1H2*X2)))
Dbx100=c(colSums(DBx100))###
c1=(R+a)*R;c2=(R-R^2);c3=(R-R^2);c4=(1-R)^2
P11=(c1*M1*M2+(c2*M1+c3*M2)*M12+c4*M12^2)
DBP11=((cen1*cen2)/P11)*(c1*(a*H1*M2*X1+a*H2*M1*X2)+(c3*M2+c2*M1)*a*H1*X1+(c3*M2+c2*M1)*a*H2*X2+
(c3*a*H2*X2+c2*a*H1*X1)*M12+2*c4*M12*(a*H1*X1+a*H2*X2))
dx1P11=((cen1*cen2)/P11)*( c1*((a*dx1H1)*M2+M1*(a*dx1H2))+(c2*M1+c3*M2)*a*(dx1H1+dx1H2)+
a*(c2*dx1H1+c3*dx1H2)*M12+2*c4*M12*a*(dx1H1+dx1H2))
DBx1P11=((cen1*cen2)/P11)*(c1*(a*(dx1H1*M2+a*H1*dx1H2)*X1+a*(dx1H2*M1+a*H2*dx1H1)*X2)+
(c3*M2+c2*M1)*a*dx1H1*X1+(a*c3*dx1H2+a*c2*dx1H1)*a*H1*X1+(c3*M2+c2*M1)*a*dx1H2*X2+(a*c3*dx1H2+a*c2*dx1H1)*a*H2*X2+
(c3*a*dx1H2*X2+c2*a*dx1H1*X1)*M12+(c3*a*H2*X2+c2*a*H1*X1)*a*(dx1H1+dx1H2)+2*c4*a*(dx1H1+dx1H2)*(a*H1*X1+a*H2*X2)+2*c4*M12*(a*dx1H1*X1+a*dx1H2*X2)   )
Dbx1p11=c(colSums(DBx1P11))-c(colSums(c(dx1P11)*DBP11))###
P10=(R*M1+(1-R)*M12)
DBP10=((cen1*(1-cen2))/P10)*(R*a*H1*X1+(1-R)*(a*H1*X1+a*H2*X2))
dx1P10=((cen1*(1-cen2))/P10)*(R*a*dx1H1+(1-R)*a*(dx1H1+dx1H2))
DBx1P10=((cen1*(1-cen2))/P10)*(R*a*dx1H1*X1+(1-R)*(a*dx1H1*X1+a*dx1H2*X2))
Dbx1p10=c(colSums(DBx1P10))-c(colSums(c(dx1P10)*DBP10))###
P01=(R*M2+(1-R)*M12)
dx1P01=(((1-cen1)*cen2)/P01)*(R*a*dx1H2+(1-R)*(a*dx1H1+a*dx1H2))
DBP01=(((1-cen1)*cen2)/P01)*(R*a*H2*X2+(1-R)*(a*H1*X1+a*H2*X2))
DBx1P01=(((1-cen1)*cen2)/P01)*(R*a*dx1H2*X2+(1-R)*(a*dx1H1*X1+a*dx1H2*X2))
Dbx1p01=c(colSums(DBx1P01))-c(colSums(c(dx1P01)*DBP01))###
DBx1=Dbx100+Dbx1p11+Dbx1p10+Dbx1p01#######
DBx1
}
################
d2bderivP11dv= function (Ic11,newtht,Y1,Y2,H1,H2){
Y1=as.matrix(Y1);Y2=as.matrix(Y2);X1=Y1[Ic11,];X2=Y2[Ic11,];X1=as.matrix(X1);X2=as.matrix(X2)
a1 = abs(newtht[1]);a2 = abs(newtht[2]);R = abs(newtht[3])
H1=H1[Ic11];H2=H2[Ic11]
M1=(1+a1*H1);M2=(1+a2*H2);M12=(1+a1*H1+a2*H2)
c1=(R+(a1^(1/2)*a2^(1/2)))*R;c2=((a1^(1/2)/a2^(1/2))*R-R^2);c3=((a2^(1/2)/a1^(1/2))*R-R^2)
c4=(1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R)
da1c1=((1/2)*(a1^(-1/2)*a2^(1/2)))*R;da2c1=((1/2)*(a1^(1/2)*a2^(-1/2)))*R;dRc1=(2*R+(a1^(1/2)*a2^(1/2)))
da1c2=((1/2)*(a1^(-1/2)/a2^(1/2))*R);da2c2=(-(1/2)*(a1^(1/2)/a2^(3/2))*R);dRc2=((a1^(1/2)/a2^(1/2))-2*R)
da1c3=(-(1/2)*(a2^(1/2)/a1^(3/2))*R);da2c3=((1/2)*(a2^(-1/2)/a1^(1/2))*R);dRc3=((a2^(1/2)/a1^(1/2))-2*R)
da1c4=(-(1/2)*(a1^(-1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R)+(1-(a1^(1/2)/a2^(1/2))*R)*((1/2)*(a2^(1/2)/a1^(3/2))*R)
da2c4=((1/2)*(a1^(1/2)/a2^(3/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R)+(1-(a1^(1/2)/a2^(1/2))*R)*(-(1/2)*(a2^(-1/2)/a1^(1/2))*R)
dRc4=(-(a1^(1/2)/a2^(1/2)))*(1-(a2^(1/2)/a1^(1/2))*R)+(1-(a1^(1/2)/a2^(1/2))*R)*(-(a2^(1/2)/a1^(1/2)))
P11=((c1*(1+a1*H1)*(1+a2*H2)+(c2*(1+a1*H1)+c3*(1+a2*H2))*(1+a1*H1+a2*H2)+c4*((1+a1*H1+a2*H2))^2))
DBP11=(1/P11)*(c1*(a1*H1*M2*X1+a2*H2*M1*X2)+(c3*M2+c2*M1)*a1*H1*X1+(c3*M2+c2*M1)*a2*H2*X2+
(c3*a2*H2*X2+c2*a1*H1*X1)*M12+2*c4*M12*(a1*H1*X1+a2*H2*X2))
da1P11=(1/P11)*(((da1c1*(1+a1*H1)+c1*H1)*(1+a2*H2)+(c2*(1+a1*H1)+c3*(1+a2*H2))*(H1)+
(da1c2*(1+a1*H1)+c2*(H1)+da1c3*(1+a2*H2))*(1+a1*H1+a2*H2)+da1c4*((1+a1*H1+a2*H2))^2+2*c4*((1+a1*H1+a2*H2))*H1))
da2P11=(1/P11)*(((1+a1*H1)*(da2c1*(1+a2*H2)+c1*H2)+(c2*(1+a1*H1)+c3*(1+a2*H2))*(H2)+
(da2c2*(1+a1*H1)+da2c3*(1+a2*H2)+c3*H2)*(1+a1*H1+a2*H2)+da2c4*((1+a1*H1+a2*H2))^2+2*c4*((1+a1*H1+a2*H2))*H2))
dRP11=(1/P11)*(( dRc1*(1+a1*H1)*(1+a2*H2)+(dRc2*(1+a1*H1)+dRc3*(1+a2*H2))*(1+a1*H1+a2*H2)+dRc4*((1+a1*H1+a2*H2))^2))
DBa1P11=(1/P11)*(da1c1*(a1*H1*M2*X1+a2*H2*M1*X2)+c1*(H1*M2*X1+a2*H2*H1*X2)+
(c3*M2+c2*M1)*H1*X1+(da1c3*M2+da1c2*M1+c2*H1)*a1*H1*X1+(da1c3*M2+da1c2*M1+c2*H1)*a2*H2*X2+
(da1c3*a2*H2*X2+da1c2*a1*H1*X1+c2*H1*X1)*M12+(c3*a2*H2*X2+c2*a1*H1*X1)*H1+
2*(da1c4*M12+c4*H1)*(a1*H1*X1+a2*H2*X2)+2*c4*M12*(H1*X1))
DBa2P11=(1/P11)*(da2c1*(a1*H1*M2*X1+a2*H2*M1*X2)+c1*(a1*H1*H2*X1+H2*M1*X2)+
(c3*M2+c2*M1)*H2*X2+(da2c3*M2+c3*H2+da2c2*M1)*a2*H2*X2+(da2c3*M2+c3*H2+da2c2*M1)*a1*H1*X1+
((da2c3*a2+c3)*H2*X2+da2c2*a1*H1*X1)*M12+(c3*a2*H2*X2+c2*a1*H1*X1)*H2+
2*(da2c4*M12+c4*H2)*(a1*H1*X1+a2*H2*X2)+2*c4*M12*(H2*X2))
DBRP11=(1/P11)*(dRc1*(a1*H1*M2*X1+a2*H2*M1*X2)+(dRc3*M2+dRc2*M1)*a1*H1*X1+(dRc3*M2+dRc2*M1)*a2*H2*X2+
(dRc3*a2*H2*X2+dRc2*a1*H1*X1)*M12+2*dRc4*M12*(a1*H1*X1+a2*H2*X2))
Dba1=c(colSums(DBa1P11))-c(colSums(c(da1P11)*DBP11))
Dba2=c(colSums(DBa2P11))-c(colSums(c(da2P11)*DBP11))
DbR=c(colSums(DBRP11))-c(colSums(c(dRP11)*DBP11))
Dbtht=rbind(Dba1,Dba2,DbR)##########
n_cov_coef= ncol(X1);data.n= nrow(X1)
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=X2)
X12<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=X1)
X21<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=X1)
X11<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=X2)
X22<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
D2BP11=(1/P11)*( c1*((c(a1*H1*M2)*X11+c(a1*H1*a2*H2)*X21)+(c(a2*H2*M1)*X22+c(a1*H1*a2*H2)*X12))+
c3*(c(a1*H1*a2*H2)*X21+c(a1*H1*M2)*X11)+c2*(c(a1*H1*a1*H1)*X11+c(a1*H1*M1)*X11)+
c3*(c(a2*H2*a2*H2)*X22+c(a2*H2*M2)*X22)+c2*(c(a1*H1*a2*H2)*X12+c(a2*H2*M1)*X22)+
M12*(c3*a2*H2*X22+c2*a1*H1*X11)+c3*c(a1*H1*a2*H2)*X21+c3*c(a2*H2*a2*H2)*X22+c2*c(a1*H1*a1*H1)*X11+c2*c(a1*H1*a2*H2)*X12+
2*c4*M12*(c(a1*H1)*X11+ c(a2*H2)*X22)+
2*c4*(c((a1*H1)^2)*X11+c(a1*H1*a2*H2)*X12+c(a1*H1*a2*H2)*X21+ c((a2*H2)^2)*X22))
D2b=matrix(c(colSums(D2BP11)),n_cov_coef,n_cov_coef)-t(DBP11)%*%DBP11######
list(D2B=D2b,DBtht=Dbtht)
}

d2bderivP10dv= function (Ic10,newtht,Y1,Y2,H1,H2){
Y1=as.matrix(Y1);Y2=as.matrix(Y2);X1=Y1[Ic10,];X2=Y2[Ic10,];X1=as.matrix(X1);X2=as.matrix(X2)
a1 = abs(newtht[1]);a2 = abs(newtht[2]);R = abs(newtht[3])
H1=H1[Ic10];H2=H2[Ic10]
M1=(1+a1*H1);M2=(1+a2*H2);M12=(1+a1*H1+a2*H2)
c1=((a1^(1/2)*a2^(-1/2)))*R;c2=(1-(a1^(1/2)*a2^(-1/2))*R)
da1c1=(1/2)*((a1^(-1/2)*a2^(-1/2)))*R;da2c1=-((a1^(1/2)*a2^(-3/2)))*R;dRc1=((a1^(1/2)*a2^(-1/2)))
da1c2=(-(1/2)*(a1^(-1/2)*a2^(-1/2))*R);da2c2=((1/2)*(a1^(1/2)*a2^(-3/2))*R);dRc2=(-(a1^(1/2)*a2^(-1/2)))
P10=(c1*M1+c2*M12)
DBP10=(1/P10)*(c1*a1*H1*X1+c2*(a1*H1*X1+a2*H2*X2))
da1P10=(1/P10)*(da1c1*M1+c1*H1+da1c2*M12+c2*H1)
da2P10=(1/P10)*(da2c1*M1+da2c2*M12+c2*H2)
dRP10=(1/P10)*(dRc1*M1+dRc2*M12)
DBa1P10=(1/P10)*( (da1c1*a1+c1)*H1*X1+da1c2*(a1*H1*X1+a2*H2*X2)+c2*H1*X1)
DBa2P10=(1/P10)*(da2c1*a1*H1*X1+da2c2*(a1*H1*X1+a2*H2*X2)+c2*H2*X2)
DBRP10=(1/P10)*(dRc1*a1*H1*X1+dRc2*(a1*H1*X1+a2*H2*X2))
Dba1=c(colSums(DBa1P10))-c(colSums(c(da1P10)*DBP10))
Dba2=c(colSums(DBa2P10))-c(colSums(c(da2P10)*DBP10))
DbR=c(colSums(DBRP10))-c(colSums(c(dRP10)*DBP10))
Dbtht=rbind(Dba1,Dba2,DbR)###########
n_cov_coef= ncol(X1);data.n= nrow(X1)
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=X1)
X11<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=X2)
X22<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
D2BP10=(1/P10)*(c1*a1*H1*X11+c2*(a1*H1*X11+a2*H2*X22))
D2b=matrix(c(colSums(D2BP10)),n_cov_coef,n_cov_coef)-t(DBP10)%*%DBP10######
list(D2B=D2b,DBtht=Dbtht)
}

d2bderivP01dv= function (Ic01,newtht,Y1,Y2,H1,H2){
Y1=as.matrix(Y1);Y2=as.matrix(Y2);X1=Y1[Ic01,];X2=Y2[Ic01,];X1=as.matrix(X1);X2=as.matrix(X2)
a1 = abs(newtht[1]);a2 = abs(newtht[2]);R = abs(newtht[3])
H1=H1[Ic01];H2=H2[Ic01]
M1=(1+a1*H1);M2=(1+a2*H2);M12=(1+a1*H1+a2*H2)
c1=((a1^(-1/2)*a2^(1/2)))*R;c2=(1-(a1^(-1/2)*a2^(1/2))*R)
da1c1=-(1/2)*((a1^(-3/2)*a2^(1/2)))*R;da2c1=(1/2)*((a1^(-1/2)*a2^(-1/2)))*R;dRc1=((a1^(-1/2)*a2^(1/2)))
da1c2=((1/2)*(a1^(-3/2)*a2^(1/2))*R);da2c2=(-(1/2)*(a1^(-1/2)*a2^(-1/2))*R);dRc2=(-(a1^(-1/2)*a2^(1/2)))
P01=(c1*M2+c2*M12)
DBP01=(1/P01)*(c1*a2*H2*X2+c2*(a1*H1*X1+a2*H2*X2))
da1P01=(1/P01)*(da1c1*M2+da1c2*M12+c2*H1)
da2P01=(1/P01)*(da2c1*M2+c1*H2+da2c2*M12+c2*H2)
dRP01=(1/P01)*(dRc1*M2+dRc2*M12)
DBa1P01=(1/P01)*(da1c1*a2*H2*X2+da1c2*(a1*H1*X1+a2*H2*X2)+c2*H1*X1)
DBa2P01=(1/P01)*( (da2c1*a2+c1)*H2*X2+da2c2*(a1*H1*X1+a2*H2*X2)+c2*H2*X2)
DBRP01=(1/P01)*(dRc1*a2*H2*X2+dRc2*(a1*H1*X1+a2*H2*X2))
Dba1=c(colSums(DBa1P01))-c(colSums(c(da1P01)*DBP01))
Dba2=c(colSums(DBa2P01))-c(colSums(c(da2P01)*DBP01))
DbR=c(colSums(DBRP01))-c(colSums(c(dRP01)*DBP01))
Dbtht=rbind(Dba1,Dba2,DbR)###########
n_cov_coef= ncol(X1);data.n= nrow(X1)
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=X1)
X11<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=X2)
X22<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
D2BP01=(1/P01)*(c1*a2*H2*X22+c2*(a1*H1*X11+a2*H2*X22))
D2b=matrix(c(colSums(D2BP01)),n_cov_coef,n_cov_coef)-t(DBP01)%*%DBP01######
list(D2B=D2b,DBtht=Dbtht)
}



d2bderiv00dv= function (newtht,X1,X2,H1,H2,cen1,cen2){
X1=as.matrix(X1);X2=as.matrix(X2)
a1 = abs(newtht[1]);a2 = abs(newtht[2]);R = abs(newtht[3])
k1=((a1^(-1)-(R*a1^(-1/2)*a2^(-1/2)))+cen1);k2=((a2^(-1)-(R*a1^(-1/2)*a2^(-1/2)))+cen2);k0=(R*a1^(-1/2)*a2^(-1/2)+cen1+cen2)
da1k1=(-a1^(-2)+(1/2)*R*a1^(-3/2)*a2^(-1/2));da2k1=((1/2)*R*a1^(-1/2)*a2^(-3/2));dRk1=(-a1^(-1/2)*a2^(-1/2))
da1k2=((1/2)*R*a1^(-3/2)*a2^(-1/2));da2k2=(-a2^(-2)+(1/2)*R*a1^(-1/2)*a2^(-3/2));dRk2=(-a1^(-1/2)*a2^(-1/2))
da1k0=((-1/2)*R*a1^(-3/2)*a2^(-1/2));da2k0=((-1/2)*R*a1^(-1/2)*a2^(-3/2));dRk0=(a1^(-1/2)*a2^(-1/2))
#-k1*log(1+a1*H1)-k2*log(1+a2*H2)-k0*log(1+a1*H1+a2*H2)#LL
DB00=-k1*(a1*H1/(1+a1*H1))*X1-k2*(a2*H2/(1+a2*H2))*X2-k0*(1/(1+a1*H1+a2*H2))*(a1*H1*X1+a2*H2*X2)
n_cov_coef= ncol(X1);data.n= nrow(X1)
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=X2);X12<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=X1);X21<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=X1);X11<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=X2);X22<-matrix(resinteracmatx,data.n,n_cov_coef*n_cov_coef)
D2B00=( -k1*((a1*H1/(1+a1*H1))-(((a1*H1)^2)/(1+a1*H1)^2))*X11-k2*( (a2*H2/(1+a2*H2))-((a2*H2)^2)/(1+a2*H2)^2 )*X22-
k0*(-1/(1+a1*H1+a2*H2)^2)*((a1*H1)^2*X11+a1*H1*a2*H2*X12+a1*H1*a2*H2*X21+(a2*H2)^2*X22)-k0*(1/(1+a1*H1+a2*H2))*(a1*H1*X11+a2*H2*X22))
D2b=matrix(c(colSums(D2B00)),n_cov_coef,n_cov_coef)###########
DBa100=(-da1k1*(a1*H1/(1+a1*H1))*X1-k1*(H1/(1+a1*H1)-a1*H1^2/(1+a1*H1)^2)*X1-da1k2*(a2*H2/(1+a2*H2))*X2-
da1k0*(1/(1+a1*H1+a2*H2))*(a1*H1*X1+a2*H2*X2)-k0*((-H1/(1+a1*H1+a2*H2)^2)*(a1*H1*X1+a2*H2*X2)+(1/(1+a1*H1+a2*H2))*(H1*X1)))
DBa200=(-da2k1*(a1*H1/(1+a1*H1))*X1-da2k2*(a2*H2/(1+a2*H2))*X2-k2*(H2/(1+a2*H2)-a2*H2^2/(1+a2*H2)^2)*X2-
da2k0*(1/(1+a1*H1+a2*H2))*(a1*H1*X1+a2*H2*X2)-k0*((-H2/(1+a1*H1+a2*H2)^2)*(a1*H1*X1+a2*H2*X2)+(1/(1+a1*H1+a2*H2))*(H2*X2)))
DBR00=-dRk1*(a1*H1/(1+a1*H1))*X1-dRk2*(a2*H2/(1+a2*H2))*X2-dRk0*(1/(1+a1*H1+a2*H2))*(a1*H1*X1+a2*H2*X2)
Dba100=c(colSums(DBa100))
Dba200=c(colSums(DBa200))
DbR00=c(colSums(DBR00))
Dbtht=rbind(Dba100,Dba200,DbR00)###########
list(D2B=D2b,DBtht=Dbtht)
}


D2bnthtderivdv<-function(newtht,X1,X2,H1,H2,cen1,cen2){
bivi=bivcenindic(Cen1=cen1,Cen2=cen2)
Ic11=bivi$Ic11;Ic10=bivi$Ic10;Ic01=bivi$Ic01
comp1=d2bderiv00dv(newtht=newtht,X1=X1,X2=X2,H1=H1,H2=H2,cen1=cen1,cen2=cen2)
D2B0=comp1$D2B;DBtht0=comp1$DBtht
nulld2b=matrix(0,ncol(X1),ncol(X1));nulltht=matrix(0,3,ncol(X1))
if(length(Ic11)>0){
comp2=d2bderivP11dv(Ic11=Ic11,newtht=newtht,Y1=X1,Y2=X2,H1=H1,H2=H2)
D2B11=comp2$D2B;DBtht11=comp2$DBtht}else{D2B11=nulld2b;DBtht11=nulltht}
if(length(Ic10)>0){
comp3=d2bderivP10dv(Ic10=Ic10,newtht=newtht,Y1=X1,Y2=X2,H1=H1,H2=H2)
D2B10=comp3$D2B;DBtht10=comp3$DBtht}else{D2B10=nulld2b;DBtht10=nulltht}
if(length(Ic01)>0){
comp4=d2bderivP01dv(Ic01=Ic01,newtht=newtht,Y1=X1,Y2=X2,H1=H1,H2=H2)
D2B01=comp4$D2B;DBtht01=comp4$DBtht}else{D2B01=nulld2b;DBtht01=nulltht}
D2b=D2B0+D2B11+D2B10+D2B01
Dbtht=DBtht0+DBtht11+DBtht10+DBtht01
list(D2B=D2b,DBtht=Dbtht)
}

derivdbdxdv<-function (newtht,cen1,cen2,X1,X2,H1,H2,dx1H1,dx1H2){
X1=as.matrix(X1);X2=as.matrix(X2)
a1 = abs(newtht[1]);a2 = abs(newtht[2]);R = abs(newtht[3])
k1=((a1^(-1)-(R*a1^(-1/2)*a2^(-1/2)))+cen1);k2=((a2^(-1)-(R*a1^(-1/2)*a2^(-1/2)))+cen2);k0=(R*a1^(-1/2)*a2^(-1/2)+cen1+cen2)
DB00=-k1*(a1*H1/(1+a1*H1))*X1-k2*(a2*H2/(1+a2*H2))*X2-k0*(1/(1+a1*H1+a2*H2))*(a1*H1*X1+a2*H2*X2)
DBx100=(-k1*((a1*dx1H1/(1+a1*H1))-(a1*H1*a1*dx1H1/(1+a1*H1)^2))*X1-k2*((a2*dx1H2/(1+a2*H2))-(a2*H2*a2*dx1H2/(1+a2*H2)^2))*X2-
k0*((-(a1*dx1H1+a2*dx1H2)/(1+a1*H1+a2*H2)^2)*(a1*H1*X1+a2*H2*X2)+(1/(1+a1*H1+a2*H2))*(a1*dx1H1*X1+a2*dx1H2*X2)))
Dbx100=c(colSums(DBx100))
M1=(1+a1*H1);M2=(1+a2*H2);M12=(1+a1*H1+a2*H2)
c1=(R+(a1^(1/2)*a2^(1/2)))*R;c2=((a1^(1/2)/a2^(1/2))*R-R^2);c3=((a2^(1/2)/a1^(1/2))*R-R^2)
c4=(1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R)
P11=((c1*(1+a1*H1)*(1+a2*H2)+(c2*(1+a1*H1)+c3*(1+a2*H2))*(1+a1*H1+a2*H2)+c4*((1+a1*H1+a2*H2))^2))
dx1P11=((cen1*cen2)/P11)*(( c1*((a1*dx1H1)*(1+a2*H2)+(1+a1*H1)*(a2*dx1H2))+(c2*(1+a1*H1)+c3*(1+a2*H2))*(a1*dx1H1+a2*dx1H2)+
(c2*(a1*dx1H1)+c3*(a2*dx1H2))*(1+a1*H1+a2*H2)+2*c4*((1+a1*H1+a2*H2))*(a1*dx1H1+a2*dx1H2)))
DBP11=((cen1*cen2)/P11)*(c1*(a1*H1*M2*X1+a2*H2*M1*X2)+(c3*M2+c2*M1)*a1*H1*X1+(c3*M2+c2*M1)*a2*H2*X2+
(c3*a2*H2*X2+c2*a1*H1*X1)*M12+2*c4*M12*(a1*H1*X1+a2*H2*X2))
DBx1P11=((cen1*cen2)/P11)*(c1*(a1*(dx1H1*M2+H1*a2*dx1H2)*X1+a2*(dx1H2*M1+H2*a1*dx1H1)*X2)+
(c3*a2*dx1H2+c2*a1*dx1H1)*a1*H1*X1+(c3*M2+c2*M1)*a1*dx1H1*X1+ (c3*M2+c2*M1)*a2*dx1H2*X2+(c3*a2*dx1H2+c2*a1*dx1H1)*a2*H2*X2+
(c3*a2*dx1H2*X2+c2*a1*dx1H1*X1)*M12+(c3*a2*H2*X2+c2*a1*H1*X1)*(a1*dx1H1+a2*dx1H2)+2*c4*(a1*dx1H1+a2*dx1H2)*(a1*H1*X1+a2*H2*X2)+
2*c4*M12*(a1*dx1H1*X1+a2*dx1H2*X2))
Dbx1p11=c(colSums(DBx1P11))-c(colSums(c(dx1P11)*DBP11))
c1=((a1^(1/2)*a2^(-1/2)))*R;c2=(1-(a1^(1/2)*a2^(-1/2))*R)
P10=(c1*M1+c2*M12)
dx1P10=((cen1*(1-cen2))/P10)*(c1*a1*dx1H1+c2*(a1*dx1H1+a2*dx1H2))
DBP10=((cen1*(1-cen2))/P10)*(c1*a1*H1*X1+c2*(a1*H1*X1+a2*H2*X2))
DBx1P10=((cen1*(1-cen2))/P10)*(c1*a1*dx1H1*X1+c2*(a1*dx1H1*X1+a2*dx1H2*X2))
Dbx1p10=c(colSums(DBx1P10))-c(colSums(c(dx1P10)*DBP10))
c1=((a1^(-1/2)*a2^(1/2)))*R;c2=(1-(a1^(-1/2)*a2^(1/2))*R)
P01=(c1*M2+c2*M12)
dx1P01=(((1-cen1)*cen2)/P01)*(c1*a2*dx1H2+c2*(a1*dx1H1+a2*dx1H2))
DBP01=(((1-cen1)*cen2)/P01)*(c1*a2*H2*X2+c2*(a1*H1*X1+a2*H2*X2))
DBx1P01=(((1-cen1)*cen2)/P01)*(c1*a2*dx1H2*X2+c2*(a1*dx1H1*X1+a2*dx1H2*X2))
Dbx1p01=c(colSums(DBx1P01))-c(colSums(c(dx1P01)*DBP01))###
DBx1=Dbx100+Dbx1p11+Dbx1p10+Dbx1p01#######
DBx1
}

derivdbcp<-function (a,R,r,cen1,cen2,X1,X2,H1,H2){
k=(1-r)*R*(r*a)^(-1);k1=(1-r)*(1-R)*(r*a)^(-1)
M1=(1+(a/(1-r))*H1);M2=(1+(a/(1-r))*H2);M12=(1+(a/(1-r))*(H1+H2))
k1*(1-M1^r)
DB00=-k1*r*M1^(r-1)*(a/(1-r))*H1*X1-k1*r*M2^(r-1)*(a/(1-r))*H2*X2-k*r*M12^(r-1)*(a/(1-r))*(H1*X1+H2*X2)
c1=(1-R)^2;c2=(R-R^2);c3=(R^2);c4=(R*a)
P11=(c1*M1^(r-1)*M2^(r-1)+c2*M12^(r-1)*(M1^(r-1)+M2^(r-1))+c3*M12^(2*r-2)+c4*M12^(r-2))
DBP11=((cen1*cen2)/P11)*(  a*c1*(-(M2^(r-1))*(M1^(r-2))*H1*X1-(M2^(r-2))*(M1^(r-1))*H2*X2)+
c2*(-a*(M1^(r-1)+M2^(r-1))*M12^(r-2)*(H1*X1+H2*X2)-a*M12^(r-1)*((M1^(r-2))*H1*X1+ (M2^(r-2))*H2*X2))-
c3*a*2*(M12^(2*r-3))*(H1*X1+H2*X2)+c4*a*(M12^(r-3))*(H1*X1+H2*X2)*((r-2)/(1-r)))
P10=(R*M12^(r-1)+(1-R)*M1^(r-1))
DBP10=((cen1*(1-cen2))/P10)*(-R*a*M12^(r-2)*(H1*X1+H2*X2)-a*(1-R)*M1^(r-2)*H1*X1)
P01=(R*M12^(r-1)+(1-R)*M2^(r-1))
DBP01=(((1-cen1)*cen2)/P01)*(-R*a*M12^(r-2)*(H1*X1+H2*X2)-a*(1-R)*M2^(r-2)*H2*X2)
DB0=DB00+DBP11+DBP10+DBP01
DB=c(colSums((cen1*X1+cen2*X2)))+c(colSums(DB0))###
DB
}

##################################################
############ Expected values of frailty variables####
.expfrailfundv<- function(x,k0,k1,k2,sa1,sa2){
q1<-q2<-q3<-q4<-p1<-p2<-p3<-p4<-su<-NULL
B0<-B1<-B2<-frai<-NULL
sa1<-sa1;sa2<-sa2
if((x[1]==0) & (x[2]==0)){
B0<-(k0/x[5])
B1<-(k1/x[3]);B2<-(k2/x[4])
frai<-c(B0,B1,B2)}
if((x[1]==1) & (x[2]==0)){
q1=((sa1/sa2)*(k0/x[5])); q2=(k1/x[3])
p1=(q1/(q1+q2)); p2=(q2/(q1+q2))
B0=((p1*(k0+1)+p2*k0)/x[5])
B2=(k2/x[4])
B1=((p1*k1+p2*(k1+1))/x[3])
frai<-c(B0,B1,B2)}
if((x[1]==0) & (x[2]==1)){
q1=((sa2/sa1)*(k0/x[5])); q2=(k2/x[4])
p1=(q1/(q1+q2)); p2=(q2/(q1+q2))
B0=((p1*(k0+1)+p2*k0)/x[5])
B1=(k1/x[3])
B2=((p1*k2+p2*(k2+1))/x[4])
frai<-c(B0,B1,B2)}
if((x[1]==1) & (x[2]==1)){
q1=((k0*(k0+1))/x[5]^2); q2=(((sa2/sa1)*(k0*k1))/(x[5]*x[3]))
q3=(((sa1/sa2)*k0*k2)/(x[5]*x[4])); q4=((k1*k2)/(x[3]*x[4]))
su=(q1+q2+q3+q4)
p1=(q1/su); p2=(q2/su); p3=(q3/su); p4=(q4/su)
B0=((p1*(k0+2)+(p2+p3)*(k0+1)+p4*k0)/(x[5]))
B1=(((p1+p3)*k1+(p2+p4)*(k1+1))/(x[3]))
B2=(((p1+p2)*k2+(p3+p4)*(k2+1))/(x[4]))
frai<-c(B0,B1,B2)}
frai
}

.Expefraildv=function(newtht,xx){
a1<-newtht[1];a2<-newtht[2];R<-newtht[3]
sa1<-sqrt(a1);sa2<-sqrt(a2)
H1=xx[,1];H2=xx[,2];cen1=xx[,3];cen2=xx[,4]
k0<-(R/(sa1*sa2));k1<-(1-(sa1/sa2)*R)/a1;k2<-(1-(sa2/sa1)*R)/a2
x<-cbind(cen1,cen2,(1/a1+H1),(1/a2+H2),(1/(sa1*sa2)+(sa1/sa2)*H1+(sa2/sa1)*H2))
frailcomp <- apply(x,1,.expfrailfundv,k0=k0,k1=k1,k2=k2,sa1=sa1,sa2=sa2)
frailcomp =t(as.matrix(frailcomp))
z1=(sa1/sa2)*c(frailcomp[,1])+c(frailcomp[,2])
z2=(sa2/sa1)*c(frailcomp[,1])+c(frailcomp[,3])
list(frail1=log(z1),frail2=log(z2))
}
#####################################################

gradifortht = function (theta,H1,H2,cen1,cen2){
e1 = theta[1];e2 = theta[2];e3 = theta[3]
de1=sum(((exp(e1)/exp(e1)^2 - exp(e3) * ((exp(e1))^((1/2) - 1) * ((1/2) * exp(e1)) * (exp(e2))^(1/2))/((exp(e1))^(1/2) * (exp(e2))^(1/2))^2) *
    log(1 + exp(e1) * H1) - ((1/exp(e1)) - (exp(e3)/((exp(e1))^(1/2) * (exp(e2))^(1/2)))) * (exp(e1) * H1/(1 + exp(e1) * H1)) -
    exp(e3) * ((exp(e1))^((1/2) - 1) * ((1/2) * exp(e1)) * (exp(e2))^(1/2))/((exp(e1))^(1/2) * (exp(e2))^(1/2))^2 * log(1 + exp(e2) * H2) - ((exp(e3)/((exp(e1))^(1/2) *
    (exp(e2))^(1/2))) * (exp(e1) * H1/(1 + exp(e1) * H1 + exp(e2) * H2)) - exp(e3) * ((exp(e1))^((1/2) - 1) * ((1/2) * exp(e1)) *
    (exp(e2))^(1/2))/((exp(e1))^(1/2) * (exp(e2))^(1/2))^2 * log(1 + exp(e1) * H1 + exp(e2) * H2)) + cen1 * cen2 * ((((exp(e1))^((1/2) -
    1) * ((1/2) * exp(e1)) * (exp(e2))^(1/2) * exp(e3) * (1 + exp(e1) * H1) + (exp(e3) + ((exp(e1))^(1/2) * (exp(e2))^(1/2))) *
    exp(e3) * (exp(e1) * H1)) * (1 + exp(e2) * H2) + (((exp(e1))^((1/2) - 1) * ((1/2) * exp(e1))/(exp(e2))^(1/2) * exp(e3) * (1 + exp(e1) *  H1) +
 (((exp(e1))^(1/2)/(exp(e2))^(1/2)) * exp(e3) - (exp(e3))^2) *
    (exp(e1) * H1) - (exp(e2))^(1/2) * ((exp(e1))^((1/2) - 1) * ((1/2) * exp(e1)))/((exp(e1))^(1/2))^2 * exp(e3) * (1 + exp(e2) *
    H2)) * (1 + exp(e1) * H1 + exp(e2) * H2) + ((((exp(e1))^(1/2)/(exp(e2))^(1/2)) * exp(e3) - (exp(e3))^2) * (1 + exp(e1) * H1) + (((exp(e2))^(1/2)/(exp(e1))^(1/2)) *
    exp(e3) - (exp(e3))^2) * (1 + exp(e2) * H2)) * (exp(e1) *  H1)) + (((1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) * exp(e3)) *
    ((exp(e2))^(1/2) * ((exp(e1))^((1/2) - 1) * ((1/2) * exp(e1)))/((exp(e1))^(1/2))^2 *  exp(e3)) - (exp(e1))^((1/2) - 1) * ((1/2) * exp(e1))/(exp(e2))^(1/2) *
    exp(e3) * (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) * exp(e3))) * ((1 + exp(e1) * H1 + exp(e2) * H2))^2 + (1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) *
    exp(e3)) * (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) * exp(e3)) * (2 * (exp(e1) * H1 * ((1 + exp(e1) * H1 + exp(e2) * H2))))))/((exp(e3) +
    ((exp(e1))^(1/2) * (exp(e2))^(1/2))) * exp(e3) * (1 + exp(e1) *  H1) * (1 + exp(e2) * H2) + ((((exp(e1))^(1/2)/(exp(e2))^(1/2)) *
    exp(e3) - (exp(e3))^2) * (1 + exp(e1) * H1) + (((exp(e2))^(1/2)/(exp(e1))^(1/2)) * exp(e3) - (exp(e3))^2) * (1 + exp(e2) * H2)) * (1 + exp(e1) *
    H1 + exp(e2) * H2) + (1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) *  exp(e3)) * (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) * exp(e3)) *
    ((1 + exp(e1) * H1 + exp(e2) * H2))^2) - (exp(e1) * H1/(1 + exp(e1) * H1) + 2 * (exp(e1) * H1/(1 + exp(e1) * H1 + exp(e2) *
    H2)))) + cen1 * (1 - cen2) * ((exp(e1) * H1 - (exp(e1))^((1/2) - 1) * ((1/2) * exp(e1))/(exp(e2))^(1/2) * exp(e3) * (1 + exp(e2) *
    H2) + (exp(e1))^((1/2) - 1) * ((1/2) * exp(e1))/(exp(e2))^(1/2) *  exp(e3))/((1 + exp(e1) * H1) + (1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) *
    exp(e3)) * (1 + exp(e2) * H2) - (1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) * exp(e3))) - (exp(e1) * H1/(1 + exp(e1) * H1) + exp(e1) *
    H1/(1 + exp(e1) * H1 + exp(e2) * H2))) + (1 - cen1) * cen2 * (((exp(e2))^(1/2) * ((exp(e1))^((1/2) - 1) * ((1/2) * exp(e1)))/((exp(e1))^(1/2))^2 *
        exp(e3) * (1 + exp(e1) * H1) + (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) * exp(e3)) * (exp(e1) * H1) - (exp(e2))^(1/2) * ((exp(e1))^((1/2) -
        1) * ((1/2) * exp(e1)))/((exp(e1))^(1/2))^2 * exp(e3))/((1 + exp(e2) * H2) + (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) *
        exp(e3)) * (1 + exp(e1) * H1) - (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) *  exp(e3))) - exp(e1) * H1/(1 + exp(e1) * H1 + exp(e2) * H2))))
de2=sum((cen1 * cen2 * (((exp(e1))^(1/2) * ((exp(e2))^((1/2) - 1) * ((1/2) *
    exp(e2))) * exp(e3) * (1 + exp(e1) * H1) * (1 + exp(e2) * H2) + (exp(e3) + ((exp(e1))^(1/2) * (exp(e2))^(1/2))) * exp(e3) *
    (1 + exp(e1) * H1) * (exp(e2) * H2) + (((exp(e2))^((1/2) - 1) * ((1/2) * exp(e2))/(exp(e1))^(1/2) * exp(e3) * (1 + exp(e2) *  H2) + (((exp(e2))^(1/2)/(exp(e1))^(1/2)) * exp(e3) - (exp(e3))^2) *
    (exp(e2) * H2) - (exp(e1))^(1/2) * ((exp(e2))^((1/2) - 1) * ((1/2) * exp(e2)))/((exp(e2))^(1/2))^2 * exp(e3) * (1 + exp(e1) *
    H1)) * (1 + exp(e1) * H1 + exp(e2) * H2) + ((((exp(e1))^(1/2)/(exp(e2))^(1/2)) * exp(e3) - (exp(e3))^2) * (1 + exp(e1) * H1) + (((exp(e2))^(1/2)/(exp(e1))^(1/2)) *
    exp(e3) - (exp(e3))^2) * (1 + exp(e2) * H2)) * (exp(e2) *  H2)) + (((exp(e1))^(1/2) * ((exp(e2))^((1/2) - 1) * ((1/2) *
    exp(e2)))/((exp(e2))^(1/2))^2 * exp(e3) * (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) *
    exp(e3)) - (1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) * exp(e3)) * ((exp(e2))^((1/2) - 1) * ((1/2) * exp(e2))/(exp(e1))^(1/2) *
        exp(e3))) * ((1 + exp(e1) * H1 + exp(e2) * H2))^2 + (1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) * exp(e3)) * (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) *
    exp(e3)) * (2 * (exp(e2) * H2 * ((1 + exp(e1) * H1 + exp(e2) *  H2))))))/((exp(e3) + ((exp(e1))^(1/2) * (exp(e2))^(1/2))) *
    exp(e3) * (1 + exp(e1) * H1) * (1 + exp(e2) * H2) + ((((exp(e1))^(1/2)/(exp(e2))^(1/2)) *  exp(e3) - (exp(e3))^2) * (1 + exp(e1) * H1) + (((exp(e2))^(1/2)/(exp(e1))^(1/2)) *
    exp(e3) - (exp(e3))^2) * (1 + exp(e2) * H2)) * (1 + exp(e1) * H1 + exp(e2) * H2) + (1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) *
    exp(e3)) * (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) * exp(e3)) * ((1 + exp(e1) * H1 + exp(e2) * H2))^2) - (exp(e2) * H2/(1 +
    exp(e2) * H2) + 2 * (exp(e2) * H2/(1 + exp(e1) * H1 + exp(e2) *  H2)))) - (exp(e3) * ((exp(e1))^(1/2) * ((exp(e2))^((1/2) -
    1) * ((1/2) * exp(e2))))/((exp(e1))^(1/2) * (exp(e2))^(1/2))^2 *  log(1 + exp(e1) * H1) + (((1/exp(e2)) - (exp(e3)/((exp(e1))^(1/2) *
    (exp(e2))^(1/2)))) * (exp(e2) * H2/(1 + exp(e2) * H2)) - (exp(e2)/exp(e2)^2 - exp(e3) * ((exp(e1))^(1/2) * ((exp(e2))^((1/2) -
        1) * ((1/2) * exp(e2))))/((exp(e1))^(1/2) * (exp(e2))^(1/2))^2) * log(1 + exp(e2) * H2)) + ((exp(e3)/((exp(e1))^(1/2) *
    (exp(e2))^(1/2))) * (exp(e2) * H2/(1 + exp(e1) * H1 + exp(e2) * H2)) - exp(e3) * ((exp(e1))^(1/2) * ((exp(e2))^((1/2) - 1) *
    ((1/2) * exp(e2))))/((exp(e1))^(1/2) * (exp(e2))^(1/2))^2 * log(1 + exp(e1) * H1 + exp(e2) * H2))) + cen1 * (1 - cen2) *
    (((exp(e1))^(1/2) * ((exp(e2))^((1/2) - 1) * ((1/2) * exp(e2)))/((exp(e2))^(1/2))^2 *  exp(e3) * (1 + exp(e2) * H2) + (1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) *
        exp(e3)) * (exp(e2) * H2) - (exp(e1))^(1/2) * ((exp(e2))^((1/2) - 1) * ((1/2) * exp(e2)))/((exp(e2))^(1/2))^2 * exp(e3))/((1 +
        exp(e1) * H1) + (1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) * exp(e3)) * (1 + exp(e2) * H2) - (1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) *
        exp(e3))) - exp(e2) * H2/(1 + exp(e1) * H1 + exp(e2) *  H2)) + (1 - cen1) * cen2 * ((exp(e2) * H2 - (exp(e2))^((1/2) -
    1) * ((1/2) * exp(e2))/(exp(e1))^(1/2) * exp(e3) * (1 + exp(e1) * H1) + (exp(e2))^((1/2) - 1) * ((1/2) * exp(e2))/(exp(e1))^(1/2) *
    exp(e3))/((1 + exp(e2) * H2) + (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) * exp(e3)) * (1 + exp(e1) * H1) - (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) *
    exp(e3))) - (exp(e2) * H2/(1 + exp(e2) * H2) + exp(e2) * H2/(1 + exp(e1) * H1 + exp(e2) * H2)))))
de3=sum((exp(e3)/((exp(e1))^(1/2) * (exp(e2))^(1/2)) * log(1 + exp(e1) * H1) + exp(e3)/((exp(e1))^(1/2) * (exp(e2))^(1/2)) * log(1 +
    exp(e2) * H2) - exp(e3)/((exp(e1))^(1/2) * (exp(e2))^(1/2)) * log(1 + exp(e1) * H1 + exp(e2) * H2) + cen1 * cen2 * (((exp(e3) *
    exp(e3) + (exp(e3) + ((exp(e1))^(1/2) * (exp(e2))^(1/2))) * exp(e3)) * (1 + exp(e1) * H1) * (1 + exp(e2) * H2) + ((((exp(e1))^(1/2)/(exp(e2))^(1/2)) *
    exp(e3) - 2 * (exp(e3) * (exp(e3)))) * (1 + exp(e1) * H1) + (((exp(e2))^(1/2)/(exp(e1))^(1/2)) * exp(e3) - 2 * (exp(e3) *
        (exp(e3)))) * (1 + exp(e2) * H2)) * (1 + exp(e1) * H1 + exp(e2) * H2) - ((1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) *
    exp(e3)) * (((exp(e2))^(1/2)/(exp(e1))^(1/2)) * exp(e3)) + ((exp(e1))^(1/2)/(exp(e2))^(1/2)) * exp(e3) * (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) *
        exp(e3))) * ((1 + exp(e1) * H1 + exp(e2) * H2))^2)/((exp(e3) + ((exp(e1))^(1/2) * (exp(e2))^(1/2))) * exp(e3) * (1 + exp(e1) *
    H1) * (1 + exp(e2) * H2) + ((((exp(e1))^(1/2)/(exp(e2))^(1/2)) *  exp(e3) - (exp(e3))^2) * (1 + exp(e1) * H1) + (((exp(e2))^(1/2)/(exp(e1))^(1/2)) *
    exp(e3) - (exp(e3))^2) * (1 + exp(e2) * H2)) * (1 + exp(e1) * H1 + exp(e2) * H2) + (1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) *
    exp(e3)) * (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) * exp(e3)) * ((1 + exp(e1) * H1 + exp(e2) * H2))^2)) - cen1 * (1 - cen2) *
    ((((exp(e1))^(1/2)/(exp(e2))^(1/2)) * exp(e3) * (1 + exp(e2) * H2) - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) * exp(e3))/((1 +
        exp(e1) * H1) + (1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) * exp(e3)) * (1 + exp(e2) * H2) - (1 - ((exp(e1))^(1/2)/(exp(e2))^(1/2)) *
        exp(e3)))) - (1 - cen1) * cen2 * ((((exp(e2))^(1/2)/(exp(e1))^(1/2)) * exp(e3) * (1 + exp(e1) * H1) - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) *
    exp(e3))/((1 + exp(e2) * H2) + (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) * exp(e3)) * (1 + exp(e1) * H1) - (1 - ((exp(e2))^(1/2)/(exp(e1))^(1/2)) *  exp(e3))))))
gd=c(de1,de2,de3)
gd
}

.Llikgammadvln2 = function (theta,H1,H2,cen1,cen2,QQ){
e1 = theta[1];e2 = theta[2];e3 = theta[3]
logli=QQ+sum((-((1/exp(e1))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e1)*H1)-((1/exp(e2))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e2)*H2)-
(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2)))*log(1+exp(e1)*H1+exp(e2)*H2))+cen1*cen2*(-log(1+exp(e1)*H1)-log(1+exp(e2)*H2)-2*log(1+exp(e1)*H1+exp(e2)*H2)+
log(((exp(e3)+((exp(e1))^(1/2)*(exp(e2))^(1/2)))*exp(e3)*(1+exp(e1)*H1)*(1+exp(e2)*H2)+((((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e1)*H1)+
(((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e2)*H2))*(1+exp(e1)*H1+exp(e2)*H2)+
(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*((1+exp(e1)*H1+exp(e2)*H2))^2)))+
cen1*(1-cen2)*(-log(1+exp(e1)*H1)-log(1+exp(e1)*H1+exp(e2)*H2)+
log(((1+exp(e1)*H1)+(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1+exp(e2)*H2)-(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)))))+
(1-cen1)*cen2*(-log(1+exp(e2)*H2)-log(1+exp(e1)*H1+exp(e2)*H2)+
log(((1+exp(e2)*H2)+(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*(1+exp(e1)*H1)-(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))))))
-logli
    }

.fdLlikgammadvln2 = function (theta,H1,H2,cen1,cen2,QQ){
gd=gradifortht(theta=theta,H1=H1,H2=H2,cen1=cen1,cen2=cen2)
-gd
}


.sdLlikgammadv= function (theta,xx){
a1 = abs(theta[1]);a2 = abs(theta[2]);R = abs(theta[3])
H1=xx[,1];H2=xx[,2];cen1=xx[,3];cen2=xx[,4]
llkexprss <- expression((-((1/a1)-(R/(a1^(1/2)*a2^(1/2))))*log(1+a1*H1)-((1/a2)-(R/(a1^(1/2)*a2^(1/2))))*log(1+a2*H2)-
(R/(a1^(1/2)*a2^(1/2)))*log(1+a1*H1+a2*H2))+cen1*cen2*(-log(1+a1*H1)-log(1+a2*H2)-2*log(1+a1*H1+a2*H2)+
log(((R+(a1^(1/2)*a2^(1/2)))*R*(1+a1*H1)*(1+a2*H2)+(((a1^(1/2)/a2^(1/2))*R-R^2)*(1+a1*H1)+
((a2^(1/2)/a1^(1/2))*R-R^2)*(1+a2*H2))*(1+a1*H1+a2*H2)+
(1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R)*((1+a1*H1+a2*H2))^2)))+
cen1*(1-cen2)*(-log(1+a1*H1)-log(1+a1*H1+a2*H2)+
log(((1+a1*H1)+(1-(a1^(1/2)/a2^(1/2))*R)*(1+a2*H2)-(1-(a1^(1/2)/a2^(1/2))*R))))+
(1-cen1)*cen2*(-log(1+a2*H2)-log(1+a1*H1+a2*H2)+
log(((1+a2*H2)+(1-(a2^(1/2)/a1^(1/2))*R)*(1+a1*H1)-(1-(a2^(1/2)/a1^(1/2))*R)))))
dxy <- deriv3(llkexprss, c("a1","a2","R"),hessian = TRUE)
sr=eval(dxy)
sr1<-attributes(sr)
Hes=colSums(sr1$hessian)
-Hes
}

.SEbcfrailgamdv=function(bet,newtht,n_eve,etime,h0,censor,time,X,H){
n_eve<-as.vector(n_eve);etime<-as.vector(etime);censor<-as.vector(censor)
h0<-as.vector(h0);time<-as.vector(time)
newtht<-as.vector(newtht);a1 = (newtht[1]);a2 = (newtht[2]);R = (newtht[3])
X <- as.matrix(X);g0<-c(exp(X%*%bet))
HH=H;HH<-as.vector(HH)
n_cov_coef=n4= ncol(X);data.n1=n1= nrow(X);data.n=n=n1/2
indic1<-2*array(1:data.n)-1;indic2<-2*array(1:data.n)
k0<-(R*a1^(-1/2)*a2^(-1/2));k1<-(1/a1-k0);k2<-(1/a2-k0)
c1<-(R^2+R*a1^(1/2)*a2^(1/2));c2<-(R*(a2^(1/2)/a1^(1/2))-R^2)
c3<-(R*(a1^(1/2)/a2^(1/2))-R^2);c4<-(1-R*(a2^(1/2)/a1^(1/2))-R*(a1^(1/2)/a2^(1/2))+R^2)
da1k0<-(-(1/2)*(R*a1^(-3/2)*a2^(-1/2)));da2k0<-(-(1/2)*(R*a1^(-1/2)*a2^(-3/2)));dRk0<-(a1^(-1/2)*a2^(-1/2))
da1k1<-(-(1/a1^2)-da1k0);da2k1<-(-da2k0);dRk1<-(-dRk0)
da1k2<-(-da1k0);da2k2<-(-(1/a2^2)-da2k0);dRk2<-(-dRk0)
da1c1=((1/2)*R*a1^(-1/2)*a2^(1/2));da2c1=((1/2)*R*a1^(1/2)*a2^(-1/2));dRc1<-(2*R+a1^(1/2)*a2^(1/2))
da1c2<-((-1/2)*R*(a2^(1/2)*a1^(-3/2)));da2c2<-((1/2)*R*(a2^(-1/2)*a1^(-1/2)));dRc2<-((a2^(1/2)/a1^(1/2))-2*R)
da1c3<-((1/2)*R*(a2^(-1/2)*a1^(-1/2)));da2c3<-((-1/2)*R*(a1^(1/2)*a2^(-3/2)));dRc3<-((a1^(1/2)/a2^(1/2))-2*R)
da1c4<-((1/2)*R*(a2^(1/2)*a1^(-3/2))-(1/2)*R*(a1^(-1/2)*a2^(-1/2)));da2c4<-(-(1/2)*R*(a2^(-1/2)*a1^(-1/2))+(1/2)*R*(a1^(1/2)*a2^(-3/2)))
dRc4<-(-(a2^(1/2)*a1^(-1/2))-(a1^(1/2)*a2^(-1/2))+2*R)
H1=HH[indic1];H2=HH[indic2]
time1=time[indic1];time2=time[indic2]
g01=g0[indic1];g02=g0[indic2]
cen1=censor[indic1];cen2=censor[indic2]
di=(cen1+cen2)
X1=X[indic1,];X2=X[indic2,]
X1<- as.matrix(X1)
X2<- as.matrix(X2)
AA=(1+a1*H1+a2*H2);AA1=(1+a1*H1);AA2=(1+a2*H2)
n_eve0=as.numeric(n_eve>0)
trevntimein=n_eve0*array(1:length(n_eve0))
trevntimein1=trevntimein[trevntimein>0]
trevntime=etime[trevntimein1]
nonzero_h0<-h0[trevntimein1]
nev1<-n_eve[trevntimein1]
RIE1 <- apply(as.array(time1),1,rissksetdr,x=trevntime)
RIE001 <-(t(RIE1)*c(g01))
RIE2 <- apply(as.array(time2),1,rissksetdr,x=trevntime)
RIE002 <-(t(RIE2)*c(g02))
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=RIE001);IMX1<-matrix(resinteracmatx,data.n,n_cov_coef*ncol(RIE001))
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=RIE001);IMX12<-matrix(resinteracmatx,data.n,n_cov_coef*ncol(RIE001))
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=RIE002);IMX2<-matrix(resinteracmatx,data.n,n_cov_coef*ncol(RIE002))
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=RIE002);IMX21<-matrix(resinteracmatx,data.n,n_cov_coef*ncol(RIE002))
Dhp00i=c(-c(k1+cen1)*(1/AA1))*(c(a1)*RIE001)-c(c(k2+cen2)*(1/AA2))*(c(a2)*RIE002)-c(c(k0+di)*c(1/AA))*(c(a1)*RIE001+c(a2)*RIE002)
Dbp00i=(c(-c(k1+cen1)*(1/AA1))*(c(a1*H1)*X1)+c(-c(k2+cen2)*(1/AA2))*(c(a2*H2)*X2)+c(-c(k0+di)*(1/AA))*(c(a1*H1)*X1+c(a2*H2)*X2))
Da1p00i= -c(da1k1)*log(AA1)-c(k1+cen1)*(H1/AA1)-c(da1k2)*log(AA2)-c(da1k0)*log(AA)-c(k0+di)*(H1/AA)
Da2p00i= -c(da2k2)*log(AA2)-c(k2+cen2)*(H2/AA2)-c(da2k1)*log(AA1)-c(da2k0)*log(AA)-c(k0+di)*(H2/AA)
DRp00i=-c(dRk1)*log(AA1)-c(dRk2)*log(AA2)-c(dRk0)*log(AA)
D2hp00=(t((c(k1+cen1)*c(1/AA1^2)*c(a1)*RIE001))%*%(c(a1)*RIE001)+ t((c(k2+cen2)*c(1/AA2^2)*c(a2)*RIE002))%*%(c(a2)*RIE002)+
t((c(k0+di)*c(1/AA^2)*(c(a1)*RIE001+c(a2)*RIE002)))%*%(c(a1)*RIE001+c(a2)*RIE002))#####
Dhbp00i= (matrix(c(colSums(c(-c(k1+cen1)*c(1/AA1))*(c(a1)*IMX1))),ncol(RIE001),n_cov_coef)+ t(c(a1)*RIE001)%*%( c(c(k1+cen1)*c(1/AA1^2))*(c(a1*H1)*X1))+
matrix(c(colSums(c(-c(k2+cen2)*c(1/AA2))*(c(a2)*IMX2))),ncol(RIE002),n_cov_coef)+ t(c(a2)*RIE002)%*%(c(c(k2+cen2)*c(1/AA2^2))*(c(a2*H2)*X2))+
matrix(c(colSums(-c(c(k0+di)*c(1/AA))*(c(a1)*IMX1+c(a2)*IMX2))),ncol(RIE001),n_cov_coef)+
t(c(a1)*RIE001+c(a2)*RIE002 )%*%( c(c(k0+di)*c(1/AA^2))*(c(a1*H1)*X1+c(a2*H2)*X2)))####
Dha1p00i=colSums(c(-c(da1k1)*(1/AA1))*(c(a1)*RIE001)  -c(k1+cen1)*(c(1/AA1)*RIE001 -c(H1/AA1^2)*c(a1)*RIE001)-c(c(da1k2)*(1/AA2))*(c(a2)*RIE002)-
c(c(da1k0)*c(1/AA))*(c(a1)*RIE001+c(a2)*RIE002)-c(k0+di)*(c(1/AA)*RIE001 -c(H1/AA^2)*(c(a1)*RIE001+c(a2)*RIE002)))#####
Dha2p00i=colSums(c(-c(da2k2)*(1/AA2))*(c(a2)*RIE002)  -c(k2+cen2)*(c(1/AA2)*RIE002 -c(H2/AA2^2)*c(a2)*RIE002)-c(c(da2k1)*(1/AA1))*(c(a1)*RIE001)-
c(c(da2k0)*c(1/AA))*(c(a1)*RIE001+c(a2)*RIE002)-c(k0+di)*(c(1/AA)*RIE002 -c(H2/AA^2)*(c(a1)*RIE001+c(a2)*RIE002)))######
DhRp00i=colSums(c(-c(dRk1)*(1/AA1))*(c(a1)*RIE001)-c(c(dRk2)*(1/AA2))*(c(a2)*RIE002)-c(c(dRk0)*c(1/AA))*(c(a1)*RIE001+c(a2)*RIE002))#####
P11=( c1*AA1*AA2+c2*AA2*AA+c3*AA1*AA+c4*AA^(2))
DhlogP11=((cen1*cen2)/P11)*( c1*( c(AA2)*(c(a1)*RIE001)+c(AA1)*(c(a2)*RIE002))+
c2*(c(AA2)*(c(a1)*RIE001+c(a2)*RIE002)+ c(AA)*(c(a2)*RIE002))+c3*(c(AA1)*(c(a1)*RIE001+c(a2)*RIE002)+ c(AA)*(c(a1)*RIE001))+
c4*2*AA*(c(a1)*RIE001+c(a2)*RIE002))
DblogP11=((cen1*cen2)/P11)*(c(c1*a1*H1*AA2)*X1+c(c1*AA1*a2*H2)*X2+
c(c2*a2*H2*AA)*X2+c(c2*AA2)*(a1*H1*X1+a2*H2*X2)+c(c3*a1*H1*AA)*X1+c(c3*AA1)*(a1*H1*X1+a2*H2*X2)+c4*2*AA*(a1*H1*X1+a2*H2*X2))
da1logP11=((cen1*cen2)/P11)*(da1c1*AA1*AA2+c1*H1*AA2+da1c2*AA2*AA+c2*H1*AA2+da1c3*AA1*AA+c3*(AA1*H1+H1*AA)+da1c4*AA^(2)+c4*2*AA*H1)
da2logP11=((cen1*cen2)/P11)*(da2c1*AA1*AA2+c1*H2*AA1+da2c2*AA2*AA+c2*(H2*AA+H2*AA2)+da2c3*AA1*AA+c3*AA1*H2+da2c4*AA^(2)+c4*2*AA*H2)
dRlogP11=((cen1*cen2)/P11)*(dRc1*AA1*AA2+dRc2*AA2*AA+dRc3*AA1*AA+dRc4*AA^(2))
D2hlogP110=(t(c(c(cen1*cen2/P11)*c(2)*c1)*(c(a1)*RIE001))%*%(c(a2)*RIE002)+
t(c(c(cen1*cen2/P11)*c(2)*c2)*(c(a2)*RIE002))%*%(c(a1)*RIE001+c(a2)*RIE002)+
t(c(c(cen1*cen2/P11)*c(2)*c3)*(c(a1)*RIE001))%*%(c(a1)*RIE001+c(a2)*RIE002)+
t(c(c(cen1*cen2/P11)*c(2)*c4)*(c(a1)*RIE001+c(a2)*RIE002))%*%(c(a1)*RIE001+c(a2)*RIE002))
D2hlogP11=D2hlogP110-t(DhlogP11)%*%DhlogP11###############
DhblogP110=((cen1*cen2)/P11)*(c1*((c(a1*a2*H2)*IMX12)+c(AA2)*(c(a1)*IMX1)+(c(a2*a1*H1)*IMX21)+c(AA1)*(c(a2)*IMX2))+
c2*((c(a1*a2*H2)*IMX12+c(a2*a2*H2)*IMX2)+ c(AA2)*(c(a1)*IMX1+c(a2)*IMX2)+(c(a1*a2*H1)*IMX21+c(a2*a2*H2)*IMX2)+c(AA)*(c(a2)*IMX2))+
c3*((c(a1*a1*H1)*IMX1+c(a1*a2*H1)*IMX21)+ c(AA1)*(c(a1)*IMX1+c(a2)*IMX2)+(c(a1*a2*H1)*IMX1+c(a2*a2*H2)*IMX12)+c(AA)*(c(a1)*IMX1))+
c4*2*((c(a1*a1*H1)*IMX1+c(a2*a1*H1)*IMX21+c(a1*a2*H2)*IMX12+c(a2*a2*H2)*IMX2)+AA*(c(a1)*IMX1+c(a2)*IMX2)))
DhblogP11=matrix(c(colSums(DhblogP110)),ncol(RIE001),n_cov_coef)-t(DhlogP11)%*%DblogP11#############
Dha1logP110=((cen1*cen2)/P11)*( da1c1*( c(AA2)*(c(a1)*RIE001)+c(AA1)*(c(a2)*RIE002))+c1*(c(AA2)*RIE001+c(H1)*(c(a2)*RIE002))+
da1c2*(c(AA2)*(c(a1)*RIE001+c(a2)*RIE002)+ c(AA)*(c(a2)*RIE002))+c2*(c(AA2)*RIE001+c(H1)*(c(a2)*RIE002))+
da1c3*(c(AA1)*(c(a1)*RIE001+c(a2)*RIE002)+ c(AA)*(c(a1)*RIE001))+c3*(c(H1)*(c(a1)*RIE001+c(a2)*RIE002)+c(AA1)*RIE001+c(H1)*(c(a1)*RIE001)+c(AA)*RIE001)+
da1c4*2*AA*(c(a1)*RIE001+c(a2)*RIE002)+c4*2*(H1*(c(a1)*RIE001+c(a2)*RIE002)+c(AA)*RIE001))
Dha1logP11=colSums((Dha1logP110)-c(da1logP11)*DhlogP11)######
Dha2logP110=((cen1*cen2)/P11)*( da2c1*( c(AA2)*(c(a1)*RIE001)+c(AA1)*(c(a2)*RIE002))+c1*(c(H2)*(c(a1)*RIE001)+c(AA1)*RIE002)+
da2c2*(c(AA2)*(c(a1)*RIE001+c(a2)*RIE002)+ c(AA)*(c(a2)*RIE002))+c2*(c(H2)*(c(a1)*RIE001+c(a2)*RIE002)+c(AA2)*RIE002+c(H2)*(c(a2)*RIE002)+c(AA)*RIE002)+
da2c3*(c(AA1)*(c(a1)*RIE001+c(a2)*RIE002)+ c(AA)*(c(a1)*RIE001))+c3*(c(AA1)*RIE002+ c(H2)*(c(a1)*RIE001))+
da2c4*2*AA*(c(a1)*RIE001+c(a2)*RIE002)+c4*2*(H2*(c(a1)*RIE001+c(a2)*RIE002)+c(AA)*RIE002))
Dha2logP11=colSums((Dha2logP110)-c(da2logP11)*DhlogP11)######
DhRlogP110=((cen1*cen2)/P11)*( dRc1*( c(AA2)*(c(a1)*RIE001)+c(AA1)*(c(a2)*RIE002))+
dRc2*(c(AA2)*(c(a1)*RIE001+c(a2)*RIE002)+ c(AA)*(c(a2)*RIE002))+dRc3*(c(AA1)*(c(a1)*RIE001+c(a2)*RIE002)+ c(AA)*(c(a1)*RIE001))+
dRc4*2*AA*(c(a1)*RIE001+c(a2)*RIE002))
DhRlogP11=colSums((DhRlogP110)-c(dRlogP11)*DhlogP11)######
P10=(a1^(1/2)*a2^(-1/2))*R*AA1+(1-(a1^(1/2)*a2^(-1/2))*R)*AA
DhlogP10=(cen1*(1-cen2)/P10)*(c((a1^(1/2)*a2^(-1/2))*R)*(c(a1)*RIE001)+c((1-(a1^(1/2)*a2^(-1/2))*R))*(c(a1)*RIE001+c(a2)*RIE002))
DblogP10=(cen1*(1-cen2)/P10)*( c((a1^(3/2)*a2^(-1/2))*R*H1)*X1+c((1-(a1^(1/2)*a2^(-1/2))*R))*(c(a1*H1)*X1+c(a2*H2)*X2))
Da1logP10=(cen1*(1-cen2)/P10)*( (1/2)*(a1^(-1/2)*a2^(-1/2))*R*AA1+(a1^(1/2)*a2^(-1/2))*R*H1-(1/2)*(a1^(-1/2)*a2^(-1/2))*R*AA+
(1-(a1^(1/2)*a2^(-1/2))*R)*H1)
Da2logP10=(cen1*(1-cen2)/P10)*((-1/2)*(a1^(1/2)*a2^(-3/2))*R*AA1+(1/2)*(a1^(1/2)*a2^(-3/2))*R*AA +(1-(a1^(1/2)*a2^(-1/2))*R)*H2)
DRlogP10=(cen1*(1-cen2)/P10)*((a1^(1/2)*a2^(-1/2))*AA1-(a1^(1/2)*a2^(-1/2))*AA)
D2hlogP10=-t(DhlogP10)%*%DhlogP10####
DhblogP100=(cen1*(1-cen2)/P10)*(c((a1^(1/2)*a2^(-1/2))*R)*(c(a1)*IMX1)+c((1-(a1^(1/2)*a2^(-1/2))*R))*(c(a1)*IMX1+c(a2)*IMX2))
DhblogP10=matrix(c(colSums(DhblogP100)),ncol(RIE002),n_cov_coef)- t(DhlogP10)%*%DblogP10######
Dha1logP100=(cen1*(1-cen2)/P10)*(c((1/2)*(a1^(-1/2)*a2^(-1/2))*R)*(c(a1)*RIE001)+c((a1^(1/2)*a2^(-1/2))*R)*(RIE001)+
c(-(1/2)*(a1^(-1/2)*a2^(-1/2))*R)*(c(a1)*RIE001+c(a2)*RIE002)+c((1-(a1^(1/2)*a2^(-1/2))*R))*(RIE001))
Dha1logP10=colSums((Dha1logP100)-c(Da1logP10)*DhlogP10)######
Dha2logP100=(cen1*(1-cen2)/P10)*(c((-1/2)*(a1^(1/2)*a2^(-3/2))*R)*(c(a1)*RIE001)+c((1-(a1^(1/2)*a2^(-1/2))*R))*(RIE002)+
c((1/2)*(a1^(1/2)*a2^(-3/2))*R)*(c(a1)*RIE001+c(a2)*RIE002))
Dha2logP10=colSums((Dha2logP100)-c(Da2logP10)*DhlogP10)######
DhRlogP100=(cen1*(1-cen2)/P10)*(c((a1^(1/2)*a2^(-1/2)))*(c(a1)*RIE001)-c((a1^(1/2)*a2^(-1/2)))*(c(a1)*RIE001+c(a2)*RIE002))
DhRlogP10=colSums((DhRlogP100)-c(DRlogP10)*DhlogP10)######
P01=(a2^(1/2)*a1^(-1/2))*R*AA2+(1-(a2^(1/2)*a1^(-1/2))*R)*AA
DhlogP01=((1-cen1)*cen2/P01)*(c((a2^(1/2)*a1^(-1/2))*R)*(c(a2)*RIE002)+c((1-(a2^(1/2)*a1^(-1/2))*R))*(c(a1)*RIE001+c(a2)*RIE002))
DblogP01=((1-cen1)*cen2/P01)*( c((a2^(1/2)*a1^(-1/2))*R*a2*H2)*X2+c((1-(a2^(1/2)*a1^(-1/2))*R))*(c(a1*H1)*X1+c(a2*H2)*X2))
Da1logP01=(((1-cen1)*cen2)/P01)*( -(1/2)*(a1^(-3/2)*a2^(1/2))*R*AA2+(1-(a1^(-1/2)*a2^(1/2))*R)*H1 +(1/2)*(a1^(-3/2)*a2^(1/2))*R*AA)
Da2logP01=(((1-cen1)*cen2)/P01)*( (1/2)*(a1^(-1/2)*a2^(-1/2))*R*AA2+(a1^(-1/2)*a2^(1/2))*R*H2-(1/2)*(a1^(-1/2)*a2^(-1/2))*R*AA+(1-(a1^(-1/2)*a2^(1/2))*R)*H2)
DRlogP01=(((1-cen1)*cen2)/P01)*((a2^(1/2)*a1^(-1/2))*AA2-(a2^(1/2)*a1^(-1/2))*AA)
D2hlogP01=-t(DhlogP01)%*%DhlogP01####
DhblogP010=((1-cen1)*cen2/P01)*(c((a2^(1/2)*a1^(-1/2))*R)*(c(a2)*IMX2)+c((1-(a2^(1/2)*a1^(-1/2))*R))*(c(a1)*IMX1+c(a2)*IMX2))
DhblogP01=matrix(c(colSums(DhblogP010)),ncol(RIE002),n_cov_coef)- t(DhlogP01)%*%DblogP01######
Dha1logP010=((1-cen1)*cen2/P01)*(c( (-1/2)*(a2^(1/2)*a1^(-3/2))*R)*(c(a2)*RIE002)+
(1/2)*(a2^(1/2)*a1^(-3/2))*R*(c(a1)*RIE001+c(a2)*RIE002)+c((1-(a2^(1/2)*a1^(-1/2))*R))*(RIE001))
Dha1logP01=colSums((Dha1logP010)-c(Da1logP01)*DhlogP01)######
Dha2logP010=((1-cen1)*cen2/P01)*(c((1/2)*(a2^(-1/2)*a1^(-1/2))*R)*(c(a2)*RIE002)+c((a2^(1/2)*a1^(-1/2))*R)*(RIE002)+
-(1/2)*(a2^(-1/2)*a1^(-1/2))*R*(c(a1)*RIE001+c(a2)*RIE002)+c((1-(a2^(1/2)*a1^(-1/2))*R))*(RIE002))
Dha2logP01=colSums((Dha2logP010)-c(Da2logP01)*DhlogP01)######
DhRlogP010=((1-cen1)*cen2/P01)*(c((a2^(1/2)*a1^(-1/2)))*(c(a2)*RIE002)-(a2^(1/2)*a1^(-1/2))*(c(a1)*RIE001+c(a2)*RIE002))
DhRlogP01=colSums((DhRlogP010)-c(DRlogP01)*DhlogP01)#####
DbDh=Dhbp00i+DhblogP11+DhblogP10+DhblogP01
D2h=(D2hp00+D2hlogP11+D2hlogP10+D2hlogP01)
diag(D2h)<-diag(D2h)-(nev1/nonzero_h0^2)
DhDa=cbind(c(Dha1p00i+Dha1logP11+Dha1logP10+Dha1logP01),c(Dha2p00i+Dha2logP11+Dha2logP10+Dha2logP01),c(DhRp00i+DhRlogP11+DhRlogP10+DhRlogP01))
comp=D2bnthtderivdv(newtht=newtht,X1=X1,X2=X2,H1=H1,H2=H2,cen1=cen1,cen2=cen2)
D2b=comp$D2B
Dbtht=comp$DBtht
xv=matrix(c(H1,H2,cen1,cen2),data.n,4)
D2a=-.sdLlikgammadv(theta=newtht,xx=xv)
MA=rbind(D2b,Dbtht,DbDh);MB=rbind(t(Dbtht),D2a,DhDa);MC=rbind(t(DbDh),t(DhDa),D2h)
HES=-cbind(MA,MB,MC)
RG2=t(HES)
HES[lower.tri(HES, diag = FALSE)]<-RG2[lower.tri(RG2, diag = FALSE)]
INVE<-solve(HES)
seofthet=as.vector(sqrt(diag(INVE)))
SEofbetandsig=c(seofthet[1:(n_cov_coef+3)])
seeho=c(seofthet[(n_cov_coef+4):length(seofthet)])
ary=array(1:(n_cov_coef+3))
ii=cbind(rep(ary,(n_cov_coef+3)),rep(ary, each=(n_cov_coef+3)))
vcovbtht=matrix(c(INVE[ii]),(n_cov_coef+3),(n_cov_coef+3))
seho=cbind(unqtime=trevntime,ho=nonzero_h0,SEho=seeho)
list(se=SEofbetandsig,vco=-D2a,parvcov=vcovbtht,vcovb=-D2b,seho=seho)}
########## End SE for bcfraildv gamma fit###########
##################################################
#

fitbcdv.gammasp<-function(X,Y,initfrailp,control){
time=Y[, 1];censor=Y[, 2]
order=sort(time, decreasing = FALSE, index.return = TRUE);indx=order$ix;timeo=order$x
uniq_tim<-unique(sort(time))
ind.haz=match(time,uniq_tim)
if(any(is.na(ind.haz))){
tord_diff<-as.array(diff(c(0,timeo)))
id.zero_tord <- which(apply(((tord_diff<0.0000001)&(tord_diff>0)),1, all))
if(length(id.zero_tord)>0){time[indx[id.zero_tord]]<- time[indx[id.zero_tord-1]]
order=sort(time, decreasing = FALSE, index.return = TRUE);indx=order$ix;timeo=order$x}
uniq_tim<-unique(sort(time))
ind.haz=match(time,uniq_tim)}
Y[, 1]<-time
data.n1 <- nrow(X);data.n <-data.n1/2#### data.n is the number of pairs
RI <- apply(as.array(uniq_tim),1,risskset,x=time)
RI <-t(RI)
uniind=array(1:length(uniq_tim))
Rev <- apply(as.array(uniind),1,tofevent,x=ind.haz,cen=censor)
n_eve <-as.vector(t(Rev))
i2<-2*array(1:data.n);i1<-i2-1
cph0 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset = NULL, init = NULL, control = survival::coxph.control(),
weights = NULL, method = "breslow", rownames = NULL)
bet<-cph0$coefficients
x_bet<-X%*%bet
svexp_bet_xo=as.vector(RI%*%(exp(x_bet)))
H0<-cumsum(c(n_eve/svexp_bet_xo))###obtain initial parameter for H0(t)
H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
cen1=censor[i1];cen2=censor[i2]
W<-NULL;new.diff=newdiff=Z=1;beto=bet
NOC=list(cen1=cen1,cen2=cen2,cen2,n_eve=n_eve,ind.haz=ind.haz,i1=i1,i2=i2,censor=censor)
ui0=matrix(0,2,3);ci0=rep(0,2)
ui0[1,1]<-c(-0.5);ui0[1,2]<-c(0.5);ui0[1,3]<-c(-1)
ui0[2,1]<-c(0.5);ui0[2,2]<-c(-0.5);ui0[2,3]<-c(-1)
if(length(initfrailp)>0){newtht=initfrailp
        iter1=0
        repeat{
            iter1=iter1+1
            beto=bet
            xx=matrix(c(H_bet_x[i1],H_bet_x[i2],cen1,cen2),data.n,4)
            e_frail<-.Expefraildv(newtht=newtht,xx=xx)
            W[i1]<-e_frail$frail1;W[i2]<-e_frail$frail2;W[is.na(W)]<-0
            Z=exp(W)
            cph1 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
                                        offset =W, init = NULL, control = survival::coxph.control(),
                                        weights = NULL, method = "breslow", rownames = NULL)
            bet<-cph1$coefficients
            x_bet<-X%*%bet
            svexp_bet_xo=as.vector(RI%*%(exp(x_bet+W)))
            H0<-cumsum(c(n_eve/svexp_bet_xo))
            H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
            newdiff=max(abs(abs(bet)-abs(beto)))
            if((newdiff < 1e-10)  |  (iter1 >= 150)) break}
        xx=matrix(c(H_bet_x[i1],H_bet_x[i2],cen1,cen2),data.n,4)
        QQ=(cph1$loglik[2]-sum(censor*W)+sum(censor))
lik=-.Llikgammadvln2(log(newtht),H1=H_bet_x[i1],H2=H_bet_x[i2],cen1=cen1,cen2=cen2,QQ=QQ)}else{
    datu=data.frame(time=time,censor=censor,X)
    IID=array(1:data.n1);ar=array(1:data.n);PID=rep(ar,each=2)
    form<-as.formula(paste(paste("Surv(time,censor)"),"~",paste(colnames(X), collapse = "+ "),
                           "+ frailty(PID)",collapse=""))
    bcfitt=bcfrailph(form,data=datu)
    bet=bcfitt$coefficients;thty=bcfitt$frailparest
    x_bet<-X%*%bet
    W=log(bcfitt$frail)
svexp_bet_xo=as.vector(RI%*%(exp(x_bet+W)))
H0<-cumsum(c(n_eve/svexp_bet_xo))#
H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
    xx=matrix(c(H_bet_x[i1],H_bet_x[i2],cen1,cen2),data.n,4)
    QQ=(bcfitt$loglilk-sum(censor*W)+sum(censor))+10
    thty[thty<0.0001]=0.0001
    if(thty[2]>0.95){thty[2]=0.8}
    newtht=c(thty[1],thty[1],thty[2])
fittr=do.call(constrOptim, args=c(list(theta=log(newtht),f=.Llikgammadvln2,grad=.fdLlikgammadvln2,
H1=H_bet_x[i1],H2=H_bet_x[i2],cen1=cen1,cen2=cen2,QQ=QQ,ui=ui0,ci=ci0)))
newtht=exp(fittr$par)
lik=-fittr$value}
newthtln=log(newtht)
xx=matrix(c(H_bet_x[i1],H_bet_x[i2],cen1,cen2),data.n,4)
    iter=0
    repeat{
        iter=iter+1
        lik0=lik
        iter1=0
        repeat{
            iter1=iter1+1
            beto=bet
            xx=matrix(c(H_bet_x[i1],H_bet_x[i2],cen1,cen2),data.n,4)
            e_frail<-.Expefraildv(newtht=newtht,xx=xx)
            W[i1]<-e_frail$frail1;W[i2]<-e_frail$frail2
            Z=exp(W)
            cph1 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
                                        offset =W, init = NULL, control = survival::coxph.control(),
                                        weights = NULL, method = "breslow", rownames = NULL)
            bet<-cph1$coefficients
            x_bet<-X%*%bet
            svexp_bet_xo=as.vector(RI%*%(exp(x_bet+W)))
            H0<-cumsum(c(n_eve/svexp_bet_xo))###obtain initial parameter for H0(t)
            H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
            newdiff=max(abs(abs(bet)-abs(beto)))
            if((newdiff < 1e-10)  |  (iter1 >= 150)) break}
        xx=matrix(c(H_bet_x[i1],H_bet_x[i2],cen1,cen2),data.n,4)
        QQ=(cph1$loglik[2]-sum(censor*W)+sum(censor))
        fittr=do.call(constrOptim, args=c(list(theta=log(newtht),f=.Llikgammadvln2,grad=.fdLlikgammadvln2,
                                               H1=H_bet_x[i1],H2=H_bet_x[i2],cen1=cen1,cen2=cen2,QQ=QQ,ui=ui0,ci=ci0)))
        newtht=exp(fittr$par)
        lik=-fittr$value
        new.diff=lik-lik0
        if((new.diff < control$tol)  |  (iter >= control$max.iter)) break}
h0=diff(c(0,H0));nonzero_h0=h0[h0>0]
H=H0[ind.haz];h=h0[ind.haz];exp_bet_x=exp(x_bet)
vcov=cph1$var
colnames(vcov) <- rownames(vcov) <- colnames(X)
adj_se=.SEbcfrailgamdv(bet=bet,newtht=newtht,n_eve=n_eve,etime=uniq_tim,h0=h0,
censor=censor,time=time,X=X,H=c(H_bet_x))
adjse=c(adj_se$se)
seho=adj_se$seho
ary=array(1:length(bet))
ary1=array((1+length(bet)):(length(bet)+3))
vcovbtht=adj_se$parvcov
ii1=cbind(rep(ary,length(bet)),rep(ary, each=length(bet)))
vcov=matrix(c(vcovbtht[ii1]),length(bet),length(bet))
ii2=cbind(rep(ary1,3),rep(ary1, each=3))
vcovth=matrix(c(vcovbtht[ii2]),3,3)
vcov2=cph1$var
vcovth2<-solve(adj_se$vco)
colnames(vcovth) <- rownames(vcovth) <- c("theta1","theta2","Row")
if(any(is.na(adjse))){
ncoef <- ncol(X)
if(any(is.na(adjse[1:ncoef]))){vcov=vcov2;adjse[1:ncoef]=sqrt(diag(vcov))}
if(any(is.na(adjse[(1+ncoef):(ncoef+length(adjse))]))){
falsese=sqrt(abs(diag(vcovth)))
fakese=c(adjse[1:ncoef],falsese)
adjse[is.na(adjse)]<-fakese[is.na(adjse)]
warning("The standard error of the frailty parameters are not correct.")
warning("frailty parameters might be at the boundary of parameter space.")
warning("In obtaining SE Na is produced and not correct SE for frailty parameters")}}
res <-list(coefficients=bet,frailparest= c(theta1=newtht[1],theta2=newtht[2],Row=newtht[3]),
vcov = vcov,vcovth= vcovth,stderr=adjse,loglilk0=cph0$loglik[2],loglilk=cph1$loglik[2],
Iloglilk=lik,X=X,time=time,censor=censor,resid=H_bet_x,lin.prid=cph1$linear.predictors,
frail=exp(W),iteration=fittr$outer.iterations,e.time=uniq_tim,n.event=n_eve,bhaz=seho,
converg = fittr$convergence)
res$call <- match.call()
class(res) <- c("bcfraildv")
res
}

########################################
####################for log normal fits#############
#####################################
llpennlognGenidv<-function (par,newtht,censor,X,RI,n_eve,ind.haz,i1,i2){
ncoef=ncol(X);n1=nrow(X)
a1=newtht[1];a2=newtht[2];roh=newtht[3]
bet=par[1:ncoef]
W=par[(ncoef+1):length(par)]
W1=W[i1];W2=W[i2]
x_bet<-X%*%bet
x_bet_w<-X%*%bet+W
expx_bet_w<-exp(c(x_bet_w))
svexp_bet_xo=as.vector(RI%*%(exp(x_bet_w)))
dbpart1svexp=(RI%*%(c(exp(x_bet_w))*X))
af=c(n_eve/svexp_bet_xo)
dbsvexp_bet_xo=as.vector(colSums(af*dbpart1svexp))
sumx=as.vector(colSums((c(censor)*X)))
H0<-cumsum(c(n_eve/svexp_bet_xo))
H_bet_xw=c(H0[ind.haz]*expx_bet_w)
llpennlognGenidv=(-sum(censor*(c(x_bet_w)))+sum(n_eve*log(svexp_bet_xo))+
(1/(2*(1-roh^2)))*sum((1/a1)*W1^2+(1/a2)*W2^2-2*(roh/(a1^(1/2)*a2^(1/2)))*W1*W2))
Dw<-(-(censor)+c(H_bet_xw))
Dw[i1]<-Dw[i1]+((1/a1)*W1-(roh/(a1^(1/2)*a2^(1/2)))*W2)*(1/(1-roh^2))
Dw[i2]<-Dw[i2]+((1/a2)*W2-(roh/(a1^(1/2)*a2^(1/2)))*W1)*(1/(1-roh^2))
attr(llpennlognGenidv,"gradient")<-c(c(-sumx+dbsvexp_bet_xo),c(Dw))
llpennlognGenidv
}
Inv.HesPPLdv<-function(W,bet,newtht,X,RI,INC,n_eve,ind.haz,indx){
ncoef=ncol(X);n=nrow(X)
a1=newtht[1];a2=newtht[2];roh=newtht[3]
i1=INC[,1];i2=INC[,2];i3=INC[,3]
x_bet_w<-X%*%bet+W
expx_bet<-exp(c(x_bet_w))
svexp_bet_xo=as.vector(RI%*%(exp(x_bet_w)))
H0<-cumsum(c(n_eve/svexp_bet_xo))#
as0=H_bet_xw=H0[ind.haz]*expx_bet
as0=as0[indx]
vexpx_bet=expx_bet[indx]
ak=cumsum(c((-n_eve/svexp_bet_xo^2)))
as2=ak[ind.haz]*expx_bet
as2=as2[indx]
ma0<-matrix(0,n,n)
ma0[lower.tri(ma0, diag = TRUE)]<-1
ma1<-t((ma0*vexpx_bet))
ma2<-ma1*c(as2)
digg<-diag(ma2)+as0
ma3<-t(ma2)
D2W<-ma3+ma2
diag(D2W)<-digg
ma0[indx,]=D2W
ma1=t(ma0)
D2W[indx,]=ma1
ma0<-ma1<-ma2<-ma3<-NULL
digu=diag(D2W)
digu[i1]=digu[i1]+(1/(a1*(1-roh^2)))
digu[i2]=digu[i2]+(1/(a2*(1-roh^2)))
diag(D2W)<-digu
D2W[cbind(i1,i2)]<-D2W[cbind(i1,i2)]-(roh/(a1^(1/2)*a2^(1/2)*(1-roh^2)))
D2W[cbind(i2,i1)]<-D2W[cbind(i2,i1)]-(roh/(a1^(1/2)*a2^(1/2)*(1-roh^2)))
AB<-try(chol(D2W),silent=TRUE)
if(length(attr(AB,"class"))==0){INVE<-chol2inv(AB);diet<-2*sum(log(diag(AB)))}
if(length(attr(AB,"class"))!=0){INVE<-solve(D2W);diet<-log(det(D2W))}
ij=cbind(i1,i2);as3=INVE[ij];as2=diag(INVE)
Hii1=sum(as2[i1]);Hii2=sum(as2[i2]);Hij=sum(as3)
if(is.infinite(diet)){
ij=cbind(i1,i2)
as3=D2W[ij];as2=diag(D2W)
diet=sum(log(as2[i1]*as2[i2]-as3^2))}
list(Hii1=Hii1,Hii2=Hii2,Hij=Hij,ddet=diet)
}

SEbetalogndv= function(W,bet,newtht,X,RI,INC,n_eve,ind.haz,indx,Xoxo){
ncoef=length(bet);n1=length(W);n=n1/2
a1=newtht[1];a2=newtht[2];r=newtht[3]
i1=INC[,1];i2=INC[,2];i3=INC[,3]
x_bet_w<-X%*%bet+W
expx_bet<-exp(c(x_bet_w))
svexp_bet_xo=as.vector(RI%*%(exp(x_bet_w)))
dbpart1svexp=(RI%*%(c(exp(x_bet_w))*X))
af=c(n_eve/svexp_bet_xo)
H0<-cumsum(c(n_eve/svexp_bet_xo))##
as0=H_bet_xw=H0[ind.haz]*expx_bet
as0=as0[indx]
vexpx_bet=expx_bet[indx]
ak=cumsum(c((-n_eve/svexp_bet_xo^2)))
as2=ak[ind.haz]*expx_bet
as2=as2[indx]
ma0<-matrix(0,n1,n1)
ma0[lower.tri(ma0, diag = TRUE)]<-1
ma1<-t((ma0*vexpx_bet))
ma2<-ma1*c(as2)
digg<-diag(ma2)+as0
ma3<-t(ma2)
D2W<-ma3+ma2
diag(D2W)<-digg
ma0[indx,]=D2W
ma1=t(ma0)
D2W[indx,]=ma1
ma0<-ma1<-ma2<-ma3<-NULL
digu=diag(D2W)
digu[i1]=digu[i1]+(1/(a1*(1-r^2)))
digu[i2]=digu[i2]+(1/(a2*(1-r^2)))
diag(D2W)<-digu
D2W[cbind(i1,i2)]<-D2W[cbind(i1,i2)]-(r/(a1^(1/2)*a2^(1/2)*(1-r^2)))
D2W[cbind(i2,i1)]<-D2W[cbind(i2,i1)]-(r/(a1^(1/2)*a2^(1/2)*(1-r^2)))
dwbpart20<-(dbpart1svexp)*c(-n_eve/svexp_bet_xo^2)
dwbpart21<-apply(as.matrix(dwbpart20),2,matcolcumsum)
dwbpart22=dwbpart21[ind.haz,]
DBW=dwbpart22*expx_bet+H_bet_xw*X
dwbpart30<-(af*dbpart1svexp)
dwbpart31<-(c(1/svexp_bet_xo)*dbpart1svexp)
d2bpart1svexp=(RI%*%(c(expx_bet)*Xoxo))
d2bsvexp_bet_xo=matrix(c(as.vector(colSums(af*d2bpart1svexp))),ncoef,ncoef)
D2B=d2bsvexp_bet_xo-t(dwbpart30)%*%dwbpart31
MA=rbind(D2B,DBW);MB=rbind(t(DBW),D2W)
HES=cbind(MA,MB)
AB<-try(chol(HES),silent=TRUE)
if(length(attr(AB,"class"))==0){INVE<-chol2inv(AB)}
if(length(attr(AB,"class"))!=0){INVE<-solve(HES)}
se=sqrt(diag(INVE))
SEofbetandsig=se[1:ncoef]
er1=array(1:ncoef)
inx=cbind(rep(er1,ncoef),rep(er1,each=ncoef))
vcovb=matrix(c(INVE[inx]),ncoef,ncoef)
INVE<-HES<-AB<-NULL
list(SE=SEofbetandsig,vcovb=vcovb)}
#############################################
#
fitbcdv.logn<-function(X,Y,initfrailp,control){
time=Y[, 1];censor=Y[, 2]
order=sort(time, decreasing = FALSE, index.return = TRUE);indx=order$ix;timeo=order$x
uniq_tim<-unique(sort(time))
ind.haz=match(time,uniq_tim)
if(any(is.na(ind.haz))){
tord_diff<-as.array(diff(c(0,timeo)))
id.zero_tord <- which(apply(((tord_diff<0.0000001)&(tord_diff>0)),1, all))
if(length(id.zero_tord)>0){time[indx[id.zero_tord]]<- time[indx[id.zero_tord-1]]
order=sort(time, decreasing = FALSE, index.return = TRUE);indx=order$ix;timeo=order$x}
uniq_tim<-unique(sort(time))
ind.haz=match(time,uniq_tim)}
Y[, 1]<-time
order=sort(time, decreasing = FALSE, index.return = TRUE);indx=order$ix;timeo=order$x
uniq_tim<-unique(sort(time))
data.n1 <- nrow(X);data.n <-data.n1/2#### data.n is the number of pairs
ncovar_coef<-ncol(X)
ar=array(1:data.n)
uniind=array(1:length(uniq_tim))
Rev <- apply(as.array(uniind),1,tofevent,x=ind.haz,cen=censor)
n_eve <-as.vector(t(Rev))
i1<-2*ar-1;i2<-2*ar
clustind=array(1:data.n1)
uniq_clus_id=unique(clustind)
firi<-match(uniq_clus_id,clustind)
firi2=firi[-1]-1
firi3<-c(firi2,length(clustind))
tau=cbind(firi,firi3)
RI <- apply(as.array(uniq_tim),1,risskset,x=time)
RI <-t(RI)
cen1=censor[i1];cen2=censor[i2]
resinteracmat<-apply(as.matrix(X),2,interacmat,u=X)
Xoxo<-matrix(resinteracmat,data.n1,ncovar_coef^2)
cph0 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset = NULL, init = NULL, control = survival::coxph.control(),
weights = NULL , method = "breslow", rownames = NULL)
bet<-cph0$coefficients
x_bet<-X%*%bet
svexp_bet_xo=as.vector(RI%*%(exp(x_bet)))
H0=cumsum(n_eve/svexp_bet_xo)
H_bet_x=H0[ind.haz]*c(exp(X%*%bet))
W=estimo=new.diff=NULL;new.diff=1
i3=indicmin(i1=i1,i2=i2,time)
INC<-matrix(c(i1,i2,i3),data.n,3)
ii=cbind(i1,i2)
newtht<-NULL
if(length(initfrailp)>0){newtht<-initfrailp}else{
datu=data.frame(time=time,censor=censor,X)
IID=array(1:data.n1);ar=array(1:data.n);PID=rep(ar,each=2)
form<-as.formula(paste(paste("Surv(time,censor)"),"~",paste(colnames(X), collapse = "+ "),
"+ frailty.gaussian(IID)",collapse=""))
cph.fit <- coxph(form,data=datu,method = "breslow")
bet=cph.fit$coefficients
re=cph.fit$history$`frailty.gaussian(IID)`$history
re1=re[,1];tht=re1[length(re1)]
if(tht<0.01){
form<-as.formula(paste(paste("Surv(time,censor)"),"~",paste(colnames(X), collapse = "+ "),
"+ frailty.gaussian(PID)",collapse=""))
cph.fit <- coxph(form,data=datu,method = "breslow")
bet=cph.fit$coefficients
re=cph.fit$history$`frailty.gaussian(PID)`$history
re1=re[,1];tht=re1[length(re1)]}
if(tht<0.01){newtht=c(0.1,0.1,0)}else{newtht=c(tht,tht,0)}}
paro=rep(0,(data.n1+length(bet)))
paro[1:length(bet)]=bet
fittrp=do.call(nlm, args=c(list(f=llpennlognGenidv,p=paro,newtht=newtht,
censor=censor,X=X,RI=RI,n_eve=n_eve,ind.haz=ind.haz,i1=i1,i2=i2,steptol= 1e-06,fscale = 1,
print.level = 0, ndigit = 12,iterlim=20,gradtol = 1e-06,check.analyticals = FALSE)))
paro=fittrp$estimate;W=paro[(1+ncovar_coef):(data.n1+ncovar_coef)]
bet=paro[1:ncovar_coef];W1=W[i1];W2=W[i2]
oppD2W<-Inv.HesPPLdv(W=W,bet=bet,newtht=newtht,X=X,RI=RI,INC=INC,
n_eve=n_eve,ind.haz=ind.haz,indx=indx)
ddet=oppD2W$ddet
lik0=-0.5*data.n*(log(newtht[1])+log(newtht[2])+log(1-newtht[3]^2))-0.5*ddet-fittrp$minimum
bet0=bet;newtht0=newtht;W0=W
iter=0
repeat{
iter=iter+1
lik=lik0;bet=bet0;newtht=newtht0;W=W0
fittrp=do.call(nlm, args=c(list(f=llpennlognGenidv,p=paro,newtht=newtht,
censor=censor,X=X,RI=RI,n_eve=n_eve,ind.haz=ind.haz,i1=i1,i2=i2,steptol= 1e-06,fscale = 1,
print.level = 0, ndigit = 12,iterlim=20,gradtol = 1e-06,check.analyticals = FALSE)))
paro=fittrp$estimate;W0=paro[(1+ncovar_coef):(data.n1+ncovar_coef)]
bet0=paro[1:ncovar_coef];W1=W0[i1];W2=W0[i2]
oppD2W<-Inv.HesPPLdv(W=W0,bet=bet0,newtht=newtht,X=X,RI=RI,INC=INC,
n_eve=n_eve,ind.haz=ind.haz,indx=indx)
Hii1=oppD2W$Hii1;Hii2=oppD2W$Hii2;Hii=Hii1+Hii2
Hij=oppD2W$Hij;ddet=oppD2W$ddet
wsq1=sum(W1^2);wsq2=sum(W2^2)
V1=Hii1+wsq1;V2=Hii2+wsq2;J1=Hij+sum(W1*W2)
r0=(J1/(sqrt(V1)*sqrt(V2)))
a01=(V1-(J1^2)/V2)/(data.n*(1-r0^2))
a02=(a01*V2)/V1
if(a01<=0.00001){a01<-0.00001};if(a02<=0.00001){a02<-0.00001}
if(r0<=c(-0.999)){r0<-c(-0.999)};if(r0>=c(0.999)){r0<-c(0.999)}
newtht0=c(a01,a02,r0)
lik0=-0.5*data.n*(log(a01)+log(a02)+log(1-r0^2))-0.5*ddet-fittrp$minimum
new.diff=(lik0-lik)
if((new.diff < control$tol)  |  (iter >= control$max.iter)) break}
cph1 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset = W, init = NULL, control = survival::coxph.control(),
weights = NULL , method = "breslow", rownames = NULL)
bet=cph1$coefficients
x_bet<-X%*%bet
svexp_bet_xo=as.vector(RI%*%(exp(x_bet+W)))
H0=cumsum(n_eve/svexp_bet_xo)
H_bet_x=H0[ind.haz]*c(exp(X%*%bet));h0=diff(c(0,H0))
n_eve0=as.numeric(n_eve>0);trevntimein=n_eve0*array(1:length(n_eve0))
trevntimein1=trevntimein[trevntimein>0]
trevntime=uniq_tim[trevntimein1];nonzero_h0<-h0[trevntimein1]
seho=cbind(unqtime=trevntime,ho=nonzero_h0)
obthes=SEbetalogndv(W=W,bet=bet,newtht=newtht,X=X,RI=RI,INC=INC,
n_eve=n_eve,ind.haz=ind.haz,indx=indx,Xoxo=Xoxo)
adjse<-obthes$SE;vcov<-obthes$vcovb;vcov2=cph1$var
if(anyNA(adjse)){if(anyNA(sqrt(diag(vcov)))){adjse<-c(sqrt(diag(vcov2)));vcov=vcov2}}
res <-list(coefficients=bet,frailparest=newtht,
vcov2 = vcov,stderr=c(adjse),loglilk0=cph0$loglik[2],loglilk=cph1$loglik[2],
Iloglilk=lik,bhaz=seho,X=X,time=time,censor=censor,
resid=cph1$residuals,lin.prid=cph1$linear.predictors,
frail=exp(W),iteration=iter,convergence<-ifelse(new.diff < control$tol,0,1),
e.time=uniq_tim,n.event=n_eve)
res$call <- match.call()
class(res) <- c("bcfraildv")
res
}
#
genbcfraildv<-function(psize,cenr,cent1,cent2,beta,frailty,fpar,bhaz,bhazpar,
covartype,covarpar,inputcovar,covar1,covar2,comncovar){
n<-psize; n1=n*2;IID=array(1:n1);PID=1;e1=array(1:n);indic2=2*e1;indic1=indic2-1
PID[indic1]=e1;PID[indic2]=e1
if(frailty==c("gamma")){
lam0=1/(sqrt(fpar[1])*sqrt(fpar[2]))
k0=fpar[3]/(sqrt(fpar[1])*sqrt(fpar[2]))
k1=(1/fpar[1])-k0;k2=(1/fpar[2])-k0
lam1=(1/fpar[1]);lam2=(1/fpar[2])
y0=rgamma(n,shape=k0,scale=1/lam0) ;y1=rgamma(n,shape=k1,scale=1/lam1)
y2=rgamma(n,shape=k2,scale=1/lam2)
z1=((lam0/lam1)*y0+y1);z2=((lam0/lam2)*y0+y2)}
if(frailty==c("lognormal")){
WW=rbivnormdv(psize=psize,ssq1=fpar[1],ssq2=fpar[2],r=fpar[3])
z1=exp(WW[,1]);z2=exp(WW[,2])}
if(length(beta)>0){
Beta=matrix(beta,length(beta),1)
if(length(inputcovar)==0){
covar1<-covar2<-matrix(0,psize,length(beta))
for(j in 1:length(beta)){
if(covartype[j]==c("B")){
x1=rbinom(n,size=covarpar$fargs[j],prob=covarpar$sargs[j])
x2=rbinom(n,size=covarpar$fargs[j],prob=covarpar$sargs[j])
covar1[,j]=x1;covar2[,j]=x2}
if(covartype[j]==c("U")){
x1=runif(n,min=covarpar$fargs[j],max=covarpar$sargs[j])
x2=runif(n,min=covarpar$fargs[j],max=covarpar$sargs[j])
covar1[,j]=x1;covar2[,j]=x2}
if(covartype[j]==c("N")){
x1=rnorm(n,mean=covarpar$fargs[j],sd=covarpar$sargs[j])
x2=rnorm(n,mean=covarpar$fargs[j],sd=covarpar$sargs[j])
covar1[,j]=x1;covar2[,j]=x2}}}
if(length(comncovar)>0){covar1[,comncovar]=covar2[,comncovar]}
u1<-runif(n,  min=0, max=1);u2<-runif(n,  min=0, max=1)
if (bhaz==c("weibull")){
T1 <- (-log(u1) / ((bhazpar$scale)*z1*exp(c(covar1%*%Beta))))^(1/(bhazpar$shape))
T2 <- (-log(u2) / ((bhazpar$scale)*z2*exp(c(covar2%*%Beta))))^(1/(bhazpar$shape))}
if (bhaz==c("gompertz")){
T1 <- 1/(bhazpar$shape)*log(1-(bhazpar$shape)*log(u1)/((bhazpar$scale)*exp(c(covar1%*%Beta))*z1))
T2 <- 1/(bhazpar$shape)*log(1-(bhazpar$shape)*log(u2)/((bhazpar$scale)*exp(c(covar2%*%Beta))*z2))}
if (bhaz==c("exponential")){
eij1=rexp(n,rate=(bhazpar$scale));eij2=rexp(n,rate=(bhazpar$scale))
T1=exp(-c(covar1%*%Beta)-log(z1))*eij1;T2=exp(-c(covar2%*%Beta)-log(z2))*eij2}}
if(length(beta)==0){
u1<-runif(n,  min=0, max=1);u2<-runif(n,  min=0, max=1)
if (bhaz==c("weibull")){
T1 <- (-log(u1) / ((bhazpar$scale)*z1))^(1/(bhazpar$shape))
T2 <- (-log(u2) / ((bhazpar$scale)*z2))^(1/(bhazpar$shape))}
if (bhaz==c("gompertz")){
T1 <- 1/(bhazpar$shape)*log(1-(bhazpar$shape)*log(u1)/((bhazpar$scale)*z1))
T2 <- 1/(bhazpar$shape)*log(1-(bhazpar$shape)*log(u2)/((bhazpar$scale)*z2))}
if (bhaz==c("exponential")){
eij1=rexp(n,rate=(bhazpar$scale));eij2=rexp(n,rate=(bhazpar$scale))
T1=exp(-log(z1))*eij1;T2=exp(-log(z2))*eij2}}
cen1=cen2<-t1<-t2<-NULL
if(cenr==0){t1=T1;t2=T2;cen1=cen2=rep(1,n)}
if(cenr>0){
if((length(cent1)==0)&(length(cent2)==0)){
cet1<-quantile(c(T1,T2), probs = c(1-cenr))
t1=pmin(T1,cet1);cen1=as.numeric(T1<=cet1)
t2=pmin(T2,cet1);cen2=as.numeric(T2<=cet1)}
if((length(cent1)>0)&(length(cent2)>0)){
t1=pmin(T1,cent1);cen1=as.numeric(T1<=cent1)
t2=pmin(T2,cent2);cen2=as.numeric(T2<=cent2)}}
time<-censor<-NULL
time[indic1]=t1;time[indic2]=t2;censor[indic1]=cen1;censor[indic2]=cen2
if(length(beta)>0){
RW=matrix(0,n1,length(beta));RW[indic1,]<-covar1;RW[indic2,]<-covar2
if(length(inputcovar)==0){
if(length(beta)==1){colnames(RW)<-c("X1")}
if(length(beta)==2){colnames(RW)<-c("X1","X2")}
if(length(beta)==3){colnames(RW)<-c("X1","X2","X3")}
if(length(beta)==4){colnames(RW)<-c("X1","X2","X3","X4")}
if(length(beta)==5){colnames(RW)<-c("X1","X2","X3","X4","X5")}
if(length(beta)>5){colnames(RW)<-c(rep(c("X"),length(beta)))}}
if(length(inputcovar)>0){
nam=colnames(covar1)
if(length(nam)==0){colnames(RW)<-c(rep(c("X"),length(beta)))}
if(length(nam)>0){colnames(RW)<-nam}}
dataa=cbind(IID=IID,PID=PID,time=time,censor=censor,RW)
dataa=as.data.frame(dataa)}
if(length(beta)==0){
dataa=cbind(IID=IID,PID=PID,time=time,censor=censor)
dataa=as.data.frame(dataa)}
dataa
}
##############################################################################

############Parametric bcfrail utilities########
############Gamma frailty with Gompertz baseline ########
#####################################################
#
#######################with covariates#######################
.Llikgompertzcv = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);v = abs(theta[4]);bet = theta[5:length(theta)]
EBX1=exp(c(X1%*%bet));EBX2=exp(c(X2%*%bet))
H01=(la/v)*(exp(v*t1)-1);H02=(la/v)*(exp(v*t2)-1);H1=H01*EBX1;H2=H02*EBX2
logli=sum( (cen1*(log(la)+v*t1+c(X1%*%bet))+cen2*(log(la)+v*t2+c(X2%*%bet))-
((1-R)/a)*(log(1+a*H1)+log(1+a*H2))-(R/a)*log(1+a*(H1+H2)))+
cen1*cen2*log((R+a)*R*(1+a*(H1+H2))^(-2)+(1-R)*R*(1+a*(H1+H2))^(-1)*((1+a*H1)^(-1)+(1+a*H2)^(-1))+
((1-R)^2)*(1+a*H1)^(-1)*(1+a*H2)^(-1))+
cen1*(1-cen2)*log(R*(1+a*(H1+H2))^(-1)+(1-R)*(1+a*H1)^(-1))+
(1-cen1)*cen2*log(R*(1+a*(H1+H2))^(-1)+(1-R)*(1+a*H2)^(-1)))
-logli}

.fdLlikgompertzcv = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);v = abs(theta[4]);bet = theta[5:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
H1=H2<-NULL
llkexprss <- expression(((cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)-
((1-R)/a)*(log(1+a*(la/v)*(exp(v*t1)-1)*eb1)+log(1+a*(la/v)*(exp(v*t2)-1)*eb2))-
(R/a)*log(1+a*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2)))+
cen1*cen2*log((R+a)*R*(1+a*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(-2)+
(1-R)*R*(1+a*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(-1)*
((1+a*(la/v)*(exp(v*t1)-1)*eb1)^(-1)+(1+a*(la/v)*(exp(v*t2)-1)*eb2)^(-1))+
((1-R)^2)*(1+a*(la/v)*(exp(v*t1)-1)*eb1)^(-1)*(1+a*(la/v)*(exp(v*t2)-1)*eb2)^(-1))+
cen1*(1-cen2)*log(R*(1+a*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(-1)+(1-R)*(1+a*(la/v)*(exp(v*t1)-1)*eb1)^(-1))+
(1-cen1)*cen2*log(R*(1+a*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(-1)+(1-R)*(1+a*(la/v)*(exp(v*t2)-1)*eb2)^(-1))))
dxy <- deriv(llkexprss, c("a", "R","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
H01=(la/v)*(exp(v*t1)-1);H02=(la/v)*(exp(v*t2)-1)
H1=H01*eb1;H2=H02*eb2
db<-derivdbcv(theta=theta,cen1=cen1,cen2=cen2,X1=X1,X2=X2,H1=H1,H2=H2)
db=as.vector(db)
gd=c(c(gdpart1),c(db))
-gd
}

optimgomperzcv<-function(thet,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2)
ui0=matrix(0,5,length(thet));ci0=rep(0,5);ci0[3]<-(-1)
ui0[1,1]<-1;ui0[2,2]<-1;ui0[3,2]<-(-1);ui0[4,3]<-(1);ui0[5,4]<-(1)
if(thet[1]<0.000001){thet[1]<-0.000001};if(thet[2]<0.000001){thet[2]<-0.000001}
if(thet[3]>=1){thet[3]<-(1-0.000001)};if(thet[3]<0.000001){thet[3]<-0.000001}
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikgompertzcv,grad=.fdLlikgompertzcv,
xx=xx,X1=X1,X2=X2,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("theta","Row","scale","shape",colnames(X1))
SE=sqrt(diag(INVE))
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(coefficients=newtht[5:length(thet)],frailparest=c(theta=newtht[1],Row=newtht[2]),
basehazpar=c(scale=newtht[3],shape=newtht[4]),vcov=INVE,stderr=SE,loglik=lik,
convergence=fittr$convergence,iterations=fittr$outer.iterations,AIC=aic,BIC=bic)
res
}

####################### without covariates#######################
.Llikgompertzcvnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);v = abs(theta[4])
H1=(la/v)*(exp(v*t1)-1);H2=(la/v)*(exp(v*t2)-1)
logli=sum( (cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)-
((1-R)/a)*(log(1+a*H1)+log(1+a*H2))-(R/a)*log(1+a*(H1+H2)))+
cen1*cen2*log((R+a)*R*(1+a*(H1+H2))^(-2)+(1-R)*R*(1+a*(H1+H2))^(-1)*((1+a*H1)^(-1)+(1+a*H2)^(-1))+
((1-R)^2)*(1+a*H1)^(-1)*(1+a*H2)^(-1))+
cen1*(1-cen2)*log(R*(1+a*(H1+H2))^(-1)+(1-R)*(1+a*H1)^(-1))+
(1-cen1)*cen2*log(R*(1+a*(H1+H2))^(-1)+(1-R)*(1+a*H2)^(-1)))
-logli}

.fdLlikgompertzcvnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);v = abs(theta[4])
H1=H2<-NULL
llkexprss <- expression((cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)-
((1-R)/a)*(log(1+a*(la/v)*(exp(v*t1)-1))+log(1+a*(la/v)*(exp(v*t2)-1)))-
(R/a)*log(1+a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1))))+
cen1*cen2*log((R+a)*R*(1+a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(-2)+
(1-R)*R*(1+a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(-1)*((1+a*(la/v)*(exp(v*t1)-1))^(-1)+(1+a*(la/v)*(exp(v*t2)-1))^(-1))+
((1-R)^2)*(1+a*(la/v)*(exp(v*t1)-1))^(-1)*(1+a*(la/v)*(exp(v*t2)-1))^(-1))+
cen1*(1-cen2)*log(R*(1+a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(-1)+(1-R)*(1+a*(la/v)*(exp(v*t1)-1))^(-1))+
(1-cen1)*cen2*log(R*(1+a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(-1)+(1-R)*(1+a*(la/v)*(exp(v*t2)-1))^(-1)))
dxy <- deriv(llkexprss, c("a", "R","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gd=c(colSums(sr1$gradient))
-gd
}
.sdLlikgompertzcvnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);v = abs(theta[4])
H1=H2<-NULL
llkexprss <- expression((cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)-
((1-R)/a)*(log(1+a*(la/v)*(exp(v*t1)-1))+log(1+a*(la/v)*(exp(v*t2)-1)))-
(R/a)*log(1+a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1))))+
cen1*cen2*log((R+a)*R*(1+a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(-2)+
(1-R)*R*(1+a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(-1)*((1+a*(la/v)*(exp(v*t1)-1))^(-1)+(1+a*(la/v)*(exp(v*t2)-1))^(-1))+
((1-R)^2)*(1+a*(la/v)*(exp(v*t1)-1))^(-1)*(1+a*(la/v)*(exp(v*t2)-1))^(-1))+
cen1*(1-cen2)*log(R*(1+a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(-1)+(1-R)*(1+a*(la/v)*(exp(v*t1)-1))^(-1))+
(1-cen1)*cen2*log(R*(1+a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(-1)+(1-R)*(1+a*(la/v)*(exp(v*t2)-1))^(-1)))
dxy <- deriv3(llkexprss, c("a", "R","la","v"),hessian = TRUE)
sr=eval(dxy)
sr1<-attributes(sr)
Hes=colSums(sr1$hessian)
-Hes
}

optimgomperzcvnc<-function(thet,xx){
xx=as.matrix(xx)
ui0=matrix(0,5,length(thet));ci0=rep(0,5);ci0[3]<-(-1)
ui0[1,1]<-1;ui0[2,2]<-1;ui0[3,2]<-(-1);ui0[4,3]<-(1);ui0[5,4]<-(1)
if(thet[1]<0.000001){thet[1]<-0.000001};if(thet[2]<0.000001){thet[2]<-0.000001}
if(thet[3]>=1){thet[3]<-(1-0.000001)};if(thet[3]<0.000001){thet[3]<-0.000001}
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikgompertzcvnc,grad=.fdLlikgompertzcvnc,
xx=xx,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("theta","Row","scale","shape")
SE=sqrt(diag(INVE))
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(frailparest=c(theta=newtht[1],Row=newtht[2]),
basehazpar=c(scale=newtht[3],shape=newtht[4]),stderr=SE,vcov=INVE,loglik=lik,
convergence=fittr$convergence,iterations=fittr$outer.iterations,AIC=aic,BIC=bic)
res
}
############Gamma frailty with Weibull baseline ########
#####################################################
#
#######################with covariates#######################
#
.Llikweibcv = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);v = abs(theta[4]);bet = theta[5:length(theta)]
EBX1=exp(c(X1%*%bet));EBX2=exp(c(X2%*%bet))
H01=la*t1^v;H02=la*t2^v
H1=H01*EBX1;H2=H02*EBX2
logli=sum( (cen1*(log(la)+log(v)+(v-1)*log(t1)+c(X1%*%bet))+cen2*(log(la)+log(v)+(v-1)*log(t2)+c(X2%*%bet))-
((1-R)/a)*(log(1+a*H1)+log(1+a*H2))-(R/a)*log(1+a*(H1+H2)))+
cen1*cen2*log((R+a)*R*(1+a*(H1+H2))^(-2)+(1-R)*R*(1+a*(H1+H2))^(-1)*((1+a*H1)^(-1)+(1+a*H2)^(-1))+
((1-R)^2)*(1+a*H1)^(-1)*(1+a*H2)^(-1))+
cen1*(1-cen2)*log(R*(1+a*(H1+H2))^(-1)+(1-R)*(1+a*H1)^(-1))+
(1-cen1)*cen2*log(R*(1+a*(H1+H2))^(-1)+(1-R)*(1+a*H2)^(-1)))
-logli}

.fdLlikweibcv = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);v = abs(theta[4]);bet = theta[5:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
llkexprss <- expression(((cen1*(log(la)+log(v)+(v-1)*log(t1)+(log(eb1)))+cen2*(log(la)+log(v)+(v-1)*log(t2)+(log(eb2)))-
((1-R)/a)*(log(1+a*la*t1^v*eb1)+log(1+a*la*t2^v*eb2))-(R/a)*log(1+a*(la*t1^v*eb1+la*t2^v*eb2)))+
cen1*cen2*log((R+a)*R*(1+a*(la*t1^v*eb1+la*t2^v*eb2))^(-2)+
(1-R)*R*(1+a*(la*t1^v*eb1+la*t2^v*eb2))^(-1)*((1+a*la*t1^v*eb1)^(-1)+(1+a*la*t2^v*eb2)^(-1))+
((1-R)^2)*(1+a*la*t1^v*eb1)^(-1)*(1+a*la*t2^v*eb2)^(-1))+
cen1*(1-cen2)*log(R*(1+a*(la*t1^v*eb1+la*t2^v*eb2))^(-1)+(1-R)*(1+a*la*t1^v*eb1)^(-1))+
(1-cen1)*cen2*log(R*(1+a*(la*t1^v*eb1+la*t2^v*eb2))^(-1)+(1-R)*(1+a*la*t2^v*eb2)^(-1))))
dxy <- deriv(llkexprss, c("a", "R","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
H1=la*t1^v*eb1;H2=la*t2^v*eb2
db<-derivdbcv(theta,cen1=cen1,cen2=cen2,X1=X1,X2=X2,H1=H1,H2=H2)
gd=c(c(gdpart1),c(db))
-gd
}

optimweibcv<-function(thet,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2)
ui0=matrix(0,5,length(thet));ci0=rep(0,5);ci0[3]<-(-1)
ui0[1,1]<-1;ui0[2,2]<-1;ui0[3,2]<-(-1);ui0[4,3]<-(1);ui0[5,4]<-(1)
if(thet[1]<0.000001){thet[1]<-0.000001};if(thet[2]<0.000001){thet[2]<-0.000001}
if(thet[3]>=1){thet[3]<-(1-0.000001)};if(thet[3]<0.000001){thet[3]<-0.000001}
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikweibcv,grad=.fdLlikweibcv,
xx=xx,X1=X1,X2=X2,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("theta","Row","scale","shape",colnames(X1))
SE=sqrt(diag(INVE))
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(coefficients=newtht[5:length(thet)],frailparest=c(theta=newtht[1],Row=newtht[2]),
basehazpar=c(scale=newtht[3],shape=newtht[4]),vcov=INVE,stderr=SE,loglik=lik,
convergence=fittr$convergence,iterations=fittr$outer.iterations,AIC=aic,BIC=bic)
res
}

#######################without covariates#######################
.Llikweibcvnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);v = abs(theta[4])
H1=la*t1^v;H2=la*t2^v
logli=sum( (cen1*(log(la)+log(v)+(v-1)*log(t1))+cen2*(log(la)+log(v)+(v-1)*log(t2))-
((1-R)/a)*(log(1+a*H1)+log(1+a*H2))-(R/a)*log(1+a*(H1+H2)))+
cen1*cen2*log((R+a)*R*(1+a*(H1+H2))^(-2)+(1-R)*R*(1+a*(H1+H2))^(-1)*((1+a*H1)^(-1)+(1+a*H2)^(-1))+
((1-R)^2)*(1+a*H1)^(-1)*(1+a*H2)^(-1))+
cen1*(1-cen2)*log(R*(1+a*(H1+H2))^(-1)+(1-R)*(1+a*H1)^(-1))+
(1-cen1)*cen2*log(R*(1+a*(H1+H2))^(-1)+(1-R)*(1+a*H2)^(-1)))
-logli}

.fdLlikweibcvnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);v = abs(theta[4])
llkexprss <- expression((cen1*(log(la)+log(v)+(v-1)*log(t1))+cen2*(log(la)+log(v)+(v-1)*log(t2))-
((1-R)/a)*(log(1+a*la*t1^v)+log(1+a*la*t2^v))-(R/a)*log(1+a*(la*t1^v+la*t2^v)))+
cen1*cen2*log((R+a)*R*(1+a*(la*t1^v+la*t2^v))^(-2)+
(1-R)*R*(1+a*(la*t1^v+la*t2^v))^(-1)*((1+a*la*t1^v)^(-1)+(1+a*la*t2^v)^(-1))+
((1-R)^2)*(1+a*la*t1^v)^(-1)*(1+a*la*t2^v)^(-1))+
cen1*(1-cen2)*log(R*(1+a*(la*t1^v+la*t2^v))^(-1)+(1-R)*(1+a*la*t1^v)^(-1))+
(1-cen1)*cen2*log(R*(1+a*(la*t1^v+la*t2^v))^(-1)+(1-R)*(1+a*la*t2^v)^(-1)))
dxy <- deriv(llkexprss, c("a", "R","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gd=c(colSums(sr1$gradient))
-gd
}

optimweibcvnc<-function(thet,xx){
xx=as.matrix(xx)
ui0=matrix(0,5,length(thet));ci0=rep(0,5);ci0[3]<-(-1)
ui0[1,1]<-1;ui0[2,2]<-1;ui0[3,2]<-(-1);ui0[4,3]<-(1);ui0[5,4]<-(1)
if(thet[1]<0.000001){thet[1]<-0.000001};if(thet[2]<0.000001){thet[2]<-0.000001}
if(thet[3]>=1){thet[3]<-(1-0.000001)};if(thet[3]<0.000001){thet[3]<-0.000001}
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikweibcvnc,grad=.fdLlikweibcvnc,
xx=xx,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("theta","Row","scale","shape")
SE=sqrt(diag(INVE))
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(frailparest=c(theta=newtht[1],Row=newtht[2]),
basehazpar=c(scale=newtht[3],shape=newtht[4]),stderr=SE,vcov=INVE,loglik=lik,
convergence=fittr$convergence,iterations=fittr$outer.iterations,AIC=aic,BIC=bic)
res
}

#
############Gamma frailty with exponential baseline ########
#####################################################
#
#######################with covariates#######################

.Llikexpcv = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);bet = theta[4:length(theta)]
EBX1=exp(c(X1%*%bet));EBX2=exp(c(X2%*%bet))
H01=la*t1;H02=la*t2
H1=H01*EBX1;H2=H02*EBX2
logli=sum( (cen1*(log(la)+c(X1%*%bet))+cen2*(log(la)+c(X2%*%bet))-
((1-R)/a)*(log(1+a*H1)+log(1+a*H2))-(R/a)*log(1+a*(H1+H2)))+
cen1*cen2*log((R+a)*R*(1+a*(H1+H2))^(-2)+(1-R)*R*(1+a*(H1+H2))^(-1)*((1+a*H1)^(-1)+(1+a*H2)^(-1))+
((1-R)^2)*(1+a*H1)^(-1)*(1+a*H2)^(-1))+
cen1*(1-cen2)*log(R*(1+a*(H1+H2))^(-1)+(1-R)*(1+a*H1)^(-1))+
(1-cen1)*cen2*log(R*(1+a*(H1+H2))^(-1)+(1-R)*(1+a*H2)^(-1)))
-logli}


.fdLlikexpcv = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);bet = theta[4:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
H1=H2<-v<-NULL
llkexprss <- expression((cen1*(log(la))+cen2*(log(la))-
((1-R)/a)*(log(1+a*la*t1*eb1)+log(1+a*la*t2*eb2))-(R/a)*log(1+a*(la*t1*eb1+la*t2*eb2)))+
cen1*cen2*log((R+a)*R*(1+a*(la*t1*eb1+la*t2*eb2))^(-2)+
(1-R)*R*(1+a*(la*t1*eb1+la*t2*eb2))^(-1)*((1+a*la*t1*eb1)^(-1)+(1+a*la*t2*eb2)^(-1))+
((1-R)^2)*(1+a*la*t1*eb1)^(-1)*(1+a*la*t2*eb2)^(-1))+
cen1*(1-cen2)*log(R*(1+a*(la*t1*eb1+la*t2*eb2))^(-1)+(1-R)*(1+a*la*t1*eb1)^(-1))+
(1-cen1)*cen2*log(R*(1+a*(la*t1*eb1+la*t2*eb2))^(-1)+(1-R)*(1+a*la*t2*eb2)^(-1)))
dxy <- deriv(llkexprss, c("a", "R","la"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
H01=la*t1;H02=la*t2
H1=H01*eb1;H2=H02*eb2
db<-derivdbcv(theta,cen1=cen1,cen2=cen2,X1=X1,X2=X2,H1=H1,H2=H2)
gd=c(c(gdpart1),c(db))
-gd
}

optimexpcv<-function(thet,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2)
ui0=matrix(0,4,length(thet));ci0=rep(0,4);ci0[3]<-(-1)
ui0[1,1]<-1;ui0[2,2]<-1;ui0[3,2]<-(-1);ui0[4,3]<-(1)
if(thet[1]<0.000001){thet[1]<-0.000001};if(thet[2]<0.000001){thet[2]<-0.000001}
if(thet[3]>=1){thet[3]<-(1-0.000001)};if(thet[3]<0.000001){thet[3]<-0.000001}
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikexpcv,grad=.fdLlikexpcv,
xx=xx,X1=X1,X2=X2,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("theta","Row","scale",colnames(X1))
SE=sqrt(diag(INVE))
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(coefficients=newtht[4:length(thet)],frailparest=c(theta=newtht[1],Row=newtht[2]),
basehazpar=c(scale=newtht[3]),stderr=SE,vcov=INVE,loglik=lik,AIC=aic,BIC=bic,
convergence=fittr$convergence,iterations=fittr$iterations)
res
}

#######################without covariates#######################
.Llikexpcvnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3])
H1=la*t1;H2=la*t2
logli=sum( (cen1*(log(la))+cen2*(log(la))-
((1-R)/a)*(log(1+a*H1)+log(1+a*H2))-(R/a)*log(1+a*(H1+H2)))+
cen1*cen2*log((R+a)*R*(1+a*(H1+H2))^(-2)+(1-R)*R*(1+a*(H1+H2))^(-1)*((1+a*H1)^(-1)+(1+a*H2)^(-1))+
((1-R)^2)*(1+a*H1)^(-1)*(1+a*H2)^(-1))+
cen1*(1-cen2)*log(R*(1+a*(H1+H2))^(-1)+(1-R)*(1+a*H1)^(-1))+
(1-cen1)*cen2*log(R*(1+a*(H1+H2))^(-1)+(1-R)*(1+a*H2)^(-1)))
-logli}

.fdLlikexpcvnc= function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3])
llkexprss <- expression((cen1*(log(la))+cen2*(log(la))-
((1-R)/a)*(log(1+a*la*t1)+log(1+a*la*t2))-(R/a)*log(1+a*(la*t1+la*t2)))+
cen1*cen2*log((R+a)*R*(1+a*(la*t1+la*t2))^(-2)+
(1-R)*R*(1+a*(la*t1+la*t2))^(-1)*((1+a*la*t1)^(-1)+(1+a*la*t2)^(-1))+
((1-R)^2)*(1+a*la*t1)^(-1)*(1+a*la*t2)^(-1))+
cen1*(1-cen2)*log(R*(1+a*(la*t1+la*t2))^(-1)+(1-R)*(1+a*la*t1)^(-1))+
(1-cen1)*cen2*log(R*(1+a*(la*t1+la*t2))^(-1)+(1-R)*(1+a*la*t2)^(-1)))
dxy <- deriv(llkexprss, c("a", "R","la"))
sr=eval(dxy)
sr1<-attributes(sr)
gd=c(colSums(sr1$gradient))
-gd
}

optimexpcvnc<-function(thet,xx){
xx=as.matrix(xx)
ui0=matrix(0,4,length(thet));ci0=rep(0,4);ci0[3]<-(-1)
ui0[1,1]<-1;ui0[2,2]<-1;ui0[3,2]<-(-1);ui0[4,3]<-(1)
if(thet[1]<0.000001){thet[1]<-0.000001};if(thet[2]<0.000001){thet[2]<-0.000001}
if(thet[3]>=0.999999999){thet[3]<-0.9999999};if(thet[3]<0.000001){thet[3]<-0.000001}
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikexpcvnc,grad=.fdLlikexpcvnc,
xx=xx,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("theta","Row","scale")
SE=sqrt(diag(INVE))
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(frailparest=c(theta=newtht[1],Row=newtht[2]),
basehazpar=c(scale=newtht[3]),stderr=SE,vcov=INVE,loglik=lik,AIC=aic,BIC=bic,
convergence=fittr$convergence,iterations=fittr$outer.iterations)
res
}
################################################

fitbccv.gammapar<-function(X,Y,initfrailp,inithazp,initbeta,haz){
time=Y[, 1];censor=Y[, 2]
data.n1 <- length(time);data.n <-data.n1/2#### data.n is the number of pairs
ar=array(1:data.n)
i1<-2*ar-1;i2<-2*ar
cen1=censor[i1];cen2=censor[i2]
t1=time[i1];t2=time[i2]
xx=matrix(c(t1,t2,cen1,cen2),length(cen1),4)
thet=1
n_covar=ncol(X)
optimfunc<-NULL
if(n_covar>0){
X1=X[i1,];X2=X[i2,]
if(n_covar==1){
X2=matrix(c(X2),data.n,1);X1=matrix(c(X1),data.n,1)}
X1=as.matrix(X1);X2=as.matrix(X2)
colnames(X1)<-colnames(X2)<-colnames(X)
if(length(initbeta)>0){initbeta=initbeta}else{
cph0 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset =NULL, init = NULL, control = survival::coxph.control(),
weights = NULL,method = "breslow", resid=FALSE)
initbeta<-cph0$coefficients}
if(length(initfrailp)>0){initfrailp=initfrailp}else{initfrailp<-c(0.5,0.5)}
if (haz== c("weibull")) {optimfunc<-optimweibcv
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05,0.05)}
thet=c(initfrailp,inithazp,initbeta)}
if (haz== c("gompertz")) {optimfunc<-optimgomperzcv
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05,0.05)}
thet=c(initfrailp,inithazp,initbeta)}
if (haz== c("exponential")) {optimfunc<-optimexpcv
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05)}
thet=c(initfrailp,inithazp,initbeta)}
res<-optimfunc(thet,xx,X1,X2)}
if(n_covar==0){
if(length(initfrailp)>0){initfrailp=initfrailp}else{initfrailp<-c(0.5,0.5)}
if (haz== c("weibull")) {optimfunc<-optimweibcvnc
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05,0.05)}
thet=c(initfrailp,inithazp)}
if (haz== c("gompertz")) {optimfunc<-optimgomperzcvnc
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05,0.05)}
thet=c(initfrailp,inithazp)}
if (haz== c("exponential")) {optimfunc<-optimexpcvnc
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05)}
thet=c(initfrailp,inithazp)}
res<-optimfunc(thet,xx)
}
res$time <- time
res$censor <- censor
res$call <- match.call()
class(res) <- c("bcfrailpar")
res
}
#
############Inverse gaussian frailty with gompertz baseline ########
#####################################################
#######################with covariates#######################
#######################
.Llikgompinvg = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2)
t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3])
v = abs(theta[4]);bet = theta[5:length(theta)]
EBX1=exp(c(X1%*%bet));EBX2=exp(c(X2%*%bet))
H01=(la/v)*(exp(v*t1)-1);H02=(la/v)*(exp(v*t2)-1);H1=H01*EBX1;H2=H02*EBX2
logli=sum( ((1-R)/a)*(1-(1+2*a*H1)^(1/2))+((1-R)/a)*(1-(1+2*a*H2)^(1/2))+
(R/a)*(1-(1+2*a*(H1+H2))^(1/2))+
cen1*(log(la)+v*t1+c(X1%*%bet))+cen2*(log(la)+v*t2+c(X2%*%bet))+
cen1*cen2*log( (R^2)*(1+2*a*(H1+H2))^(-1)+(R*a)*(1+2*a*(H1+H2))^(-3/2)+
(1-R)*R*(1+2*a*(H1+H2))^(-1/2)*((1+2*a*H1)^(-1/2)+(1+2*a*H2)^(-1/2))+
((1-R)^2)*(1+2*a*H1)^(-1/2)*(1+2*a*H2)^(-1/2))+
cen1*(1-cen2)*log( R*(1+2*a*(H1+H2))^(-1/2)+(1-R)*(1+2*a*H1)^(-1/2))+
(1-cen1)*cen2*log( R*(1+2*a*(H1+H2))^(-1/2)+(1-R)*(1+2*a*H2)^(-1/2)))
-logli}

.fdLlikgompinvg = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2)
t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3])
v = abs(theta[4]);bet = theta[5:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
H1=H2<-NULL
llkexprss <- expression( ((1-R)/a)*(1-(1+2*a*(la/v)*(exp(v*t1)-1)*eb1)^(1/2))+((1-R)/a)*(1-(1+2*a*(la/v)*(exp(v*t2)-1)*eb2)^(1/2))+
(R/a)*(1-(1+2*a*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(1/2))+
cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)+
cen1*cen2*log( (R^2)*(1+2*a*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(-1)+
(R*a)*(1+2*a*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(-3/2)+
(1-R)*R*(1+2*a*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(-1/2)*((1+2*a*(la/v)*(exp(v*t1)-1)*eb1)^(-1/2)+
(1+2*a*(la/v)*(exp(v*t2)-1)*eb2)^(-1/2))+
((1-R)^2)*(1+2*a*(la/v)*(exp(v*t1)-1)*eb1)^(-1/2)*(1+2*a*(la/v)*(exp(v*t2)-1)*eb2)^(-1/2))+
cen1*(1-cen2)*log( R*(1+2*a*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(-1/2)+(1-R)*(1+2*a*(la/v)*(exp(v*t1)-1)*eb1)^(-1/2))+
(1-cen1)*cen2*log( R*(1+2*a*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(-1/2)+(1-R)*(1+2*a*(la/v)*(exp(v*t2)-1)*eb2)^(-1/2)))
dxy <- deriv(llkexprss, c("a", "R","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
H01=(la/v)*(exp(v*t1)-1);H02=(la/v)*(exp(v*t2)-1)
H1=H01*eb1;H2=H02*eb2
db<-derivdbcp(a=a,R=R,r=0.5,cen1=cen1,cen2=cen2,X1=X1,X2=X2,H1=H1,H2=H2)
db=as.vector(db)
gd=c(c(gdpart1),c(db))
-gd
}

optimgompinvg<-function(thet,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2)
thet0=thet[1:4]
thet0[thet0<0.000001]<-0.000001
if(thet0[2]>=1){thet0[2]<-(1-0.000001)}
thet[1:4]<-thet0
ui0=matrix(0,5,length(thet));ci0=rep(0,5);ci0[3]<-(-1)
ui0[1,1]<-1;ui0[2,2]<-1;ui0[3,2]<-(-1);ui0[4,3]<-(1);ui0[5,4]<-(1)
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikgompinvg,grad=.fdLlikgompinvg,
xx=xx,X1=X1,X2=X2,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("theta","Row","scale","shape",colnames(X1))
SE=sqrt(diag(INVE))
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(coefficients=newtht[5:length(thet)],frailparest=c(theta=newtht[1],Row=newtht[2]),
basehazpar=c(scale=newtht[3],shape=newtht[4]),vcov=INVE,stderr=SE,loglik=lik,
convergence=fittr$convergence,iterations=fittr$outer.iterations,AIC=aic,BIC=bic)
res
}


#######################without covariates#######################

.Llikgompinvgnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);v = abs(theta[4])
H1=(la/v)*(exp(v*t1)-1);H2=(la/v)*(exp(v*t2)-1)
logli=sum( ((1-R)/a)*(1-(1+2*a*H1)^(1/2))+((1-R)/a)*(1-(1+2*a*H2)^(1/2))+
(R/a)*(1-(1+2*a*(H1+H2))^(1/2))+
cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)+
cen1*cen2*log( (R^2)*(1+2*a*(H1+H2))^(-1)+(R*a)*(1+2*a*(H1+H2))^(-3/2)+
(1-R)*R*(1+2*a*(H1+H2))^(-1/2)*((1+2*a*H1)^(-1/2)+(1+2*a*H2)^(-1/2))+
((1-R)^2)*(1+2*a*H1)^(-1/2)*(1+2*a*H2)^(-1/2))+
cen1*(1-cen2)*log( R*(1+2*a*(H1+H2))^(-1/2)+(1-R)*(1+2*a*H1)^(-1/2))+
(1-cen1)*cen2*log( R*(1+2*a*(H1+H2))^(-1/2)+(1-R)*(1+2*a*H2)^(-1/2)))
-logli}

.fdLlikgompinvgnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);v = abs(theta[4])
H1=H2<-NULL
llkexprss <- expression( ((1-R)/a)*(1-(1+2*a*(la/v)*(exp(v*t1)-1))^(1/2))+((1-R)/a)*(1-(1+2*a*(la/v)*(exp(v*t2)-1))^(1/2))+
(R/a)*(1-(1+2*a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(1/2))+
cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)+
cen1*cen2*log((R^2)*(1+2*a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(-1)+
(R*a)*(1+2*a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(-3/2)+
(1-R)*R*(1+2*a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(-1/2)*((1+2*a*(la/v)*(exp(v*t1)-1))^(-1/2)+
(1+2*a*(la/v)*(exp(v*t2)-1))^(-1/2))+
((1-R)^2)*(1+2*a*(la/v)*(exp(v*t1)-1))^(-1/2)*(1+2*a*(la/v)*(exp(v*t2)-1))^(-1/2))+
cen1*(1-cen2)*log( R*(1+2*a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(-1/2)+(1-R)*(1+2*a*(la/v)*(exp(v*t1)-1))^(-1/2))+
(1-cen1)*cen2*log( R*(1+2*a*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(-1/2)+(1-R)*(1+2*a*(la/v)*(exp(v*t2)-1))^(-1/2)))
dxy <- deriv(llkexprss, c("a", "R","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
gd=c(c(gdpart1))
-gd
}

optimgompinvgnc<-function(thet,xx){
xx=as.matrix(xx)
thet[thet<0.000001]<-0.000001
if(thet[2]>=1){thet[2]<-(1-0.000001)}
ui0=matrix(0,5,length(thet));ci0=rep(0,5);ci0[3]<-(-1)
ui0[1,1]<-1;ui0[2,2]<-1;ui0[3,2]<-(-1);ui0[4,3]<-(1);ui0[5,4]<-(1)
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikgompinvgnc,grad=.fdLlikgompinvgnc,
xx=xx,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("theta","Row","scale","shape")
SE=sqrt(diag(INVE))
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(frailparest=c(theta=newtht[1],Row=newtht[2]),
basehazpar=c(scale=newtht[3],shape=newtht[4]),vcov=INVE,stderr=SE,loglik=lik,
convergence=fittr$convergence,iterations=fittr$outer.iterations,AIC=aic,BIC=bic)
res
}


#
############Inverse gaussian frailty with weibull baseline ########
#####################################################
#
#######################with covariates#######################
#

.Llikweibinvg = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3])
v = abs(theta[4]);bet = theta[5:length(theta)]
EBX1=exp(c(X1%*%bet));EBX2=exp(c(X2%*%bet))
H01=la*t1^v;H02=la*t2^v;H1=H01*EBX1;H2=H02*EBX2
logli=sum( ((1-R)/a)*(1-(1+2*a*H1)^(1/2))+((1-R)/a)*(1-(1+2*a*H2)^(1/2))+
(R/a)*(1-(1+2*a*(H1+H2))^(1/2))+
cen1*(log(la)+log(v)+(v-1)*log(t1)+c(X1%*%bet))+cen2*(log(la)+log(v)+(v-1)*log(t2)+c(X2%*%bet))+
cen1*cen2*log( (R^2)*(1+2*a*(H1+H2))^(-1)+(R*a)*(1+2*a*(H1+H2))^(-3/2)+
(1-R)*R*(1+2*a*(H1+H2))^(-1/2)*((1+2*a*H1)^(-1/2)+(1+2*a*H2)^(-1/2))+
((1-R)^2)*(1+2*a*H1)^(-1/2)*(1+2*a*H2)^(-1/2))+
cen1*(1-cen2)*log( R*(1+2*a*(H1+H2))^(-1/2)+(1-R)*(1+2*a*H1)^(-1/2))+
(1-cen1)*cen2*log( R*(1+2*a*(H1+H2))^(-1/2)+(1-R)*(1+2*a*H2)^(-1/2)))
-logli}

.fdLlikweibinvg = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3])
v = abs(theta[4]);bet = theta[5:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
H1=H2<-NULL
llkexprss <- expression(((1-R)/a)*(1-(1+2*a*la*t1^v*eb1)^(1/2))+((1-R)/a)*(1-(1+2*a*la*t2^v*eb2)^(1/2))+
(R/a)*(1-(1+2*a*(la*t1^v*eb1+la*t2^v*eb2))^(1/2))+
cen1*(log(la)+log(v)+(v-1)*log(t1))+cen2*(log(la)+log(v)+(v-1)*log(t2))+
cen1*cen2*log( (R^2)*(1+2*a*(la*t1^v*eb1+la*t2^v*eb2))^(-1)+(R*a)*(1+2*a*(la*t1^v*eb1+la*t2^v*eb2))^(-3/2)+
(1-R)*R*(1+2*a*(la*t1^v*eb1+la*t2^v*eb2))^(-1/2)*((1+2*a*la*t1^v*eb1)^(-1/2)+(1+2*a*la*t2^v*eb2)^(-1/2))+
((1-R)^2)*(1+2*a*la*t1^v*eb1)^(-1/2)*(1+2*a*la*t2^v*eb2)^(-1/2))+
cen1*(1-cen2)*log( R*(1+2*a*(la*t1^v*eb1+la*t2^v*eb2))^(-1/2)+(1-R)*(1+2*a*la*t1^v*eb1)^(-1/2))+
(1-cen1)*cen2*log( R*(1+2*a*(la*t1^v*eb1+la*t2^v*eb2))^(-1/2)+(1-R)*(1+2*a*la*t2^v*eb2)^(-1/2)))
dxy <- deriv(llkexprss, c("a", "R","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
H01=la*t1^v;H02=la*t2^v
H1=H01*eb1;H2=H02*eb2
db<-derivdbcp(a=a,R=R,r=0.5,cen1=cen1,cen2=cen2,X1=X1,X2=X2,H1=H1,H2=H2)
db=as.vector(db)
gd=c(c(gdpart1),c(db))
-gd
}

optimweibinvg<-function(thet,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2)
thet0=thet[1:4]
thet0[thet0<0.000001]<-0.000001
if(thet0[2]>=1){thet0[2]<-(1-0.000001)}
thet[1:4]<-thet0
ui0=matrix(0,5,length(thet));ci0=rep(0,5);ci0[3]<-(-1)
ui0[1,1]<-1;ui0[2,2]<-1;ui0[3,2]<-(-1);ui0[4,3]<-(1);ui0[5,4]<-(1)
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikweibinvg,grad=.fdLlikweibinvg,
xx=xx,X1=X1,X2=X2,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("theta","Row","scale","shape",colnames(X1))
SE=sqrt(diag(INVE))
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(coefficients=newtht[5:length(thet)],frailparest=c(theta=newtht[1],Row=newtht[2]),
basehazpar=c(scale=newtht[3],shape=newtht[4]),vcov=INVE,stderr=SE,loglik=lik,
convergence=fittr$convergence,iterations=fittr$outer.iterations,AIC=aic,BIC=bic)
res
}


#######################without covariates#######################

.Llikweibinvgnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3])
v = abs(theta[4])
H1=la*t1^v;H2=la*t2^v
logli=sum( ((1-R)/a)*(1-(1+2*a*H1)^(1/2))+((1-R)/a)*(1-(1+2*a*H2)^(1/2))+
(R/a)*(1-(1+2*a*(H1+H2))^(1/2))+
cen1*(log(la)+log(v)+(v-1)*log(t1))+cen2*(log(la)+log(v)+(v-1)*log(t2))+
cen1*cen2*log( (R^2)*(1+2*a*(H1+H2))^(-1)+(R*a)*(1+2*a*(H1+H2))^(-3/2)+
(1-R)*R*(1+2*a*(H1+H2))^(-1/2)*((1+2*a*H1)^(-1/2)+(1+2*a*H2)^(-1/2))+
((1-R)^2)*(1+2*a*H1)^(-1/2)*(1+2*a*H2)^(-1/2))+
cen1*(1-cen2)*log( R*(1+2*a*(H1+H2))^(-1/2)+(1-R)*(1+2*a*H1)^(-1/2))+
(1-cen1)*cen2*log( R*(1+2*a*(H1+H2))^(-1/2)+(1-R)*(1+2*a*H2)^(-1/2)))
-logli}

.fdLlikweibinvgnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3])
v = abs(theta[4])
H1=H2<-NULL
llkexprss <- expression(((1-R)/a)*(1-(1+2*a*la*t1^v)^(1/2))+((1-R)/a)*(1-(1+2*a*la*t2^v)^(1/2))+
(R/a)*(1-(1+2*a*(la*t1^v+la*t2^v))^(1/2))+
cen1*(log(la)+log(v)+(v-1)*log(t1))+cen2*(log(la)+log(v)+(v-1)*log(t2))+
cen1*cen2*log( (R^2)*(1+2*a*(la*t1^v+la*t2^v))^(-1)+(R*a)*(1+2*a*(la*t1^v+la*t2^v))^(-3/2)+
(1-R)*R*(1+2*a*(la*t1^v+la*t2^v))^(-1/2)*((1+2*a*la*t1^v)^(-1/2)+(1+2*a*la*t2^v)^(-1/2))+
((1-R)^2)*(1+2*a*la*t1^v)^(-1/2)*(1+2*a*la*t2^v)^(-1/2))+
cen1*(1-cen2)*log( R*(1+2*a*(la*t1^v+la*t2^v))^(-1/2)+(1-R)*(1+2*a*la*t1^v)^(-1/2))+
(1-cen1)*cen2*log( R*(1+2*a*(la*t1^v+la*t2^v))^(-1/2)+(1-R)*(1+2*a*la*t2^v)^(-1/2)))
dxy <- deriv(llkexprss, c("a", "R","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
gd=c(c(gdpart1))
-gd
}

optimweibinvgnc<-function(thet,xx){
xx=as.matrix(xx)
thet[thet<0.000001]<-0.000001
if(thet[2]>=1){thet[2]<-(1-0.000001)}
ui0=matrix(0,5,length(thet));ci0=rep(0,5);ci0[3]<-(-1)
ui0[1,1]<-1;ui0[2,2]<-1;ui0[3,2]<-(-1);ui0[4,3]<-(1);ui0[5,4]<-(1)
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikweibinvgnc,grad=.fdLlikweibinvgnc,
xx=xx,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("theta","Row","scale","shape")
SE=sqrt(diag(INVE))
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(frailparest=c(theta=newtht[1],Row=newtht[2]),
basehazpar=c(scale=newtht[3],shape=newtht[4]),vcov=INVE,stderr=SE,loglik=lik,
convergence=fittr$convergence,iterations=fittr$outer.iterations,AIC=aic,BIC=bic)
res
}


#
############Inverse gaussian frailty with exponential baseline ########
#####################################################
#
#######################with covariates#######################
#

.Llikexpinvg = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);bet = theta[4:length(theta)]
EBX1=exp(c(X1%*%bet));EBX2=exp(c(X2%*%bet))
H01=la*t1;H02=la*t2;H1=H01*EBX1;H2=H02*EBX2
logli=sum( ((1-R)/a)*(1-(1+2*a*H1)^(1/2))+((1-R)/a)*(1-(1+2*a*H2)^(1/2))+
(R/a)*(1-(1+2*a*(H1+H2))^(1/2))+
cen1*(log(la)+c(X1%*%bet))+cen2*(log(la)+c(X2%*%bet))+
cen1*cen2*log( (R^2)*(1+2*a*(H1+H2))^(-1)+(R*a)*(1+2*a*(H1+H2))^(-3/2)+
(1-R)*R*(1+2*a*(H1+H2))^(-1/2)*((1+2*a*H1)^(-1/2)+(1+2*a*H2)^(-1/2))+
((1-R)^2)*(1+2*a*H1)^(-1/2)*(1+2*a*H2)^(-1/2))+
cen1*(1-cen2)*log( R*(1+2*a*(H1+H2))^(-1/2)+(1-R)*(1+2*a*H1)^(-1/2))+
(1-cen1)*cen2*log( R*(1+2*a*(H1+H2))^(-1/2)+(1-R)*(1+2*a*H2)^(-1/2)))
-logli}

.fdLlikexpinvg = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3]);bet = theta[4:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
H1=H2<-NULL
llkexprss <- expression(((1-R)/a)*(1-(1+2*a*la*t1*eb1)^(1/2))+((1-R)/a)*(1-(1+2*a*la*t2*eb2)^(1/2))+
(R/a)*(1-(1+2*a*(la*t1*eb1+la*t2*eb2))^(1/2))+
cen1*(log(la))+cen2*(log(la))+
cen1*cen2*log( (R^2)*(1+2*a*(la*t1*eb1+la*t2*eb2))^(-1)+(R*a)*(1+2*a*(la*t1*eb1+la*t2*eb2))^(-3/2)+
(1-R)*R*(1+2*a*(la*t1*eb1+la*t2*eb2))^(-1/2)*((1+2*a*la*t1*eb1)^(-1/2)+(1+2*a*la*t2*eb2)^(-1/2))+
((1-R)^2)*(1+2*a*la*t1*eb1)^(-1/2)*(1+2*a*la*t2*eb2)^(-1/2))+
cen1*(1-cen2)*log( R*(1+2*a*(la*t1*eb1+la*t2*eb2))^(-1/2)+(1-R)*(1+2*a*la*t1*eb1)^(-1/2))+
(1-cen1)*cen2*log( R*(1+2*a*(la*t1*eb1+la*t2*eb2))^(-1/2)+(1-R)*(1+2*a*la*t2*eb2)^(-1/2)))
dxy <- deriv(llkexprss, c("a","R","la"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
H01=la*t1;H02=la*t2
H1=H01*eb1;H2=H02*eb2
db<-derivdbcp(a=a,R=R,r=0.5,cen1=cen1,cen2=cen2,X1=X1,X2=X2,H1=H1,H2=H2)
db=as.vector(db)
gd=c(c(gdpart1),c(db))
-gd
}

optimexpinvg<-function(thet,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2)
thet0=thet[1:3]
thet0[thet0<0.000001]<-0.000001
if(thet0[2]>=1){thet0[2]<-(1-0.000001)}
thet[1:3]<-thet0
ui0=matrix(0,4,length(thet));ci0=rep(0,4);ci0[3]<-(-1)
ui0[1,1]<-1;ui0[2,2]<-1;ui0[3,2]<-(-1);ui0[4,3]<-(1)
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikexpinvg,grad=.fdLlikexpinvg,
xx=xx,X1=X1,X2=X2,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("theta","Row","scale",colnames(X1))
SE=sqrt(diag(INVE))
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(coefficients=newtht[4:length(thet)],frailparest=c(theta=newtht[1],Row=newtht[2]),
basehazpar=c(scale=newtht[3]),vcov=INVE,stderr=SE,loglik=lik,AIC=aic,BIC=bic,
convergence=fittr$convergence,iterations=fittr$outer.iterations)
res
}

#######################without covariates#######################

.Llikexpinvgnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3])
H1=la*t1;H2=la*t2
logli=sum( ((1-R)/a)*(1-(1+2*a*H1)^(1/2))+((1-R)/a)*(1-(1+2*a*H2)^(1/2))+
(R/a)*(1-(1+2*a*(H1+H2))^(1/2))+
cen1*(log(la))+cen2*(log(la))+
cen1*cen2*log( (R^2)*(1+2*a*(H1+H2))^(-1)+(R*a)*(1+2*a*(H1+H2))^(-3/2)+
(1-R)*R*(1+2*a*(H1+H2))^(-1/2)*((1+2*a*H1)^(-1/2)+(1+2*a*H2)^(-1/2))+
((1-R)^2)*(1+2*a*H1)^(-1/2)*(1+2*a*H2)^(-1/2))+
cen1*(1-cen2)*log( R*(1+2*a*(H1+H2))^(-1/2)+(1-R)*(1+2*a*H1)^(-1/2))+
(1-cen1)*cen2*log( R*(1+2*a*(H1+H2))^(-1/2)+(1-R)*(1+2*a*H2)^(-1/2)))
-logli}

.fdLlikexpinvgnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a = abs(theta[1]);R = abs(theta[2]);la = abs(theta[3])
H1=H2<-NULL
llkexprss <- expression(((1-R)/a)*(1-(1+2*a*la*t1)^(1/2))+((1-R)/a)*(1-(1+2*a*la*t2)^(1/2))+
(R/a)*(1-(1+2*a*(la*t1+la*t2))^(1/2))+cen1*log(la)+cen2*log(la)+
cen1*cen2*log( (R^2)*(1+2*a*(la*t1+la*t2))^(-1)+(R*a)*(1+2*a*(la*t1+la*t2))^(-3/2)+
(1-R)*R*(1+2*a*(la*t1+la*t2))^(-1/2)*((1+2*a*la*t1)^(-1/2)+(1+2*a*la*t2)^(-1/2))+
((1-R)^2)*(1+2*a*la*t1)^(-1/2)*(1+2*a*la*t2)^(-1/2))+
cen1*(1-cen2)*log( R*(1+2*a*(la*t1+la*t2))^(-1/2)+(1-R)*(1+2*a*la*t1)^(-1/2))+
(1-cen1)*cen2*log( R*(1+2*a*(la*t1+la*t2))^(-1/2)+(1-R)*(1+2*a*la*t2)^(-1/2)))
dxy <- deriv(llkexprss, c("a", "R","la"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
gd=c(c(gdpart1))
-gd
}

optimexpinvgnc<-function(thet,xx){
xx=as.matrix(xx)
thet[thet<0.000001]<-0.000001
if(thet[2]>=1){thet[2]<-(1-0.000001)}
ui0=matrix(0,4,length(thet));ci0=rep(0,4);ci0[3]<-(-1)
ui0[1,1]<-1;ui0[2,2]<-1;ui0[3,2]<-(-1);ui0[4,3]<-(1)
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikexpinvgnc,grad=.fdLlikexpinvgnc,
xx=xx,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("theta","Row","scale")
SE=sqrt(diag(INVE))
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(frailparest=c(theta=newtht[1],Row=newtht[2]),
basehazpar=c(scale=newtht[3]),vcov=INVE,stderr=SE,loglik=lik,AIC=aic,BIC=bic,
convergence=fittr$convergence,iterations=fittr$outer.iterations)
res
}


fitbccv.invgpar<-function(X,Y,initfrailp,inithazp,initbeta,haz){
time=Y[, 1];censor=Y[, 2]
data.n1 <- length(time);data.n <-data.n1/2#### data.n is the number of pairs
ar=array(1:data.n)
i1<-2*ar-1;i2<-2*ar
cen1=censor[i1];cen2=censor[i2]
t1=time[i1];t2=time[i2]
xx=matrix(c(t1,t2,cen1,cen2),length(cen1),4)
thet=1
n_covar=ncol(X)
optimfunc<-NULL
if(n_covar>0){
X1=X[i1,];X2=X[i2,]
if(n_covar==1){
X2=matrix(c(X2),data.n,1);X1=matrix(c(X1),data.n,1)}
X1=as.matrix(X1);X2=as.matrix(X2)
colnames(X1)<-colnames(X2)<-colnames(X)
if(length(initbeta)>0){initbeta=initbeta}else{
cph0 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset =NULL, init = NULL, control = survival::coxph.control(),
weights = NULL,method = "breslow", resid=FALSE)
initbeta<-cph0$coefficients}
if(length(initfrailp)>0){initfrailp=initfrailp}else{initfrailp<-c(0.5,0.5)}
if (haz== c("weibull")) {optimfunc<-optimweibinvg
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05,0.05)}
thet=c(initfrailp,inithazp,initbeta)}
if (haz== c("gompertz")) {optimfunc<-optimgompinvg
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05,0.05)}
thet=c(initfrailp,inithazp,initbeta)}
if (haz== c("exponential")) {optimfunc<-optimexpinvg
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05)}
thet=c(initfrailp,inithazp,initbeta)}
res<-optimfunc(thet,xx,X1,X2)}
if(n_covar==0){
if(length(initfrailp)>0){initfrailp=initfrailp}else{initfrailp<-c(0.5,0.5)}
if (haz== c("weibull")) {optimfunc<-optimweibinvgnc
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05,0.05)}
thet=c(initfrailp,inithazp)}
if (haz== c("gompertz")) {optimfunc<-optimgompinvgnc
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05,0.05)}
thet=c(initfrailp,inithazp)}
if (haz== c("exponential")) {optimfunc<-optimexpinvgnc
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05)}
thet=c(initfrailp,inithazp)}
res<-optimfunc(thet,xx)
}
res$time <- time
res$censor <- censor
res$call <- match.call()
class(res) <- c("bcfrailpar")
res
}


#
############Power variance frailty with gompertz baseline ########
#####################################################
#
#######################with covariates#######################
#
#######################
.Llikgompcp = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
r = abs(theta[1]);a = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4])
v = abs(theta[5]);bet = theta[6:length(theta)]
EBX1=exp(c(X1%*%bet));EBX2=exp(c(X2%*%bet))
H01=(la/v)*(exp(v*t1)-1);H02=(la/v)*(exp(v*t2)-1);H1=H01*EBX1;H2=H02*EBX2
logli=sum( ((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*H1)^r)+((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*H2)^r)+
((1-r)*R/(r*a))*(1-(1+(a/(1-r))*(H1+H2))^r)+
cen1*(log(la)+v*t1+c(X1%*%bet))+cen2*(log(la)+v*t2+c(X2%*%bet))+
cen1*cen2*log( (R^2)*(1+(a/(1-r))*(H1+H2))^(2*r-2)+(R*a)*(1+(a/(1-r))*(H1+H2))^(r-2)+
(1-R)*R*(1+(a/(1-r))*(H1+H2))^(r-1)*((1+(a/(1-r))*H1)^(r-1)+(1+(a/(1-r))*H2)^(r-1))+
((1-R)^2)*(1+(a/(1-r))*H1)^(r-1)*(1+(a/(1-r))*H2)^(r-1))+
cen1*(1-cen2)*log( R*(1+(a/(1-r))*(H1+H2))^(r-1)+(1-R)*(1+(a/(1-r))*H1)^(r-1))+
(1-cen1)*cen2*log( R*(1+(a/(1-r))*(H1+H2))^(r-1)+(1-R)*(1+(a/(1-r))*H2)^(r-1)))
-logli}

.fdLlikgompcp= function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
r = abs(theta[1]);a = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4])
v = abs(theta[5]);bet = theta[6:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
H1=H2<-NULL
llkexprss <- expression(((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*(la/v)*(exp(v*t1)-1)*eb1)^r)+
((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*(la/v)*(exp(v*t2)-1)*eb2)^r)+
((1-r)*R/(r*a))*(1-(1+(a/(1-r))*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^r)+
cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)+
cen1*cen2*log( (R^2)*(1+(a/(1-r))*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(2*r-2)+
(R*a)*(1+(a/(1-r))*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(r-2)+
(1-R)*R*(1+(a/(1-r))*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(r-1)*((1+(a/(1-r))*(la/v)*(exp(v*t1)-1)*eb1)^(r-1)+
(1+(a/(1-r))*(la/v)*(exp(v*t2)-1)*eb2)^(r-1))+
((1-R)^2)*(1+(a/(1-r))*(la/v)*(exp(v*t1)-1)*eb1)^(r-1)*(1+(a/(1-r))*(la/v)*(exp(v*t2)-1)*eb2)^(r-1))+
cen1*(1-cen2)*log( R*(1+(a/(1-r))*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(r-1)+(1-R)*(1+(a/(1-r))*(la/v)*(exp(v*t1)-1)*eb1)^(r-1))+
(1-cen1)*cen2*log( R*(1+(a/(1-r))*((la/v)*(exp(v*t1)-1)*eb1+(la/v)*(exp(v*t2)-1)*eb2))^(r-1)+(1-R)*(1+(a/(1-r))*(la/v)*(exp(v*t2)-1)*eb2)^(r-1)))
dxy <- deriv(llkexprss, c("r","a", "R","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
H01=(la/v)*(exp(v*t1)-1);H02=(la/v)*(exp(v*t2)-1)
H1=H01*eb1;H2=H02*eb2
db<-derivdbcp(a=a,R=R,r=r,cen1=cen1,cen2=cen2,X1=X1,X2=X2,H1=H1,H2=H2)
db=as.vector(db)
gd=c(c(gdpart1),c(db))
-gd
}

optimgomppv<-function(thet,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2)
thet[thet<0.000001]<-0.000001
if(thet[1]>=1){thet[1]<-(1-0.000001)};if(thet[3]>=1){thet[3]<-(1-0.000001)}
ui0=matrix(0,7,length(thet));ci0=rep(0,7);ci0[2]<-(-1);ci0[5]<-(-1)
ui0[1,1]<-1;ui0[2,1]<-(-1);ui0[3,2]<-1
ui0[4,3]<-1;ui0[5,3]<-(-1);ui0[6,4]<-(1);ui0[7,5]<-(1)
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikgompcp,grad=.fdLlikgompcp,
xx=xx,X1=X1,X2=X2,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("alpha","theta","Row","scale","shape",colnames(X1))
SE=sqrt(diag(INVE));sealp=SE[1];SE=SE[-1]
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(coefficients=newtht[6:length(thet)],frailparest=c(theta=newtht[2],Row=newtht[3]),
basehazpar=c(scale=newtht[4],shape=newtht[5]),alpha=newtht[1],sealpha=sealp,vcov=INVE,stderr=SE,loglik=lik,
convergence=fittr$convergence,iterations=fittr$outer.iterations,AIC=aic,BIC=bic)
res
}


#######################without covariates#######################
#

.Llikgompcpnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
r = abs(theta[1]);a = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4])
v = abs(theta[5])
H1=(la/v)*(exp(v*t1)-1);H2=(la/v)*(exp(v*t2)-1)
logli=sum( ((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*H1)^r)+((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*H2)^r)+
((1-r)*R/(r*a))*(1-(1+(a/(1-r))*(H1+H2))^r)+
cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)+
cen1*cen2*log( (R^2)*(1+(a/(1-r))*(H1+H2))^(2*r-2)+(R*a)*(1+(a/(1-r))*(H1+H2))^(r-2)+
(1-R)*R*(1+(a/(1-r))*(H1+H2))^(r-1)*((1+(a/(1-r))*H1)^(r-1)+(1+(a/(1-r))*H2)^(r-1))+
((1-R)^2)*(1+(a/(1-r))*H1)^(r-1)*(1+(a/(1-r))*H2)^(r-1))+
cen1*(1-cen2)*log( R*(1+(a/(1-r))*(H1+H2))^(r-1)+(1-R)*(1+(a/(1-r))*H1)^(r-1))+
(1-cen1)*cen2*log( R*(1+(a/(1-r))*(H1+H2))^(r-1)+(1-R)*(1+(a/(1-r))*H2)^(r-1)))
-logli}

.fdLlikgompcpnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
r = abs(theta[1]);a = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4]);v = abs(theta[5])
H1=H2<-NULL
llkexprss <- expression(((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*(la/v)*(exp(v*t1)-1))^r)+
((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*(la/v)*(exp(v*t2)-1))^r)+
((1-r)*R/(r*a))*(1-(1+(a/(1-r))*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^r)+
cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)+
cen1*cen2*log( (R^2)*(1+(a/(1-r))*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(2*r-2)+
(R*a)*(1+(a/(1-r))*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(r-2)+
(1-R)*R*(1+(a/(1-r))*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(r-1)*((1+(a/(1-r))*(la/v)*(exp(v*t1)-1))^(r-1)+
(1+(a/(1-r))*(la/v)*(exp(v*t2)-1))^(r-1))+
((1-R)^2)*(1+(a/(1-r))*(la/v)*(exp(v*t1)-1))^(r-1)*(1+(a/(1-r))*(la/v)*(exp(v*t2)-1))^(r-1))+
cen1*(1-cen2)*log( R*(1+(a/(1-r))*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(r-1)+(1-R)*(1+(a/(1-r))*(la/v)*(exp(v*t1)-1))^(r-1))+
(1-cen1)*cen2*log( R*(1+(a/(1-r))*((la/v)*(exp(v*t1)-1)+(la/v)*(exp(v*t2)-1)))^(r-1)+(1-R)*(1+(a/(1-r))*(la/v)*(exp(v*t2)-1))^(r-1)))
dxy <- deriv(llkexprss, c("r","a", "R","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
gd=c(c(gdpart1))
-gd
}


optimgomppvnc<-function(thet,xx){
xx=as.matrix(xx)
thet[thet<0.000001]<-0.000001
if(thet[1]>=1){thet[1]<-(1-0.000001)};if(thet[3]>=1){thet[3]<-(1-0.000001)}
ui0=matrix(0,7,length(thet));ci0=rep(0,7);ci0[2]<-(-1);ci0[5]<-(-1)
ui0[1,1]<-1;ui0[2,1]<-(-1);ui0[3,2]<-1
ui0[4,3]<-1;ui0[5,3]<-(-1);ui0[6,4]<-(1);ui0[7,5]<-(1)
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikgompcpnc,grad=.fdLlikgompcpnc,
xx=xx,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("alpha","theta","Row","scale","shape")
SE=sqrt(diag(INVE));sealp=SE[1];SE=SE[-1]
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(frailparest=c(theta=newtht[2],Row=newtht[3]),
basehazpar=c(scale=newtht[4],shape=newtht[5]),alpha=newtht[1],sealpha=sealp,vcov=INVE,stderr=SE,loglik=lik,
convergence=fittr$convergence,iterations=fittr$outer.iterations,AIC=aic,BIC=bic)
res
}


############Power variance frailty with Weibull baseline ########
#####################################################
#
#######################with covariates#######################
##########################################################
.Llikweibcp = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
r = abs(theta[1]);a = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4])
v = abs(theta[5]);bet = theta[6:length(theta)]
EBX1=exp(c(X1%*%bet));EBX2=exp(c(X2%*%bet))
H01=la*t1^v;H02=la*t2^v;H1=H01*EBX1;H2=H02*EBX2
logli=sum( ((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*H1)^r)+((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*H2)^r)+
((1-r)*R/(r*a))*(1-(1+(a/(1-r))*(H1+H2))^r)+
cen1*(log(la)+log(v)+(v-1)*log(t1)+c(X1%*%bet))+cen2*(log(la)+log(v)+(v-1)*log(t2)+c(X2%*%bet))+
cen1*cen2*log( (R^2)*(1+(a/(1-r))*(H1+H2))^(2*r-2)+(R*a)*(1+(a/(1-r))*(H1+H2))^(r-2)+
(1-R)*R*(1+(a/(1-r))*(H1+H2))^(r-1)*((1+(a/(1-r))*H1)^(r-1)+(1+(a/(1-r))*H2)^(r-1))+
((1-R)^2)*(1+(a/(1-r))*H1)^(r-1)*(1+(a/(1-r))*H2)^(r-1))+
cen1*(1-cen2)*log( R*(1+(a/(1-r))*(H1+H2))^(r-1)+(1-R)*(1+(a/(1-r))*H1)^(r-1))+
(1-cen1)*cen2*log( R*(1+(a/(1-r))*(H1+H2))^(r-1)+(1-R)*(1+(a/(1-r))*H2)^(r-1)))
-logli}

.fdLlikweibcp = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
r = abs(theta[1]);a = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4])
v = abs(theta[5]);bet = theta[6:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
H1=H2<-NULL
llkexprss <- expression( ((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*la*t1^v*eb1)^r)+((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*la*t2^v*eb2)^r)+
((1-r)*R/(r*a))*(1-(1+(a/(1-r))*(la*t1^v*eb1+la*t2^v*eb2))^r)+
cen1*(log(la)+log(v)+(v-1)*log(t1))+cen2*(log(la)+log(v)+(v-1)*log(t2))+
cen1*cen2*log( (R^2)*(1+(a/(1-r))*(la*t1^v*eb1+la*t2^v*eb2))^(2*r-2)+(R*a)*(1+(a/(1-r))*(la*t1^v*eb1+la*t2^v*eb2))^(r-2)+
(1-R)*R*(1+(a/(1-r))*(la*t1^v*eb1+la*t2^v*eb2))^(r-1)*((1+(a/(1-r))*la*t1^v*eb1)^(r-1)+(1+(a/(1-r))*la*t2^v*eb2)^(r-1))+
((1-R)^2)*(1+(a/(1-r))*la*t1^v*eb1)^(r-1)*(1+(a/(1-r))*la*t2^v*eb2)^(r-1))+
cen1*(1-cen2)*log( R*(1+(a/(1-r))*(la*t1^v*eb1+la*t2^v*eb2))^(r-1)+(1-R)*(1+(a/(1-r))*la*t1^v*eb1)^(r-1))+
(1-cen1)*cen2*log( R*(1+(a/(1-r))*(la*t1^v*eb1+la*t2^v*eb2))^(r-1)+(1-R)*(1+(a/(1-r))*la*t2^v*eb2)^(r-1)))
dxy <- deriv(llkexprss, c("r","a", "R","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
H01=la*t1^v;H02=la*t2^v;H1=H01*eb1;H2=H02*eb2
db<-derivdbcp(a=a,R=R,r=r,cen1=cen1,cen2=cen2,X1=X1,X2=X2,H1=H1,H2=H2)
db=as.vector(db)
gd=c(c(gdpart1),c(db))
-gd
}

optimweibpv<-function(thet,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2)
thet[thet<0.000001]<-0.000001
if(thet[1]>=1){thet[1]<-(1-0.000001)};if(thet[3]>=1){thet[3]<-(1-0.000001)}
ui0=matrix(0,7,length(thet));ci0=rep(0,7);ci0[2]<-(-1);ci0[5]<-(-1)
ui0[1,1]<-1;ui0[2,1]<-(-1);ui0[3,2]<-1
ui0[4,3]<-1;ui0[5,3]<-(-1);ui0[6,4]<-(1);ui0[7,5]<-(1)
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikweibcp,grad=.fdLlikweibcp,
xx=xx,X1=X1,X2=X2,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("alpha","theta","Row","scale","shape",colnames(X1))
SE=sqrt(diag(INVE));sealp=SE[1];SE=SE[-1]
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(coefficients=newtht[6:length(thet)],frailparest=c(theta=newtht[2],Row=newtht[3]),
basehazpar=c(scale=newtht[4],shape=newtht[5]),alpha=newtht[1],sealpha=sealp,vcov=INVE,stderr=SE,loglik=lik,
convergence=fittr$convergence,iterations=fittr$outer.iterations,AIC=aic,BIC=bic)
res
}

#######################without covariates#######################
.Llikweibcpnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
r = abs(theta[1]);a = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4]);v = abs(theta[5])
H1=la*t1^v;H2=la*t2^v
logli=sum( ((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*H1)^r)+((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*H2)^r)+
((1-r)*R/(r*a))*(1-(1+(a/(1-r))*(H1+H2))^r)+
cen1*(log(la)+log(v)+(v-1)*log(t1))+cen2*(log(la)+log(v)+(v-1)*log(t2))+
cen1*cen2*log( (R^2)*(1+(a/(1-r))*(H1+H2))^(2*r-2)+(R*a)*(1+(a/(1-r))*(H1+H2))^(r-2)+
(1-R)*R*(1+(a/(1-r))*(H1+H2))^(r-1)*((1+(a/(1-r))*H1)^(r-1)+(1+(a/(1-r))*H2)^(r-1))+
((1-R)^2)*(1+(a/(1-r))*H1)^(r-1)*(1+(a/(1-r))*H2)^(r-1))+
cen1*(1-cen2)*log( R*(1+(a/(1-r))*(H1+H2))^(r-1)+(1-R)*(1+(a/(1-r))*H1)^(r-1))+
(1-cen1)*cen2*log( R*(1+(a/(1-r))*(H1+H2))^(r-1)+(1-R)*(1+(a/(1-r))*H2)^(r-1)))
-logli}

.fdLlikweibcpnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
r = abs(theta[1]);a = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4]);v = abs(theta[5])
H1=H2<-NULL
llkexprss <- expression(((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*la*t1^v)^r)+((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*la*t2^v)^r)+
((1-r)*R/(r*a))*(1-(1+(a/(1-r))*(la*t1^v+la*t2^v))^r)+
cen1*(log(la)+log(v)+(v-1)*log(t1))+cen2*(log(la)+log(v)+(v-1)*log(t2))+
cen1*cen2*log( (R^2)*(1+(a/(1-r))*(la*t1^v+la*t2^v))^(2*r-2)+(R*a)*(1+(a/(1-r))*(la*t1^v+la*t2^v))^(r-2)+
(1-R)*R*(1+(a/(1-r))*(la*t1^v+la*t2^v))^(r-1)*((1+(a/(1-r))*la*t1^v)^(r-1)+(1+(a/(1-r))*la*t2^v)^(r-1))+
((1-R)^2)*(1+(a/(1-r))*la*t1^v)^(r-1)*(1+(a/(1-r))*la*t2^v)^(r-1))+
cen1*(1-cen2)*log( R*(1+(a/(1-r))*(la*t1^v+la*t2^v))^(r-1)+(1-R)*(1+(a/(1-r))*la*t1^v)^(r-1))+
(1-cen1)*cen2*log( R*(1+(a/(1-r))*(la*t1^v+la*t2^v))^(r-1)+(1-R)*(1+(a/(1-r))*la*t2^v)^(r-1)))
dxy <- deriv(llkexprss, c("r","a", "R","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gd=c(colSums(sr1$gradient))
-gd
}

optimweibpvnc<-function(thet,xx){
xx=as.matrix(xx)
thet[thet<0.000001]<-0.000001
if(thet[1]>=1){thet[1]<-(1-0.000001)};if(thet[3]>=1){thet[3]<-(1-0.000001)}
ui0=matrix(0,7,length(thet));ci0=rep(0,7);ci0[2]<-(-1);ci0[5]<-(-1)
ui0[1,1]<-1;ui0[2,1]<-(-1);ui0[3,2]<-1
ui0[4,3]<-1;ui0[5,3]<-(-1);ui0[6,4]<-(1);ui0[7,5]<-(1)
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikweibcpnc,grad=.fdLlikweibcpnc,
xx=xx,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("alpha","theta","Row","scale","shape")
SE=sqrt(diag(INVE));sealp=SE[1];SE=SE[-1]
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(frailparest=c(theta=newtht[2],Row=newtht[3]),
basehazpar=c(scale=newtht[4],shape=newtht[5]),alpha=newtht[1],sealpha=sealp,vcov=INVE,stderr=SE,loglik=lik,
convergence=fittr$convergence,iterations=fittr$outer.iterations,AIC=aic,BIC=bic)
res
}



############Power variance frailty with exponential baseline ########
#####################################################

#######################with covariates#######################
###########################################################
.Llikexpcp = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
r = abs(theta[1]);a = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4]);bet = theta[5:length(theta)]
EBX1=exp(c(X1%*%bet));EBX2=exp(c(X2%*%bet))
H01=la*t1;H02=la*t2;H1=H01*EBX1;H2=H02*EBX2
logli=sum( ((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*H1)^r)+((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*H2)^r)+
((1-r)*R/(r*a))*(1-(1+(a/(1-r))*(H1+H2))^r)+
cen1*(log(la)+c(X1%*%bet))+cen2*(log(la)+c(X2%*%bet))+
cen1*cen2*log( (R^2)*(1+(a/(1-r))*(H1+H2))^(2*r-2)+(R*a)*(1+(a/(1-r))*(H1+H2))^(r-2)+
(1-R)*R*(1+(a/(1-r))*(H1+H2))^(r-1)*((1+(a/(1-r))*H1)^(r-1)+(1+(a/(1-r))*H2)^(r-1))+
((1-R)^2)*(1+(a/(1-r))*H1)^(r-1)*(1+(a/(1-r))*H2)^(r-1))+
cen1*(1-cen2)*log( R*(1+(a/(1-r))*(H1+H2))^(r-1)+(1-R)*(1+(a/(1-r))*H1)^(r-1))+
(1-cen1)*cen2*log( R*(1+(a/(1-r))*(H1+H2))^(r-1)+(1-R)*(1+(a/(1-r))*H2)^(r-1)))
-logli}

.fdLlikexpcp = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
r = abs(theta[1]);a = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4]);bet = theta[5:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
H1=H2<-NULL
llkexprss <- expression( ((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*la*t1*eb1)^r)+((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*la*t2*eb2)^r)+
((1-r)*R/(r*a))*(1-(1+(a/(1-r))*(la*t1*eb1+la*t2*eb2))^r)+
cen1*(log(la))+cen2*(log(la))+
cen1*cen2*log( (R^2)*(1+(a/(1-r))*(la*t1*eb1+la*t2*eb2))^(2*r-2)+(R*a)*(1+(a/(1-r))*(la*t1*eb1+la*t2*eb2))^(r-2)+
(1-R)*R*(1+(a/(1-r))*(la*t1*eb1+la*t2*eb2))^(r-1)*((1+(a/(1-r))*la*t1*eb1)^(r-1)+(1+(a/(1-r))*la*t2*eb2)^(r-1))+
((1-R)^2)*(1+(a/(1-r))*la*t1*eb1)^(r-1)*(1+(a/(1-r))*la*t2*eb2)^(r-1))+
cen1*(1-cen2)*log( R*(1+(a/(1-r))*(la*t1*eb1+la*t2*eb2))^(r-1)+(1-R)*(1+(a/(1-r))*la*t1*eb1)^(r-1))+
(1-cen1)*cen2*log( R*(1+(a/(1-r))*(la*t1*eb1+la*t2*eb2))^(r-1)+(1-R)*(1+(a/(1-r))*la*t2*eb2)^(r-1)))
dxy <- deriv(llkexprss, c("r","a","R","la"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
H01=la*t1;H02=la*t2;H1=H01*eb1;H2=H02*eb2
db<-derivdbcp(a=a,R=R,r=r,cen1=cen1,cen2=cen2,X1=X1,X2=X2,H1=H1,H2=H2)
db=as.vector(db)
gd=c(c(gdpart1),c(db))
-gd
}

optimexppv<-function(thet,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2)
thet0=thet[1:4];thet0[thet0<0.000001]<-0.000001
if(thet0[1]>=1){thet0[1]<-(1-0.000001)};if(thet0[3]>=1){thet0[3]<-(1-0.000001)}
thet[1:4]<-thet0
ui0=matrix(0,6,length(thet));ci0=rep(0,6);ci0[2]<-(-1);ci0[5]<-(-1)
ui0[1,1]<-1;ui0[2,1]<-(-1);ui0[3,2]<-1
ui0[4,3]<-1;ui0[5,3]<-(-1);ui0[6,4]<-(1)
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikexpcp,grad=.fdLlikexpcp,
xx=xx,X1=X1,X2=X2,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("alpha","theta","Row","scale",colnames(X1))
SE=sqrt(diag(INVE));sealp=SE[1];SE=SE[-1]
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(coefficients=newtht[5:length(thet)],frailparest=c(theta=newtht[2],Row=newtht[3]),
basehazpar=c(scale=newtht[4]),alpha=newtht[1],sealpha=sealp,vcov=INVE,stderr=SE,
loglik=lik,convergence=fittr$convergence,iterations=fittr$outer.iterations,AIC=aic,BIC=bic)
res
}


#######################without covariates#######################

.Llikexpcpnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
r = abs(theta[1]);a = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4])
H1=la*t1;H2=la*t2
logli=sum( ((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*H1)^r)+((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*H2)^r)+
((1-r)*R/(r*a))*(1-(1+(a/(1-r))*(H1+H2))^r)+
cen1*(log(la))+cen2*(log(la))+
cen1*cen2*log( (R^2)*(1+(a/(1-r))*(H1+H2))^(2*r-2)+(R*a)*(1+(a/(1-r))*(H1+H2))^(r-2)+
(1-R)*R*(1+(a/(1-r))*(H1+H2))^(r-1)*((1+(a/(1-r))*H1)^(r-1)+(1+(a/(1-r))*H2)^(r-1))+
((1-R)^2)*(1+(a/(1-r))*H1)^(r-1)*(1+(a/(1-r))*H2)^(r-1))+
cen1*(1-cen2)*log( R*(1+(a/(1-r))*(H1+H2))^(r-1)+(1-R)*(1+(a/(1-r))*H1)^(r-1))+
(1-cen1)*cen2*log( R*(1+(a/(1-r))*(H1+H2))^(r-1)+(1-R)*(1+(a/(1-r))*H2)^(r-1)))
-logli}

.fdLlikexpcpnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
r = abs(theta[1]);a = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4])
H1=H2<-NULL
llkexprss <- expression( ((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*la*t1)^r)+((1-r)*(1-R)/(r*a))*(1-(1+(a/(1-r))*la*t2)^r)+
((1-r)*R/(r*a))*(1-(1+(a/(1-r))*(la*t1+la*t2))^r)+
cen1*(log(la))+cen2*(log(la))+
cen1*cen2*log( (R^2)*(1+(a/(1-r))*(la*t1+la*t2))^(2*r-2)+(R*a)*(1+(a/(1-r))*(la*t1+la*t2))^(r-2)+
(1-R)*R*(1+(a/(1-r))*(la*t1+la*t2))^(r-1)*((1+(a/(1-r))*la*t1)^(r-1)+(1+(a/(1-r))*la*t2)^(r-1))+
((1-R)^2)*(1+(a/(1-r))*la*t1)^(r-1)*(1+(a/(1-r))*la*t2)^(r-1))+
cen1*(1-cen2)*log( R*(1+(a/(1-r))*(la*t1+la*t2))^(r-1)+(1-R)*(1+(a/(1-r))*la*t1)^(r-1))+
(1-cen1)*cen2*log( R*(1+(a/(1-r))*(la*t1+la*t2))^(r-1)+(1-R)*(1+(a/(1-r))*la*t2)^(r-1)))
dxy <- deriv(llkexprss, c("r","a","R","la"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
gd=c(c(gdpart1))
-gd
}

optimexppvnc<-function(thet,xx){
xx=as.matrix(xx)
thet[thet<0.000001]<-0.000001
if(thet[1]>=1){thet[1]<-(1-0.000001)};if(thet[3]>=1){thet[3]<-(1-0.000001)}
ui0=matrix(0,6,length(thet));ci0=rep(0,6);ci0[2]<-(-1);ci0[5]<-(-1)
ui0[1,1]<-1;ui0[2,1]<-(-1);ui0[3,2]<-1
ui0[4,3]<-1;ui0[5,3]<-(-1);ui0[6,4]<-(1)
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikexpcpnc,grad=.fdLlikexpcpnc,
xx=xx,ui=ui0,ci=ci0,hessian=TRUE)))
newtht=fittr$par
lik=-fittr$value
INVE=solve(fittr$hessian)
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("alpha","theta","Row","scale")
SE=sqrt(diag(INVE));sealp=SE[1];SE=SE[-1]
aic=-2*lik+2*length(thet)
bic=-2*lik+log(2*nrow(xx))*length(thet)
res=list(frailparest=c(theta=newtht[2],Row=newtht[3]),
basehazpar=c(scale=newtht[4]),alpha=newtht[1],sealpha=sealp,vcov=INVE,stderr=SE,loglik=lik,
convergence=fittr$convergence,iterations=fittr$outer.iterations,AIC=aic,BIC=bic)
res
}


fitbccv.pvpar<-function(X,Y,initfrailp,inithazp,initbeta,haz){
time=Y[, 1];censor=Y[, 2]
data.n1 <- length(time);data.n <-data.n1/2#### data.n is the number of pairs
ar=array(1:data.n)
i1<-2*ar-1;i2<-2*ar
cen1=censor[i1];cen2=censor[i2]
t1=time[i1];t2=time[i2]
xx=matrix(c(t1,t2,cen1,cen2),length(cen1),4)
thet=1
n_covar=ncol(X)
optimfunc<-NULL
if(n_covar>0){
X1=X[i1,];X2=X[i2,]
if(n_covar==1){
X2=matrix(c(X2),data.n,1);X1=matrix(c(X1),data.n,1)}
X1=as.matrix(X1);X2=as.matrix(X2)
colnames(X1)<-colnames(X2)<-colnames(X)
if(length(initbeta)>0){initbeta=initbeta}else{
cph0 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset =NULL, init = NULL, control = survival::coxph.control(),
weights = NULL,method = "breslow", resid=FALSE)
initbeta<-cph0$coefficients}
if(length(initfrailp)>0){initfrailp=initfrailp}else{initfrailp<-c(0.5,0.5)}
if (haz== c("weibull")) {optimfunc<-optimweibpv
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05,0.05)}
thet=c(0.1,initfrailp,inithazp,initbeta)}
if (haz== c("gompertz")) {optimfunc<-optimgomppv
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05,0.05)}
thet=c(0.1,initfrailp,inithazp,initbeta)}
if (haz== c("exponential")) {optimfunc<-optimexppv
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05)}
thet=c(0.1,initfrailp,inithazp,initbeta)}
res<-optimfunc(thet,xx,X1,X2)}
if(n_covar==0){
if(length(initfrailp)>0){initfrailp=initfrailp}else{initfrailp<-c(0.5,0.5)}
if (haz== c("weibull")) {optimfunc<-optimweibpvnc
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05,0.05)}
thet=c(0.2,initfrailp,inithazp)}
if (haz== c("gompertz")) {optimfunc<-optimgomppvnc
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05,0.05)}
thet=c(0.2,initfrailp,inithazp)}
if (haz== c("exponential")) {optimfunc<-optimexppvnc
if(length(inithazp)>0){inithazp=inithazp}else{inithazp<-c(0.05)}
thet=c(0.2,initfrailp,inithazp)}
res<-optimfunc(thet,xx)
}
res$time <- time
res$censor <- censor
res$call <- match.call()
class(res) <- c("bcfrailpar")
res
}

########################different variance with covariates
########

SEofgompertzdv = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a1 = abs(theta[1]);a2 = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4])
v = abs(theta[5]);bet = theta[6:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
H1<-H2<-NULL
llkexprss <- expression(((cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)-
((1/a1)-(R/(a1^(1/2)*a2^(1/2))))*log(1+a1*(la/v)*(exp(v*t1)-1)*eb1)-((1/a2)-(R/(a1^(1/2)*a2^(1/2))))*log(1+a2*(la/v)*(exp(v*t2)-1)*eb2)-
(R/(a1^(1/2)*a2^(1/2)))*log(1+a1*(la/v)*(exp(v*t1)-1)*eb1+a2*(la/v)*(exp(v*t2)-1)*eb2))+
cen1*cen2*(log(((R+(a1^(1/2)*a2^(1/2)))*R)*(1/(1+a1*(la/v)*(exp(v*t1)-1)*eb1+a2*(la/v)*(exp(v*t2)-1)*eb2)^2)+
(1/(1+a1*(la/v)*(exp(v*t1)-1)*eb1+a2*(la/v)*(exp(v*t2)-1)*eb2))*((R*(a2^(1/2)/a1^(1/2))-R^2)*(1/(1+a1*(la/v)*(exp(v*t1)-1)*eb1))+
(R*(a1^(1/2)/a2^(1/2))-R^2)*(1/(1+a2*(la/v)*(exp(v*t2)-1)*eb2)))+
((1/(1+a1*(la/v)*(exp(v*t1)-1)*eb1))*(1/(1+a2*(la/v)*(exp(v*t2)-1)*eb2)))*((1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R))))+
cen1*(1-cen2)*(log((a1^(1/2)/a2^(1/2))*R*(1/(1+a1*(la/v)*(exp(v*t1)-1)*eb1+a2*(la/v)*(exp(v*t2)-1)*eb2))+
(1-(a1^(1/2)/a2^(1/2))*R)*(1/(1+a1*(la/v)*(exp(v*t1)-1)*eb1))))+
(1-cen1)*cen2*(log((a2^(1/2)/a1^(1/2))*R*(1/(1+a1*(la/v)*(exp(v*t1)-1)*eb1+a2*(la/v)*(exp(v*t2)-1)*eb2))+
(1-(a2^(1/2)/a1^(1/2))*R)*(1/(1+a2*(la/v)*(exp(v*t2)-1)*eb2))))))
dxy <- deriv3(llkexprss, c("a1","a2", "R","la","v"),hessian = TRUE)
sr=eval(dxy)
sr1<-attributes(sr)
Hes0=colSums(sr1$hessian)
digg=diag(Hes0)
if(any(digg>=0)){
digg0=digg[digg>=0];nl=length(digg0)
order=sort(digg, decreasing = TRUE, index.return = TRUE)
indx=order$ix[1:nl]
tht=theta[indx]
tht[tht<0.000001]<-0.000001
theta[indx]<-tht
a1 = abs(theta[1]);a2 = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4])
v = abs(theta[5]);bet = theta[6:length(theta)]
sr=eval(dxy)
sr1<-attributes(sr)
Hes0=colSums(sr1$hessian)}
EBX1=eb1;EBX2=eb2
H01=(la/v)*(exp(v*t1)-1);H02=(la/v)*(exp(v*t2)-1);H1=H01*EBX1;H2=H02*EBX2
dlaH01=(1/v)*(exp(v*t1)-1);dlaH02=(1/v)*(exp(v*t2)-1)
dvH01=-(la/v^2)*(exp(v*t1)-1)+(la/v)*(exp(v*t1)*t1);dvH02=-(la/v^2)*(exp(v*t2)-1)+(la/v)*(exp(v*t2)*t2)
dlaH1=dlaH01*EBX1;dlaH2=dlaH02*EBX2
dvH1=dvH01*EBX1;dvH2=dvH02*EBX2
dbla=derivdbdxdv(newtht=theta,cen1=cen1,cen2=cen2,X1=X1,X2=X2,H1=H1,H2=H2,dx1H1=dlaH1,dx1H2=dlaH2)
dbv=derivdbdxdv(newtht=theta,cen1=cen1,cen2=cen2,X1=X1,X2=X2,H1=H1,H2=H2,dx1H1=dvH1,dx1H2=dvH2)
Dbx1x2=rbind(dbla,dbv)
comp=D2bnthtderivdv(newtht=theta,X1=X1,X2=X2,H1=H1,H2=H2,cen1=cen1,cen2=cen2)
D2b=comp$D2B;Dbtht=comp$DBtht
Hes1=cbind(t(Dbtht),t(Dbx1x2))
Hes2=rbind(Hes0,Hes1)
Hes3=rbind(Dbtht,Dbx1x2,D2b)
Hes=cbind(Hes2,Hes3)
INVE=solve(-Hes)
SE=sqrt(diag(INVE))
list(vcov=INVE,stderr=SE)
}

.Llikgompertzdv = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
e1 = theta[1];e2 = theta[2];e3 = theta[3];la = abs(theta[4]);v = abs(theta[5]);bet = theta[6:length(theta)]
EBX1=exp(c(X1%*%bet));EBX2=exp(c(X2%*%bet))
H01=(la/v)*(exp(v*t1)-1);H02=(la/v)*(exp(v*t2)-1);H1=H01*EBX1;H2=H02*EBX2
logli=sum(cen1*(log(la)+v*t1+c(X1%*%bet))+cen2*(log(la)+v*t2+c(X2%*%bet))+(-((1/exp(e1))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e1)*H1)-((1/exp(e2))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e2)*H2)-
(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2)))*log(1+exp(e1)*H1+exp(e2)*H2))+cen1*cen2*(-log(1+exp(e1)*H1)-log(1+exp(e2)*H2)-2*log(1+exp(e1)*H1+exp(e2)*H2)+
log(((exp(e3)+((exp(e1))^(1/2)*(exp(e2))^(1/2)))*exp(e3)*(1+exp(e1)*H1)*(1+exp(e2)*H2)+((((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e1)*H1)+
(((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e2)*H2))*(1+exp(e1)*H1+exp(e2)*H2)+
(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*((1+exp(e1)*H1+exp(e2)*H2))^2)))+
cen1*(1-cen2)*(-log(1+exp(e1)*H1)-log(1+exp(e1)*H1+exp(e2)*H2)+
log(((1+exp(e1)*H1)+(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1+exp(e2)*H2)-(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)))))+
(1-cen1)*cen2*(-log(1+exp(e2)*H2)-log(1+exp(e1)*H1+exp(e2)*H2)+
log(((1+exp(e2)*H2)+(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*(1+exp(e1)*H1)-(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))))))
-logli
    }

.fdLlikgompertzdv = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
e1 = theta[1];e2 = theta[2];e3 = theta[3];la = abs(theta[4]);v = abs(theta[5]);bet = theta[6:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
H1<-H2<-NULL
llkexprss <- expression( (  cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)+
((-( (1/exp(e1))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e1)*(la/v)*(exp(v*t1)-1)*eb1)-
((1/exp(e2))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e2)*(la/v)*(exp(v*t2)-1)*eb2)-
(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2)))*log(1+exp(e1)*(la/v)*(exp(v*t1)-1)*eb1+exp(e2)*(la/v)*(exp(v*t2)-1)*eb2))+
cen1*cen2*(-log(1+exp(e1)*(la/v)*(exp(v*t1)-1)*eb1)-log(1+exp(e2)*(la/v)*(exp(v*t2)-1)*eb2)-2*log(1+exp(e1)*(la/v)*(exp(v*t1)-1)*eb1+exp(e2)*(la/v)*(exp(v*t2)-1)*eb2)+
log(((exp(e3)+((exp(e1))^(1/2)*(exp(e2))^(1/2)))*exp(e3)*(1+exp(e1)*(la/v)*(exp(v*t1)-1)*eb1)*(1+exp(e2)*(la/v)*(exp(v*t2)-1)*eb2)+
((((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e1)*(la/v)*(exp(v*t1)-1)*eb1)+
(((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e2)*(la/v)*(exp(v*t2)-1)*eb2))*(1+exp(e1)*(la/v)*(exp(v*t1)-1)*eb1+exp(e2)*(la/v)*(exp(v*t2)-1)*eb2)+
(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*((1+exp(e1)*(la/v)*(exp(v*t1)-1)*eb1+exp(e2)*(la/v)*(exp(v*t2)-1)*eb2))^2)))+
cen1*(1-cen2)*(-log(1+exp(e1)*(la/v)*(exp(v*t1)-1)*eb1)-log(1+exp(e1)*(la/v)*(exp(v*t1)-1)*eb1+exp(e2)*(la/v)*(exp(v*t2)-1)*eb2)+
log(((1+exp(e1)*(la/v)*(exp(v*t1)-1)*eb1)+(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1+exp(e2)*(la/v)*(exp(v*t2)-1)*eb2)-(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)))))+
(1-cen1)*cen2*(-log(1+exp(e2)*(la/v)*(exp(v*t2)-1)*eb2)-log(1+exp(e1)*(la/v)*(exp(v*t1)-1)*eb1+exp(e2)*(la/v)*(exp(v*t2)-1)*eb2)+
log(((1+exp(e2)*(la/v)*(exp(v*t2)-1)*eb2)+(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*(1+exp(e1)*(la/v)*(exp(v*t1)-1)*eb1)-(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))))))))
dxy <- deriv(llkexprss, c("e1","e2", "e3","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
EBX1=eb1;EBX2=eb2
dbEBX1=c(EBX1)*X1;dbEBX2=c(EBX2)*X2
H01=(la/v)*(exp(v*t1)-1);H02=(la/v)*(exp(v*t2)-1)
H1=H01*EBX1;H2=H02*EBX2
dbH1=c(H01)*dbEBX1;dbH2=c(H02)*dbEBX2
a1=exp(e1);a2=exp(e2);R=exp(e3)
P11=( ((R+(a1^(1/2)*a2^(1/2)))*R)*(1/(1+a1*H1+a2*H2)^2)+
(1/(1+a1*H1+a2*H2))*((R*(a2^(1/2)/a1^(1/2))-R^2)*(1/(1+a1*H1))+(R*(a1^(1/2)/a2^(1/2))-R^2)*(1/(1+a2*H2)))+
((1/(1+a1*H1))*(1/(1+a2*H2)))*((1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R)) )
dbP11=(  ((R+(a1^(1/2)*a2^(1/2)))*R)*(-2/(1+a1*H1+a2*H2)^3)*(a1*dbH1+a2*dbH2)+
((-1/(1+a1*H1+a2*H2)^2)*((R*(a2^(1/2)/a1^(1/2))-R^2)*(1/(1+a1*H1))+(R*(a1^(1/2)/a2^(1/2))-R^2)*(1/(1+a2*H2))))*(a1*dbH1+a2*dbH2)+
(1/(1+a1*H1+a2*H2))*((R*(a2^(1/2)/a1^(1/2))-R^2)*(-1/(1+a1*H1)^2)*(a1*dbH1)+(R*(a1^(1/2)/a2^(1/2))-R^2)*(-1/(1+a2*H2)^2)*(a2*dbH2))+
( (-1/(1+a1*H1)^2)*(1/(1+a2*H2))*(a1*dbH1)+(1/(1+a1*H1))*(-1/(1+a2*H2)^2)*(a2*dbH2) )*((1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R) ) )
P10=((a1^(1/2)/a2^(1/2))*R*(1/(1+a1*H1+a2*H2))+ (1-(a1^(1/2)/a2^(1/2))*R)*(1/(1+a1*H1)))
dbP10=((a1^(1/2)/a2^(1/2))*R*(-1/(1+a1*H1+a2*H2)^2)*(a1*dbH1+a2*dbH2)+(1-(a1^(1/2)/a2^(1/2))*R)*(-1/(1+a1*H1)^2)*(a1*dbH1))
P01=((a2^(1/2)/a1^(1/2))*R*(1/(1+a1*H1+a2*H2))+ (1-(a2^(1/2)/a1^(1/2))*R)*(1/(1+a2*H2)))
dbP01=((a2^(1/2)/a1^(1/2))*R*(-1/(1+a1*H1+a2*H2)^2)*(a1*dbH1+a2*dbH2)+(1-(a2^(1/2)/a1^(1/2))*R)*(-1/(1+a2*H2)^2)*(a2*dbH2))
db=c(colSums(cen1*X1+cen2*X2+(-c(((1/a1)-(R/(a1^(1/2)*a2^(1/2))))*(a1/(1+a1*H1)))*dbH1-
c(((1/a2)-(R/(a1^(1/2)*a2^(1/2))))*(a2/(1+a2*H2)))*dbH2-(R/(a1^(1/2)*a2^(1/2)))*(1/(1+a1*H1+a2*H2))*(a1*dbH1+a2*dbH2))+
((cen1*cen2)/P11)*dbP11+((cen1*(1-cen2))/P10)*dbP10+(((1-cen1)*cen2)/P01)*dbP01))
gd=c(c(gdpart1),c(db))
-gd
}

optimgomperzdv<-function(thet,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2)
ui0=matrix(0,4,length(thet));ci0=rep(0,4)
ui0[1,1]<-c(-0.5);ui0[1,2]<-c(0.5);ui0[1,3]<-c(-1)
ui0[2,1]<-c(0.5);ui0[2,2]<-c(-0.5);ui0[2,3]<-c(-1)
ui0[3,4]<-c(1);ui0[4,5]<-c(1)
thet[1:3]<-log(thet[1:3])
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikgompertzdv,grad=.fdLlikgompertzdv,
xx=xx,X1=X1,X2=X2,ui=ui0,ci=ci0)))
newtht=lnewtht=fittr$par
newtht0=newtht[1:3]
newtht[1:3]<-exp(newtht0)
newtht0=newtht
rbb=feasibleregforR(newtht0)
if((rbb-newtht0[3])<0.0001){newtht0[3]=newtht0[3]*0.99}
if(newtht0[3]>=0.999){newtht0[3]<-(0.999)}
see=SEofgompertzdv(newtht0,xx=xx,X1=X1,X2=X2)
INVE=see$vcov
SE=see$stderr
if(any(diag(INVE)<0)){
warning("Check the approperiatness of the model.")
warning("estimates might be at the boundary of parameter space.")
warning("SE of estimates is false or not correct.")}
INVE=as.matrix(INVE)
aic=2*fittr$value+2*length(thet)
bic=2*fittr$value+log(2*nrow(xx))*length(thet)
colnames(INVE) <- rownames(INVE) <- c("theta1","theta2","Row","scale","shape",colnames(X1))
res=list(coefficients=lnewtht[6:length(thet)],frailparest=c(theta1=newtht[1],theta2=newtht[2],Row=newtht[3]),
logfrailparest=lnewtht,AIC=aic,BIC=bic,
basehazpar=c(scale=lnewtht[4],shape=lnewtht[5]),vcov=INVE,stderr=SE,
loglik=(-fittr$value))
res
}

######

########################different variance without covariates
.Llikgompertzdvnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
e1 = theta[1];e2 = theta[2];e3 = theta[3];la = abs(theta[4]);v = abs(theta[5])
H1=(la/v)*(exp(v*t1)-1);H2=(la/v)*(exp(v*t2)-1)
logli=sum(cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)+(-((1/exp(e1))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e1)*H1)-((1/exp(e2))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e2)*H2)-
(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2)))*log(1+exp(e1)*H1+exp(e2)*H2))+cen1*cen2*(-log(1+exp(e1)*H1)-log(1+exp(e2)*H2)-2*log(1+exp(e1)*H1+exp(e2)*H2)+
log(((exp(e3)+((exp(e1))^(1/2)*(exp(e2))^(1/2)))*exp(e3)*(1+exp(e1)*H1)*(1+exp(e2)*H2)+((((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e1)*H1)+
(((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e2)*H2))*(1+exp(e1)*H1+exp(e2)*H2)+
(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*((1+exp(e1)*H1+exp(e2)*H2))^2)))+
cen1*(1-cen2)*(-log(1+exp(e1)*H1)-log(1+exp(e1)*H1+exp(e2)*H2)+
log(((1+exp(e1)*H1)+(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1+exp(e2)*H2)-(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)))))+
(1-cen1)*cen2*(-log(1+exp(e2)*H2)-log(1+exp(e1)*H1+exp(e2)*H2)+
log(((1+exp(e2)*H2)+(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*(1+exp(e1)*H1)-(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))))))
-logli
}

.fdLlikgompertzdvnc= function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
e1 = theta[1];e2 = theta[2];e3 = theta[3];la = abs(theta[4]);v = abs(theta[5])
H1<-H2<-NULL
llkexprss <- expression((cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)+(-((1/exp(e1))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e1)*(la/v)*(exp(v*t1)-1))-
((1/exp(e2))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e2)*(la/v)*(exp(v*t2)-1))-
(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2)))*log(1+exp(e1)*(la/v)*(exp(v*t1)-1)+exp(e2)*(la/v)*(exp(v*t2)-1)))+cen1*cen2*(-log(1+exp(e1)*(la/v)*(exp(v*t1)-1))-
log(1+exp(e2)*(la/v)*(exp(v*t2)-1))-2*log(1+exp(e1)*(la/v)*(exp(v*t1)-1)+exp(e2)*(la/v)*(exp(v*t2)-1))+
log(((exp(e3)+((exp(e1))^(1/2)*(exp(e2))^(1/2)))*exp(e3)*(1+exp(e1)*(la/v)*(exp(v*t1)-1))*(1+exp(e2)*(la/v)*(exp(v*t2)-1))+
((((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e1)*(la/v)*(exp(v*t1)-1))+
(((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e2)*(la/v)*(exp(v*t2)-1)))*(1+exp(e1)*(la/v)*(exp(v*t1)-1)+exp(e2)*(la/v)*(exp(v*t2)-1))+
(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*((1+exp(e1)*(la/v)*(exp(v*t1)-1)+exp(e2)*(la/v)*(exp(v*t2)-1)))^2)))+
cen1*(1-cen2)*(-log(1+exp(e1)*(la/v)*(exp(v*t1)-1))-log(1+exp(e1)*(la/v)*(exp(v*t1)-1)+exp(e2)*(la/v)*(exp(v*t2)-1))+
log(((1+exp(e1)*(la/v)*(exp(v*t1)-1))+(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1+exp(e2)*(la/v)*(exp(v*t2)-1))-(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)))))+
(1-cen1)*cen2*(-log(1+exp(e2)*(la/v)*(exp(v*t2)-1))-log(1+exp(e1)*(la/v)*(exp(v*t1)-1)+exp(e2)*(la/v)*(exp(v*t2)-1))+
log(((1+exp(e2)*(la/v)*(exp(v*t2)-1))+(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*(1+exp(e1)*(la/v)*(exp(v*t1)-1))-(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)))))))
dxy <- deriv(llkexprss, c("e1","e2", "e3","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gd=c(colSums(sr1$gradient))
-gd
}

.sdLlikgompertzdvnc= function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a1 = (theta[1]);a2 = (theta[2]);R = (theta[3]);la = abs(theta[4]);v = abs(theta[5])
llkexprss <- expression(((cen1*(log(la)+v*t1)+cen2*(log(la)+v*t2)-
((1/a1)-(R/(a1^(1/2)*a2^(1/2))))*log(1+a1*(la/v)*(exp(v*t1)-1))-((1/a2)-(R/(a1^(1/2)*a2^(1/2))))*log(1+a2*(la/v)*(exp(v*t2)-1))-
(R/(a1^(1/2)*a2^(1/2)))*log(1+a1*(la/v)*(exp(v*t1)-1)+a2*(la/v)*(exp(v*t2)-1)))+
cen1*cen2*(log(((R+(a1^(1/2)*a2^(1/2)))*R)*(1/(1+a1*(la/v)*(exp(v*t1)-1)+a2*(la/v)*(exp(v*t2)-1))^2)+
(1/(1+a1*(la/v)*(exp(v*t1)-1)+a2*(la/v)*(exp(v*t2)-1)))*((R*(a2^(1/2)/a1^(1/2))-R^2)*(1/(1+a1*(la/v)*(exp(v*t1)-1)))+
(R*(a1^(1/2)/a2^(1/2))-R^2)*(1/(1+a2*(la/v)*(exp(v*t2)-1))))+
((1/(1+a1*(la/v)*(exp(v*t1)-1)))*(1/(1+a2*(la/v)*(exp(v*t2)-1))))*((1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R))))+
cen1*(1-cen2)*(log((a1^(1/2)/a2^(1/2))*R*(1/(1+a1*(la/v)*(exp(v*t1)-1)+a2*(la/v)*(exp(v*t2)-1)))+
(1-(a1^(1/2)/a2^(1/2))*R)*(1/(1+a1*(la/v)*(exp(v*t1)-1)))))+
(1-cen1)*cen2*(log((a2^(1/2)/a1^(1/2))*R*(1/(1+a1*(la/v)*(exp(v*t1)-1)+a2*(la/v)*(exp(v*t2)-1)))+
(1-(a2^(1/2)/a1^(1/2))*R)*(1/(1+a2*(la/v)*(exp(v*t2)-1)))))))
dxy <- deriv3(llkexprss, c("a1","a2", "R","la","v"),hessian = TRUE)
sr=eval(dxy)
sr1<-attributes(sr)
Hes=colSums(sr1$hessian)
digg=diag(Hes)
if(any(digg>=0)){
digg0=digg[digg>=0];nl=length(digg0)
order=sort(digg, decreasing = TRUE, index.return = TRUE)
indx=order$ix[1:nl]
tht=theta[indx]
tht[tht<0.000001]<-0.000001
theta[indx]<-tht
a1 = abs(theta[1]);a2 = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4]);v = abs(theta[5])
sr=eval(dxy)
sr1<-attributes(sr)
Hes=colSums(sr1$hessian)}
INVE=solve(-Hes)
SE=sqrt(diag(INVE))
list(vcov=INVE,stderr=SE)
}




optimgomperzdvnc<-function(thet,xx){
xx=as.matrix(xx)
ui0=matrix(0,4,length(thet));ci0=rep(0,4)
ui0[1,1]<-c(-0.5);ui0[1,2]<-c(0.5);ui0[1,3]<-c(-1)
ui0[2,1]<-c(0.5);ui0[2,2]<-c(-0.5);ui0[2,3]<-c(-1)
ui0[3,4]<-c(1);ui0[4,5]<-c(1)
thet[1:3]<-log(thet[1:3])
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikgompertzdvnc,grad=.fdLlikgompertzdvnc,
xx=xx,ui=ui0,ci=ci0)))
newtht=lnewtht=fittr$par
newtht0=newtht[1:3]
newtht[1:3]<-exp(newtht0)
newtht0=newtht;newtht00=newtht0[1:3]
newtht00[newtht00<0.000001]<-0.000001
rbb=feasibleregforR(newtht00)
if((rbb-newtht00[3])<0.001){newtht00[3]=newtht00[3]*0.95}
if(newtht00[3]>=0.999){newtht00[3]<-(0.999)}
newtht0[1:3]<-newtht00
OIM=.sdLlikgompertzdvnc(theta=newtht0,xx=xx)
INVE=OIM$vcov
SE=OIM$stderr
INVE=as.matrix(INVE)
aic=2*fittr$value+2*length(thet)
bic=2*fittr$value+log(2*nrow(xx))*length(thet)
colnames(INVE) <- rownames(INVE) <- c("theta1","theta2","Row","scale","shape")
res=list(frailparest=c(theta1=newtht[1],theta2=newtht[2],Row=newtht[3]),logfrailparest=lnewtht,
         AIC=aic,BIC=bic,
basehazpar=c(scale=lnewtht[4],shape=lnewtht[5]),vcov=INVE,stderr=SE,loglik=(-fittr$value))
res
}



########################different variance with covariates
########

.Llikweibdv = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
e1 = theta[1];e2 = theta[2];e3 = theta[3];la = abs(theta[4]);v = abs(theta[5]);bet = theta[6:length(theta)]
EBX1=exp(c(X1%*%bet));EBX2=exp(c(X2%*%bet))
H01=la*t1^v;H02=la*t2^v;H1=H01*EBX1;H2=H02*EBX2
logli=sum(cen1*(log(la)+log(v)+(v-1)*log(t1)+c(X1%*%bet))+cen2*(log(la)+log(v)+(v-1)*log(t2)+c(X2%*%bet))+
((-((1/exp(e1))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e1)*H1)-((1/exp(e2))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e2)*H2)-
(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2)))*log(1+exp(e1)*H1+exp(e2)*H2))+cen1*cen2*(-log(1+exp(e1)*H1)-log(1+exp(e2)*H2)-2*log(1+exp(e1)*H1+exp(e2)*H2)+
log(((exp(e3)+((exp(e1))^(1/2)*(exp(e2))^(1/2)))*exp(e3)*(1+exp(e1)*H1)*(1+exp(e2)*H2)+((((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e1)*H1)+
(((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e2)*H2))*(1+exp(e1)*H1+exp(e2)*H2)+
(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*((1+exp(e1)*H1+exp(e2)*H2))^2)))+
cen1*(1-cen2)*(-log(1+exp(e1)*H1)-log(1+exp(e1)*H1+exp(e2)*H2)+
log(((1+exp(e1)*H1)+(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1+exp(e2)*H2)-(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)))))+
(1-cen1)*cen2*(-log(1+exp(e2)*H2)-log(1+exp(e1)*H1+exp(e2)*H2)+
log(((1+exp(e2)*H2)+(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*(1+exp(e1)*H1)-(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)))))))
-logli
    }


.fdLlikweibdv = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
e1 = theta[1];e2 = theta[2];e3 = theta[3];la = abs(theta[4]);v = abs(theta[5]);bet = theta[6:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
llkexprss <- expression(cen1*(log(la)+log(v)+(v-1)*log(t1))+cen2*(log(la)+log(v)+(v-1)*log(t2))+
(-((1/exp(e1))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e1)*la*t1^v*eb1)-
((1/exp(e2))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e2)*la*t2^v*eb2)-
(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2)))*log(1+exp(e1)*la*t1^v*eb1+exp(e2)*la*t2^v*eb2))+
cen1*cen2*(-log(1+exp(e1)*la*t1^v*eb1)-log(1+exp(e2)*la*t2^v*eb2)-2*log(1+exp(e1)*la*t1^v*eb1+exp(e2)*la*t2^v*eb2)+
log(((exp(e3)+((exp(e1))^(1/2)*(exp(e2))^(1/2)))*exp(e3)*(1+exp(e1)*la*t1^v*eb1)*(1+exp(e2)*la*t2^v*eb2)+
((((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e1)*la*t1^v*eb1)+
(((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e2)*la*t2^v*eb2))*(1+exp(e1)*la*t1^v*eb1+exp(e2)*la*t2^v*eb2)+
(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*((1+exp(e1)*la*t1^v*eb1+exp(e2)*la*t2^v*eb2))^2)))+
cen1*(1-cen2)*(-log(1+exp(e1)*la*t1^v*eb1)-log(1+exp(e1)*la*t1^v*eb1+exp(e2)*la*t2^v*eb2)+
log(((1+exp(e1)*la*t1^v*eb1)+(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1+exp(e2)*la*t2^v*eb2)-(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)))))+
(1-cen1)*cen2*(-log(1+exp(e2)*la*t2^v*eb2)-log(1+exp(e1)*la*t1^v*eb1+exp(e2)*la*t2^v*eb2)+
log(((1+exp(e2)*la*t2^v*eb2)+(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*(1+exp(e1)*la*t1^v*eb1)-(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))))))
dxy <- deriv(llkexprss, c("e1","e2", "e3","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
a1=exp(e1);a2=exp(e2);R=exp(e3)
dbEBX1=c(eb1)*X1;dbEBX2=c(eb2)*X2
H01=la*t1^v;H02=la*t2^v;H1=H01*eb1;H2=H02*eb2
dbH1=c(H01)*dbEBX1;dbH2=c(H02)*dbEBX2
a1=exp(e1);a2=exp(e2);R=exp(e3)
P11=( ((R+(a1^(1/2)*a2^(1/2)))*R)*(1/(1+a1*H1+a2*H2)^2)+
(1/(1+a1*H1+a2*H2))*((R*(a2^(1/2)/a1^(1/2))-R^2)*(1/(1+a1*H1))+(R*(a1^(1/2)/a2^(1/2))-R^2)*(1/(1+a2*H2)))+
((1/(1+a1*H1))*(1/(1+a2*H2)))*((1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R)) )
dbP11=(  ((R+(a1^(1/2)*a2^(1/2)))*R)*(-2/(1+a1*H1+a2*H2)^3)*(a1*dbH1+a2*dbH2)+
((-1/(1+a1*H1+a2*H2)^2)*((R*(a2^(1/2)/a1^(1/2))-R^2)*(1/(1+a1*H1))+(R*(a1^(1/2)/a2^(1/2))-R^2)*(1/(1+a2*H2))))*(a1*dbH1+a2*dbH2)+
(1/(1+a1*H1+a2*H2))*((R*(a2^(1/2)/a1^(1/2))-R^2)*(-1/(1+a1*H1)^2)*(a1*dbH1)+(R*(a1^(1/2)/a2^(1/2))-R^2)*(-1/(1+a2*H2)^2)*(a2*dbH2))+
( (-1/(1+a1*H1)^2)*(1/(1+a2*H2))*(a1*dbH1)+(1/(1+a1*H1))*(-1/(1+a2*H2)^2)*(a2*dbH2) )*((1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R) ) )
P10=((a1^(1/2)/a2^(1/2))*R*(1/(1+a1*H1+a2*H2))+ (1-(a1^(1/2)/a2^(1/2))*R)*(1/(1+a1*H1)))
dbP10=((a1^(1/2)/a2^(1/2))*R*(-1/(1+a1*H1+a2*H2)^2)*(a1*dbH1+a2*dbH2)+(1-(a1^(1/2)/a2^(1/2))*R)*(-1/(1+a1*H1)^2)*(a1*dbH1))
P01=((a2^(1/2)/a1^(1/2))*R*(1/(1+a1*H1+a2*H2))+ (1-(a2^(1/2)/a1^(1/2))*R)*(1/(1+a2*H2)))
dbP01=((a2^(1/2)/a1^(1/2))*R*(-1/(1+a1*H1+a2*H2)^2)*(a1*dbH1+a2*dbH2)+(1-(a2^(1/2)/a1^(1/2))*R)*(-1/(1+a2*H2)^2)*(a2*dbH2))
db=c(colSums(cen1*X1+cen2*X2+(-c(((1/a1)-(R/(a1^(1/2)*a2^(1/2))))*(a1/(1+a1*H1)))*dbH1-
c(((1/a2)-(R/(a1^(1/2)*a2^(1/2))))*(a2/(1+a2*H2)))*dbH2-(R/(a1^(1/2)*a2^(1/2)))*(1/(1+a1*H1+a2*H2))*(a1*dbH1+a2*dbH2))+
((cen1*cen2)/P11)*dbP11+((cen1*(1-cen2))/P10)*dbP10+(((1-cen1)*cen2)/P01)*dbP01))
gd=c(c(gdpart1),c(db))
-gd
}



SEofweibdv = function(theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a1 = abs(theta[1]);a2 = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4])
v = abs(theta[5]);bet = theta[6:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
H1<-H2<-NULL
llkexprss <- expression(((cen1*(log(la)+log(v)+(v-1)*log(t1))+cen2*(log(la)+log(v)+(v-1)*log(t2))-
((1/a1)-(R/(a1^(1/2)*a2^(1/2))))*log(1+a1*la*t1^v*eb1)-((1/a2)-(R/(a1^(1/2)*a2^(1/2))))*log(1+a2*la*t2^v*eb2)-
(R/(a1^(1/2)*a2^(1/2)))*log(1+a1*la*t1^v*eb1+a2*la*t2^v*eb2))+
cen1*cen2*(log(((R+(a1^(1/2)*a2^(1/2)))*R)*(1/(1+a1*la*t1^v*eb1+a2*la*t2^v*eb2)^2)+
(1/(1+a1*la*t1^v*eb1+a2*la*t2^v*eb2))*((R*(a2^(1/2)/a1^(1/2))-R^2)*(1/(1+a1*la*t1^v*eb1))+
(R*(a1^(1/2)/a2^(1/2))-R^2)*(1/(1+a2*la*t2^v*eb2)))+
((1/(1+a1*la*t1^v*eb1))*(1/(1+a2*la*t2^v*eb2)))*((1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R))))+
cen1*(1-cen2)*(log((a1^(1/2)/a2^(1/2))*R*(1/(1+a1*la*t1^v*eb1+a2*la*t2^v*eb2))+ (1-(a1^(1/2)/a2^(1/2))*R)*(1/(1+a1*la*t1^v*eb1))))+
(1-cen1)*cen2*(log((a2^(1/2)/a1^(1/2))*R*(1/(1+a1*la*t1^v*eb1+a2*la*t2^v*eb2))+ (1-(a2^(1/2)/a1^(1/2))*R)*(1/(1+a2*la*t2^v*eb2))))))
dxy <- deriv3(llkexprss, c("a1","a2", "R","la","v"),hessian = TRUE)
sr=eval(dxy)
sr1<-attributes(sr)
Hes0=colSums(sr1$hessian)
digg=diag(Hes0)
if(any(digg>=0)){
digg0=digg[digg>=0];nl=length(digg0)
order=sort(digg, decreasing = TRUE, index.return = TRUE)
indx=order$ix[1:nl]
tht=theta[indx]
tht[tht<0.000001]<-0.000001
theta[indx]<-tht
a1 = abs(theta[1]);a2 = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4])
v = abs(theta[5]);bet = theta[6:length(theta)]
sr=eval(dxy)
sr1<-attributes(sr)
Hes0=colSums(sr1$hessian)}
H01=la*t1^v;H02=la*t2^v;H1=H01*eb1;H2=H02*eb2
dlaH01=t1^v;dlaH02=t2^v
dvH01=la*t1^v*log(t1);dvH02=la*t2^v*log(t2)
dlaH1=dlaH01*eb1;dlaH2=dlaH02*eb2
dvH1=dvH01*eb1;dvH2=dvH02*eb2
dbla=derivdbdxdv(newtht=theta,cen1=cen1,cen2=cen2,X1=X1,X2=X2,H1=H1,H2=H2,dx1H1=dlaH1,dx1H2=dlaH2)
dbv=derivdbdxdv(newtht=theta,cen1=cen1,cen2=cen2,X1=X1,X2=X2,H1=H1,H2=H2,dx1H1=dvH1,dx1H2=dvH2)
Dbx1x2=rbind(dbla,dbv)
comp=D2bnthtderivdv(newtht=theta,X1=X1,X2=X2,H1=H1,H2=H2,cen1=cen1,cen2=cen2)
D2b=comp$D2B;Dbtht=comp$DBtht
Hes1=cbind(t(Dbtht),t(Dbx1x2))
Hes2=rbind(Hes0,Hes1)
Hes3=rbind(Dbtht,Dbx1x2,D2b)
Hes=cbind(Hes2,Hes3)
INVE=solve(-Hes)
SE=sqrt(diag(INVE))
list(vcov=INVE,stderr=SE)
}


optimweibdv<-function(thet,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2)
ui0=matrix(0,4,length(thet));ci0=rep(0,4)
ui0[1,1]<-c(-0.5);ui0[1,2]<-c(0.5);ui0[1,3]<-c(-1)
ui0[2,1]<-c(0.5);ui0[2,2]<-c(-0.5);ui0[2,3]<-c(-1)
ui0[3,4]<-c(1);ui0[4,5]<-c(1)
thet[1:3]<-log(thet[1:3])
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikweibdv,grad=.fdLlikweibdv,
xx=xx,X1=X1,X2=X2,ui=ui0,ci=ci0)))
newtht=lnewtht=fittr$par
newtht0=newtht[1:3]
newtht[1:3]<-exp(newtht0)
newtht0=newtht
see=SEofweibdv(newtht0,xx=xx,X1=X1,X2=X2)
INVE=see$vcov
SE=see$stderr
if(any(diag(INVE)<0)){
warning("Check the approperiatness of the model.")
warning("estimates might be at the boundary of parameter space.")
warning("SE of estimates is false or not correct.")}
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("theta1","theta2","Row","scale","shape",colnames(X1))
if(any(is.na(SE))){
warning("Check the approperiatness of the model.")
warning("estimates might be at the boundary of parameter space.")}
aic=2*fittr$value+2*length(thet)
bic=2*fittr$value+log(2*nrow(xx))*length(thet)
res=list(coefficients=lnewtht[6:length(thet)],frailparest=c(theta1=newtht[1],theta2=newtht[2],Row=newtht[3]),
logfrailparest=lnewtht,AIC=aic,BIC=bic,
basehazpar=c(scale=lnewtht[4],shape=lnewtht[5]),vcov=INVE,stderr=SE,
loglik=(-fittr$value))
res
}



######
########################different variance without covariates

.Llikweibdvnc= function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
e1 = theta[1];e2 = theta[2];e3 = theta[3];la = abs(theta[4]);v = abs(theta[5])
H1=la*t1^v;H2=la*t2^v
logli=sum(cen1*(log(la)+log(v)+(v-1)*log(t1))+cen2*(log(la)+log(v)+(v-1)*log(t2))+
((-((1/exp(e1))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e1)*H1)-((1/exp(e2))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e2)*H2)-
(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2)))*log(1+exp(e1)*H1+exp(e2)*H2))+cen1*cen2*(-log(1+exp(e1)*H1)-log(1+exp(e2)*H2)-2*log(1+exp(e1)*H1+exp(e2)*H2)+
log(((exp(e3)+((exp(e1))^(1/2)*(exp(e2))^(1/2)))*exp(e3)*(1+exp(e1)*H1)*(1+exp(e2)*H2)+((((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e1)*H1)+
(((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e2)*H2))*(1+exp(e1)*H1+exp(e2)*H2)+
(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*((1+exp(e1)*H1+exp(e2)*H2))^2)))+
cen1*(1-cen2)*(-log(1+exp(e1)*H1)-log(1+exp(e1)*H1+exp(e2)*H2)+
log(((1+exp(e1)*H1)+(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1+exp(e2)*H2)-(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)))))+
(1-cen1)*cen2*(-log(1+exp(e2)*H2)-log(1+exp(e1)*H1+exp(e2)*H2)+
log(((1+exp(e2)*H2)+(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*(1+exp(e1)*H1)-(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)))))))
-logli
}

.fdLlikweibdvnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
e1 = theta[1];e2 = theta[2];e3 = theta[3];la = abs(theta[4]);v = abs(theta[5])
H1<-H2<-NULL
llkexprss <- expression(cen1*(log(la)+log(v)+(v-1)*log(t1))+cen2*(log(la)+log(v)+(v-1)*log(t2))+
(-((1/exp(e1))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e1)*la*t1^v)-
((1/exp(e2))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e2)*la*t2^v)-
(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2)))*log(1+exp(e1)*la*t1^v+exp(e2)*la*t2^v))+
cen1*cen2*(-log(1+exp(e1)*la*t1^v)-log(1+exp(e2)*la*t2^v)-2*log(1+exp(e1)*la*t1^v+exp(e2)*la*t2^v)+
log(((exp(e3)+((exp(e1))^(1/2)*(exp(e2))^(1/2)))*exp(e3)*(1+exp(e1)*la*t1^v)*(1+exp(e2)*la*t2^v)+
((((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e1)*la*t1^v)+
(((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e2)*la*t2^v))*(1+exp(e1)*la*t1^v+exp(e2)*la*t2^v)+
(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*((1+exp(e1)*la*t1^v+exp(e2)*la*t2^v))^2)))+
cen1*(1-cen2)*(-log(1+exp(e1)*la*t1^v)-log(1+exp(e1)*la*t1^v+exp(e2)*la*t2^v)+
log(((1+exp(e1)*la*t1^v)+(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1+exp(e2)*la*t2^v)-(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)))))+
(1-cen1)*cen2*(-log(1+exp(e2)*la*t2^v)-log(1+exp(e1)*la*t1^v+exp(e2)*la*t2^v)+
log(((1+exp(e2)*la*t2^v)+(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*(1+exp(e1)*la*t1^v)-(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))))))
dxy <- deriv(llkexprss, c("e1","e2", "e3","la","v"))
sr=eval(dxy)
sr1<-attributes(sr)
gd=c(colSums(sr1$gradient))
-gd
}


.sdLlikweibdvnc = function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a1 = (theta[1]);a2 = (theta[2]);R = (theta[3]);la = abs(theta[4]);v = abs(theta[5])
llkexprss <- expression(((cen1*(log(la)+log(v)+(v-1)*log(t1))+cen2*(log(la)+log(v)+(v-1)*log(t2))-
((1/a1)-(R/(a1^(1/2)*a2^(1/2))))*log(1+a1*la*t1^v)-((1/a2)-(R/(a1^(1/2)*a2^(1/2))))*log(1+a2*la*t2^v)-
(R/(a1^(1/2)*a2^(1/2)))*log(1+a1*la*t1^v+a2*la*t2^v))+
cen1*cen2*(log(((R+(a1^(1/2)*a2^(1/2)))*R)*(1/(1+a1*la*t1^v+a2*la*t2^v)^2)+
(1/(1+a1*la*t1^v+a2*la*t2^v))*((R*(a2^(1/2)/a1^(1/2))-R^2)*(1/(1+a1*la*t1^v))+
(R*(a1^(1/2)/a2^(1/2))-R^2)*(1/(1+a2*la*t2^v)))+
((1/(1+a1*la*t1^v))*(1/(1+a2*la*t2^v)))*((1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R))))+
cen1*(1-cen2)*(log((a1^(1/2)/a2^(1/2))*R*(1/(1+a1*la*t1^v+a2*la*t2^v))+ (1-(a1^(1/2)/a2^(1/2))*R)*(1/(1+a1*la*t1^v))))+
(1-cen1)*cen2*(log((a2^(1/2)/a1^(1/2))*R*(1/(1+a1*la*t1^v+a2*la*t2^v))+ (1-(a2^(1/2)/a1^(1/2))*R)*(1/(1+a2*la*t2^v))))))
dxy <- deriv3(llkexprss, c("a1","a2", "R","la","v"),hessian = TRUE)
sr=eval(dxy)
sr1<-attributes(sr)
Hes=colSums(sr1$hessian)
INVE=solve(-Hes)
SE=sqrt(diag(INVE))
list(vcov=INVE,stderr=SE)
}


optimweibdvnc<-function(thet,xx){
xx=as.matrix(xx)
ui0=matrix(0,4,length(thet));ci0=rep(0,4)
ui0[1,1]<-c(-0.5);ui0[1,2]<-c(0.5);ui0[1,3]<-c(-1)
ui0[2,1]<-c(0.5);ui0[2,2]<-c(-0.5);ui0[2,3]<-c(-1)
ui0[3,4]<-c(1);ui0[4,5]<-c(1)
thet[1:3]<-log(thet[1:3])
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikweibdvnc,grad=.fdLlikweibdvnc,
xx=xx,ui=ui0,ci=ci0)))
newtht=lnewtht=fittr$par
newtht0=newtht[1:3]
newtht[1:3]<-exp(newtht0)
newtht0=newtht
rbb=feasibleregforR(newtht0)
if((rbb-newtht0[3])<0.0001){newtht0[3]=newtht0[3]*0.99}
if(newtht0[3]>=0.999){newtht0[3]<-(0.999)}
OIM=.sdLlikweibdvnc(theta=newtht0,xx=xx)
INVE=OIM$vcov
SE=OIM$stderr
INVE=as.matrix(INVE)
aic=2*fittr$value+2*length(thet)
bic=2*fittr$value+log(2*nrow(xx))*length(thet)
colnames(INVE) <- rownames(INVE) <- c("theta1","theta2","Row","scale","shape")
res=list(frailparest=c(theta1=newtht[1],theta2=newtht[2],Row=newtht[3]),logfrailparest=lnewtht,
basehazpar=c(scale=lnewtht[4],shape=lnewtht[5]),vcov=INVE,stderr=SE,AIC=aic,BIC=bic,
loglik=(-fittr$value))
res
}


########################different variance with covariates
########
.Llikexpdv= function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
e1 = (theta[1]);e2 = (theta[2]);e3 = (theta[3])
la = abs(theta[4]);bet = theta[5:length(theta)]
EBX1=exp(c(X1%*%bet));EBX2=exp(c(X2%*%bet))
H01=la*t1;H02=la*t2;H1=H01*EBX1;H2=H02*EBX2
logli=sum(cen1*(log(la)+c(X1%*%bet))+cen2*(log(la)+c(X2%*%bet))+
((-((1/exp(e1))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e1)*H1)-((1/exp(e2))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e2)*H2)-
(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2)))*log(1+exp(e1)*H1+exp(e2)*H2))+cen1*cen2*(-log(1+exp(e1)*H1)-log(1+exp(e2)*H2)-2*log(1+exp(e1)*H1+exp(e2)*H2)+
log(((exp(e3)+((exp(e1))^(1/2)*(exp(e2))^(1/2)))*exp(e3)*(1+exp(e1)*H1)*(1+exp(e2)*H2)+((((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e1)*H1)+
(((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e2)*H2))*(1+exp(e1)*H1+exp(e2)*H2)+
(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*((1+exp(e1)*H1+exp(e2)*H2))^2)))+
cen1*(1-cen2)*(-log(1+exp(e1)*H1)-log(1+exp(e1)*H1+exp(e2)*H2)+
log(((1+exp(e1)*H1)+(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1+exp(e2)*H2)-(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)))))+
(1-cen1)*cen2*(-log(1+exp(e2)*H2)-log(1+exp(e1)*H1+exp(e2)*H2)+
log(((1+exp(e2)*H2)+(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*(1+exp(e1)*H1)-(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)))))))
-logli
}

.fdLlikexpdv= function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
e1 = theta[1];e2 = theta[2];e3 = theta[3];la = abs(theta[4]);bet = theta[5:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
H1<-H2<-NULL
llkexprss <- expression((cen1*(log(la))+cen2*(log(la))+
((-((1/exp(e1))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e1)*la*t1*eb1)-((1/exp(e2))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e2)*la*t2*eb2)-
(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2)))*log(1+exp(e1)*la*t1*eb1+exp(e2)*la*t2*eb2))+cen1*cen2*(-log(1+exp(e1)*la*t1*eb1)-log(1+exp(e2)*la*t2*eb2)-2*log(1+exp(e1)*la*t1*eb1+exp(e2)*la*t2*eb2)+
log(((exp(e3)+((exp(e1))^(1/2)*(exp(e2))^(1/2)))*exp(e3)*(1+exp(e1)*la*t1*eb1)*(1+exp(e2)*la*t2*eb2)+((((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e1)*la*t1*eb1)+
(((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e2)*la*t2*eb2))*(1+exp(e1)*la*t1*eb1+exp(e2)*la*t2*eb2)+
(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*((1+exp(e1)*la*t1*eb1+exp(e2)*la*t2*eb2))^2)))+
cen1*(1-cen2)*(-log(1+exp(e1)*la*t1*eb1)-log(1+exp(e1)*la*t1*eb1+exp(e2)*la*t2*eb2)+
log(((1+exp(e1)*la*t1*eb1)+(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1+exp(e2)*la*t2*eb2)-(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)))))+
(1-cen1)*cen2*(-log(1+exp(e2)*la*t2*eb2)-log(1+exp(e1)*la*t1*eb1+exp(e2)*la*t2*eb2)+
log(((1+exp(e2)*la*t2*eb2)+(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*(1+exp(e1)*la*t1*eb1)-(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))))))))
dxy <- deriv(llkexprss, c("e1","e2", "e3","la"))
sr=eval(dxy)
sr1<-attributes(sr)
gdpart1=c(colSums(sr1$gradient))
EBX1=eb1;EBX2=eb2
dbEBX1=c(EBX1)*X1;dbEBX2=c(EBX2)*X2
H01=la*t1;H02=la*t2;H1=H01*EBX1;H2=H02*EBX2
H1=H01*EBX1;H2=H02*EBX2
dbH1=c(H01)*dbEBX1;dbH2=c(H02)*dbEBX2
a1=exp(e1);a2=exp(e2);R=exp(e3)
P11=( ((R+(a1^(1/2)*a2^(1/2)))*R)*(1/(1+a1*H1+a2*H2)^2)+
(1/(1+a1*H1+a2*H2))*((R*(a2^(1/2)/a1^(1/2))-R^2)*(1/(1+a1*H1))+(R*(a1^(1/2)/a2^(1/2))-R^2)*(1/(1+a2*H2)))+
((1/(1+a1*H1))*(1/(1+a2*H2)))*((1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R)) )
dbP11=(((R+(a1^(1/2)*a2^(1/2)))*R)*(-2/(1+a1*H1+a2*H2)^3)*(a1*dbH1+a2*dbH2)+
((-1/(1+a1*H1+a2*H2)^2)*((R*(a2^(1/2)/a1^(1/2))-R^2)*(1/(1+a1*H1))+(R*(a1^(1/2)/a2^(1/2))-R^2)*(1/(1+a2*H2))))*(a1*dbH1+a2*dbH2)+
(1/(1+a1*H1+a2*H2))*((R*(a2^(1/2)/a1^(1/2))-R^2)*(-1/(1+a1*H1)^2)*(a1*dbH1)+(R*(a1^(1/2)/a2^(1/2))-R^2)*(-1/(1+a2*H2)^2)*(a2*dbH2))+
( (-1/(1+a1*H1)^2)*(1/(1+a2*H2))*(a1*dbH1)+(1/(1+a1*H1))*(-1/(1+a2*H2)^2)*(a2*dbH2) )*((1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R) ) )
P10=((a1^(1/2)/a2^(1/2))*R*(1/(1+a1*H1+a2*H2))+ (1-(a1^(1/2)/a2^(1/2))*R)*(1/(1+a1*H1)))
dbP10=((a1^(1/2)/a2^(1/2))*R*(-1/(1+a1*H1+a2*H2)^2)*(a1*dbH1+a2*dbH2)+(1-(a1^(1/2)/a2^(1/2))*R)*(-1/(1+a1*H1)^2)*(a1*dbH1))
P01=((a2^(1/2)/a1^(1/2))*R*(1/(1+a1*H1+a2*H2))+ (1-(a2^(1/2)/a1^(1/2))*R)*(1/(1+a2*H2)))
dbP01=((a2^(1/2)/a1^(1/2))*R*(-1/(1+a1*H1+a2*H2)^2)*(a1*dbH1+a2*dbH2)+(1-(a2^(1/2)/a1^(1/2))*R)*(-1/(1+a2*H2)^2)*(a2*dbH2))
db=c(colSums(cen1*X1+cen2*X2+(-c(((1/a1)-(R/(a1^(1/2)*a2^(1/2))))*(a1/(1+a1*H1)))*dbH1-
c(((1/a2)-(R/(a1^(1/2)*a2^(1/2))))*(a2/(1+a2*H2)))*dbH2-(R/(a1^(1/2)*a2^(1/2)))*(1/(1+a1*H1+a2*H2))*(a1*dbH1+a2*dbH2))+
((cen1*cen2)/P11)*dbP11+((cen1*(1-cen2))/P10)*dbP10+(((1-cen1)*cen2)/P01)*dbP01))
gd=c(c(gdpart1),c(db))
-gd
}

SEofexpdv = function (theta,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a1 = abs(theta[1]);a2 = abs(theta[2]);R = abs(theta[3]);la = abs(theta[4]);bet = theta[5:length(theta)]
eb1=exp(c(X1%*%bet));eb2=exp(c(X2%*%bet))
H1<-H2<-NULL
v<-NULL
llkexprss <- expression(((cen1*(log(la))+cen2*(log(la))-
((1/a1)-(R/(a1^(1/2)*a2^(1/2))))*log(1+a1*la*t1*eb1)-((1/a2)-(R/(a1^(1/2)*a2^(1/2))))*log(1+a2*la*t2*eb2)-
(R/(a1^(1/2)*a2^(1/2)))*log(1+a1*la*t1*eb1+a2*la*t2*eb2))+
cen1*cen2*(log(((R+(a1^(1/2)*a2^(1/2)))*R)*(1/(1+a1*la*t1*eb1+a2*la*t2*eb2)^2)+
(1/(1+a1*la*t1*eb1+a2*la*t2*eb2))*((R*(a2^(1/2)/a1^(1/2))-R^2)*(1/(1+a1*la*t1*eb1))+
(R*(a1^(1/2)/a2^(1/2))-R^2)*(1/(1+a2*la*t2*eb2)))+
((1/(1+a1*la*t1*eb1))*(1/(1+a2*la*t2*eb2)))*((1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R))))+
cen1*(1-cen2)*(log((a1^(1/2)/a2^(1/2))*R*(1/(1+a1*la*t1*eb1+a2*la*t2*eb2))+ (1-(a1^(1/2)/a2^(1/2))*R)*(1/(1+a1*la*t1*eb1))))+
(1-cen1)*cen2*(log((a2^(1/2)/a1^(1/2))*R*(1/(1+a1*la*t1*eb1+a2*la*t2*eb2))+ (1-(a2^(1/2)/a1^(1/2))*R)*(1/(1+a2*la*t2*eb2))))))
dxy <- deriv3(llkexprss, c("a1","a2", "R","la"),hessian = TRUE)
sr=eval(dxy)
sr1<-attributes(sr)
Hes0=colSums(sr1$hessian)
H01=la*t1;H02=la*t2;H1=H01*eb1;H2=H02*eb2
dlaH01=t1;dlaH02=t2
dlaH1=dlaH01*eb1;dlaH2=dlaH02*eb2
dbla=derivdbdxdv(newtht=theta,cen1=cen1,cen2=cen2,X1=X1,X2=X2,H1=H1,H2=H2,dx1H1=dlaH1,dx1H2=dlaH2)
dbla=matrix(c(dbla),1,length(bet))
comp=D2bnthtderivdv(newtht=theta,X1=X1,X2=X2,H1=H1,H2=H2,cen1=cen1,cen2=cen2)
D2b=comp$D2B;Dbtht=comp$DBtht
Hes1=cbind(t(Dbtht),t(dbla))
Hes2=rbind(Hes0,Hes1)
Hes3=rbind(Dbtht,dbla,D2b)
Hes=cbind(Hes2,Hes3)
INVE=solve(-Hes)
SE=sqrt(diag(INVE))
list(vcov=INVE,stderr=SE)
}



optimexpdv<-function(thet,xx,X1,X2){
xx=as.matrix(xx);X1=as.matrix(X1);X2=as.matrix(X2);New.diff=1
ui0=matrix(0,3,length(thet));ci0=rep(0,3)
ui0[1,1]<-c(-0.5);ui0[1,2]<-c(0.5);ui0[1,3]<-c(-1)
ui0[2,1]<-c(0.5);ui0[2,2]<-c(-0.5);ui0[2,3]<-c(-1)
ui0[3,4]<-c(1)
thet[1:3]<-log(thet[1:3])
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikexpdv,grad=.fdLlikexpdv,
xx=xx,X1=X1,X2=X2,ui=ui0,ci=ci0)))
newtht=lnewtht=fittr$par
newtht0=newtht[1:3]
newtht[1:3]<-exp(newtht0)
newtht0=newtht
rbb=feasibleregforR(newtht0)
if((rbb-newtht0[3])<0.0001){newtht0[3]=newtht0[3]*0.99}
if(newtht0[3]>=0.999){newtht0[3]<-(0.999)}
see=SEofexpdv(theta=newtht0,xx=xx,X1=X1,X2=X2)
INVE=see$vcov
SE=see$stderr
INVE=as.matrix(INVE)
colnames(INVE) <- rownames(INVE) <- c("theta1","theta2","Row","scale",colnames(X1))
if(any(is.na(SE))){
warning("Check the approperiatness of the model.")
warning("estimates might be at the boundary of parameter space.")}
aic=2*fittr$value+2*length(thet)
bic=2*fittr$value+log(2*nrow(xx))*length(thet)
res=list(coefficients=newtht[5:length(thet)],frailparest=c(theta1=newtht[1],theta2=newtht[2],Row=newtht[3]),
logfrailparest=lnewtht,AIC=aic,BIC=bic,
basehazpar=c(scale=lnewtht[4]),vcov=INVE,stderr=SE,loglik=(-fittr$value))
res
}



######

########################different variance without covariates
.Llikexpdvnc= function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
e1 = (theta[1]);e2 = (theta[2]);e3 = (theta[3]);la = abs(theta[4])
H1=la*t1;H2=la*t2
logli=sum(cen1*(log(la))+cen2*(log(la))+
((-((1/exp(e1))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e1)*H1)-((1/exp(e2))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e2)*H2)-
(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2)))*log(1+exp(e1)*H1+exp(e2)*H2))+cen1*cen2*(-log(1+exp(e1)*H1)-log(1+exp(e2)*H2)-2*log(1+exp(e1)*H1+exp(e2)*H2)+
log(((exp(e3)+((exp(e1))^(1/2)*(exp(e2))^(1/2)))*exp(e3)*(1+exp(e1)*H1)*(1+exp(e2)*H2)+((((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e1)*H1)+
(((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e2)*H2))*(1+exp(e1)*H1+exp(e2)*H2)+
(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*((1+exp(e1)*H1+exp(e2)*H2))^2)))+
cen1*(1-cen2)*(-log(1+exp(e1)*H1)-log(1+exp(e1)*H1+exp(e2)*H2)+
log(((1+exp(e1)*H1)+(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1+exp(e2)*H2)-(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)))))+
(1-cen1)*cen2*(-log(1+exp(e2)*H2)-log(1+exp(e1)*H1+exp(e2)*H2)+
log(((1+exp(e2)*H2)+(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*(1+exp(e1)*H1)-(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)))))))
-logli
}

.fdLlikexpdvnc= function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
e1 = (theta[1]);e2 = (theta[2]);e3 = (theta[3]);la = abs(theta[4])
H1<-H2<-eb1<-eb2<-NULL
llkexprss <- expression((cen1*(log(la))+cen2*(log(la))+
((-((1/exp(e1))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e1)*la*t1)-((1/exp(e2))-(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2))))*log(1+exp(e2)*la*t2)-
(exp(e3)/((exp(e1))^(1/2)*(exp(e2))^(1/2)))*log(1+exp(e1)*la*t1+exp(e2)*la*t2))+cen1*cen2*(-log(1+exp(e1)*la*t1)-log(1+exp(e2)*la*t2)-2*log(1+exp(e1)*la*t1+exp(e2)*la*t2)+
log(((exp(e3)+((exp(e1))^(1/2)*(exp(e2))^(1/2)))*exp(e3)*(1+exp(e1)*la*t1)*(1+exp(e2)*la*t2)+((((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e1)*la*t1)+
(((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3)-(exp(e3))^2)*(1+exp(e2)*la*t2))*(1+exp(e1)*la*t1+exp(e2)*la*t2)+
(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*((1+exp(e1)*la*t1+exp(e2)*la*t2))^2)))+
cen1*(1-cen2)*(-log(1+exp(e1)*la*t1)-log(1+exp(e1)*la*t1+exp(e2)*la*t2)+
log(((1+exp(e1)*la*t1)+(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3))*(1+exp(e2)*la*t2)-(1-((exp(e1))^(1/2)/(exp(e2))^(1/2))*exp(e3)))))+
(1-cen1)*cen2*(-log(1+exp(e2)*la*t2)-log(1+exp(e1)*la*t1+exp(e2)*la*t2)+
log(((1+exp(e2)*la*t2)+(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))*(1+exp(e1)*la*t1)-(1-((exp(e2))^(1/2)/(exp(e1))^(1/2))*exp(e3))))))))
dxy <- deriv(llkexprss, c("e1","e2", "e3","la"))
sr=eval(dxy)
sr1<-attributes(sr)
gd=c(colSums(sr1$gradient))
-gd
}

.sdLlikexpdvnc= function (theta,xx){
xx=as.matrix(xx);t1=xx[,1];t2=xx[,2];cen1=xx[,3];cen2=xx[,4]
a1 = (theta[1]);a2 = (theta[2]);R = (theta[3]);la = abs(theta[4])
H1<-H2<-eb1<-eb2<-NULL
llkexprss <- expression(((cen1*(log(la))+cen2*(log(la))-
((1/a1)-(R/(a1^(1/2)*a2^(1/2))))*log(1+a1*la*t1)-((1/a2)-(R/(a1^(1/2)*a2^(1/2))))*log(1+a2*la*t2)-
(R/(a1^(1/2)*a2^(1/2)))*log(1+a1*la*t1+a2*la*t2))+
cen1*cen2*(log(((R+(a1^(1/2)*a2^(1/2)))*R)*(1/(1+a1*la*t1+a2*la*t2)^2)+
(1/(1+a1*la*t1+a2*la*t2))*((R*(a2^(1/2)/a1^(1/2))-R^2)*(1/(1+a1*la*t1))+
(R*(a1^(1/2)/a2^(1/2))-R^2)*(1/(1+a2*la*t2)))+
((1/(1+a1*la*t1))*(1/(1+a2*la*t2)))*((1-(a1^(1/2)/a2^(1/2))*R)*(1-(a2^(1/2)/a1^(1/2))*R))))+
cen1*(1-cen2)*(log((a1^(1/2)/a2^(1/2))*R*(1/(1+a1*la*t1+a2*la*t2))+ (1-(a1^(1/2)/a2^(1/2))*R)*(1/(1+a1*la*t1))))+
(1-cen1)*cen2*(log((a2^(1/2)/a1^(1/2))*R*(1/(1+a1*la*t1+a2*la*t2))+ (1-(a2^(1/2)/a1^(1/2))*R)*(1/(1+a2*la*t2))))))
dxy <- deriv3(llkexprss, c("a1","a2", "R","la"),hessian = TRUE)
sr=eval(dxy)
sr1<-attributes(sr)
Hes=colSums(sr1$hessian)
INVE=solve(-Hes)
SE=sqrt(diag(INVE))
list(vcov=INVE,stderr=SE)
}

optimexpdvnc<-function(thet,xx){
xx=as.matrix(xx)
ui0=matrix(0,3,length(thet));ci0=rep(0,3)
ui0[1,1]<-c(-0.5);ui0[1,2]<-c(0.5);ui0[1,3]<-c(-1)
ui0[2,1]<-c(0.5);ui0[2,2]<-c(-0.5);ui0[2,3]<-c(-1)
ui0[3,4]<-c(1)
thet[1:3]<-log(thet[1:3])
fittr=do.call(constrOptim, args=c(list(theta=thet,f=.Llikexpdvnc,grad=.fdLlikexpdvnc,
xx=xx,ui=ui0,ci=ci0)))
newtht=lnewtht=fittr$par
newtht0=newtht[1:3]
newtht[1:3]<-exp(newtht0)
OIM=.sdLlikexpdvnc(theta=newtht,xx=xx)
INVE=OIM$vcov
SE=OIM$stderr
INVE=as.matrix(INVE)
aic=2*fittr$value+2*length(thet)
bic=2*fittr$value+log(2*nrow(xx))*length(thet)
colnames(INVE) <- rownames(INVE) <- c("theta1","theta2","Row","scale")
res=list(frailparest=c(theta1=newtht[1],theta2=newtht[2],Row=newtht[3]),logfrailparest=lnewtht,
basehazpar=c(scale=lnewtht[4]),vcov=INVE,stderr=SE,loglik=(-fittr$value),AIC=aic,BIC=bic)
res
}

fitbcdv.gammapar<-function(X,Y,initfrailp,inithazp,initbeta,haz){
time=Y[, 1];censor=Y[, 2]
data.n1 <- length(time);data.n <-data.n1/2#### data.n is the number of pairs
ar=array(1:data.n)
i1<-2*ar-1;i2<-2*ar
cen1=censor[i1];cen2=censor[i2]
t1=time[i1];t2=time[i2]
xx=matrix(c(t1,t2,cen1,cen2),length(cen1),4)
n_covar=ncol(X)
optimfunc<-NULL
if(n_covar>0){
thet=c(initfrailp,inithazp,initbeta)
X1=X[i1,];X2=X[i2,]
if(n_covar==1){
X2=matrix(c(X2),data.n,1);X1=matrix(c(X1),data.n,1)}
X1=as.matrix(X1);X2=as.matrix(X2)
colnames(X1)<-colnames(X2)<-colnames(X)
if (haz== c("weibull")) {optimfunc<-optimweibdv}
if (haz== c("gompertz")) {optimfunc<-optimgomperzdv}
if (haz== c("exponential")) {optimfunc<-optimexpdv}
res<-optimfunc(thet,xx,X1,X2)}
if(n_covar==0){
thet=c(initfrailp,inithazp)
if (haz== c("weibull")) {optimfunc<-optimweibdvnc}
if (haz== c("gompertz")) {optimfunc<-optimgomperzdvnc}
if (haz== c("exponential")) {optimfunc<-optimexpdvnc}
res<-optimfunc(thet,xx)}
res$time <- time
res$censor <- censor
res$call <- match.call()
class(res) <- c("bcfrailpar")
res
}


#

