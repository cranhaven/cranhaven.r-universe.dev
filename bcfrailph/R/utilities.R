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
coverp <- function(x,L,R) ifelse(x>=L & x<=R,1,0)
zq=abs(qnorm(0.025,mean=0,sd=1))# Z alpha/2
#
# log gamma function i.e., log(gamma(1/a+x)) for x=0,1,2,..
funloggamm <- function(x,a){
kr=sum(log(1/a+array(0:x)))-log(1/a+x)
kr}
#
# first derivative of log gamma function
funfdloggamm <- function(x,a){
kr=sum(-(1/a^2)/(1/a+array(0:x)))+(1/a^2)/(1/a+x)
kr}
#
# second derivative of log gamma function
funsdloggamm <- function(x,a){
kr=(sum((2/a^3)/(1/a+array(0:x))+(1/a^2)*(1/a^2)/(1/a+array(0:x))^2)+
(-2/a^3)/(1/a+x)-(1/a^2)*(1/a^2)/(1/a+x)^2 )
kr}
#
indicmin<-function(i1,i2,time){
i3=i1
for(k in 1:(length(time)/2)){
i3[k]=i1[k]
if(time[i1[k]]>time[i2[k]]){i3[k]=i2[k]}}
i3}
#
funbf <- function(RIU,u){
re0=RIU[u]
re0}
#
risskf <- function(RIU0,indic1,indic2){
re0=RIU0[indic1]+RIU0[indic2]
re0}
#
interacmat <- function(X,u){X*u}
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
newmoppp <- function(yy,daDI,drDI){
WI=matrix(c(yy[1],yy[2],yy[2],yy[3]),2,2)
HIda=(((WI%*%daDI%*%WI)%*%daDI))
HIdar=(((WI%*%drDI%*%WI)%*%daDI))
HIdr=(((t(WI%*%drDI)%*%(WI%*%drDI))))
c(0.5*sum(diag(HIda)),0.5*sum(diag(HIdar)),0.5*sum(diag(HIdr)))
}
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
rissksetoppi <- function(uniq_tim,x,y){
cx<-ifelse(uniq_tim[1]<=x,1,0)
sum(cx*y)}
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
rbivnorm<-function(psize,ssq,r){
w1=rnorm(psize,mean=0,sd=sqrt(ssq))
w2=rnorm(psize,mean=r*w1,sd=sqrt((1-r^2)*ssq))
cbind(w1,w2)}
#
#
weigtevent <- function(uniind,x,we,cen){
cde=ifelse(uniind[1]==x,1,0)
cdr=c(cen)*cde*array(1:length(x))
cdeb=as.vector(cdr[cdr>0])
if(length(cdeb)==0){re=c(0)}
if(length(cdeb)>0){re=c(sum(we[cdeb]))}
re}
#
sortingfunc <- function(time,censor,weights){
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
uniind=array(1:length(uniq_tim))
if(length(weights)!=0){
Rev <- apply(as.array(uniind),1,weigtevent,x=ind.haz,we=weights,cen=censor)
n_eve <-as.vector(t(Rev))}
if(length(weights)==0){
Rev <- apply(as.array(uniind),1,tofevent,x=ind.haz,cen=censor)
n_eve <-as.vector(t(Rev))
weights<-rep(1,length(time))}
Rev <- apply(as.array(uniind),1,tofevent,x=ind.haz,cen=rep(1,length(time)))
t_n_eve <-as.vector(t(Rev))
csd3=cumsum(t_n_eve)
csd0=c(1+csd3)
csd2=c(1,csd0[-length(csd0)])
eventtau=cbind(csd2,csd3)
n_evep=n_eve
n_evep[n_evep<=0]<-1
list(uniq_tim=uniq_tim,ind.haz=ind.haz,n_eve=n_eve,n_evep=n_evep,
t_n_eve=t_n_eve,eventtau=eventtau,weights=weights,indx=indx,t=time)}
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
###################################################################
############### Utilities for Bivariate gamma frailty fit #####
###################################################################
#
################SE for gamma fit, derived from log of mariginal likelihood ############
.SEbcfrailcv=function(bet,newtht,n_eve,etime,h0,censor,time,X,H){
beto=bet;beto<-as.vector(beto);n_cov_coef=length(beto);bet=matrix(c(beto),n_cov_coef,1)
n_eve=n_eve;n_eve<-as.vector(n_eve);etime=etime;etime<-as.vector(etime);censor=censor;censor<-as.vector(censor)
h0=h0;h0<-as.vector(h0);time=time;time<-as.vector(time);newtht=newtht;newtht<-as.vector(newtht)
data.n1= length(time);data.n=data.n1/2
a = (newtht[1]);R = (newtht[2])
X <- X;X <-matrix(X,data.n1,n_cov_coef);g0<-c(exp(X%*%bet))
HH=H;HH<-as.vector(HH)
indic1<-2*array(1:data.n)-1;indic2<-2*array(1:data.n)
k0<-c(R*a^(-1));k<-c((1-R)*a^(-1))
c1<-c(R^2+R*a);c2<-c(R-R^2);c3<-c2;c4<-c(1-2*R+R^2)
dak0<-c(-R*a^(-2));dRk0<-c(a^(-1))
dak<-c(-(1-R)*a^(-2));dRk<-c(-a^(-1))
H1=HH[indic1];H2=HH[indic2];time1=time[indic1];time2=time[indic2]
g01=g0[indic1];g02=g0[indic2];cen1=censor[indic1];cen2=censor[indic2]
di=(cen1+cen2)
X1=X[indic1,];X2=X[indic2,]
X1<- matrix(X1,data.n,n_cov_coef);X2<- matrix(X2,data.n,n_cov_coef)
AA=(1+a*(H1+H2));AA1=(1+a*H1);AA2=(1+a*H2)
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
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=RIE001)
IMX1<-matrix(resinteracmatx,data.n,n_cov_coef*ncol(RIE001))
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=RIE001)
IMX12<-matrix(resinteracmatx,data.n,n_cov_coef*ncol(RIE001))
resinteracmatx<-apply(as.matrix(X2),2,interacmat,u=RIE002)
IMX2<-matrix(resinteracmatx,data.n,n_cov_coef*ncol(RIE002))
resinteracmatx<-apply(as.matrix(X1),2,interacmat,u=RIE002)
IMX21<-matrix(resinteracmatx,data.n,n_cov_coef*ncol(RIE002))
Dhp00i=c(-c(k+cen1)*(1/AA1))*(c(a)*RIE001)-c(c(k+cen2)*(1/AA2))*(c(a)*RIE002)-c(c(k0+di)*c(1/AA))*(c(a)*RIE001+c(a)*RIE002)
Dbp00i=(c(-c(k+cen1)*(1/AA1))*(c(a*H1)*X1)+c(-c(k+cen2)*(1/AA2))*(c(a*H2)*X2)+c(-c(k0+di)*(1/AA))*(c(a*H1)*X1+c(a*H2)*X2))
Dap00i= -c(dak)*log(AA1)-c(k+cen1)*(H1/AA1)-c(dak)*log(AA2)-c(k+cen2)*(H2/AA2)-c(dak0)*log(AA)-c(k0+di)*((H1+H2)/AA)
DRp00i=-c(dRk)*log(AA1)-c(dRk)*log(AA2)-c(dRk0)*log(AA)
D2hp00=(t((c(k+cen1)*c(1/AA1^2)*c(a)*RIE001))%*%(c(a)*RIE001)+ t((c(k+cen2)*c(1/AA2^2)*c(a)*RIE002))%*%(c(a)*RIE002)+
t((c(k0+di)*c(1/AA^2)*(c(a)*RIE001+c(a)*RIE002)))%*%(c(a)*RIE001+c(a)*RIE002))#####
Dhbp00i= (matrix(c(colSums(c(-c(k+cen1)*c(1/AA1))*(c(a)*IMX1))),ncol(RIE001),n_cov_coef)+ t(c(a)*RIE001)%*%( c(c(k+cen1)*c(1/AA1^2))*(c(a*H1)*X1))+
matrix(c(colSums(c(-c(k+cen2)*c(1/AA2))*(c(a)*IMX2))),ncol(RIE002),n_cov_coef)+ t(c(a)*RIE002)%*%(c(c(k+cen2)*c(1/AA2^2))*(c(a*H2)*X2))+
matrix(c(colSums(-c(c(k0+di)*c(1/AA))*(c(a)*IMX1+c(a)*IMX2))),ncol(RIE001),n_cov_coef)+
t(c(a)*RIE001+c(a)*RIE002 )%*%( c(c(k0+di)*c(1/AA^2))*(c(a*H1)*X1+c(a*H2)*X2)))####
Dhap00i=colSums(c(-c(dak)*(1/AA1))*(c(a)*RIE001)-c(k+cen1)*((1/AA1)*(RIE001)-(H1/AA1^2)*(c(a)*RIE001))-
c(c(dak)*(1/AA2))*(c(a)*RIE002)-c(k+cen2)*((1/AA2)*(RIE002)-(H2/AA2^2)*(c(a)*RIE002))-
c(c(dak0)*c(1/AA))*(c(a)*RIE001+c(a)*RIE002)-c(k0+di)*(c(1/AA)*(RIE001+RIE002)-c((H1+H2)/AA^2)*(c(a)*RIE001+c(a)*RIE002)))
DhRp00i=colSums(c(-c(dRk)*(1/AA1))*(c(a)*RIE001)-c(c(dRk)*(1/AA2))*(c(a)*RIE002)-c(c(dRk0)*c(1/AA))*(c(a)*RIE001+c(a)*RIE002))#####
P11=( c1*AA1*AA2+c2*AA2*AA+c3*AA1*AA+c4*AA^(2))
DhlogP11=((cen1*cen2)/P11)*( c1*( c(AA2)*(c(a)*RIE001)+c(AA1)*(c(a)*RIE002))+
c2*(c(AA2)*(c(a)*RIE001+c(a)*RIE002)+ c(AA)*(c(a)*RIE002))+c3*(c(AA1)*(c(a)*RIE001+c(a)*RIE002)+ c(AA)*(c(a)*RIE001))+
c4*2*AA*(c(a)*RIE001+c(a)*RIE002))
DblogP11=((cen1*cen2)/P11)*(c(c1*a*H1*AA2)*X1+c(c1*AA1*a*H2)*X2+
c(c2*a*H2*AA)*X2+c(c2*AA2)*(a*H1*X1+a*H2*X2)+c(c3*a*H1*AA)*X1+c(c3*AA1)*(a*H1*X1+a*H2*X2)+c4*2*AA*(a*H1*X1+a*H2*X2))
dalogP11=((cen1*cen2)/P11)*( R*AA1*AA2+c1*(H1*AA2+AA1*H2)+c2*(H2*AA+AA2*(H1+H2))+c3*(H1*AA+AA1*(H1+H2))+2*c4*AA*(H1+H2))
dRlogP11=((cen1*cen2)/P11)*((2*R+a)*AA1*AA2+(1-2*R)*AA2*AA+(1-2*R)*AA1*AA+(-2+2*R)*AA^(2))
D2hlogP110=(t(c(c(cen1*cen2/P11)*c(2)*c1)*(c(a)*RIE001))%*%(c(a)*RIE002)+
t(c(c(cen1*cen2/P11)*c(2)*c2)*(c(a)*RIE002))%*%(c(a)*RIE001+c(a)*RIE002)+
t(c(c(cen1*cen2/P11)*c(2)*c3)*(c(a)*RIE001))%*%(c(a)*RIE001+c(a)*RIE002)+
t(c(c(cen1*cen2/P11)*c(2)*c4)*(c(a)*RIE001+c(a)*RIE002))%*%(c(a)*RIE001+c(a)*RIE002))
D2hlogP11=D2hlogP110-t(DhlogP11)%*%DhlogP11###############
DhblogP110=((cen1*cen2)/P11)*(c1*((c(a*a*H2)*IMX12)+c(AA2)*(c(a)*IMX1)+(c(a*a*H1)*IMX21)+c(AA1)*(c(a)*IMX2))+
c2*((c(a*a*H2)*IMX12+c(a*a*H2)*IMX2)+ c(AA2)*(c(a)*IMX1+c(a)*IMX2)+(c(a*a*H1)*IMX21+c(a*a*H2)*IMX2)+c(AA)*(c(a)*IMX2))+
c3*((c(a*a*H1)*IMX1+c(a*a*H1)*IMX21)+ c(AA1)*(c(a)*IMX1+c(a)*IMX2)+(c(a*a*H1)*IMX1+c(a*a*H2)*IMX12)+c(AA)*(c(a)*IMX1))+
c4*2*((c(a*a*H1)*IMX1+c(a*a*H1)*IMX21+c(a*a*H2)*IMX12+c(a*a*H2)*IMX2)+AA*(c(a)*IMX1+c(a)*IMX2)))
DhblogP11=matrix(c(colSums(DhblogP110)),ncol(RIE001),n_cov_coef)-t(DhlogP11)%*%DblogP11#############
DhalogP110=((cen1*cen2)/P11)*( R*( c(AA2)*(c(a)*RIE001)+c(AA1)*(c(a)*RIE002))+
c1*(c(AA2+a*H2)*RIE001+c(AA1+a*H1)*RIE002)+
c2*(c(AA2+a*H2)*(RIE001+RIE002)+c(AA+a*(H1+H2))*RIE002)+
c3*(c(AA1+a*H1)*(RIE001+RIE002)+c(AA+a*(H1+H2))*RIE001)+
c4*2*c(AA+a*(H1+H2))*(RIE001+RIE002))
DhalogP11=colSums((DhalogP110)-c(dalogP11)*DhlogP11)######
DhRlogP110=((cen1*cen2)/P11)*( (2*R+a)*( c(AA2)*(c(a)*RIE001)+c(AA1)*(c(a)*RIE002))+
(1-2*R)*(c(AA2)*(c(a)*RIE001+c(a)*RIE002)+ c(AA)*(c(a)*RIE002))+(1-2*R)*(c(AA1)*(c(a)*RIE001+c(a)*RIE002)+ c(AA)*(c(a)*RIE001))+
(-2+2*R)*2*AA*(c(a)*RIE001+c(a)*RIE002))
DhRlogP11=colSums((DhRlogP110)-c(dRlogP11)*DhlogP11)######
P10=R*AA1+(1-R)*AA
DhlogP10=(cen1*(1-cen2)/P10)*(c(R)*(c(a)*RIE001)+c((1-R))*(c(a)*RIE001+c(a)*RIE002))
DblogP10=(cen1*(1-cen2)/P10)*( c(a*R*H1)*X1+c((1-R))*(c(a*H1)*X1+c(a*H2)*X2))
DalogP10=(cen1*(1-cen2)/P10)*(R*H1+(1-R)*(H1+H2))
DRlogP10=(cen1*(1-cen2)/P10)*(AA1-AA)
D2hlogP10=-t(DhlogP10)%*%DhlogP10####
DhblogP100=(cen1*(1-cen2)/P10)*(c(R)*(c(a)*IMX1)+c((1-R))*(c(a)*IMX1+c(a)*IMX2))
DhblogP10=matrix(c(colSums(DhblogP100)),ncol(RIE002),n_cov_coef)- t(DhlogP10)%*%DblogP10######
DhalogP100=(cen1*(1-cen2)/P10)*(c(R)*(RIE001)+c((1-R))*(RIE001+RIE002))
DhalogP10=colSums((DhalogP100)-c(DalogP10)*DhlogP10)######
DhRlogP100=(cen1*(1-cen2)/P10)*((c(a)*RIE001)-(c(a)*RIE001+c(a)*RIE002))
DhRlogP10=colSums((DhRlogP100)-c(DRlogP10)*DhlogP10)######
P01=R*AA2+(1-R)*AA
DhlogP01=((1-cen1)*cen2/P01)*(c(R)*(c(a)*RIE002)+c((1-R))*(c(a)*RIE001+c(a)*RIE002))
DblogP01=((1-cen1)*cen2/P01)*( c(a*R*H2)*X2+c((1-R))*(c(a*H1)*X1+c(a*H2)*X2))
DalogP01=((1-cen1)*cen2/P01)*(R*H2+(1-R)*(H1+H2))
DRlogP01=((1-cen1)*cen2/P01)*(AA2-AA)
D2hlogP01=-t(DhlogP01)%*%DhlogP01####
DhblogP010=((1-cen1)*cen2/P01)*(c(R)*(c(a)*IMX2)+c((1-R))*(c(a)*IMX1+c(a)*IMX2))
DhblogP01=matrix(c(colSums(DhblogP010)),ncol(RIE002),n_cov_coef)- t(DhlogP01)%*%DblogP01######
DhalogP010=((1-cen1)*cen2/P01)*(c(R)*(RIE002)+c((1-R))*(RIE001+RIE002))
DhalogP01=colSums((DhalogP010)-c(DalogP01)*DhlogP01)######
DhRlogP010=((1-cen1)*cen2/P01)*((c(a)*RIE002)-(c(a)*RIE001+c(a)*RIE002))
DhRlogP01=colSums((DhRlogP010)-c(DRlogP01)*DhlogP01)######
DbDh=Dhbp00i+DhblogP11+DhblogP10+DhblogP01
D2h=(D2hp00+D2hlogP11+D2hlogP10+D2hlogP01)
diag(D2h)<-diag(D2h)-(nev1/nonzero_h0^2)
DhDa=cbind(c(Dhap00i+DhalogP11+DhalogP10+DhalogP01),c(DhRp00i+DhRlogP11+DhRlogP10+DhRlogP01))
comp=D2bnthtderivcv(newtht=newtht,X1=X1,X2=X2,H1=H1,H2=H2,cen1=cen1,cen2=cen2)
D2b=comp$D2B
Dbtht=comp$DBtht
D2a=-.sdLlikgammacv(theta=newtht,cen1=cen1,cen2=cen2,H1=H1,H2=H2)
MA=rbind(D2b,Dbtht,DbDh);MB=rbind(t(Dbtht),D2a,DhDa);MC=rbind(t(DbDh),t(DhDa),D2h)
HES=-cbind(MA,MB,MC)
RG2=t(HES)
HES[lower.tri(HES, diag = FALSE)]<-RG2[lower.tri(RG2, diag = FALSE)]
INVE<-solve(HES)
seofthet=sqrt(diag(INVE))
SEofbetandsig=c(seofthet[1:(n_cov_coef+2)])
seeho=c(seofthet[(n_cov_coef+3):length(seofthet)])
ary=array(1:(n_cov_coef+2))
ii=cbind(rep(ary,(n_cov_coef+2)),rep(ary, each=(n_cov_coef+2)))
vcovbtht=matrix(c(INVE[ii]),(n_cov_coef+2),(n_cov_coef+2))
seho=cbind(unqtime=trevntime,ho=nonzero_h0,SEho=seeho)
list(se=SEofbetandsig,vco=-D2a,parvcov=vcovbtht,vcovb=-D2b,seho=seho)}
#
#######
SE.sharedgam=function(bet,newtht,n_eve,H_bet_x,censor,X,W,RI,ind.haz,tau,clustind,Xoxo,we1){
n_cov_coef= ncol(X);data.n1=nrow(X);data.n=data.n1/2
Wu=W[clustind]
WEIGHT=we1[clustind]
RUU=RI
x_bet<-X%*%bet
g0<-c(c(WEIGHT)*exp(x_bet+Wu))
svexp_bet_xo=as.vector(RI%*%(c(WEIGHT)*exp(x_bet+Wu)))
dbexp_bet_xo=(RI%*%(c(g0)*X))
d2bexp_bet_xo=(RI%*%(c(g0)*Xoxo))
RUU1=RUU*(c(-n_eve/svexp_bet_xo^2))
RUU2=t(RUU1)
RUU3=RUU2*(c(g0))
NR0 <- apply(tau,1,matcolopp,MAT=RUU3,nc=ncol(RUU3))
ETS0<- apply(NR0,2,matcolcumsum)
ETSU0=ETS0[ind.haz,]
ETSU<- apply(tau,1,matcolopp,MAT=ETSU0,nc=ncol(ETSU0))
ETSU<-t(ETSU)
ETSUB=-ETSU*(c(exp(W)))
hazini <- apply(tau,1,clusievent,fx=(H_bet_x*c(WEIGHT)))
Hcomp=(diag(ETSUB)-hazini*exp(W))
diag(ETSUB)<-(Hcomp-(1/newtht)*exp(W))
RGF=t(ETSUB)
ETSUB[lower.tri(ETSUB, diag = FALSE)]<-RGF[lower.tri(RGF, diag = FALSE)]
ddhh0=dbexp_bet_xo*c(-(n_eve/svexp_bet_xo^2))
ddhh0=as.matrix(ddhh0)
ddhh0<- apply(ddhh0,2,matcolcumsum)
ddhh=ddhh0[ind.haz,]
dbpart1=-c(WEIGHT)*ddhh*c(exp(c(x_bet)))-c(c(WEIGHT)*H_bet_x)*X
dbpart1=as.matrix(dbpart1)
dbpart2<- apply(tau,1,matcolopp,MAT=dbpart1,nc=ncol(dbpart1))
if(n_cov_coef==1){dbpart2=matrix(dbpart2,ncol(dbpart1),length(W))}
dbpart2<-t(dbpart2)
dbpart2<-(dbpart2)*c(exp(W))
dbpart2=as.matrix(dbpart2)
D2b=(matrix(c(colSums(d2bexp_bet_xo*c(-(n_eve/svexp_bet_xo)))),n_cov_coef,n_cov_coef)+
t(dbexp_bet_xo)%*%(c((n_eve/svexp_bet_xo^2))*dbexp_bet_xo))
MA=rbind(D2b,dbpart2);MB=rbind(t(dbpart2),ETSUB)
HESppl=cbind(MA,MB)
INVE=solve(-HESppl)
seofthet=sqrt(diag(INVE))
seofthet=as.vector(seofthet)
SEofbetandsig=c(c(seofthet[1:n_cov_coef]))
vcovb=solve(-D2b)
list(SE=SEofbetandsig,vcovb=vcovb,Hesppl=HESppl)
}

###############################################
######Functions for Bivariate gamma frailty fit##########
#####################################
# Expected frailities#####
############################
.expfrailfun <- function(x,k0,k1,k2){
q1<-q2<-q3<-q4<-p1<-p2<-p3<-p4<-su<-NULL
B0<-B1<-B2<-frai<-NULL
if((x[1]==0) & (x[2]==0)){
B0<-(k0/x[5])
B1<-(k1/x[3]);B2<-(k2/x[4])
frai<-c(B0,B1,B2)}
if((x[1]==1) & (x[2]==0)){
q1=(k0/x[5]); q2=(k1/x[3])
p1=(q1/(q1+q2)); p2=(q2/(q1+q2))
B0=((p1*(k0+1)+p2*k0)/x[5])
B2=(k2/x[4])
B1=((p1*k1+p2*(k1+1))/x[3])
frai<-c(B0,B1,B2)}
if((x[1]==0) & (x[2]==1)){
q1=(k0/x[5]); q2=(k2/x[4])
p1=(q1/(q1+q2)); p2=(q2/(q1+q2))
B0=((p1*(k0+1)+p2*k0)/x[5])
B1=(k1/x[3])
B2=((p1*k2+p2*(k2+1))/x[4])
frai<-c(B0,B1,B2)}
if((x[1]==1) & (x[2]==1)){
q1=((k0*(k0+1))/x[5]^2); q2=((k0*k1)/(x[5]*x[3]))
q3=((k0*k2)/(x[5]*x[4])); q4=((k1*k2)/(x[3]*x[4]))
su=(q1+q2+q3+q4)
p1=(q1/su); p2=(q2/su); p3=(q3/su); p4=(q4/su)
B0=((p1*(k0+2)+(p2+p3)*(k0+1)+p4*k0)/(x[5]))
B1=(((p1+p3)*k1+(p2+p4)*(k1+1))/(x[3]))
B2=(((p1+p2)*k2+(p3+p4)*(k2+1))/(x[4]))
frai<-c(B0,B1,B2)}
frai
}
.Expefrail=function(newtht,H1,H2,cen1,cen2){
newtht[newtht<0.000001]<-0.000001
if(newtht[2]>0.99999){newtht[2]>0.99999}
a<-newtht[1];R<-newtht[2]
k0<-(R/a);k1<-k2<-(1-R)/a
x<-cbind(cen1,cen2,(1/a+H1),(1/a+H2),(1/a+H1+H2))
frailcomp <- apply(x,1,.expfrailfun,k0=k0,k1=k1,k2=k2)
frailcomp =t(as.matrix(frailcomp))
z1=c(frailcomp[,1])+c(frailcomp[,2])
z2=c(frailcomp[,1])+c(frailcomp[,3])
list(z1=z1,z2=z2)
}
#
.WExpfrailfunetsu<- function(x,k0,k1){
q1<-q2<-q3<-q4<-p1<-p2<-p3<-p4<-su<-NULL
B0<-B1<-B2<-frai<-NULL
if((x[1]==0) & (x[2]==0)){
B0<-(k0/x[5])
B1<-(k1/x[3]);B2<-(k1/x[4])
frai<-c(B0,B1,B2)}
if((x[1]==1) & (x[2]==0)){
q1=(k0/x[9]);q2=(k1/x[7])
p1=(q1/(q1+q2)); p2=(q2/(q1+q2))
B0=((p1*(k0+x[6])+p2*k0)/x[5])
B2=(k1/x[4])
B1=((p1*k1+p2*(k1+x[6]))/x[3])
frai<-c(B0,B1,B2)}
if((x[1]==0) & (x[2]==1)){
q1=(k0/x[9]);q2=(k1/x[8])
p1=(q1/(q1+q2)); p2=(q2/(q1+q2))
B0=((p1*(k0+x[6])+p2*k0)/x[5])
B1=(k1/x[3])
B2=((p1*k1+p2*(k1+x[6]))/x[4])
frai<-c(B0,B1,B2)}
if((x[1]==1) & (x[2]==1)){
q1=((k0*(k0+1))/x[9]^2)
q2=((k0*k1)/(x[9]*x[7]))
q3=((k0*k1)/(x[9]*x[8]))
q4=((k1*k1)/(x[8]*x[7]))
su=(q1+q2+q3+q4)
p1=(q1/su); p2=(q2/su); p3=(q3/su); p4=(q4/su)
B0=((p1*(k0+2*x[6])+(p2+p3)*(k0+x[6])+p4*k0)/(x[5]))
B1=(((p1+p3)*k1+(p2+p4)*(k1+x[6]))/(x[3]))
B2=(((p1+p2)*k1+(p3+p4)*(k1+x[6]))/(x[4]))
frai<-c(B0,B1,B2)}
frai
}
#
.WExpefrailetsu=function(newtht,cen1,cen2,H1,H2,Weight){
a<-newtht[1];R<-newtht[2]
if(R>0.9999){R<-0.9999}
k0<-(R/a);k1<-(1-R)/a
x<-cbind(cen1,cen2,(1/a+Weight*H1),(1/a+Weight*H2),(1/a+Weight*(H1+H2)),(Weight),(1/a+H1),(1/a+H2),(1/a+(H1+H2)))
frailcomp <- apply(x,1,.WExpfrailfunetsu,k0=k0,k1=k1)
frailcomp =t(as.matrix(frailcomp))
z1=c(frailcomp[,1])+c(frailcomp[,2])
z2=c(frailcomp[,1])+c(frailcomp[,3])
list(frail1=log(z1),frail2=log(z2),fr=frailcomp)
}
#

.newhazards<- function(x,R,a,k0,k1){
q1<-q2<-q3<-q4<-p1<-p2<-p3<-p4<-L1<-su<-sq<-NULL
if(R>0.999){R<-0.999}
h1<-h2<-h0<-frailhaz<-M1<-M2<-M0<-NULL
if((x[1]==0) & (x[2]==0)){
h0=(1/a)*(R/x[5]-1)
h1=(1/a)*((1-R)/x[6]-1)
h2=(1/a)*((1-R)/x[7]-1)
frailhaz<-c(h1,h2,h0)}
if((x[1]==1) & (x[2]==0)){
M1=(1+a*x[3]);M0=(1+a*(x[3]+x[4]))
q1=(R/M0);q2=(1-R)/M1
p1=(q1/(q1+q2));p2=(q2/(q1+q2))
h0=(1/a)*((R+p1*a)/x[5]-1)
h1=(1/a)*((1-R+p2*a)/x[6]-1)
h2=(1/a)*((1-R)/x[7]-1)
frailhaz<-c(h1,h2,h0)}
if((x[1]==0) & (x[2]==1)){
M2=(1+a*x[4]);M0=(1+a*(x[3]+x[4]))
q1=(R/M0);q2=(1-R)/M2
p1=(q1/(q1+q2));p2=(q2/(q1+q2))
h0=(1/a)*((R+p1*a)/x[5]-1)
h1=(1/a)*((1-R)/x[6]-1)
h2=(1/a)*((1-R+p2*a)/x[7]-1)
frailhaz<-c(h1,h2,h0)}
if((x[1]==1) & (x[2]==1)){
M1=(1+a*x[3]);M2=(1+a*x[4]);M0=(1+a*(x[3]+x[4]))
q1=(R*(R+a))/M0^2
q2=(R*(1-R)/(M0*M1))
q3=(R*(1-R)/(M0*M1))
q4=((1-R)^2/(M2*M1))
su=q1+q2+q3+q4
p1=q1/su;p4=q4/su;p2=q2/su;p3=q3/su
h0=(1/a)*((R+a+(p1-p4)*a)/x[5]-1)
h1=(1/a)*((1-R+(p2+p4)*a)/x[6]-1)
h2=(1/a)*((1-R+(p3+p4)*a)/x[7]-1)
frailhaz<-c(h1,h2,h0)}
frailhaz
}
#
Whazard=function(newtht,cen1,cen2,H1,H2,frailmat,Weight){
a<-newtht[1];R<-newtht[2]
k0=a/R;k1=(1-R)/a
y0=frailmat[,1];y1=frailmat[,2];y2=frailmat[,3]
x<-cbind(cen1,cen2,H1,H2,y0,y1,y2,(y0+y1),(y0+y2),Weight)
hacomp <- apply(x,1,.newhazards,a=a,R=R,k0=k0,k1=k1)
hacomp=t(as.matrix(hacomp))
H1<-hacomp[,1];H2<-hacomp[,2];HH<-hacomp[,3]
xx=matrix(c(H1,H2,HH,cen1,cen2),length(cen1),5)
xx
}

## the mariginal log likelihood of gamma fit

.Llikgammacvx = function (theta, xx,QQ){
a = abs(theta[1]);R = abs(theta[2])
H1=xx[,1];H2=xx[,2];HH=xx[,3];cen1=xx[,4];cen2=xx[,5]
logli=QQ+sum(-((1-R)/a+cen1)*log(1+a*H1)-((1-R)/a+cen2)*log(1+a*H2)-(R/a+cen1+cen2)*log(1+a*HH)+
cen1*cen2*log((R+a)*R*(1+a*H1)*(1+a*H2)+(1-R)*R*(1+a*HH)*((1+a*H1)+(1+a*H2))+(1-R)^2*(1+a*HH)^2)+
cen1*(1-cen2)*log(R*(1+a*H1)+(1-R)*(1+a*HH))+
(1-cen1)*cen2*log(R*(1+a*H2)+(1-R)*(1+a*HH)))
-logli
    }

## the gradient log likelihood of gamma fit
#
.fdLlikgammacvx = function (theta, xx,QQ){
a = abs(theta[1]);R = abs(theta[2])
H1=xx[,1];H2=xx[,2];HH=xx[,3];cen1=xx[,4];cen2=xx[,5]
da=sum(((1 - R)/a^2 * log(1 + a * H1) - ((1 - R)/a + cen1) * (H1/(1 + a * H1)) - (((1 - R)/a + cen2) * (H2/(1 + a * H2)) - (1 -
    R)/a^2 * log(1 + a * H2)) - ((R/a + cen1 + cen2) * (HH/(1 + a * HH)) - R/a^2 * log(1 + a * HH)) + cen1 * cen2 * (((R *
    (1 + a * H1) + (R + a) * R * H1) * (1 + a * H2) + (R + a) * R * (1 + a * H1) * H2 + ((1 - R) * R * HH * ((1 + a * H1) +
    (1 + a * H2)) + (1 - R) * R * (1 + a * HH) * (H1 + H2)) + (1 - R)^2 * (2 * (HH * (1 + a * HH))))/((R + a) * R * (1 +
    a * H1) * (1 + a * H2) + (1 - R) * R * (1 + a * HH) * ((1 + a * H1) + (1 + a * H2)) + (1 - R)^2 * (1 + a * HH)^2)) +
    cen1 * (1 - cen2) * ((R * H1 + (1 - R) * HH)/(R * (1 + a *  H1) + (1 - R) * (1 + a * HH))) + (1 - cen1) * cen2 *
    ((R * H2 + (1 - R) * HH)/(R * (1 + a * H2) + (1 - R) * (1 + a * HH)))))
dR=sum((1/a * log(1 + a * H1) + 1/a * log(1 + a * H2) - 1/a * log(1 + a * HH) + cen1 * cen2 * (((R + (R + a)) * (1 + a * H1) *
    (1 + a * H2) + ((1 - R) - R) * (1 + a * HH) * ((1 + a * H1) + (1 + a * H2)) - 2 * (1 - R) * (1 + a * HH)^2)/((R + a) *
    R * (1 + a * H1) * (1 + a * H2) + (1 - R) * R * (1 + a * HH) * ((1 + a * H1) + (1 + a * H2)) + (1 - R)^2 * (1 + a *
    HH)^2)) + cen1 * (1 - cen2) * (((1 + a * H1) - (1 + a * HH))/(R * (1 + a * H1) + (1 - R) * (1 + a * HH))) + (1 - cen1) * cen2 *
    (((1 + a * H2) - (1 + a * HH))/(R * (1 + a * H2) + (1 - R) * (1 + a * HH)))))
gd=c(da,dR)
-gd
}
#
.sdLlikgammacvx = function (theta,xx,QQ){
a = abs(theta[1]);R = abs(theta[2])
H1=xx[,1];H2=xx[,2];HH=xx[,3];cen1=xx[,4];cen2=xx[,5]
llkexprss <- expression(((-((1-R)/a)*log(1+a*H1)-((1-R)/a)*log(1+a*H2)-(R/a)*log(1+a*(HH)))+
cen1*cen2*log((R+a)*R*(1+a*(HH))^(-2)+(1-R)*R*((1+a*(HH))^(-1))*((1+a*H1)^(-1)+(1+a*H2)^(-1))+
((1-R)^2)*((1+a*H1)^(-1))*((1+a*H2)^(-1)))+
cen1*(1-cen2)*log(R*((1+a*(HH))^(-1))+(1-R)*((1+a*H1)^(-1)))+
(1-cen1)*cen2*log(R*((1+a*(HH))^(-1))+(1-R)*((1+a*H2)^(-1)))))
dxy <- deriv3(llkexprss, c("a","R"),hessian = TRUE)
sr=eval(dxy)
sr1<-attributes(sr)
Hes=colSums(sr1$hessian)
-Hes
}
#
.sdLlikgammacv = function (theta,cen1,cen2,H1,H2){
a = abs(theta[1]);R = abs(theta[2])
HH=(H1+H2)
llkexprss <- expression(((-((1-R)/a)*log(1+a*H1)-((1-R)/a)*log(1+a*H2)-(R/a)*log(1+a*(HH)))+
cen1*cen2*log((R+a)*R*(1+a*(HH))^(-2)+(1-R)*R*((1+a*(HH))^(-1))*((1+a*H1)^(-1)+(1+a*H2)^(-1))+
((1-R)^2)*((1+a*H1)^(-1))*((1+a*H2)^(-1)))+
cen1*(1-cen2)*log(R*((1+a*(HH))^(-1))+(1-R)*((1+a*H1)^(-1)))+
(1-cen1)*cen2*log(R*((1+a*(HH))^(-1))+(1-R)*((1+a*H2)^(-1)))))
dxy <- deriv3(llkexprss, c("a","R"),hessian = TRUE)
sr=eval(dxy)
sr1<-attributes(sr)
Hes=colSums(sr1$hessian)
-Hes
}
#

# function for the case when sampling weight is included in gamma fit
#
.optimcvw<-function(newtht,xx,control){
fittr=do.call(nlminb, args=c(list(start=newtht, objective=.Llikgammacvx,
gradient = .fdLlikgammacvx,hessian =.sdLlikgammacvx,xx=xx,QQ=0,
lower = c(0.000001,0.000001), upper = c(Inf,1),control=control$nlminb_control)))
newtht=fittr$par
if(newtht[1]<0.000001){newtht[1]=0.000001}
if(newtht[2]>0.9999999){newtht[2]=0.9999999}
newtht}
#

.bcgamfitcvw<-function(X,Y,initfrailp,weights,control){
time=Y[, 1];censor=Y[, 2]
sortt=sortingfunc(time=time,censor=censor,weights=weights)
uniq_tim=sortt$uniq_tim;ind.haz=sortt$ind.haz;n_eve=sortt$n_eve
weights=sortt$weights;indx=sortt$indx;time=sortt$t
Y[, 1]<-time
data.n1 <- nrow(X);data.n <-data.n1/2#### data.n is the number of pairs
ncovar_coef=ncol(X)
RI <- apply(as.array(uniq_tim),1,risskset,x=time)
RI <-t(RI)
i2<-2*array(1:data.n);i1<-i2-1
cph0 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset =NULL, init = NULL, control = survival::coxph.control(),
weights = weights,method = "breslow", resid=FALSE)
bet<-cph0$coefficients
x_bet<-X%*%bet
svexp_bet_xo=as.vector(RI%*%(c(weights)*exp(x_bet)))
H0<-cumsum(c(n_eve/svexp_bet_xo))
H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
cen1=censor[i1];cen2=censor[i2];we1=weights[i1];we2=weights[i2]
W=rep(0,data.n1)
if(length(initfrailp)>0){newtht<-initfrailp;lik=cph0$loglik[2]}else{
datu=data.frame(time=time,censor=censor,X)
IID=array(1:data.n1);ar=array(1:data.n);PID=rep(ar,each=2)
form<-as.formula(paste(paste("Surv(time,censor)"),"~",paste(colnames(X), collapse = "+ "),
"+ frailty(IID)",collapse=""))
shrfit <- shrgamsp(form,data=datu,weights=weights)
bet=shrfit$coefficients;x_bet<-X%*%bet;W=log(shrfit$frail)
svexp_bet_xo=as.vector(RI%*%((c(weights)*exp(x_bet+W))))
H0<-cumsum(c(n_eve/svexp_bet_xo))
H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
tht=shrfit$frailparest
newtht=c(tht,0.5)
if(tht<0.0001){
form<-as.formula(paste(paste("Surv(time,censor)"),"~",paste(colnames(X), collapse = "+ "),
"+ frailty(PID)",collapse=""))
shrfit <- shrgamsp(form,data=datu,weights=weights)
bet=shrfit$coefficients;x_bet<-X%*%bet;w=log(shrfit$frail)
W[i1]<-w;W[i2]<-w
svexp_bet_xo=as.vector(RI%*%((c(weights)*exp(x_bet+W))))
H0<-cumsum(c(n_eve/svexp_bet_xo))
H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
tht=shrfit$frailparest
newtht=c(tht,0.5)
if(tht<0.0001){newtht=c(0.001,0.001)}}}
iter=0
repeat{
iter=iter+1
estim0=newtht
iter1=0
repeat{
iter1=iter1+1
beto=bet
e_frail<-.WExpefrailetsu(newtht=newtht,cen1=cen1,
cen2=cen2,H1=H_bet_x[i1],H2=H_bet_x[i2],Weight=we1)
W[i1]<-e_frail$frail1;W[i2]<-e_frail$frail2;Z=exp(W)
cph1 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset =W, init = NULL, control = survival::coxph.control(),
weights = weights,method = "breslow", resid=FALSE)
bet<-cph1$coefficients
x_bet<-X%*%bet
svexp_bet_xo=as.vector(RI%*%(c(weights)*exp(x_bet+W)))
H0<-cumsum(c(n_eve/svexp_bet_xo))#
H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
newdiff=max(abs(bet-beto))
if((newdiff < 1e-08)  |  (iter1 >= control$max.iter)) break}
frailmat=matrix(c(e_frail$fr[,1],e_frail$fr[,2],e_frail$fr[,3]),data.n,3)
xx<-Whazard(newtht=newtht,cen1=cen1,cen2=cen2,H1=H_bet_x[i1],H2=H_bet_x[i2],
frailmat=frailmat,Weight=we1)
newtht<-.optimcvw(newtht,xx=xx,control=control)
new.diff=max(abs((c(newtht))-(estim0)))
if((new.diff < control$tol)  |  (iter >= control$max.iter)) break}
h0=diff(c(0,H0))
lik=(cph1$loglik[2]-.Llikgammacvx(theta=newtht,xx=xx,QQ=0)-sum(censor*W)+sum(censor))
resinteracmat<-apply(as.matrix(X),2,interacmat,u=X)
Xoxo<-matrix(resinteracmat,data.n1,ncovar_coef^2)
newtht0=newtht
if(newtht[1]<0.000001){newtht0[1]<-0.00001}
if(newtht[2]<0.000001){newtht0[2]<-0.00001}
if(newtht[2]>0.999999){newtht0[2]<-0.999999}
if(newtht0[2]>=0.999){
ar=array(1:data.n)
clustind=rep(ar,each=2)
uniq_clus_id=unique(clustind)
firi<-match(uniq_clus_id,clustind)
firi2=firi[-1]-1
firi3<-c(firi2,length(clustind))
tau=cbind(firi,firi3)
adjj_se=SE.sharedgam(bet=bet,newtht=newtht0,n_eve=n_eve,H_bet_x=H_bet_x,censor=censor,
X=X,W=(W[i1]),RI=RI,ind.haz=ind.haz,tau=tau,clustind=clustind,Xoxo=Xoxo,we1=we1)
adjse=c(adjj_se$SE)
vcov=cph1$var
if(anyNA(adjse)){adjse=c(sqrt(diag(vcov)))}}
if(newtht0[2]<0.999){
clustind=array(1:data.n1)
uniq_clus_id=unique(clustind)
firi<-match(uniq_clus_id,clustind)
firi2=firi[-1]-1
firi3<-c(firi2,length(clustind))
tau=cbind(firi,firi3)
adjj_se=SE.sharedgam(bet=bet,newtht=newtht0,n_eve=n_eve,H_bet_x=H_bet_x,censor=censor,
X=X,W=W,RI=RI,ind.haz=ind.haz,tau=tau,clustind=clustind,Xoxo=Xoxo,we1=weights)
adjse=c(adjj_se$SE)
vcov=cph1$var
if(anyNA(adjse)){adjse=c(sqrt(diag(vcov)))}}
colnames(vcov) <- rownames(vcov) <- colnames(X)
addj=.SEunivgamw(bet=bet,newtht=newtht0[1],n_eve=n_eve,etime=uniq_tim,
H=H_bet_x,h0=h0,censor=censor,time=time,X=X,WE=weights)
se10=addj$se[length(addj$se)]
D2a=.sdLlikgammacv(theta=newtht0,cen1=cen1,cen2=cen2,H1=H_bet_x[i1],H2=H_bet_x[i2])
setht=sqrt(diag(solve(D2a)));adjse=c(adjse,se10,setht[2])
res <-list(coefficients=bet,frailparest= c(theta=newtht[1],Row=newtht[2]),
vcov = vcov,loglilk0=cph0$loglik[2],loglilk=cph1$loglik[2],
Iloglilk=lik,cbasehaz=cbind(haz=H0,time=uniq_tim),X=X,time=time,censor=censor,
resid=cph1$residuals,lin.prid=cph1$linear.predictors,
frail=exp(W),stderr=adjse,iteration=iter,n.event=n_eve,e.time=uniq_tim,
convergence=ifelse((new.diff < control$tol),1,0))
res$call <- match.call()
class(res) <- c(".bcgamfitcvw")
res
}
###################################################################
############### End Utilities for Bivariate gamma frailty fit #####
###################################################################
#
######################################################
###########Univariate and shared gamma############
#######################################
.SEunivgam=function(bet,newtht,n_eve,uniq_tim,H,h0,censor,time,X){
a<-as.vector(newtht);n_eve<-as.vector(n_eve);uniq_tim<-as.vector(uniq_tim)
h0<-as.vector(h0);time<-as.vector(time);HH=H;HH<-as.vector(HH)
X <- as.matrix(X)
n_cov_coef= ncol(X);data.n1= nrow(X)
g0<-c(exp(X%*%bet))
AA=(1+a*HH);AA1=(1/a+HH)
n_eve0=as.numeric(n_eve>0)
trevntimein=n_eve0*array(1:length(n_eve0))
trevntimein1=trevntimein[trevntimein>0]
trevntime=uniq_tim[trevntimein1]
nonzero_h0<-h0[trevntimein1]
nev1<-n_eve[trevntimein1]
RIE <- apply(as.array(time),1,rissksetdr,x=trevntime)
RIE00 <-(t(RIE)*c(g0))
RIE0 <-(RIE00*c(1/AA1))
RIE1 <-RIE0*c(-(1/a+censor))
D2h <-(t(RIE1)%*%RIE0)
diag(D2h)<-diag(D2h)+(nev1/nonzero_h0^2)
RG2=t(D2h)
D2h[lower.tri(D2h, diag = FALSE)]<-RG2[lower.tri(RG2, diag = FALSE)]
DhDa <- c(colSums(c(c(1/a+censor)*c((1/a^2)/AA1^2))*RIE00-RIE0*c(1/a^2)))
resinteracmatx<-apply(as.matrix(X),2,interacmat,u=RIE00)
IMX<-matrix(resinteracmatx,data.n1,n_cov_coef*ncol(RIE00))
DN0=((c(HH)*X)*c(-(1/a+censor)/AA1^2))
DbDh=matrix(c(colSums(IMX*c(c(1/a+censor)*c(1/AA1)))),ncol(RIE00),n_cov_coef)+ t(RIE00)%*%DN0
resinteracmat0<-apply(as.matrix(X),2,interacmat,u=X)
IM<-matrix(resinteracmat0,data.n1,n_cov_coef^2)
DM=((c(HH)*IM)*c((1/a+censor)/AA1))
DN=(c(HH)*X)
D2b=matrix(c(colSums(DM)),n_cov_coef,n_cov_coef)+((t(DN))%*%DN0)
D2a=sum((2/a^3)*log(AA)-( (1/a+censor)*(HH^2/AA^2)+(2/a^2)*(HH/AA)))
DbDa=colSums(c(-(1/a^2)/AA1)*(c(HH)*X) +c(1/a+censor)*(c((1/a^2)/AA1^2)*(c(HH)*X)))
MA=rbind(D2b,DbDh,DbDa);MB=rbind(t(DbDh),D2h,DhDa);MC=c(c(DbDa),c(DhDa),c(D2a))
HES=cbind(MA,MB,MC)
INVE<-solve(HES)
seofthet=sqrt(diag(INVE))
SEofbetandsig=c(c(seofthet[1:n_cov_coef]),seofthet[length(seofthet)])
ii=cbind(rep(c(1:n_cov_coef),each=n_cov_coef),rep(c(1:n_cov_coef),n_cov_coef))
D2b0=D2b
D2b0[ii]<-INVE[ii]
seeho=c(seofthet[(n_cov_coef+1):(length(seofthet)-1)])
seho=cbind(unqtime=trevntime,ho=nonzero_h0,SEho=seeho)
list(se=SEofbetandsig,vco=D2b0,seho=seho)
}
.SEunivgamw=function(bet,newtht,n_eve,etime,H,h0,censor,time,X,WE){
a<-as.vector(newtht);n_eve<-as.vector(n_eve);etime<-as.vector(etime)
h0<-as.vector(h0);time<-as.vector(time);HH=H;HH<-as.vector(HH);WE=c(WE)
X <- as.matrix(X)
n_cov_coef= ncol(X);data.n1= nrow(X)
g0<-c(WE)*c(exp(X%*%bet))
AA=(1+a*c(WE)*HH);AA1=(1/a+c(WE)*HH)
di=censor*c(WE)
n_eve0=as.numeric(n_eve>0)
trevntimein=n_eve0*array(1:length(n_eve0))
trevntimein1=trevntimein[trevntimein>0]
trevntime=etime[trevntimein1]
nonzero_h0<-h0[trevntimein1]
nev1<-n_eve[trevntimein1]
rissksetdr <- function(time,x) ifelse(time[1]>=x,1,0)
RIE <- apply(as.array(time),1,rissksetdr,x=trevntime)
RIE00 <-(t(RIE)*c(g0))
RIE0 <-(RIE00*c(1/AA1))
RIE1 <-RIE0*c(-(1/a+di))
D2h <-(t(RIE1)%*%RIE0)
diag(D2h)<-diag(D2h)+(nev1/nonzero_h0^2)
RG2=t(D2h)
D2h[lower.tri(D2h, diag = FALSE)]<-RG2[lower.tri(RG2, diag = FALSE)]
DhDa <- c(colSums(c(c(1/a+di)*c((1/a^2)/AA1^2))*RIE00-RIE0*c(1/a^2)))
interacmatza <- function(X,u){X*u}
resinteracmatx<-apply(as.matrix(X),2,interacmatza,u=RIE00)
IMX<-matrix(resinteracmatx,data.n1,n_cov_coef*ncol(RIE00))
DN0=((c(HH*c(WE))*X)*c(-(1/a+di)/AA1^2))
DbDh=matrix(c(colSums(IMX*c(c(1/a+di)*c(1/AA1)))),ncol(RIE00),n_cov_coef)+ t(RIE00)%*%DN0
interacmatt <- function(X,u){X*u}
resinteracmat0<-apply(as.matrix(X),2,interacmatt,u=X)
IM<-matrix(resinteracmat0,data.n1,n_cov_coef^2)
DM=((c(HH*c(WE))*IM)*c((1/a+di)/AA1))
DN=(c(HH*c(WE))*X)
D2b=matrix(c(colSums(DM)),n_cov_coef,n_cov_coef)+((t(DN))%*%DN0)
oth=sum((di/a^2)-(2/a^3)*digamma(1/a+di)-(1/a^4)*trigamma(1/a+di)+(2/a^3)*digamma(1/a)+(1/a^4)*trigamma(1/a))
D2a=oth+sum((2/a^3)*log(AA)-( (1/a+di)*((c(WE)*HH)^2/AA^2)+(2/a^2)*(c(WE)*HH/AA)))
DbDa=colSums(c(-(1/a^2)/AA1)*(c(c(WE)*HH)*X) +c(1/a+di)*(c((1/a^2)/AA1^2)*(c(c(WE)*HH)*X)))
MA=rbind(D2b,DbDh,DbDa);MB=rbind(t(DbDh),D2h,DhDa);MC=c(c(DbDa),c(DhDa),c(D2a))
HES=cbind(MA,MB,MC)
INVE<-solve(HES)
seofthet=sqrt(diag(INVE))
SEofbetandsig=c(c(seofthet[1:n_cov_coef]),seofthet[length(seofthet)])
ii=cbind(rep(c(1:n_cov_coef),each=n_cov_coef),rep(c(1:n_cov_coef),n_cov_coef))
D2b0=D2b
D2b0[ii]<-INVE[ii]
seeho=c(seofthet[(n_cov_coef+1):(length(seofthet)-1)])
seho=cbind(unqtime=trevntime,ho=nonzero_h0,SEho=seeho)
list(se=SEofbetandsig,vco=D2b0,seho=seho)
}
.SEsharedgam=function(bet,newtht,n_eve,etime,H,h0,censor,time,X){
a<-as.vector(newtht);n_eve<-as.vector(n_eve);etime<-as.vector(etime)
h0<-as.vector(h0);time<-as.vector(time);HH=H;HH<-as.vector(HH)
X <- as.matrix(X);n_cov_coef=n4= ncol(X);data.n1=n1= nrow(X);data.n=n=n1/2
indic1<-2*array(1:data.n)-1;indic2<-2*array(1:data.n)
g0<-c(exp(X%*%bet))
HH1=HH[indic1];HH2=HH[indic2];g01=g0[indic1];g02=g0[indic2]
cen1=censor[indic1];cen2=censor[indic2]
AA=(1+a*(HH1+HH2));AA1=(1/a+(HH1+HH2))
di=(cen1+cen2)
X1=X[indic1,];X2=X[indic2,]
X1<- as.matrix(X1)
X2<- as.matrix(X2)
n_eve0=as.numeric(n_eve>0)
trevntimein=n_eve0*array(1:length(n_eve0))
trevntimein1=trevntimein[trevntimein>0]
trevntime=etime[trevntimein1]
nonzero_h0<-h0[trevntimein1]
nev1<-n_eve[trevntimein1]
rissksetj <- function(time,x) ifelse(time[1]>=x,1,0)
RIU <- apply(as.array(time),1,rissksetj,x=trevntime)
RIU <- as.matrix(RIU)
RIU0<-t(((t(RIU))*c(g0)))
RRW1 <- apply(RIU,1,funbf,u=indic1)
RRW2 <- apply(RIU,1,funbf,u=indic2)
RRW1 <-RRW1*c(g01)
RRW2 <-RRW2*c(g02)
RG <- apply(RIU0,1,risskf,indic1=indic1,indic2=indic2)
RG <- as.matrix(RG)
RG0<- (RG*c(c(-1/AA1^2)*c((1/a+di))))
D2h <-((t(RG))%*%RG0)
diag(D2h)<-diag(D2h)+(nev1/nonzero_h0^2)
RG2=t(D2h)
D2h[lower.tri(D2h, diag = FALSE)]<-RG2[lower.tri(RG2, diag = FALSE)]
resinteracmat1<-apply(as.matrix(X1),2,interacmat,u=X1)
IM1<-matrix(resinteracmat1,data.n,n_cov_coef^2)
resinteracmat2<-apply(as.matrix(X2),2,interacmat,u=X2)
IM2<-matrix(resinteracmat2,data.n,n_cov_coef^2)
resinteracmat2<-apply(as.matrix(X2),2,interacmat,u=X1)
IM21<-matrix(resinteracmat2,data.n,n_cov_coef^2)
resinteracmat1<-apply(as.matrix(X1),2,interacmat,u=X2)
IM12<-matrix(resinteracmat1,data.n,n_cov_coef^2)
DM=(c(a)*(c(HH1)*IM1+c(HH2)*IM2))*c((1/a+di)/AA)
DN=c(a)*(c(HH1)*X1+c(HH2)*X2)
DN0=(c(a)*(c(HH1)*X1+c(HH2)*X2))*c(-(1/a+di)/AA^2)
D2b=matrix(c(colSums(DM)),n_cov_coef,n_cov_coef)+((t(DN))%*%DN0)
D2a=sum(cen1*cen2*(1/(1+a)^2))+sum((2/a^3)*log(AA))- sum((1/a+di)*((HH1+HH2)^2/AA^2)+(2/a^2)*((HH1+HH2)/AA))
DbDa=colSums(c(-(1/a^2)/AA1)*(c(HH1)*X1+c(HH2)*X2) +c(1/a+di)*(c((1/a^2)/AA1^2)*(c(HH1)*X1+c(HH2)*X2)))
resinteracmatx1<-apply(as.matrix(X1),2,interacmat,u=RRW1)
IMx1<-matrix(resinteracmatx1,data.n,n_cov_coef*ncol(RRW1))
resinteracmatx2<-apply(as.matrix(X2),2,interacmat,u=RRW2)
IMx2<-matrix(resinteracmatx2,data.n,n_cov_coef*ncol(RRW2))
DbDh=matrix(c(colSums((IMx1+IMx2)*c(c(a/AA)*c((1/a+di))))),ncol(RRW2),n_cov_coef)+((t(c(a)*RG))%*%DN0)
DhDa=colSums((-1/a^2)*((RG*c(1/AA1)))+(RG*c(c((1/a^2)/AA1^2)*c((1/a+di)))))
MA=rbind(D2b,DbDh,DbDa);MB=rbind(t(DbDh),D2h,DhDa);MC=c(c(DbDa),c(DhDa),c(D2a))
HES=cbind(MA,MB,MC)
INVE<-solve(HES)
seofthet=sqrt(diag(INVE))
SEofbetandsig=c(c(seofthet[1:n_cov_coef]),seofthet[length(seofthet)])
SEofbetandsig
ii=cbind(rep(c(1:n_cov_coef),each=n_cov_coef),rep(c(1:n_cov_coef),n_cov_coef))
D2b0=D2b
D2b0[ii]<-INVE[ii]
seeho=c(seofthet[(n_cov_coef+1):(length(seofthet)-1)])
seho=cbind(unqtime=trevntime,ho=nonzero_h0,SEho=seeho)
list(se=SEofbetandsig,vco=D2b0,seho=seho)
}
.SEsharedgamw=function(bet,newtht,n_eve,etime,H,h0,censor,time,X,WE){
a<-as.vector(newtht);n_eve<-as.vector(n_eve);etime<-as.vector(etime)
h0<-as.vector(h0);time<-as.vector(time);HH=H;HH<-as.vector(HH);WE=WE
X <- as.matrix(X)
n_cov_coef=n4= ncol(X);data.n1=n1= nrow(X);data.n=n=n1/2
indic1<-2*array(1:data.n)-1;indic2<-2*array(1:data.n)
gg0<-c(rep(WE,each=2))*c(exp(X%*%bet))
HH1=HH[indic1];HH2=HH[indic2]
cen1=censor[indic1];cen2=censor[indic2]
g001=gg0[indic1];g002=gg0[indic2]
AA=AAA=(1+a*c(WE)*(HH1+HH2));AAA1=AA1=(1/a+c(WE)*(HH1+HH2))
di=d0=c(WE)*(cen1+cen2)
X1=X[indic1,];X2=X[indic2,]
X1<- as.matrix(X1);X2<- as.matrix(X2)
n_eve0=as.numeric(n_eve>0)
trevntimein=n_eve0*array(1:length(n_eve0))
trevntimein1=trevntimein[trevntimein>0]
trevntime=etime[trevntimein1]
nonzero_h0<-h0[trevntimein1]
nev1<-n_eve[trevntimein1]
rissksetj <- function(time,x) ifelse(time[1]>=x,1,0)
RIU <- apply(as.array(time),1,rissksetj,x=trevntime)
RIU <- as.matrix(RIU)
RIU0<-t(((t(RIU))*c(gg0)))
funbf <- function(RIU,u){
re0=RIU[u]
re0}
RRW1 <- apply(RIU,1,funbf,u=indic1)
RRW2 <- apply(RIU,1,funbf,u=indic2)
RRW1 <-RRW1*c(g001)
RRW2 <-RRW2*c(g002)
risskf <- function(RIU0,indic1,indic2){
re0=RIU0[indic1]+RIU0[indic2]
re0}
RG <- apply(RIU0,1,risskf,indic1=indic1,indic2=indic2)
RG <- as.matrix(RG)
RG0<- (RG*c(c(-1/AAA1^2)*c((1/a+d0))))
D2h <-((t(RG))%*%RG0)
diag(D2h)<-diag(D2h)+(nev1/nonzero_h0^2)
RG2=t(D2h)
D2h[lower.tri(D2h, diag = FALSE)]<-RG2[lower.tri(RG2, diag = FALSE)]
interacmat1 <- function(X1,u){X1*u}
resinteracmat1<-apply(as.matrix(X1),2,interacmat1,u=X1)
IM1<-matrix(resinteracmat1,data.n,n_cov_coef^2)
interacmat2 <- function(X2,u){X2*u}
resinteracmat2<-apply(as.matrix(X2),2,interacmat2,u=X2)
IM2<-matrix(resinteracmat2,data.n,n_cov_coef^2)
resinteracmat2<-apply(as.matrix(X2),2,interacmat2,u=X1)
IM21<-matrix(resinteracmat2,data.n,n_cov_coef^2)
resinteracmat1<-apply(as.matrix(X1),2,interacmat1,u=X2)
IM12<-matrix(resinteracmat1,data.n,n_cov_coef^2)
DM=(c(a)*c(WE)*(c(HH1)*IM1+c(HH2)*IM2))*c((1/a+d0)/AAA)
DN=c(a)*c(WE)*(c(HH1)*X1+c(HH2)*X2)
DN0=(c(a)*c(WE)*(c(HH1)*X1+c(HH2)*X2))*c(-(1/a+d0)/AAA^2)
D2b=matrix(c(colSums(DM)),n_cov_coef,n_cov_coef)+((t(DN))%*%DN0)
oth=sum((d0/a^2)-(2/a^3)*digamma(1/a+d0)-(1/a^4)*trigamma(1/a+d0)+(2/a^3)*digamma(1/a)+(1/a^4)*trigamma(1/a))
D2a=oth+sum((2/a^3)*log(AAA))- sum((1/a+d0)*((c(WE)*(HH1+HH2))^2/AAA^2)+(2/a^2)*(c(WE)*(HH1+HH2)/AAA))
DbDa=colSums(c(-(1/a^2)/AAA1)*(c(c(WE)*HH1)*X1+c(c(WE)*HH2)*X2) +c(1/a+d0)*(c((1/a^2)/AAA1^2)*(c(c(WE)*HH1)*X1+c(c(WE)*HH2)*X2)))
interacmatF1 <- function(X1,u){X1*u}
resinteracmatx1<-apply(as.matrix(X1),2,interacmatF1,u=RRW1)
IMx1<-matrix(resinteracmatx1,data.n,n_cov_coef*ncol(RRW1))
interacmatF2 <- function(X2,u){X2*u}
resinteracmatx2<-apply(as.matrix(X2),2,interacmatF2,u=RRW2)
IMx2<-matrix(resinteracmatx2,data.n,n_cov_coef*ncol(RRW2))
DbDh=matrix(c(colSums((IMx1+IMx2)*c(c(a/AAA)*c((1/a+d0))))),ncol(RRW2),n_cov_coef)+((t(c(a)*RG))%*%DN0)
DhDa=colSums((-1/a^2)*((RG*c(1/AAA1)))+(RG*c(c((1/a^2)/AAA1^2)*c((1/a+d0)))))
MA=rbind(D2b,DbDh,DbDa);MB=rbind(t(DbDh),D2h,DhDa);MC=c(c(DbDa),c(DhDa),c(D2a))
HES=cbind(MA,MB,MC)
INVE<-solve(HES)
seofthet=sqrt(diag(INVE))
SEofbetandsig=c(c(seofthet[1:n_cov_coef]),seofthet[length(seofthet)])
SEofbetandsig
ii=cbind(rep(c(1:n_cov_coef),each=n_cov_coef),rep(c(1:n_cov_coef),n_cov_coef))
D2b0=D2b
D2b0[ii]<-INVE[ii]
seeho=c(seofthet[(n_cov_coef+1):(length(seofthet)-1)])
seho=cbind(unqtime=trevntime,ho=nonzero_h0,SEho=seeho)
list(se=SEofbetandsig,vco=D2b0,seho=seho)
}

.Llikgamsharedd0= function (theta,X,Y,RI,H_bet_x,Othrs){
censor=Othrs$censor;ind.haz=Othrs$ind.haz;n_eve=Othrs$n_eve
tau=Othrs$tau;weights=Othrs$weights;clustind=Othrs$clustind
d0=Othrs$d0;di=Othrs$di
Z=W=H_bet_x
a = abs(theta)
newdiff=bet0=AI=CI=bet0=NULL
bet<-rep(0,ncol(X))
iter1=0
repeat{iter1=iter1+1
bet0=bet
hazi <- apply(tau,1,clusievent,fx=c(H_bet_x*weights))
AI=(1/a+d0);CI=(1/a+hazi)
z=AI/CI;Z=z[clustind];W=log(Z);w=log(z);W[is.na(W)]<-0
w[is.na(w)]<-0
cph1 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset =W, init = NULL, control = survival::coxph.control(),
weights = weights,method = "breslow", resid=FALSE)
bet<-cph1$coefficients
x_bet<-X%*%bet
exp_x_bet_w<-exp(c(c(X%*%bet)+W))
svexp_bet_xo=as.vector(RI%*%(c(weights)*exp(x_bet+W)))
H0<-cumsum(n_eve/svexp_bet_xo)
H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
newdiff=max(abs(bet-bet0))
if((newdiff < 1e-09)  |  (iter1 >= 50)) break}
QQ=(cph1$loglik[2])
mdi <- apply(as.array(di),1,funloggamm,a=a)
logli=QQ+sum(1/a*(w-exp(w))+mdi+di+(1/a)-(1/a+di)*log(1/a+di)-(1/a)*log(a))
-logli}

.fdLlikgamsharedd0= function (theta,X,Y,RI,H_bet_x,Othrs){
censor=Othrs$censor;ind.haz=Othrs$ind.haz;n_eve=Othrs$n_eve
tau=Othrs$tau;weights=Othrs$weights;clustind=Othrs$clustind
d0=Othrs$d0;di=Othrs$di
Z=W=H_bet_x
a = abs(theta)
newdiff=bet0=AI=CI=bet0=NULL
bet<-rep(0,ncol(X))
iter1=0
repeat{iter1=iter1+1
bet0=bet
hazi <- apply(tau,1,clusievent,fx=c(H_bet_x*weights))
AI=(1/a+d0);CI=(1/a+hazi)
z=AI/CI;Z=z[clustind];W=log(Z);w=log(z);W[is.na(W)]<-0
w[is.na(w)]<-0
cph1 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset =W, init = NULL, control = survival::coxph.control(),
weights = weights,method = "breslow", resid=FALSE)
bet<-cph1$coefficients
x_bet<-X%*%bet
exp_x_bet_w<-exp(c(c(X%*%bet)+W))
svexp_bet_xo=as.vector(RI%*%(c(weights)*exp(x_bet+W)))
H0<-cumsum(n_eve/svexp_bet_xo)
H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
newdiff=max(abs(bet-bet0))
if((newdiff < 1e-09)  |  (iter1 >= 50)) break}
mdi <- apply(as.array(di),1,funfdloggamm,a=a)
grr=c(sum(-(1/a^2)*(w-exp(w))+mdi-(1/a^2)+
(1/a^2)*log(1/a+di)+(1/a+di)*((1/a^2)/(1/a+di))+(1/a^2)*log(a)-(1/a^2)))
-grr
}
shrdgamma.fit<-function(X,Y,initfrailp,frailind,weights,control){
uniq_id<-unique(frailind)
order=sort(frailind, decreasing = FALSE, index.return = TRUE)
subject_indx=order$ix
ncovar_coef=ncol(X)
nam=colnames(X)
if(ncovar_coef==1){
X=matrix(X,nrow(X),1)
colnames(X)<-nam}
Y<-Y[subject_indx,];X<-X[subject_indx,]
if(ncovar_coef==1){X=as.matrix(X)
colnames(X)<-nam}
clustind=match(frailind,uniq_id)
uniq_clus_id=unique(clustind)
firi<-match(uniq_clus_id,clustind)
firi2=firi[-1]-1
firi3<-c(firi2,length(clustind))
tau=cbind(firi,firi3)
time=Y[, 1];censor=Y[, 2]
weightorg<-weights
sortt=sortingfunc(time=time,censor=censor,weights=weightorg)
uniq_tim=sortt$uniq_tim;ind.haz=sortt$ind.haz;n_eve=sortt$n_eve
t_n_eve=sortt$t_n_eve;eventtau=sortt$eventtau;weights=sortt$weights;indx=sortt$indx
time=sortt$t
n_evep=sortt$n_evep
Y[, 1]<-time
data.n1 <- nrow(X);data.n <-data.n1/2#### data.n is the number of pairs
RI <- apply(as.array(uniq_tim),1,risskset,x=time)
RI <-t(RI)
cph0 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset =NULL, init = NULL, control = survival::coxph.control(),
weights = NULL,method = "breslow", resid=FALSE)
bet<-cph0$coefficients
x_bet<-X%*%bet
svexp_bet_xo=as.vector(RI%*%(exp(x_bet)))
H0<-cumsum(c(n_eve/svexp_bet_xo))
H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
if(length(initfrailp)>0){initfrailp<-initfrailp}else{initfrailp=c(0.1)}
newtht<-NULL
typ<-funfittypp(clustind)
wei<- apply(tau,1,clusiobj,fx=weights)
di <- apply(tau,1,clusievent,fx=censor)
d0=c(di*wei)
Othrs=list(censor=censor,ind.haz=ind.haz,n_eve=n_eve,tau=tau,
weights=weights,clustind=clustind,di=di,d0=d0)
W=H_bet_x
fittr=do.call(nlminb, args=c(list(start=initfrailp, objective=.Llikgamsharedd0,
gradient = .fdLlikgamsharedd0,X=X,Y=Y,RI=RI,H_bet_x=H_bet_x,Othrs=Othrs,
lower = c(0.000001), upper = c(Inf),control=control$nlminb_control)))
newtht<-fittr$par
lik=-fittr$objective
newdiff =bet0=AI=CI=NULL
Z=W=H_bet_x
iter1=0
repeat{iter1=iter1+1
bet0=bet
hazi <- apply(tau,1,clusievent,fx=c(H_bet_x*weights))
AI=(1/newtht+d0);CI=(1/newtht+hazi)
z=AI/CI;Z=z[clustind];W=log(Z)
cph1 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset =W, init = NULL, control = survival::coxph.control(),
weights = weights,method = "breslow", resid=FALSE)
bet<-cph1$coefficients
x_bet<-X%*%bet
exp_x_bet_w<-exp(c(c(X%*%bet)+W))
svexp_bet_xo=as.vector(RI%*%(c(weights)*exp(x_bet+W)))
H0<-cumsum(n_eve/svexp_bet_xo)
H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
newdiff=max(abs(bet-bet0))
if((newdiff < 1e-10)  |  (iter1 >= 50)) break}
newtht0=newtht
if(newtht<0.000001){newtht0<-0.00001}
h0=diff(c(0,H0));nonzero_h0=h0[h0>0]
fittype<-NULL
if(length(weightorg)==0){
if(typ=="Univ"){
adjj_se=.SEunivgam(bet=bet,newtht=newtht,n_eve=n_eve,uniq_tim=uniq_tim,
H=c(H_bet_x),h0=h0,censor=censor,time=time,X=X)
adjse=c(adjj_se$se);vcov2=cph1$var;vcov=adjj_se$vco;seho=adjj_se$seho
if(anyNA(adjse)){
adjj_se=.SEunivgam(bet=bet,newtht=c(0.0001),n_eve=n_eve,uniq_tim=uniq_tim,
H=c(H_bet_x),h0=h0,censor=censor,time=time,X=X)
adjse0=c(adjj_se$se)
if(anyNA(adjse0)){adjse[length(adjse0)]=1}else{
adjse[length(adjse0)]=adjse0[length(adjse0)]}}
fittype<-c(1)}
if(typ=="Biv"){
adjj_se=.SEsharedgam(bet=bet,newtht=newtht,n_eve=n_eve,etime=uniq_tim,
H=c(H_bet_x),h0=h0,censor=censor,time=time,X=X)
seho=adjj_se$seho
adjse=c(adjj_se$se);vcov2=cph1$var;vcov=adjj_se$vco;fittype<-c(1)}
if(typ=="Shr"){
resinteracmat<-apply(as.matrix(X),2,interacmat,u=X)
Xoxo<-matrix(resinteracmat,data.n1,ncovar_coef^2)
seho=NULL
adjj_se=SE.sharedgam(bet=bet,newtht=newtht,n_eve=n_eve,H_bet_x=H_bet_x,censor=censor,
X=X,W=log(z),RI=RI,ind.haz=ind.haz,tau=tau,clustind=clustind,Xoxo=Xoxo,we1=wei)}}
if(length(weightorg)!=0){
if(typ=="Univ"){
adjj_se=.SEunivgamw(bet=bet,newtht=newtht,n_eve=n_eve,etime=uniq_tim,
H=c(H_bet_x),h0=h0,censor=censor,time=time,X=X,WE=weights)
adjse=c(adjj_se$se);vcov2=cph1$var;vcov=adjj_se$vco;seho=adjj_se$seho
if(anyNA(adjse)){
adjj_se=.SEunivgamw(bet=bet,newtht=c(0.0001),n_eve=n_eve,etime=uniq_tim,
H=c(H_bet_x),h0=h0,censor=censor,time=time,X=X,WE=weights)
adjse0=c(adjj_se$se);seho=adjj_se$seho
if(anyNA(adjse0)){adjse[length(adjse0)]=1}else{
adjse[length(adjse0)]=adjse0[length(adjse0)]}}
fittype<-c(1)}
if(typ=="Biv"){
adjj_se=.SEsharedgamw(bet=bet,newtht=newtht,n_eve=n_eve,etime=uniq_tim,
H=c(H_bet_x),h0=h0,censor=censor,time=time,X=X,WE=wei)
seho=adjj_se$seho
adjse=c(adjj_se$se);vcov2=cph1$var;vcov=adjj_se$vco;fittype<-c(1)}
if(typ=="Shr"){
resinteracmat<-apply(as.matrix(X),2,interacmat,u=X)
Xoxo<-matrix(resinteracmat,data.n1,ncovar_coef^2)
adjj_se=SE.sharedgam(bet=bet,newtht=newtht,n_eve=n_eve,H_bet_x=H_bet_x,censor=censor,
X=X,W=log(z),RI=RI,ind.haz=ind.haz,tau=tau,clustind=clustind,Xoxo=Xoxo,we1=wei)
seho=NULL
adjse=c(adjj_se$SE);vcov=adjj_se$vcovb;vcov2=cph1$var;fittype<-NULL}}
colnames(vcov) <- rownames(vcov) <- colnames(X)
colnames(vcov2) <- rownames(vcov2) <- colnames(X)
res <-list(coefficients=bet,frailparest=newtht,
vcov = vcov,stderr=adjse,loglik0=cph0$loglik[2],loglik=cph1$loglik[2],
Iloglilk=lik,bhaz=seho,X=X,time=time,censor=censor,
resid=cph1$residuals,lin.prid=cph1$linear.predictors,
frail=z,iteration=fittr$iterations,e.time=uniq_tim,n.event=n_eve,fittype=fittype,
converg = fittr$convergence)
res$call <- match.call()
class(res) <- c("shrgamsp")
res
}
########End Univariate and Shared gamma############
#######################################
#
###################################################################
###############Utilities for Bivariate Log normal frailty##########
###################################################################
llpennlognGeni<-function (par,newtht,censor,X,RI,n_eve,ind.haz,i1,i2){
ncoef=ncol(X);n1=nrow(X)
a=newtht[1];roh=newtht[2]
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
llpennlognGeni=(-sum(censor*(c(x_bet_w)))+sum(n_eve*log(svexp_bet_xo))+
(1/(2*a*(1-roh^2)))*sum(W1^2+W2^2-2*roh*W1*W2))
Dw<-(-(censor)+c(H_bet_xw))
Dw[i1]<-Dw[i1]+(W1-roh*W2)*(1/(a*(1-roh^2)))
Dw[i2]<-Dw[i2]+(W2-roh*W1)*(1/(a*(1-roh^2)))
attr(llpennlognGeni,"gradient")<-c(c(-sumx+dbsvexp_bet_xo),c(Dw))
llpennlognGeni
}

Hesfunc0<-function(par,X,RI,n_eve,ind.haz,i1,i2,i3){
n4=ncol(X);n=nrow(X);bet=par[array(1:n4)]
W=par[(1+n4):(length(par))]
W1=W[i1];W2=W[i2]
x_bet<-c(X%*%bet+W)
expx_bet<-exp(c(x_bet))
svexp_bet_xo=as.vector(RI%*%(exp(x_bet)))
H0<-cumsum(c(n_eve/svexp_bet_xo));as0=H0[ind.haz]
ak=cumsum(c((n_eve/svexp_bet_xo^2)));as2=ak[ind.haz]
as1=(as2*c(expx_bet^2)-(as0*c(expx_bet)))
as3=as2[i3]*(expx_bet[i1])*(expx_bet[i2])
xx<-matrix(c(W1,W2,as1[i1],as1[i2],as3),(n/2),5)
xx
}

.sdLliklogncv = function (thetaa, xx){
xx=as.matrix(xx)
W1=xx[,1];W2=xx[,2];S1=xx[,3];S2=xx[,4];S3=xx[,5]
a = abs(thetaa[1]) #var
e = thetaa[2] #corr
n=nrow(xx)
P11<-(S1*S2-(1/(a*(1-e^2)))*(S1+S2)+
(1/(a^2*(1-e^2)^2))-S3^2-2*(e/(a*(1-e^2)))*S3-(e^2/(a^2*(1-e^2)^2)))
daP11<-( (1/(a^2*(1-e^2)))*(S1+S2)-(2/(a^3*(1-e^2)^2))+2*(e/(a^2*(1-e^2)))*S3+
(2*e^2/(a^3*(1-e^2)^2)))
deP11<- ( -(2*a*e/(a*(1-e^2))^2)*(S1+S2)+((4*a^2*(1-e^2)*e)/(a^2*(1-e^2)^2)^2)+
2*(-1/(a*(1-e^2))-2*(e^2*a)/(a*(1-e^2))^2)*S3-
(e*2/(a^2*(1-e^2)^2)+(4*a^2*(1-e^2)*e^3)/(a^2*(1-e^2)^2)^2))
d2aP11<-( -(2/(a^3*(1-e^2)))*(S1+S2)+(6/(a^4*(1-e^2)^2))-4*(e/(a^3*(1-e^2)))*S3-
(6*e^2/(a^4*(1-e^2)^2)))
daRP11<-( ((2*a^2*e)/(a^2*(1-e^2))^2)*(S1+S2)-(8*(a^3*(1-e^2)*e)/(a^3*(1-e^2)^2)^2)+
2*(1/(a^2*(1-e^2))+2*(a^2*e^2)/(a^2*(1-e^2))^2)*S3+
(4*e/(a^3*(1-e^2)^2)+8*(a^3*(1-e^2)*e^3)/(a^3*(1-e^2)^2)^2))
deP11<- ( -(2*a*e/(a*(1-e^2))^2)*(S1+S2)+((4*a^2*(1-e^2)*e)/(a^2*(1-e^2)^2)^2)+
2*(-1/(a*(1-e^2))-2*(e^2*a)/(a*(1-e^2))^2)*S3-
( e*2/(a^2*(1-e^2)^2)+(4*a^2*(1-e^2)*e^3)/(a^2*(1-e^2)^2)^2))
d2eP11<- ( -(2*a/(a*(1-e^2))^2 + 8*a^2*e^2*(a*(1-e^2))/(a*(1-e^2))^4)*(S1+S2)+
((4 * a^2 * (1 - e^2) - 4 * a^2 * (2 * e) * e)/(a^2 * (1 - e^2)^2)^2 +
(4 * a^2 * (1 - e^2) * e) * (2 * (a^2 * (2 * (2 * e * (1 - e^2))) * (a^2 * (1 - e^2)^2)))/((a^2 * (1 - e^2)^2)^2)^2) +
(-(2 * (a * (2 * e)/(a * (1 - e^2))^2 + (2 * (2 * e * a)/(a * (1 - e^2))^2 + 2 * (e^2 * a) * (2 * (a * (2 * e) * (a * (1 -
    e^2))))/((a * (1 - e^2))^2)^2))))*S3-
(2/(a^2 * (1 - e^2)^2) + e * 2 * (a^2 * (2 * (2 * e * (1 - e^2))))/(a^2 *
    (1 - e^2)^2)^2 + ((4 * a^2 * (1 - e^2) * (3 * e^2) - 4 *
    a^2 * (2 * e) * e^3)/(a^2 * (1 - e^2)^2)^2 + (4 * a^2 * (1 -
    e^2) * e^3) * (2 * (a^2 * (2 * (2 * e * (1 - e^2))) * (a^2 *
    (1 - e^2)^2)))/((a^2 * (1 - e^2)^2)^2)^2)))
D2a=((-(2 * (2 * a) * (1 - e^2)/(2 * a^2 * (1 - e^2))^2))*sum((W1^2+W2^2-2*e*W1*W2))+
n/a^2-(1/2)*sum(d2aP11/P11-daP11^2/P11^2))
DaR=((2 * a^2 * (2 * e)/(2 * a^2 * (1 - e^2))^2)*sum((W1^2+W2^2-2*e*W1*W2))-
(1/(a^2*(1-e^2)))*sum(W1*W2)-(1/2)*sum(daRP11/P11-(daP11*deP11)/P11^2))
D2R=( (-(4 * a/(2 * a * (1 - e^2))^2 + (4 * a * e) * (2 * (2 * a * (2 *
    e) * (2 * a * (1 - e^2))))/((2 * a * (1 - e^2))^2)^2))*sum((W1^2+W2^2-2*e*W1*W2))+
((2*a*e)/(a*(1-e^2))^2)*sum(W1*W2)+(a * (2 * e)/(a * (1 - e^2))^2)*sum(W1*W2)+
n*( 1/(1-e^2)+2*e^2/(1-e^2)^2)-(1/2)*sum(d2eP11/P11-deP11^2/P11^2))
hesi=matrix(c(D2a,DaR,DaR,D2R),2,2)
-hesi
}
#
#
Inv.HesPPL<-function(W,bet,newtht,X,RI,INC,n_eve,ind.haz,indx){
ncoef=ncol(X);n=nrow(X)
i1=INC[,1];i2=INC[,2];i3=INC[,3]
x_bet_w<-X%*%bet+W
expx_bet<-exp(c(x_bet_w))
svexp_bet_xo=as.vector(RI%*%(exp(x_bet_w)))
H0<-cumsum(c(n_eve/svexp_bet_xo))
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
a=newtht[1];roh=newtht[2]
diag(D2W)<-diag(D2W)+(1/(a*(1-roh^2)))
D2W[cbind(i1,i2)]<-D2W[cbind(i1,i2)]-(roh/(a*(1-roh^2)))
D2W[cbind(i2,i1)]<-D2W[cbind(i2,i1)]-(roh/(a*(1-roh^2)))
AB<-try(chol(D2W),silent=TRUE)
if(length(attr(AB,"class"))==0){INVE<-chol2inv(AB);diet<-2*sum(log(diag(AB)))}
if(length(attr(AB,"class"))!=0){INVE<-solve(D2W);diet<-log(det(D2W))}
ij=cbind(i1,i2);as3=INVE[ij];ij=cbind(i2,i1);as4=INVE[ij];as2=diag(INVE)
Hii=sum(as2);Hij=sum(as3)
if(is.infinite(diet)){
ij=cbind(i1,i2)
as3=D2W[ij];as2=diag(D2W)
diet=sum(log(as2[i1]*as2[i2]-as3^2))}
list(Hii=Hii,Hij=Hij,ddet=diet)
}

.Etsula= function (thetaa,xx){
xx=as.matrix(xx);a = abs(thetaa[1]);r = thetaa[2];n = nrow(xx)
daDI=c(-1/(a^2*(1-r^2)))*matrix(c(1,-r,-r,1),2,2)
drDI=c(1/(a*(1-r^2)^2))*matrix(c(2*r,-(1+r^2),-(1+r^2),2*r),2,2)
part1=c(0.5*(2*n/a^2-c(2/(a^3*(1-r^2)))*sum(xx[,1]+xx[,3]-2*r*xx[,2])),
0.5*( (1/(a^2*(1-r^2)^2))*sum(2*r*(xx[,1]+xx[,3])-2*(1+r^2)*xx[,2])),
0.5*((2*n*(1+r^2)/(1-r^2)^2)-c(1/(a*(1-r^2)^3))*sum((2+6*r^2)*(xx[,1]+xx[,3])-2*(6*r+2*r^3)*xx[,2])))
kr <- apply(xx,1,newmoppp,daDI=daDI,drDI=drDI)
part=c(part1+colSums(t(kr)))
hesi=matrix(c(part[1],part[2],part[2],part[3]),2,2)
hesi}

SEbothlognph= function(W,bet,newtht,X,RI,INC,n_eve,ind.haz,indx,Xoxo){
ncoef=length(bet);n1=length(W);n=n1/2
a=newtht[1];r=newtht[2]
i1=INC[,1];i2=INC[,2];i3=INC[,3]
x_bet_w<-X%*%bet+W
expx_bet<-exp(c(x_bet_w))
svexp_bet_xo=as.vector(RI%*%(exp(x_bet_w)))
dbpart1svexp=(RI%*%(c(exp(x_bet_w))*X))
af=c(n_eve/svexp_bet_xo)
H0<-cumsum(c(n_eve/svexp_bet_xo))
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
diag(D2W)<-diag(D2W)+(1/(a*(1-r^2)))
D2W[cbind(i1,i2)]<-D2W[cbind(i1,i2)]-(r/(a*(1-r^2)))
D2W[cbind(i2,i1)]<-D2W[cbind(i2,i1)]-(r/(a*(1-r^2)))
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
se=sqrt(diag(INVE));SEofbetandsig=se[1:ncoef];er1=array(1:ncoef)
inx=cbind(rep(er1,ncoef),rep(er1,each=ncoef))
vcovb=matrix(c(INVE[inx]),ncoef,ncoef);HES<-INVE<-AB<-NULL
ABC<-try(chol(D2W),silent=TRUE)
if(length(attr(ABC,"class"))==0){INV<-chol2inv(ABC)}
if(length(attr(ABC,"class"))!=0){INV<-solve(D2W)}
ij=cbind(i1,i2);ji=cbind(i2,i1)
as3=INV[ij];as2=diag(INV);xx0=matrix(c(as2[i1],as3,as2[i2]),n,3);ABC<-D2W<-INV<-NULL
haser=.Etsula(thetaa=newtht,xx=xx0);INV=solve(haser);dh=diag(INV)
if(any(dh<0)){xx=Hesfunc0(par=c(bet,W),X=X,RI=RI,n_eve=n_eve,ind.haz=ind.haz,i1=i1,i2=i2,i3=i3)
haser1=.sdLliklogncv(thetaa=newtht,xx=xx);INV1=solve(haser1);dh0=diag(INV1);setht=sqrt(dh0)
if(dh[1]>0){setht[1]=sqrt(dh[1])}else{if(haser[1,1]>0){
setht[1]=sqrt(c(1/haser[1,1]))}}}else{setht=sqrt(dh)}
if(setht[2]>2){xx=Hesfunc0(par=c(bet,W),X=X,RI=RI,n_eve=n_eve,ind.haz=ind.haz,i1=i1,i2=i2,i3=i3)
haser1=.sdLliklogncv(thetaa=newtht,xx=xx);INV1=solve(haser1)
dh0=diag(INV1);setht0=sqrt(dh0);setht=c(max(setht0[1],setht[1]),setht0[2])}
list(SE=SEofbetandsig,vcovb=vcovb,setht=setht)
}
#
###################################################################
############### End Utilities for Bivariate Log normal frailty##########
#############################################################################
#
#
######### Function used To generate data from bcfrailph##########
#################################################################
genbcfrail<-function(psize,cenr,cent1,cent2,beta,frailty,fpar,bhaz,bhazpar,
covartype,covarpar,inputcovar,covar1,covar2,comncovar){
n<-psize; n1=n*2;IID=array(1:n1);PID=1;e1=array(1:n);indic2=2*e1;indic1=indic2-1
PID[indic1]=e1;PID[indic2]=e1
if(frailty==c("gamma")){
lam=1/fpar[1];k0=fpar[2]/(fpar[1]);k=(1-fpar[2])/fpar[1]
y0=rgamma(n,shape=k0,scale=1/lam)
y1=rgamma(n,shape=k,scale=1/lam);y2=rgamma(n,shape=k,scale=1/lam)
if(fpar[2]==0){y1=rgamma(n1,shape=k,scale=1/lam);z1=y1[indic1];z2=y1[indic2]}else{
z1=(y0+y1);z2=(y0+y2)}}
if(frailty==c("lognormal")){
W=rbivnorm(psize=psize,ssq=fpar[1],r=fpar[2])
z1=exp(W[,1]);z2=exp(W[,2])}
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
if(length(beta)==0){RW=NULL
dataa=cbind(IID=IID,PID=PID,time=time,censor=censor)
dataa=as.data.frame(dataa)}
Y=matrix(c(time,censor),n1,2)
list(dataa=dataa,X=RW,Y=Y)
}
#
#########End generate data from bcfrailph##########