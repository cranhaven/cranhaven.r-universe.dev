data(porto)
AnalogSel(fire01meantemp)
tr1=0.9
tr2=0.9
fire01meantemp=na.omit(fire01meantemp)
u=fire01meantemp
blss=Margins.mod(tr1,tr2,u=fire01meantemp)

pp=blss$uvar_ext
uu=blss$val_ext
kk=blss$uvar

interh<-"casc" #"comb or "casc"
upobj=0.001
vtau<-cor.test(x=fire01meantemp[,1],y=fire01meantemp[,2],method="kendall")$estimate

jtres<-JT.KDE.ap(u2=fire01meantemp,pbas=0.01,pobj=upobj,beta=100,kk=kk,vtau=vtau,devplot=F,mar1=uu[,1],mar2=uu[,2],px=pp[,1],py=pp[,2],interh=interh)

plot(jtres$levelcurve)
wq0ri=jtres$wq0ri
wqobj<-na.omit(jtres$levelcurve)
wqobj<-data.frame(wqobj)
wq0ri<-jtres$wq0ri
wqobj[,1]=jitter(wqobj[,1])
plot(wqobj)
jtres$etaJT
jtres$chiJT

print("Conditional model fitting")
if(length(u[,1])<5000){pb=0.02;t.sim=0.98}
if(length(u[,1])>=5000){pb=0.01;t.sim=0.99}

condexres<-Cond.mod.ap(u2=fire01meantemp,tr1,tr2,tsim=t.sim,num.sim=10000,pobj=0.001,mar1=uu[,1],mar2=uu[,2],px=pp[,1],py=pp[,2],interh=interh)
