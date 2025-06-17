cma.uni.mix <-
function(dat,delta=0,conf.level=0.95,optimizer=c("bobyqa","Nelder_Mead","optimx"),
                      mix.pkg=c("lme4","nlme"),random.indep=FALSE,random.var.equal=TRUE,u.int=FALSE)
{
  N<-length(dat)
  K<-length(dat[[1]])
  
  coe<-rep(c("A","C","B"),each=N*K)
  Sub<-factor(rep(1:N,K))
  Sub.all<-rep(Sub,3)
  
  zc<-qnorm(1-(1-conf.level)/2)
  
  coe.re<-matrix(NA,6,4)
  colnames(coe.re)<-c("Estimate","LB","UB","SE")
  rownames(coe.re)<-c("A","C","B","C2","AB.prod","AB.diff")
  
  # Estimate A and C'
  At<-matrix(NA,length(dat),length(dat[[1]]))
  colnames(At)<-paste("Session",1:K)
  C2t<-At
  for(i in 1:length(dat))
  {
    for(k in 1:length(dat[[i]]))
    {
      dd<-dat[[i]][[k]]
      re<-cma.uni.delta(dd,delta=0)
      
      At[i,k]<-re$D[1,1]
      C2t[i,k]<-re$Coefficients[4,1]
    }
  }
  if(optimizer[1]=="optimx")
  {
    fit.C2<-lmer(c(C2t)~(1|Sub),control= lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
  }else
  {
    fit.C2<-lmer(c(C2t)~(1|Sub),control=lmerControl(optimizer=optimizer[1]))
  }
  C2fix<-summary(fit.C2)$coefficients[1]
  s2.C2<-c(as.numeric(VarCorr(fit.C2)),attr(VarCorr(fit.C2),"sc")^2)
  e.C2<-C2fix+mean(ranef(fit.C2)$Sub[,"(Intercept)"])
  coe.re[4,4]<-sqrt(s2.C2[1]/N+s2.C2[2]/(N*K))
  coe.re[4,1:3]<-c(C2fix,C2fix-zc*coe.re[4,4],C2fix+zc*coe.re[4,4])
  
  # Given value of delta, estimate B and C  
  Ct<-matrix(NA,length(dat),length(dat[[1]]))
  colnames(Ct)<-paste("Session",1:K)
  Bt<-Ct
  for(i in 1:length(dat))
  {
    for(k in 1:length(dat[[i]]))
    {
      dd<-dat[[i]][[k]]
      re<-cma.uni.delta(dd,delta=delta)
      
      Bt[i,k]<-re$D[2,2]
      Ct[i,k]<-re$D[1,2]
    }
  }
  coet<-c(c(At),c(Ct),c(Bt))  
  
  if(!random.var.equal)
  {
    if(random.indep)
    {
      vec.At<-c(At)
      vec.Bt<-c(Bt)
      vec.Ct<-c(Ct)
      if(mix.pkg[1]=="nlme")
      {
        if(optimizer[1]=="optimx")
        {
          fit.A<-lme(vec.At~1,random=~1|Sub,control=lmeControl(opt="optim"))
          fit.B<-lme(vec.Bt~1,random=~1|Sub,control=lmeControl(opt="optim"))
          fit.C<-lme(vec.Ct~1,random=~1|Sub,control=lmeControl(opt="optim"))
        }else
        {
          fit.A<-lme(vec.At~1,random=~1|Sub)
          fit.B<-lme(vec.Bt~1,random=~1|Sub)
          fit.C<-lme(vec.Ct~1,random=~1|Sub)
        }
        Afix<-as.numeric(summary(fit.A)$coefficients$fixed[1])
        Bfix<-as.numeric(summary(fit.B)$coefficients$fixed[1])
        Cfix<-as.numeric(summary(fit.C)$coefficients$fixed[1])
        Phi<-diag(c(as.numeric(VarCorr(fit.A)[1,1]),as.numeric(VarCorr(fit.B)[1,1]),as.numeric(VarCorr(fit.C)[1,1])))
        Lambda<-diag(c(as.numeric(VarCorr(fit.A)[2,1]),as.numeric(VarCorr(fit.B)[2,1]),as.numeric(VarCorr(fit.C)[2,1])))
        u<-cbind(as.matrix(ranef(fit.A)),as.matrix(ranef(fit.B)),as.matrix(ranef(fit.C)))
        colnames(u)<-c("A","B","C")
        b0<-c(Afix,Bfix,Cfix)
        cor.AB=cor.AC=cor.BC<-0
        LL<-as.numeric(logLik(fit.A,REML=FALSE)+logLik(fit.B,REML=FALSE)+logLik(fit.C,REML=FALSE))
      }else
      {
        if(optimizer[1]=="optimx")
        {
          fit.A<-lmer(vec.At~1+(1|Sub),control= lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
          fit.B<-lmer(vec.Bt~1+(1|Sub),control= lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
          fit.C<-lmer(vec.Ct~1+(1|Sub),control= lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
        }else
        {
          fit.A<-lmer(vec.At~1+(1|Sub),control= lmerControl(optimizer=optimizer[1]))
          fit.B<-lmer(vec.Bt~1+(1|Sub),control= lmerControl(optimizer=optimizer[1]))
          fit.C<-lmer(vec.Ct~1+(1|Sub),control= lmerControl(optimizer=optimizer[1]))
        }
        Afix<-as.numeric(summary(fit.A)$coefficients[1])
        Bfix<-as.numeric(summary(fit.B)$coefficients[1])
        Cfix<-as.numeric(summary(fit.C)$coefficients[1])
        Phi<-diag(c(as.numeric((attr(VarCorr(fit.A)[[1]],"stddev")^2)[1]),
                    as.numeric((attr(VarCorr(fit.B)[[1]],"stddev")^2)[1]),
                    as.numeric((attr(VarCorr(fit.C)[[1]],"stddev")^2)[1])))
        Lambda<-diag(c(as.numeric((attr(VarCorr(fit.A),"sc")^2)[1]),
                       as.numeric((attr(VarCorr(fit.B),"sc")^2)[1]),
                       as.numeric((attr(VarCorr(fit.C),"sc")^2)[1])))
        u<-cbind(as.matrix(ranef(fit.A)$Sub),as.matrix(ranef(fit.B)$Sub),as.matrix(ranef(fit.C)$Sub))
        colnames(u)<-c("A","B","C")
        b0<-c(Afix,Bfix,Cfix)
        cor.AB=cor.AC=cor.BC<-0
        LL<-as.numeric(logLik(fit.A,REML=FALSE)+logLik(fit.B,REML=FALSE)+logLik(fit.C,REML=FALSE))
      }
    }else
    {
      if(mix.pkg[1]=="nlme")
      {
        if(optimizer[1]=="optimx")
        {
          fit0<-lme(c(coet)~0+coe,random=~0+coe|Sub.all,control=lmeControl(opt="optim"))
        }else
        {
          fit0<-lme(c(coet)~0+coe,random=~0+coe|Sub.all)
        }
        fit<-NULL
        try(fit<-update(fit0,weights=varIdent(form=~1|factor(coe))))
        if(is.null(fit))
        {
          warning("Equal-varaince assumption in random error is applied instead.")
          fit<-fit0
          Lambda<-diag(rep(summary(fit)$sigma^2,3))
        }else
        {
          Lambda<-diag(c((summary(fit)$sigma*coef(fit$modelStruct$varStruct, unconstrained=FALSE, allCoef=TRUE)[1])^2,
                         (summary(fit)$sigma*coef(fit$modelStruct$varStruct, unconstrained=FALSE, allCoef=TRUE)[3])^2,
                         (summary(fit)$sigma*coef(fit$modelStruct$varStruct, unconstrained=FALSE, allCoef=TRUE)[2])^2))
        }
        Afix<-as.numeric(summary(fit)$coefficients$fixed[1])
        Bfix<-as.numeric(summary(fit)$coefficients$fixed[2])
        Cfix<-as.numeric(summary(fit)$coefficients$fixed[3])
        b0<-c(Afix,Bfix,Cfix)
        u<-as.matrix(ranef(fit))
        Phi<-diag(c(as.numeric(VarCorr(fit)[1,1]),as.numeric(VarCorr(fit)[2,1]),as.numeric(VarCorr(fit)[3,1])))
        cor.AB<-as.numeric(VarCorr(fit)[2,3])
        cor.AC<-as.numeric(VarCorr(fit)[3,3])
        cor.BC<-as.numeric(VarCorr(fit)[3,4])
        Phi[1,2]=Phi[2,1]<-cor.AB*sqrt(Phi[1,1]*Phi[2,2])
        Phi[1,3]=Phi[3,1]<-cor.AC*sqrt(Phi[1,1]*Phi[3,3])
        Phi[2,3]=Phi[3,2]<-cor.BC*sqrt(Phi[2,2]*Phi[3,3])
        LL<-as.numeric(logLik(fit,REML=FALSE))
      }else
      {
        warning("Equal-varaince assumption in random error is applied instead.")
        if(optimizer[1]=="optimx")
        {
          fit<-lmer(c(coet)~0+coe+(0+coe|Sub.all),control= lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
        }else
        {
          fit<-lmer(c(coet)~0+coe+(0+coe|Sub.all),control= lmerControl(optimizer=optimizer[1]))
        }
        u<-as.matrix(ranef(fit)$Sub.all)     
        cor.t<-1-c(attr(VarCorr(fit)[[1]],"correlation")-diag(diag(attr(VarCorr(fit)[[1]],"correlation"))))
        s<-0
        while(length(which(abs(cor.t)<1e-06))>0&s<20)
        {
          s<-s+1
          fit<-lmer(c(coet)~0+coe+(0+coe|Sub.all),control= lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
          cor.t<-1-c(attr(VarCorr(fit)[[1]],"correlation")-diag(diag(attr(VarCorr(fit)[[1]],"correlation"))))
        }
        b0<-c(summary(fit)$coefficients[1,1],summary(fit)$coefficients[2,1],summary(fit)$coefficients[3,1])
        Phi<-diag(c(as.numeric((attr(VarCorr(fit)[[1]],"stddev")^2)[1]),
                    as.numeric((attr(VarCorr(fit)[[1]],"stddev")^2)[2]),
                    as.numeric((attr(VarCorr(fit)[[1]],"stddev")^2)[3])))
        cor.AB<-attr(VarCorr(fit)[[1]],"correlation")[1,2]
        cor.AC<-attr(VarCorr(fit)[[1]],"correlation")[1,3]
        cor.BC<-attr(VarCorr(fit)[[1]],"correlation")[2,3]
        Phi[1,2]=Phi[2,1]<-cor.AB*sqrt(Phi[1,1]*Phi[2,2])
        Phi[1,3]=Phi[3,1]<-cor.AC*sqrt(Phi[1,1]*Phi[3,3])
        Phi[2,3]=Phi[3,2]<-cor.BC*sqrt(Phi[2,2]*Phi[3,3])
        Lambda<-diag(rep(attr(VarCorr(fit),"sc")^2,3))
        LL<-as.numeric(logLik(fit,REML=FALSE))
      }
    }
  }else
  {
    if(mix.pkg[1]=="nlme")
    {
      if(optimizer[1]=="optimx")
      {
        fit<-lme(c(coet)~0+coe,random=~0+coe|Sub.all,control=lmeControl(opt="optim"))        
      }else
      {
        fit<-lme(c(coet)~0+coe,random=~0+coe|Sub.all)
      }
      Afix<-as.numeric(summary(fit)$coefficients$fixed[1])
      Bfix<-as.numeric(summary(fit)$coefficients$fixed[2])
      Cfix<-as.numeric(summary(fit)$coefficients$fixed[3])
      b0<-c(Afix,Bfix,Cfix)
      u<-as.matrix(ranef(fit))
      Lambda<-diag(rep(summary(fit)$sigma^2,3))
      Phi<-diag(c(as.numeric(VarCorr(fit)[1,1]),as.numeric(VarCorr(fit)[2,1]),as.numeric(VarCorr(fit)[3,1])))
      if(random.indep)
      {
        cor.AB=cor.AC=cor.BC<-0
      }else
      {
        cor.AB<-as.numeric(VarCorr(fit)[2,3])
        cor.AC<-as.numeric(VarCorr(fit)[3,3])
        cor.BC<-as.numeric(VarCorr(fit)[3,4])
      }
      Phi[1,2]=Phi[2,1]<-cor.AB*sqrt(Phi[1,1]*Phi[2,2])
      Phi[1,3]=Phi[3,1]<-cor.AC*sqrt(Phi[1,1]*Phi[3,3])
      Phi[2,3]=Phi[3,2]<-cor.BC*sqrt(Phi[2,2]*Phi[3,3])
      LL<-as.numeric(logLik(fit,REML=FALSE))
    }else
    {
      if(optimizer[1]=="optimx")
      {
        fit<-lmer(c(coet)~0+coe+(0+coe|Sub.all),control= lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
        u<-as.matrix(ranef(fit)$Sub.all)     
        cor.t<-1-c(attr(VarCorr(fit)[[1]],"correlation")-diag(diag(attr(VarCorr(fit)[[1]],"correlation"))))
        s<-0
        while(length(which(abs(cor.t)<1e-06))>0&s<20)
        {
          s<-s+1
          fit<-lmer(c(coet)~0+coe+(0+coe|Sub.all),control= lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
          cor.t<-1-c(attr(VarCorr(fit)[[1]],"correlation")-diag(diag(attr(VarCorr(fit)[[1]],"correlation"))))
        }
      }else
      {
        fit<-lmer(c(coet)~0+coe+(0+coe|Sub.all),control= lmerControl(optimizer=optimizer[1]))
        u<-as.matrix(ranef(fit)$Sub.all)     
        cor.t<-1-c(attr(VarCorr(fit)[[1]],"correlation")-diag(diag(attr(VarCorr(fit)[[1]],"correlation"))))
        s<-0
        while(length(which(abs(cor.t)<1e-06))>0&s<20)
        {
          s<-s+1
          fit<-lmer(c(coet)~0+coe+(0+coe|Sub.all),control= lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
          cor.t<-1-c(attr(VarCorr(fit)[[1]],"correlation")-diag(diag(attr(VarCorr(fit)[[1]],"correlation"))))
        }
      }
      b0<-c(summary(fit)$coefficients[1,1],summary(fit)$coefficients[2,1],summary(fit)$coefficients[3,1])
      Phi<-diag(c(as.numeric((attr(VarCorr(fit)[[1]],"stddev")^2)[1]),
                  as.numeric((attr(VarCorr(fit)[[1]],"stddev")^2)[2]),
                  as.numeric((attr(VarCorr(fit)[[1]],"stddev")^2)[3])))
      if(random.indep)
      {
        cor.AB=cor.AC=cor.BC<-0
      }else
      {
        cor.AB<-attr(VarCorr(fit)[[1]],"correlation")[1,2]
        cor.AC<-attr(VarCorr(fit)[[1]],"correlation")[1,3]
        cor.BC<-attr(VarCorr(fit)[[1]],"correlation")[2,3]
      }
      Phi[1,2]=Phi[2,1]<-cor.AB*sqrt(Phi[1,1]*Phi[2,2])
      Phi[1,3]=Phi[3,1]<-cor.AC*sqrt(Phi[1,1]*Phi[3,3])
      Phi[2,3]=Phi[3,2]<-cor.BC*sqrt(Phi[2,2]*Phi[3,3])
      Lambda<-diag(rep(attr(VarCorr(fit),"sc")^2,3))
      LL<-as.numeric(logLik(fit,REML=FALSE))
    }
  }
  
  cor.comp<-matrix(NA,3,3)
  colnames(cor.comp)<-c("A","C","B")
  rownames(cor.comp)<-c("A","C","B")
  if(random.indep==TRUE)
  {
    cor.AC=cor.AB=cor.BC<-0
  }
  cor.comp[1,2]=cor.comp[2,1]<-cor.AC
  cor.comp[1,3]=cor.comp[3,1]<-cor.AB
  cor.comp[2,3]=cor.comp[3,2]<-cor.BC
  diag(cor.comp)<-rep(1,3)
  
  delta.est<-delta
  coe.re[c(1,3,2),4]<-sqrt(diag(Phi)/N+diag(Lambda)/(N*K))
  coe.re[c(1,3,2),1]<-b0
  coe.re[c(1,3,2),2]<-b0-zc*coe.re[c(1,3,2),4]
  coe.re[c(1,3,2),3]<-b0+zc*coe.re[c(1,3,2),4]  
  coe.re[5,1]<-b0[1]*b0[2]
  coe.re[5,4]<-sqrt((b0[1]*coe.re[3,4])^2+(b0[2]*coe.re[1,4])^2)
  coe.re[5,2:3]<-c(coe.re[5,1]-zc*coe.re[5,4],coe.re[5,1]+zc*coe.re[5,4])
  coe.re[6,1]<-C2fix-b0[3]
  coe.re[6,4]<-sqrt(coe.re[4,4]^2+coe.re[2,4]^2)
  coe.re[6,2:3]<-c(coe.re[6,1]-zc*coe.re[6,4],coe.re[6,1]+zc*coe.re[6,4])  
  
  var.comp<-data.frame(delta=delta,A=Phi[1,1],C=Phi[3,3],B=Phi[2,2],Lambda[1,1],Lambda[3,3],Lambda[2,2])
  colnames(var.comp)<-c("delta","Random(A)","Random(C)","Random(B)","Var(Error A)","Var(Error C)","Var(Error B)")
  
  s2.C2<-data.frame(C2=s2.C2[1],Error=s2.C2[2],Total=sum(s2.C2))
  colnames(s2.C2)<-c("Random(C')","Var(Error)","Var(C')")
  
  HL<-cma.h(dat,delta=delta,A.ik=At,B.ik=Bt,C.ik=Ct,b=b0,u=u,Phi=Phi,Lambda=Lambda,random.indep=random.indep,u.int=u.int)
  
  re<-list(delta=delta.est,Coefficients=coe.re,Cor.comp=cor.comp,Var.comp=var.comp,Var.C2=s2.C2,logLik=LL,HL=HL$h)
  
  return(re)
}
