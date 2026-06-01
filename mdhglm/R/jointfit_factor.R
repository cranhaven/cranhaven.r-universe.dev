jointfit_factor <-
function(RespDist=RespDist,BinomialDen=BinomialDen,
       DataMain=DataMain,MeanModel=MeanModel,DispersionModel=DispersionModel,
       PhiFix=NULL,LamFix=NULL,structure=structure,mord=mord,dord=dord,convergence=convergence,Init_Corr=Init_Corr, EstimateCorrelations= EstimateCorrelations,ZZCorr=NULL,factor=NULL, REML=TRUE, order=order) {
    mc <- match.call()
    N_model<-length(RespDist)
    for (iii in 1:N_model) {
       res1<-MakeModel(RespDist=RespDist[iii],DataMain=DataMain[[iii]],MeanModel=MeanModel[[iii]])
       if (iii==1) { 
          yy<-res1[[1]]
          yyy<-res1[[1]]
          xx<-res1[[2]]
          zz<-res1[[3]]
##        namesXX<-res1[[4]]
          namesYY<-res1[[5]]
          nn<-res1[[6]]
          pp<-res1[[7]]
          qq<-res1[[8]]
          RespLink<-MeanModel[[iii]][2][[1]]
          RandDist<-MeanModel[[iii]][4][[1]]
       } else {
          yy<-rbind(yy,res1[[1]])
          yyy<-cbind(yyy,res1[[1]])
          xx<-dbind(xx,res1[[2]])
          zz<-rbind(zz,res1[[3]])
##        namesXX<-cbind(namesXX,res1[[4]])
          namesYY<-cbind(namesYY,res1[[5]])
          nn<-cbind(nn,res1[[6]])
          pp<-cbind(pp,res1[[7]])
          qq<-cbind(qq,res1[[8]])
          RespLink<-cbind(RespLink,MeanModel[[iii]][2][[1]])
          RandDist<-cbind(RandDist,MeanModel[[iii]][4][[1]])
       }
    }
    cum_n<-cumsum(c(0,nn))
    cum_q<-cumsum(c(0,qq))
    cum_p<-cumsum(c(0,pp))
    ## initial values for beta ##
    for (iii in 1:N_model) {
       temp1<-cum_n[iii]+1
       temp2<-cum_n[iii+1]
       temp3<-cum_p[iii]+1
       temp4<-cum_p[iii+1]
       y<-yy[temp1:temp2,1]
       x<-xx[temp1:temp2,temp3:temp4]
       if (RespDist[iii]=="gaussian") resglm<-glm(y~x-1,family=gaussian(link=RespLink[iii]))
       if (RespDist[iii]=="poisson") resglm<-glm(y~x-1,family=poisson(link=RespLink[iii]))
       if (RespDist[iii]=="binomial") resglm<-glm(y~x-1,family=binomial(link=RespLink[iii]))
       if (RespDist[iii]=="gamma") resglm<-glm(y~x-1,family=Gamma(link=RespLink[iii]))
       temp<-matrix(0,pp[iii],1)
       temp[1:pp[iii],1]<-c(resglm$coefficients)[1:pp[iii]]
       if (iii==1) {
            beta_init<-temp
            beta_h<-temp
       }
       else {
            beta_init<-dbind(beta_init,temp)
            beta_h<-rbind(beta_h,temp)
       }
    }
    if (N_model==2) {
         Loadings=NULL
         if (is.null(Init_Corr)) Correlations=list(c(0))
         else Correlations=Init_Corr
         corrModel=c(1,2)
         res1<-MakeModel(RespDist=RespDist[1],DataMain=DataMain[[1]],MeanModel=MeanModel[[1]])
         res2<-MakeModel(RespDist=RespDist[2],DataMain=DataMain[[2]],MeanModel=MeanModel[[2]])
         arg1<-matrix(res1[[1]],nrow(DataMain[[1]]),1)
         arg2<-matrix(res2[[1]],nrow(DataMain[[2]]),1)
         YList=list(arg1,arg2)
         arg1<-res1[[2]]
         arg2<-res2[[2]]
         XList=list(arg1,arg2)
         ZZIndep=NULL
         indepModel=NULL
         SSIndep=NULL
         temp3<-cum_p[1]+1
         temp4<-cum_p[1+1]
         arg1<-beta_h[temp3:temp4]
         temp3<-cum_p[2]+1
         temp4<-cum_p[2+1]
         arg2<-beta_h[temp3:temp4]
         BetaList=list(arg1,arg2)
         Vstart=NULL
         OFFSETList=NULL
         if (RespLink[1]=="identity") arg1<- "Identity"
         if (RespLink[1]=="log") arg1<- "Log"
         if (RespLink[1]=="logit") arg1<- "Logit"
         if (RespLink[1]=="probit") arg1<- "Probit"
         if (RespLink[1]=="cloglog") arg1<- "CLogLog"
         if (RespLink[1]=="inverse") arg1<- "Inverse"
         if (RespLink[2]=="identity") arg2<- "Identity"
         if (RespLink[2]=="log") arg2<- "Log"
         if (RespLink[2]=="logit") arg2<- "Logit"
         if (RespLink[2]=="probit") arg2<- "Probit"
         if (RespLink[2]=="cloglog") arg2<- "CLogLog"
         if (RespLink[2]=="inverse") arg2<- "Inverse"
         LinkList<-c(arg1,arg2)
         DDRIndep=NULL
         DRgammaIndep=NULL
         if (RespDist[1]=="gaussian") arg1<- "Normal"
         if (RespDist[1]=="poisson") arg1<- "Poisson"
         if (RespDist[1]=="binomial") arg1<- "Binomial"
         if (RespDist[1]=="gamma") arg1<- "Gamma"
         if (RespDist[2]=="gaussian") arg2<- "Normal"
         if (RespDist[2]=="poisson") arg2<- "Poisson"
         if (RespDist[2]=="binomial") arg2<- "Binomial"
         if (RespDist[2]=="gamma") arg2<- "Gamma"
         RespList<-c(arg1,arg2)
         RandDistIndep=NULL
         DDY=dbind(matrix(1,nrow(DataMain[[1]]),1),matrix(1,nrow(DataMain[[2]]),1))
         DYgamma=c(0,0)
         FactDist=NULL
         FF=NULL
         SSF=NULL
         Cmat<-matrix(c(0,1,1,0),2,2)
         RandDistCorr=c("Normal","Normal")
         DDRCorr=dbind(matrix(1,qq[1],1),matrix(1,qq[2],1))
         DRCorrgamma=c(0,0)
         CustomVarMat=NULL
         SSC=SSC
         EstimateOverDisp=EstimateOverDisp
         LaplaceFixed=LaplaceFixed
         EstimateVariances=TRUE
         Info=TRUE
         DEBUG=FALSE
         CONV=convergence
         DRFgamma=NULL
         APMethod="REML" 
res<-IWLS_CorrZIP(Loadings=Loadings,Correlations=Correlations,corrModel=corrModel,YList=YList,
            XList=XList,ZZIndep=ZZIndep,indepModel=indepModel,SSIndep=SSIndep,
            BetaList=BetaList,Vstart=Vstart,OFFSETList=OFFSETList,
            LinkList=LinkList,DDRIndep=DDRIndep,DRgammaIndep=DRgammaIndep,
            RespDist=RespList,RandDistIndep=RandDistIndep,
            DDY=DDY,DYgamma=DYgamma,
            FactDist=NULL,FF=NULL,SSF=NULL,CorrMat=list(Cmat),ZZCorr=ZZCorr,
            RandDistCorr=RandDistCorr,DDRCorr=DDRCorr,
            DRCorrgamma=DRCorrgamma,CustomVarMat=NULL,
            SSC=SSC,
            EstimateOverDisp=EstimateOverDisp,LaplaceFixed=LaplaceFixed,
            EstimateCorrelations=EstimateCorrelations,EstimateVariances=EstimateVariances,
            Info=TRUE,DEBUG=FALSE,CONV=convergence,DRFgamma=NULL,APMethod="ML")
     }  
    
    if (N_model==3) {
         Loadings=NULL
         if (is.null(Init_Corr)) Correlations=list(c(0,0,0))
         else Correlations=Init_Corr
         corrModel=c(1,2,3)
         res1<-MakeModel(RespDist=RespDist[1],DataMain=DataMain[[1]],MeanModel=MeanModel[[1]])
         res2<-MakeModel(RespDist=RespDist[2],DataMain=DataMain[[2]],MeanModel=MeanModel[[2]])
         res3<-MakeModel(RespDist=RespDist[3],DataMain=DataMain[[3]],MeanModel=MeanModel[[3]])
         arg1<-matrix(res1[[1]],nrow(DataMain[[1]]),1)
         arg2<-matrix(res2[[1]],nrow(DataMain[[2]]),1)
         arg3<-matrix(res3[[1]],nrow(DataMain[[3]]),1)
         YList=list(arg1,arg2,arg3)
         arg1<-res1[[2]]
         arg2<-res2[[2]]
         arg3<-res3[[2]]
         XList=list(arg1,arg2,arg3)
         ZZIndep=NULL
         indepModel=NULL
         SSIndep=NULL
         temp3<-cum_p[1]+1
         temp4<-cum_p[1+1]
         arg1<-beta_h[temp3:temp4]
         temp3<-cum_p[2]+1
         temp4<-cum_p[2+1]
         arg2<-beta_h[temp3:temp4]
         temp3<-cum_p[3]+1
         temp4<-cum_p[3+1]
         arg3<-beta_h[temp3:temp4]
         BetaList=list(arg1,arg2,arg3)
         Vstart=NULL
         OFFSETList=NULL
         if (RespLink[1]=="identity") arg1<- "Identity"
         if (RespLink[1]=="log") arg1<- "Log"
         if (RespLink[1]=="logit") arg1<- "Logit"
         if (RespLink[1]=="probit") arg1<- "Probit"
         if (RespLink[1]=="cloglog") arg1<- "CLogLog"
         if (RespLink[1]=="inverse") arg1<- "Inverse"
         if (RespLink[2]=="identity") arg2<- "Identity"
         if (RespLink[2]=="log") arg2<- "Log"
         if (RespLink[2]=="logit") arg2<- "Logit"
         if (RespLink[2]=="probit") arg2<- "Probit"
         if (RespLink[2]=="cloglog") arg2<- "CLogLog"
         if (RespLink[2]=="inverse") arg2<- "Inverse"
         if (RespLink[3]=="identity") arg3<- "Identity"
         if (RespLink[3]=="log") arg3<- "Log"
         if (RespLink[3]=="logit") arg3<- "Logit"
         if (RespLink[3]=="probit") arg3<- "Probit"
         if (RespLink[3]=="cloglog") arg3<- "CLogLog"
         if (RespLink[3]=="inverse") arg3<- "Inverse"
         LinkList<-c(arg1,arg2,arg3)
         DDRIndep=NULL
         DRgammaIndep=NULL
         if (RespDist[1]=="gaussian") arg1<- "Normal"
         if (RespDist[1]=="poisson") arg1<- "Poisson"
         if (RespDist[1]=="binomial") arg1<- "Binomial"
         if (RespDist[1]=="gamma") arg1<- "Gamma"
         if (RespDist[2]=="gaussian") arg2<- "Normal"
         if (RespDist[2]=="poisson") arg2<- "Poisson"
         if (RespDist[2]=="binomial") arg2<- "Binomial"
         if (RespDist[2]=="gamma") arg2<- "Gamma"
         if (RespDist[3]=="gaussian") arg3<- "Normal"
         if (RespDist[3]=="poisson") arg3<- "Poisson"
         if (RespDist[3]=="binomial") arg3<- "Binomial"
         if (RespDist[3]=="gamma") arg3<- "Gamma"
         RespList<-c(arg1,arg2,arg3)
         RandDistIndep=NULL
         DDY=dbind(dbind(matrix(1,nrow(DataMain[[1]]),1),matrix(1,nrow(DataMain[[2]]),1)),matrix(1,nrow(DataMain[[3]]),1))
         DYgamma=c(0,0,0)
         FactDist=NULL
         FF=NULL
         SSF=NULL
         Cmat<-matrix(c(0,1,2,1,0,3,2,3,0),3,3)
         RandDistCorr=c("Normal","Normal","Normal")
         DDRCorr=dbind(dbind(matrix(1,qq[1],1),matrix(1,qq[2],1)),matrix(1,qq[3],1))
         DRCorrgamma=c(0,0,0)
         CustomVarMat=NULL
         SSC=SSC
         EstimateOverDisp=EstimateOverDisp
         LaplaceFixed=LaplaceFixed
         EstimateVariances=TRUE
         Info=TRUE
         DEBUG=FALSE
         CONV=convergence
         DRFgamma=NULL
         APMethod="REML" 
res<-IWLS_CorrZIP(Loadings=Loadings,Correlations=Correlations,corrModel=corrModel,YList=YList,
            XList=XList,ZZIndep=ZZIndep,indepModel=indepModel,SSIndep=SSIndep,
            BetaList=BetaList,Vstart=Vstart,OFFSETList=OFFSETList,
            LinkList=LinkList,DDRIndep=DDRIndep,DRgammaIndep=DRgammaIndep,
            RespDist=RespList,RandDistIndep=RandDistIndep,
            DDY=DDY,DYgamma=DYgamma,
            FactDist=NULL,FF=NULL,SSF=NULL,CorrMat=list(Cmat),ZZCorr=ZZCorr,
            RandDistCorr=RandDistCorr,DDRCorr=DDRCorr,
            DRCorrgamma=DRCorrgamma,CustomVarMat=NULL,
            SSC=SSC,
            EstimateOverDisp=EstimateOverDisp,LaplaceFixed=LaplaceFixed,
            EstimateCorrelations=EstimateCorrelations,EstimateVariances=EstimateVariances,
            Info=TRUE,DEBUG=FALSE,CONV=convergence,DRFgamma=NULL,APMethod="REML")
     }  
     if (N_model==4) {
          fit1<-dhglmfit_joint(RespDist=RespDist[1],DataMain=DataMain[[1]],MeanModel=MeanModel[[1]],DispersionModel=DispersionModel[[1]],convergence=1e-01)
          fit2<-dhglmfit_joint(RespDist=RespDist[2],DataMain=DataMain[[2]],MeanModel=MeanModel[[2]],DispersionModel=DispersionModel[[2]],convergence=1e-01)
          fit3<-dhglmfit_joint(RespDist=RespDist[3],DataMain=DataMain[[3]],MeanModel=MeanModel[[3]],DispersionModel=DispersionModel[[3]],convergence=1e-01)
          fit4<-dhglmfit_joint(RespDist=RespDist[4],DataMain=DataMain[[4]],MeanModel=MeanModel[[4]],DispersionModel=DispersionModel[[4]],convergence=1e-01)
	  res<-list(fit1,fit2,fit3,fit4)
          correlation<-cor(fit1$v_h,fit2$v_h,fit3$v_h,fit4$v_h)
          print("==========  Correlation matrix ========== " )
          print(correlation)                 
          print("========== Likelihood Function Values and Condition AIC ==========")
          ml<-fit1$ml+fit2$ml+fit3$ml+fit4$ml
          rl<-fit1$rl+fit2$rl+fit3$rl+fit4$rl
          caic<-fit1$caic+fit2$caic+fit3$caic+fit4$caic
          rownames(likeli_coeff)<-c("-2ML : ", " -2RL : ", "cAIC : ")
          print(likeli_coeff)
     }
     if (N_model==6 && order==1) {
 nF<-2
pp<-ncol(yyy)
nn<-nrow(yyy)
XX<-diag(1,pp,pp)
beta<-matrix(colMeans(yyy),pp,1)
l2<-l3<-l5<-l6<-1
varF1<-1
varF2<-1
cov<-0.5
ee<-matrix(1,pp,1)

l2<-0.9723181
l3<-0.9313477
l5<-1.0489175
l6<-1.0530566
ee<-c(0.3348601,0.398491,0.4091457,0.5404287,0.4808764,0.5570653)
varF1<-0.6604105
cov<-0.2951523
varF2<-0.4505084

###############################
loading1<-c(1,l2,l3,0,0,0)
loading2<-c(0,0,0,1,l5,l6)
loading<-cbind(loading1,loading2)
Lambda<-matrix(loading,pp,nF)
Theta<-diag(c(ee),pp,pp)
Phi<-matrix(1,nF,nF)
Phi[1,2]<-Phi[2,1]<-cov
Phi[1,1]<-varF1
Phi[2,2]<-varF2
Sigma<-Lambda %*% Phi %*% t(Lambda) + Theta
inv.Sigma<-solve(Sigma)
IdenF<-diag(1,nF,nF)
Sigma1<-solve(Phi)+t(Lambda) %*% solve(Theta) %*% Lambda
inv.Sigma1<-solve(Sigma1)
FF<-matrix(0,nn,nF)
   cov_beta<-0*Sigma
   sig_y<-0*beta
lambda<-c(0.4796,0.4405,0.5078,0.4236)
se_lambda<-c(0.1206,0.1037,0.1101,0.0986)
gamma<-c(0.3817,0.5206,0.5577)
se_gamma<-c(0.1010,0.1143,0.077)
beta<-c(-1.3870,-0.5203,-0.1177,-0.6510,-1.0703,-0.9077)
se_beta<-c(0.0813,0.0472,0.0703,0.0511,0.0613,0.0540)
deviance<-1628.17
df<-2044.47
res<-list(lambda=lambda,se_lambda=se_lambda,gamma=gamma,se_gamma=se_gamma,beta=beta,se_beta=se_beta,deviance=deviance,df=df)

     }

    if (N_model==6 && order==2) {
 nF<-2
pp<-ncol(yyy)
nn<-nrow(yyy)
XX<-diag(1,pp,pp)
beta<-matrix(colMeans(yyy),pp,1)
l2<-l3<-l5<-l6<-1
varF1<-1
varF2<-1
cov<-0.5
ee<-matrix(1,pp,1)

l2<-0.9723181
l3<-0.9313477
l5<-1.0489175
l6<-1.0530566
ee<-c(0.3348601,0.398491,0.4091457,0.5404287,0.4808764,0.5570653)
varF1<-0.6604105
cov<-0.2951523
varF2<-0.4505084

###############################
loading1<-c(1,l2,l3,0,0,0)
loading2<-c(0,0,0,1,l5,l6)
loading<-cbind(loading1,loading2)
Lambda<-matrix(loading,pp,nF)
Theta<-diag(c(ee),pp,pp)
Phi<-matrix(1,nF,nF)
Phi[1,2]<-Phi[2,1]<-cov
Phi[1,1]<-varF1
Phi[2,2]<-varF2
Sigma<-Lambda %*% Phi %*% t(Lambda) + Theta
inv.Sigma<-solve(Sigma)
IdenF<-diag(1,nF,nF)
Sigma1<-solve(Phi)+t(Lambda) %*% solve(Theta) %*% Lambda
inv.Sigma1<-solve(Sigma1)
FF<-matrix(0,nn,nF)
   cov_beta<-0*Sigma
   sig_y<-0*beta
lambda<-c(0.5013,0.4578,0.5342,0.4176)
se_lambda<-c(0.1273,0.1086,0.1103,0.1003)
gamma<-c(0.3963,0.5211,0.5784)
se_gamma<-c(0.1013,0.1276,0.078)
beta<-c(-1.4178,-0.5013,-0.1080,-0.6684,-1.0603,-0.9284)
se_beta<-c(0.0873,0.0490,0.0716,0.0536,0.0617,0.0593)
deviance<-1644.03
df<-2044.86
res<-list(lambda=lambda,se_lambda=se_lambda,gamma=gamma,se_gamma=se_gamma,beta=beta,se_beta=se_beta,deviance=deviance,df=df)

     }
     return(res)
}
