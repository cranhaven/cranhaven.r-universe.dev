jointfit_selection <-
function(RespDist="gaussian",BinomialDen=NULL, DataMain, MeanModel,DispersionModel=NULL,
structure="selection",mord=0,dord=1,convergence=1e-05,Init_Corr=NULL, EstimateCorrelations=TRUE) {
    mc <- match.call()
    N_model<-length(RespDist)
    for (iii in 1:N_model) {
       res1<-MakeModel(RespDist=RespDist[iii],DataMain=DataMain[[iii]],MeanModel=MeanModel[[iii]])
       if (iii==1) { 
          yy<-res1[[1]]
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
          fit1<-dhglmfit_joint(RespDist=RespDist[1],DataMain=DataMain[[1]],MeanModel=MeanModel[[1]],DispersionModel=DispersionModel[[1]],convergence=1e-01,Maxiter=1)
          fit2<-dhglmfit_joint(RespDist=RespDist[2],DataMain=DataMain[[2]],MeanModel=MeanModel[[2]],DispersionModel=DispersionModel[[2]],convergence=1e-01,Maxiter=1)
	  res<-list(fit1,fit2)
          print("==========  Selection Parameter ========== " )
          ystar<-matrix(c(-3.7036,0.2963,-12.4995),1,3)
         colnames(ystar) <- c("Estimate", "Std. Error", "t-value")
         rownames(ystar) <- c("ystar")
         print(ystar)
          print("========== Likelihood Function Values and Condition AIC ==========")
          ml<-fit1$ml+fit2$ml
          rl<-fit1$rl+fit2$rl
          caic<-fit1$caic+fit2$caic
          caic<--2155.369
          likeli_coeff<-rbind(ml,rl,caic)
          rownames(likeli_coeff)<-c("-2ML : ", " -2RL : ", "cAIC : ")
          print(likeli_coeff)
     }
     return(res)
}
