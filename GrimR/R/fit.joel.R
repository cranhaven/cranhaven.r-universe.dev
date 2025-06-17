#' Function fit.joel
fit.joel <-
function(Data,MR=NULL,cw=c("ccw","cw"),optimMR=FALSE){
    if(is.null(MR) && optimMR==FALSE)
       {
           print("Please provide an MR value!")
           return()
       }
    kapini<-1.0
    cw <- match.arg(cw[1], choices=c("ccw", "cw"))
    sgrad <- Data$S
    s <- sgrad/180.0*pi
    esgrad<-Data$MS
    if (cw=="cw"){ 
        esgrad<- -esgrad
        if(!is.null(MR)) MR<--MR
    }
    es<-esgrad/180.0*pi
    ss<- as.vector(sin(s))
    cs<- as.vector(cos(s))
    s2s<- as.vector(sin(2*s))
    c2s<-as.vector( cos(2*s))
    if(optimMR==TRUE){#screen for a good starting value for MR in the range -22 to +22 deg.
        mineig5<-1e20
        for (i in -22:22){
            MR<-i
            esgrad0<-esgrad-MR
            es0<-esgrad0/180.0*pi
            c2es<-as.vector(cos(2*es0))
            s2es<-as.vector(sin(2*es0))
            t2es<-as.vector(tan(2*es0))
            datmat<-cbind(-3*t2es,2*cs,2*ss,c2s*t2es,s2s*t2es)
            svdres<-svd(datmat) #calculate startvalues by singular value decomposition (take the vector corresponding to the lowest eigenvalue)
            if(abs(svdres$d[5])<mineig5){
                mineig5<-abs(svdres$d[5])
                mini<-i
            }
        }
        MR<-mini
    }

    MRR<-MR/180*pi
    esgrad0<-esgrad-MR
    es0<-esgrad0/180.0*pi
    c2es<-as.vector(cos(2*es0))
    s2es<-as.vector(sin(2*es0))
    t2es<-as.vector(tan(2*es0))
    datmat<-cbind(-3*t2es,2*cs,2*ss,c2s*t2es,s2s*t2es)
    svdres<-svd(datmat)   
    parms<-svdres$v[,5]

    W0i<- 3*parms[1]-parms[4]*c2s-parms[5]*s2s
    V0i<- 2*parms[2]*cs+2*parms[3]*ss
    sigdelta<- (W0i*c2es+V0i*s2es)/sqrt(W0i^2+V0i^2)# cos of 2x the angle between ES and the estimate 
    sigdelta<-sign(sigdelta) 
    phimat <- matrix(c(parms[4]-parms[1],parms[5],parms[2],parms[5],-parms[4]-parms[1],parms[3],parms[2],parms[3],2*parms[1]),nrow=3,ncol=3) #set up dielectric tensor
    eigensols<-eigen(phimat,symmetric=TRUE) #determine its eigenvalues and -vectors
    eigenwerte<-eigensols$values
    Vang<- atan(sqrt(abs((eigenwerte[1]-eigenwerte[2])/(eigenwerte[2]-eigenwerte[3])))) #calculate angle V
    a1<-cos(Vang)*eigensols$vectors[,1]+sin(Vang)*eigensols$vectors[,3] #optical axes
    a2<-cos(Vang)*eigensols$vectors[,1]-sin(Vang)*eigensols$vectors[,3]
    phi1<- atan2(a1[2],a1[1]) #polar coordinates of optical axes
    phi2<- atan2(a2[2],a2[1])
    theta1<-acos(a1[3])
    theta2<-acos(a2[3])
    LL <-function(kap,p1,p2,t1,t2,mrl){
        q1<-((2*cos(t1)*cos(t2)-cos(p1-p2)*sin(t1)*sin(t2))/3)
        q2<-(cos(p1+p2)*sin(t1)*sin(t2))
        q3<-(sin(p1+p2)*sin(t1)*sin(t2))
        q4<-(cos(p1)*sin(t1)*cos(t2)+cos(p2)*cos(t1)*sin(t2))
        q5<-(sin(p1)*sin(t1)*cos(t2)+sin(p2)*cos(t1)*sin(t2))
        Wi<-(3*q1-q2*c2s-q3*s2s)
        Vi<-(2*q4*cs+2*q5*ss)
        Like<-  -(sum(sigdelta*(Wi*cos(2*(es-mrl))+Vi*sin(2*(es-mrl)))/sqrt(Wi*Wi+Vi*Vi)-1))/kap
        return(Like)
    }
    if(optimMR==TRUE)   
        res<-mle(LL,start=list(p1=phi1,p2=phi2,t1=theta1,t2=theta2,mrl=MRR),fixed=list(kap=1),method="BFGS",control=list(trace=TRUE))
    else
        res<-mle(LL,start=list(p1=phi1,p2=phi2,t1=theta1,t2=theta2),fixed=list(kap=1,mrl=MRR),method="BFGS",control=list(trace=TRUE))
    cres<-as.vector(coef(res)) 
    kapini<-eval(abs(LL(kap=cres[1],p1=cres[2],p2=cres[3],t1=cres[4],t2=cres[5],mrl=cres[6])/(length(es)-length(cres)+1+ifelse(optimMR==FALSE,1,0))))
    ese<-(sqrt(0.5)*sqrt(kapini)*180/pi)
    if(optimMR==TRUE)   
        res<-mle(LL,start=list(p1=cres[2],p2=cres[3],t1=cres[4],t2=cres[5],mrl=cres[6]),fixed=list(kap=kapini),method="BFGS",control=list(trace=TRUE))
    else
        res<-mle(LL,start=list(p1=cres[2],p2=cres[3],t1=cres[4],t2=cres[5]),fixed=list(kap=kapini,mrl=cres[6]),method="BFGS",control=list(trace=TRUE))
    cres<-as.vector(coef(res)) 
    coeffs<-c(coef(res)[2:5])
    covmat <-vcov(res)[1:4,1:4]

    cat("\n\n\nRotation desk direction\n\n")
    if (cw=="cw")print("clockwise")
    else print("counter-clockwise")
    cat("\n\n\nEstimated standard error\n\n")
    print(ese)
    cat("\n\n\nReference azimuth MR\n")
    if(optimMR) cat("MR has been refined\n\n")
    else cat("MR has not been refined\n\n")
    MR<-as.numeric(coef(res)[6]*180/pi)
    print(ifelse(cw=="ccw",MR,-MR))
    
    esgrad<-esgrad-coef(res)[6]*180/pi
    esgrad<-esgrad-180*floor(esgrad/180)
    esgrad2<-ifelse(esgrad>90,esgrad-90,esgrad+90)
    es<-esgrad/180.0*pi

    if (sin(coeffs[1])*sin(coeffs[3])>0){
        a1x<-"(cos(p1)*sin(t1))"
        a1y<-"(sin(p1)*sin(t1))"
        a1z<-"(cos(t1))"
    }
    else
    {
        a1x<-"(-cos(p1)*sin(t1))"
        a1y<-"(-sin(p1)*sin(t1))"
        a1z<-"(-cos(t1))"
    }

    deltaa1x<-deltaMethod(coeffs,a1x,vcov.=covmat,func="OA1y")
    deltaa1y<-deltaMethod(coeffs,a1y,vcov.=covmat,func="OA1z")
    deltaa1z<-deltaMethod(coeffs,a1z,vcov.=covmat,func="OA1x")

    if (sin(coeffs[2])*sin(coeffs[4])>0){
        a2x<-"(cos(p2)*sin(t2))"
        a2y<-"(sin(p2)*sin(t2))"
        a2z<-"(cos(t2))"
    }
    else
    {
        a2x<-"(-cos(p2)*sin(t2))"
        a2y<-"(-sin(p2)*sin(t2))"
        a2z<-"(-cos(t2))"
    }
    deltaa2x<-deltaMethod(coeffs,a2x,vcov.=covmat,func="OA2y")
    deltaa2y<-deltaMethod(coeffs,a2y,vcov.=covmat,func="OA2z")
    deltaa2z<-deltaMethod(coeffs,a2z,vcov.=covmat,func="OA2x")
    exp2V<-paste0("acos((",a1x,"*",a2x,"+",a1y,"*",a2y,"+",a1z,"*",a2z,"))*180/pi",sep="") # Angle 2V (degrees)
    signAB<-+1
    if (eval(parse(text=exp2V),as.list(coef(res)))>90){
        exp2V<-paste0("180-",exp2V,sep="") # Angle 2V (degrees)
        signAB<--1
    }
    delta2V<-deltaMethod(coeffs,exp2V,vcov.=covmat,func="2V")
    phitemp<-eval(parse(text=paste0("atan(",a1y,"/",a1x,")*180/pi",sep="")),as.list(coef(res)))
    if(phitemp>0){
        A1phi<-paste0("(atan(",a1y,"/",a1x,")*180/pi)",sep="")}
    else{
        A1phi<-paste0("(180+atan(",a1y,"/",a1x,")*180/pi)",sep="")
    }
    A1theta<-paste0("(acos(",a1z,")*180/pi)",sep="")
    deltaa1phi<-deltaMethod(coeffs,A1phi,vcov.=covmat,func="OA1 S")
    deltaa1theta<- deltaMethod(coeffs,A1theta,vcov.=covmat,func="OA1 ES")

    phitemp<-eval(parse(text=paste0("atan(",a2y,"/",a2x,")*180/pi",sep="")),as.list(coef(res)))
    if(phitemp>0){
        A2phi<-paste0("(atan(",a2y,"/",a2x,")*180/pi)",sep="")
    }
    else{
        A2phi<-paste0("(180+atan(",a2y,"/",a2x,")*180/pi)",sep="")
    }
    A2theta<-paste0("(acos(",a2z,")*180/pi)",sep="")
    deltaa2phi<-deltaMethod(coeffs,A2phi,vcov.=covmat,func="OA2 S")
    deltaa2theta<-deltaMethod(coeffs,A2theta,vcov.=covmat,func="OA2 ES")
   
    ONx<-paste0("(", a1y,"*",a2z,"-",a1z,"*",a2y,")",sep="")
    ONy<-paste0("(", a1z,"*",a2x,"-",a1x,"*",a2z,")",sep="")
    ONz<-paste0("(", a1x,"*",a2y,"-",a1y,"*",a2x,")",sep="")
    ONnorm<-paste0("sqrt(",ONx,"^2+",ONy,"^2+",ONz,"^2)",sep="")
    signy<-ifelse (eval(parse(text=ONy),as.list(coef(res)))>0,1,-1)
    ONx<-paste0("(",ONx,"/",ONnorm,"*",signy,")",sep="")
    ONy<-paste0("(",ONy,"/",ONnorm,"*",signy,")",sep="")
    ONz<-paste0("(",ONz,"/",ONnorm,"*",signy,")",sep="")
    deltaONx<-deltaMethod(coeffs,ONx,vcov.=covmat,func="ONy")
    deltaONy<-deltaMethod(coeffs,ONy,vcov.=covmat,func="ONz")
    deltaONz<-deltaMethod(coeffs,ONz,vcov.=covmat,func="ONx")

    phitemp<-eval(parse(text=paste0("atan(",ONy,"/",ONx,")",sep="")),as.list(coef(res)))
    if(phitemp>0){
        ONphi<-paste0("(atan(",ONy,"/",ONx,")*180/pi)",sep="")
    }
    else{
        ONphi<-paste0("(180+atan(",ONy,"/",ONx,")*180/pi)",sep="")
    }
    ONtheta<-paste0("(acos(",ONz,")*180/pi)",sep="")
    deltaONphi<-deltaMethod(coeffs,ONphi,vcov.=covmat,func="ON S")
    deltaONtheta<-deltaMethod(coeffs,ONtheta,vcov.=covmat,func="ON ES")

    ABx<-paste0("(",a1x,"+",signAB,"*",a2x,")",sep="")
    ABy<-paste0("(",a1y,"+",signAB,"*",a2y,")",sep="")
    ABz<-paste0("(",a1z,"+",signAB,"*",a2z,")",sep="")
    ABnorm<-paste0("sqrt(",ABx,"^2+",ABy,"^2+",ABz,"^2)",sep="")
    signy<-ifelse (eval(parse(text=ABy),as.list(coef(res)))>0,1,-1)
    ABx<-paste0("(",ABx,"/",ABnorm,"*",signy,")",sep="")
    ABy<-paste0("(",ABy,"/",ABnorm,"*",signy,")",sep="")
    ABz<-paste0("(",ABz,"/",ABnorm,"*",signy,")",sep="")
    
    deltaABx<-deltaMethod(coeffs,ABx,vcov.=covmat,func="ABy")
    deltaABy<-deltaMethod(coeffs,ABy,vcov.=covmat,func="ABz")
    deltaABz<-deltaMethod(coeffs,ABz,vcov.=covmat,func="ABx")

    phitemp<-eval(parse(text=paste0("atan(",ABy,"/",ABx,")*180/pi",sep="")),as.list(coef(res)))
    if(phitemp>0){
        ABphi<-paste0("(atan(",ABy,"/",ABx,")*180/pi)",sep="")
    }
    else{
        ABphi<-paste0("(180+atan(",ABy,"/",ABx,")*180/pi)",sep="")
    }
    
    ABtheta<-paste0("(acos(",ABz,")*180/pi)",sep="")
    deltaABphi<-deltaMethod(coeffs,ABphi,vcov.=covmat,func="AB S")
    deltaABtheta<-deltaMethod(coeffs,ABtheta,vcov.=covmat,func="AB ES")

    OBx<-paste0("(",a1x,"-",signAB,"*",a2x,")",sep="")
    OBy<-paste0("(",a1y,"-",signAB,"*",a2y,")",sep="")
    OBz<-paste0("(",a1z,"-",signAB,"*",a2z,")",sep="")
    OBnorm<-paste0("sqrt(",OBx,"^2+",OBy,"^2+",OBz,"^2)",sep="")
    signy<-ifelse (eval(parse(text=OBy),as.list(coef(res)))>0,1,-1)
    OBx<-paste0("(",OBx,"/",OBnorm,"*",signy,")",sep="")
    OBy<-paste0("(",OBy,"/",OBnorm,"*",signy,")",sep="")
    OBz<-paste0("(",OBz,"/",OBnorm,"*",signy,")",sep="")
    deltaOBx<-deltaMethod(coeffs,OBx,vcov.=covmat,func="OBy")
    deltaOBy<-deltaMethod(coeffs,OBy,vcov.=covmat,func="OBz")
    deltaOBz<-deltaMethod(coeffs,OBz,vcov.=covmat,func="OBx")

    phitemp<-eval(parse(text=paste0("atan(",OBy,"/",OBx,")*180/pi",sep="")),as.list(coef(res)))
    if(phitemp>0){
        OBphi<-paste0("(atan(",OBy,"/",OBx,")*180/pi)",sep="")
    }
    else{
        OBphi<-paste0("(180+atan(",OBy,"/",OBx,")*180/pi)",sep="")
    }
    OBtheta<-paste0("(acos(",OBz,")*180/pi)",sep="")
    deltaOBphi<-deltaMethod(coeffs,OBphi,vcov.=covmat,func="OB S")
    deltaOBtheta<-deltaMethod(coeffs,OBtheta,vcov.=covmat,func="OB ES")
    names(delta2V)<-c("Estimate", "SE", "CI_l","CI_u")
    cat("\n\n\nAxis angle 2V\n\n")
    print(delta2V)
    kartnames<-c(row.names(deltaa1z),row.names(deltaa1x),row.names(deltaa1y),row.names(deltaa2z),row.names(deltaa2x),row.names(deltaa2y),row.names(deltaONz),row.names(deltaONx),row.names(deltaONy),row.names(deltaABz),row.names(deltaABx),row.names(deltaABy),row.names(deltaOBz),row.names(deltaOBx),row.names(deltaOBy))
    kartest<-c(deltaa1z$Estimate,deltaa1x$Estimate,deltaa1y$Estimate,deltaa2z$Estimate,deltaa2x$Estimate,deltaa2y$Estimate,deltaONz$Estimate,deltaONx$Estimate,deltaONy$Estimate,deltaABz$Estimate,deltaABx$Estimate,deltaABy$Estimate,deltaOBz$Estimate,deltaOBx$Estimate,deltaOBy$Estimate)
    kartSE<-c(deltaa1z$SE,deltaa1x$SE,deltaa1y$SE,deltaa2z$SE,deltaa2x$SE,deltaa2y$SE,deltaONz$SE,deltaONx$SE,deltaONy$SE,deltaABz$SE,deltaABx$SE,deltaABy$SE,deltaOBz$SE,deltaOBx$SE,deltaOBy$SE)
    kartCIl<-c(deltaa1z$`2.5 %`,deltaa1x$`2.5 %`,deltaa1y$`2.5 %`,deltaa2z$`2.5 %`,deltaa2x$`2.5 %`,deltaa2y$`2.5 %`,deltaONz$`2.5 %`,deltaONx$`2.5 %`,deltaONy$`2.5 %`,deltaABz$`2.5 %`,deltaABx$`2.5 %`,deltaABy$`2.5 %`,deltaOBz$`2.5 %`,deltaOBx$`2.5 %`,deltaOBy$`2.5 %`)
    kartCIu<-c(deltaa1z$`97.5 %`,deltaa1x$`97.5 %`,deltaa1y$`97.5 %`,deltaa2z$`97.5 %`,deltaa2x$`97.5 %`,deltaa2y$`97.5 %`,deltaONz$`97.5 %`,deltaONx$`97.5 %`,deltaONy$`97.5 %`,deltaABz$`97.5 %`,deltaABx$`97.5 %`,deltaABy$`97.5 %`,deltaOBz$`97.5 %`,deltaOBx$`97.5 %`,deltaOBy$`97.5 %`)
    kart<-data.frame(kartnames,kartest,kartSE,kartCIl,kartCIu)
    names(kart)<-c("parameter","Estimate","SE","CI_l","CI_u")
    cat("\n\n\nCartesian coordinates of axes\n\n")
    print(kart)
    sphaername<-c(row.names(deltaa1phi),row.names(deltaa1theta),row.names(deltaa2phi),row.names(deltaa2theta),row.names(deltaONphi),row.names(deltaONtheta),row.names(deltaABphi),row.names(deltaABtheta),row.names(deltaOBphi),row.names(deltaOBtheta))
    sphaerEst<-c(deltaa1phi$Estimate,deltaa1theta$Estimate,deltaa2phi$Estimate,deltaa2theta$Estimate,deltaONphi$Estimate,deltaONtheta$Estimate,deltaABphi$Estimate,deltaABtheta$Estimate,deltaOBphi$Estimate,deltaOBtheta$Estimate)
    sphaerSE<-c(deltaa1phi$SE,deltaa1theta$SE,deltaa2phi$SE,deltaa2theta$SE,deltaONphi$SE,deltaONtheta$SE,deltaABphi$SE,deltaABtheta$SE,deltaOBphi$SE,deltaOBtheta$SE)
    sphaerCIl<-c(deltaa1phi$`2.5 %`,deltaa1theta$`2.5 %`,deltaa2phi$`2.5 %`,deltaa2theta$`2.5 %`,deltaONphi$`2.5 %`,deltaONtheta$`2.5 %`,deltaABphi$`2.5 %`,deltaABtheta$`2.5 %`,deltaOBphi$`2.5 %`,deltaOBtheta$`2.5 %`)
    sphaerCIu<-c(deltaa1phi$`97.5 %`,deltaa1theta$`97.5 %`,deltaa2phi$`97.5 %`,deltaa2theta$`97.5 %`,deltaONphi$`97.5 %`,deltaONtheta$`97.5 %`,deltaABphi$`97.5 %`,deltaABtheta$`97.5 %`,deltaOBphi$`97.5 %`,deltaOBtheta$`97.5 %`)
    prinname<-c("AB","OB","ON")
    prinS<-c(deltaABphi$Estimate,deltaOBphi$Estimate,deltaONphi$Estimate)
    prinABMSEW<-ifelse(cw=="ccw",deltaABtheta$Estimate+MR,-MR-deltaABtheta$Estimate)
    prinABMSEW<-prinABMSEW-180*floor(prinABMSEW/180)
    prinABMSNS<-ifelse(prinABMSEW>90,prinABMSEW-90,prinABMSEW+90)
    prinOBMSEW<-ifelse(cw=="ccw",deltaOBtheta$Estimate+MR,-MR-deltaOBtheta$Estimate)
    prinOBMSEW<-prinOBMSEW-180*floor(prinOBMSEW/180)
    prinOBMSNS<-ifelse(prinOBMSEW>90,prinOBMSEW-90,prinOBMSEW+90)
    prinONMSEW<-ifelse(cw=="ccw",deltaONtheta$Estimate+MR,-MR-deltaONtheta$Estimate)
    prinONMSEW<-prinONMSEW-180*floor(prinONMSEW/180)
    prinONMSNS<-ifelse(prinONMSEW>90,prinONMSEW-90,prinONMSEW+90)
    prinMSEW<-c(prinABMSEW,prinOBMSEW,prinONMSEW)
    prinMSNS<-c(prinABMSNS,prinOBMSNS,prinONMSNS)
    principal<-data.frame(prinname,prinS,prinMSEW,prinMSNS)
    names(principal)<-c("Axis","S", "MS(EW)", "MS(NS)")
    cat("\n\n\nPrincipal axes, spindle and extinction angles\n\n")
    print(principal)

    sphaer<-data.frame(sphaername,sphaerEst,sphaerSE,sphaerCIl,sphaerCIu)
    names(sphaer)<-c("Parameter","Estimate","SE","CI_l","CI_u")
    cat("\n\n\nAxes in spherical coordinates\n\n")
    print(sphaer)

    fesexp<-function(x){
        ss<- sin(x)
        cs<- cos(x)
        s2s<- sin(2*x)
        c2s<- cos(2*x)
        q1<-((2*cos(coeffs[3])*cos(coeffs[4])-cos(coeffs[1]-coeffs[2])*sin(coeffs[3])*sin(coeffs[4]))/3)
        q2<-(cos(coeffs[1]+coeffs[2])*sin(coeffs[3])*sin(coeffs[4]))
        q3<-(sin(coeffs[1]+coeffs[2])*sin(coeffs[3])*sin(coeffs[4]))
        q4<-(cos(coeffs[1])*sin(coeffs[3])*cos(coeffs[4])+cos(coeffs[2])*cos(coeffs[3])*sin(coeffs[4]))
        q5<-(sin(coeffs[1])*sin(coeffs[3])*cos(coeffs[4])+sin(coeffs[2])*cos(coeffs[3])*sin(coeffs[4]))
        Wi<-(3*q1-q2*c2s-q3*s2s)
        Vi<-(2*q4*cs+2*q5*ss)
        y<-0.5*atan(Vi/Wi)*180/pi
        y<-ifelse(y<0,y+180,y)
        return(y)
    }
    esexp<-fesexp(s)
    esexp2<-ifelse(esexp>90,esexp-90,esexp+90)
    deltaES<-esgrad-esexp
    deltaES<-deltaES-180*floor(deltaES/180)
    deltaES2<-esgrad-esexp2
    deltaES2<-deltaES2-180*floor(deltaES2/180)
    deltaES<-ifelse(cos(2*deltaES*pi/180)>cos(2*deltaES2*pi/180),deltaES,deltaES2)
    deltaES<-ifelse(deltaES<90,deltaES,deltaES-180)
    EScalc<-esgrad-deltaES
    
    Extinctions<-data.frame(sgrad,Data$MS,esgrad,EScalc,deltaES)
    names(Extinctions)<-c("S","MS","ES obs.", "ES calc.", "ES obs. - ES calc.")

    cat("\n\n\nMeasured and calculated extinction angles\n\n")
    print(Extinctions)
    #transform angles for S>180 to that of their antipodal points for plotting
    esgrad<-ifelse(sgrad<180,esgrad,180-esgrad)
    esgrad2<-ifelse(sgrad<180,esgrad2,180-esgrad2)
    esexp<-ifelse(sgrad<180,esexp,180-esexp)
    esexp2<-ifelse(sgrad<180,esexp2,180-esexp2)
    bg<-ifelse(sgrad<180,"white","red")
    sgrad<-ifelse(sgrad<180,sgrad,sgrad-180)


    Wulffdat<-list(sgrad=sgrad,bg=bg,esgrad=esgrad,esgrad2=esgrad2,esexp=esexp,esexp2=esexp2,A1phi=deltaa1phi$Estimate,A1theta=deltaa1theta$Estimate, A2phi=deltaa2phi$Estimate,A2theta=deltaa2theta$Estimate, ABphi=deltaABphi$Estimate,ABtheta=deltaABtheta$Estimate, ONphi=deltaONphi$Estimate,ONtheta=deltaONtheta$Estimate, OBphi=deltaOBphi$Estimate,OBtheta=deltaOBtheta$Estimate)

    
    mod<-list(coeffs=coeffs,covmat=covmat,ese=ese, delta2V=delta2V,kart=kart,sphaer=sphaer,principal=principal,Extinctions=Extinctions,Wulffdat=Wulffdat)
    return(mod)
}
