	## Copyright Eva M. Strucken 2021
	## estrucke@une.edu.au
	## eva.strucken@gmail.com
  ##
	## A script to run multiple lactation curve models and
	## extract selection criterions (RSE, R2, log likelihood,
	## AIC, AICC, and BIC.
	##
	## Arguments
	## ==================================================================
	## x: 		data frame containing londitudinal trait records
	##			and timepoints of record collection.
	## trait: 		specifies the column name containing longitudianl
	##			trait records.
	## dim:		specifies the column name containing timepoints
	## ======
	## Output
	## ==================================================================
	## modelnames:	provides names of models and number (order)
	## model:		    gives model formula, default starting parameters,
	##			        and other model specifications
	## bestmodel:	  gives model formula for best model for each
	##			        selection criterion
	## modeldescrip:	gives RSS, RSD, and F-value for each model
	## critbest:	  gives values for best model for all selection
	##			        criterions
	## critall:	  	gives values for all models sorted from top to
	##		        	bottom according to specified selection criterion
	## Error:		    gives a Warning model failed to converge
	## ModelParam:	gives a list with three tables with curve
	##		        	parameters for each converged model
	## summary*:  	give the summary of the model output, e.g. summary1
	## ==================================================================

AllCurves <- function(x,trait,dim){

#########################################
# specify character for exponential function
e=exp(1)
n=c(nrow(x))

#########################################
######## Lactation Curve Models #########

# (1) Michaelis-Menten
MM=try(nls(trait ~ (a*dim) /(b+dim) , data=x, na.action=na.exclude,
start=list(a=19.8,b=-1.65),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(MM)=="nls" & MM$convInfo$isConv==TRUE){
wMM=c(MM$convInfo$stopMessage,MM$convInfo$isConv)
coefMM=length(coef(MM))
R2MM=cor(trait,predict(MM),use="complete.obs")^2
R2adjMM=1-(n-1)/(n-coefMM)*(1-R2MM)
RSEMM=if(wMM[2]=="TRUE"){summary(MM)$sigma} else {"NA"}
LMM=logLik(MM)
AICMM=AIC(MM)
AICCMM=AICMM+(2*coefMM*(coefMM+1)/n-coefMM-1)
BICMM=BIC(MM)
mMM=MM$call
RSSMM=sum(residuals(MM)^2, na.rm=TRUE)
TSSMM=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSMM=sum((predict(MM)-mean(predict(MM), na.rm=TRUE))^2, na.rm=TRUE)
FMM=if(TSSMM>PSSMM){TSSMM/PSSMM} else {PSSMM/TSSMM}
RSDMM=sqrt(RSSMM/(n-coefMM))
RSMM=residuals(MM)
RS1MM=as.numeric(c(residuals(MM)[-1],"NA"))
DWMM=sum((RSMM-RS1MM)^2, na.rm=TRUE)/sum(RSMM^2, na.rm=TRUE)
paramMM=coef(MM)
summaryMM=summary(MM)
	}else
{wMM=c("Missing value or infinity produced","FALSE")
coefMM=NA
R2MM="NA"
R2adjMM="NA"
RSEMM="NA"
LMM="NA"
AICMM="NA"
AICCMM="NA"
BICMM="NA"
mMM="NA"
RSSMM="NA"
FMM="NA"
RSDMM="NA"
DWMM="NA"
paramMM="NA"
summaryMM="NA"}

remove(MM,TSSMM,PSSMM,RSMM,RS1MM)

# (1a) Michaelis-Menten (Rook)
MMR=try(nls(trait ~ 1/(1+a/(b+dim)) , data=x, na.action=na.exclude,
start=list(a=-10,b=-1.4),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(MMR)=="nls" & MMR$convInfo$isConv==TRUE){
wMMR=c(MMR$convInfo$stopMessage,MMR$convInfo$isConv)
coefMMR=length(coef(MMR))
R2MMR=cor(trait,predict(MMR),use="complete.obs")^2
R2adjMMR=1-(n-1)/(n-coefMMR)*(1-R2MMR)
RSEMMR=if(wMMR[2]=="TRUE"){summary(MMR)$sigma} else {"NA"}
LMMR=logLik(MMR)
AICMMR=AIC(MMR)
AICCMMR=AICMMR+(2*coefMMR*(coefMMR+1)/n-coefMMR-1)
BICMMR=BIC(MMR)
mMMR=MMR$call
RSSMMR=sum(residuals(MMR)^2, na.rm=TRUE)
TSSMMR=sum((trait-mean(trait,na.rm=TRUE))^2, na.rm=TRUE)
PSSMMR=sum((predict(MMR)-mean(predict(MMR), na.rm=TRUE))^2, na.rm=TRUE)
FMMR=if(TSSMMR>PSSMMR){TSSMMR/PSSMMR} else {PSSMMR/TSSMMR}
RSDMMR=sqrt(RSSMMR/(n-coefMMR))
RSMMR=residuals(MMR)
RS1MMR=as.numeric(c(residuals(MMR)[-1],"NA"))
DWMMR=sum((RSMMR-RS1MMR)^2, na.rm=TRUE)/sum(RSMMR^2, na.rm=TRUE)
paramMMR=coef(MMR)
summaryMMR=summary(MMR)
	}else
{wMMR=c("Missing value or infinity produced","FALSE")
coefMMR=NA
R2MMR="NA"
R2adjMMR="NA"
RSEMMR="NA"
LMMR="NA"
AICMMR="NA"
AICCMMR="NA"
BICMMR="NA"
mMMR="NA"
RSSMMR="NA"
FMMR="NA"
RSDMMR="NA"
DWMMR="NA"
paramMMR="NA"
summaryMMR="NA"}

remove(MMR,TSSMMR,PSSMMR,RSMMR,RS1MMR)

# (1b) Michaelis-Menten + exponential (Rook)
MME=try(nls(trait ~ a*(1/(1+b/(c+dim)))*e^(-d*dim), data=x, na.action=na.exclude,
start=list(a=-0.06608,b=317.49, c=-328.06, d=-0.027),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(MME)=="nls" & MME$convInfo$isConv==TRUE){
wMME=c(MME$convInfo$stopMessage,MME$convInfo$isConv)
coefMME=length(coef(MME))
R2MME=cor(trait,predict(MME),use="complete.obs")^2
R2adjMME=1-(n-1)/(n-coefMME)*(1-R2MME)
RSEMME=if(wMME[2]=="TRUE"){summary(MME)$sigma} else {"NA"}
LMME=logLik(MME)
AICMME=AIC(MME)
AICCMME=AICMME+(2*coefMME*(coefMME+1)/n-coefMME-1)
BICMME=BIC(MME)
mMME=MME$call
RSSMME=sum(residuals(MME)^2, na.rm=TRUE)
TSSMME=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSMME=sum((predict(MME)-mean(predict(MME), na.rm=TRUE))^2, na.rm=TRUE)
FMME=if(TSSMME>PSSMME){TSSMME/PSSMME} else {PSSMME/TSSMME}
RSDMME=sqrt(RSSMME/(n-coefMME))
RSMME=residuals(MME)
RS1MME=as.numeric(c(residuals(MME)[-1],"NA"))
DWMME=sum((RSMME-RS1MME)^2, na.rm=TRUE)/sum(RSMME^2, na.rm=TRUE)
paramMME=coef(MME)
summaryMME=summary(MME)
	}else
{wMME=c("Missing value or infinity produced","FALSE")
coefMME=NA
R2MME="NA"
R2adjMME="NA"
RSEMME="NA"
LMME="NA"
AICMME="NA"
AICCMME="NA"
BICMME="NA"
mMME="NA"
RSSMME="NA"
FMME="NA"
RSDMME="NA"
DWMME="NA"
paramMME="NA"
summaryMME="NA"}

remove(MME,TSSMME,PSSMME,RSMME,RS1MME)

# (2) Brody (1923)
brody23=try(nls(trait ~ a*e^(-b*dim) , data=x, na.action=na.exclude,
start=list(a=25.6,b=0.0015),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(brody23)=="nls" & brody23$convInfo$isConv==TRUE){
wbrody23=c(brody23$convInfo$stopMessage,brody23$convInfo$isConv)
coefbrody23=length(coef(brody23))
R2brody23=cor(trait,predict(brody23),use="complete.obs")^2
R2adjbrody23=1-(n-1)/(n-coefbrody23)*(1-R2brody23)
RSEbrody23=if(wbrody23[2]=="TRUE"){summary(brody23)$sigma} else {"NA"}
Lbrody23=logLik(brody23)
AICbrody23=AIC(brody23)
AICCbrody23=AICbrody23+(2*coefbrody23*(coefbrody23+1)/n-coefbrody23-1)
BICbrody23=BIC(brody23)
mbrody23=brody23$call
RSSbrody23=sum(residuals(brody23)^2, na.rm=TRUE)
TSSbrody23=sum((trait-mean(trait,na.rm=TRUE))^2, na.rm=TRUE)
PSSbrody23=sum((predict(brody23)-mean(predict(brody23),na.rm=TRUE))^2, na.rm=TRUE)
Fbrody23=if(TSSbrody23>PSSbrody23){TSSbrody23/PSSbrody23} else {PSSbrody23/TSSbrody23}
RSDbrody23=sqrt(RSSbrody23/(n-coefbrody23))
RSbrody23=residuals(brody23)
RS1brody23=as.numeric(c(residuals(brody23)[-1],"NA"))
DWbrody23=sum((RSbrody23-RS1brody23)^2, na.rm=TRUE)/sum(RSbrody23^2, na.rm=TRUE)
parambrody23=coef(brody23)
summarybrody23=summary(brody23)
	}else
{wbrody23=c("Missing value or infinity produced","FALSE")
coefbrody23=NA
R2brody23="NA"
R2adjbrody23="NA"
RSEbrody23="NA"
Lbrody23="NA"
AICbrody23="NA"
AICCbrody23="NA"
BICbrody23="NA"
mbrody23="NA"
RSSbrody23="NA"
Fbrody23="NA"
RSDbrody23="NA"
DWbrody23="NA"
parambrody23="NA"
summarybrody23="NA"}

remove(brody23,TSSbrody23,PSSbrody23,RSbrody23,RS1brody23)

# (3) Brody (1924)
brody24=try(nls(trait ~ a*e^(-b*dim)-a*e^(-c*dim), data=x, na.action=na.exclude,
start=list(a=26.127,b=0.0017,c=0.2575),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(brody24)=="nls" & brody24$convInfo$isConv==TRUE){
wbrody24=c(brody24$convInfo$stopMessage,brody24$convInfo$isConv)
coefbrody24=length(coef(brody24))
R2brody24=cor(trait,predict(brody24),use="complete.obs")^2
R2adjbrody24=1-(n-1)/(n-coefbrody24)*(1-R2brody24)
RSEbrody24=if(wbrody24[2]=="TRUE"){summary(brody24)$sigma} else {"NA"}
Lbrody24=logLik(brody24)
AICbrody24=AIC(brody24)
AICCbrody24=AICbrody24+(2*coefbrody24*(coefbrody24+1)/n-coefbrody24-1)
BICbrody24=BIC(brody24)
mbrody24=brody24$call
RSSbrody24=sum(residuals(brody24)^2, na.rm=TRUE)
TSSbrody24=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSbrody24=sum((predict(brody24)-mean(predict(brody24), na.rm=TRUE))^2, na.rm=TRUE)
Fbrody24=if(TSSbrody24>PSSbrody24){TSSbrody24/PSSbrody24} else {PSSbrody24/TSSbrody24}
RSDbrody24=sqrt(RSSbrody24/(n-coefbrody24))
RSbrody24=residuals(brody24)
RS1brody24=as.numeric(c(residuals(brody24)[-1],"NA"))
DWbrody24=sum((RSbrody24-RS1brody24)^2, na.rm=TRUE)/sum(RSbrody24^2, na.rm=TRUE)
parambrody24=coef(brody24)
summarybrody24=summary(brody24)
  }else
{wbrody24=c("Missing value or infinity produced","FALSE")
coefbrody24=NA
R2brody24="NA"
R2adjbrody24="NA"
RSEbrody24="NA"
Lbrody24="NA"
AICbrody24="NA"
AICCbrody24="NA"
BICbrody24="NA"
mbrody24="NA"
RSSbrody24="NA"
Fbrody24="NA"
RSDbrody24="NA"
DWbrody24="NA"
parambrody24="NA"
summarybrody24="NA"}

remove(brody24,TSSbrody24,PSSbrody24,RSbrody24,RS1brody24)

# (4) Schumacher
SCH=try(nls(trait ~ e^a+b/dim, data=x, na.action=na.exclude,
start=list(a=3, b=58),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(SCH)=="nls" & SCH$convInfo$isConv==TRUE){
wSCH=c(SCH$convInfo$stopMessage,SCH$convInfo$isConv)
coefSCH=length(coef(SCH))
R2SCH=cor(trait,predict(SCH),use="complete.obs")^2
R2adjSCH=1-(n-1)/(n-coefSCH)*(1-R2SCH)
RSESCH=if(wSCH[2]=="TRUE"){summary(SCH)$sigma} else {"NA"}
LSCH=logLik(SCH)
AICSCH=AIC(SCH)
AICCSCH=AICSCH+(2*coefSCH*(coefSCH+1)/n-coefSCH-1)
BICSCH=BIC(SCH)
mSCH=SCH$call
RSSSCH=sum(residuals(SCH)^2, na.rm=TRUE)
TSSSCH=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSSCH=sum((predict(SCH)-mean(predict(SCH), na.rm=TRUE))^2, na.rm=TRUE)
FSCH=if(TSSSCH>PSSSCH){TSSSCH/PSSSCH} else {PSSSCH/TSSSCH}
RSDSCH=sqrt(RSSSCH/(n-coefSCH))
RSSCH=residuals(SCH)
RS1SCH=as.numeric(c(residuals(SCH)[-1],"NA"))
DWSCH=sum((RSSCH-RS1SCH)^2, na.rm=TRUE)/sum(RSSCH^2, na.rm=TRUE)
paramSCH=coef(SCH)
summarySCH=summary(SCH)
	}else
{wSCH=c("Missing value or infinity produced","FALSE")
coefSCH=NA
R2SCH="NA"
R2adjSCH="NA"
RSESCH="NA"
LSCH="NA"
AICSCH="NA"
AICCSCH="NA"
BICSCH="NA"
mSCH="NA"
RSSSCH="NA"
FSCH="NA"
RSDSCH="NA"
DWSCH="NA"
paramSCH="NA"
summarySCH="NA"}

remove(SCH,TSSSCH,PSSSCH,RSSCH,RS1SCH)

# (4a) Schumacher (Lopez et al. 2015)
SCHL=try(nls(trait ~ a*e^(b*dim/(dim+1)), data=x, na.action=na.exclude,
start=list(a=264, b=-2.6),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(SCHL)=="nls" & SCHL$convInfo$isConv==TRUE){
wSCHL=c(SCHL$convInfo$stopMessage,SCHL$convInfo$isConv)
coefSCHL=length(coef(SCHL))
R2SCHL=cor(trait,predict(SCHL),use="complete.obs")^2
R2adjSCHL=1-(n-1)/(n-coefSCHL)*(1-R2SCHL)
RSESCHL=if(wSCHL[2]=="TRUE"){summary(SCHL)$sigma} else {"NA"}
LSCHL=logLik(SCHL)
AICSCHL=AIC(SCHL)
AICCSCHL=AICSCHL+(2*coefSCHL*(coefSCHL+1)/n-coefSCHL-1)
BICSCHL=BIC(SCHL)
mSCHL=SCHL$call
RSSSCHL=sum(residuals(SCHL)^2, na.rm=TRUE)
TSSSCHL=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSSCHL=sum((predict(SCHL)-mean(predict(SCHL), na.rm=TRUE))^2, na.rm=TRUE)
FSCHL=if(TSSSCHL>PSSSCHL){TSSSCHL/PSSSCHL} else {PSSSCHL/TSSSCHL}
RSDSCHL=sqrt(RSSSCHL/(n-coefSCHL))
RSSCHL=residuals(SCHL)
RS1SCHL=as.numeric(c(residuals(SCHL)[-1],"NA"))
DWSCHL=sum((RSSCHL-RS1SCHL)^2, na.rm=TRUE)/sum(RSSCHL^2, na.rm=TRUE)
paramSCHL=coef(SCHL)
summarySCHL=summary(SCHL)
	}else
{wSCHL=c("Missing value or infinity produced","FALSE")
coefSCHL=NA
R2SCHL="NA"
R2adjSCHL="NA"
RSESCHL="NA"
LSCHL="NA"
AICSCHL="NA"
AICCSCHL="NA"
BICSCHL="NA"
mSCHL="NA"
RSSSCHL="NA"
FSCHL="NA"
RSDSCHL="NA"
DWSCHL="NA"
paramSCHL="NA"
summarySCH="NA"}

remove(SCHL,TSSSCHL,PSSSCHL,RSSCHL,RS1SCHL)

# (5) Parabolic exponential (Sikka, Adediran)
PBE=try(nls(trait ~ a * e^((b*dim)-(c*dim^2)), data=x, na.action=na.exclude,
start=list(a=25, b=-0.0008, c=0.000002),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(PBE)=="nls" & PBE$convInfo$isConv==TRUE){
wPBE=c(PBE$convInfo$stopMessage,PBE$convInfo$isConv)
coefPBE=length(coef(PBE))
R2PBE=cor(trait,predict(PBE),use="complete.obs")^2
R2adjPBE=1-(n-1)/(n-coefPBE)*(1-R2PBE)
RSEPBE=if(wPBE[2]=="TRUE"){summary(PBE)$sigma} else {"NA"}
LPBE=logLik(PBE)
AICPBE=AIC(PBE)
AICCPBE=AICPBE+(2*coefPBE*(coefPBE+1)/n-coefPBE-1)
BICPBE=BIC(PBE)
mPBE=PBE$call
RSSPBE=sum(residuals(PBE)^2, na.rm=TRUE)
TSSPBE=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSPBE=sum((predict(PBE)-mean(predict(PBE), na.rm=TRUE))^2, na.rm=TRUE)
FPBE=if(TSSPBE>PSSPBE){TSSPBE/PSSPBE} else {PSSPBE/TSSPBE}
RSDPBE=sqrt(RSSPBE/(n-coefPBE))
RSPBE=residuals(PBE)
RS1PBE=as.numeric(c(residuals(PBE)[-1],"NA"))
DWPBE=sum((RSPBE-RS1PBE)^2, na.rm=TRUE)/sum(RSPBE^2, na.rm=TRUE)
paramPBE=coef(PBE)
summaryPBE=summary(PBE)
	}else
{wPBE=c("Missing value or infinity produced","FALSE")
coefPBE=NA
R2PBE="NA"
R2adjPBE="NA"
RSEPBE="NA"
LPBE="NA"
AICPBE="NA"
AICCPBE="NA"
BICPBE="NA"
mPBE="NA"
RSSPBE="NA"
FPBE="NA"
RSDPBE="NA"
DWPBE="NA"
paramPBE="NA"
summaryPBE="NA"}

remove(PBE,TSSPBE,PSSPBE,RSPBE,RS1PBE)

# (6) Wood
wood=try(nls(trait ~ a * dim^b* e^(-c*dim) , data=x, na.action=na.exclude,
start=list(a=20.5,b=0.07,c=0.002),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(wood)=="nls" & wood$convInfo$isConv==TRUE){
wwood=c(wood$convInfo$stopMessage,wood$convInfo$isConv)
coefwood=length(coef(wood))
R2wood=cor(trait,predict(wood),use="complete.obs")^2
R2adjwood=1-(n-1)/(n-coefwood)*(1-R2wood)
RSEwood=if(wwood[2]=="TRUE"){summary(wood)$sigma} else {"NA"}
Lwood=logLik(wood)
AICwood=AIC(wood)
AICCwood=AICwood+(2*coefwood*(coefwood+1)/n-coefwood-1)
BICwood=BIC(wood)
mwood=wood$call
RSSwood=sum(residuals(wood)^2, na.rm=TRUE)
TSSwood=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSwood=sum((predict(wood)-mean(predict(wood), na.rm=TRUE))^2, na.rm=TRUE)
Fwood=if(TSSwood>PSSwood){TSSwood/PSSwood} else {PSSwood/TSSwood}
RSDwood=sqrt(RSSwood/(n-coefwood))
RSwood=residuals(wood)
RS1wood=as.numeric(c(residuals(wood)[-1],"NA"))
DWwood=sum((RSwood-RS1wood)^2, na.rm=TRUE)/sum(RSwood^2, na.rm=TRUE)
paramwood=coef(wood)
summarywood=summary(wood)
	}else
{wwood=c("Missing value or infinity produced","FALSE")
coefwood=NA
R2wood="NA"
R2adjwood="NA"
RSEwood="NA"
Lwood="NA"
AICwood="NA"
AICCwood="NA"
BICwood="NA"
mwood="NA"
RSSwood="NA"
Fwood="NA"
RSDwood="NA"
DWwood="NA"
paramwood="NA"
summarywood="NA"}

remove(wood,TSSwood,PSSwood,RSwood,RS1wood)

# (6a) Wood reparameterized (Dhanoa)
DHA=try(nls(trait ~ a*(dim^(b*c))*e^(-c*dim), data=x, na.action=na.exclude,
start=list(a=20.5, b=30.7, c=0.002),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(DHA)=="nls" & DHA$convInfo$isConv==TRUE){
wDHA=c(DHA$convInfo$stopMessage,DHA$convInfo$isConv)
coefDHA=length(coef(DHA))
R2DHA=cor(trait,predict(DHA),use="complete.obs")^2
R2adjDHA=1-(n-1)/(n-coefDHA)*(1-R2DHA)
RSEDHA=if(wDHA[2]=="TRUE"){summary(DHA)$sigma} else {"NA"}
LDHA=logLik(DHA)
AICDHA=AIC(DHA)
AICCDHA=AICDHA+(2*coefDHA*(coefDHA+1)/n-coefDHA-1)
BICDHA=BIC(DHA)
mDHA=DHA$call
RSSDHA=sum(residuals(DHA)^2, na.rm=TRUE)
TSSDHA=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSDHA=sum((predict(DHA)-mean(predict(DHA), na.rm=TRUE))^2, na.rm=TRUE)
FDHA=if(TSSDHA>PSSDHA){TSSDHA/PSSDHA} else {PSSDHA/TSSDHA}
RSDDHA=sqrt(RSSDHA/(n-coefDHA))
RSDHA=residuals(DHA)
RS1DHA=as.numeric(c(residuals(DHA)[-1],"NA"))
DWDHA=sum((RSDHA-RS1DHA)^2, na.rm=TRUE)/sum(RSDHA^2, na.rm=TRUE)
paramDHA=coef(DHA)
summaryDHA=summary(DHA)
  }else
{wDHA=c("Missing value or infinity produced","FALSE")
coefDHA=NA
R2DHA="NA"
R2adjDHA="NA"
RSEDHA="NA"
LDHA="NA"
AICDHA="NA"
AICCDHA="NA"
BICDHA="NA"
mDHA="NA"
RSSDHA="NA"
FDHA="NA"
RSDDHA="NA"
DWDHA="NA"
paramDHA="NA"
summaryDHA="NA"}

remove(DHA,TSSDHA,PSSDHA,RSDHA,RS1DHA)

# (6b) Wood non-linear (Cappio-Borlino)
CB=try(nls(trait ~ a*(dim^b)*e^(-c*dim), data=x, na.action=na.exclude,
start=list(a=20.5, b=0.07, c=0.002),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(CB)=="nls" & CB$convInfo$isConv==TRUE){
wCB=c(CB$convInfo$stopMessage,CB$convInfo$isConv)
coefCB=length(coef(CB))
R2CB=cor(trait,predict(CB),use="complete.obs")^2
R2adjCB=1-(n-1)/(n-coefCB)*(1-R2CB)
RSECB=if(wCB[2]=="TRUE"){summary(CB)$sigma} else {"NA"}
LCB=logLik(CB)
AICCB=AIC(CB)
AICCCB=AICCB+(2*coefCB*(coefCB+1)/n-coefCB-1)
BICCB=BIC(CB)
mCB=CB$call
RSSCB=sum(residuals(CB)^2, na.rm=TRUE)
TSSCB=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSCB=sum((predict(CB)-mean(predict(CB), na.rm=TRUE))^2, na.rm=TRUE)
FCB=if(TSSCB>PSSCB){TSSCB/PSSCB} else {PSSCB/TSSCB}
RSDCB=sqrt(RSSCB/(n-coefCB))
RSCB=residuals(CB)
RS1CB=as.numeric(c(residuals(CB)[-1],"NA"))
DWCB=sum((RSCB-RS1CB)^2, na.rm=TRUE)/sum(RSCB^2, na.rm=TRUE)
paramCB=coef(CB)
summaryCB=summary(CB)
	}else
{wCB=c("Missing value or infinity produced","FALSE")
coefCB=NA
R2CB="NA"
R2adjCB="NA"
RSECB="NA"
LCB="NA"
AICCB="NA"
AICCCB="NA"
BICCB="NA"
mCB="NA"
RSSCB="NA"
FCB="NA"
RSDCB="NA"
DWCB="NA"
paramCB="NA"
summaryCB="NA"}

remove(CB,TSSCB,PSSCB,RSCB,RS1CB)

# (7) Quadratic Polynomial
QP=try(nls(trait ~ a + b*dim + c*dim^2, data=x, na.action=na.exclude,
start=list(a=25, b=-0.02, c=-0.000015),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(QP)=="nls" & QP$convInfo$isConv==TRUE){
wQP=c(QP$convInfo$stopMessage,QP$convInfo$isConv)
coefQP=length(coef(QP))
R2QP=cor(trait,predict(QP),use="complete.obs")^2
R2adjQP=1-(n-1)/(n-coefQP)*(1-R2QP)
RSEQP=if(wQP[2]=="TRUE"){summary(QP)$sigma} else {"NA"}
LQP=logLik(QP)
AICQP=AIC(QP)
AICCQP=AICQP+(2*coefQP*(coefQP+1)/n-coefQP-1)
BICQP=BIC(QP)
mQP=QP$call
RSSQP=sum(residuals(QP)^2, na.rm=TRUE)
TSSQP=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSQP=sum((predict(QP)-mean(predict(QP), na.rm=TRUE))^2, na.rm=TRUE)
FQP=if(TSSQP>PSSQP){TSSQP/PSSQP} else {PSSQP/TSSQP}
RSDQP=sqrt(RSSQP/(n-coefQP))
RSQP=residuals(QP)
RS1QP=as.numeric(c(residuals(QP)[-1],"NA"))
DWQP=sum((RSQP-RS1QP)^2, na.rm=TRUE)/sum(RSQP^2, na.rm=TRUE)
paramQP=coef(QP)
summaryQP=summary(QP)
	}else
{wQP=c("Missing value or infinity produced","FALSE")
coefQP=NA
R2QP="NA"
R2adjQP="NA"
RSEQP="NA"
LQP="NA"
AICQP="NA"
AICCQP="NA"
BICQP="NA"
mQP="NA"
RSSQP="NA"
FQP="NA"
RSDQP="NA"
DWQP="NA"
paramQP="NA"
summaryQP="NA"}

remove(QP,TSSQP,PSSQP,RSQP,RS1QP)

# (8) Cobby and Le Du (Vargas)
CLD=try(nls(trait ~ a - b*dim - a*e^(-c*dim), data=x, na.action=na.exclude,
start=list(a=25, b=0.03, c=0.28),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(CLD)=="nls" & CLD$convInfo$isConv==TRUE){
wCLD=c(CLD$convInfo$stopMessage,CLD$convInfo$isConv)
coefCLD=length(coef(CLD))
R2CLD=cor(trait,predict(CLD),use="complete.obs")^2
R2adjCLD=1-(n-1)/(n-coefCLD)*(1-R2CLD)
RSECLD=if(wCLD[2]=="TRUE"){summary(CLD)$sigma} else {"NA"}
LCLD=logLik(CLD)
AICCLD=AIC(CLD)
AICCCLD=AICCLD+(2*coefCLD*(coefCLD+1)/n-coefCLD-1)
BICCLD=BIC(CLD)
mCLD=CLD$call
RSSCLD=sum(residuals(CLD)^2, na.rm=TRUE)
TSSCLD=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSCLD=sum((predict(CLD)-mean(predict(CLD), na.rm=TRUE))^2, na.rm=TRUE)
FCLD=if(TSSCLD>PSSCLD){TSSCLD/PSSCLD} else {PSSCLD/TSSCLD}
RSDCLD=sqrt(RSSCLD/(n-coefCLD))
RSCLD=residuals(CLD)
RS1CLD=as.numeric(c(residuals(CLD)[-1],"NA"))
DWCLD=sum((RSCLD-RS1CLD)^2, na.rm=TRUE)/sum(RSCLD^2, na.rm=TRUE)
paramCLD=coef(CLD)
summaryCLD=summary(CLD)
	}else
{wCLD=c("Missing value or infinity produced","FALSE")
coefCLD=NA
R2CLD="NA"
R2adjCLD="NA"
RSECLD="NA"
LCLD="NA"
AICCLD="NA"
AICCCLD="NA"
BICCLD="NA"
mCLD="NA"
RSSCLD="NA"
FCLD="NA"
RSDCLD="NA"
DWCLD="NA"
paramCLD="NA"
summaryCLD="NA"}

remove(CLD,TSSCLD,PSSCLD,RSCLD,RS1CLD)

# (9) Papajcsik and Bodero 1
PapBo1=try(nls(trait ~ a*dim^b/cosh(c*dim), data=x, na.action=na.exclude,
start=list(a=24.7, b=-0.0098, c=0.0033),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(PapBo1)=="nls" & PapBo1$convInfo$isConv==TRUE){
wPapBo1=c(PapBo1$convInfo$stopMessage,PapBo1$convInfo$isConv)
coefPapBo1=length(coef(PapBo1))
R2PapBo1=cor(trait,predict(PapBo1),use="complete.obs")^2
R2adjPapBo1=1-(n-1)/(n-coefPapBo1)*(1-R2PapBo1)
RSEPapBo1=if(wPapBo1[2]=="TRUE"){summary(PapBo1)$sigma} else {"NA"}
LPapBo1=logLik(PapBo1)
AICPapBo1=AIC(PapBo1)
AICCPapBo1=AICPapBo1+(2*coefPapBo1*(coefPapBo1+1)/n-coefPapBo1-1)
BICPapBo1=BIC(PapBo1)
mPapBo1=PapBo1$call
RSSPapBo1=sum(residuals(PapBo1)^2, na.rm=TRUE)
TSSPapBo1=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSPapBo1=sum((predict(PapBo1)-mean(predict(PapBo1), na.rm=TRUE))^2, na.rm=TRUE)
FPapBo1=if(TSSPapBo1>PSSPapBo1){TSSPapBo1/PSSPapBo1} else {PSSPapBo1/TSSPapBo1}
RSDPapBo1=sqrt(RSSPapBo1/(n-coefPapBo1))
RSPapBo1=residuals(PapBo1)
RS1PapBo1=as.numeric(c(residuals(PapBo1)[-1],"NA"))
DWPapBo1=sum((RSPapBo1-RS1PapBo1)^2, na.rm=TRUE)/sum(RSPapBo1^2, na.rm=TRUE)
paramPapBo1=coef(PapBo1)
summaryPapBo1=summary(PapBo1)
	}else
{wPapBo1=c("Missing value or infinity produced","FALSE")
coefPapBo1=NA
R2PapBo1="NA"
R2adjPapBo1="NA"
RSEPapBo1="NA"
LPapBo1="NA"
AICPapBo1="NA"
AICCPapBo1="NA"
BICPapBo1="NA"
mPapBo1="NA"
RSSPapBo1="NA"
FPapBo1="NA"
RSDPapBo1="NA"
DWPapBo1="NA"
paramPapBo1="NA"
summaryPapBo1="NA"}

remove(PapBo1,TSSPapBo1,PSSPapBo1,RSPapBo1,RS1PapBo1)

# (10) Papajcsik and Bodero 2
PapBo2=try(nls(trait ~ a*(1-e^(-b*dim))/cosh(c*dim), data=x, na.action=na.exclude,
start=list(a=23.896, b=0.398, c=0.0034),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(PapBo2)=="nls" & PapBo2$convInfo$isConv==TRUE){
wPapBo2=c(PapBo2$convInfo$stopMessage,PapBo2$convInfo$isConv)
coefPapBo2=length(coef(PapBo2))
R2PapBo2=cor(trait,predict(PapBo2),use="complete.obs")^2
R2adjPapBo2=1-(n-1)/(n-coefPapBo2)*(1-R2PapBo2)
RSEPapBo2=if(wPapBo2[2]=="TRUE"){summary(PapBo2)$sigma} else {"NA"}
LPapBo2=logLik(PapBo2)
AICPapBo2=AIC(PapBo2)
AICCPapBo2=AICPapBo2+(2*coefPapBo2*(coefPapBo2+1)/n-coefPapBo2-1)
BICPapBo2=BIC(PapBo2)
mPapBo2=PapBo2$call
RSSPapBo2=sum(residuals(PapBo2)^2, na.rm=TRUE)
TSSPapBo2=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSPapBo2=sum((predict(PapBo2)-mean(predict(PapBo2), na.rm=TRUE))^2, na.rm=TRUE)
FPapBo2=if(TSSPapBo2>PSSPapBo2){TSSPapBo2/PSSPapBo2} else {PSSPapBo2/TSSPapBo2}
RSDPapBo2=sqrt(RSSPapBo2/(n-coefPapBo2))
RSPapBo2=residuals(PapBo2)
RS1PapBo2=as.numeric(c(residuals(PapBo2)[-1],"NA"))
DWPapBo2=sum((RSPapBo2-RS1PapBo2)^2, na.rm=TRUE)/sum(RSPapBo2^2, na.rm=TRUE)
paramPapBo2=coef(PapBo2)
summaryPapBo2=summary(PapBo2)
	}else
{wPapBo2=c("Missing value or infinity produced","FALSE")
coefPapBo2=NA
R2PapBo2="NA"
R2adjPapBo2="NA"
RSEPapBo2="NA"
LPapBo2="NA"
AICPapBo2="NA"
AICCPapBo2="NA"
BICPapBo2="NA"
mPapBo2="NA"
RSSPapBo2="NA"
FPapBo2="NA"
RSDPapBo2="NA"
DWPapBo2="NA"
paramPapBo2="NA"
summaryPapBo2="NA"}

remove(PapBo2,TSSPapBo2,PSSPapBo2,RSPapBo2,RS1PapBo2)

# (11) Papajcsik and Bodero 3
PapBo3=try(nls(trait ~ a*atan(b*dim)/cosh(c*dim), data=x, na.action=na.exclude,
start=list(a=15.27, b=2.587, c=0.00346),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(PapBo3)=="nls" & PapBo3$convInfo$isConv==TRUE){
wPapBo3=c(PapBo3$convInfo$stopMessage,PapBo3$convInfo$isConv)
coefPapBo3=length(coef(PapBo3))
R2PapBo3=cor(trait,predict(PapBo3),use="complete.obs")^2
R2adjPapBo3=1-(n-1)/(n-coefPapBo3)*(1-R2PapBo3)
RSEPapBo3=if(wPapBo3[2]=="TRUE"){summary(PapBo3)$sigma} else {"NA"}
LPapBo3=logLik(PapBo3)
AICPapBo3=AIC(PapBo3)
AICCPapBo3=AICPapBo3+(2*coefPapBo3*(coefPapBo3+1)/n-coefPapBo3-1)
BICPapBo3=BIC(PapBo3)
mPapBo3=PapBo3$call
RSSPapBo3=sum(residuals(PapBo3)^2, na.rm=TRUE)
TSSPapBo3=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSPapBo3=sum((predict(PapBo3)-mean(predict(PapBo3), na.rm=TRUE))^2, na.rm=TRUE)
FPapBo3=if(TSSPapBo3>PSSPapBo3){TSSPapBo3/PSSPapBo3} else {PSSPapBo3/TSSPapBo3}
RSDPapBo3=sqrt(RSSPapBo3/(n-coefPapBo3))
RSPapBo3=residuals(PapBo3)
RS1PapBo3=as.numeric(c(residuals(PapBo3)[-1],"NA"))
DWPapBo3=sum((RSPapBo3-RS1PapBo3)^2, na.rm=TRUE)/sum(RSPapBo3^2, na.rm=TRUE)
paramPapBo3=coef(PapBo3)
summaryPapBo3=summary(PapBo3)
	}else
{wPapBo3=c("Missing value or infinity produced","FALSE")
coefPapBo3=NA
R2PapBo3="NA"
R2adjPapBo3="NA"
RSEPapBo3="NA"
LPapBo3="NA"
AICPapBo3="NA"
AICCPapBo3="NA"
BICPapBo3="NA"
mPapBo3="NA"
RSSPapBo3="NA"
FPapBo3="NA"
RSDPapBo3="NA"
DWPapBo3="NA"
paramPapBo3="NA"
summaryPapBo3="NA"}

remove(PapBo3,TSSPapBo3,PSSPapBo3,RSPapBo3,RS1PapBo3)

# (12) Papajcsik and Bodero 4
PapBo4=try(nls(trait ~ a*log(b*dim)*e^(-c*dim), data=x, na.action=na.exclude,
start=list(a=1.763, b=76690, c=0.00219),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

if(class(PapBo4)=="try-error")

#options(warn=-1)
if(class(PapBo4)=="nls"){
wPapBo4=c(PapBo4$convInfo$stopMessage,PapBo4$convInfo$isConv)
coefPapBo4=length(coef(PapBo4))
R2PapBo4=cor(trait,predict(PapBo4),use="complete.obs")^2
R2adjPapBo4=1-(n-1)/(n-coefPapBo4)*(1-R2PapBo4)
RSEPapBo4=if(wPapBo4[2]=="TRUE"){summary(PapBo4)$sigma} else {"NA"}
LPapBo4=logLik(PapBo4)
AICPapBo4=AIC(PapBo4)
AICCPapBo4=AICPapBo4+(2*coefPapBo4*(coefPapBo4+1)/n-coefPapBo4-1)
BICPapBo4=BIC(PapBo4)
mPapBo4=PapBo4$call
RSSPapBo4=sum(residuals(PapBo4)^2, na.rm=TRUE)
TSSPapBo4=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSPapBo4=sum((predict(PapBo4)-mean(predict(PapBo4), na.rm=TRUE))^2, na.rm=TRUE)
FPapBo4=if(TSSPapBo4>PSSPapBo4){TSSPapBo4/PSSPapBo4} else {PSSPapBo4/TSSPapBo4}
RSDPapBo4=sqrt(RSSPapBo4/(n-coefPapBo4))
RSPapBo4=residuals(PapBo4)
RS1PapBo4=as.numeric(c(residuals(PapBo4)[-1],"NA"))
DWPapBo4=sum((RSPapBo4-RS1PapBo4)^2, na.rm=TRUE)/sum(RSPapBo4^2, na.rm=TRUE)
paramPapBo4=coef(PapBo4)
summaryPapBo4=summary(PapBo4)
	}else
{wPapBo4=c("Missing value or infinity produced","FALSE")
coefPapBo4=NA
R2PapBo4="NA"
R2adjPapBo4="NA"
RSEPapBo4="NA"
LPapBo4="NA"
AICPapBo4="NA"
AICCPapBo4="NA"
BICPapBo4="NA"
mPapBo4="NA"
RSSPapBo4="NA"
FPapBo4="NA"
RSDPapBo4="NA"
DWPapBo4="NA"
paramPapBo4="NA"
summaryPapBo4="NA"}

remove(PapBo4,TSSPapBo4,PSSPapBo4,RSPapBo4,RS1PapBo4)

# (13) Papajcsik and Bodero 6
PapBo6=try(nls(trait ~ a*atan(b*dim)*e^(-c*dim), data=x, na.action=na.exclude,
start=list(a=17.25, b=0.493, c=0.0018),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(PapBo6)=="nls" & PapBo6$convInfo$isConv==TRUE){
wPapBo6=c(PapBo6$convInfo$stopMessage,PapBo6$convInfo$isConv)
coefPapBo6=length(coef(PapBo6))
R2PapBo6=cor(trait,predict(PapBo6),use="complete.obs")^2
R2adjPapBo6=1-(n-1)/(n-coefPapBo6)*(1-R2PapBo6)
RSEPapBo6=if(wPapBo6[2]=="TRUE"){summary(PapBo6)$sigma} else {"NA"}
LPapBo6=logLik(PapBo6)
AICPapBo6=AIC(PapBo6)
AICCPapBo6=AICPapBo6+(2*coefPapBo6*(coefPapBo6+1)/n-coefPapBo6-1)
BICPapBo6=BIC(PapBo6)
mPapBo6=PapBo6$call
RSSPapBo6=sum(residuals(PapBo6)^2, na.rm=TRUE)
TSSPapBo6=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSPapBo6=sum((predict(PapBo6)-mean(predict(PapBo6), na.rm=TRUE))^2, na.rm=TRUE)
FPapBo6=if(TSSPapBo6>PSSPapBo6){TSSPapBo6/PSSPapBo6} else {PSSPapBo6/TSSPapBo6}
RSDPapBo6=sqrt(RSSPapBo6/(n-coefPapBo6))
RSPapBo6=residuals(PapBo6)
RS1PapBo6=as.numeric(c(residuals(PapBo6)[-1],"NA"))
DWPapBo6=sum((RSPapBo6-RS1PapBo6)^2, na.rm=TRUE)/sum(RSPapBo6^2, na.rm=TRUE)
paramPapBo6=coef(PapBo6)
summaryPapBo6=summary(PapBo6)
  }else
{wPapBo6=c("Missing value or infinity produced","FALSE")
coefPapBo6=NA
R2PapBo6="NA"
R2adjPapBo6="NA"
RSEPapBo6="NA"
LPapBo6="NA"
AICPapBo6="NA"
AICCPapBo6="NA"
BICPapBo6="NA"
mPapBo6="NA"
RSSPapBo6="NA"
FPapBo6="NA"
RSDPapBo6="NA"
DWPapBo6="NA"
paramPapBo6="NA"
summaryPapBo6="NA"}

remove(PapBo6,TSSPapBo6,PSSPapBo6,RSPapBo6,RS1PapBo6)

# (14) Mixed log model 1 (Guo and Swalve)
GS1=try(nls(trait ~ a+b*dim^0.5 + c*log(dim), data=x, na.action=na.exclude,
start=list(a=18.28, b=-1.58, c=4.33),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(GS1)=="nls" & GS1$convInfo$isConv==TRUE){
wGS1=c(GS1$convInfo$stopMessage,GS1$convInfo$isConv)
coefGS1=length(coef(GS1))
R2GS1=cor(trait,predict(GS1),use="complete.obs")^2
R2adjGS1=1-(n-1)/(n-coefGS1)*(1-R2GS1)
RSEGS1=if(wGS1[2]=="TRUE"){summary(GS1)$sigma} else {"NA"}
LGS1=logLik(GS1)
AICGS1=AIC(GS1)
AICCGS1=AICGS1+(2*coefGS1*(coefGS1+1)/n-coefGS1-1)
BICGS1=BIC(GS1)
mGS1=GS1$call
RSSGS1=sum(residuals(GS1)^2, na.rm=TRUE)
TSSGS1=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSGS1=sum((predict(GS1)-mean(predict(GS1), na.rm=TRUE))^2, na.rm=TRUE)
FGS1=if(TSSGS1>PSSGS1){TSSGS1/PSSGS1} else {PSSGS1/TSSGS1}
RSDGS1=sqrt(RSSGS1/(n-coefGS1))
RSGS1=residuals(GS1)
RS1GS1=as.numeric(c(residuals(GS1)[-1],"NA"))
DWGS1=sum((RSGS1-RS1GS1)^2, na.rm=TRUE)/sum(RSGS1^2, na.rm=TRUE)
paramGS1=coef(GS1)
summaryGS1=summary(GS1)
  }else
{wGS1=c("Missing value or infinity produced","FALSE")
coefGS1=NA
R2GS1="NA"
R2adjGS1="NA"
RSEGS1="NA"
LGS1="NA"
AICGS1="NA"
AICCGS1="NA"
BICGS1="NA"
mGS1="NA"
RSSGS1="NA"
FGS1="NA"
RSDGS1="NA"
DWGS1="NA"
paramGS1="NA"
summaryGS1="NA"}

remove(GS1,TSSGS1,PSSGS1,RSGS1,RS1GS1)

# (15) Mixed log model 3 (Guo and Swalve)
GS2=try(nls(trait ~ a+b*dim^0.5 + c*log(dim) + d*dim^4, data=x, na.action=na.exclude,
start=list(a=17.75, b=-1.72, c=4.81, d=0.00000000007),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(GS2)=="nls" & GS2$convInfo$isConv==TRUE){
wGS2=c(GS2$convInfo$stopMessage,GS2$convInfo$isConv)
coefGS2=length(coef(GS2))
R2GS2=cor(trait,predict(GS2),use="complete.obs")^2
R2adjGS2=1-(n-1)/(n-coefGS2)*(1-R2GS2)
RSEGS2=if(wGS2[2]=="TRUE"){summary(GS2)$sigma} else {"NA"}
LGS2=logLik(GS2)
AICGS2=AIC(GS2)
AICCGS2=AICGS2+(2*coefGS2*(coefGS2+1)/n-coefGS2-1)
BICGS2=BIC(GS2)
mGS2=GS2$call
RSSGS2=sum(residuals(GS2)^2, na.rm=TRUE)
TSSGS2=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSGS2=sum((predict(GS2)-mean(predict(GS2), na.rm=TRUE))^2, na.rm=TRUE)
FGS2=if(TSSGS2>PSSGS2){TSSGS2/PSSGS2} else {PSSGS2/TSSGS2}
RSDGS2=sqrt(RSSGS2/(n-coefGS2))
RSGS2=residuals(GS2)
RS1GS2=as.numeric(c(residuals(GS2)[-1],"NA"))
DWGS2=sum((RSGS2-RS1GS2)^2, na.rm=TRUE)/sum(RSGS2^2, na.rm=TRUE)
paramGS2=coef(GS2)
summaryGS2=summary(GS2)
  }else
{wGS2=c("Missing value or infinity produced","FALSE")
coefGS2=NA
R2GS2="NA"
R2adjGS2="NA"
RSEGS2="NA"
LGS2="NA"
AICGS2="NA"
AICCGS2="NA"
BICGS2="NA"
mGS2="NA"
RSSGS2="NA"
FGS2="NA"
RSDGS2="NA"
DWGS2="NA"
paramGS2="NA"
summaryGS2="NA"}

remove(GS2,TSSGS2,PSSGS2,RSGS2,RS1GS2)

# (16) Log-quadratic (Adediran et al. 2012)
LQ=try(nls(trait ~ e^(a*(b-log(dim))^2 + c) , data=x, na.action=na.exclude,
start=list(a=-0.08,b=3,c=3),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(LQ)=="nls" & LQ$convInfo$isConv==TRUE){
wLQ=c(LQ$convInfo$stopMessage,LQ$convInfo$isConv)
coefLQ=length(coef(LQ))
R2LQ=cor(trait,predict(LQ),use="complete.obs")^2
R2adjLQ=1-(n-1)/(n-coefLQ)*(1-R2LQ)
RSELQ=if(wLQ[2]=="TRUE"){summary(LQ)$sigma} else {"NA"}
LLQ=logLik(LQ)
AICLQ=AIC(LQ)
AICCLQ=AICLQ+(2*coefLQ*(coefLQ+1)/n-coefLQ-1)
BICLQ=BIC(LQ)
mLQ=LQ$call
RSSLQ=sum(residuals(LQ)^2, na.rm=TRUE)
TSSLQ=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSLQ=sum((predict(LQ)-mean(predict(LQ), na.rm=TRUE))^2, na.rm=TRUE)
FLQ=if(TSSLQ>PSSLQ){TSSLQ/PSSLQ} else {PSSLQ/TSSLQ}
RSDLQ=sqrt(RSSLQ/(n-coefLQ))
RSLQ=residuals(LQ)
RS1LQ=as.numeric(c(residuals(LQ)[-1],"NA"))
DWLQ=sum((RSLQ-RS1LQ)^2, na.rm=TRUE)/sum(RSLQ^2, na.rm=TRUE)
paramLQ=coef(LQ)
summaryLQ=summary(LQ)
  }else
{wLQ=c("Missing value or infinity produced","FALSE")
coefLQ=NA
R2LQ="NA"
R2adjLQ="NA"
RSELQ="NA"
LLQ="NA"
AICLQ="NA"
AICCLQ="NA"
BICLQ="NA"
mLQ="NA"
RSSLQ="NA"
FLQ="NA"
RSDLQ="NA"
DWLQ="NA"
paramLQ="NA"
summaryLQ="NA"}

remove(LQ,TSSLQ,PSSLQ,RSLQ,RS1LQ)

# (17) Wilmink
wil=try(nls(trait ~ a + b*e^(-k*dim) + c*dim , data=x, na.action=na.exclude,
start=list(a=25,b=-7,c=-0.03,k=0.1),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(wil)=="nls" & wil$convInfo$isConv==TRUE){
wwil=c(wil$convInfo$stopMessage,wil$convInfo$isConv)
coefwil=length(coef(wil))
R2wil=cor(trait,predict(wil),use="complete.obs")^2
R2adjwil=1-(n-1)/(n-coefwil)*(1-R2wil)
RSEwil=if(wwil[2]=="TRUE"){summary(wil)$sigma} else {"NA"}
Lwil=logLik(wil)
AICwil=AIC(wil)
AICCwil=AICwil+(2*coefwil*(coefwil+1)/n-coefwil-1)
BICwil=BIC(wil)
mwil=wil$call
RSSwil=sum(residuals(wil)^2, na.rm=TRUE)
TSSwil=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSwil=sum((predict(wil)-mean(predict(wil), na.rm=TRUE))^2, na.rm=TRUE)
Fwil=if(TSSwil>PSSwil){TSSwil/PSSwil} else {PSSwil/TSSwil}
RSDwil=sqrt(RSSwil/(n-coefwil))
RSwil=residuals(wil)
RS1wil=as.numeric(c(residuals(wil)[-1],"NA"))
DWwil=sum((RSwil-RS1wil)^2, na.rm=TRUE)/sum(RSwil^2, na.rm=TRUE)
paramwil=coef(wil)
summarywil=summary(wil)
  }else
{wwil=c("Missing value or infinity produced","FALSE")
coefwil=NA
R2wil="NA"
R2adjwil="NA"
RSEwil="NA"
Lwil="NA"
AICwil="NA"
AICCwil="NA"
BICwil="NA"
mwil="NA"
RSSwil="NA"
Fwil="NA"
RSDwil="NA"
DWwil="NA"
paramwil="NA"
summarywil="NA"}

remove(wil,TSSwil,PSSwil,RSwil,RS1wil)

# (17a) modified Wilmink (Jacobsen)
wilk=try(nls(trait ~ a + b*e^(-k*dim) + c*(dim/100) , data=x, na.action=na.exclude,
start=list(a=25,b=-7,c=-3,k=0.1),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(wilk)=="nls" & wilk$convInfo$isConv==TRUE){
wwilk=c(wilk$convInfo$stopMessage,wilk$convInfo$isConv)
coefwilk=length(coef(wilk))
R2wilk=cor(trait,predict(wilk),use="complete.obs")^2
R2adjwilk=1-(n-1)/(n-coefwilk)*(1-R2wilk)
RSEwilk=if(wwilk[2]=="TRUE"){summary(wilk)$sigma} else {"NA"}
Lwilk=logLik(wilk)
AICwilk=AIC(wilk)
AICCwilk=AICwilk+(2*coefwilk*(coefwilk+1)/n-coefwilk-1)
BICwilk=BIC(wilk)
mwilk=wilk$call
RSSwilk=sum(residuals(wilk)^2, na.rm=TRUE)
TSSwilk=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSwilk=sum((predict(wilk)-mean(predict(wilk), na.rm=TRUE))^2, na.rm=TRUE)
Fwilk=if(TSSwilk>PSSwilk){TSSwilk/PSSwilk} else {PSSwilk/TSSwilk}
RSDwilk=sqrt(RSSwilk/(n-coefwilk))
RSwilk=residuals(wilk)
RS1wilk=as.numeric(c(residuals(wilk)[-1],"NA"))
DWwilk=sum((RSwilk-RS1wilk)^2, na.rm=TRUE)/sum(RSwilk^2, na.rm=TRUE)
paramwilk=coef(wilk)
summarywilk=summary(wilk)
  }else
{wwilk=c("Missing value or infinity produced","FALSE")
coefwilk=NA
R2wilk="NA"
R2adjwilk="NA"
RSEwilk="NA"
Lwilk="NA"
AICwilk="NA"
AICCwilk="NA"
BICwilk="NA"
mwilk="NA"
RSSwilk="NA"
Fwilk="NA"
RSDwilk="NA"
DWwilk="NA"
paramwilk="NA"
summarywilk="NA"}

remove(wilk,TSSwilk,PSSwilk,RSwilk,RS1wilk)

# (17b) modified Wilmink (Laurenson & Strucken)
wilycsml=try(nls(trait ~ a + (b-a)*(1-e^(-k*dim)) - c*dim, data=x, na.action=na.exclude,
start=list(a=20,b=30,c=0.005,k=0.08),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(wilycsml)=="nls" & wilycsml$convInfo$isConv==TRUE){
wwilycsml=c(wilycsml$convInfo$stopMessage,wilycsml$convInfo$isConv)
coefwilycsml=length(coef(wilycsml))
R2wilycsml=cor(trait,predict(wilycsml),use="complete.obs")^2
R2adjwilycsml=1-(n-1)/(n-coefwilycsml)*(1-R2wilycsml)
RSEwilycsml=if(wwilycsml[2]=="TRUE"){summary(wilycsml)$sigma} else {"NA"}
Lwilycsml=logLik(wilycsml)
AICwilycsml=AIC(wilycsml)
AICCwilycsml=AICwilycsml+(2*coefwilycsml*(coefwilycsml+1)/n-coefwilycsml-1)
BICwilycsml=BIC(wilycsml)
mwilycsml=wilycsml$call
RSSwilycsml=sum(residuals(wilycsml)^2, na.rm=TRUE)
TSSwilycsml=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSwilycsml=sum((predict(wilycsml)-mean(predict(wilycsml), na.rm=TRUE))^2, na.rm=TRUE)
Fwilycsml=if(TSSwilycsml>PSSwilycsml){TSSwilycsml/PSSwilycsml} else {PSSwilycsml/TSSwilycsml}
RSDwilycsml=sqrt(RSSwilycsml/(n-coefwilycsml))
RSwilycsml=residuals(wilycsml)
RS1wilycsml=as.numeric(c(residuals(wilycsml)[-1],"NA"))
DWwilycsml=sum((RSwilycsml-RS1wilycsml)^2, na.rm=TRUE)/sum(RSwilycsml^2, na.rm=TRUE)
paramwilycsml=coef(wilycsml)
summarywilycsml=summary(wilycsml)
	}else
{wwilycsml=c("Missing value or infinity produced","FALSE")
coefwilycsml=NA
R2wilycsml="NA"
R2adjwilycsml="NA"
RSEwilycsml="NA"
Lwilycsml="NA"
AICwilycsml="NA"
AICCwilycsml="NA"
BICwilycsml="NA"
mwilycsml="NA"
RSSwilycsml="NA"
Fwilycsml="NA"
RSDwilycsml="NA"
DWwilycsml="NA"
paramwilycsml="NA"
summarywilycsml="NA"}

remove(wilycsml,TSSwilycsml,PSSwilycsml,RSwilycsml,RS1wilycsml)

# (18) Bicompartemental (Ferguson and Boston 1993)
BC=try(nls(trait ~ a*e^(-b*dim)+c*e^(-d*dim), data=x, na.action=na.exclude,
start=list(a=26.6, b=0.0017, c=-7.68, d=0.08),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(BC)=="nls" & BC$convInfo$isConv==TRUE){
wBC=c(BC$convInfo$stopMessage,BC$convInfo$isConv)
coefBC=length(coef(BC))
R2BC=cor(trait,predict(BC),use="complete.obs")^2
R2adjBC=1-(n-1)/(n-coefBC)*(1-R2BC)
RSEBC=if(wBC[2]=="TRUE"){summary(BC)$sigma} else {"NA"}
LBC=logLik(BC)
AICBC=AIC(BC)
AICCBC=AICBC+(2*coefBC*(coefBC+1)/n-coefBC-1)
BICBC=BIC(BC)
mBC=BC$call
RSSBC=sum(residuals(BC)^2, na.rm=TRUE)
TSSBC=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSBC=sum((predict(BC)-mean(predict(BC), na.rm=TRUE))^2, na.rm=TRUE)
FBC=if(TSSBC>PSSBC){TSSBC/PSSBC} else {PSSBC/TSSBC}
RSDBC=sqrt(RSSBC/(n-coefBC))
RSBC=residuals(BC)
RS1BC=as.numeric(c(residuals(BC)[-1],"NA"))
DWBC=sum((RSBC-RS1BC)^2, na.rm=TRUE)/sum(RSBC^2, na.rm=TRUE)
paramBC=coef(BC)
summaryBC=summary(BC)
	}else
{wBC=c("Missing value or infinity produced","FALSE")
coefBC=NA
R2BC="NA"
R2adjBC="NA"
RSEBC="NA"
LBC="NA"
AICBC="NA"
AICCBC="NA"
BICBC="NA"
mBC="NA"
RSSBC="NA"
FBC="NA"
RSDBC="NA"
DWBC="NA"
paramBC="NA"
summaryBC="NA"}

remove(BC,TSSBC,PSSBC,RSBC,RS1BC)

# (19) Dijkstra
DJK=try(nls(trait ~ a * e^(b*(1-e^(-c*dim))  /  c - (d*dim)), data=x, na.action=na.exclude,
start=list(a=19, b=0.027, c=0.08, d=0.0017),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(DJK)=="nls" & DJK$convInfo$isConv==TRUE){
wDJK=c(DJK$convInfo$stopMessage,DJK$convInfo$isConv)
coefDJK=length(coef(DJK))
R2DJK=cor(trait,predict(DJK),use="complete.obs")^2
R2adjDJK=1-(n-1)/(n-coefDJK)*(1-R2DJK)
RSEDJK=if(wDJK[2]=="TRUE"){summary(DJK)$sigma} else {"NA"}
LDJK=logLik(DJK)
AICDJK=AIC(DJK)
AICCDJK=AICDJK+(2*coefDJK*(coefDJK+1)/n-coefDJK-1)
BICDJK=BIC(DJK)
mDJK=DJK$call
RSSDJK=sum(residuals(DJK)^2, na.rm=TRUE)
TSSDJK=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSDJK=sum((predict(DJK)-mean(predict(DJK), na.rm=TRUE))^2, na.rm=TRUE)
FDJK=if(TSSDJK>PSSDJK){TSSDJK/PSSDJK} else {PSSDJK/TSSDJK}
RSDDJK=sqrt(RSSDJK/(n-coefDJK))
RSDJK=residuals(DJK)
RS1DJK=as.numeric(c(residuals(DJK)[-1],"NA"))
DWDJK=sum((RSDJK-RS1DJK)^2, na.rm=TRUE)/sum(RSDJK^2, na.rm=TRUE)
paramDJK=coef(DJK)
summaryDJK=summary(DJK)
  }else
{wDJK=c("Missing value or infinity produced","FALSE")
coefDJK=NA
R2DJK="NA"
R2adjDJK="NA"
RSEDJK="NA"
LDJK="NA"
AICDJK="NA"
AICCDJK="NA"
BICDJK="NA"
mDJK="NA"
RSSDJK="NA"
FDJK="NA"
RSDDJK="NA"
DWDJK="NA"
paramDJK="NA"
summaryDJK="NA"}

remove(DJK,TSSDJK,PSSDJK,RSDJK,RS1DJK)

# (20) Morant and Gnanasakthy (Pollott et al 2000)
MG2=try(nls(trait ~ e^(a + b*((dim-150)/100) + c*((dim-150)/100)^2 + d/dim), data=x, na.action=na.exclude,
start=list(a=3, b=-0.18, c=0.002, d=-1.4),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(MG2)=="nls" & MG2$convInfo$isConv==TRUE){
wMG2=c(MG2$convInfo$stopMessage,MG2$convInfo$isConv)
coefMG2=length(coef(MG2))
R2MG2=cor(trait,predict(MG2),use="complete.obs")^2
R2adjMG2=1-(n-1)/(n-coefMG2)*(1-R2MG2)
RSEMG2=if(wMG2[2]=="TRUE"){summary(MG2)$sigma} else {"NA"}
LMG2=logLik(MG2)
AICMG2=AIC(MG2)
AICCMG2=AICMG2+(2*coefMG2*(coefMG2+1)/n-coefMG2-1)
BICMG2=BIC(MG2)
mMG2=MG2$call
RSSMG2=sum(residuals(MG2)^2, na.rm=TRUE)
TSSMG2=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSMG2=sum((predict(MG2)-mean(predict(MG2), na.rm=TRUE))^2, na.rm=TRUE)
FMG2=if(TSSMG2>PSSMG2){TSSMG2/PSSMG2} else {PSSMG2/TSSMG2}
RSDMG2=sqrt(RSSMG2/(n-coefMG2))
RSMG2=residuals(MG2)
RS1MG2=as.numeric(c(residuals(MG2)[-1],"NA"))
DWMG2=sum((RSMG2-RS1MG2)^2, na.rm=TRUE)/sum(RSMG2^2, na.rm=TRUE)
paramMG2=coef(MG2)
summaryMG2=summary(MG2)
  }else
{wMG2=c("Missing value or infinity produced","FALSE")
coefMG2=NA
R2MG2="NA"
R2adjMG2="NA"
RSEMG2="NA"
LMG2="NA"
AICMG2="NA"
AICCMG2="NA"
BICMG2="NA"
mMG2="NA"
RSSMG2="NA"
FMG2="NA"
RSDMG2="NA"
DWMG2="NA"
paramMG2="NA"
summaryMG2="NA"}

remove(MG2,TSSMG2,PSSMG2,RSMG2,RS1MG2)

# (21) Morant and Gnanasakthy (Vargas et al 2000)
MG4=try(nls(trait ~ a*e^(b * ((dim-150)/100)/2 + c/dim - d*(1+((dim-21.4)/100)/2)*((dim-21.4)/100)),
data=x, na.action=na.exclude, start=list(a=20.5, b=-0.38, c=-1.45, d=-0.005),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(MG4)=="nls" & MG4$convInfo$isConv==TRUE){
wMG4=c(MG4$convInfo$stopMessage,MG4$convInfo$isConv)
coefMG4=length(coef(MG4))
R2MG4=cor(trait,predict(MG4),use="complete.obs")^2
R2adjMG4=1-(n-1)/(n-coefMG4)*(1-R2MG4)
RSEMG4=if(wMG4[2]=="TRUE"){summary(MG4)$sigma} else {"NA"}
LMG4=logLik(MG4)
AICMG4=AIC(MG4)
AICCMG4=AICMG4+(2*coefMG4*(coefMG4+1)/n-coefMG4-1)
BICMG4=BIC(MG4)
mMG4=MG4$call
RSSMG4=sum(residuals(MG4)^2, na.rm=TRUE)
TSSMG4=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSMG4=sum((predict(MG4)-mean(predict(MG4), na.rm=TRUE))^2, na.rm=TRUE)
FMG4=if(TSSMG4>PSSMG4){TSSMG4/PSSMG4} else {PSSMG4/TSSMG4}
RSDMG4=sqrt(RSSMG4/(n-coefMG4))
RSMG4=residuals(MG4)
RS1MG4=as.numeric(c(residuals(MG4)[-1],"NA"))
DWMG4=sum((RSMG4-RS1MG4)^2, na.rm=TRUE)/sum(RSMG4^2, na.rm=TRUE)
paramMG4=coef(MG4)
summaryMG4=summary(MG4)
  }else
{wMG4=c("Missing value or infinity produced","FALSE")
coefMG4=NA
R2MG4="NA"
R2adjMG4="NA"
RSEMG4="NA"
LMG4="NA"
AICMG4="NA"
AICCMG4="NA"
BICMG4="NA"
mMG4="NA"
RSSMG4="NA"
FMG4="NA"
RSDMG4="NA"
DWMG4="NA"
paramMG4="NA"
summaryMG4="NA"}

remove(MG4,TSSMG4,PSSMG4,RSMG4,RS1MG4)

# (22) Morant and Gnanasakthy (Adediran et al. 2012)
MG=try(nls(trait ~ e^(a - b*((dim-150)/100) + c*((dim-150)/100)^2 + d/dim), data=x, na.action=na.exclude,
start=list(a=3, b=0.18, c=0.002, d=-1.4),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(MG)=="nls" & MG$convInfo$isConv==TRUE){
wMG=c(MG$convInfo$stopMessage,MG$convInfo$isConv)
coefMG=length(coef(MG))
R2MG=cor(trait,predict(MG),use="complete.obs")^2
R2adjMG=1-(n-1)/(n-coefMG)*(1-R2MG)
RSEMG=if(wMG[2]=="TRUE"){summary(MG)$sigma} else {"NA"}
LMG=logLik(MG)
AICMG=AIC(MG)
AICCMG=AICMG+(2*coefMG*(coefMG+1)/n-coefMG-1)
BICMG=BIC(MG)
mMG=MG$call
RSSMG=sum(residuals(MG)^2, na.rm=TRUE)
TSSMG=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSMG=sum((predict(MG)-mean(predict(MG), na.rm=TRUE))^2, na.rm=TRUE)
FMG=if(TSSMG>PSSMG){TSSMG/PSSMG} else {PSSMG/TSSMG}
RSDMG=sqrt(RSSMG/(n-coefMG))
RSMG=residuals(MG)
RS1MG=as.numeric(c(residuals(MG)[-1],"NA"))
DWMG=sum((RSMG-RS1MG)^2, na.rm=TRUE)/sum(RSMG^2, na.rm=TRUE)
paramMG=coef(MG)
summaryMG=summary(MG)
  }else
{wMG=c("Missing value or infinity produced","FALSE")
coefMG=NA
R2MG="NA"
R2adjMG="NA"
RSEMG="NA"
LMG="NA"
AICMG="NA"
AICCMG="NA"
BICMG="NA"
mMG="NA"
RSSMG="NA"
FMG="NA"
RSDMG="NA"
DWMG="NA"
paramMG="NA"
summaryMG="NA"}

remove(MG,TSSMG,PSSMG,RSMG,RS1MG)

# (23) Khandekar (Guo and Swalve)
KHN=try(nls(trait ~ a+b*dim + c*dim^2 + d*dim^3 + f*log(dim), data=x, na.action=na.exclude,
start=list(a=15, b=-0.15, c=0.00043, d=0.0000005, f=4.05),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(KHN)=="nls" & KHN$convInfo$isConv==TRUE){
wKHN=c(KHN$convInfo$stopMessage,KHN$convInfo$isConv)
coefKHN=length(coef(KHN))
R2KHN=cor(trait,predict(KHN),use="complete.obs")^2
R2adjKHN=1-(n-1)/(n-coefKHN)*(1-R2KHN)
RSEKHN=if(wKHN[2]=="TRUE"){summary(KHN)$sigma} else {"NA"}
LKHN=logLik(KHN)
AICKHN=AIC(KHN)
AICCKHN=AICKHN+(2*coefKHN*(coefKHN+1)/n-coefKHN-1)
BICKHN=BIC(KHN)
mKHN=KHN$call
RSSKHN=sum(residuals(KHN)^2, na.rm=TRUE)
TSSKHN=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSKHN=sum((predict(KHN)-mean(predict(KHN), na.rm=TRUE))^2, na.rm=TRUE)
FKHN=if(TSSKHN>PSSKHN){TSSKHN/PSSKHN} else {PSSKHN/TSSKHN}
RSDKHN=sqrt(RSSKHN/(n-coefKHN))
RSKHN=residuals(KHN)
RS1KHN=as.numeric(c(residuals(KHN)[-1],"NA"))
DWKHN=sum((RSKHN-RS1KHN)^2, na.rm=TRUE)/sum(RSKHN^2, na.rm=TRUE)
paramKHN=coef(KHN)
summaryKHN=summary(KHN)
  }else
{wKHN=c("Missing value or infinity produced","FALSE")
coefKHN=NA
R2KHN="NA"
R2adjKHN="NA"
RSEKHN="NA"
LKHN="NA"
AICKHN="NA"
AICCKHN="NA"
BICKHN="NA"
mKHN="NA"
RSSKHN="NA"
FKHN="NA"
RSDKHN="NA"
DWKHN="NA"
paramKHN="NA"
summaryKHN="NA"}

remove(KHN,TSSKHN,PSSKHN,RSKHN,RS1KHN)

# (24) Ali and Schaeffer
AS=try(nls(trait ~ a + b*(dim/340) + c*(dim^2/340) + d*log(340/dim) + f*log(340/dim)^2, data=x, na.action=na.exclude,
start=list(a=19, b=-5, c=0.003, d=5.3, f=-1.1),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(AS)=="nls" & AS$convInfo$isConv==TRUE){
wAS=c(AS$convInfo$stopMessage,AS$convInfo$isConv)
coefAS=length(coef(AS))
R2AS=cor(trait,predict(AS),use="complete.obs")^2
R2adjAS=1-(n-1)/(n-coefAS)*(1-R2AS)
RSEAS=if(wAS[2]=="TRUE"){summary(AS)$sigma} else {"NA"}
LAS=logLik(AS)
AICAS=AIC(AS)
AICCAS=AICAS+(2*coefAS*(coefAS+1)/n-coefAS-1)
BICAS=BIC(AS)
mAS=AS$call
RSSAS=sum(residuals(AS)^2, na.rm=TRUE)
TSSAS=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSAS=sum((predict(AS)-mean(predict(AS), na.rm=TRUE))^2, na.rm=TRUE)
FAS=if(TSSAS>PSSAS){TSSAS/PSSAS} else {PSSAS/TSSAS}
RSDAS=sqrt(RSSAS/(n-coefAS))
RSAS=residuals(AS)
RS1AS=as.numeric(c(residuals(AS)[-1],"NA"))
DWAS=sum((RSAS-RS1AS)^2, na.rm=TRUE)/sum(RSAS^2, na.rm=TRUE)
paramAS=coef(AS)
summaryAS=summary(AS)
  }else
{wAS=c("Missing value or infinity produced","FALSE")
coefAS=NA
R2AS="NA"
R2adjAS="NA"
RSEAS="NA"
LAS="NA"
AICAS="NA"
AICCAS="NA"
BICAS="NA"
mAS="NA"
RSSAS="NA"
FAS="NA"
RSDAS="NA"
DWAS="NA"
paramAS="NA"
summaryAS="NA"}

remove(AS,TSSAS,PSSAS,RSAS,RS1AS)

# (25) Fractional Polynomial (Elvira et al. 2013)
FRP=try(nls(trait ~ a + b*dim + c*log(dim)+ d*dim^0.5 + f*dim^2, data=x, na.action=na.exclude,
start=list(a=15, b=-0.08, c=8, d=-3, f=-0.00004),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(FRP)=="nls" & FRP$convInfo$isConv==TRUE){
wFRP=c(FRP$convInfo$stopMessage,FRP$convInfo$isConv)
coefFRP=length(coef(FRP))
R2FRP=cor(trait,predict(FRP),use="complete.obs")^2
R2adjFRP=1-(n-1)/(n-coefFRP)*(1-R2FRP)
RSEFRP=if(wFRP[2]=="TRUE"){summary(FRP)$sigma} else {"NA"}
LFRP=logLik(FRP)
AICFRP=AIC(FRP)
AICCFRP=AICFRP+(2*coefFRP*(coefFRP+1)/n-coefFRP-1)
BICFRP=BIC(FRP)
mFRP=FRP$call
RSSFRP=sum(residuals(FRP)^2, na.rm=TRUE)
TSSFRP=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSFRP=sum((predict(FRP)-mean(predict(FRP), na.rm=TRUE))^2, na.rm=TRUE)
FFRP=if(TSSFRP>PSSFRP){TSSFRP/PSSFRP} else {PSSFRP/TSSFRP}
RSDFRP=sqrt(RSSFRP/(n-coefFRP))
RSFRP=residuals(FRP)
RS1FRP=as.numeric(c(residuals(FRP)[-1],"NA"))
DWFRP=sum((RSFRP-RS1FRP)^2, na.rm=TRUE)/sum(RSFRP^2, na.rm=TRUE)
paramFRP=coef(FRP)
summaryFRP=summary(FRP)
  }else
{wFRP=c("Missing value or infinity produced","FALSE")
coefFRP=NA
R2FRP="NA"
R2adjFRP="NA"
RSEFRP="NA"
LFRP="NA"
AICFRP="NA"
AICCFRP="NA"
BICFRP="NA"
mFRP="NA"
RSSFRP="NA"
FFRP="NA"
RSDFRP="NA"
DWFRP="NA"
paramFRP="NA"
summaryFRP="NA"}

remove(FRP,TSSFRP,PSSFRP,RSFRP,RS1FRP)

# (26) Pollott multiplicative
PTmult=try(nls(trait ~ a/(1+((1-0.999999)/0.999999)*e^(-0.1*dim))  *  (2-e^(-b*dim)), data=x, na.action=na.exclude,
start=list(a=25, b=-0.001),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(PTmult)=="nls" & PTmult$convInfo$isConv==TRUE){
wPTmult=c(PTmult$convInfo$stopMessage,PTmult$convInfo$isConv)
coefPTmult=length(coef(PTmult))
R2PTmult=cor(trait,predict(PTmult),use="complete.obs")^2
R2adjPTmult=1-(n-1)/(n-coefPTmult)*(1-R2PTmult)
RSEPTmult=if(wPTmult[2]=="TRUE"){summary(PTmult)$sigma} else {"NA"}
LPTmult=logLik(PTmult)
AICPTmult=AIC(PTmult)
AICCPTmult=AICPTmult+(2*coefPTmult*(coefPTmult+1)/n-coefPTmult-1)
BICPTmult=BIC(PTmult)
mPTmult=PTmult$call
RSSPTmult=sum(residuals(PTmult)^2, na.rm=TRUE)
TSSPTmult=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSPTmult=sum((predict(PTmult)-mean(predict(PTmult), na.rm=TRUE))^2, na.rm=TRUE)
FPTmult=if(TSSPTmult>PSSPTmult){TSSPTmult/PSSPTmult} else {PSSPTmult/TSSPTmult}
RSDPTmult=sqrt(RSSPTmult/(n-coefPTmult))
RSPTmult=residuals(PTmult)
RS1PTmult=as.numeric(c(residuals(PTmult)[-1],"NA"))
DWPTmult=sum((RSPTmult-RS1PTmult)^2, na.rm=TRUE)/sum(RSPTmult^2, na.rm=TRUE)
paramPTmult=coef(PTmult)
summaryPTmult=summary(PTmult)
  }else
{wPTmult=c("Missing value or infinity produced","FALSE")
coefPTmult=NA
R2PTmult="NA"
R2adjPTmult="NA"
RSEPTmult="NA"
LPTmult="NA"
AICPTmult="NA"
AICCPTmult="NA"
BICPTmult="NA"
mPTmult="NA"
RSSPTmult="NA"
FPTmult="NA"
RSDPTmult="NA"
DWPTmult="NA"
paramPTmult="NA"
summaryPTmult="NA"}

remove(PTmult,TSSPTmult,PSSPTmult,RSPTmult,RS1PTmult)

# (27) Pollott modified
PTmod=try(nls(trait ~ a/((1+b*(e^(-c*dim))))*(2-e^(-d*dim)), data=x, na.action=na.exclude,
start=list(a=25, b=0.36, c=0.13, d=-0.001),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(PTmod)=="nls" & PTmod$convInfo$isConv==TRUE){
wPTmod=c(PTmod$convInfo$stopMessage,PTmod$convInfo$isConv)
coefPTmod=length(coef(PTmod))
R2PTmod=cor(trait,predict(PTmod),use="complete.obs")^2
R2adjPTmod=1-(n-1)/(n-coefPTmod)*(1-R2PTmod)
RSEPTmod=if(wPTmod[2]=="TRUE"){summary(PTmod)$sigma} else {"NA"}
LPTmod=logLik(PTmod)
AICPTmod=AIC(PTmod)
AICCPTmod=AICPTmod+(2*coefPTmod*(coefPTmod+1)/n-coefPTmod-1)
BICPTmod=BIC(PTmod)
mPTmod=PTmod$call
RSSPTmod=sum(residuals(PTmod)^2, na.rm=TRUE)
TSSPTmod=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSPTmod=sum((predict(PTmod)-mean(predict(PTmod), na.rm=TRUE))^2, na.rm=TRUE)
FPTmod=if(TSSPTmod>PSSPTmod){TSSPTmod/PSSPTmod} else {PSSPTmod/TSSPTmod}
RSDPTmod=sqrt(RSSPTmod/(n-coefPTmod))
RSPTmod=residuals(PTmod)
RS1PTmod=as.numeric(c(residuals(PTmod)[-1],"NA"))
DWPTmod=sum((RSPTmod-RS1PTmod)^2, na.rm=TRUE)/sum(RSPTmod^2, na.rm=TRUE)
paramPTmod=coef(PTmod)
summaryPTmod=summary(PTmod)
  }else
{wPTmod=c("Missing value or infinity produced","FALSE")
coefPTmod=NA
R2PTmod="NA"
R2adjPTmod="NA"
RSEPTmod="NA"
LPTmod="NA"
AICPTmod="NA"
AICCPTmod="NA"
BICPTmod="NA"
mPTmod="NA"
RSSPTmod="NA"
FPTmod="NA"
RSDPTmod="NA"
DWPTmod="NA"
paramPTmod="NA"
summaryPTmod="NA"}

remove(PTmod,TSSPTmod,PSSPTmod,RSPTmod,RS1PTmod)

# (28) Monophasic Grossman
MonoG=try(nls(trait ~ a*b*(1-tanh(b*(dim-c))), data=x, na.action=na.exclude,
start=list(a=7703, b=0.002, c=277),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(MonoG)=="nls" & MonoG$convInfo$isConv==TRUE){
wMonoG=c(MonoG$convInfo$stopMessage,MonoG$convInfo$isConv)
coefMonoG=length(coef(MonoG))
R2MonoG=cor(trait,predict(MonoG),use="complete.obs")^2
R2adjMonoG=1-(n-1)/(n-coefMonoG)*(1-R2MonoG)
RSEMonoG=if(wMonoG[2]=="TRUE"){summary(MonoG)$sigma} else {"NA"}
LMonoG=logLik(MonoG)
AICMonoG=AIC(MonoG)
AICCMonoG=AICMonoG+(2*coefMonoG*(coefMonoG+1)/n-coefMonoG-1)
BICMonoG=BIC(MonoG)
mMonoG=MonoG$call
RSSMonoG=sum(residuals(MonoG)^2, na.rm=TRUE)
TSSMonoG=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSMonoG=sum((predict(MonoG)-mean(predict(MonoG), na.rm=TRUE))^2, na.rm=TRUE)
FMonoG=if(TSSMonoG>PSSMonoG){TSSMonoG/PSSMonoG} else {PSSMonoG/TSSMonoG}
RSDMonoG=sqrt(RSSMonoG/(n-coefMonoG))
RSMonoG=residuals(MonoG)
RS1MonoG=as.numeric(c(residuals(MonoG)[-1],"NA"))
DWMonoG=sum((RSMonoG-RS1MonoG)^2, na.rm=TRUE)/sum(RSMonoG^2, na.rm=TRUE)
paramMonoG=coef(MonoG)
summaryMonoG=summary(MonoG)
  }else
{wMonoG=c("Missing value or infinity produced","FALSE")
coefMonoG=NA
R2MonoG="NA"
R2adjMonoG="NA"
RSEMonoG="NA"
LMonoG="NA"
AICMonoG="NA"
AICCMonoG="NA"
BICMonoG="NA"
mMonoG="NA"
RSSMonoG="NA"
FMonoG="NA"
RSDMonoG="NA"
DWMonoG="NA"
paramMonoG="NA"
summaryMonoG="NA"}

remove(MonoG,TSSMonoG,PSSMonoG,RSMonoG,RS1MonoG)

# (29) Monophasic Power Transformed (Grossman 1999)
MonoGpw=try(nls(trait ~ a*(1-tanh(b*(dim^k-c))^2), data=x, na.action=na.exclude,
start=list(a=24.5707, b=0.6292, c=1.9977, k=0.1978),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(MonoGpw)=="nls" & MonoGpw$convInfo$isConv==TRUE){
wMonoGpw=c(MonoGpw$convInfo$stopMessage,MonoGpw$convInfo$isConv)
coefMonoGpw=length(coef(MonoGpw))
R2MonoGpw=cor(trait,predict(MonoGpw),use="complete.obs")^2
R2adjMonoGpw=1-(n-1)/(n-coefMonoGpw)*(1-R2MonoGpw)
RSEMonoGpw=if(wMonoGpw[2]=="TRUE"){summary(MonoGpw)$sigma} else {"NA"}
LMonoGpw=logLik(MonoGpw)
AICMonoGpw=AIC(MonoGpw)
AICCMonoGpw=AICMonoGpw+(2*coefMonoGpw*(coefMonoGpw+1)/n-coefMonoGpw-1)
BICMonoGpw=BIC(MonoGpw)
mMonoGpw=MonoGpw$call
RSSMonoGpw=sum(residuals(MonoGpw)^2, na.rm=TRUE)
TSSMonoGpw=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSMonoGpw=sum((predict(MonoGpw)-mean(predict(MonoGpw), na.rm=TRUE))^2, na.rm=TRUE)
FMonoGpw=if(TSSMonoGpw>PSSMonoGpw){TSSMonoGpw/PSSMonoGpw} else {PSSMonoGpw/TSSMonoGpw}
RSDMonoGpw=sqrt(RSSMonoGpw/(n-coefMonoGpw))
RSMonoGpw=residuals(MonoGpw)
RS1MonoGpw=as.numeric(c(residuals(MonoGpw)[-1],"NA"))
DWMonoGpw=sum((RSMonoGpw-RS1MonoGpw)^2, na.rm=TRUE)/sum(RSMonoGpw^2, na.rm=TRUE)
paramMonoGpw=coef(MonoGpw)
summaryMonoGpw=summary(MonoGpw)
  }else
{wMonoGpw=c("Missing value or infinity produced","FALSE")
coefMonoGpw=NA
R2MonoGpw="NA"
R2adjMonoGpw="NA"
RSEMonoGpw="NA"
LMonoGpw="NA"
AICMonoGpw="NA"
AICCMonoGpw="NA"
BICMonoGpw="NA"
mMonoGpw="NA"
RSSMonoGpw="NA"
FMonoGpw="NA"
RSDMonoGpw="NA"
DWMonoGpw="NA"
paramMonoGpw="NA"
summaryMonoGpw="NA"}

remove(MonoGpw,TSSMonoGpw,PSSMonoGpw,RSMonoGpw,RS1MonoGpw)

# (30) Diphasic (Grossman 1999)
DiG=try(nls(trait ~ a*b*(1-tanh(b*(dim-c))^2)+d*f*(1-tanh(f*(dim-g))^2), data=x, na.action=na.exclude,
start=list(a=353.8,b=0.016,c=36.3, d=7371, f=0.003, g=113.5),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(DiG)=="nls" & DiG$convInfo$isConv==TRUE){
wDiG=c(DiG$convInfo$stopMessage,DiG$convInfo$isConv)
coefDiG=length(coef(DiG))
R2DiG=cor(trait,predict(DiG),use="complete.obs")^2
R2adjDiG=1-(n-1)/(n-coefDiG)*(1-R2DiG)
RSEDiG=if(wDiG[2]=="TRUE"){summary(DiG)$sigma} else {"NA"}
LDiG=logLik(DiG)
AICDiG=AIC(DiG)
AICCDiG=AICDiG+(2*coefDiG*(coefDiG+1)/n-coefDiG-1)
BICDiG=BIC(DiG)
mDiG=DiG$call
RSSDiG=sum(residuals(DiG)^2, na.rm=TRUE)
TSSDiG=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSDiG=sum((predict(DiG)-mean(predict(DiG), na.rm=TRUE))^2, na.rm=TRUE)
FDiG=if(TSSDiG>PSSDiG){TSSDiG/PSSDiG} else {PSSDiG/TSSDiG}
RSDDiG=sqrt(RSSDiG/(n-coefDiG))
RSDiG=residuals(DiG)
RS1DiG=as.numeric(c(residuals(DiG)[-1],"NA"))
DWDiG=sum((RSDiG-RS1DiG)^2, na.rm=TRUE)/sum(RSDiG^2, na.rm=TRUE)
paramDiG=coef(DiG)
summaryDiG=summary(DiG)
  }else
{wDiG=c("Missing value or infinity produced","FALSE")
coefDiG=NA
R2DiG="NA"
R2adjDiG="NA"
RSEDiG="NA"
LDiG="NA"
AICDiG="NA"
AICCDiG="NA"
BICDiG="NA"
mDiG="NA"
RSSDiG="NA"
FDiG="NA"
RSDDiG="NA"
DWDiG="NA"
paramDiG="NA"
summaryDiG="NA"}

remove(DiG,TSSDiG,PSSDiG,RSDiG,RS1DiG)

# (31) Diphasic Power Transformed (Grossman 1999)
DiGpw=try(nls(trait ~ a*b*(1-tanh(b*(dim^k-c))^2)+d*f*(1-tanh(f*(dim-g))^2), data=x, na.action=na.exclude,
start=list(a=15.99,b=0.3351,c=4.693, d=8687, f=0.00226, g=80.33, k=0.435),control=list(maxiter=100,warnOnly=TRUE)),silent=TRUE)

#options(warn=-1)
if(class(DiGpw)=="nls" & DiGpw$convInfo$isConv==TRUE){
wDiGpw=c(DiGpw$convInfo$stopMessage,DiGpw$convInfo$isConv)
coefDiGpw=length(coef(DiGpw))
R2DiGpw=cor(trait,predict(DiGpw),use="complete.obs")^2
R2adjDiGpw=1-(n-1)/(n-coefDiGpw)*(1-R2DiGpw)
RSEDiGpw=if(wDiGpw[2]=="TRUE"){summary(DiGpw)$sigma} else {"NA"}
LDiGpw=logLik(DiGpw)
AICDiGpw=AIC(DiGpw)
AICCDiGpw=AICDiGpw+(2*coefDiGpw*(coefDiGpw+1)/n-coefDiGpw-1)
BICDiGpw=BIC(DiGpw)
mDiGpw=DiGpw$call
RSSDiGpw=sum(residuals(DiGpw)^2, na.rm=TRUE)
TSSDiGpw=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSDiGpw=sum((predict(DiGpw)-mean(predict(DiGpw), na.rm=TRUE))^2, na.rm=TRUE)
FDiGpw=if(TSSDiGpw>PSSDiGpw){TSSDiGpw/PSSDiGpw} else {PSSDiGpw/TSSDiGpw}
RSDDiGpw=sqrt(RSSDiGpw/(n-coefDiGpw))
RSDiGpw=residuals(DiGpw)
RS1DiGpw=as.numeric(c(residuals(DiGpw)[-1],"NA"))
DWDiGpw=sum((RSDiGpw-RS1DiGpw)^2, na.rm=TRUE)/sum(RSDiGpw^2, na.rm=TRUE)
paramDiGpw=coef(DiGpw)
summaryDiGpw=summary(DiGpw)
  }else
{wDiGpw=c("Missing value or infinity produced","FALSE")
coefDiGpw=NA
R2DiGpw="NA"
R2adjDiGpw="NA"
RSEDiGpw="NA"
LDiGpw="NA"
AICDiGpw="NA"
AICCDiGpw="NA"
BICDiGpw="NA"
mDiGpw="NA"
RSSDiGpw="NA"
FDiGpw="NA"
RSDDiGpw="NA"
DWDiGpw="NA"
paramDiGpw="NA"
summaryDiGpw="NA"}

remove(DiGpw,TSSDiGpw,PSSDiGpw,RSDiGpw,RS1DiGpw)

# (32) Legendre Polynomial (3th order)
y <- 1:nrow(x)
leg3 <- as.matrix(as.data.frame(polynomial.values(polynomials=legendre.polynomials(n=3, normalized=TRUE),x=scaleX(y, u=-1, v=1))))
colnames(leg3) <- c("leg0", "leg1", "leg2", "leg3")
leg3 <- leg3[, 2:ncol(leg3)]

legpol3=try(lm(trait ~ leg3, data=x, na.action=na.exclude),silent=TRUE)

#options(warn=-1)
if(class(legpol3)=="lm"){
wlegpol3=c("converged","TRUE")
coeflegpol3=length(coef(legpol3))
R2legpol3=cor(trait,predict(legpol3),use="complete.obs")^2
R2adjlegpol3=1-(n-1)/(n-coeflegpol3)*(1-R2legpol3)
RSElegpol3=if(wlegpol3[2]=="TRUE"){summary(legpol3)$sigma} else {"NA"}
Llegpol3=logLik(legpol3)
AIClegpol3=AIC(legpol3)
AICClegpol3=AIClegpol3+(2*coeflegpol3*(coeflegpol3+1)/n-coeflegpol3-1)
BIClegpol3=BIC(legpol3)
mlegpol3=legpol3$call
RSSlegpol3=sum(residuals(legpol3)^2, na.rm=TRUE)
TSSlegpol3=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSlegpol3=sum((predict(legpol3)-mean(predict(legpol3), na.rm=TRUE))^2, na.rm=TRUE)
Flegpol3=if(TSSlegpol3>PSSlegpol3){TSSlegpol3/PSSlegpol3} else {PSSlegpol3/TSSlegpol3}
RSDlegpol3=sqrt(RSSlegpol3/(n-coeflegpol3))
RSlegpol3=residuals(legpol3)
RS1legpol3=as.numeric(c(residuals(legpol3)[-1],"NA"))
DWlegpol3=sum((RSlegpol3-RS1legpol3)^2, na.rm=TRUE)/sum(RSlegpol3^2, na.rm=TRUE)
paramleg3=coef(legpol3)
summaryleg3=summary(legpol3)
  }else
{wlegpol3=c("Missing value or infinity produced","FALSE")
coeflegpol3=NA
R2legpol3="NA"
R2adjlegpol3="NA"
RSElegpol3="NA"
Llegpol3="NA"
AIClegpol3="NA"
AICClegpol3="NA"
BIClegpol3="NA"
mlegpol3="NA"
RSSlegpol3="NA"
Flegpol3="NA"
RSDlegpol3="NA"
DWlegpol3="NA"
paramleg3="NA"
summaryleg3="NA"}

remove(legpol3,TSSlegpol3,PSSlegpol3,RSlegpol3,RS1legpol3)

# (33) Legendre Polynomial (4th order)
y <- 1:nrow(x)
leg4 <- as.matrix(as.data.frame(polynomial.values(polynomials=legendre.polynomials(n=4, normalized=TRUE),x=scaleX(y, u=-1, v=1))))
colnames(leg4) <- c("leg0", "leg1", "leg2", "leg3", "leg4")
leg4 <- leg4[, 2:ncol(leg4)]

legpol4=try(lm(trait ~ leg4, data=x, na.action=na.exclude),silent=TRUE)

#options(warn=-1)
if(class(legpol4)=="lm"){
wlegpol4=c("converged","TRUE")
coeflegpol4=length(coef(legpol4))
R2legpol4=cor(trait,predict(legpol4),use="complete.obs")^2
R2adjlegpol4=1-(n-1)/(n-coeflegpol4)*(1-R2legpol4)
RSElegpol4=if(wlegpol4[2]=="TRUE"){summary(legpol4)$sigma} else {"NA"}
Llegpol4=logLik(legpol4)
AIClegpol4=AIC(legpol4)
AICClegpol4=AIClegpol4+(2*coeflegpol4*(coeflegpol4+1)/n-coeflegpol4-1)
BIClegpol4=BIC(legpol4)
mlegpol4=legpol4$call
RSSlegpol4=sum(residuals(legpol4)^2, na.rm=TRUE)
TSSlegpol4=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSlegpol4=sum((predict(legpol4)-mean(predict(legpol4), na.rm=TRUE))^2, na.rm=TRUE)
Flegpol4=if(TSSlegpol4>PSSlegpol4){TSSlegpol4/PSSlegpol4} else {PSSlegpol4/TSSlegpol4}
RSDlegpol4=sqrt(RSSlegpol4/(n-coeflegpol4))
RSlegpol4=residuals(legpol4)
RS1legpol4=as.numeric(c(residuals(legpol4)[-1],"NA"))
DWlegpol4=sum((RSlegpol4-RS1legpol4)^2, na.rm=TRUE)/sum(RSlegpol4^2, na.rm=TRUE)
paramleg4=coef(legpol4)
summaryleg4=summary(legpol4)
  }else
{wlegpol4=c("Missing value or infinity produced","FALSE")
coeflegpol4=NA
R2legpol4="NA"
R2adjlegpol4="NA"
RSElegpol4="NA"
Llegpol4="NA"
AIClegpol4="NA"
AICClegpol4="NA"
BIClegpol4="NA"
mlegpol4="NA"
RSSlegpol4="NA"
Flegpol4="NA"
RSDlegpol4="NA"
DWlegpol4="NA"
paramleg4="NA"
summaryleg4="NA"}

remove(legpol4,TSSlegpol4,PSSlegpol4,RSlegpol4,RS1legpol4)

# (34) Legendre + Wilmink (Lidauer)
y <- 1:nrow(x)
leg4 <- as.matrix(as.data.frame(polynomial.values(polynomials=legendre.polynomials(n=4, normalized=TRUE),x=scaleX(y, u=-1, v=1))))
colnames(leg4) <- c("leg0", "leg1", "leg2", "leg3", "leg4")
leg4 <- leg4[, 2:ncol(leg4)]

legpolWil=try(nls(trait ~ a*leg4[,1]+b*leg4[,2]+c*leg4[,3]+d*e^(-k*dim), data=x, na.action=na.exclude,
start=list(a=-0.8, b=-0.6,c=0.1,d=25.7, k=0.002)),silent=TRUE)

#options(warn=-1)
if(class(legpolWil)=="nls" & legpolWil$convInfo$isConv==TRUE){
wlegpolWil=c("converged","TRUE")
coeflegpolWil=length(coef(legpolWil))
R2legpolWil=cor(trait,predict(legpolWil),use="complete.obs")^2
R2adjlegpolWil=1-(n-1)/(n-coeflegpolWil)*(1-R2legpolWil)
RSElegpolWil=if(wlegpolWil[2]=="TRUE"){summary(legpolWil)$sigma} else {"NA"}
LlegpolWil=logLik(legpolWil)
AIClegpolWil=AIC(legpolWil)
AICClegpolWil=AIClegpolWil+(2*coeflegpolWil*(coeflegpolWil+1)/n-coeflegpolWil-1)
BIClegpolWil=BIC(legpolWil)
mlegpolWil=legpolWil$call
RSSlegpolWil=sum(residuals(legpolWil)^2, na.rm=TRUE)
TSSlegpolWil=sum((trait-mean(trait))^2, na.rm=TRUE)
PSSlegpolWil=sum((predict(legpolWil)-mean(predict(legpolWil), na.rm=TRUE))^2, na.rm=TRUE)
FlegpolWil=if(TSSlegpolWil>PSSlegpolWil){TSSlegpolWil/PSSlegpolWil} else {PSSlegpolWil/TSSlegpolWil}
RSDlegpolWil=sqrt(RSSlegpolWil/(n-coeflegpolWil))
RSlegpolWil=residuals(legpolWil)
RS1legpolWil=as.numeric(c(residuals(legpolWil)[-1],"NA"))
DWlegpolWil=sum((RSlegpolWil-RS1legpolWil)^2, na.rm=TRUE)/sum(RSlegpolWil^2, na.rm=TRUE)
parampolWil=coef(legpolWil)
summarypolWil=summary(legpolWil)
  }else
{wlegpolWil=c("Missing value or infinity produced","FALSE")
coeflegpolWil=NA
R2legpolWil="NA"
R2adjlegpolWil="NA"
RSElegpolWil="NA"
LlegpolWil="NA"
AIClegpolWil="NA"
AICClegpolWil="NA"
BIClegpolWil="NA"
mlegpolWil="NA"
RSSlegpolWil="NA"
FlegpolWil="NA"
RSDlegpolWil="NA"
DWlegpolWil="NA"
parampolWil="NA"
summarypolWil="NA"}

remove(legpolWil,TSSlegpolWil,PSSlegpolWil,RSlegpolWil,RS1legpolWil)

# (35) Natural Cubic Spline (3 percentiles)
cubsplin3=try(lm(trait ~ ns(dim, df = 4), data = x, na.action=na.exclude),silent=TRUE)

#options(warn=-1)
if(class(cubsplin3)=="lm"){
wcubsplin3=c("converged","TRUE")
coefcubsplin3=length(coef(cubsplin3))
R2cubsplin3=cor(trait,predict(cubsplin3),use="complete.obs")^2
R2adjcubsplin3=1-(n-1)/(n-coefcubsplin3)*(1-R2cubsplin3)
RSEcubsplin3=if(wcubsplin3[2]=="TRUE"){summary(cubsplin3)$sigma} else {"NA"}
Lcubsplin3=logLik(cubsplin3)
AICcubsplin3=AIC(cubsplin3)
AICCcubsplin3=AICcubsplin3+(2*coefcubsplin3*(coefcubsplin3+1)/n-coefcubsplin3-1)
BICcubsplin3=BIC(cubsplin3)
mcubsplin3=cubsplin3$call
RSScubsplin3=sum(residuals(cubsplin3)^2, na.rm=TRUE)
TSScubsplin3=sum((trait-mean(trait))^2, na.rm=TRUE)
PSScubsplin3=sum((predict(cubsplin3)-mean(predict(cubsplin3), na.rm=TRUE))^2, na.rm=TRUE)
Fcubsplin3=if(TSScubsplin3>PSScubsplin3){TSScubsplin3/PSScubsplin3} else {PSScubsplin3/TSScubsplin3}
RSDcubsplin3=sqrt(RSScubsplin3/(n-coefcubsplin3))
RScubsplin3=residuals(cubsplin3)
RS1cubsplin3=as.numeric(c(residuals(cubsplin3)[-1],"NA"))
DWcubsplin3=sum((RScubsplin3-RS1cubsplin3)^2, na.rm=TRUE)/sum(RScubsplin3^2, na.rm=TRUE)
paramcubsplin3=coef(cubsplin3)
summarycubsplin3=summary(cubsplin3)
  }else
{wcubsplin3=c("Missing value or infinity produced","FALSE")
coefcubsplin3=NA
R2cubsplin3="NA"
R2adjcubsplin3="NA"
RSEcubsplin3="NA"
Lcubsplin3="NA"
AICcubsplin3="NA"
AICCcubsplin3="NA"
BICcubsplin3="NA"
mcubsplin3="NA"
RSScubsplin3="NA"
Fcubsplin3="NA"
RSDcubsplin3="NA"
DWcubsplin3="NA"
paramcubsplin3="NA"
summarycubsplin3="NA"}

remove(cubsplin3,TSScubsplin3,PSScubsplin3,RScubsplin3,RS1cubsplin3)

# (36) Natural Cubic Spline (4 percentiles)
cubsplin4=try(lm(trait ~ ns(dim, df = 5), data = x, na.action=na.exclude),silent=TRUE)

#options(warn=-1)
if(class(cubsplin4)=="lm"){
wcubsplin4=c("converged","TRUE")
coefcubsplin4=length(coef(cubsplin4))
R2cubsplin4=cor(trait,predict(cubsplin4),use="complete.obs")^2
R2adjcubsplin4=1-(n-1)/(n-coefcubsplin4)*(1-R2cubsplin4)
RSEcubsplin4=if(wcubsplin4[2]=="TRUE"){summary(cubsplin4)$sigma} else {"NA"}
Lcubsplin4=logLik(cubsplin4)
AICcubsplin4=AIC(cubsplin4)
AICCcubsplin4=AICcubsplin4+(2*coefcubsplin4*(coefcubsplin4+1)/n-coefcubsplin4-1)
BICcubsplin4=BIC(cubsplin4)
mcubsplin4=cubsplin4$call
RSScubsplin4=sum(residuals(cubsplin4)^2, na.rm=TRUE)
TSScubsplin4=sum((trait-mean(trait))^2, na.rm=TRUE)
PSScubsplin4=sum((predict(cubsplin4)-mean(predict(cubsplin4), na.rm=TRUE))^2, na.rm=TRUE)
Fcubsplin4=if(TSScubsplin4>PSScubsplin4){TSScubsplin4/PSScubsplin4} else {PSScubsplin4/TSScubsplin4}
RSDcubsplin4=sqrt(RSScubsplin4/(n-coefcubsplin4))
RScubsplin4=residuals(cubsplin4)
RS1cubsplin4=as.numeric(c(residuals(cubsplin4)[-1],"NA"))
DWcubsplin4=sum((RScubsplin4-RS1cubsplin4)^2, na.rm=TRUE)/sum(RScubsplin4^2, na.rm=TRUE)
paramcubsplin4=coef(cubsplin4)
summarycubsplin4=summary(cubsplin4)
  }else
{wcubsplin4=c("Missing value or infinity produced","FALSE")
coefcubsplin4=NA
R2cubsplin4="NA"
R2adjcubsplin4="NA"
RSEcubsplin4="NA"
Lcubsplin4="NA"
AICcubsplin4="NA"
AICCcubsplin4="NA"
BICcubsplin4="NA"
mcubsplin4="NA"
RSScubsplin4="NA"
Fcubsplin4="NA"
RSDcubsplin4="NA"
DWcubsplin4="NA"
paramcubsplin4="NA"
summarycubsplin4="NA"}

remove(cubsplin4,TSScubsplin4,PSScubsplin4,RScubsplin4,RS1cubsplin4)

# (37) Natural Cubic Spline (5 percentiles)
cubsplin5=try(lm(trait ~ ns(dim, df = 6), data = x, na.action=na.exclude),silent=TRUE)

#options(warn=-1)
if(class(cubsplin5)=="lm"){
wcubsplin5=c("converged","TRUE")
coefcubsplin5=length(coef(cubsplin5))
R2cubsplin5=cor(trait,predict(cubsplin5),use="complete.obs")^2
R2adjcubsplin5=1-(n-1)/(n-coefcubsplin5)*(1-R2cubsplin5)
RSEcubsplin5=if(wcubsplin5[2]=="TRUE"){summary(cubsplin5)$sigma} else {"NA"}
Lcubsplin5=logLik(cubsplin5)
AICcubsplin5=AIC(cubsplin5)
AICCcubsplin5=AICcubsplin5+(2*coefcubsplin5*(coefcubsplin5+1)/n-coefcubsplin5-1)
BICcubsplin5=BIC(cubsplin5)
mcubsplin5=cubsplin5$call
RSScubsplin5=sum(residuals(cubsplin5)^2, na.rm=TRUE)
TSScubsplin5=sum((trait-mean(trait))^2, na.rm=TRUE)
PSScubsplin5=sum((predict(cubsplin5)-mean(predict(cubsplin5), na.rm=TRUE))^2, na.rm=TRUE)
Fcubsplin5=if(TSScubsplin5>PSScubsplin5){TSScubsplin5/PSScubsplin5} else {PSScubsplin5/TSScubsplin5}
RSDcubsplin5=sqrt(RSScubsplin5/(n-coefcubsplin5))
RScubsplin5=residuals(cubsplin5)
RS1cubsplin5=as.numeric(c(residuals(cubsplin5)[-1],"NA"))
DWcubsplin5=sum((RScubsplin5-RS1cubsplin5)^2, na.rm=TRUE)/sum(RScubsplin5^2, na.rm=TRUE)
paramcubsplin5=coef(cubsplin5)
summarycubsplin5=summary(cubsplin5)
  }else
{wcubsplin5=c("Missing value or infinity produced","FALSE")
coefcubsplin5=NA
R2cubsplin5="NA"
R2adjcubsplin5="NA"
RSEcubsplin5="NA"
Lcubsplin5="NA"
AICcubsplin5="NA"
AICCcubsplin5="NA"
BICcubsplin5="NA"
mcubsplin5="NA"
RSScubsplin5="NA"
Fcubsplin5="NA"
RSDcubsplin5="NA"
DWcubsplin5="NA"
paramcubsplin5="NA"
summarycubsplin5="NA"}

remove(cubsplin5,TSScubsplin5,PSScubsplin5,RScubsplin5,RS1cubsplin5)

# (38) Natural Cubic Spline (defined knots according to Harrell 2001)
cubsplindef=try(lm(trait ~ ns(dim, knots =c(49,78,112,157,210)), data = x, na.action=na.exclude),silent=TRUE)

#options(warn=-1)
if(class(cubsplindef)=="lm"){
wcubsplindef=c("converged","TRUE")
coefcubsplindef=length(coef(cubsplindef))
R2cubsplindef=cor(trait,predict(cubsplindef),use="complete.obs")^2
R2adjcubsplindef=1-(n-1)/(n-coefcubsplindef)*(1-R2cubsplindef)
RSEcubsplindef=if(wcubsplindef[2]=="TRUE"){summary(cubsplindef)$sigma} else {"NA"}
Lcubsplindef=logLik(cubsplindef)
AICcubsplindef=AIC(cubsplindef)
AICCcubsplindef=AICcubsplindef+(2*coefcubsplindef*(coefcubsplindef+1)/n-coefcubsplindef-1)
BICcubsplindef=BIC(cubsplindef)
mcubsplindef=cubsplindef$call
RSScubsplindef=sum(residuals(cubsplindef)^2, na.rm=TRUE)
TSScubsplindef=sum((trait-mean(trait))^2, na.rm=TRUE)
PSScubsplindef=sum((predict(cubsplindef)-mean(predict(cubsplindef), na.rm=TRUE))^2, na.rm=TRUE)
Fcubsplindef=if(TSScubsplindef>PSScubsplindef){TSScubsplindef/PSScubsplindef} else {PSScubsplindef/TSScubsplindef}
RSDcubsplindef=sqrt(RSScubsplindef/(n-coefcubsplindef))
RScubsplindef=residuals(cubsplindef)
RS1cubsplindef=as.numeric(c(residuals(cubsplindef)[-1],"NA"))
DWcubsplindef=sum((RScubsplindef-RS1cubsplindef)^2, na.rm=TRUE)/sum(RScubsplindef^2, na.rm=TRUE)
paramcubsplindef=coef(cubsplindef)
summarycubsplindef=summary(cubsplindef)
  }else
{wcubsplindef=c("Missing value or infinity produced","FALSE")
coefcubsplindef=NA
R2cubsplindef="NA"
R2adjcubsplindef="NA"
RSEcubsplindef="NA"
Lcubsplindef="NA"
AICcubsplindef="NA"
AICCcubsplindef="NA"
BICcubsplindef="NA"
mcubsplindef="NA"
RSScubsplindef="NA"
Fcubsplindef="NA"
RSDcubsplindef="NA"
DWcubsplindef="NA"
paramcubsplindef="NA"
summarycubsplindef="NA"}

remove(cubsplindef,TSScubsplindef,PSScubsplindef,RScubsplindef,RS1cubsplindef)



#########################################
############## !WARNINGS! ###############

warning=rbind(
	wMM,wMMR,wMME,wbrody23,wbrody24,wSCH,
	wSCHL,wPBE,wwood,wDHA,wCB,wQP,wCLD,
	wPapBo1,wPapBo2,wPapBo3,wPapBo4,wPapBo6,
	wGS1,wGS2,wLQ,wwil,wwilk,wwilycsml,wBC,
	wDJK,wMG2,wMG4,wMG,wKHN,wAS,wFRP,wPTmult,
	wPTmod,wMonoG,wMonoGpw,wDiG,wDiGpw,
	wlegpol3,wlegpol4,wlegpolWil,wcubsplin3,
	wcubsplin4,wcubsplin5,wcubsplindef
	)

rownames(warning)=c(
	"(1) Michaelis-Menten",
	"(1a) Michaelis-Menten (Rook)",
	"(1b) Michaelis-Menten + Exponential (Rook)",
	"(2) Brody 1923",
	"(3) Brody 1924",
	"(4) Schumacher",
	"(4a) Schumacher (Lopez)",
	"(5) Parabolic Exponential (Sikka, Adediran)",
	"(6) Wood",
	"(6a) Wood (Dhanoa)",
	"(6b) Wood (Cappio-Borlino)",
	"(7) Quadratic Polynomial (Dave)",
	"(8) Cobby & Le Du",
	"(9) Papajcsik and Bodero 1",
	"(10) Papajcsik and Bodero 2",
	"(11) Papajcsik and Bodero 3",
	"(12) Papajcsik and Bodero 4",
	"(13) Papajcsik and Bodero 6",
	"(14) Mixed log model 1 (Guo & Swalve)",
	"(15) Mixed log model 3 (Guo & Swalve)",
	"(16) Log-quadratic (Adediran)",
	"(17) Wilmink",
	"(17a) Wilmink (Kheidirabadi)",
	"(17b) Wilmink (Laurenson & Strucken)",
	"(18) Bicompartemental (Ferguson & Boston)",
	"(19) Dijkstra",
	"(20) Morant & Gnanasakthy (Pollott)",
	"(21) Morant & Gnanasakthy (Vargas)",
	"(22) Morant & Gnanasakthy (Adediran)",
	"(23) Khandekar (Guo & Swalve)",
	"(24) Ali & Schaeffer",
	"(25) Fractional Polynomial (Elvira)",
	"(26) Pollott multiplicative reduced (Elvira)",
	"(27) Pollott modified (Adediran)",
	"(28) Monophasic Grossman",
	"(29) Monophasic Grossman power",
	"(30) Diphasic Grossman",
	"(31) Diphasic Grossman power",
	"(32) Legendre Polynomial 3th (Jacobson)",
	"(33) Legendre Polynomial 4th (Jacobson)",
	"(34) Legendre + Wilmink (Lindauer)",
	"(35) Natural Cubic Spline (3 percentiles)",
	"(36) Natural Cubic Spline (4 percentiles)",
	"(37) Natural Cubic Spline (5 percentiles)",
	"(38) Natural Cubic Spline (defined Harrell)"
	)

#########################################
######### Selection Criterion ###########

p=c(
	coefMM,coefMMR,coefMME,coefbrody23,
	coefbrody24,coefSCH,coefSCHL,coefPBE,
	coefwood,coefDHA,coefCB,coefQP,
	coefCLD,coefPapBo1,coefPapBo2,coefPapBo3,
	coefPapBo4,coefPapBo6,coefGS1,coefGS2,
	coefLQ,coefwil,coefwilk,coefwilycsml,
	coefBC,coefDJK,coefMG2,coefMG4,
	coefMG,coefKHN,coefAS,coefFRP,
	coefPTmult,coefPTmod,coefMonoG,coefMonoGpw,
	coefDiG,coefDiGpw,coeflegpol3,coeflegpol4,
	coeflegpolWil,coefcubsplin3,coefcubsplin4,
	coefcubsplin5,coefcubsplindef
	)

R2=c(
	R2MM,R2MMR,R2MME,R2brody23,
	R2brody24,R2SCH,R2SCHL,R2PBE,
	R2wood,R2DHA,R2CB,R2QP,
	R2CLD,R2PapBo1,R2PapBo2,R2PapBo3,
	R2PapBo4,R2PapBo6,R2GS1,R2GS2,
	R2LQ,R2wil,R2wilk,R2wilycsml,
	R2BC,R2DJK,R2MG2,R2MG4,
	R2MG,R2KHN,R2AS,R2FRP,
	R2PTmult,R2PTmod,R2MonoG,R2MonoGpw,
	R2DiG,R2DiGpw,R2legpol3,R2legpol4,
	R2legpolWil,R2cubsplin3,R2cubsplin4,
	R2cubsplin5,R2cubsplindef
	)

R2adj=c(
	R2adjMM,R2adjMMR,R2adjMME,R2adjbrody23,
	R2adjbrody24,R2adjSCH,R2adjSCHL,R2adjPBE,
	R2adjwood,R2adjDHA,R2adjCB,R2adjQP,
	R2adjCLD,R2adjPapBo1,R2adjPapBo2,R2adjPapBo3,
	R2adjPapBo4,R2adjPapBo6,R2adjGS1,R2adjGS2,
	R2adjLQ,R2adjwil,R2adjwilk,R2adjwilycsml,
	R2adjBC,R2adjDJK,R2adjMG2,R2adjMG4,
	R2adjMG,R2adjKHN,R2adjAS,R2adjFRP,
	R2adjPTmult,R2adjPTmod,R2adjMonoG,R2adjMonoGpw,
	R2adjDiG,R2adjDiGpw,R2adjlegpol3,R2adjlegpol4,
	R2adjlegpolWil,R2adjcubsplin3,R2adjcubsplin4,
	R2adjcubsplin5,R2adjcubsplindef
	)

RSE=c(
	if(warning[1,2]=="TRUE"){RSEMM} else {"NA"},
	if(warning[2,2]=="TRUE"){RSEMMR} else {"NA"},
	if(warning[3,2]=="TRUE"){RSEMME} else {"NA"},
	if(warning[4,2]=="TRUE"){RSEbrody23} else {"NA"},
	if(warning[5,2]=="TRUE"){RSEbrody24} else {"NA"},
	if(warning[6,2]=="TRUE"){RSESCH} else {"NA"},
	if(warning[7,2]=="TRUE"){RSESCHL} else {"NA"},
	if(warning[8,2]=="TRUE"){RSEPBE} else {"NA"},
	if(warning[9,2]=="TRUE"){RSEwood} else {"NA"},
	if(warning[10,2]=="TRUE"){RSEDHA} else {"NA"},
	if(warning[11,2]=="TRUE"){RSECB} else {"NA"},
	if(warning[12,2]=="TRUE"){RSEQP} else {"NA"},
	if(warning[13,2]=="TRUE"){RSECLD} else {"NA"},
	if(warning[14,2]=="TRUE"){RSEPapBo1} else {"NA"},
	if(warning[15,2]=="TRUE"){RSEPapBo2} else {"NA"},
	if(warning[16,2]=="TRUE"){RSEPapBo3} else {"NA"},
	if(warning[17,2]=="TRUE"){RSEPapBo4} else {"NA"},
	if(warning[18,2]=="TRUE"){RSEPapBo6} else {"NA"},
	if(warning[19,2]=="TRUE"){RSEGS1} else {"NA"},
	if(warning[20,2]=="TRUE"){RSEGS2} else {"NA"},
	if(warning[21,2]=="TRUE"){RSELQ} else {"NA"},
	if(warning[22,2]=="TRUE"){RSEwil} else {"NA"},
	if(warning[23,2]=="TRUE"){RSEwilk} else {"NA"},
	if(warning[24,2]=="TRUE"){RSEwilycsml} else {"NA"},
	if(warning[25,2]=="TRUE"){RSEBC} else {"NA"},
	if(warning[26,2]=="TRUE"){RSEDJK} else {"NA"},
	if(warning[27,2]=="TRUE"){RSEMG2} else {"NA"},
	if(warning[28,2]=="TRUE"){RSEMG4} else {"NA"},
	if(warning[29,2]=="TRUE"){RSEMG} else {"NA"},
	if(warning[30,2]=="TRUE"){RSEKHN} else {"NA"},
	if(warning[31,2]=="TRUE"){RSEAS} else {"NA"},
	if(warning[32,2]=="TRUE"){RSEFRP} else {"NA"},
	if(warning[33,2]=="TRUE"){RSEPTmult} else {"NA"},
	if(warning[34,2]=="TRUE"){RSEPTmod} else {"NA"},
	if(warning[35,2]=="TRUE"){RSEMonoG} else {"NA"},
	if(warning[36,2]=="TRUE"){RSEMonoGpw} else {"NA"},
	if(warning[37,2]=="TRUE"){RSEDiG} else {"NA"},
	if(warning[38,2]=="TRUE"){RSEDiGpw} else {"NA"},
	if(warning[39,2]=="TRUE"){RSElegpol3} else {"NA"},
	if(warning[40,2]=="TRUE"){RSElegpol4} else {"NA"},
	if(warning[41,2]=="TRUE"){RSElegpolWil} else {"NA"},
	if(warning[42,2]=="TRUE"){RSEcubsplin3} else {"NA"},
	if(warning[43,2]=="TRUE"){RSEcubsplin4} else {"NA"},
	if(warning[44,2]=="TRUE"){RSEcubsplin5} else {"NA"},
	if(warning[45,2]=="TRUE"){RSEcubsplindef} else {"NA"}
	)

logL=c(
	LMM,LMMR,LMME,Lbrody23,
	Lbrody24,LSCH,LSCHL,LPBE,
	Lwood,LDHA,LCB,LQP,
	LCLD,LPapBo1,LPapBo2,LPapBo3,
	LPapBo4,LPapBo6,LGS1,LGS2,
	LLQ,Lwil,Lwilk,Lwilycsml,
	LBC,LDJK,LMG2,LMG4,
	LMG,LKHN,LAS,LFRP,
	LPTmult,LPTmod,LMonoG,LMonoGpw,
	LDiG,LDiGpw,Llegpol3,Llegpol4,
	LlegpolWil,Lcubsplin3,Lcubsplin4,
	Lcubsplin5,Lcubsplindef
	)

AIC=c(
	AICMM,AICMMR,AICMME,AICbrody23,
	AICbrody24,AICSCH,AICSCHL,AICPBE,
	AICwood,AICDHA,AICCB,AICQP,
	AICCLD,AICPapBo1,AICPapBo2,AICPapBo3,
	AICPapBo4,AICPapBo6,AICGS1,AICGS2,
	AICLQ,AICwil,AICwilk,AICwilycsml,
	AICBC,AICDJK,AICMG2,AICMG4,
	AICMG,AICKHN,AICAS,AICFRP,
	AICPTmult,AICPTmod,AICMonoG,AICMonoGpw,
	AICDiG,AICDiGpw,AIClegpol3,AIClegpol4,
	AIClegpolWil,AICcubsplin3,AICcubsplin4,
	AICcubsplin5,AICcubsplindef
	)

AICC=c(
	AICCMM,AICCMMR,AICCMME,AICCbrody23,
	AICCbrody24,AICCSCH,AICCSCHL,AICCPBE,
	AICCwood,AICCDHA,AICCCB,AICCQP,
	AICCCLD,AICCPapBo1,AICCPapBo2,AICCPapBo3,
	AICCPapBo4,AICCPapBo6,AICCGS1,AICCGS2,
	AICCLQ,AICCwil,AICCwilk,AICCwilycsml,
	AICCBC,AICCDJK,AICCMG2,AICCMG4,
	AICCMG,AICCKHN,AICCAS,AICCFRP,
	AICCPTmult,AICCPTmod,AICCMonoG,AICCMonoGpw,
	AICCDiG,AICCDiGpw,AICClegpol3,AICClegpol4,
	AICClegpolWil,AICCcubsplin3,AICCcubsplin4,
	AICCcubsplin5,AICCcubsplindef
	)

BIC=c(
	BICMM,BICMMR,BICMME,BICbrody23,
	BICbrody24,BICSCH,BICSCHL,BICPBE,
	BICwood,BICDHA,BICCB,BICQP,
	BICCLD,BICPapBo1,BICPapBo2,BICPapBo3,
	BICPapBo4,BICPapBo6,BICGS1,BICGS2,
	BICLQ,BICwil,BICwilk,BICwilycsml,
	BICBC,BICDJK,BICMG2,BICMG4,
	BICMG,BICKHN,BICAS,BICFRP,
	BICPTmult,BICPTmod,BICMonoG,BICMonoGpw,
	BICDiG,BICDiGpw,BIClegpol3,BIClegpol4,
	BIClegpolWil,BICcubsplin3,BICcubsplin4,
	BICcubsplin5,BICcubsplindef
	)

DW=c(
	DWMM,DWMMR,DWMME,DWbrody23,
	DWbrody24,DWSCH,DWSCHL,DWPBE,
	DWwood,DWDHA,DWCB,DWQP,
	DWCLD,DWPapBo1,DWPapBo2,DWPapBo3,
	DWPapBo4,DWPapBo6,DWGS1,DWGS2,
	DWLQ,DWwil,DWwilk,DWwilycsml,
	DWBC,DWDJK,DWMG2,DWMG4,
	DWMG,DWKHN,DWAS,DWFRP,
	DWPTmult,DWPTmod,DWMonoG,DWMonoGpw,
	DWDiG,DWDiGpw,DWlegpol3,DWlegpol4,
	DWlegpolWil,DWcubsplin3,DWcubsplin4,
	DWcubsplin5,DWcubsplindef
	)

selcri=cbind(R2,R2adj,RSE,logL,AIC,AICC,BIC,DW)
rownames(selcri)=c(
	"(1) Michaelis-Menten",
	"(1a) Michaelis-Menten (Rook)",
	"(1b) Michaelis-Menten + Exponential (Rook)",
	"(2) Brody 1923",
	"(3) Brody 1924",
	"(4) Schumacher  ",
	"(4a) Schumacher (Lopez)",
	"(5) Parabolic Exponential (Sikka, Adediran)",
	"(6) Wood",
	"(6a) Wood (Dhanoa)",
	"(6b) Wood (Cappio-Borlino)",
	"(7) Quadratic Polynomial (Dave)",
	"(8) Cobby & Le Du",
	"(9) Papajcsik and Bodero 1",
	"(10) Papajcsik and Bodero 2",
	"(11) Papajcsik and Bodero 3",
	"(12) Papajcsik and Bodero 4",
	"(13) Papajcsik and Bodero 6",
	"(14) Mixed log model 1 (Guo & Swalve)",
	"(15) Mixed log model 3 (Guo & Swalve)",
	"(16) Log-quadratic (Adediran)",
	"(17) Wilmink",
	"(17a) Wilmink (Kheidirabadi)",
	"(17b) Wilmink (Laurenson & Strucken)",
	"(18) Bicompartemental (Ferguson & Boston)",
	"(19) Dijkstra",
	"(20) Morant & Gnanasakthy (Pollott)",
	"(21) Morant & Gnanasakthy (Vargas)",
	"(22) Morant & Gnanasakthy (Adediran)",
	"(23) Khandekar (Guo & Swalve)",
	"(24) Ali & Schaeffer",
	"(25) Fractional Polynomial (Elvira)",
	"(26) Pollott multiplicative reduced (Elvira)",
	"(27) Pollott modified (Adediran)",
	"(28) Monophasic Grossman",
	"(29) Monophasic Grossman power",
	"(30) Diphasic Grossman",
	"(31) Diphasic Grossman power",
	"(32) Legendre Polynomial 3th (Jacobson)",
	"(33) Legendre Polynomial 4th (Jacobson)",
	"(34) Legendre + Wilmink (Lindauer)",
	"(35) Natural Cubic Spline (3 percentiles)",
	"(36) Natural Cubic Spline (4 percentiles)",
	"(37) Natural Cubic Spline (5 percentiles)",
	"(38) Natural Cubic Spline (defined Harrell)"
	)

selcri2=selcri
selcri2[1,]=if(warning[1,2]=="TRUE"){selcri2[1,]} else {"Error"}
selcri2[2,]=if(warning[2,2]=="TRUE"){selcri2[2,]} else {"Error"}
selcri2[3,]=if(warning[3,2]=="TRUE"){selcri2[3,]} else {"Error"}
selcri2[4,]=if(warning[4,2]=="TRUE"){selcri2[4,]} else {"Error"}
selcri2[5,]=if(warning[5,2]=="TRUE"){selcri2[5,]} else {"Error"}
selcri2[6,]=if(warning[6,2]=="TRUE"){selcri2[6,]} else {"Error"}
selcri2[7,]=if(warning[7,2]=="TRUE"){selcri2[7,]} else {"Error"}
selcri2[8,]=if(warning[8,2]=="TRUE"){selcri2[8,]} else {"Error"}
selcri2[9,]=if(warning[9,2]=="TRUE"){selcri2[9,]} else {"Error"}
selcri2[10,]=if(warning[10,2]=="TRUE"){selcri2[10,]} else {"Error"}
selcri2[11,]=if(warning[11,2]=="TRUE"){selcri2[11,]} else {"Error"}
selcri2[12,]=if(warning[12,2]=="TRUE"){selcri2[12,]} else {"Error"}
selcri2[13,]=if(warning[13,2]=="TRUE"){selcri2[13,]} else {"Error"}
selcri2[14,]=if(warning[14,2]=="TRUE"){selcri2[14,]} else {"Error"}
selcri2[15,]=if(warning[15,2]=="TRUE"){selcri2[15,]} else {"Error"}
selcri2[16,]=if(warning[16,2]=="TRUE"){selcri2[16,]} else {"Error"}
selcri2[17,]=if(warning[17,2]=="TRUE"){selcri2[17,]} else {"Error"}
selcri2[18,]=if(warning[18,2]=="TRUE"){selcri2[18,]} else {"Error"}
selcri2[19,]=if(warning[19,2]=="TRUE"){selcri2[19,]} else {"Error"}
selcri2[20,]=if(warning[20,2]=="TRUE"){selcri2[20,]} else {"Error"}
selcri2[21,]=if(warning[21,2]=="TRUE"){selcri2[21,]} else {"Error"}
selcri2[22,]=if(warning[22,2]=="TRUE"){selcri2[22,]} else {"Error"}
selcri2[23,]=if(warning[23,2]=="TRUE"){selcri2[23,]} else {"Error"}
selcri2[24,]=if(warning[24,2]=="TRUE"){selcri2[24,]} else {"Error"}
selcri2[25,]=if(warning[25,2]=="TRUE"){selcri2[25,]} else {"Error"}
selcri2[26,]=if(warning[26,2]=="TRUE"){selcri2[26,]} else {"Error"}
selcri2[27,]=if(warning[27,2]=="TRUE"){selcri2[27,]} else {"Error"}
selcri2[28,]=if(warning[28,2]=="TRUE"){selcri2[28,]} else {"Error"}
selcri2[29,]=if(warning[29,2]=="TRUE"){selcri2[29,]} else {"Error"}
selcri2[30,]=if(warning[30,2]=="TRUE"){selcri2[30,]} else {"Error"}
selcri2[31,]=if(warning[31,2]=="TRUE"){selcri2[31,]} else {"Error"}
selcri2[32,]=if(warning[32,2]=="TRUE"){selcri2[32,]} else {"Error"}
selcri2[33,]=if(warning[33,2]=="TRUE"){selcri2[33,]} else {"Error"}
selcri2[34,]=if(warning[34,2]=="TRUE"){selcri2[34,]} else {"Error"}
selcri2[35,]=if(warning[35,2]=="TRUE"){selcri2[35,]} else {"Error"}
selcri2[36,]=if(warning[36,2]=="TRUE"){selcri2[36,]} else {"Error"}
selcri2[37,]=if(warning[37,2]=="TRUE"){selcri2[37,]} else {"Error"}
selcri2[38,]=if(warning[38,2]=="TRUE"){selcri2[38,]} else {"Error"}
selcri2[39,]=if(warning[39,2]=="TRUE"){selcri2[39,]} else {"Error"}
selcri2[40,]=if(warning[40,2]=="TRUE"){selcri2[40,]} else {"Error"}
selcri2[41,]=if(warning[41,2]=="TRUE"){selcri2[41,]} else {"Error"}
selcri2[42,]=if(warning[42,2]=="TRUE"){selcri2[42,]} else {"Error"}
selcri2[43,]=if(warning[43,2]=="TRUE"){selcri2[43,]} else {"Error"}
selcri2[44,]=if(warning[44,2]=="TRUE"){selcri2[44,]} else {"Error"}
selcri2[45,]=if(warning[45,2]=="TRUE"){selcri2[45,]} else {"Error"}

### only keep best value per selection criterion ###

new=matrix("",45,8)
#options(warn=-1)
new[,1][selcri2[,1]==max(as.numeric(selcri2[,1]),na.rm=TRUE)]=max(as.numeric(selcri2[,1]),na.rm=TRUE) #R2
new[,2][selcri2[,2]==max(as.numeric(selcri2[,2]),na.rm=TRUE)]=max(as.numeric(selcri2[,2]),na.rm=TRUE) #R2adj
new[,3][selcri2[,3]==min(as.numeric(selcri2[,3]),na.rm=TRUE)]=min(as.numeric(selcri2[,3]),na.rm=TRUE) #RSE
new[,4][selcri2[,4]==max(as.numeric(selcri2[,4]),na.rm=TRUE)]=max(as.numeric(selcri2[,4]),na.rm=TRUE) #logL
new[,5][selcri2[,5]==min(as.numeric(selcri2[,5]),na.rm=TRUE)]=min(as.numeric(selcri2[,5]),na.rm=TRUE) #AIC
new[,6][selcri2[,6]==min(as.numeric(selcri2[,6]),na.rm=TRUE)]=min(as.numeric(selcri2[,6]),na.rm=TRUE) #AICC
new[,7][selcri2[,7]==min(as.numeric(selcri2[,7]),na.rm=TRUE)]=min(as.numeric(selcri2[,7]),na.rm=TRUE) #BIC
new[,8][selcri2[,8]==selcri2[which.min(abs(as.numeric(selcri2[,8])-2)),8]]=selcri2[which.min(abs(as.numeric(selcri2[,8])-2)),8] #DW

colnames(new)=colnames(selcri)[1:8]
rownames(new)=rownames(selcri)

new2=new
new2[1,]=if(warning[1,2]=="TRUE"){new2[1,]} else {"Error"}
new2[2,]=if(warning[2,2]=="TRUE"){new2[2,]} else {"Error"}
new2[3,]=if(warning[3,2]=="TRUE"){new2[3,]} else {"Error"}
new2[4,]=if(warning[4,2]=="TRUE"){new2[4,]} else {"Error"}
new2[5,]=if(warning[5,2]=="TRUE"){new2[5,]} else {"Error"}
new2[6,]=if(warning[6,2]=="TRUE"){new2[6,]} else {"Error"}
new2[7,]=if(warning[7,2]=="TRUE"){new2[7,]} else {"Error"}
new2[8,]=if(warning[8,2]=="TRUE"){new2[8,]} else {"Error"}
new2[9,]=if(warning[9,2]=="TRUE"){new2[9,]} else {"Error"}
new2[10,]=if(warning[10,2]=="TRUE"){new2[10,]} else {"Error"}
new2[11,]=if(warning[11,2]=="TRUE"){new2[11,]} else {"Error"}
new2[12,]=if(warning[12,2]=="TRUE"){new2[12,]} else {"Error"}
new2[13,]=if(warning[13,2]=="TRUE"){new2[13,]} else {"Error"}
new2[14,]=if(warning[14,2]=="TRUE"){new2[14,]} else {"Error"}
new2[15,]=if(warning[15,2]=="TRUE"){new2[15,]} else {"Error"}
new2[16,]=if(warning[16,2]=="TRUE"){new2[16,]} else {"Error"}
new2[17,]=if(warning[17,2]=="TRUE"){new2[17,]} else {"Error"}
new2[18,]=if(warning[18,2]=="TRUE"){new2[18,]} else {"Error"}
new2[19,]=if(warning[19,2]=="TRUE"){new2[19,]} else {"Error"}
new2[20,]=if(warning[20,2]=="TRUE"){new2[20,]} else {"Error"}
new2[21,]=if(warning[21,2]=="TRUE"){new2[21,]} else {"Error"}
new2[22,]=if(warning[22,2]=="TRUE"){new2[22,]} else {"Error"}
new2[23,]=if(warning[23,2]=="TRUE"){new2[23,]} else {"Error"}
new2[24,]=if(warning[24,2]=="TRUE"){new2[24,]} else {"Error"}
new2[25,]=if(warning[25,2]=="TRUE"){new2[25,]} else {"Error"}
new2[26,]=if(warning[26,2]=="TRUE"){new2[26,]} else {"Error"}
new2[27,]=if(warning[27,2]=="TRUE"){new2[27,]} else {"Error"}
new2[28,]=if(warning[28,2]=="TRUE"){new2[28,]} else {"Error"}
new2[29,]=if(warning[29,2]=="TRUE"){new2[29,]} else {"Error"}
new2[30,]=if(warning[30,2]=="TRUE"){new2[30,]} else {"Error"}
new2[31,]=if(warning[31,2]=="TRUE"){new2[31,]} else {"Error"}
new2[32,]=if(warning[32,2]=="TRUE"){new2[32,]} else {"Error"}
new2[33,]=if(warning[33,2]=="TRUE"){new2[33,]} else {"Error"}
new2[34,]=if(warning[34,2]=="TRUE"){new2[34,]} else {"Error"}
new2[35,]=if(warning[35,2]=="TRUE"){new2[35,]} else {"Error"}
new2[36,]=if(warning[36,2]=="TRUE"){new2[36,]} else {"Error"}
new2[37,]=if(warning[37,2]=="TRUE"){new2[37,]} else {"Error"}
new2[38,]=if(warning[38,2]=="TRUE"){new2[38,]} else {"Error"}
new2[39,]=if(warning[39,2]=="TRUE"){new2[39,]} else {"Error"}
new2[40,]=if(warning[40,2]=="TRUE"){new2[40,]} else {"Error"}
new2[41,]=if(warning[41,2]=="TRUE"){new2[41,]} else {"Error"}
new2[42,]=if(warning[42,2]=="TRUE"){new2[42,]} else {"Error"}
new2[43,]=if(warning[43,2]=="TRUE"){new2[43,]} else {"Error"}
new2[44,]=if(warning[44,2]=="TRUE"){new2[44,]} else {"Error"}
new2[45,]=if(warning[45,2]=="TRUE"){new2[45,]} else {"Error"}

## best selection criterion ##
R2=c(rownames(selcri2)[selcri2[,1]==max(as.numeric(selcri2[,1]),na.rm=TRUE)],max(as.numeric(selcri2[,1]),na.rm=TRUE))
R2adj=c(rownames(selcri2)[selcri2[,2]==max(as.numeric(selcri2[,2]),na.rm=TRUE)],max(as.numeric(selcri2[,2]),na.rm=TRUE))
RSE=c(rownames(selcri2)[selcri2[,3]==min(as.numeric(selcri2[,3]),na.rm=TRUE)],min(as.numeric(selcri2[,3]),na.rm=TRUE))
logL=c(rownames(selcri2)[selcri2[,4]==max(as.numeric(selcri2[,4]),na.rm=TRUE)],max(as.numeric(selcri2[,4]),na.rm=TRUE))
AIC=c(rownames(selcri2)[selcri2[,5]==min(as.numeric(selcri2[,5]),na.rm=TRUE)],min(as.numeric(selcri2[,5]),na.rm=TRUE))
AICC=c(rownames(selcri2)[selcri2[,6]==min(as.numeric(selcri2[,6]),na.rm=TRUE)],min(as.numeric(selcri2[,6]),na.rm=TRUE))
BIC=c(rownames(selcri2)[selcri2[,7]==min(as.numeric(selcri2[,7]),na.rm=TRUE)],min(as.numeric(selcri2[,7]),na.rm=TRUE))
DW=c(rownames(selcri2)[selcri2[,8]==selcri2[which.min(abs(as.numeric(selcri2[,8])-2)),8]],selcri2[which.min(abs(as.numeric(selcri2[,8])-2)),8])


## best model according to best selection criterion ##
critbest=as.data.frame(cbind(R2,R2adj,RSE,logL,AIC,AICC,BIC,DW))

R2model=c(
	R2=(if(critbest[1,1]=="(1) Michaelis-Menten")mMM),
	R2=(if(critbest[1,1]=="(1a) Michaelis-Menten (Rook)")mMMR),
	R2=(if(critbest[1,1]=="(1b) Michaelis-Menten + Exponential (Rook)")mMME),
	R2=(if(critbest[1,1]=="(2) Brody 1923")mbrody23),
	R2=(if(critbest[1,1]=="(3) Brody 1924")mbrody24),
	R2=(if(critbest[1,1]=="(4) Schumacher")mSCH),
	R2=(if(critbest[1,1]=="(4a) Schumacher (Lopez)")mSCHL),
	R2=(if(critbest[1,1]=="(5) Parabolic Exponential (Sikka, Adediran)")mPBE),
	R2=(if(critbest[1,1]=="(6) Wood")mwood),
	R2=(if(critbest[1,1]=="(6a) Wood (Dhanoa)")mDHA),
	R2=(if(critbest[1,1]=="(6b) Wood (Cappio-Borlino)")mCB),
	R2=(if(critbest[1,1]=="(7) Quadratic Polynomial (Dave)")mQP),
	R2=(if(critbest[1,1]=="(8) Cobby & Le Du")mCLD),
	R2=(if(critbest[1,1]=="(9) Papajcsik and Bodero 1")mPapBo1),
	R2=(if(critbest[1,1]=="(10) Papajcsik and Bodero 2")mPapBo2),
	R2=(if(critbest[1,1]=="(11) Papajcsik and Bodero 3")mPapBo3),
	R2=(if(critbest[1,1]=="(12) Papajcsik and Bodero 4")mPapBo4),
	R2=(if(critbest[1,1]=="(13) Papajcsik and Bodero 6")mPapBo6),
	R2=(if(critbest[1,1]=="(14) Mixed log model 1 (Guo & Swalve)")mGS1),
	R2=(if(critbest[1,1]=="(15) Mixed log model 3 (Guo & Swalve)")mGS2),
	R2=(if(critbest[1,1]=="(16) Log-quadratic (Adediran)")mLQ),
	R2=(if(critbest[1,1]=="(17) Wilmink")mwil),
	R2=(if(critbest[1,1]=="(17a) Wilmink (Kheidirabadi)")mwilk),
	R2=(if(critbest[1,1]=="(17b) Wilmink (Laurenson & Strucken)")mwilycsml),
	R2=(if(critbest[1,1]=="(18) Bicompartemental (Ferguson & Boston)")mBC),
	R2=(if(critbest[1,1]=="(19) Dijkstra")mDJK),
	R2=(if(critbest[1,1]=="(20) Morant & Gnanasakthy (Pollott)")mMG2),
	R2=(if(critbest[1,1]=="(21) Morant & Gnanasakthy (Vargas)")mMG4),
	R2=(if(critbest[1,1]=="(22) Morant & Gnanasakthy (Adediran)")mMG),
	R2=(if(critbest[1,1]=="(23) Khandekar (Guo & Swalve)")mKHN),
	R2=(if(critbest[1,1]=="(24) Ali & Schaeffer")mAS),
	R2=(if(critbest[1,1]=="(25) Fractional Polynomial (Elvira)")mFRP),
	R2=(if(critbest[1,1]=="(26) Pollott multiplicative reduced (Elvira)")mPTmult),
	R2=(if(critbest[1,1]=="(27) Pollott modified (Adediran)")mPTmod),
	R2=(if(critbest[1,1]=="(28) Monophasic Grossman")mMonoG),
	R2=(if(critbest[1,1]=="(29) Monophasic Grossman power")mMonoGpw),
	R2=(if(critbest[1,1]=="(30) Diphasic Grossman")mDiG),
	R2=(if(critbest[1,1]=="(31) Diphasic Grossman power")mDiGpw),
	R2=(if(critbest[1,1]=="(32) Legendre Polynomial 3th (Jacobson)")mlegpol3),
	R2=(if(critbest[1,1]=="(33) Legendre Polynomial 4th (Jacobson)")mlegpol4),
	R2=(if(critbest[1,1]=="(34) Legendre + Wilmink (Lindauer)")mlegpolWil),
	R2=(if(critbest[1,1]=="(35) Natural Cubic Spline (3 percentiles)")mcubsplin3),
	R2=(if(critbest[1,1]=="(36) Natural Cubic Spline (4 percentiles)")mcubsplin4),
	R2=(if(critbest[1,1]=="(37) Natural Cubic Spline (5 percentiles)")mcubsplin5),
	R2=(if(critbest[1,1]=="(38) Natural Cubic Spline (defined Harrell)")mcubsplindef)
	)

R2adjmodel=c(
	R2adj=(if(critbest[1,2]=="(1) Michaelis-Menten")mMM),
	R2adj=(if(critbest[1,2]=="(1a) Michaelis-Menten (Rook)")mMMR),
	R2adj=(if(critbest[1,2]=="(1b) Michaelis-Menten + Exponential (Rook)")mMME),
	R2adj=(if(critbest[1,2]=="(2) Brody 1923")mbrody23),
	R2adj=(if(critbest[1,2]=="(3) Brody 1924")mbrody24),
	R2adj=(if(critbest[1,2]=="(4) Schumacher")mSCH),
	R2adj=(if(critbest[1,2]=="(4a) Schumacher (Lopez)")mSCHL),
	R2adj=(if(critbest[1,2]=="(5) Parabolic Exponential (Sikka, Adediran)")mPBE),
	R2adj=(if(critbest[1,2]=="(6) Wood")mwood),
	R2adj=(if(critbest[1,2]=="(6a) Wood (Dhanoa)")mDHA),
	R2adj=(if(critbest[1,2]=="(6b) Wood (Cappio-Borlino)")mCB),
	R2adj=(if(critbest[1,2]=="(7) Quadratic Polynomial (Dave)")mQP),
	R2adj=(if(critbest[1,2]=="(8) Cobby & Le Du")mCLD),
	R2adj=(if(critbest[1,2]=="(9) Papajcsik and Bodero 1")mPapBo1),
	R2adj=(if(critbest[1,2]=="(10) Papajcsik and Bodero 2")mPapBo2),
	R2adj=(if(critbest[1,2]=="(11) Papajcsik and Bodero 3")mPapBo3),
	R2adj=(if(critbest[1,2]=="(12) Papajcsik and Bodero 4")mPapBo4),
	R2adj=(if(critbest[1,2]=="(13) Papajcsik and Bodero 6")mPapBo6),
	R2adj=(if(critbest[1,2]=="(14) Mixed log model 1 (Guo & Swalve)")mGS1),
	R2adj=(if(critbest[1,2]=="(15) Mixed log model 3 (Guo & Swalve)")mGS2),
	R2adj=(if(critbest[1,2]=="(16) Log-quadratic (Adediran)")mLQ),
	R2adj=(if(critbest[1,2]=="(17) Wilmink")mwil),
	R2adj=(if(critbest[1,2]=="(17a) Wilmink (Kheidirabadi)")mwilk),
	R2adj=(if(critbest[1,2]=="(17b) Wilmink (Laurenson & Strucken)")mwilycsml),
	R2adj=(if(critbest[1,2]=="(18) Bicompartemental (Ferguson & Boston)")mBC),
	R2adj=(if(critbest[1,2]=="(19) Dijkstra")mDJK),
	R2adj=(if(critbest[1,2]=="(20) Morant & Gnanasakthy (Pollott)")mMG2),
	R2adj=(if(critbest[1,2]=="(21) Morant & Gnanasakthy (Vargas)")mMG4),
	R2adj=(if(critbest[1,2]=="(22) Morant & Gnanasakthy (Adediran)")mMG),
	R2adj=(if(critbest[1,2]=="(23) Khandekar (Guo & Swalve)")mKHN),
	R2adj=(if(critbest[1,2]=="(24) Ali & Schaeffer")mAS),
	R2adj=(if(critbest[1,2]=="(25) Fractional Polynomial (Elvira)")mFRP),
	R2adj=(if(critbest[1,2]=="(26) Pollott multiplicative reduced (Elvira)")mPTmult),
	R2adj=(if(critbest[1,2]=="(27) Pollott modified (Adediran)")mPTmod),
	R2adj=(if(critbest[1,2]=="(28) Monophasic Grossman")mMonoG),
	R2adj=(if(critbest[1,2]=="(29) Monophasic Grossman power")mMonoGpw),
	R2adj=(if(critbest[1,2]=="(30) Diphasic Grossman")mDiG),
	R2adj=(if(critbest[1,2]=="(31) Diphasic Grossman power")mDiGpw),
	R2adj=(if(critbest[1,2]=="(32) Legendre Polynomial 3th (Jacobson)")mlegpol3),
	R2adj=(if(critbest[1,2]=="(33) Legendre Polynomial 4th (Jacobson)")mlegpol4),
	R2adj=(if(critbest[1,2]=="(34) Legendre + Wilmink (Lindauer)")mlegpolWil),
	R2adj=(if(critbest[1,2]=="(35) Natural Cubic Spline (3 percentiles)")mcubsplin3),
	R2adj=(if(critbest[1,2]=="(36) Natural Cubic Spline (4 percentiles)")mcubsplin4),
	R2adj=(if(critbest[1,2]=="(37) Natural Cubic Spline (5 percentiles)")mcubsplin5),
	R2adj=(if(critbest[1,2]=="(38) Natural Cubic Spline (defined Harrell)")mcubsplindef)
	)

RSEmodel=c(
	RSE=(if(critbest[1,3]=="(1) Michaelis-Menten")mMM),
	RSE=(if(critbest[1,3]=="(1a) Michaelis-Menten (Rook)")mMMR),
	RSE=(if(critbest[1,3]=="(1b) Michaelis-Menten + Exponential (Rook)")mMME),
	RSE=(if(critbest[1,3]=="(2) Brody 1923")mbrody23),
	RSE=(if(critbest[1,3]=="(3) Brody 1924")mbrody24),
	RSE=(if(critbest[1,3]=="(4) Schumacher")mSCH),
	RSE=(if(critbest[1,3]=="(4a) Schumacher (Lopez)")mSCHL),
	RSE=(if(critbest[1,3]=="(5) Parabolic Exponential (Sikka, Adediran)")mPBE),
	RSE=(if(critbest[1,3]=="(6) Wood")mwood),
	RSE=(if(critbest[1,3]=="(6a) Wood (Dhanoa)")mDHA),
	RSE=(if(critbest[1,3]=="(6b) Wood (Cappio-Borlino)")mCB),
	RSE=(if(critbest[1,3]=="(7) Quadratic Polynomial (Dave)")mQP),
	RSE=(if(critbest[1,3]=="(8) Cobby & Le Du")mCLD),
	RSE=(if(critbest[1,3]=="(9) Papajcsik and Bodero 1")mPapBo1),
	RSE=(if(critbest[1,3]=="(10) Papajcsik and Bodero 2")mPapBo2),
	RSE=(if(critbest[1,3]=="(11) Papajcsik and Bodero 3")mPapBo3),
	RSE=(if(critbest[1,3]=="(12) Papajcsik and Bodero 4")mPapBo4),
	RSE=(if(critbest[1,3]=="(13) Papajcsik and Bodero 6")mPapBo6),
	RSE=(if(critbest[1,3]=="(14) Mixed log model 1 (Guo & Swalve)")mGS1),
	RSE=(if(critbest[1,3]=="(15) Mixed log model 3 (Guo & Swalve)")mGS2),
	RSE=(if(critbest[1,3]=="(16) Log-quadratic (Adediran)")mLQ),
	RSE=(if(critbest[1,3]=="(17) Wilmink")mwil),
	RSE=(if(critbest[1,3]=="(17a) Wilmink (Kheidirabadi)")mwilk),
	RSE=(if(critbest[1,3]=="(17b) Wilmink (Laurenson & Strucken)")mwilycsml),
	RSE=(if(critbest[1,3]=="(18) Bicompartemental (Ferguson & Boston)")mBC),
	RSE=(if(critbest[1,3]=="(19) Dijkstra")mDJK),
	RSE=(if(critbest[1,3]=="(20) Morant & Gnanasakthy (Pollott)")mMG2),
	RSE=(if(critbest[1,3]=="(21) Morant & Gnanasakthy (Vargas)")mMG4),
	RSE=(if(critbest[1,3]=="(22) Morant & Gnanasakthy (Adediran)")mMG),
	RSE=(if(critbest[1,3]=="(23) Khandekar (Guo & Swalve)")mKHN),
	RSE=(if(critbest[1,3]=="(24) Ali & Schaeffer")mAS),
	RSE=(if(critbest[1,3]=="(25) Fractional Polynomial (Elvira)")mFRP),
	RSE=(if(critbest[1,3]=="(26) Pollott multiplicative reduced (Elvira)")mPTmult),
	RSE=(if(critbest[1,3]=="(27) Pollott modified (Adediran)")mPTmod),
	RSE=(if(critbest[1,3]=="(28) Monophasic Grossman")mMonoG),
	RSE=(if(critbest[1,3]=="(29) Monophasic Grossman power")mMonoGpw),
	RSE=(if(critbest[1,3]=="(30) Diphasic Grossman")mDiG),
	RSE=(if(critbest[1,3]=="(31) Diphasic Grossman power")mDiGpw),
	RSE=(if(critbest[1,3]=="(32) Legendre Polynomial 3th (Jacobson)")mlegpol3),
	RSE=(if(critbest[1,3]=="(33) Legendre Polynomial 4th (Jacobson)")mlegpol4),
	RSE=(if(critbest[1,3]=="(34) Legendre + Wilmink (Lindauer)")mlegpolWil),
	RSE=(if(critbest[1,3]=="(35) Natural Cubic Spline (3 percentiles)")mcubsplin3),
	RSE=(if(critbest[1,3]=="(36) Natural Cubic Spline (4 percentiles)")mcubsplin4),
	RSE=(if(critbest[1,3]=="(37) Natural Cubic Spline (5 percentiles)")mcubsplin5),
	RSE=(if(critbest[1,3]=="(38) Natural Cubic Spline (defined Harrell)")mcubsplindef)
	)

logLmodel=c(
	logL=(if(critbest[1,4]=="(1) Michaelis-Menten")mMM),
	logL=(if(critbest[1,4]=="(1a) Michaelis-Menten (Rook)")mMMR),
	logL=(if(critbest[1,4]=="(1b) Michaelis-Menten + Exponential (Rook)")mMME),
	logL=(if(critbest[1,4]=="(2) Brody 1923")mbrody23),
	logL=(if(critbest[1,4]=="(3) Brody 1924")mbrody24),
	logL=(if(critbest[1,4]=="(4) Schumacher")mSCH),
	logL=(if(critbest[1,4]=="(4a) Schumacher (Lopez)")mSCHL),
	logL=(if(critbest[1,4]=="(5) Parabolic Exponential (Sikka, Adediran)")mPBE),
	logL=(if(critbest[1,4]=="(6) Wood")mwood),
	logL=(if(critbest[1,4]=="(6a) Wood (Dhanoa)")mDHA),
	logL=(if(critbest[1,4]=="(6b) Wood (Cappio-Borlino)")mCB),
	logL=(if(critbest[1,4]=="(7) Quadratic Polynomial (Dave)")mQP),
	logL=(if(critbest[1,4]=="(8) Cobby & Le Du")mCLD),
	logL=(if(critbest[1,4]=="(9) Papajcsik and Bodero 1")mPapBo1),
	logL=(if(critbest[1,4]=="(10) Papajcsik and Bodero 2")mPapBo2),
	logL=(if(critbest[1,4]=="(11) Papajcsik and Bodero 3")mPapBo3),
	logL=(if(critbest[1,4]=="(12) Papajcsik and Bodero 4")mPapBo4),
	logL=(if(critbest[1,4]=="(13) Papajcsik and Bodero 6")mPapBo6),
	logL=(if(critbest[1,4]=="(14) Mixed log model 1 (Guo & Swalve)")mGS1),
	logL=(if(critbest[1,4]=="(15) Mixed log model 3 (Guo & Swalve)")mGS2),
	logL=(if(critbest[1,4]=="(16) Log-quadratic (Adediran)")mLQ),
	logL=(if(critbest[1,4]=="(17) Wilmink")mwil),
	logL=(if(critbest[1,4]=="(17a) Wilmink (Kheidirabadi)")mwilk),
	logL=(if(critbest[1,4]=="(17b) Wilmink (Laurenson & Strucken)")mwilycsml),
	logL=(if(critbest[1,4]=="(18) Bicompartemental (Ferguson & Boston)")mBC),
	logL=(if(critbest[1,4]=="(19) Dijkstra")mDJK),
	logL=(if(critbest[1,4]=="(20) Morant & Gnanasakthy (Pollott)")mMG2),
	logL=(if(critbest[1,4]=="(21) Morant & Gnanasakthy (Vargas)")mMG4),
	logL=(if(critbest[1,4]=="(22) Morant & Gnanasakthy (Adediran)")mMG),
	logL=(if(critbest[1,4]=="(23) Khandekar (Guo & Swalve)")mKHN),
	logL=(if(critbest[1,4]=="(24) Ali & Schaeffer")mAS),
	logL=(if(critbest[1,4]=="(25) Fractional Polynomial (Elvira)")mFRP),
	logL=(if(critbest[1,4]=="(26) Pollott multiplicative reduced (Elvira)")mPTmult),
	logL=(if(critbest[1,4]=="(27) Pollott modified (Adediran)")mPTmod),
	logL=(if(critbest[1,4]=="(28) Monophasic Grossman")mMonoG),
	logL=(if(critbest[1,4]=="(29) Monophasic Grossman power")mMonoGpw),
	logL=(if(critbest[1,4]=="(30) Diphasic Grossman")mDiG),
	logL=(if(critbest[1,4]=="(31) Diphasic Grossman power")mDiGpw),
	logL=(if(critbest[1,4]=="(32) Legendre Polynomial 3th (Jacobson)")mlegpol3),
	logL=(if(critbest[1,4]=="(33) Legendre Polynomial 4th (Jacobson)")mlegpol4),
	logL=(if(critbest[1,4]=="(34) Legendre + Wilmink (Lindauer)")mlegpolWil),
	logL=(if(critbest[1,4]=="(35) Natural Cubic Spline (3 percentiles)")mcubsplin3),
	logL=(if(critbest[1,4]=="(36) Natural Cubic Spline (4 percentiles)")mcubsplin4),
	logL=(if(critbest[1,4]=="(37) Natural Cubic Spline (5 percentiles)")mcubsplin5),
	logL=(if(critbest[1,4]=="(38) Natural Cubic Spline (defined Harrell)")mcubsplindef)
	)

AICmodel=c(
	AIC=(if(critbest[1,5]=="(1) Michaelis-Menten")mMM),
	AIC=(if(critbest[1,5]=="(1a) Michaelis-Menten (Rook)")mMMR),
	AIC=(if(critbest[1,5]=="(1b) Michaelis-Menten + Exponential (Rook)")mMME),
	AIC=(if(critbest[1,5]=="(2) Brody 1923")mbrody23),
	AIC=(if(critbest[1,5]=="(3) Brody 1924")mbrody24),
	AIC=(if(critbest[1,5]=="(4) Schumacher")mSCH),
	AIC=(if(critbest[1,5]=="(4a) Schumacher (Lopez)")mSCHL),
	AIC=(if(critbest[1,5]=="(5) Parabolic Exponential (Sikka, Adediran)")mPBE),
	AIC=(if(critbest[1,5]=="(6) Wood")mwood),
	AIC=(if(critbest[1,5]=="(6a) Wood (Dhanoa)")mDHA),
	AIC=(if(critbest[1,5]=="(6b) Wood (Cappio-Borlino)")mCB),
	AIC=(if(critbest[1,5]=="(7) Quadratic Polynomial (Dave)")mQP),
	AIC=(if(critbest[1,5]=="(8) Cobby & Le Du")mCLD),
	AIC=(if(critbest[1,5]=="(9) Papajcsik and Bodero 1")mPapBo1),
	AIC=(if(critbest[1,5]=="(10) Papajcsik and Bodero 2")mPapBo2),
	AIC=(if(critbest[1,5]=="(11) Papajcsik and Bodero 3")mPapBo3),
	AIC=(if(critbest[1,5]=="(12) Papajcsik and Bodero 4")mPapBo4),
	AIC=(if(critbest[1,5]=="(13) Papajcsik and Bodero 6")mPapBo6),
	AIC=(if(critbest[1,5]=="(14) Mixed log model 1 (Guo & Swalve)")mGS1),
	AIC=(if(critbest[1,5]=="(15) Mixed log model 3 (Guo & Swalve)")mGS2),
	AIC=(if(critbest[1,5]=="(16) Log-quadratic (Adediran)")mLQ),
	AIC=(if(critbest[1,5]=="(17) Wilmink")mwil),
	AIC=(if(critbest[1,5]=="(17a) Wilmink (Kheidirabadi)")mwilk),
	AIC=(if(critbest[1,5]=="(17b) Wilmink (Laurenson & Strucken)")mwilycsml),
	AIC=(if(critbest[1,5]=="(18) Bicompartemental (Ferguson & Boston)")mBC),
	AIC=(if(critbest[1,5]=="(19) Dijkstra")mDJK),
	AIC=(if(critbest[1,5]=="(20) Morant & Gnanasakthy (Pollott)")mMG2),
	AIC=(if(critbest[1,5]=="(21) Morant & Gnanasakthy (Vargas)")mMG4),
	AIC=(if(critbest[1,5]=="(22) Morant & Gnanasakthy (Adediran)")mMG),
	AIC=(if(critbest[1,5]=="(23) Khandekar (Guo & Swalve)")mKHN),
	AIC=(if(critbest[1,5]=="(24) Ali & Schaeffer")mAS),
	AIC=(if(critbest[1,5]=="(25) Fractional Polynomial (Elvira)")mFRP),
	AIC=(if(critbest[1,5]=="(26) Pollott multiplicative reduced (Elvira)")mPTmult),
	AIC=(if(critbest[1,5]=="(27) Pollott modified (Adediran)")mPTmod),
	AIC=(if(critbest[1,5]=="(28) Monophasic Grossman")mMonoG),
	AIC=(if(critbest[1,5]=="(29) Monophasic Grossman power")mMonoGpw),
	AIC=(if(critbest[1,5]=="(30) Diphasic Grossman")mDiG),
	AIC=(if(critbest[1,5]=="(31) Diphasic Grossman power")mDiGpw),
	AIC=(if(critbest[1,5]=="(32) Legendre Polynomial 3th (Jacobson)")mlegpol3),
	AIC=(if(critbest[1,5]=="(33) Legendre Polynomial 4th (Jacobson)")mlegpol4),
	AIC=(if(critbest[1,5]=="(34) Legendre + Wilmink (Lindauer)")mlegpolWil),
	AIC=(if(critbest[1,5]=="(35) Natural Cubic Spline (3 percentiles)")mcubsplin3),
	AIC=(if(critbest[1,5]=="(36) Natural Cubic Spline (4 percentiles)")mcubsplin4),
	AIC=(if(critbest[1,5]=="(37) Natural Cubic Spline (5 percentiles)")mcubsplin5),
	AIC=(if(critbest[1,5]=="(38) Natural Cubic Spline (defined Harrell)")mcubsplindef)
	)

AICCmodel=c(
	AICC=(if(critbest[1,6]=="(1) Michaelis-Menten")mMM),
	AICC=(if(critbest[1,6]=="(1a) Michaelis-Menten (Rook)")mMMR),
	AICC=(if(critbest[1,6]=="(1b) Michaelis-Menten + Exponential (Rook)")mMME),
	AICC=(if(critbest[1,6]=="(2) Brody 1923")mbrody23),
	AICC=(if(critbest[1,6]=="(3) Brody 1924")mbrody24),
	AICC=(if(critbest[1,6]=="(4) Schumacher")mSCH),
	AICC=(if(critbest[1,6]=="(4a) Schumacher (Lopez)")mSCHL),
	AICC=(if(critbest[1,6]=="(5) Parabolic Exponential (Sikka, Adediran)")mPBE),
	AICC=(if(critbest[1,6]=="(6) Wood")mwood),
	AICC=(if(critbest[1,6]=="(6a) Wood (Dhanoa)")mDHA),
	AICC=(if(critbest[1,6]=="(6b) Wood (Cappio-Borlino)")mCB),
	AICC=(if(critbest[1,6]=="(7) Quadratic Polynomial (Dave)")mQP),
	AICC=(if(critbest[1,6]=="(8) Cobby & Le Du")mCLD),
	AICC=(if(critbest[1,6]=="(9) Papajcsik and Bodero 1")mPapBo1),
	AICC=(if(critbest[1,6]=="(10) Papajcsik and Bodero 2")mPapBo2),
	AICC=(if(critbest[1,6]=="(11) Papajcsik and Bodero 3")mPapBo3),
	AICC=(if(critbest[1,6]=="(12) Papajcsik and Bodero 4")mPapBo4),
	AICC=(if(critbest[1,6]=="(13) Papajcsik and Bodero 6")mPapBo6),
	AICC=(if(critbest[1,6]=="(14) Mixed log model 1 (Guo & Swalve)")mGS1),
	AICC=(if(critbest[1,6]=="(15) Mixed log model 3 (Guo & Swalve)")mGS2),
	AICC=(if(critbest[1,6]=="(16) Log-quadratic (Adediran)")mLQ),
	AICC=(if(critbest[1,6]=="(17) Wilmink")mwil),
	AICC=(if(critbest[1,6]=="(17a) Wilmink (Kheidirabadi)")mwilk),
	AICC=(if(critbest[1,6]=="(17b) Wilmink (Laurenson & Strucken)")mwilycsml),
	AICC=(if(critbest[1,6]=="(18) Bicompartemental (Ferguson & Boston)")mBC),
	AICC=(if(critbest[1,6]=="(19) Dijkstra")mDJK),
	AICC=(if(critbest[1,6]=="(20) Morant & Gnanasakthy (Pollott)")mMG2),
	AICC=(if(critbest[1,6]=="(21) Morant & Gnanasakthy (Vargas)")mMG4),
	AICC=(if(critbest[1,6]=="(22) Morant & Gnanasakthy (Adediran)")mMG),
	AICC=(if(critbest[1,6]=="(23) Khandekar (Guo & Swalve)")mKHN),
	AICC=(if(critbest[1,6]=="(24) Ali & Schaeffer")mAS),
	AICC=(if(critbest[1,6]=="(25) Fractional Polynomial (Elvira)")mFRP),
	AICC=(if(critbest[1,6]=="(26) Pollott multiplicative reduced (Elvira)")mPTmult),
	AICC=(if(critbest[1,6]=="(27) Pollott modified (Adediran)")mPTmod),
	AICC=(if(critbest[1,6]=="(28) Monophasic Grossman")mMonoG),
	AICC=(if(critbest[1,6]=="(29) Monophasic Grossman power")mMonoGpw),
	AICC=(if(critbest[1,6]=="(30) Diphasic Grossman")mDiG),
	AICC=(if(critbest[1,6]=="(31) Diphasic Grossman power")mDiGpw),
	AICC=(if(critbest[1,6]=="(32) Legendre Polynomial 3th (Jacobson)")mlegpol3),
	AICC=(if(critbest[1,6]=="(33) Legendre Polynomial 4th (Jacobson)")mlegpol4),
	AICC=(if(critbest[1,6]=="(34) Legendre + Wilmink (Lindauer)")mlegpolWil),
	AICC=(if(critbest[1,6]=="(35) Natural Cubic Spline (3 percentiles)")mcubsplin3),
	AICC=(if(critbest[1,6]=="(36) Natural Cubic Spline (4 percentiles)")mcubsplin4),
	AICC=(if(critbest[1,6]=="(37) Natural Cubic Spline (5 percentiles)")mcubsplin5),
	AICC=(if(critbest[1,6]=="(38) Natural Cubic Spline (defined Harrell)")mcubsplindef)
	)

BICmodel=c(
	BIC=(if(critbest[1,7]=="(1) Michaelis-Menten")mMM),
	BIC=(if(critbest[1,7]=="(1a) Michaelis-Menten (Rook)")mMMR),
	BIC=(if(critbest[1,7]=="(1b) Michaelis-Menten + Exponential (Rook)")mMME),
	BIC=(if(critbest[1,7]=="(2) Brody 1923")mbrody23),
	BIC=(if(critbest[1,7]=="(3) Brody 1924")mbrody24),
	BIC=(if(critbest[1,7]=="(4) Schumacher")mSCH),
	BIC=(if(critbest[1,7]=="(4a) Schumacher (Lopez)")mSCHL),
	BIC=(if(critbest[1,7]=="(5) Parabolic Exponential (Sikka, Adediran)")mPBE),
	BIC=(if(critbest[1,7]=="(6) Wood")mwood),
	BIC=(if(critbest[1,7]=="(6a) Wood (Dhanoa)")mDHA),
	BIC=(if(critbest[1,7]=="(6b) Wood (Cappio-Borlino)")mCB),
	BIC=(if(critbest[1,7]=="(7) Quadratic Polynomial (Dave)")mQP),
	BIC=(if(critbest[1,7]=="(8) Cobby & Le Du")mCLD),
	BIC=(if(critbest[1,7]=="(9) Papajcsik and Bodero 1")mPapBo1),
	BIC=(if(critbest[1,7]=="(10) Papajcsik and Bodero 2")mPapBo2),
	BIC=(if(critbest[1,7]=="(11) Papajcsik and Bodero 3")mPapBo3),
	BIC=(if(critbest[1,7]=="(12) Papajcsik and Bodero 4")mPapBo4),
	BIC=(if(critbest[1,7]=="(13) Papajcsik and Bodero 6")mPapBo6),
	BIC=(if(critbest[1,7]=="(14) Mixed log model 1 (Guo & Swalve)")mGS1),
	BIC=(if(critbest[1,7]=="(15) Mixed log model 3 (Guo & Swalve)")mGS2),
	BIC=(if(critbest[1,7]=="(16) Log-quadratic (Adediran)")mLQ),
	BIC=(if(critbest[1,7]=="(17) Wilmink")mwil),
	BIC=(if(critbest[1,7]=="(17a) Wilmink (Kheidirabadi)")mwilk),
	BIC=(if(critbest[1,7]=="(17b) Wilmink (Laurenson & Strucken)")mwilycsml),
	BIC=(if(critbest[1,7]=="(18) Bicompartemental (Ferguson & Boston)")mBC),
	BIC=(if(critbest[1,7]=="(19) Dijkstra")mDJK),
	BIC=(if(critbest[1,7]=="(20) Morant & Gnanasakthy (Pollott)")mMG2),
	BIC=(if(critbest[1,7]=="(21) Morant & Gnanasakthy (Vargas)")mMG4),
	BIC=(if(critbest[1,7]=="(22) Morant & Gnanasakthy (Adediran)")mMG),
	BIC=(if(critbest[1,7]=="(23) Khandekar (Guo & Swalve)")mKHN),
	BIC=(if(critbest[1,7]=="(24) Ali & Schaeffer")mAS),
	BIC=(if(critbest[1,7]=="(25) Fractional Polynomial (Elvira)")mFRP),
	BIC=(if(critbest[1,7]=="(26) Pollott multiplicative reduced (Elvira)")mPTmult),
	BIC=(if(critbest[1,7]=="(27) Pollott modified (Adediran)")mPTmod),
	BIC=(if(critbest[1,7]=="(28) Monophasic Grossman")mMonoG),
	BIC=(if(critbest[1,7]=="(29) Monophasic Grossman power")mMonoGpw),
	BIC=(if(critbest[1,7]=="(30) Diphasic Grossman")mDiG),
	BIC=(if(critbest[1,7]=="(31) Diphasic Grossman power")mDiGpw),
	BIC=(if(critbest[1,7]=="(32) Legendre Polynomial 3th (Jacobson)")mlegpol3),
	BIC=(if(critbest[1,7]=="(33) Legendre Polynomial 4th (Jacobson)")mlegpol4),
	BIC=(if(critbest[1,7]=="(34) Legendre + Wilmink (Lindauer)")mlegpolWil),
	BIC=(if(critbest[1,7]=="(35) Natural Cubic Spline (3 percentiles)")mcubsplin3),
	BIC=(if(critbest[1,7]=="(36) Natural Cubic Spline (4 percentiles)")mcubsplin4),
	BIC=(if(critbest[1,7]=="(37) Natural Cubic Spline (5 percentiles)")mcubsplin5),
	BIC=(if(critbest[1,7]=="(38) Natural Cubic Spline (defined Harrell)")mcubsplindef)
	)

DWmodel=c(
	DW=(if(critbest[1,8]=="(1) Michaelis-Menten")mMM),
	DW=(if(critbest[1,8]=="(1a) Michaelis-Menten (Rook)")mMMR),
	DW=(if(critbest[1,8]=="(1b) Michaelis-Menten + Exponential (Rook)")mMME),
	DW=(if(critbest[1,8]=="(2) Brody 1923")mbrody23),
	DW=(if(critbest[1,8]=="(3) Brody 1924")mbrody24),
	DW=(if(critbest[1,8]=="(4) Schumacher")mSCH),
	DW=(if(critbest[1,8]=="(4a) Schumacher (Lopez)")mSCHL),
	DW=(if(critbest[1,8]=="(5) Parabolic Exponential (Sikka, Adediran)")mPBE),
	DW=(if(critbest[1,8]=="(6) Wood")mwood),
	DW=(if(critbest[1,8]=="(6a) Wood (Dhanoa)")mDHA),
	DW=(if(critbest[1,8]=="(6b) Wood (Cappio-Borlino)")mCB),
	DW=(if(critbest[1,8]=="(7) Quadratic Polynomial (Dave)")mQP),
	DW=(if(critbest[1,8]=="(8) Cobby & Le Du")mCLD),
	DW=(if(critbest[1,8]=="(9) Papajcsik and Bodero 1")mPapBo1),
	DW=(if(critbest[1,8]=="(10) Papajcsik and Bodero 2")mPapBo2),
	DW=(if(critbest[1,8]=="(11) Papajcsik and Bodero 3")mPapBo3),
	DW=(if(critbest[1,8]=="(12) Papajcsik and Bodero 4")mPapBo4),
	DW=(if(critbest[1,8]=="(13) Papajcsik and Bodero 6")mPapBo6),
	DW=(if(critbest[1,8]=="(14) Mixed log model 1 (Guo & Swalve)")mGS1),
	DW=(if(critbest[1,8]=="(15) Mixed log model 3 (Guo & Swalve)")mGS2),
	DW=(if(critbest[1,8]=="(16) Log-quadratic (Adediran)")mLQ),
	DW=(if(critbest[1,8]=="(17) Wilmink")mwil),
	DW=(if(critbest[1,8]=="(17a) Wilmink (Kheidirabadi)")mwilk),
	DW=(if(critbest[1,8]=="(17b) Wilmink (Laurenson & Strucken)")mwilycsml),
	DW=(if(critbest[1,8]=="(18) Bicompartemental (Ferguson & Boston)")mBC),
	DW=(if(critbest[1,8]=="(19) Dijkstra")mDJK),
	DW=(if(critbest[1,8]=="(20) Morant & Gnanasakthy (Pollott)")mMG2),
	DW=(if(critbest[1,8]=="(21) Morant & Gnanasakthy (Vargas)")mMG4),
	DW=(if(critbest[1,8]=="(22) Morant & Gnanasakthy (Adediran)")mMG),
	DW=(if(critbest[1,8]=="(23) Khandekar (Guo & Swalve)")mKHN),
	DW=(if(critbest[1,8]=="(24) Ali & Schaeffer")mAS),
	DW=(if(critbest[1,8]=="(25) Fractional Polynomial (Elvira)")mFRP),
	DW=(if(critbest[1,8]=="(26) Pollott multiplicative reduced (Elvira)")mPTmult),
	DW=(if(critbest[1,8]=="(27) Pollott modified (Adediran)")mPTmod),
	DW=(if(critbest[1,8]=="(28) Monophasic Grossman")mMonoG),
	DW=(if(critbest[1,8]=="(29) Monophasic Grossman power")mMonoGpw),
	DW=(if(critbest[1,8]=="(30) Diphasic Grossman")mDiG),
	DW=(if(critbest[1,8]=="(31) Diphasic Grossman power")mDiGpw),
	DW=(if(critbest[1,8]=="(32) Legendre Polynomial 3th (Jacobson)")mlegpol3),
	DW=(if(critbest[1,8]=="(33) Legendre Polynomial 4th (Jacobson)")mlegpol4),
	DW=(if(critbest[1,8]=="(34) Legendre +  Wilmink (Lindauer)")mlegpolWil),
	DW=(if(critbest[1,8]=="(35) Natural Cubic Spline (3 percentiles)")mcubsplin3),
	DW=(if(critbest[1,8]=="(36) Natural Cubic Spline (4 percentiles)")mcubsplin4),
	DW=(if(critbest[1,8]=="(37) Natural Cubic Spline (5 percentiles)")mcubsplin5),
	DW=(if(critbest[1,8]=="(38) Natural Cubic Spline (defined Harrell)")mcubsplindef)
	)

#################################################################
############### Model Parameters RSS, RSD, F ####################

RSS=c(
	RSSMM,RSSMMR,RSSMME,RSSbrody23,
	RSSbrody24,RSSSCH,RSSSCHL,RSSPBE,
	RSSwood,RSSDHA,RSSCB,RSSQP,
	RSSCLD,RSSPapBo1,RSSPapBo2,RSSPapBo3,
	RSSPapBo4,RSSPapBo6,RSSGS1,RSSGS2,
	RSSLQ,RSSwil,RSSwilk,RSSwilycsml,
	RSSBC,RSSDJK,RSSMG2,RSSMG4,
	RSSMG,RSSKHN,RSSAS,RSSFRP,
	RSSPTmult,RSSPTmod,RSSMonoG,RSSMonoGpw,
	RSSDiG,RSSDiGpw,RSSlegpol3,RSSlegpol4,
	RSSlegpolWil,RSScubsplin3,RSScubsplin4,
	RSScubsplin5,RSScubsplindef
	)

RSD=c(
	RSDMM,RSDMMR,RSDMME,RSDbrody23,
	RSDbrody24,RSDSCH,RSDSCHL,RSDPBE,
	RSDwood,RSDDHA,RSDCB,RSDQP,
	RSDCLD,RSDPapBo1,RSDPapBo2,RSDPapBo3,
	RSDPapBo4,RSDPapBo6,RSDGS1,RSDGS2,
	RSDLQ,RSDwil,RSDwilk,RSDwilycsml,
	RSDBC,RSDDJK,RSDMG2,RSDMG4,
	RSDMG,RSDKHN,RSDAS,RSDFRP,
	RSDPTmult,RSDPTmod,RSDMonoG,RSDMonoGpw,
	RSDDiG,RSDDiGpw,RSDlegpol3,RSDlegpol4,
	RSDlegpolWil,RSDcubsplin3,RSDcubsplin4,
	RSDcubsplin5,RSDcubsplindef
	)

F=c(
	FMM,FMMR,FMME,Fbrody23,
	Fbrody24,FSCH,FSCHL,FPBE,
	Fwood,FDHA,FCB,FQP,
	FCLD,FPapBo1,FPapBo2,FPapBo3,
	FPapBo4,FPapBo6,FGS1,FGS2,
	FLQ,Fwil,Fwilk,Fwilycsml,
	FBC,FDJK,FMG2,FMG4,
	FMG,FKHN,FAS,FFRP,
	FPTmult,FPTmod,FMonoG,FMonoGpw,
	FDiG,FDiGpw,Flegpol3,Flegpol4,
	FlegpolWil,Fcubsplin3,Fcubsplin4,
	Fcubsplin5,Fcubsplindef
	)

ModelPar=cbind(RSS,RSD,F)
rownames(ModelPar)=c(
	"(1) Michaelis-Menten",
	"(1a) Michaelis-Menten (Rook)",
	"(1b) Michaelis-Menten + Exponential (Rook)",
	"(2) Brody 1923",
	"(3) Brody 1924",
	"(4) Schumacher  ",
	"(4a) Schumacher (Lopez)",
	"(5) Parabolic Exponential (Sikka, Adediran)",
	"(6) Wood",
	"(6a) Wood (Dhanoa)",
	"(6b) Wood (Cappio-Borlino)",
	"(7) Quadratic Polynomial (Dave)",
	"(8) Cobby & Le Du",
	"(9) Papajcsik and Bodero 1",
	"(10) Papajcsik and Bodero 2",
	"(11) Papajcsik and Bodero 3",
	"(12) Papajcsik and Bodero 4",
	"(13) Papajcsik and Bodero 6",
	"(14) Mixed log model 1 (Guo & Swalve)",
	"(15) Mixed log model 3 (Guo & Swalve)",
	"(16) Log-quadratic (Adediran)",
	"(17) Wilmink",
	"(17a) Wilmink (Kheidirabadi)",
	"(17b) Wilmink (Laurenson & Strucken)",
	"(18) Bicompartemental (Ferguson & Boston)",
	"(19) Dijkstra",
	"(20) Morant & Gnanasakthy (Pollott)",
	"(21) Morant & Gnanasakthy (Vargas)",
	"(22) Morant & Gnanasakthy (Adediran)",
	"(23) Khandekar (Guo & Swalve)",
	"(24) Ali & Schaeffer",
	"(25) Fractional Polynomial (Elvira)",
	"(26) Pollott multiplicative reduced (Elvira)",
	"(27) Pollott modified (Adediran)",
	"(28) Monophasic Grossman",
	"(29) Monophasic Grossman power",
	"(30) Diphasic Grossman",
	"(31) Diphasic Grossman power",
	"(32) Legendre Polynomial 3th (Jacobson)",
	"(33) Legendre Polynomial 4th (Jacobson)",
	"(34) Legendre + Wilmink (Lindauer)",
	"(35) Natural Cubic Spline (3 percentiles)",
	"(36) Natural Cubic Spline (4 percentiles)",
	"(37) Natural Cubic Spline (5 percentiles)",
	"(38) Natural Cubic Spline (defined Harrell)"
	)

ModelPar2=ModelPar
ModelPar2[1,]=if(warning[1,2]=="TRUE"){ModelPar2[1,]} else {"Error"}
ModelPar2[2,]=if(warning[2,2]=="TRUE"){ModelPar2[2,]} else {"Error"}
ModelPar2[3,]=if(warning[3,2]=="TRUE"){ModelPar2[3,]} else {"Error"}
ModelPar2[4,]=if(warning[4,2]=="TRUE"){ModelPar2[4,]} else {"Error"}
ModelPar2[5,]=if(warning[5,2]=="TRUE"){ModelPar2[5,]} else {"Error"}
ModelPar2[6,]=if(warning[6,2]=="TRUE"){ModelPar2[6,]} else {"Error"}
ModelPar2[7,]=if(warning[7,2]=="TRUE"){ModelPar2[7,]} else {"Error"}
ModelPar2[8,]=if(warning[8,2]=="TRUE"){ModelPar2[8,]} else {"Error"}
ModelPar2[9,]=if(warning[9,2]=="TRUE"){ModelPar2[9,]} else {"Error"}
ModelPar2[10,]=if(warning[10,2]=="TRUE"){ModelPar2[10,]} else {"Error"}
ModelPar2[11,]=if(warning[11,2]=="TRUE"){ModelPar2[11,]} else {"Error"}
ModelPar2[12,]=if(warning[12,2]=="TRUE"){ModelPar2[12,]} else {"Error"}
ModelPar2[13,]=if(warning[13,2]=="TRUE"){ModelPar2[13,]} else {"Error"}
ModelPar2[14,]=if(warning[14,2]=="TRUE"){ModelPar2[14,]} else {"Error"}
ModelPar2[15,]=if(warning[15,2]=="TRUE"){ModelPar2[15,]} else {"Error"}
ModelPar2[16,]=if(warning[16,2]=="TRUE"){ModelPar2[16,]} else {"Error"}
ModelPar2[17,]=if(warning[17,2]=="TRUE"){ModelPar2[17,]} else {"Error"}
ModelPar2[18,]=if(warning[18,2]=="TRUE"){ModelPar2[18,]} else {"Error"}
ModelPar2[19,]=if(warning[19,2]=="TRUE"){ModelPar2[19,]} else {"Error"}
ModelPar2[20,]=if(warning[20,2]=="TRUE"){ModelPar2[20,]} else {"Error"}
ModelPar2[21,]=if(warning[21,2]=="TRUE"){ModelPar2[21,]} else {"Error"}
ModelPar2[22,]=if(warning[22,2]=="TRUE"){ModelPar2[22,]} else {"Error"}
ModelPar2[23,]=if(warning[23,2]=="TRUE"){ModelPar2[23,]} else {"Error"}
ModelPar2[24,]=if(warning[24,2]=="TRUE"){ModelPar2[24,]} else {"Error"}
ModelPar2[25,]=if(warning[25,2]=="TRUE"){ModelPar2[25,]} else {"Error"}
ModelPar2[26,]=if(warning[26,2]=="TRUE"){ModelPar2[26,]} else {"Error"}
ModelPar2[27,]=if(warning[27,2]=="TRUE"){ModelPar2[27,]} else {"Error"}
ModelPar2[28,]=if(warning[28,2]=="TRUE"){ModelPar2[28,]} else {"Error"}
ModelPar2[29,]=if(warning[29,2]=="TRUE"){ModelPar2[29,]} else {"Error"}
ModelPar2[30,]=if(warning[30,2]=="TRUE"){ModelPar2[30,]} else {"Error"}
ModelPar2[31,]=if(warning[31,2]=="TRUE"){ModelPar2[31,]} else {"Error"}
ModelPar2[32,]=if(warning[32,2]=="TRUE"){ModelPar2[32,]} else {"Error"}
ModelPar2[33,]=if(warning[33,2]=="TRUE"){ModelPar2[33,]} else {"Error"}
ModelPar2[34,]=if(warning[34,2]=="TRUE"){ModelPar2[34,]} else {"Error"}
ModelPar2[35,]=if(warning[35,2]=="TRUE"){ModelPar2[35,]} else {"Error"}
ModelPar2[36,]=if(warning[36,2]=="TRUE"){ModelPar2[36,]} else {"Error"}
ModelPar2[37,]=if(warning[37,2]=="TRUE"){ModelPar2[37,]} else {"Error"}
ModelPar2[38,]=if(warning[38,2]=="TRUE"){ModelPar2[38,]} else {"Error"}
ModelPar2[39,]=if(warning[39,2]=="TRUE"){ModelPar2[39,]} else {"Error"}
ModelPar2[40,]=if(warning[40,2]=="TRUE"){ModelPar2[40,]} else {"Error"}
ModelPar2[41,]=if(warning[41,2]=="TRUE"){ModelPar2[41,]} else {"Error"}
ModelPar2[42,]=if(warning[42,2]=="TRUE"){ModelPar2[42,]} else {"Error"}
ModelPar2[43,]=if(warning[43,2]=="TRUE"){ModelPar2[43,]} else {"Error"}
ModelPar2[44,]=if(warning[44,2]=="TRUE"){ModelPar2[44,]} else {"Error"}
ModelPar2[45,]=if(warning[45,2]=="TRUE"){ModelPar2[45,]} else {"Error"}


#################################################################
##################### Model Parameters ##########################

length(paramMM)=7
length(paramMMR)=7
length(paramMME)=7
length(parambrody23)=7
length(parambrody24)=7
length(paramSCH)=7
length(paramSCHL)=7
length(paramPBE)=7
length(paramwood)=7
length(paramDHA)=7
length(paramCB)=7
length(paramQP)=7
length(paramCLD)=7
length(paramPapBo1)=7
length(paramPapBo2)=7
length(paramPapBo3)=7
length(paramPapBo4)=7
length(paramPapBo6)=7
length(paramGS1)=7
length(paramGS2)=7
length(paramLQ)=7
paramwil2=c(paramwil[1:3],"NA","NA","NA",paramwil[4])
paramwilk2=c(paramwilk[1:3],"NA","NA","NA",paramwilk[4])
paramwilycsml2=c(paramwilycsml[1:3],"NA","NA","NA",paramwilycsml[4])
length(paramBC)=7
length(paramDJK)=7
length(paramMG2)=7
length(paramMG4)=7
length(paramMG)=7
length(paramKHN)=7
length(paramAS)=7
length(paramFRP)=7
length(paramPTmult)=7
length(paramPTmod)=7
length(paramMonoG)=7
paramMonoGpw2=c(paramMonoGpw[1:3],"NA","NA","NA",paramMonoGpw[4])
length(paramDiG)=7
length(paramDiGpw)=7
length(parampolWil)=7

length(paramleg3)=5
length(paramleg4)=5

length(paramcubsplin3)=7
length(paramcubsplin4)=7
length(paramcubsplin5)=7
length(paramcubsplindef)=7

paramabcdfgk=t(cbind(
  paramMM,paramMMR,paramMME,parambrody23,
  parambrody24,paramSCH,paramSCHL,paramPBE,
  paramwood,paramDHA,paramCB,paramQP,
  paramCLD,paramPapBo1,paramPapBo2,paramPapBo3,
  paramPapBo4,paramPapBo6,paramGS1,paramGS2,
  paramLQ,paramwil2,paramwilk2,paramwilycsml2,
  paramBC,paramDJK,paramMG2,paramMG4,
  paramMG,paramKHN,paramAS,paramFRP,
  paramPTmult,paramPTmod,paramMonoG,paramMonoGpw2,
  paramDiG,paramDiGpw,parampolWil
))

paramleg=t(cbind(
  paramleg3,paramleg4
))

paramspline=t(cbind(
  paramcubsplin3,paramcubsplin4,
  paramcubsplin5,paramcubsplindef
))

rownames(paramabcdfgk)=c(
  "(1) Michaelis-Menten",
  "(1a) Michaelis-Menten (Rook)",
  "(1b) Michaelis-Menten + Exponential (Rook)",
  "(2) Brody 1923",
  "(3) Brody 1924",
  "(4) Schumacher  ",
  "(4a) Schumacher (Lopez)",
  "(5) Parabolic Exponential (Sikka, Adediran)",
  "(6) Wood",
  "(6a) Wood (Dhanoa)",
  "(6b) Wood (Cappio-Borlino)",
  "(7) Quadratic Polynomial (Dave)",
  "(8) Cobby & Le Du",
  "(9) Papajcsik and Bodero 1",
  "(10) Papajcsik and Bodero 2",
  "(11) Papajcsik and Bodero 3",
  "(12) Papajcsik and Bodero 4",
  "(13) Papajcsik and Bodero 6",
  "(14) Mixed log model 1 (Guo & Swalve)",
  "(15) Mixed log model 3 (Guo & Swalve)",
  "(16) Log-quadratic (Adediran)",
  "(17) Wilmink",
  "(17a) Wilmink (Kheidirabadi)",
  "(17b) Wilmink (Laurenson & Strucken)",
  "(18) Bicompartemental (Ferguson & Boston)",
  "(19) Dijkstra",
  "(20) Morant & Gnanasakthy (Pollott)",
  "(21) Morant & Gnanasakthy (Vargas)",
  "(22) Morant & Gnanasakthy (Adediran)",
  "(23) Khandekar (Guo & Swalve)",
  "(24) Ali & Schaeffer",
  "(25) Fractional Polynomial (Elvira)",
  "(26) Pollott multiplicative reduced (Elvira)",
  "(27) Pollott modified (Adediran)",
  "(28) Monophasic Grossman",
  "(29) Monophasic Grossman power (Weigel, Sherchand)",
  "(30) Diphasic Grossman",
  "(31) Diphasic Grossman power (Weigel, Sherchand)",
  "(34) Legendre + Wilmink (Lopez)"
)
colnames(paramabcdfgk)=c("a","b","c","d","f","g","k")

rownames(paramleg)=c(
  "(32) Legendre Polynomial 3th (Kirkpatrick)",
  "(33) Legendre Polynomial 4th (Kirkpatrick)"
)
colnames(paramleg)=c("Intercept","leg1","leg2","leg3","leg4")

rownames(paramspline)=c(
  "(35) Natural Cubic Spline (3 percentiles)",
  "(36) Natural Cubic Spline (4 percentiles)",
  "(37) Natural Cubic Spline (5 percentiles)",
  "(38) Natural Cubic Spline (defined Harrell)"
)
colnames(paramspline)=c("Intercept","ns1","ns2","ns3","ns4","ns5","ns6")


#########################################
################ Output #################

list(

modelnames=c(
	"(1) Michaelis-Menten",
	"(1a) Michaelis-Menten (Rook)",
	"(1b) Michaelis-Menten + Exponential (Rook)",
	"(2) Brody 1923",
	"(3) Brody 1924",
	"(4) Schumacher  ",
	"(4a) Schumacher (Lopez)",
	"(5) Parabolic Exponential (Sikka, Adediran)",
	"(6) Wood",
	"(6a) Wood (Dhanoa)",
	"(6b) Wood (Cappio-Borlino)",
	"(7) Quadratic Polynomial (Dave)",
	"(8) Cobby & Le Du",
	"(9) Papajcsik and Bodero 1",
	"(10) Papajcsik and Bodero 2",
	"(11) Papajcsik and Bodero 3",
	"(12) Papajcsik and Bodero 4",
	"(13) Papajcsik and Bodero 6",
	"(14) Mixed log model 1 (Guo & Swalve)",
	"(15) Mixed log model 3 (Guo & Swalve)",
	"(16) Log-quadratic (Adediran)",
	"(17) Wilmink",
	"(17a) Wilmink (Kheidirabadi)",
	"(17b) Wilmink (Laurenson & Strucken)",
	"(18) Bicompartemental (Ferguson & Boston)",
	"(19) Dijkstra",
	"(20) Morant & Gnanasakthy (Pollott)",
	"(21) Morant & Gnanasakthy (Vargas)",
	"(22) Morant & Gnanasakthy (Adediran)",
	"(23) Khandekar (Guo & Swalve)",
	"(24) Ali & Schaeffer",
	"(25) Fractional Polynomial (Elvira)",
	"(26) Pollott multiplicative reduced (Elvira)",
	"(27) Pollott modified (Adediran)",
	"(28) Monophasic Grossman",
	"(29) Monophasic Grossman power",
	"(30) Diphasic Grossman",
	"(31) Diphasic Grossman power",
	"(32) Legendre Polynomial 3th (Jacobson)",
	"(33) Legendre Polynomial 4th (Jacobson)",
	"(34) Legendre + Wilmink (Lindauer)",
	"(35) Natural Cubic Spline (3 percentiles)",
	"(36) Natural Cubic Spline (4 percentiles)",
	"(37) Natural Cubic Spline (5 percentiles)",
	"(38) Natural Cubic Spline (defined Harrell)"
	),

model=c(
	MichaelisMenten=mMM,
	MichaelisMentenA=mMMR,
	MichaelisMentenB=mMME,
	Brody1=mbrody23,
	Brody2=mbrody24,
	Schumacher=mSCH,
	SchumacherA=mSCHL,
	ParabolicExponential=mPBE,
	Wood=mwood,
	WoodA=mDHA,
	WoodB=mCB,
	QuadraticPolynomial=mQP,
	CobbyLeDu=mCLD,
	PapajcsikBodero1=mPapBo1,
	PapajcsikBodero2=mPapBo2,
	PapajcsikBodero3=mPapBo3,
	PapajcsikBodero4=mPapBo4,
	PapajcsikBodero6=mPapBo6,
	Mixedlog1=mGS1,
	Mixedlog3=mGS2,
	LogQuadratic=mLQ,
	Wilmink=mwil,
	WilminkA=mwilk,
	WilminkB=mwilycsml,
	Bicompartemental=mBC,
	Dijkstra=mDJK,
	MorantGnanasakthy1=mMG2,
	MorantGnanasakthy2=mMG4,
	MorantGnanasakthy3=mMG,
	Khandekar=mKHN,
	AliSchaeffer=mAS,
	FractionalPolynomial=mFRP,
	Pollott1=mPTmult,
	Pollott2=mPTmod,
	Monophasic=mMonoG,
	Monophasicpower=mMonoGpw,
	Diphasic=mDiG,
	Diphasicpower=mDiGpw,
	Legendre3th=mlegpol3,
	Legendre4th=mlegpol4,
	LegendreWilmink=mlegpolWil,
	CubicSpline3=mcubsplin3,
	CubicSpline4=mcubsplin4,
	CubicSpline5=mcubsplin5,
	CubicSplinedefined=mcubsplindef
	),

critall=as.data.frame(selcri2),
modeldescrip=as.data.frame(ModelPar2),
critbest=as.data.frame(new2),
bestmodel=c(R2model,R2adjmodel,RSEmodel,logLmodel,AICmodel,AICCmodel,BICmodel,DWmodel),
Error=print(subset(warning[,1],warning[,2]=="FALSE")),
ModelParam=list(mathematical=as.data.frame(paramabcdfgk),Legendre=as.data.frame(paramleg),Splines=as.data.frame(paramspline)),
summary1=summaryMM,
summary1a=summaryMMR,
summary1b=summaryMME,
summary2=summarybrody23,
summary3=summarybrody24,
summary4=summarySCH,
summary4a=summarySCHL,
summary5=summaryPBE,
summary6=summarywood,
summary6a=summaryDHA,
summary6b=summaryCB,
summary7=summaryQP,
summary8=summaryCLD,
summary9=summaryPapBo1,
summary10=summaryPapBo2,
summary11=summaryPapBo3,
summary12=summaryPapBo4,
summary13=summaryPapBo6,
summary14=summaryGS1,
summary15=summaryGS2,
summary16=summaryLQ,
summary17=summarywil,
summary17a=summarywilk,
summary17b=summarywilycsml,
summary18=summaryBC,
summary19=summaryDJK,
summary20=summaryMG2,
summary21=summaryMG4,
summary22=summaryMG,
summary23=summaryKHN,
summary24=summaryAS,
summary25=summaryFRP,
summary26=summaryPTmult,
summary27=summaryPTmod,
summary28=summaryMonoG,
summary29=summaryMonoGpw,
summary30=summaryDiG,
summary31=summaryDiGpw,
summary32=summaryleg3,
summary33=summaryleg4,
summary34=summarypolWil,
summary35=summarycubsplin3,
summary36=summarycubsplin4,
summary37=summarycubsplin5,
summary38=summarycubsplindef
)

}








