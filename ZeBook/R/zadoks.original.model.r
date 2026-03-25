################################################################################
# "Working with dynamic models for agriculture"
# R script for practical work
# Daniel Wallach (INRA), David Makowski (INRA), James W. Jones (U.of Florida),
# Francois Brun (ACTA)
# version : 2018-03-04
# Model described in the book, Appendix. Models used as illustrative examples: description and R code
# model for Course SMACH - January 2014
################################ FUNCTIONS #####################################
#' @title Classical SEIR model for plant diseases from Zadoks (1971)
#' @description \strong{Model description.}
#' This model is a classical SEIR model for plant disease. It was written from it description included in the original publication of Zadoks (1971)
#' @details
#' This model is a classical SEIR model proposed by Zadoks (1971) to simulate epidemics of diseases of crops. It is a Susceptible-Exposed-Infectious-Removed (SEIR) model. This simple model of an epidemic is based on the epidemiological concepts "latent period", "infectious period", and "multiplication factor". The crop is considered to consist of a large but finite number of infectious sites. The physical dimensions of an infectious site roughly coincide with the reproductive unit of the parasite studied. Different pathosystems (with different infectious site definitions) can be considered with this model. A full description, is available in the original paper:
#' The model has four essential state variables representing the number of sites in each state
#' XVAC for vacant (healthy) sites, XLAT for latent site, XINF for infectant sites and XCTR for the cumulative total of removal (post infectious) sites.
#' Two supplementary variables based on the state variables are used defined as XTO1 = XLAT+XINF+XCTR and XSEV = XINF+XCTR.
#' Fluxes or rates between the state variables are defined as rocc for occupation, rapp for apparition and rrem for removal.
#' The model has a time step of one day (dt=1). The system modeled is one hectare of a wheat crop.

#' @param nlpd : latent period (in degree.day) - default value for Puccinia triticina : ~10 days at 20degC
#' @param nipd : infectious period (in degree.day) - default value for Puccinia triticina : ~20 days at 20degC
#' @param dmfr : daily multiplication factor - default value number of effective spores produced by lesion
#' @param SITE0 :
#' @param weather : weather data.frame for one single year
#' @param sdate : starting date
#' @param ldate : ending date
#' @param XLAT0 :
#' @return list with a data.frame with daily day, DACE, XVAC, XLAT, XINF, XCTR ,XTO1, XSEV= XSEV, severity
#' and a vector of parameter value (nlpd, nipd, dmfr, SITE0).
#' @seealso \code{\link{epirice.model}}
#' @export
#' @source Script written from equation described in Zadoks, J.C. 1971. Systems Analysis and the Dynamics of Epidemics. Phytopathology. 61:441-598.
#' @examples
#' weather=subset(weather_FranceWest, WEYR==1997 & idsite==39)
#' out=zakoks.original.model(nlpd=4*10,nipd=1*10,dmfr=16,SITE0 = 5*10^9,
#' weather, sdate = 145, ldate = 145+50 , XLAT0=1)
#' plot(out$sim$DACE,out$sim$severity, type="l")
################################################################################
zakoks.original.model = function (nlpd=4*10,nipd=1*10,dmfr=16,SITE0 = 5*10^9,weather, sdate = 145, ldate = 145+50, XLAT0=1){
    weather$TEEQ_L = 1/(nlpd)*pmax(0,(weather$TMIN+weather$TMAX)/2)
    weather$TEEQ_I = 1/(nipd)*pmax(0,(weather$TMIN+weather$TMAX)/2)

    XVAC <- rep(NA, ldate)
    XLAT <- rep(NA, ldate)
    XINF <- rep(NA, ldate)
    XCTR <- rep(NA, ldate)
    XTO1 <- rep(NA, ldate)
    # Latent : Boxcar train
    BOXL=matrix(0,nrow=ldate, ncol=2, dimnames =list(NULL, c("XLAT","sumTEEQ")))
    # Infectious : Boxcar train
    BOXI=matrix(0,nrow=ldate, ncol=2, dimnames =list(NULL, c("XINF","sumTEEQ")))

    XVAC[sdate] <- SITE0-XLAT0
    XLAT[sdate] <-  XLAT0
    XINF[sdate] <- 0
    XCTR[sdate] <- 0

    BOXL[sdate,"sumTEEQ"] = weather$TEEQ_L[sdate]
    BOXL[sdate,"XLAT"] = XLAT[sdate]
    #BOXI[sdate,"sumTEEQ"] = 0 #weather$TEEQ_I[sdate]
    #BOXI[sdate,"XINF"] = XINF[sdate]

    for (day in sdate:(ldate - 1)) {
        #if (day==inf_start){BOXI[day,"XINF"]=1, BOXI[sdate,"sumTEEQ"] = 1}
        # correction factor : feed back from total occupied sites
        cofr<-max(XVAC[day]/SITE0, 0)
        # rocc: rate of occupation : nb of sites Vacant=>Latent
        rocc = cofr * dmfr * sum(BOXI[,"XINF"],na.rm=TRUE)

        # rapp: rate of apparition : nb of sites Latent=>Infectant
        #print(BOXL[sdate:ldate,])
        pos_rapp = which(BOXL[,"sumTEEQ"]>=1&BOXL[,"XLAT"]>0)
        #print(pos_rapp)
        if(length(pos_rapp)>0){
          rapp=sum(BOXL[pos_rapp,"XLAT"])
          BOXL[pos_rapp,c("sumTEEQ","XLAT")]=c(NA,NA)
          }
          else {
          rapp=0
        }

        # rrem: rate of removal : nb of sites Infectant=>removed
        # rapp: rate of apparition : nb of sites Latent=>Infectant
        #print(BOXI[sdate:ldate,])
        pos_rrem = which(BOXI[,"sumTEEQ"]>=1&BOXI[,"XINF"]>0)
        #print(pos_rrem)
        if(length(pos_rrem)>0){
          rrem=sum(BOXI[pos_rrem,"XINF"])
          BOXI[pos_rrem,c("sumTEEQ","XINF")]=c(NA,NA)
          }
          else {
          rrem=0
        }

        XVAC[day+1] <- XVAC[day]-rocc
        XLAT[day + 1] <-XLAT[day] +rocc - rapp
        XINF[day+1] <- XINF[day] + rapp - rrem
        XCTR[day+1] <- XCTR[day] + rrem
        BOXL[1:(day+1),"sumTEEQ"] = BOXL[1:(day+1),"sumTEEQ"] + weather$TEEQ_L[day]
        BOXL[day+1,"XLAT"] = +rocc
        BOXI[1:(day+1),"sumTEEQ"] = BOXI[1:(day+1),"sumTEEQ"] + weather$TEEQ_I[day]
        BOXI[day+1,"XINF"] = +rapp

    }

    XTO1 = XLAT+XINF+XCTR
    XSEV = XINF+XCTR
    severity=XSEV/(XLAT+XINF+XCTR+XVAC)
    return(list(sim=data.frame(day = sdate:ldate, DACE = ((sdate:ldate)-sdate), XVAC = XVAC[sdate:ldate], XLAT = XLAT[sdate:ldate], XINF = XINF[sdate:ldate],XCTR = XCTR[sdate:ldate],XTO1=XTO1[sdate:ldate],  XSEV= XSEV[sdate:ldate],severity=severity[sdate:ldate]), param=c(nlpd=nlpd,nipd=nipd,dmfr=dmfr,SITE0 = SITE0)))
}

################################################################################
#' @title Plot output of a Classical SEIR model for plant disease
#' @description Plot the output of the Zadoks classical SEIR model for plant disease.
#' @param out : output of the zadoks.original.model
#' @param typel : type of plot (default : s)
#' @param all : if all=true (default), plot all the state variable
#' @param param : if param (default), add the values of param on the plot
#' @return plot
#' @seealso \code{\link{zakoks.original.model}}
#' @export
graph_epid_s=function(out,typel="s",all=TRUE, param=TRUE){
# a tip to show value=0 on the log plot
sim=out$sim
plot(sim$day,sim$XSEV, type=typel,lty=1, lwd=3, xlab="Time after first infection (day)", ylim=c(0,10^30),ylab="X (nb of infectious sites)")
if(all){
  lines(sim$day,sim$XLAT, type=typel,lty=1, lwd=2, col="orange")
  lines(sim$day,sim$XINF, type=typel,lty=1,lwd=2, col="red")
  lines(sim$day,sim$XCTR, type=typel,lty=1,lwd=2, col="grey")
  }
if(param){
text(seq(30,100,length.out=length(out$param)),max(sim$XSEV),names(out$param),cex=0.75)
text(seq(30,100,length.out=length(out$param)),0.95*max(sim$XSEV),(out$param),cex=0.75)
}
par(ann=FALSE,new=TRUE)
plot.default(sim$day,sim$severity,axes=FALSE,type="l",ylim=c(0,1),ylab='',lty=3, lwd=3,col="darkgrey")
axis(side=4)
mtext("severity (%)", side=4,line=2)
mtext(paste("AUDPC = ",round(sum(sim$severity,1))),side=3,line=0,cex=0.8)

legend("topleft",legend=c("XSEV","XLAT","XINF","XCTR","severity"),lty=c(1,1,2,1,3),
lwd=c(2,2,2,2,3),col=c("black","orange","red","grey","darkgrey"),
cex=0.6, ncol = 2)
return(invisible(out))
}
################################################################################
#' @title Plot output of a Classical SEIR model for plant disease
#' @description Plot the output of the Zadoks classical SEIR model for plant disease.
#' @param out : output of the zadoks.original.model
#' @param typel : type of plot (default : s)
#' @param all : if all=true (default), plot all the state variable
#' @param param : if param (default), add the values of param on the plot
#' @return plot
#' @seealso \code{\link{zakoks.original.model}}
#' @export
graph_epid=function(out,typel="s",all=TRUE, param=TRUE){
# a tip to show value=0 on the log plot
sim=out$sim
sim[sim==0]=0.5
plot(sim$DACE,sim$XSEV,log = "y", type=typel,lty=1, lwd=3,ylim=c(0.1,10^14),yaxt = "n", xlab="Time after first infection (day)", ylab="X (nb of infectious sites)")
axis(2, at=c(0.5,1,100,10^4,10^6,10^8,10^10,10^12), labels=c("0","1","100","10^4","10^6","10^8","10^10","10^12"))
if(all){
  lines(sim$DACE,sim$XCTR, type=typel,lty=1, lwd=1, col="darkgrey")
  lines(sim$DACE,sim$XINF, type=typel,lty=2,lwd=2)
  }
if(param){
text(seq(5,30,length.out=length(out$param)),10^14,names(out$param),cex=0.75)
text(seq(5,30,length.out=length(out$param)),10^13.5,(out$param),cex=0.75)
}
}

# end of file
