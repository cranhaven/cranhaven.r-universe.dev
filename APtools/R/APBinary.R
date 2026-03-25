#' Estimating the AP and the AUC for Binary Outcome Data.
#'
#' \code{APBinary} This function calculates the estimates of the AP and AUC
#' for binary outcomes as well as their confidence intervals
#' using the perturbation or the nonparametric bootstrap
#' resampling method.
#'
#' @param status Binary indicator, 1 indicates case / the class of prediction interest and 0 otherwise.
#' @param marker Numeric risk score. Data can be continuous or ordinal.
#' @param cut.values risk score values to use as a cut-off for calculation of positive predictive values (PPV) and true positive fractions (TPF). The default value is NULL.
#' @param method Method to obtain confidence intervals.
#' @param alpha Confidence level.  The default level is 0.95.
#' @param B Number of resampling to obtain confidence interval. The default value is 1000.
#' @param weight Optional. The default weight is 1, same object length as the "status" and "marker" object.
#' @importFrom stats rexp
#' @importFrom stats quantile
#' @importFrom utils write.csv
#' @export APBinary
APBinary <- function(status,marker,cut.values=NULL,method="none",alpha=0.95,B=1000,weight=NULL)
{
	############Checking the Formation############

	if(length(status)!=length(marker)){
		stop("The lengths of each data are not equal!\n")
	}

    if(is.null(weight)){
        vk = rep(1,length(status))
    }else{
        if(length(weight)!=length(marker)){
            stop("The length of weight does not match!\n")
        }else{
            vk = weight
        }
    }

	data0=cbind(status,marker)
	nn<-nrow(data0)

	auc=ap=ap_event=array(0,dim=c(B+1))
	dk=status;zk=marker
	############Set Cut-Off Value############
	if(!is.null(cut.values)){
		if(!((max(cut.values)<=max(marker))&(min(cut.values)>=min(marker)))){
			cut.values=cut.values[(min(marker)<=cut.values)&(cut.values<=max(marker))]
			cat("Warning: Some cut values are out of range!\n")
		}
		if(length(cut.values)==0){cut.values=NULL;cat("Warning: No avaliable cut values!\n")}
		if(!is.null(cut.values)){
			scl=cut.values
			PPV=TPF=array(0,dim=c(length(scl),2))
			PPV[,1]=TPF[,1]=scl

			TPF[,2] = sum.I(scl,"<",zk,1*(dk==1)*vk)/sum(1*(dk==1)*vk) ## P(Z> cl|D=1)
			PPV[,2] = sum.I(scl,"<",zk,1*(dk==1)*vk)/sum.I(scl,"<",zk,vk) ## P(D=1|Z> cl)
		}
	}

	if(method=="none"){

		auc = sum((0.5*sum.I(zk,"<",zk,1*(dk==1)*vk)+0.5*sum.I(zk,"<=",zk,1*(dk==1)*vk))*(dk==0)*vk)/(sum(vk*(dk==1))*sum(vk*(dk==0)))
		ap =  sum(vk*(dk==1)*sum.I(zk,"<=",zk,1*(dk==1)*vk)/sum.I(zk,"<=",zk,vk),na.rm=T)/sum((dk==1)*vk)
		event_rate=sum(status)/nn

		if(!is.null(cut.values)){
			colnames(PPV)=c("cut.off values","PPV")
			colnames(TPF)=c("cut.off values","TPF")
			write.csv(signif(PPV,3),file=paste("APBinary_PPV.csv",sep=""))
			write.csv(signif(TPF,3),file=paste("APBinary_TPF.csv",sep=""))
			return(list(PPV=signif(PPV,3),TPF=signif(TPF,3),ap=ap,auc=auc,event_rate=event_rate))
		}
		return(list(ap=ap,auc=auc,event_rate=event_rate))
	}

	if(method=="perturbation"){
		vk1<-matrix(rexp(nn*B,1),nrow=nn,ncol=B)

		auc[1] = sum((0.5*sum.I(zk,"<",zk,1*(dk==1)*vk)+0.5*sum.I(zk,"<=",zk,1*(dk==1)*vk))*(dk==0)*vk)/(sum(vk*(dk==1))*sum(vk*(dk==0)))
		ap[1] =  sum(vk*(dk==1)*sum.I(zk,"<=",zk,1*(dk==1)*vk)/sum.I(zk,"<=",zk,vk),na.rm=T)/sum((dk==1)*vk)
		####save true value
		auc[2:(B+1)] = apply((0.5*sum.I(zk,"<",zk,1*(dk==1)*vk1)+0.5*sum.I(zk,"<=",zk,1*(dk==1)*vk1))*(dk==0)*vk1,2,sum,na.rm=T)/(apply(vk1*(dk==1),2,sum)*apply(vk1*(dk==0),2,sum))
		ap[2:(B+1)] = apply(vk1*(dk==1)*sum.I(zk,"<=",zk,1*(dk==1)*vk1)/sum.I(zk,"<=",zk,vk1),2,sum,na.rm=T)/apply((dk==1)*vk1,2,sum)
	}

	if(method=="bootstrap"){
		data_resam=array(0,dim=c(nn,ncol(data0),B+1))
		data_resam[,,1]=as.matrix(data0)
		for(k in 2:(B+1)){
			index=sample(c(1:nn),nn,replace=TRUE)
			data_resam[,,k]=as.matrix(data0[as.vector(index),])
		}
		for(k in 1:(B+1)){
			dk=data_resam[,1,k];zk=data_resam[,2,k]
			auc[k] = sum((0.5*sum.I(zk,"<",zk,1*(dk==1)*vk)+0.5*sum.I(zk,"<=",zk,1*(dk==1)*vk))*(dk==0)*vk)/(sum(vk*(dk==1))*sum(vk*(dk==0)))
			ap[k] =  sum(vk*(dk==1)*sum.I(zk,"<=",zk,1*(dk==1)*vk)/sum.I(zk,"<=",zk,vk),na.rm=T)/sum((dk==1)*vk)
		}
	}

	event_rate=sum(status)/nn
	event_rate_sd=sqrt(event_rate*(1-event_rate)/nn)


	auc_summary=array(0,dim=c(1,3))
	ap_summary=array(0,dim=c(2,3))

	#####summary of AUC#####
	auc_summary[1,1]<-auc[1]
	auc_summary[1,2]<-max(quantile(auc,(1-alpha)/2,na.rm=T),0)
	auc_summary[1,3]<-min(quantile(auc,(1+alpha)/2,na.rm=T),1)

	######summary of AP
	ap_summary[1,1]<-event_rate
	ap_summary[1,2]<-event_rate-1.96*event_rate_sd
	ap_summary[1,3]<-event_rate+1.96*event_rate_sd
	ap_summary[2,1]<-ap[1]
	ap_summary[2,2]<-max(quantile(ap,(1-alpha)/2,na.rm=T),0)
	ap_summary[2,3]<-min(quantile(ap,(1+alpha)/2,na.rm=T),1)

	colnames(auc_summary)<-c("Point Estimate",paste("Lower Limit(a=",alpha,")",sep=""),paste("Upper Limit(a=",alpha,")",sep=""))
	rownames(auc_summary) = c("AUC")
	write.csv(signif(auc_summary,3),file=paste("APBinary_auc_summary(","method=",method,",B=",B,").csv",sep=""))

	colnames(ap_summary)<-c("Point Estimate",paste("Lower Limit(a=",alpha,")",sep=""),paste("Upper Limit(a=",alpha,")",sep=""))
	rownames(ap_summary) = c("event rate","AP")
	write.csv(signif(ap_summary,3),file=paste("APBinary_ap_summary(","method=",method,",B=",B,").csv",sep=""))

	if(!is.null(cut.values)){
		colnames(PPV)=c("cut.off values","PPV")
		colnames(TPF)=c("cut.off values","TPF")
		write.csv(signif(PPV,3),file=paste("APBinary_PPV.csv",sep=""))
		write.csv(signif(TPF,3),file=paste("APBinary_TPF.csv",sep=""))
		return(list(PPV=signif(PPV,3),TPF=signif(TPF,3),ap_summary=signif(ap_summary,3),auc_summary=signif(auc_summary,3)))
	}
	return(list(ap_summary=signif(ap_summary,3),auc_summary=signif(auc_summary,3)))
}
