simfam <-
function(N.fam, design="pop", variation="none", interaction=FALSE, depend=NULL, 
         base.dist="Weibull", frailty.dist=NULL, base.parms, vbeta, 
         allelefreq=c(0.02, 0.2), dominant.m=TRUE, dominant.s=TRUE, mrate=0, hr=0, 
         probandage=c(45, 2), agemin = 20, agemax = 100)
{
	
if(!is.element(variation, c("none", "frailty", "secondgene"))) stop("Unrecognized variation; variation should be one of none, frailty, or secondgene")

if(agemin > 70) warning("agemin should be less than 70.")
if(agemin >= agemax) stop("agemax should be greater than agemin.")

if(!is.element(base.dist, c("Weibull", "loglogistic", "Gompertz", "lognormal", "logBurr"))) 
  stop("Unrecognized base.dist; base.dist should be one of \"Weibull\", \"loglogistic\", \"Gompertz\", \"lognormal\", or \"logBurr\". ")
  
if(variation=="frailty"){
  if(!any(frailty.dist==c("lognormal",  "gamma"))) stop("Unrecognized frailty distribution; frailty.dist should be either \"gamma\" or \"lognormal\". ")
  if(is.null(depend)) stop("depend should be specified.")
  if(depend <= 0) stop("depend should be > 0")
}
else{
  if(any(frailty.dist==c("lognormal",  "gamma")) ) stop("Variation should be specified as variation = \"frailty\".")
  if(!is.null(frailty.dist) ) stop("frailty.dist should be specified only with variation=\"frailty\" ")
  if(!is.null(depend)) stop("depend should be specified only with variation=\"frailty\" ")
}
  
if(!is.element(design, c("pop","pop+","cli","cli+","twostage"))) stop("Unrecognized design; should be one of pop, pop+, cli, cli+ or twostage")  

nvbeta = 2+(variation=="secondgene")+interaction
if(length(vbeta) != nvbeta) stop(paste("vbeta should be a vector of length", nvbeta))

if(base.dist=="logBurr" & length(base.parms)!=3) stop("parms should be a vector of length 3")
else if(base.dist!="logBurr" & length(base.parms)!=2) stop("parms should be a vector of length 2")

if(design=="pop") {affectnum=1; m.carrier=0}
if(design=="pop+"){affectnum=1; m.carrier=1}
if(design=="cli") {affectnum=3; m.carrier=0}
if(design=="cli+"){affectnum=3; m.carrier=1}
if(design=="twostage"){
	affectnum = 2
	m.carrier = 0 #proband is not necessary to be a carrier.
	if(hr==0) stop("Please specify the sampling rate of high risk families (0 < hr < 1)")
	else if(hr>1 | hr <0) stop("hr should be between 0 and 1 (0 < hr < 1)")
	}

dat <- data.frame(familyDesign(n=N.fam, affectnum=affectnum, m.carrier=m.carrier, 
        base.dist=base.dist, frailty.dist=frailty.dist,
        dominant.m=dominant.m, dominant.s=dominant.s, interaction=interaction, depend=depend, vbeta=vbeta, 
        parms=base.parms, variation=variation, allelefreq=allelefreq, 
        mrate=mrate, probandage=probandage, agemin=agemin, agemax=agemax))

if(hr==0) dat$weight <- 1 #one stage sampling
else { # Two stage sampling 
	if(design!="twostage") stop ("hr can be used only with twostage design" )
  en.hr <- round(hr*N.fam)  # expected number of high risk families 
  en.lr <- N.fam - en.hr	# expected number of low risk families
  datp <- dat[dat$proband==1,]
  n.hr <- sum(datp$naff>2) # number of high risk families in the simulated families
  
	fam.id <- datp$famID 
	lfam.id <- fam.id[datp$naff<3] # family IDs of low risk families
	hfam.id <- fam.id[datp$naff>2] # family IDs of high risk families 

	if(length(lfam.id) < en.lr)	id <- lfam.id 
	else id <- sample(lfam.id, en.lr, replace=FALSE) # sampled low risk families
	n.more <- ifelse(en.hr < n.hr, 0, en.hr-n.hr)
	# dat2: additinal high risk families to generate

	dat1 <- dat[is.element(dat$famID, c(id, hfam.id)), ]
	dat2 <- data.frame(familyDesign(n=n.more, affectnum=3, m.carrier=m.carrier,
			base.dist=base.dist, frailty.dist=frailty.dist,
			dominant.m=dominant.m, dominant.s=dominant.s, interaction=interaction, depend=depend, vbeta=vbeta,
      parms=base.parms, variation=variation, allelefreq=allelefreq, 
      mrate=mrate, probandage=probandage, agemin=agemin, agemax=agemax))
	
	dat <- rbind(dat1, dat2)
	samrate <- en.lr/(N.fam-n.hr)/(en.hr/n.hr) 
	# samrate=sampling rate for low risk families when we fix sampling rate for high risk as 1
	# Eg: Suppose we want to include 30% high risk families i.e. HR: LR = 30:70
	# From random sampling, HR:LR ratio is 15:85
	# As we fix sampling rate for HR families as 1, the sampling rate for LR families should be (70/30) / (85/15) 

	dat$weight <- ifelse(dat$naff>2, 1, samrate) 
	# weight=1 for HR families and Weight=1/samrate for LR families

} 

#dat <- dat[dat$currentage>agemin, ]
#fsize <- aggregate(dat$time, by=list(dat$famID), length)[,2]
#dat$fsize <- rep(fsize, fsize)

class(dat)<-c("simfam","data.frame")
attr(dat, "design") <- design
attr(dat, "variation") <- variation
attr(dat, "interaction") <- interaction
attr(dat, "frailty.dist") <- frailty.dist
attr(dat, "base.dist") <- base.dist
attr(dat, "base.parms") <- base.parms
attr(dat, "vbeta") <- vbeta
attr(dat, "n.fam") <- N.fam
attr(dat, "agemin") <- agemin
return(dat)
}
