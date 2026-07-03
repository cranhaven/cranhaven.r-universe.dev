\name{balance8pars}

\docType{data}

\alias{balance8pars}

\title{Parameter estimates of models for the balance8 data set}

\description{

	Parameter estimates of hidden Markov models with 3-8 states for the balance8 data. 
	
}

\usage{data(balance8pars)}
\format{
  A (named) list with parameter estumates for models with 3-8 states. Each 
  element contains additional attributes such as "message" reflecting the 
  corresponding slots of the depmix.fitted object from which the parameters
  were extracted (using getpars).

}

\details{

The list is generated with the following code: 

    \preformatted{
data(balance8)
multstart <- function(model, nr=10, initIters=10, verbose=TRUE) {
  llbest <- as.numeric(logLik(model))
  bestmodel <- model
  for(i in 1:nr) {
    fmod <- fit(model, emcontrol=em.control(maxit=initIters)) 
	if(verbose) print(paste(i,": ", logLik(fmod)))
    if(logLik(fmod) > llbest) {
      llbest <- logLik(fmod)
      bestmodel <- fmod
    }
  }
  bestmodel <- fit(bestmodel, emcontrol=em.control(random.start=FALSE))
  return(bestmodel)
}

set.seed(12)

hm3id <- depmix(list(cbind(wc,wi)~1,cbind(dc,di)~1,cbind(cwc,cwi)~1,
  cbind(cdc,cdi)~1,cbind(cbc,cbi)~1), 
	data=balance8, family=list(binomial("identity"),binomial("identity"),
	binomial("identity"),binomial("identity"),binomial("identity")), 
	ntimes=rep(8,1004), ns=3,
	respst=rep(0.5,15))
	
fhm3id <- multstart(hm3id)

hm4id <- depmix(list(cbind(wc,wi)~1,cbind(dc,di)~1,cbind(cwc,cwi)~1,
  cbind(cdc,cdi)~1,cbind(cbc,cbi)~1), 
	data=balance8, family=list(binomial("identity"),binomial("identity"),
	binomial("identity"),binomial("identity"),binomial("identity")), 
	ntimes=rep(8,1004), ns=4,
	respst=rep(0.5,20))
	
fhm4id <- multstart(hm4id)


hm5id <- depmix(list(cbind(wc,wi)~1,cbind(dc,di)~1,cbind(cwc,cwi)~1,
  cbind(cdc,cdi)~1,cbind(cbc,cbi)~1), 
	data=balance8, family=list(binomial("identity"),binomial("identity"),
	binomial("identity"),binomial("identity"),binomial("identity")), 
	ntimes=rep(8,1004), ns=5,
	respst=rep(0.5,25))
	
fhm5id <- multstart(hm5id)

hm6id <- depmix(list(cbind(wc,wi)~1,cbind(dc,di)~1,cbind(cwc,cwi)~1,
  cbind(cdc,cdi)~1,cbind(cbc,cbi)~1), 
	data=balance8, family=list(binomial("identity"),binomial("identity"),
	binomial("identity"),binomial("identity"),binomial("identity")), 
	ntimes=rep(8,1004), ns=6,
	respst=rep(0.5,30))
	
fhm6id <- multstart(hm6id)

hm7id <- depmix(list(cbind(wc,wi)~1,cbind(dc,di)~1,cbind(cwc,cwi)~1,
  cbind(cdc,cdi)~1,cbind(cbc,cbi)~1), 
	data=balance8, family=list(binomial("identity"),binomial("identity"),
	binomial("identity"),binomial("identity"),binomial("identity")), 
	ntimes=rep(8,1004), ns=7,
	respst=rep(0.5,35))
	
fhm7id <- multstart(hm7id)

set.seed(1)

hm8id <- depmix(list(cbind(wc,wi)~1,cbind(dc,di)~1,cbind(cwc,cwi)~1,
  cbind(cdc,cdi)~1,cbind(cbc,cbi)~1), 
	data=balance8, family=list(binomial("identity"),binomial("identity"),
	binomial("identity"),binomial("identity"),binomial("identity")), 
	ntimes=rep(8,1004), ns=8,
	respst=rep(0.5,40))
	
fhm8id <- multstart(hm8id)

balance8models <- list(fhm3id,fhm4id,fhm5id,fhm6id,fhm7id,fhm8id)

balance8pars <- list()
for(i in 1:length(balance8models)) {
  balance8pars[[i]] <- getpars(balance8models[[i]])
  attr(balance8pars[[i]], "message") <- balance8models[[i]]@message
  attr(balance8pars[[i]], "conMat") <- balance8models[[i]]@conMat
  attr(balance8pars[[i]], "lin.upper") <- balance8models[[i]]@lin.upper
  attr(balance8pars[[i]], "lin.lower") <- balance8models[[i]]@lin.lower
}
names(balance8pars) <- c("fhm3id","fhm4id","fhm5id","fhm6id","fhm7id","fhm8id")
}
}

\examples{
data(balance8)
data(balance8pars)
# reconstruct the list of fitted models from the parameters
balance8models <- list()
for(i in 1:length(balance8pars)) {
  # define model
  mod <- depmix(list(cbind(wc,wi)~1,cbind(dc,di)~1,cbind(cwc,cwi)~1,
    cbind(cdc,cdi)~1,cbind(cbc,cbi)~1), 
    data=balance8, family=list(binomial("identity"),binomial("identity"),
    binomial("identity"),binomial("identity"),binomial("identity")), 
    ntimes=rep(8,1004), ns=attr(balance8pars[[i]],"nstates"))
  # set the parameters to the estimated ones
  mod <- setpars(mod, balance8pars[[i]])
  # convert to a depmix.fitted object
  mod <- as(mod,"depmix.fitted")
  # set slots of depmix.fitted object
  mod@message <- attr(balance8pars[[i]],"message")
  mod@conMat <- attr(balance8pars[[i]],"conMat")
  mod@lin.upper <- attr(balance8pars[[i]],"lin.upper")
  mod@lin.lower <- attr(balance8pars[[i]],"lin.lower")
  mod@posterior <- viterbi(mod)
  # add to list of models
  balance8models[[i]] <- mod
}
names(balance8models) <- c("fhm3id","fhm4id","fhm5id","fhm6id","fhm7id","fhm8id")
}

\keyword{datasets}
