.cdf.func<-function(link){
  if (link=="probit"){
    value <- function(p){pnorm(p)}
  } else if (link=="logit"){
    value <- function(p){plogis(p)}
  } else if (link=="cauchit"){
    value <- function(p){pcauchy(p)}
  } else if (link=="loglog"){
    value <-function(p){exp(-exp(-p))}
  } else if (link=="cloglog"){
    value <- function(p){1-exp(-exp(p))}
  } else {
    stop("Specified link not available.")
  }
  value
}

.invcdf.func<-function(link){
  if (link=="probit"){
    value <- function(p){qnorm(p)}
  } else if (link=="logit"){
    value <- function(p){qlogis(p)}
  } else if (link=="cauchit"){
    value <- function(p){qcauchy(p)}
  } else if (link=="loglog"){
    value <-function(p){-log(-log(p))}
  } else if (link=="cloglog"){
    value <- function(p){log(-log(1-p))}
  } else {
    stop("Specified link not available.")
  }
  value 
}


.pdf.func<-function(link){
  if (link=="probit"){
    value <- function(p){dnorm(p)}
  } else if (link=="logit"){
    value <- function(p){dlogis(p)}
  } else if (link=="cauchit"){
    value <- function(p){dcauchy(p)}
  } else if (link=="loglog"){
    value <-function(p){exp(-exp(-p))*exp(-p)}
  } else if (link=="cloglog"){
    value <- function(p){exp(-exp(p))*exp(p)}
  } else {
    stop("Specified link not available.")
  }
  value
}

.Dpdf.func<-function(link){
  if (link=="probit"){
    value <- function(p){result<- -p*dnorm(p); result[p==Inf | p==-Inf]<-0; return(result)}
  } else if (link=="logit"){
    value <- function(p){dlogis(p)*(1-2*plogis(p))}
  } else if (link=="cauchit"){
    value <- function(p){result<- -2*p*dcauchy(p)/(1+p^2); result[p==Inf | p==-Inf]<-0; return(result)}
  } else if (link=="loglog"){
    value <-function(p){result<-exp(-exp(-p))*(exp(-p)*(exp(-p)-1)); result[exp(-p)==Inf]<-0; return(result)}
  } else if (link=="cloglog"){
    value <- function(p){result<-exp(-exp(p))*exp(p)*(1-exp(p)); result[exp(p)==Inf]<-0; return(result)}
  } else {
    stop("Specified link not available.")
  }
  value
}

.DDpdf.func<-function(link){
  if (link=="probit"){
    value <- function(p){result<-(p^2-1)*dnorm(p);result[p==Inf | p==-Inf]<-0; return(result)}
  } else if (link=="logit"){
    value <- function(p){dlogis(p)*(1-2*plogis(p))^2-2*dlogis(p)^2}
  } else if (link=="cauchit"){
    value <- function(p){result<-(dcauchy(p)/(p^2+1)^4)*(6*p^4+8*p^2-2);result[p==Inf | p==-Inf]<-0; return(result)}
  } else if (link=="loglog"){
    value <- function(p){result<-exp(-exp(-p))*(exp(-3*p)-3*exp(-2*p)+exp(-p));result[p==-Inf]<-0; return(result)}
  } else if (link=="cloglog"){
    value <- function(p){result<-exp(-exp(p))*(exp(3*p)-3*exp(2*p)+exp(p));result[p==Inf]<-0; return(result)}
  } else {
    stop("Specified link not available.")
  }
  value
}