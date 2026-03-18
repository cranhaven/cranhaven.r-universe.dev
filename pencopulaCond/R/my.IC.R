my.IC <- function(penden.env,temp=FALSE) {
  if(!temp) {
  aa <- try(solve(-get("Derv2.pen",penden.env)))
  #aa<- my.positive.definite.solve(-get("Derv2.pen",penden.env))
  #browser()
  eps <- 1e+16
  #i <- 1
  if(class(aa)=="try-error") aa<- my.positive.definite.solve(-get("Derv2.pen",penden.env))
  #  aa <- try(solve(get("Derv2.pen",penden.env)+diag(i*eps,dim(get("Derv2.pen",penden.env))[1])))
  #  i <-  i+1
  #}
  if(get("base",penden.env)=="B-spline") mytrace <- sum(diag(aa%*%(-get("Derv2.cal",penden.env))))
  if(get("base",penden.env)=="Bernstein") mytrace <- get("DD",penden.env)

  assign("AIC",-2*get("log.like",penden.env)+2*mytrace,penden.env)
  assign("cAIC",get("AIC",penden.env)+(2*mytrace*(mytrace+1))/(get("n",penden.env)-mytrace-1),penden.env)
  assign("BIC",-2*get("log.like",penden.env)+mytrace*log(get("n",penden.env)),penden.env)
  assign("mytrace",mytrace,penden.env)
  }

  if(temp) {
  aa <- try(solve(-get("Derv2.pen.temp",penden.env)))
  #aa<- my.positive.definite.solve(-get("Derv2.pen.temp",penden.env))
  eps <- 1e+08
  i <- 1
  if(class(aa)=="try-error") aa<- my.positive.definite.solve(-get("Derv2.pen.temp",penden.env))
  #  browser()
  #  aa <- try(solve(get("Derv2.pen.temp",penden.env)+diag(i*eps,dim(get("Derv2.pen.temp",penden.env))[1])))
  #  i <-  i+1
  #}
  if(get("base",penden.env)=="B-spline") mytrace <- sum(diag(aa%*%(-get("Derv2.cal.temp",penden.env))))
  if(get("base",penden.env)=="Bernstein") mytrace <- get("DD",penden.env)

  assign("AIC.temp",-2*get("log.like.temp",penden.env)+2*mytrace,penden.env)
  assign("cAIC.temp",get("AIC.temp",penden.env)+(2*mytrace*(mytrace+1))/(get("n",penden.env)-mytrace-1),penden.env)
  assign("BIC.temp",-2*get("log.like.temp",penden.env)+mytrace*log(get("n",penden.env)),penden.env)
  assign("mytrace",mytrace,penden.env)
  }
}
