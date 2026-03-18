new.lambda <- function(penden.env) {
  cond<-get("cond",penden.env)
  lambda <- get("lambda",penden.env)[1]
  eps <- 1
  eps2<-0.05
  epsdf <- 0
  p <- get("p",penden.env)
  calc <- TRUE
  u <- t(get("ck.val.temp",penden.env))%*%get("DDD.sum",penden.env)%*%get("ck.val.temp",penden.env)
  help2 <- get("eigen.pen.mat",penden.env)
  index <- get("index.eigen.pen.mat",penden.env)
  Utilde <- get("Utilde.eigen.pen.mat",penden.env)
  t.Utilde <- t(Utilde)
  diag.help2 <- diag(help2$values[index])
  hh <-1
  while(calc) {
    #print(lambda)
    if(hh==51) {
      assign("df.val",df.val,penden.env)
      break
    }
    Derv2(penden.env,temp.lam=TRUE,lam.fit=lambda[hh])
    aa1<-try(df.val <- sum(diag(x=solve(t.Utilde%*%(-get("Derv2.cal.temp",penden.env))%*%Utilde+lambda[hh]*diag.help2)%*%(t.Utilde%*%(-get("Derv2.cal.temp",penden.env))%*%Utilde))),silent=TRUE)
    if(get("errorDerv2",penden.env)) return(lambda[hh])
    if(class(aa1)=="try-error") df.val<-sum(diag(x=my.positive.definite.solve(t.Utilde%*%(-get("Derv2.cal.temp",penden.env))%*%Utilde+lambda[hh]*diag.help2)%*%(t.Utilde%*%(-get("Derv2.cal.temp",penden.env))%*%Utilde)))
    if(is.na(df.val)|is.nan(df.val)|is.infinite(df.val)|df.val < epsdf) {
      print("df kleiner 0")
      assign("lambda.crash",TRUE,penden.env)
      break
    }
    help.val <- abs(df.val/u - lambda[hh])
    help2 <- abs(df.val/u - lambda[1])
    if((df.val/u)<0) {
      assign("df.val",df.val,penden.env)
      if(hh==1&get("i",penden.env)>2) {
        assign("lambda.out",TRUE,penden.env)
        return(lambda)
      } 
      else assign("lambda.crash",TRUE,penden.env)
      return(lambda[hh-1])
    }
    if(help.val<(eps2*lambda[hh])) {#&!(get("i",penden.env)==2)) {
      calc <- FALSE
      lam1<-df.val/u
      assign("df.val",df.val,penden.env)
      assign("lambda.hold",FALSE,penden.env)
      return(lam1)
    }
      lambda[hh+1] <- df.val/u
      assign("lambda.temp",lambda[hh+1],penden.env)
      hh <- hh+1
    }

  return(lambda[hh])
}
