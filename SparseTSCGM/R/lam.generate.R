
lam.generate <- function(model=c("ar1","ar2"), nlambda=NULL, xty=xty, xtx=xtx, yty=yty,T=T, n=n)
{
   #if(q <= 50){
    if(model == "ar1") {
          SX <- xtx/(n*T)
          mSX <- glasso(SX,0.05,penalize.diagonal=FALSE)
          SXi <- mSX$wi
          SS =(yty)/(n*T)
          SS = cov2cor(SS)
          SAs = xty/(n*T)
          SA = SAs %*% SXi
         }
      if(model == "ar2") {
          SX <- xtx/(n*T)
          mSX <- glasso(SX,0.05,penalize.diagonal=FALSE)
          SXi <- mSX$wi
          SS =(yty)/(n*T)
          SS = cov2cor(SS)
          SAs = xty/(n*T)
          SA =  SXi %*% SAs
         }
      lambda <-  lambda.seq(SS=SS,SA=SA, nlambda=nlambda)
      lam1 <- round(lambda$lambda1,3) 
      lam2 <- round(lambda$lambda2,3)
      lam2 <- round(lam2/max(lam2),3)
      nlambda <- lambda$nlambda
      lowr=nlambda-(nlambda-2)
      upr =nlambda-2
      #lowr1=nlambda-(nlambda-2)
      #upr1 =nlambda-2
      lam1 = round(seq(lam1[lowr],lam1[upr], length=nlambda), 3) 
      lam2 = round(seq(lam2[lowr],lam2[upr], length=nlambda), 3)   
    
    out.lam = list(lam1=lam1, lam2=lam2)
    return(out.lam)
}

