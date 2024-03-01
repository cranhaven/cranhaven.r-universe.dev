fampower <- function(N.fam, N.sim, effectsize, beta.sex, alpha=0.05, side = 2, design="pop",
variation="none", interaction=FALSE, depend=NULL, base.dist="Weibull", frailty.dist=NULL,
base.parms, allelefreq=c(0.02, 0.2), dominant.m=TRUE, dominant.s=TRUE, mrate=0, hr=0,
probandage=c(45, 2), agemin=20, agemax=100){

if(side!=1 & side!=2) stop("side should be 1 or 2.")
if(alpha < 0 | alpha >1) stop("alpha should be between 0 and 1.")
if(interaction & length(effectsize)!=2) stop("effectsize should be length of 2.")
  
vbeta <- c(beta.sex, effectsize)
w <- w2 <- numeric()
i <- 0
cat("Number of simulations: ")
while(i < N.sim){
  fam <- simfam(N.fam,design=design, variation=variation, interaction = interaction, depend=depend, base.dist=base.dist, frailty.dist=frailty.dist,
                base.parms=base.parms, vbeta, allelefreq=allelefreq, 
                dominant.m=dominant.m, dominant.s=dominant.s, mrate=mrate, hr=hr, probandage=probandage, 
                agemin=agemin, agemax=agemax)
  
  if(mrate==0){
    if(interaction) fit <- try(penmodel(Surv(time, status)~ gender + mgene + gender*mgene, cluster="famID", parms = c(base.parms, vbeta), data = fam, design=design, base.dist=base.dist))
    else fit <- try(penmodel(Surv(time, status)~ gender + mgene, cluster="famID", parms = c(base.parms, vbeta), data = fam, design=design, base.dist=base.dist))
  }
  else{
    if(interaction) fit <- try(penmodelEM(Surv(time, status)~ gender + mgene + gender*mgene, cluster="famID", parms = c(base.parms, vbeta), data = fam, design=design, base.dist=base.dist), silent=TRUE)
    else fit <- try(penmodelEM(Surv(time, status)~ gender + mgene, cluster="famID", parms = c(base.parms, vbeta), data = fam, design=design, base.dist=base.dist), silent=TRUE)
  }
    if(attr(fit, "class") != "try-error"){
    i <- i + 1
    cat(i, " ")
    est <- summary(fit)$estimates
    rownames(est)
    w[i] <- est[4, 3]
    if(interaction)  w2[i] <- est[5, 3]
      #w2[i] <- sum(est[c(4,5),1])/sqrt(sum(fit$varcov[c(4,5), c(4,5)]))
  }
}
pwr = sum(abs(w) > qnorm(1-alpha/side) ) / N.sim
if(interaction) effsize = vbeta[2]
else effsize = c(vbeta[2], vbeta[3])

cat("\nNumber of families =", N.fam, "\n")
cat(side, "sided test \n")
cat("alpha =", alpha, "\n")

if(interaction){
  pwr2 = sum(abs(w2) > qnorm(1-alpha/side) ) / N.sim
  cat("Effect size =", vbeta[2], "for main effect,", vbeta[3] , "for interaction effect \n" )
  cat("Power =", pwr, "for main effect,", pwr2 , "for interaction effect \n" )
  out = c(pwr, pwr2)
}
else{ 
  cat("Effect size =", vbeta[2] , "\n" )
  cat("Power = ",  pwr , "\n" )
  out = pwr
}

invisible(out)
}
