# Carrier prob conditional on the phenotype
carrierprobpheno <- function(method="data", fit=NULL, data, mode="dominant", q=0.02)
{
  
  if(sum(is.na(data$mgene))==0) stop("Mutatioin carrier statuses are all known")
  if(method=="data"){
    
    carrp <- data$mgene
    id.na <- data$indID[is.na(data$mgene)]
    mut.ca <- data$relation[data$mgene==1 & !is.na(data$mgene)]
    
    majorgene <- data$majorgene
    pAA <- ifelse(is.na(carrp), NA, ifelse(majorgene==1, 1, 0)) # P(1=AA), A is disease gene
    pAa <- ifelse(is.na(carrp), NA, ifelse(majorgene==2, 1, 0)) # P(2=Aa)
    paa <- ifelse(is.na(carrp), NA, ifelse(majorgene==3, 1, 0)) # P(3=aa)
    
#    cfam.id <- data$famID[data$proband==1 & data$mgene==1]
#    nfam.id <- data$famID[data$proband==1 & data$mgene==0]
#    i.cfam <- is.element(data$famID,cfam.id)
#    i.nfam <- is.element(data$famID,nfam.id)
    
    for(g in unique(data$relation)){
      for(s in c(0,1)){
        for(d in c(0,1)){
          # carrier families
          # carrp[i.cfam & is.na(data$mgene) & data$relation==g & data$gender==s & 
          #        data$status==d] <- mean(data$mgene[i.cfam & !is.na(data$mgene) & data$relation==g & data$gender==s & data$status==d])
          # non-carrier famiiies
          # carrp[i.nfam & is.na(data$mgene) & data$relation==g & data$gender==s & 
          #        data$status==d] <- mean(data$mgene[i.nfam & !is.na(data$mgene) & data$relation==g & data$gender==s & data$status==d])
          
          #carrp[is.na(data$mgene) & data$relation == g & data$gender == s & data$status == d] <- mean(data$mgene[!is.na(data$mgene) & data$relation == g & data$gender == s & data$status == d])
          pAA[is.na(data$mgene) & data$relation == g & data$gender == s & data$status == d] <- mean(data$majorgene[!is.na(data$mgene) & data$relation == g & data$gender == s & data$status == d]==1)
          pAa[is.na(data$mgene) & data$relation == g & data$gender == s & data$status == d] <- mean(data$majorgene[!is.na(data$mgene) & data$relation == g & data$gender == s & data$status == d]==2)
          paa[is.na(data$mgene) & data$relation == g & data$gender == s & data$status == d] <- mean(data$majorgene[!is.na(data$mgene) & data$relation == g & data$gender == s & data$status == d]==3)
          
        }
      }
    }
    
    if(mode == "dominant") carrp <- pAA+pAa
    else carrp <- pAA
    
    carrp[is.na(carrp)] <- 0
    data$carrp.pheno <- carrp
    data$pAA <- pAA
    data$pAa <- pAa
    data$paa <- paa
    
  }  # close for method=="data"
  else if(method=="model"){
    if(is.null(fit)) stop("fit should be specified.")
    theta <- fit$estimates 
    base.dist <- attr(fit, "base.dist")
    agemin <- attr(fit, "agemin")
    nbase <- attr(fit, "nbase")
    cuts <- attr(fit, "cuts")
    formula <- attr(fit, "formula")
    gvar <- attr(fit, "gvar")
    data <- attr(fit, "data")
    
    Y <- attr(fit, "Y")
    X <- attr(fit, "X")
    var.names <- colnames(X)
    
    X0 <- X1 <- X
    X0[, gvar] <- 0
    X1[, gvar] <- 1
    xbeta <- c(X%*%theta[-c(1:nbase)])
    xbeta0 <- c(X0%*%theta[-c(1:nbase)])
    xbeta1 <- c(X1%*%theta[-c(1:nbase)])
    
    time0 <- Y[,1] - agemin
    cuts0 <- cuts - agemin
    status <- Y[,2]
    
    parms <- exp(theta[1:nbase])
    if(base.dist=="lognormal") parms[1] <- theta[1]

    p.geno <- data$carrp.geno
    if(is.null(p.geno)) {
      p.geno <- carrierprobgeno(data=data, method="mendelian", mode=mode, q=q)$carrp.geno
      data$carrp.geno <- p.geno
    }
    
    p1 <- cprob(theta, X1, time0, status, p=p.geno, base.dist=base.dist, cuts=cuts0, nbase=nbase)
    p0 <- cprob(theta, X0, time0, status, p=1-p.geno, base.dist=base.dist, cuts=cuts0, nbase=nbase)
    carrp <- p1/(p1+p0) #P(x=1|Xp, y)=P(y|x=1)*P(x=1|Xp)/(p1+p0) for EM
    
    carrp[!is.na(data$mgene)] <- data$mgene[!is.na(data$mgene)]
    carrp[is.na(carrp)] <- 0
    data$carrp.pheno <- carrp
#    data$pAA <- pAA
#    data$pAa <- pAa
#    data$paa <- paa
    
  } # close for method=="model

  
  return(data)

}