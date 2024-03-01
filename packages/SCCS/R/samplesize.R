
samplesize <- function(eexpo, risk, astart, aend, p, alpha=0.05, power=0.8, eage=NULL, agegrp=NULL) {
  
  if (is.null(agegrp) & !is.null(eage)){
    
    stop("Please specify age group cut points 'agegrp'")
  }
  
  if (!is.null(agegrp) & is.null(eage)){
    
    stop("Please specify age related relative incidences 'eage'")
  }
  
  
  if (length(p)!= (length(eage)+1) | length(p)!= (length(agegrp)+1)){
    
    stop("Please specify appropriate number of eage and agegrp")
  }
  
  if (sum(p)<=0 | sum(p)>1){
    
    stop("The sum of the vector/scalar 'p' must be greater than 0 and less than or equal to 1 (0, 1]")
  }
  
  eage <- c(1, eage)
  rho=eexpo
  agecupts <- c(astart, agegrp, aend)
  agegrplengths <- diff(agecupts)
  agegrplengths[length(agegrplengths)] <- agegrplengths[length(agegrplengths)]+1
  es <- agegrplengths # lengths of the age groups
  
  if (risk>=min(es)){
    
    stop("risk length must be less than the length of the shortest age group")
  }
  
  estr<- risk
  sss <- sum(eage*es)
  r <- rep(NA, length(es))
  for (i in 1:length(es))
  {
  r[i] <- (eage[i]*estr)/sss # Weighted ratio of time at risk to the over all risk period in each age group
  }
  
  pi <- (r*rho)/(r*rho + 1 - r)   # pi=probability that an event occurs in exposure risk period given the risk period is in age group j
 
  #-----------------------------------------------------------#
  # vj=# Probability that a case is exposed in age group j 
  vj <- rep(NA, length(es))
  
  vjden <- ((1-sum(p))+sum(p*(r*rho + 1 - r))) # Denominator of vj
  
  for (i in 1:length(es))
  {
  
  vj[i] <- (p[i]*(r[i]*rho + 1 - r[i]))/vjden # Probability that a case is exposed in age group j 
  
  }
  
  A <- 2*sum(vj*(pi*log(rho) - log(r*rho + 1 - r)))
  B <- (((log(rho))^2)/A)*sum(vj*pi*(1-pi))
  
  za <- round(qnorm(alpha/2, mean = 0, sd = 1, lower.tail = F, log.p = FALSE), 4)
  zb <- round(qnorm(power, mean = 0, sd = 1, lower.tail = T, log.p = FALSE), 4)
  
  n <- ((za + zb*sqrt(B))^2)/A
  #print("The required sample size is:")
  return(ceiling(n))
}
