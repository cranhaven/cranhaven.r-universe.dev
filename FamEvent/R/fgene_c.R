fgene_c <-
function(base.dist, frailty.dist, depend, affage, affsex, interaction, variation="secondgene", parms, vbeta, alpha,
          pg=0, m.carrier=0, dominant.m=TRUE, aq){
# returns 2x3 matrix , first row for the major gene, 
#	second row for the second gene
  base.dist1 <- base.dist[1]
  base.dist2 <- base.dist[2]

  vbeta1 <- vbeta[[1]]
  vbeta2 <- vbeta[[2]]
  parms1 <- parms[[1]]
  parms2 <- parms[[2]]

pAA <- pAa <- paa <- 0
AAq <- Aaq <- aaq <- 0
AAq[1] <- Pgene(1, pg=pg[1], a.freq=aq[1])
Aaq[1] <- Pgene(2, pg=pg[1], a.freq=aq[1])
aaq[1] <- Pgene(3, pg=pg[1], a.freq=aq[1])

if(variation=="secondgene"){
  if(pg[1]==0) pg[2]<-0
  AAq[2] <- Pgene(1, pg=pg[2], a.freq=aq[2])
  Aaq[2] <- Pgene(2, pg=pg[2], a.freq=aq[2])
  aaq[2] <- Pgene(3, pg=pg[2], a.freq=aq[2])
  Ft <- matrix(0, ncol=2, nrow=2)
  for(i in c(0,1)) for(j in c(0,1)) {
    
    if(interaction[1]) xbeta1 <- affsex*vbeta1[1] + i*vbeta1[2] + affsex*i*vbeta1[3]+ j*vbeta1[4]
    else xbeta1 <- affsex*vbeta1[1] + i*vbeta1[2] + j*vbeta1[3]
    if(interaction[2]) xbeta2 <- affsex*vbeta2[1] + i*vbeta2[2] + affsex*i*vbeta2[3]+ j*vbeta2[4]
    else xbeta2 <- affsex*vbeta2[1] + i*vbeta2[2] + j*vbeta2[3]
    
    Ft[(i+1), (j+1)] <- 1-surv_dist_c(affage, base.dist=base.dist, parms=parms, xbeta=c(xbeta1, xbeta2), alpha=alpha, cuts=NULL, res=0)
      #pen1_c(affage, status=1, base.dist=base.dist, parms=parms, xbeta=c(xbeta1, xbeta2), alpha=alpha)
  
  }
  if(!dominant.m){
    pAA[1] <- Ft[2,2]*AAq[1]*(AAq[2]+Aaq[2]) + Ft[2, 1]*AAq[1]*aaq[2]
    pAa[1] <- Ft[1,2]*Aaq[1]*(AAq[2]+Aaq[2]) + Ft[1, 1]*Aaq[1]*aaq[2]
    paa[1] <- Ft[1,2]*aaq[1]*(AAq[2]+Aaq[2]) + Ft[1, 1]*aaq[1]*aaq[2]
    pAA[2] <- Ft[2,2]*AAq[2]*AAq[1]+Ft[1,2]*AAq[2]*(Aaq[1]+aaq[1])*(1-m.carrier)
    pAa[2] <- Ft[2,2]*Aaq[2]*AAq[1]+Ft[1,2]*Aaq[2]*(Aaq[1]+aaq[1])*(1-m.carrier)
    paa[2] <- Ft[2,1]*aaq[2]*AAq[1]+Ft[1,1]*aaq[2]*(Aaq[1]+aaq[1])*(1-m.carrier)
    }
 else{ 
    pAA[1] <- Ft[2,2]*AAq[1]*(AAq[2]+Aaq[2]) + Ft[2,1]*AAq[1]*aaq[2]
    pAa[1] <- Ft[2,2]*Aaq[1]*(AAq[2]+Aaq[2]) + Ft[2,1]*Aaq[1]*aaq[2]
    paa[1] <- Ft[1,2]*aaq[1]*(AAq[2]+Aaq[2]) + Ft[1,1]*aaq[1]*aaq[2]
    pAA[2] <- Ft[2,2]*AAq[2]*(AAq[1]+Aaq[1]) + Ft[1,2]*AAq[2]*aaq[1]*(1-m.carrier)
    pAa[2] <- Ft[2,2]*Aaq[2]*(AAq[1]+Aaq[1]) + Ft[1,2]*Aaq[2]*aaq[1]*(1-m.carrier)
    paa[2] <- Ft[2,1]*aaq[2]*(AAq[1]+Aaq[1]) + Ft[1,1]*aaq[2]*aaq[1]*(1-m.carrier)
 }
  return( cbind(pAA, pAa, paa)/c(pAA+pAa+paa))
}# close if variation=="secondgene"
else { # if (variation == "frailty" | "Null")

  Ft <- 0
  for(i in c(0,1)){
    if(interaction[1]) xbeta1 <- affsex*vbeta1[1] + i*vbeta1[2] + affsex*i*vbeta1[3]
    else xbeta1 <- affsex*vbeta1[1] + i*vbeta1[2] 
    if(interaction[2]) xbeta2 <- affsex*vbeta2[1] + i*vbeta2[2] + affsex*i*vbeta2[3]
    else xbeta2 <- affsex*vbeta2[1] + i*vbeta2[2] 
    Ft[(i+1)] <- 1-surv_dist_c(affage, base.dist=base.dist, parms=parms, xbeta=c(xbeta1, xbeta2), alpha=alpha, cuts=NULL, res=0)
      #pen1_c(affage, status=1, base.dist=base.dist, parms=parms, xbeta=c(xbeta1, xbeta2), alpha=alpha)
  }
  
  pAA <- Ft[2]*AAq[1]
  if(dominant.m) pAa <- Ft[2]*Aaq[1]
  else pAa <- Ft[1]*Aaq[1]
  paa <- Ft[1]*aaq[1]
  return( cbind(pAA, pAa, paa)/c(pAA+pAa+paa))
  
}


}
