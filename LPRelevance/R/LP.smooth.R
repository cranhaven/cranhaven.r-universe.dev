LP.smooth <-
function(LP.tab,n,method='BIC'){  
  CR<-LP.tab[1,]/(sqrt(n)*LP.tab[2,])
  CR.s <- sort(CR^2,decreasing=TRUE,index=TRUE)$x
  aa <- rep(0,length(CR.s))
  penalty <- log(n)
  if(method=="AIC"){ penalty <- 2}
  if(method=="BIC"){ penalty <- log(n)}
  aa[1] <- CR.s[1] - penalty/n
  if(aa[1]< 0){ return(rep(0,length(CR)))  }
  if(length(aa)==1){ return(CR) }
  for(i in 2:length(CR.s)){
    aa[i] <- aa[(i-1)] + (CR.s[i] - penalty/n)
  }
  CR[CR^2<CR.s[which(aa==max(aa))]] <- 0
  CR<-sqrt(n)*LP.tab[2,]*CR
  return(CR)
}
