LP.smooth <-
function(CR,k, method ="BIC"){ #--CR just denotes the c vector.
  CR.s <- sort(CR^2,decreasing=TRUE,index=TRUE)$x
  aa <- rep(0,length(CR.s))
  if(method == "BIC") {penalty <- log(k)
		} else {
		penalty <- 2}	
   aa[1] <- CR.s[1] - penalty/k
   if(aa[1]< 0){ return(rep(0,length(CR)))  }
   ####Needed an additional if statement to handle m =1 significant
   if(length(CR) > 1){
		for(i in 2: length(CR.s)){
		aa[i] <- aa[(i-1)] + (CR.s[i] - penalty/k)
		}
		}
  CR[CR^2<CR.s[which(aa==max(aa))]] <- 0
return(CR)
}
