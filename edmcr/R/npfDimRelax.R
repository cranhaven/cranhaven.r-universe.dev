npfDimRelax <- function(A, H, FHat,d,dmax,decreaseDim=1,S,toler,Method="NLP"){
  
  for(r2 in d:dmax){
    
    rprime <- r2
    
    P <- npfConfigInit(FHat,rprime)
    P <- P*S
    PVector <- as.vector(P)
    #POptim <- npfOptim(PVector,H,A)
    POptim <- lbfgs(npfLocalMin,npfGr,vars= PVector,A=A,H=H,invisible=1)
    PSolVect <- as.vector(POptim$par)
    PSol <- matrix(PSolVect,nrow=nrow(A), byrow=FALSE)
    
    fPSol <- npffAH(A,H,PSol)
    
    while(fPSol <= toler){
      if(rprime == d){
        return(PSol)
      }else{      
        if(Method == "Linear"){
          rprime <- max(rprime - decreaseDim,d)
          PReducedDim <- cmdscale(dist(PSol),k=rprime)
        }else if(Method == "NLP"){
          #Initial Guess
          W <- cmdscale(dist(PSol),k=rprime)
          
          #Reduce Dimension
          rprime <- max(rprime - decreaseDim,d)
          
          #NLP Program
          PReducedDim <- npfNLP(W,A,H,rprime)
        }
        
        PVector <- S*as.vector(PReducedDim)
        #POptim <- npfOptim(PVector,H,A)
        POptim <- lbfgs(npfLocalMin,npfGr,vars= PVector,A=A,H=H,invisible=1)
        PSolVect <- as.vector(POptim$par)
        PSol <- matrix(PSolVect,nrow=nrow(A), byrow=FALSE)

        fPSol <- npffAH(A,H,PSol)
      }
    }
  }
  return(NULL)
}