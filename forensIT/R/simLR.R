#' @title Simulate LR
#' @description Simulate LR
#' @param lprobg_ped list of probability distributions
#' @param numSim number of simulations
#' @param epsilon small number to avoid log(0)
#' @param bplot boolean to plot
#' @param bLRs boolean to return LRs
#' @param seed seed
#' @importFrom reshape2 melt
#' @importFrom graphics abline barplot layout legend par
#' @importFrom stats aggregate ecdf
#' @importFrom utils write.table
#' @return LRs
#' @export

simLR <- function(lprobg_ped,numSim=10000,epsilon=1e-20,bplot=FALSE,bLRs=FALSE,seed=123457){

  if(!is.null(seed)) set.seed(seed)
  a <- lapply(lprobg_ped,function(x){
    p0 <- x$pop
    p1 <- x$bnet
    i0 <- which(p1==0)
    if(length(i0)>1){
      p1[i0] <- epsilon
      p1 <- p1/sum(p1)
    }
    lr1 <- sample(log10(p1/p0),size = numSim,replace = TRUE,prob = p1)
    lr0 <- sample(log10(p1/p0),size = numSim,replace = TRUE,prob = p0)
    return(cbind(H0=lr0,H1=lr1))#,nx=sum(lr0<log10(epsilon)/2)))
  })
  
  res <- 0     #por aca: calcular NX AL CALCULAR res? EN LUGAR DE ARRIBA
  nx  <- rep(0,nrow(a[[1]]))
  for(i in seq_along(a)){
    res <- res + a[[i]]
    bx  <- a[[i]][,'H0']<log10(epsilon)/2
    nx  <- nx  + as.numeric(bx) 
  }
  nx <- table(nx)
  #calculo la posicion de los picos de H0
  mux <-c()
  for(i in seq_along(nx)){
    start <- ifelse(i==1,1,sum(nx[1:(i-1)])+1)
    stop  <- sum(nx[1:i])
    mux   <- c(mux,mean(sort(res[,'H0'],decreasing=TRUE)[start:stop]))
  }
  names(mux) <- paste0('n',names(nx))
  
  nx  <- paste(nx,collapse=":")
  mux <- paste(signif(mux,4),collapse=":")
  
  pfp <- 1 - ecdf(res[,"H0"])(0)
  pfn <- ecdf(res[,"H1"])(0)
  ptp <- 1 - ecdf(res[,"H1"])(0)
  ptn <- ecdf(res[,"H0"])(0)
  
  mcc <- (ptp*ptn-pfp*pfn)/sqrt((ptp+pfp)*(ptp+pfn)*(ptn+pfp)*(ptn+pfn))
  f1  <- 2* ptp/(2*ptp+pfp+pfn)  
    
  if(FALSE){#cuento (chotamente) poblacion de picos
    
    if(FALSE){
      h0<-res[,'H0']
      nmax <- ceiling(min(h0)/log10(epsilon))
      nx <- c()
      for(i in 1:nmax){
        min <- ifelse(i==nmax,min(h0),i*log10(epsilon)/2)
        max <- ifelse(i==1,20,(i-1)*log10(epsilon)/2)
        nx <- c(nx,sum(h0>=min & h0<max))
      }
      nx <- paste(nx,collapse=":")
      
    }else{
      # Function to find local peaks and valleys
      
      h<-hist(h0)
      fp<-findPeaksValleys(h$counts) 
      nx <- c()
      for(i in seq_along(fp$valleys)){
        min <- ifelse(i==1,min(h0),h$mids[fp$valleys[i-1]])
        max <- h$mids[fp$valleys[i]]
        nx <- c(nx,sum(h0>=min & h0<max))
      }
      nx <- c(nx,sum(h0>=max)) #agrego el ultimo
      nx <- paste(nx,collapse=":")
    }
  }
  value <- Var2 <- NULL
  reslong <- reshape2::melt(res)
  if(bplot){
    p <- ggplot(reslong,aes(x=value,col=Var2)) +
      geom_density() +
      xlab('Log10(LR)')
#      annotate("label", x = 0, y = 0.25,  label = paste('pfp:',signif(pfp,4),
    #                                                       ' pfn:',signif(pfn,4))) 
    if(bLRs){
      return(list(fig=p,metrics=c(tp=ptp,fp=pfp,tn=ptn,fn=pfn,mcc=mcc,f1=f1),LRs=reslong,nH0peaks=nx,muH0peaks=mux))
    }else{
      return(list(fig=p,metrics=c(tp=ptp,fp=pfp,tn=ptn,fn=pfn,mcc=mcc,f1=f1,nH0peaks=nx,muH0peaks=mux)))
    }
  }else{
    if(bLRs){
      return(list(metrics=c(tp=ptp,fp=pfp,tn=ptn,fn=pfn,mcc=mcc,f1=f1),LRs=reslong,nH0peaks=nx,muH0peaks=mux))  
    }else{
      return(list(metrics=c(tp=ptp,fp=pfp,tn=ptn,fn=pfn,mcc=mcc,f1=f1),nH0peaks=nx,muH0peaks=mux))
    }
  }
}
