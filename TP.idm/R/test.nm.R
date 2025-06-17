test.nm <-
function(data, s, t="last"){
    ## conditions:
    if(missing(data)) stop("Argument 'data' is missing with no default")
    if(!is.data.frame(data)) stop("Argument 'data' must be a data.frame")
    data<-data[,c("time1","event1","Stime","event")]
    if(length(names(data))!=4) stop("'data' must have 4 variables")
    if(sum(c("time1","event1","Stime","event")==names(data))!=4) stop("'data' must contain the rigth variables")
    if(!is.numeric(data$time1)) {stop ("'time1' must be a numeric vector, which describes the time to firt event")}
    if(!is.numeric(data$Stime)) {stop ("'Stime' must be a numeric vector, which describes the survival or censoring time")}
    n=length(data[,1])
    for (i in 1:n){
      if(data$event1[i]==0 & data$event[i]==1) {stop("Argument 'event' must be 0 or argument 'event1' must be 1")}
    }
    
    mint<-min(data$time1[data$event1==1])
    
    if(s<0){stop("Argument 's' must be 0")} 
    if(s>0 & s<=mint){stop("Argument 's' must be 0 or greater than min(data$time1)=",mint)}
    if(t<=s){stop("Argument 't' must be greater than 's'")}
    
    
    # states:
    states<-c("1","2","3")
    
    # ns== number of states
    ns<-length(states)
    
    # non-absorbing states
    tr.states <- states[!states=="3"]
    
    
    # estimate both methods
    
    # method 'NM'
    # prepare data set and NM method:
    NM.est<- fun.NM(data, states, s, t, tr.states)
    
    # method 'AJ'
    # start time ==0
    data$start.time<-0
    
    # initial probabilities for each initial state
    i.state<-integer(length(data[,1]))
    for(i in 1:length(data[,1])){
      if(data$start.time[i]==0) i.state[i]=1
      if(data$start.time[i]==data$time1[i]) i.state[i]=2
      if(data$start.time[i]==data$Stime[i]) i.state[i]=3
    }
    i.state <- factor(i.state,levels=states,labels=states)
    
    initial.probs <- prop.table(table(i.state))
    
    
    # prepare data set to compute AJ method:
    ds.prep.AJ <- prep.data.AJ(data, states, tr.states)
    
    # reduces to event times:
    ds.event.AJ <- prep.data.event.AJ(ds.prep.AJ$dNs, ds.prep.AJ$Ys, ds.prep.AJ$sum_dNs, states, tr.states)
    event.times <- as.numeric(as.character(rownames(ds.event.AJ$dNs)))
    
    # AJ estimator:
    AJ.est <- fun.AJ(ns,states, ds.event.AJ$dNs, ds.event.AJ$Ys, ds.event.AJ$sum_dNs,
                     s, t,  event.times, initial.probs)
    
    ## plot:
    if(s>0){
      chosen.tr<- c("1 2", "1 3", "2 2")      
      
      par(mfrow=c(1,length(chosen.tr)))
      
      if(length(AJ.est$e.times.id_tr)!=length(NM.est$e.times)){
        for(i in 1:length(chosen.tr)){
          tit<-paste("p", chosen.tr[[i]])
          plot(NM.est$all.est[-1,,chosen.tr[[i]]], AJ.est$all.est[,,chosen.tr[[i]]], xlab="NM estimator", ylab="AJ estimator", main=tit)
          abline(0,1, col="red") 
        }
      } else {
        
        for(i in 1:length(chosen.tr)){
          tit<-paste("p", chosen.tr[[i]])
          plot(NM.est$all.est[,,chosen.tr[[i]]], AJ.est$all.est[,,chosen.tr[[i]]], xlab="NM estimator", ylab="AJ estimator", main=tit)
          abline(0,1, col="red") 
        }        
      }     
      title("Diagnostic plot", outer=TRUE,  line=-1)      
    }
    
    # results:    
    cat("Parameters:","\n")
    cat("s=", s, "\n")
    cat("t=", AJ.est$t, "\n")
    
    cat("\n")
    if(s==0){
      cat("Markov assumption is not relevant for the estimation of occupation probabilities (s==0).", "\n", "\n")                
    }        
  }
