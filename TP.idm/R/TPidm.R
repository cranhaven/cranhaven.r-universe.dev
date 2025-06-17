TPidm <-
function(data, s, t="last", cov=NULL, CI=TRUE, level=0.95, ci.transformation="linear", method="NM"){
    ## conditions:
    if(missing(data)) stop("Argument 'data' is missing with no default")
    if(!is.data.frame(data)) stop("Argument 'data' must be a data.frame")
    if (is.null(cov)){
      data<-data[,c("time1","event1","Stime","event")]
      if(length(names(data))!=4) stop("'data' must have 4 variables")
      if(sum(c("time1","event1","Stime","event")==names(data))!=4) stop("'data' must contain the rigth variables")      
    } else{
      data<-data[,c("time1","event1","Stime","event",cov)]
      if(length(names(data))!=5) stop("'data' must have 5 variables")
      if(sum(c("time1","event1","Stime","event",cov)==names(data))!=5) stop("'data' must contain the rigth variables")
      cc<-match(cov,names(data))
      if(!is.factor(data[,cc])) stop("In cov a factor is needed")
    }
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
    
    # choose method
    ci.transf.type<-c("linear","log","log-log","cloglog")
    q<-charmatch(ci.transformation,ci.transf.type,nomatch=0)
    # charmatch function returns:
    # value 0 (if ci.transformation != c("linear","log","log-log","cloglog") 
    if (q==0)
      stop("ci.transformation should be 'linear', 'log', 'log-log' or 'cloglog'")
    
    
    # states:
    states<-c("1","2","3")
    
    # ns== number of states
    ns<-length(states)
    
    # non-absorbing states
    tr.states <- states[!states=="3"]
    
    if (is.null(cov)){
      # choose method
      method.type<-c("NM","AJ")
      m<-charmatch(method,method.type,nomatch=0)
      # charmatch function returns:
      # value 0 (if method != c(NM,AJ)) 
      # value 1 (if method = NM) or value 2 (if method = AJ)
      if (m==0)
        stop("method should be 'NM' or 'AJ'")
      if (m==1){
        # the method is 'NM'
        # prepare data set and NM method:
        NM.est<- fun.NM(data, states, s, t, tr.states)
        
        if(CI==TRUE){
          # var using Titman method:
          variances<- var.NM(data, ns, states, tr.states, s, t, NM.est$all.probs)   
          
          # ci:
          ci<- ci.NM(s, t, NM.est$all.probs, variances$var.nm, level, ci.transformation, NM.est$e.times)
          
          # results:
          res <- list(
            # states information:
            cov=cov,
            method=method,s=s,t=NM.est$t,states=states, ns=ns, tr.states=tr.states, 
            ci.transformation=ci.transformation,
            # event times:
            times=NM.est$e.times,
            # confidence intervals:
            probs=ci$CI, all.probs=ci$all.CI,
            # posible transitions:
            p.trans=NM.est$p.trans,CI=CI)
        }else{
          # results:
          res <- list(
            # states information:
            cov=cov,
            method=method,s=s,t=NM.est$t,states=states, ns=ns, tr.states=tr.states,
            ci.transformation=ci.transformation,
            # event times:
            times=NM.est$e.times,
            # occupation or transition probabilities:
            probs=NM.est$probs, all.probs=NM.est$all.est,
            # posible transitions:
            p.trans=NM.est$p.trans,CI=CI)
        }
      }
      if (m==2){
        # the method is 'AJ'
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
        
        
        if(CI==TRUE){
          # variances of AJ estimator:
          variances <- var.AJ(ns, states, AJ.est$dNs.id_tr, AJ.est$Ys.id_tr, AJ.est$sum_dNs.id_tr,
                              AJ.est$TP.AJs, AJ.est$all.I.dA, tr.states)
          
          # CI of AJ estimator:
          ci <- ci.AJ(s,t, level, ci.transformation, AJ.est$dNs.id_tr, AJ.est$TP.AJs,
                      variances$cov.AJs, AJ.est$e.times.id_tr)
          
          
          # results:
          res <- list(
            # states information:
            cov=cov,
            method=method,s=s,t=AJ.est$t,states=states, ns=ns, tr.states=tr.states,
            ci.transformation=ci.transformation,
            # event times:
            times=AJ.est$e.times.id_tr,
            # confidence intervals:
            probs=ci$CI, all.probs=ci$all.CI,
            # posible transitions:
            p.trans=AJ.est$p.trans,CI=CI)
        }else{      
          # results:
          res <- list(
            # states information:
            cov=cov,
            method=method,s=s,t=AJ.est$t,states=states, ns=ns, tr.states=tr.states, 
            ci.transformation=ci.transformation,
            # event times:
            times=AJ.est$e.times.id_tr,
            # occupation or transition probabilities:
            probs=AJ.est$probs,all.probs=AJ.est$all.est,
            # posible transitions:
            p.trans=AJ.est$p.trans,CI=CI)
        }
      }
    } else {
      colcov<-match(cov,names(data))
      covs<-data[,colcov]
      # choose method
      method.type<-c("NM","AJ")
      m<-charmatch(method,method.type,nomatch=0)
      # charmatch function returns:
      # value 0 (if method != c(NM,AJ)) 
      # value 1 (if method = NM) or value 2 (if method = AJ)
      if (m==0)
        stop("method should be 'NM' or 'AJ'")
      if (m==1){
        # the method is 'NM'
        # prepare data set and NM method:
        names.cov <- levels(covs) # names of categories
        n.cat<-length(names.cov) # number of categories
        
        M<-c(NA)
        for(i in 1:n.cat){
          w.cat <- which(covs==names.cov[[i]])
          M[i]<-length(w.cat)
        }
        
        results.times.NM <- vector('list',n.cat)
        results.all.probs.NM <- results.probs.NM <- results.all.est.NM<- vector('list', n.cat)
        
        if(CI==TRUE){
          results.all.ci.NM <- vector('list', n.cat)
          results.ci.NM <- vector('list', n.cat)
        }
        
        
        for(i in 1:n.cat){
          events<-which(covs==names.cov[[i]])
          
          data_i <- data[events, , drop=FALSE] ## reduces data
          
          # method 'NM'
          # prepare data set and NM method:
          NM.est<- fun.NM(data_i, states, s, t, tr.states)
          
          results.times.NM[[i]] <- NM.est$e.times
          results.all.est.NM[[i]] <- NM.est$all.est
          results.probs.NM[[i]] <- NM.est$probs
          results.all.probs.NM[[i]] <- NM.est$all.probs
          
          if(CI==TRUE){
            times_i<-results.times.NM[[i]]
            probs_i<-results.all.probs.NM[[i]]
            
            
            # var using Titman method:
            variances<- var.NM(data_i, ns, states, tr.states, s, t, probs_i) 
            #           
            # ci:
            ci<- ci.NM(s, t, probs_i, variances$var.nm, level, ci.transformation, times_i)
            
            results.all.ci.NM[[i]]<-ci$all.CI
            results.ci.NM[[i]]<-ci$CI
          }
        }
        
        
        m.t.nm <-vector('list',n.cat)
        for(i in 1:n.cat){
          m.t.nm[i]<-max(results.times.NM[[i]])
        }
        
        names(results.times.NM) <- paste(rep("t",n.cat), sep="")
        names(results.all.probs.NM) <- names(results.probs.NM) <- names(results.all.est.NM) <- paste(rep("probs",n.cat),sep="")
        names(m.t.nm) <- paste(rep("t",n.cat), sep="")
        
        if(CI==TRUE){
          names(results.all.ci.NM)<-paste(rep("CI",n.cat), sep="")
          names(results.ci.NM)<-paste(rep("CI",n.cat), sep="")
        }
        
        if (s==0){
          p.trans<-c("1 1", "1 2", "1 3")
        }else{
          p.trans<-c("1 1", "1 2", "1 3", "2 2", "2 3")
        }
        
        if(CI==TRUE){
          # results:
          res <- list(
            # states information:
            cov=cov, names.cov=names.cov,
            method=method,s=s,
            t=m.t.nm,
            states=states, ns=ns, tr.states=tr.states, 
            ci.transformation=ci.transformation,
            # event times:
            times=results.times.NM,
            # confidence intervals:
            probs=results.ci.NM, all.probs=results.all.ci.NM,
            # posible transitions:
            p.trans=p.trans,CI=CI)
        }else{
          # results:
          res <- list(
            # states information:
            cov=cov, names.cov=names.cov,
            method=method,s=s,
            t=m.t.nm,
            states=states, ns=ns, tr.states=tr.states,
            ci.transformation=ci.transformation,
            # event times:
            times=results.times.NM,
            # occupation or transition probabilities:
            probs=results.probs.NM, all.probs=results.all.est.NM,
            # posible transitions:
            p.trans=p.trans,CI=CI)
        }
      }
      if (m==2){
        
        # the method is 'AJ'
        names.cov <- levels(covs) # names of categories
        n.cat<-length(names.cov) # number of categories
        
        M<-c(NA)
        for(i in 1:n.cat){
          w.cat <- which(covs==names.cov[[i]])
          M[i]<-length(w.cat)
        }
        
        results.times.AJ <- vector('list',n.cat)
        results.probs.AJ <- results.all.est.AJ <- vector('list', n.cat)
        
        if(CI==TRUE){
          results.all.ci.AJ <- vector('list', n.cat)
          results.ci.AJ <- vector('list', n.cat)
        }
        
        for(ii in 1:n.cat){
          events<-which(covs==names.cov[[ii]])
          
          data_i <- data[events, , drop=FALSE] ## reduces data
          
          # start time ==0
          data_i$start.time<-0
          
          # initial probabilities for each initial state
          i.state<-integer(length(data_i[,1]))
          for(i in 1:length(data_i[,1])){
            if(data_i$start.time[i]==0) i.state[i]=1
            if(data_i$start.time[i]==data_i$time1[i]) i.state[i]=2
            if(data_i$start.time[i]==data_i$Stime[i]) i.state[i]=3
          }
          i.state <- factor(i.state,levels=states,labels=states)
          
          initial.probs <- prop.table(table(i.state))
          
          
          # prepare data set to compute AJ method:
          ds.prep.AJ <- prep.data.AJ(data_i, states, tr.states)
          
          # reduces to event times:
          ds.event.AJ <- prep.data.event.AJ(ds.prep.AJ$dNs, ds.prep.AJ$Ys, ds.prep.AJ$sum_dNs, states, tr.states)
          event.times <- as.numeric(as.character(rownames(ds.event.AJ$dNs)))
          
          # AJ estimator:
          AJ.est <- fun.AJ(ns,states, ds.event.AJ$dNs, ds.event.AJ$Ys, ds.event.AJ$sum_dNs,
                           s, t,  event.times, initial.probs)
          
          # results for each category:
          results.times.AJ[[ii]] <- AJ.est$e.times.id_tr
          results.times.AJ[[ii]] <- AJ.est$e.times
          results.all.est.AJ[[ii]] <- AJ.est$all.est
          results.probs.AJ[[ii]] <- AJ.est$probs
          
          if(CI==TRUE){
            # variances of AJ estimator:
            variances <- var.AJ(ns, states, AJ.est$dNs.id_tr, AJ.est$Ys.id_tr, AJ.est$sum_dNs.id_tr,
                                AJ.est$TP.AJs, AJ.est$all.I.dA, tr.states)
            
            # CI of AJ estimator:
            ci <- ci.AJ(s,t, level, ci.transformation, AJ.est$dNs.id_tr, AJ.est$TP.AJs,
                        variances$cov.AJs, AJ.est$e.times.id_tr)
            
            results.all.ci.AJ[[ii]]<-ci$all.CI
            results.ci.AJ[[ii]]<-ci$CI
          }
        }
        
        m.t.aj <-vector('list',n.cat)
        for(i in 1:n.cat){
          m.t.aj[i]<-max(results.times.AJ[[i]])
        }
        
        names(results.times.AJ) <- paste(rep("t",n.cat), sep="")
        names(results.probs.AJ) <- names(results.all.est.AJ) <- paste(rep("probs",n.cat), sep="")
        names(m.t.aj) <- paste(rep("t",n.cat), sep="")
        
        if(CI==TRUE){
          names(results.all.ci.AJ)<-paste(rep("CI",n.cat), sep="")
          names(results.ci.AJ)<-paste(rep("CI",n.cat), sep="")
        }
        
        if (s==0){
          p.trans<-c("1 1", "1 2", "1 3")
        }else{
          p.trans<-c("1 1", "1 2", "1 3", "2 2", "2 3")
        } 
        
        
        if(CI==TRUE){
          # results:
          res <- list(
            # states information:
            cov=cov, names.cov=names.cov,
            method=method,s=s,
            t=m.t.aj,
            states=states, ns=ns, tr.states=tr.states, 
            ci.transformation=ci.transformation,
            # event times:
            times=results.times.AJ,
            # confidence intervals:
            probs=results.ci.AJ, all.probs=results.all.ci.AJ,
            # posible transitions:
            p.trans=p.trans,CI=CI)
        }else{
          # results:
          res <- list(
            # states information:
            cov=cov, names.cov=names.cov,
            method=method,s=s,
            t=m.t.aj,
            states=states, ns=ns, tr.states=tr.states,
            ci.transformation=ci.transformation,
            # event times:
            times=results.times.AJ,
            # occupation or transition probabilities:
            probs=results.probs.AJ, all.probs=results.all.est.AJ,
            # posible transitions:
            p.trans=p.trans,CI=CI)
        }      
      }
    }
    
    
    res$call<-match.call()
    class(res) = "TPidm"
    res
    
  }
