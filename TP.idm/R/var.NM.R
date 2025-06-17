var.NM <-
function(data, ns, states, tr.states, s, t, probs){  
    
    data_or<-data[order(data$time1),]
    
    n=nrow(data)
    s1<-c(rep(0,nrow(data)))
    for (i in 1:n){
      if(data_or$time1[i]>s) s1[i]=1
      else s1[i]=0
    }
    data_or$s1<-s1
    
    s2<-c(rep(0,nrow(data)))
    for (i in 1:n){
      if(data_or$time1[i]<=s & s<data_or$Stime[i]) s2[i]=1
      else s2[i]=0
    }
    data_or$s2<-s2
    
    data<-data_or
    
    split_ev <- strsplit(colnames(data), "  ") ## string splits names    
    s1.col <- which(sapply(split_ev, function(x) x[1]=="s1"))
    s2.col <- which(sapply(split_ev, function(x) x[1]=="s2"))
    ev1.col <- which(sapply(split_ev, function(x) x[1]=="event1"))
    ev.col <- which(sapply(split_ev, function(x) x[1]=="event"))
    t1.col <- which(sapply(split_ev, function(x) x[1]=="time1"))
    St.col <- which(sapply(split_ev, function(x) x[1]=="Stime"))
    
    ##  looks at noncensored individuals
    event.id <- which(sapply(split_ev, function(x) x[1]=="s1" | x[1]=="s2"))
    event.rows<- which(apply(data[, event.id, drop=FALSE], 1, function(x) any(x>0)))
    data.event <- data[event.rows, , drop=FALSE] 
    
    event.id.s <- which(sapply(split_ev, function(x) x[1]=="event1"))
    event.rows.s<- which(apply(data.event[, event.id.s, drop=FALSE], 1, function(x) any(x>0)))
    data.event.s <- data.event[event.rows.s, , drop=FALSE] 
    
    time1.s<-data.event.s[,t1.col][data.event.s[,ev1.col]==1]
    Stime.s<-data.event.s[,St.col][data.event.s[,ev.col]==1]
    
    ttime<-c(time1.s,Stime.s)
    times<- sort(unique(ttime))
    
    if (t=="last") t <- times[length(times)]
    id_tr.s <- which(s<=times & times<=t) ## location of those [s, t]
    l.id_tr.s <- length(id_tr.s)
    
    e.times<-times[id_tr.s]
    
    # Titman variances:
    if (s==0){    
      # subset 1
      set_s1<-subset(data,data$s1==1)
      # status 1:
      n_s1<-nrow(set_s1)
      
      ### 
      s1_times1<-set_s1$time1   
      s1_Stimes<-set_s1$Stime
      
      set_s1_z.ord <- set_s1[order(set_s1$time1),]
      set_s1_t.ord <- set_s1[order(set_s1$Stime),]
      
      # create the useful matrix
      sum.var.p11<-sum.var.p13<-sum.var.p12<-c(NA)
      
      d11<-d13<-d12<-c(NA)
      for(j in 1:n_s1){
        d11[j]<-sum(set_s1$time1>=s1_times1[j]) 
        d13[j]<-sum(set_s1$Stime>s1_Stimes[j])
        d12[j]<-sum(set_s1$Stime>=s1_Stimes[j])
      }
      
      for(i in 1:l.id_tr.s){
        ti<-e.times[i]
        tts1<-which(set_s1$time1<=ti)
        tts1.2<-which(set_s1$Stime<=ti)
        
        # to compute var(p11)
        if(length(tts1)>0){
          d1.z<-set_s1$event1[tts1]  
          d11.ti<-d11[tts1]
          rate.d1.z.d11<-d1.z/(d11.ti/n_s1)^2 
        } else {
          rate.d1.z.d11<-0
        }
        sum.var.p11[i]<-sum(rate.d1.z.d11)
        
        # to compute var(p13)
        if(length(tts1.2)>0){
          d1.t<-set_s1$event[tts1.2]
          d13.ti<-d13[tts1.2]
          rate.d1.t.d13<-d1.t/(d13.ti/n_s1)^2         
        }else{
          rate.d1.t.d13<-0
        }
        sum.var.p13[i]<-sum(rate.d1.t.d13)
        
        # to compute var(p12)  
        if(length(tts1)>0){
          sum.var.p12.z<-d1.z/(d11.ti/n_s1)
        } else {
          sum.var.p12.z<-0
        }
        sum.var.p12.z.n<-rep(0,n_s1)
        
        if(length(tts1)>0){sum.var.p12.z.n[tts1]<-sum.var.p12.z}
        
        sum.var.p12.t.n<-rep(0,n_s1)
        if(length(tts1.2)>0){
          d12.ti<-d12[tts1.2]
          sum.var.p12.t<-d1.t[tts1.2]/(d12.ti/n_s1)    
          sum.var.p12.t.n[tts1.2]<-sum.var.p12.t
        }
        
        tts1.ord<-which(set_s1_z.ord$time1<=ti)
        tts1.2.ord<-which(set_s1_t.ord$Stime<=ti)
        
        sum.var.p12.t.A.n<-rep(0,n_s1)
        if(length(tts1.ord)>0){
          d12.A<-set_s1_z.ord$event1[tts1.ord]
          d11.ti.ord<-d11[tts1.ord]
          sum.var.p12.t.A<-d12.A/(d11.ti.ord/n_s1)^2 
          sum.var.p12.t.A.n[tts1.ord]<-sum.var.p12.t.A
        }
        
        
        sum.var.p12.t.B.n<-rep(0,n_s1)
        if(length(tts1.2.ord)>0){
          d12.B<-set_s1_t.ord$event[tts1.2.ord]
          d12.ti.ord<-d12[tts1.2.ord]
          sum.var.p12.t.B<-d12.B/(d12.ti.ord/n_s1)^2
          sum.var.p12.t.B.n[tts1.2.ord]<-sum.var.p12.t.B 
        }
        
        A<- sum.var.p12.z.n - (1/n_s1)*sum.var.p12.t.A.n
        B<- sum.var.p12.t.n - (1/n_s1)*sum.var.p12.t.B.n
        
        S.A <- probs[1,1,i]* A
        S.B <- (1-probs[1,3,i])* B
        SZ_ST <- (S.A-S.B)^2
        
        sum.var.p12[i]<-sum(SZ_ST,na.rm=T)
        
        
      } # end for
      
      ### var(p11)
      var.p11<-sum.var.p11*((probs[1,1,]/n_s1)^2)
      
      ### var(p13)
      var.p13<-sum.var.p13*(((1-probs[1,3,])/n_s1)^2)
      
      ## var(p12)      
      var.p12 <- ((1/n_s1)^2)*sum.var.p12
      
      p.transitions<-c("1 1", "1 2", "1 3")
      var.nm<-matrix(NA,nrow=length(e.times),ncol=length(p.transitions))   
      var.nm<-cbind(var.p11, var.p12, var.p13)
      rownames(var.nm)<-e.times
      colnames(var.nm)<-paste(p.transitions)           
      
    } else {
      # s>0:    
      
      set_s1<-subset(data,data$s1==1)
      # status 1:
      n_s1<-nrow(set_s1)
      
      set_s2<-subset(data,data$s2==1)
      n_s2<-nrow(set_s2)
      
      ### 
      s1_times1<-set_s1$time1   
      s1_Stimes<-set_s1$Stime
      s2_Stimes<-set_s2$Stime
      
      set_s1_z.ord <- set_s1[order(set_s1$time1),]
      set_s1_t.ord <- set_s1[order(set_s1$Stime),]
      
      
      # create the useful matrix
      sum.var.p11<-sum.var.p13<-sum.var.p12<-sum.var.p22<-c(NA)
      
      d11<-d13<-d12<-d22<-c(NA)
      for(j in 1:n_s1){
        d11[j]<-sum(set_s1$time1>=s1_times1[j]) 
        d13[j]<-sum(set_s1$Stime>s1_Stimes[j])
        d12[j]<-sum(set_s1$Stime>=s1_Stimes[j])
      }
      
      for(h in 1:n_s2){
        d22[h]<-sum(set_s2$Stime>=s2_Stimes[h])      
      }
      
      for(i in 1:l.id_tr.s){
        ti<-e.times[i]
        tts1<-which(set_s1$time1<=ti)
        tts1.2<-which(set_s1$Stime<=ti)
        tts2<-which(set_s2$Stime<=ti)
        
        # to compute var(p11)
        if(length(tts1)>0){
          d1.z<-set_s1$event1[tts1]  
          d11.ti<-d11[tts1]
          rate.d1.z.d11<-d1.z/(d11.ti/n_s1)^2 
        } else {
          rate.d1.z.d11<-0
        }
        sum.var.p11[i]<-sum(rate.d1.z.d11)
        
        # to compute var(p13)
        if(length(tts1.2)>0){
          d1.t<-set_s1$event[tts1.2]
          d13.ti<-d13[tts1.2]
          rate.d1.t.d13<-d1.t/(d13.ti/n_s1)^2         
        }else{
          rate.d1.t.d13<-0
        }
        sum.var.p13[i]<-sum(rate.d1.t.d13)
        
        # to compute var(p12)       
        if(length(tts1)>0){
          sum.var.p12.z<-d1.z/(d11.ti/n_s1)
        } else {
          sum.var.p12.z<-0
        }
        sum.var.p12.z.n<-rep(0,n_s1)
        
        if(length(tts1)>0){sum.var.p12.z.n[tts1]<-sum.var.p12.z}
        
        sum.var.p12.t.n<-rep(0,n_s1)
        if(length(tts1.2)>0){
          d12.ti<-d12[tts1.2]
          sum.var.p12.t<-d1.t[tts1.2]/(d12.ti/n_s1)    
          sum.var.p12.t.n[tts1.2]<-sum.var.p12.t
        }
        
        tts1.ord<-which(set_s1_z.ord$time1<=ti)
        tts1.2.ord<-which(set_s1_t.ord$Stime<=ti)
        
        sum.var.p12.t.A.n<-rep(0,n_s1)
        if(length(tts1.ord)>0){
          d12.A<-set_s1_z.ord$event1[tts1.ord]
          d11.ti.ord<-d11[tts1.ord]
          sum.var.p12.t.A<-d12.A/(d11.ti.ord/n_s1)^2 
          sum.var.p12.t.A.n[tts1.ord]<-sum.var.p12.t.A
        }
        
        
        sum.var.p12.t.B.n<-rep(0,n_s1)
        if(length(tts1.2.ord)>0){
          d12.B<-set_s1_t.ord$event[tts1.2.ord]
          d12.ti.ord<-d12[tts1.2.ord]
          sum.var.p12.t.B<-d12.B/(d12.ti.ord/n_s1)^2
          sum.var.p12.t.B.n[tts1.2.ord]<-sum.var.p12.t.B 
        }
        
        A<- sum.var.p12.z.n - (1/n_s1)*sum.var.p12.t.A.n
        B<- sum.var.p12.t.n - (1/n_s1)*sum.var.p12.t.B.n
        
        S.A <- probs[1,1,i]* A
        S.B <- (1-probs[1,3,i])* B
        
        SZ_ST <- (S.A-S.B)^2
        
        sum.var.p12[i]<-sum(SZ_ST,na.rm=T)
        
        ## to compute var(p22)
        if(length(tts2)>0){
          d2.t<-set_s2$event[tts2]
          d22.ti<-d22[tts2]
          rate.d2.t.d22<-d2.t/(d22.ti/n_s2)^2
        }else{
          rate.d2.t.d22<-0
        }
        
        sum.var.p22[i]<-sum(rate.d2.t.d22)
        
      } # end for
      
      ### var(p11)
      var.p11<-sum.var.p11*((probs[1,1,]/n_s1)^2)
      
      ### var(p13)
      var.p13<-sum.var.p13*(((1-probs[1,3,])/n_s1)^2)
      
      ### var(p12)      
      var.p12 <- ((1/n_s1)^2)*sum.var.p12
      
      ### var(p22)
      var.p22<-sum.var.p22*((probs[2,2,]/n_s2)^2)
      
      ## var(p23)
      var.p23<-var.p22
      
      
      p.transitions<-c("1 1", "1 2", "1 3", "2 2", "2 3")
      var.nm<-matrix(NA,nrow=length(e.times),ncol=length(p.transitions))   
      var.nm<-cbind(var.p11, var.p12, var.p13, var.p22, var.p23)
      rownames(var.nm)<-e.times
      colnames(var.nm)<-paste(p.transitions)   
      
    }
    
    res <- list(var.nm=var.nm)
    return(res)
    
  }
