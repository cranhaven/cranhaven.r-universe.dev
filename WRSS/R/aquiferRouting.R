aquiferRouting <-
function(demand               =NA ,
              priority        =NA ,
              area                ,
              volume              ,
              rechargeTS      =NA ,
              leakageFraction =NA ,
              initialStorage  =NA ,
              Sy              =0.1,
              simulation)
{
   if(any(c(missing(area),missing(volume),missing(simulation))))
   {
      stop('either of aquifer parameters; area, volume, or simulation; is missing!')
   }
   names(simulation)<-c("start","end","interval")
   dates<-seq(as.Date(simulation$start),as.Date(simulation$end),simulation$interval)
   if(all(is.na(rechargeTS)))
   {
      rechargeTS<-rep(0,length(dates))
   }
   if(length(dates)>length(rechargeTS))
   {
      temp<-rep(0,length(dates))
      temp[1:length(rechargeTS)]<-rechargeTS
      rechargeTS<-temp
      warning('simulation steps miss match with the data length!')
   }
   if(length(dates)<length(rechargeTS))
   {
      rechargeTS<-rechargeTS[1:length(dates)]
      warning('simulation steps miss match with the recharge flow length!')
   }
   rechargeTS<-as.matrix(rechargeTS)

   if(!all(is.na(demand)))
   {
      demand<-as.matrix(demand)
      if (nrow(demand) > length(dates))
      {
          demand <- demand[1:length(dates),]
          warning('simulation steps miss match with the demand vector length!')
      }
      if (nrow(demand) < length(dates)) 
      {
          demand <- rbind(demand, matrix(0, length(dates)-nrow(demand), ncol(demand)))
          warning('simulation steps miss match with the demand vector length!')
      }
   }else{
      demand<-data.frame(zero_demand=rep(0,length(dates)))
   }
   if(any(is.na(demand)))
   {
      stop('demand with missing values!')
   }

   if (is.na(leakageFraction)) {leakageFraction <- 0}
   actual_capacity<-volume*Sy
   leakage        <-rep(0,length(dates))
   storage        <-rep(0,length(dates))
   energy         <-rep(0,length(dates))
   release        <-matrix(0,length(dates),ncol(demand))
   demand         <-as.matrix(demand)
   D              <-apply(demand,1,sum)
   R              <-rep(0,length(dates))
   storage[1]     <-ifelse(is.na(initialStorage),actual_capacity,initialStorage)

   for(iter in 1:ifelse(is.na(initialStorage),10,1))
   {
     if(iter>1)
     {
        storage[1]<-storage[length(dates)]
     }
     if((rechargeTS[1]+storage[1]-D[1])>actual_capacity)
     {
       R[1]<-D[1]
       leakage[1]<-rechargeTS[1]+storage[1]-D[1]-actual_capacity + actual_capacity*leakageFraction
       storage[1]<-actual_capacity-storage[1]*leakageFraction
     }
     if((rechargeTS[1]+storage[1]-D[1])<actual_capacity && (rechargeTS[1]+storage[1]-D[1])>0)
     {
       R[1]<-D[1]
       storage[1]<-rechargeTS[1]+storage[1]-D[1]
       leakage[1]<-storage[1]*leakageFraction
       storage[1]<-storage[1]-leakage[1]
     }
     if((rechargeTS[1]+storage[1])<D[1])
     {
       R[1]<-rechargeTS[1]+storage[1]
       storage[1]<-0
       leakage[1]<-storage[1]*leakageFraction
     }
     energy[1]<-storage[1]/area/Sy
    
     for(t in 2:length(dates))
     {
        if((rechargeTS[t]+storage[t-1]-D[t])>actual_capacity)
        {
           R[t]<-D[t]
           leakage[t]<-rechargeTS[t]+storage[t-1]-D[t]-actual_capacity+actual_capacity*leakageFraction
           storage[t]<-actual_capacity-leakage[t]
        }
        if((rechargeTS[t]+storage[t-1]-D[t])<actual_capacity && (rechargeTS[t]+storage[t-1]-D[t])>0)
        {
           R[t]<-D[t]
           leakage[t]<-storage[t-1]*leakageFraction
           storage[t]<-rechargeTS[t]+storage[t-1]-R[t]-leakage[t]
        }
        if((rechargeTS[t]+storage[t-1])<D[t])
        {
           R[t]<-rechargeTS[t]+storage[t-1]-storage[t-1]*leakageFraction
           leakage[t]<-storage[t-1]*leakageFraction
           storage[t]<-0
        }
        energy[t]<-storage[t]/area/Sy
      }
   }   

   if(all(is.na(priority)))
   {
      Release<-matrix(NA,length(dates),ncol(demand))
      for(i in 1:ncol(demand))
      {
         Release[,i]<-ifelse(R>demand[,i],demand[,i],R)
         R<-R-Release[,i]
      }
   }else{
      sorted<-sort(priority,index.return = TRUE)
      priority_index<-sorted$ix
      unique_priority<-sort(unique(priority))
      if(length(unique_priority)<length(priority))
      {
         merged_Demand<-matrix(NA,length(dates),length(unique_priority))
         for(d in 1:length(unique_priority)){merged_Demand[,d]<-apply(as.matrix(demand[,which(!is.na(match(priority,unique_priority[d])))]),1,sum)}
         release<-matrix(0,length(dates),ncol(merged_Demand))
         for(d in 1:length(unique_priority)){release[,d]<-ifelse(R>merged_Demand[,d],merged_Demand[,d],R);R<-R-release[,d]}
         Release<-matrix(NA,length(dates),ncol(demand))
         for(d in 1:length(unique_priority))
         {
            Release[,which(!is.na(match(priority,unique_priority[d])))]<-
                           release[,d]*
                           (as.matrix(demand[,which(!is.na(match(priority,unique_priority[d])))])/
                           apply(as.matrix(demand[,which(!is.na(match(priority,unique_priority[d])))]),1,sum))
         }
         if(!is.null(colnames(demand))){colnames(Release)<-colnames(demand)}
      }else{
         Release<-matrix(NA,length(dates),ncol(demand))
         if(!is.null(colnames(demand))){colnames(Release)<-colnames(demand)}
         demand<-as.matrix(demand[,priority_index])
         release<-matrix(0,length(dates),ncol(demand))
         for(d in 1:length(unique_priority)){release[,d]<-ifelse(R>demand[,d],demand[,d],R);R<-R-release[,d]}
         for(d in 1:length(unique_priority)){Release[,which(!is.na(match(priority,unique_priority[d])))]<-release[,d]}
      }
   }
    Release[which(is.nan(Release[,1])),]<-0
    Release<-as.data.frame(Release);rownames(Release)<-dates
    rechargeTS<-as.data.frame(rechargeTS);rownames(rechargeTS)<-dates
    leakage<-as.data.frame(leakage);rownames(leakage)<-dates
    storage<-as.data.frame(storage);rownames(storage)<-dates
    demand<-as.data.frame(demand);rownames(demand)<-dates
    sim<-list(release=Release,demand=demand,recharge=rechargeTS,leakage=leakage,storage=storage)
    return(sim)
}