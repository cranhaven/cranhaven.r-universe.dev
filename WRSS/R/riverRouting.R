riverRouting <-
     function(demand         =NA,
              priority       =NA,
              discharge      =NA,
              seepageFraction=NA,
              simulation)
{
   if(any(c(missing(discharge),missing(simulation))))
   {
      stop('either of river parameters; inflow or simulation; is missing!')
   }
   names(simulation)<-c("start","end","interval")
   dates<-seq(as.Date(simulation$start),as.Date(simulation$end),simulation$interval)
   if(is.na(seepageFraction)) seepageFraction<-0
   if(seepageFraction>1 | seepageFraction<0)
   {
      stop('the seepageFraction must be in [0 1] interval!')
   }
   if(all(is.na(discharge)))
   {
      discharge<-rep(0,length(dates))
   }
   if(!all(is.na(demand)))
   {
      demand<-as.matrix(demand)
      if (nrow(demand) > length(dates))
      {
          demand <- demand[1:length(dates),]
          warning('simulation steps miss match with the demand matrix dimensions!')
      }
      if (nrow(demand) < length(dates)) 
      {
          demand <- rbind(demand, matrix(0, length(dates)-nrow(demand), ncol(demand)))
          warning('simulation steps miss match with the demand matrix dimensions!')
      }
   }else{
      demand<-data.frame(zero_demand=rep(0,length(dates)))
   }
   if(any(is.na(demand)))
   {
      stop('demand with missing values!')
   }
   if(any(is.na(discharge)))
   {
      stop('discharge with missing values!')
   }
   if (length(discharge) > length(dates))
   {
       discharge <- discharge[1:length(dates),]
       warning('simulation steps miss match with the discharge vector length!')
   }
   if (length(discharge) < length(dates)) 
   {
       discharge <- c(discharge, rep(0, length(dates)-length(discharge)))
       warning('simulation steps miss match with the discharge vector length!')
   }
   seepage<-discharge*seepageFraction
   R<-discharge*(1-seepageFraction)
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
         release<-matrix(0,length(dates),ncol(demand))
         if(!is.null(colnames(demand))){colnames(Release)<-colnames(demand)}
         demand<-as.matrix(demand[,priority_index])
         for(d in 1:length(unique_priority)){release[,d]<-ifelse(R>demand[,d],demand[,d],R);R<-R-release[,d]}
         for(d in 1:length(unique_priority)){Release[,which(!is.na(match(priority,unique_priority[d])))]<-release[,d]}
      }
   }
    Release[which(is.nan(Release[,1])),]<-0
    Release<-as.data.frame(Release);rownames(Release)<-dates
    discharge<-as.data.frame(discharge);rownames(discharge)<-dates
    demand<-as.data.frame(demand);rownames(demand)<-dates
    outflow<-data.frame(outflow=discharge-apply(Release,1,sum))
    seepage<-data.frame(seepage=seepage);rownames(seepage)<-dates
    sim<-list(discharge=discharge,outflow=outflow,release=Release,demand=demand)
    return(sim)
}