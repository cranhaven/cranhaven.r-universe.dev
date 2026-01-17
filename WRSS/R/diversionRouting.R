diversionRouting <-
function(demand   = NA,
         priority = NA,
         capacity     ,
         inflow       ,
         simulation)
{
   names(simulation)<-c("start","end","interval")
   dates<-seq(as.Date(simulation$start),as.Date(simulation$end),simulation$interval)
   conversionTable<-cbind(c('month','week','day'),c(0.382614,1.653439,11.57407))
   conversionFactor<-as.numeric(conversionTable[which(simulation$interval==conversionTable[,1]),2])
   capacity<-capacity/conversionFactor

   if(any(c(missing(inflow),missing(capacity),missing(simulation))))
   {
      stop('either of diversion parameters; inflow, capacity, or simulation; is missing!')
   }

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
   if(any(is.na(inflow)))
   {
      stop('inflow with missing values!')
   }
   if (length(inflow) > length(dates))
   {
       inflow <- inflow[1:length(dates),]
       warning('simulation steps miss match with the inflow vector length!')
   }
   if (length(inflow) < length(dates)) 
   {
       inflow <- c(inflow, rep(0, length(dates)-length(inflow)))
       warning('simulation steps miss match with the inflow vector length!')
   }

   R<-ifelse(inflow>capacity,capacity,inflow)
   overflow<-inflow-R

   if(all(is.na(priority)))
   {
      Release<-matrix(0,length(dates),ncol(demand))
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
   Release[which(is.nan(Release))]<-0
   Release<-as.data.frame(Release);rownames(Release)<-dates
   R<-as.matrix(R+apply(Release,1,sum));colnames(R)<-'diverted'
   diverted<-as.data.frame(R);rownames(diverted)<-dates
   overflow<-as.data.frame(overflow);rownames(overflow)<-dates
   demand<-as.data.frame(demand);rownames(demand)<-dates
   sim<-list(release=Release,demand=demand,diverted=diverted,overflow=overflow)
   return(sim)
}