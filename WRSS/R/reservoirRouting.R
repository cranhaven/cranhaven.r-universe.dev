reservoirRouting<-function(type='storage',inflow,netEvaporation=NA,demand=NA,priority=NA,seepageFraction=NA,
                     geometry=list(storageAreaTable=NULL,storageElevationTable=NULL,dischargeElevationTable=NULL,deadStorage=0,capacity=NULL),
                     plant=list(installedCapacity=NULL,efficiency=NULL,designHead=NULL,designFlow=NULL,turbineAxisElevation=NULL,submerged=FALSE,loss=0),
                     penstock=list(diameter=NULL,length=0,roughness=110),initialStorage=NA,simulation)
{
   ####### initialization #########
   if(missing(inflow))
   {
      stop('inflow time series is not specified!')
   }
   if(missing(simulation))
   {
      stop('simulation parameters are not specified!')
   }

   geometryBase<-list(storageAreaTable=NULL,
                      storageElevationTable=NULL,
                      dischargeElevationTable=NULL,
                      deadStorage=NULL,
                      capacity=NULL)
   if(any(names(geometry)=='storageElevationTable'))   geometryBase$storageElevationTable<-geometry$storageElevationTable
   if(any(names(geometry)=='storageAreaTable'))        geometryBase$storageAreaTable<-geometry$storageAreaTable
   if(any(names(geometry)=='dischargeElevationTable')) geometryBase$dischargeElevationTable<-geometry$dischargeElevationTable
   if(any(names(geometry)=='deadStorage'))             geometryBase$deadStorage<-geometry$deadStorage
   if(any(names(geometry)=='capacity'))                geometryBase$capacity<-geometry$capacity
   geometry<-geometryBase

   if(type == 'storage')
   {
      if(any(c(is.null(geometry$storageAreaTable),is.null(geometry$capacity))))
      {
         stop('reservoir geometric specifications are not specified!')
      }
   }
   if(type == 'hydropower')
   {
      plantBase<-list(installedCapacity=NULL,
                      efficiency=NULL,
                      designHead=NULL,
                      designFlow=NULL,
                      turbineAxisElevation=NULL,
                      submerged=FALSE,
                      loss=0)
      penstockBase<-list(diameter=NULL,
                         length=NULL,
                         roughness=110)
      if(any(names(plant)   =='installedCapacity'))       plantBase$installedCapacity<-plant$installedCapacity
      if(any(names(plant)   =='efficiency'))              plantBase$efficiency<-plant$efficiency
      if(any(names(plant)   =='designHead'))              plantBase$designHead<-plant$designHead
      if(any(names(plant)   =='designFlow'))              plantBase$designFlow<-plant$designFlow
      if(any(names(plant)   =='turbineAxisElevation'))    plantBase$turbineAxisElevation<-plant$turbineAxisElevation
      if(any(names(plant)   =='submerged'))               plantBase$submerged<-plant$submerged
      if(any(names(plant)   =='loss'))                    plantBase$loss<-plant$loss
      if(any(names(penstock)=='diameter'))                penstockBase$diameter<-penstock$diameter
      if(any(names(penstock)=='length'))                  penstockBase$length<-penstock$length
      if(any(names(penstock)=='roughness'))               penstockBase$roughness<-penstock$roughness
      plant<-plantBase
      penstock<-penstockBase
      if(any(c(is.null(geometry$storageElevationTable),
               is.null(geometry$capacity),
               ifelse(plant$submerged,is.null(geometry$dischargeElevationTable),FALSE),
               is.null(plant$installedCapacity),
               is.null(plant$efficiency),
               is.null(plant$designHead),
               is.null(plant$designFlow),
               is.null(plant$turbineAxisElevation),
               ifelse(penstock$length>0,is.null(penstock$diameter),FALSE))))
      {
          stop('plant parameter(s) or reservoir geometric specifications required for power simulation are missing!')
      }
   }
   if(length(simulation) <3 | !any(simulation[[3]] == c('day','week','month')))
   {
      stop('simulation parameters are not in a suitable form!')
   }
   names(simulation)<-c("start","end","interval")
   dates<-seq(as.Date(simulation$start),as.Date(simulation$end),simulation$interval)
   if(length(dates)>length(inflow))
   {
      dates<-dates[1:length(inflow)]
      warning('inflow length is adjusted with the length of simulation period!')
   }
   if(length(dates)<length(inflow))
   {
      inflow<-inflow[1:length(dates)]
      warning('inflow length is adjusted with the length of simulation period!')
   }
   if(!all(is.na(demand)))
   {
      demand<-as.matrix(demand)
      if (nrow(demand) > length(dates))
      {
          demand <- demand[1:length(dates),]
          warning('demand length is adjusted with the length of simulation period!')
      }
      if (nrow(demand) < length(dates)) 
      {
         demand <- rbind(demand, matrix(0, length(dates)-nrow(demand), ncol(demand)))
         warning('demand length is adjusted with the length of simulation period!')
      }
      D <- apply(demand, 1, sum)
   }else{
      demand<-data.frame(zero_demand=rep(0,length(dates)))
      D <- apply(demand, 1, sum)
   }
   if (is.na(seepageFraction)) {seepageFraction <- 0}
   conversionTable<-cbind(c('month','week','day'),c(0.382614,1.653439,11.57407))
   conversionFactor<-as.numeric(conversionTable[which(simulation$interval==conversionTable[,1]),2])
   seepage<-storage<-spill<-R<-RCMS<-power<-loss<-rep(0,length(dates))
   storage[1]<-ifelse(is.na(initialStorage),geometry$capacity,initialStorage)
   storageAreaFunction<-function(s) approxExtrap(x=geometry$storageAreaTable[,1],y=geometry$storageAreaTable[,2],xout=s)$y
   if(type=='hydropower') storageElevationFunction<-function(s) approxExtrap(x=geometry$storageElevationTable[,1],y=geometry$storageElevationTable[,2],xout=s)$y
   if(type=='hydropower' && plant$submerged) dischargeElevationFunction<-function(q) approxExtrap(x=geometry$dischargeElevationTable[,1],y=geometry$dischargeElevationTable[,2],xout=q)$y
   if(all(is.na(netEvaporation))) netEvaporation<-rep(0,length(dates))
   if(length(netEvaporation)>length(dates))
   {
      netEvaporation<-netEvaporation[1:length(dates)]
      warning('netEvaporation length is adjusted with the length of simulation period!')
   }
   if(length(netEvaporation)<length(dates))
   {
      temp<-rep(0,length(dates))
      temp[1:length(netEvaporation)]<-netEvaporation
      netEvaporation<-temp
      warning('netEvaporation length is adjusted with the length of simulation period!')
   }

   ####### splitter function to split the vector of release between demands #########
   splitter<-function (demand, priority, R) 
   {
       if (all(is.na(priority)))
       {
          Release<-matrix(NA,length(dates),ncol(demand))
          for (i in 1:ncol(demand))
          {
             Release[, i] <- ifelse(R > demand[, i], demand[, i], R)
             R <- R-Release[, i]
          }
       }else{
           priority[which(is.na(priority))]<-Inf
           sorted <- sort(priority, index.return = TRUE)
           priority_index <- sorted$ix
           unique_priority <- sort(unique(priority))
           if (length(unique_priority) < length(priority))
           {
              merged_Demand <- matrix(NA, length(dates), length(unique_priority))
              for (d in 1:length(unique_priority))
              {
                  merged_Demand[,d] <- apply(as.matrix(demand[,which(!is.na(match(priority,unique_priority[d])))]),1,sum)
              }
              release <- matrix(0, length(dates), ncol(merged_Demand))
              for (d in 1:length(unique_priority))
              {
                  release[,d] <- ifelse(R > merged_Demand[,d],merged_Demand[,d],R)
                  R <- R-release[, d]
              }
              Release <- matrix(NA, length(dates), ncol(demand))
              for (d in 1:length(unique_priority))
              {
                  Release[,which(!is.na(match(priority, unique_priority[d])))]<-release[,d]*(as.matrix(demand[,which(!is.na(match(priority,unique_priority[d])))])/apply(as.matrix(demand[,which(!is.na(match(priority,unique_priority[d])))]),1,sum))
              }
              if (!is.null(colnames(demand)))
              {
                  colnames(Release) <- colnames(demand)
              }
          }else{
              Release <- matrix(NA, length(dates), ncol(demand))
              if (!is.null(colnames(demand)))
              {
                  colnames(Release) <- colnames(demand)
              }
              demand <- demand[,priority_index, drop = FALSE]
              release <- matrix(0, length(dates), ncol(demand))
              for (d in 1:length(unique_priority))
              {
                  release[,d]<-ifelse(R>demand[,d],demand[,d],R)
                  R <- R-release[, d]
              }
              for (d in 1:length(unique_priority))
              {
                  Release[,which(!is.na(match(priority,unique_priority[d])))] <- release[,d]
              }
          }
      }
      Release[which(is.nan(Release))]<-0
      return(Release)
  }
   ####### cost functions #########
   fHydropowerStorage<-function(release)
   {
      penalty1<-FALSE
      penalty2<-FALSE
      if((storage[t-1]+inflow[t-1]-R[t])>geometry$capacity)
      {
         seep1 <- storage[t-1]*seepageFraction
         E1<-storageElevationFunction(storage[t-1])
         A1<-storageAreaFunction(storage[t-1])
         storage[t]<-geometry$capacity
         spill[t]<-storage[t-1]+inflow[t-1]-R[t]-geometry$capacity
         seep2 <- storage[t]*seepageFraction
         A2<-storageAreaFunction(storage[t])
         loss[t-1]<-(A1+A2)*netEvaporation[t-1]/2
         seepage[t]<-(seep1+seep2)/2
         if ((storage[t]-seepage[t]-loss[t]) < 0)
         {
             seepage[t]<-seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
             loss[t]   <-loss[t]  -(seepage[t]+loss[t]-storage[t])*loss[t]   /(seepage[t]+loss[t])
         }
         storage[t]<-storage[t]-seepage[t]-loss[t]
         E2<-storageElevationFunction(storage[t])
      }
      if((storage[t-1]+inflow[t-1]-R[t])<geometry$deadStorage)
      {
         seep1 <- storage[t-1]*seepageFraction
         E1<-storageElevationFunction(storage[t-1])
         A1<-storageAreaFunction(storage[t-1])
         storage[t]<-storage[t-1]+inflow[t-1]-R[t]
         spill[t]<-0
         seep2 <- storage[t]*seepageFraction
         A2<-storageAreaFunction(storage[t])
         loss[t-1]<-(A1+A2)*netEvaporation[t-1]/2
         seepage[t]<-(seep1+seep2)/2
         if ((storage[t]-seepage[t]-loss[t]) < 0)
         {
             seepage[t]<-seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
             loss[t]   <-loss[t]  -(seepage[t]+loss[t]-storage[t])*loss[t]   /(seepage[t]+loss[t])
         }
         storage[t]<-storage[t]-seepage[t]-loss[t]
         E2<-storageElevationFunction(storage[t])
         penalty1<-TRUE
      }
      if((storage[t-1]+inflow[t-1]-R[t])<geometry$capacity && (storage[t-1]+inflow[t-1]-R[t])>geometry$deadStorage)
      {
         seep1 <- storage[t-1]*seepageFraction
         E1<-storageElevationFunction(storage[t-1])
         A1<-storageAreaFunction(storage[t-1])
         storage[t]<-storage[t-1]+inflow[t-1]-R[t]
         spill[t]<-0
         seep2 <- storage[t]*seepageFraction
         A2<-storageAreaFunction(storage[t])
         loss[t-1]<-(A1+A2)*netEvaporation[t-1]/2
         seepage[t]<-(seep1+seep2)/2
         if ((storage[t]-seepage[t]-loss[t]) < 0)
         {
             seepage[t]<-seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
             loss[t]   <-loss[t]  -(seepage[t]+loss[t]-storage[t])*loss[t]   /(seepage[t]+loss[t])
         }
         storage[t]<-storage[t]-seepage[t]-loss[t]
         E2<-storageElevationFunction(storage[t])
      }
      release<-release*conversionFactor
      phi<-approxExtrap(x=plant$efficiency[,1],y=plant$efficiency[,2],xout=release)$y
      totalHeadLoss<-plant$loss+ifelse(penstock$length>0,(10.67*penstock$length/penstock$diameter^4.8704)*(release/penstock$roughness)^1.852,0)
      grossHead<-mean(c(E1,E2))-ifelse(plant$submerged,max(dischargeElevationFunction(release),plant$turbineAxisElevation),plant$turbineAxisElevation)-totalHeadLoss
      P<-9806*phi*release*grossHead
      obj<-plant$installedCapacity-P/1e6+1e5*((penalty1)+(grossHead<0)+ifelse(P/1e6>plant$installedCapacity,P/1e6-plant$installedCapacity,0))
      if(release<plant$designFlow[1] | release>plant$designFlow[2])
      {
         obj<-ifelse(release<plant$designFlow[1],1e5*(plant$designFlow[1]-release),0)+
         ifelse(release>plant$designFlow[2],1e5*(release-plant$designFlow[2]),0)
      }
      if(any((E1-plant$turbineAxisElevation)<plant$designHead[1],
             (E2-plant$turbineAxisElevation)<plant$designHead[1],
             (E1-plant$turbineAxisElevation)>plant$designHead[2],
             (E2-plant$turbineAxisElevation)>plant$designHead[2]))
      {
          obj<-obj+
          ifelse((E1-plant$turbineAxisElevation-plant$designHead[1])<0,abs(E1-plant$turbineAxisElevation-plant$designHead[1])*1e5,0)+
          ifelse((E2-plant$turbineAxisElevation-plant$designHead[1])<0,abs(E2-plant$turbineAxisElevation-plant$designHead[1])*1e5,0)+
          ifelse((E1-plant$turbineAxisElevation-plant$designHead[2])>0,abs(E1-plant$turbineAxisElevation-plant$designHead[2])*1e5,0)+
          ifelse((E2-plant$turbineAxisElevation-plant$designHead[2])>0,abs(E2-plant$turbineAxisElevation-plant$designHead[2])*1e5,0)
      }
      return(abs(obj))
   }
 
   f0HydropowerStorage<-function(release)
   {
      penalty1<-FALSE
      penalty2<-FALSE
      if((storage[1]+inflow[1]-R[1])>geometry$capacity)
      {
         seep1 <- storage[1]*seepageFraction
         E1<-storageElevationFunction(storage[1])
         A1<-storageAreaFunction(storage[1])
         spill[1]<-storage[1]+inflow[1]-R[1]-geometry$capacity
         storage[1]<-geometry$capacity
         seep2 <- storage[1]*seepageFraction
         A2<-storageAreaFunction(storage[1])
         loss[1]<-(A1+A2)*netEvaporation[1]/2
         seepage[1]<-(seep1+seep2)/2
         if ((storage[1]-seepage[1]-loss[1])< 0)
         {
             seepage[1]<-seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
             loss[1]   <-loss[1]  -(seepage[1]+loss[1]-storage[1])*loss[1]   /(seepage[1]+loss[1])
         }
         storage[1]<-storage[1]-seepage[1]-loss[1]
         E2<-storageElevationFunction(storage[1])
      }
      if((storage[1]+inflow[1]-R[1])<geometry$deadStorage)
      {
         seep1 <- storage[1]*seepageFraction
         E1<-storageElevationFunction(storage[1])
         A1<-storageAreaFunction(storage[1])
         spill[t]<-0
         storage[1]<-storage[1]+inflow[1]-R[1]
         seep2 <- storage[1]*seepageFraction
         A2<-storageAreaFunction(storage[1])
         loss[1]<-(A1+A2)*netEvaporation[1]/2
         seepage[1]<-(seep1+seep2)/2
         if ((storage[1]-seepage[1]-loss[1])< 0)
         {
             seepage[1]<-seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
             loss[1]   <-loss[1]  -(seepage[1]+loss[1]-storage[1])*loss[1]   /(seepage[1]+loss[1])
         }
         storage[1]<-storage[1]-seepage[1]-loss[1]
         E2<-storageElevationFunction(storage[1])
         penalty1<-TRUE
      }  
      if((storage[1]+inflow[1]-R[1])<geometry$capacity && (storage[1]+inflow[1]-R[1])>geometry$deadStorage)
      {
         seep1 <- storage[1]*seepageFraction
         E1<-storageElevationFunction(storage[1])
         A1<-storageAreaFunction(storage[1])
         spill[1]<-0
         storage[1]<-storage[1]+inflow[1]-R[1]
         seep2 <- storage[1]*seepageFraction
         A2<-storageAreaFunction(storage[1])
         loss[1]<-(A1+A2)*netEvaporation[1]/2
         seepage[1]<-(seep1+seep2)/2
         if ((storage[1]-seepage[1]-loss[1])< 0)
         {
             seepage[1]<-seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
             loss[1]   <-loss[1]  -(seepage[1]+loss[1]-storage[1])*loss[1]   /(seepage[1]+loss[1])
         }
         storage[1]<-storage[1]-seepage[1]-loss[1]
         E2<-storageElevationFunction(storage[1])
      }
      release<-release*conversionFactor
      phi<-approxExtrap(x=plant$efficiency[,1],y=plant$efficiency[,2],xout=release)$y
      totalHeadLoss<-plant$loss+ifelse(penstock$length>0,(10.67*penstock$length/penstock$diameter^4.8704)*(release/penstock$roughness)^1.852,0)
      grossHead<-mean(c(E1,E2))-ifelse(plant$submerged,max(dischargeElevationFunction(release),plant$turbineAxisElevation),plant$turbineAxisElevation)-totalHeadLoss
      P<-9806*phi*release*grossHead
      obj<-plant$installedCapacity-P/1e6 +1e5*((penalty1)+(grossHead<0)+ifelse(P/1e6>plant$installedCapacity,P/1e6-plant$installedCapacity,0))
      if(release<plant$designFlow[1] | release>plant$designFlow[2])
      {
         obj<-ifelse(release<plant$designFlow[1],1e5*(plant$designFlow[1]-release),0)+
         ifelse(release>plant$designFlow[2],1e5*(release-plant$designFlow[2]),0)
      }
      if(any((E1-plant$turbineAxisElevation)<plant$designHead[1],
             (E2-plant$turbineAxisElevation)<plant$designHead[1],
             (E1-plant$turbineAxisElevation)>plant$designHead[2],
             (E2-plant$turbineAxisElevation)>plant$designHead[2]))
      {
         obj<-obj+
           ifelse((E1-plant$turbineAxisElevation-plant$designHead[1])<0,abs(E1-plant$turbineAxisElevation-plant$designHead[1])*1e5,0)+
           ifelse((E2-plant$turbineAxisElevation-plant$designHead[1])<0,abs(E2-plant$turbineAxisElevation-plant$designHead[1])*1e5,0)+
           ifelse((E1-plant$turbineAxisElevation-plant$designHead[2])>0,abs(E1-plant$turbineAxisElevation-plant$designHead[2])*1e5,0)+
           ifelse((E2-plant$turbineAxisElevation-plant$designHead[2])>0,abs(E2-plant$turbineAxisElevation-plant$designHead[2])*1e5,0)
      }
      return(abs(obj))
   }

   fHydropower<-function(release)
   {
      penalty1<-FALSE
      penalty2<-FALSE
      if((storage[t-1]+inflow[t-1]-release)>geometry$capacity)
      {
         seep1 <- storage[t-1]*seepageFraction
         E1<-storageElevationFunction(storage[t-1])
         A1<-storageAreaFunction(storage[t-1])
         storage[t]<-geometry$capacity
         spill[t]<-storage[t-1]+inflow[t-1]-release-geometry$capacity
         seep2 <- storage[t]*seepageFraction
         A2<-storageAreaFunction(storage[t])
         loss[t-1]<-(A1+A2)*netEvaporation[t-1]/2
         seepage[t]<-(seep1+seep2)/2
         if ((storage[t]-seepage[t]-loss[t]) < 0)
         {
             seepage[t]<-seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
             loss[t]   <-loss[t]  -(seepage[t]+loss[t]-storage[t])*loss[t]   /(seepage[t]+loss[t])
         }
         storage[t]<-storage[t]-seepage[t]-loss[t]
         E2<-storageElevationFunction(storage[t])
      }
      if((storage[t-1]+inflow[t-1]-release)<geometry$deadStorage)
      {
         seep1 <- storage[t-1]*seepageFraction
         E1<-storageElevationFunction(storage[t-1])
         A1<-storageAreaFunction(storage[t-1])
         storage[t]<-storage[t-1]+inflow[t-1]-release
         spill[t]<-0
         seep2 <- storage[t]*seepageFraction
         A2<-storageAreaFunction(storage[t])
         loss[t-1]<-(A1+A2)*netEvaporation[t-1]/2
         seepage[t]<-(seep1+seep2)/2
         if ((storage[t]-seepage[t]-loss[t]) < 0)
         {
             seepage[t]<-seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
             loss[t]   <-loss[t]  -(seepage[t]+loss[t]-storage[t])*loss[t]   /(seepage[t]+loss[t])
         }
         storage[t]<-storage[t]-seepage[t]-loss[t]
         E2<-storageElevationFunction(storage[t])
         penalty1<-TRUE
      }
      if((storage[t-1]+inflow[t-1]-release)<geometry$capacity && (storage[t-1]+inflow[t-1]-release)>geometry$deadStorage)
      {
         seep1 <- storage[t-1]*seepageFraction
         E1<-storageElevationFunction(storage[t-1])
         A1<-storageAreaFunction(storage[t-1])
         storage[t]<-storage[t-1]+inflow[t-1]-release
         spill[t]<-0
         seep2 <- storage[t]*seepageFraction
         A2<-storageAreaFunction(storage[t])
         loss[t-1]<-(A1+A2)*netEvaporation[t-1]/2
         seepage[t]<-(seep1+seep2)/2
         if ((storage[t]-seepage[t]-loss[t]) < 0)
         {
             seepage[t]<-seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
             loss[t]   <-loss[t]  -(seepage[t]+loss[t]-storage[t])*loss[t]   /(seepage[t]+loss[t])
         }
         storage[t]<-storage[t]-seepage[t]-loss[t]
         E2<-storageElevationFunction(storage[t])
      }
      release<-release*conversionFactor
      phi<-approxExtrap(x=plant$efficiency[,1],y=plant$efficiency[,2],xout=release)$y
      totalHeadLoss<-plant$loss+ifelse(penstock$length>0,(10.67*penstock$length/penstock$diameter^4.8704)*(release/penstock$roughness)^1.852,0)
      grossHead<-mean(c(E1,E2))-ifelse(plant$submerged,max(dischargeElevationFunction(release),plant$turbineAxisElevation),plant$turbineAxisElevation)-totalHeadLoss
      P<-9806*phi*release*grossHead
      obj<-plant$installedCapacity-P/1e6+1e5*((penalty1)+(grossHead<0)+ifelse(P/1e6>plant$installedCapacity,P/1e6-plant$installedCapacity,0))
      if(release<plant$designFlow[1] | release>plant$designFlow[2])
      {
         obj<-ifelse(release<plant$designFlow[1],1e5*(plant$designFlow[1]-release),0)+
         ifelse(release>plant$designFlow[2],1e5*(release-plant$designFlow[2]),0)
      }
      if(any((E1-plant$turbineAxisElevation)<plant$designHead[1],
             (E2-plant$turbineAxisElevation)<plant$designHead[1],
             (E1-plant$turbineAxisElevation)>plant$designHead[2],
             (E2-plant$turbineAxisElevation)>plant$designHead[2]))
      {
          obj<-obj+
          ifelse((E1-plant$turbineAxisElevation-plant$designHead[1])<0,abs(E1-plant$turbineAxisElevation-plant$designHead[1])*1e5,0)+
          ifelse((E2-plant$turbineAxisElevation-plant$designHead[1])<0,abs(E2-plant$turbineAxisElevation-plant$designHead[1])*1e5,0)+
          ifelse((E1-plant$turbineAxisElevation-plant$designHead[2])>0,abs(E1-plant$turbineAxisElevation-plant$designHead[2])*1e5,0)+
          ifelse((E2-plant$turbineAxisElevation-plant$designHead[2])>0,abs(E2-plant$turbineAxisElevation-plant$designHead[2])*1e5,0)
      }
      return(abs(obj))
   }
 
   f0Hydropower<-function(release)
   {
      penalty1<-FALSE
      penalty2<-FALSE
      if((storage[1]+inflow[1]-release)>geometry$capacity)
      {
         seep1 <- storage[1]*seepageFraction
         E1<-storageElevationFunction(storage[1])
         A1<-storageAreaFunction(storage[1])
         spill[1]<-storage[1]+inflow[1]-release-geometry$capacity
         storage[1]<-geometry$capacity
         seep2 <- storage[1]*seepageFraction
         A2<-storageAreaFunction(storage[1])
         loss[1]<-(A1+A2)*netEvaporation[1]/2
         seepage[1]<-(seep1+seep2)/2
         if ((storage[1]-seepage[1]-loss[1])< 0)
         {
             seepage[1]<-seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
             loss[1]   <-loss[1]  -(seepage[1]+loss[1]-storage[1])*loss[1]   /(seepage[1]+loss[1])
         }
         storage[1]<-storage[1]-seepage[1]-loss[1]
         E2<-storageElevationFunction(storage[1])
      }
      if((storage[1]+inflow[1]-release)<geometry$deadStorage)
      {
         seep1 <- storage[1]*seepageFraction
         E1<-storageElevationFunction(storage[1])
         A1<-storageAreaFunction(storage[1])
         spill[t]<-0
         storage[1]<-storage[1]+inflow[1]-release
         seep2 <- storage[1]*seepageFraction
         A2<-storageAreaFunction(storage[1])
         loss[1]<-(A1+A2)*netEvaporation[1]/2
         seepage[1]<-(seep1+seep2)/2
         if ((storage[1]-seepage[1]-loss[1])< 0)
         {
             seepage[1]<-seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
             loss[1]   <-loss[1]  -(seepage[1]+loss[1]-storage[1])*loss[1]   /(seepage[1]+loss[1])
         }
         storage[1]<-storage[1]-seepage[1]-loss[1]
         E2<-storageElevationFunction(storage[1])
         penalty1<-TRUE
      }  
      if((storage[1]+inflow[1]-release)<geometry$capacity && (storage[1]+inflow[1]-release)>geometry$deadStorage)
      {
         seep1 <- storage[1]*seepageFraction
         E1<-storageElevationFunction(storage[1])
         A1<-storageAreaFunction(storage[1])
         spill[t]<-0
         storage[1]<-storage[1]+inflow[1]-release
         seep2 <- storage[1]*seepageFraction
         A2<-storageAreaFunction(storage[1])
         loss[1]<-(A1+A2)*netEvaporation[1]/2
         seepage[1]<-(seep1+seep2)/2
         if ((storage[1]-seepage[1]-loss[1])< 0)
         {
             seepage[1]<-seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
             loss[1]   <-loss[1]  -(seepage[1]+loss[1]-storage[1])*loss[1]   /(seepage[1]+loss[1])
         }
         storage[1]<-storage[1]-seepage[1]-loss[1]
         E2<-storageElevationFunction(storage[1])
      }

      release<-release*conversionFactor
      phi<-approxExtrap(x=plant$efficiency[,1],y=plant$efficiency[,2],xout=release)$y
      totalHeadLoss<-plant$loss+ifelse(penstock$length>0,(10.67*penstock$length/penstock$diameter^4.8704)*(release/penstock$roughness)^1.852,0)
      grossHead<-mean(c(E1,E2))-ifelse(plant$submerged,max(dischargeElevationFunction(release),plant$turbineAxisElevation),plant$turbineAxisElevation)-totalHeadLoss
      P<-9806*phi*release*grossHead
      obj<-plant$installedCapacity-P/1e6 +1e5*((penalty1)+(grossHead<0)+ifelse(P/1e6>plant$installedCapacity,P/1e6-plant$installedCapacity,0))
      if(release<plant$designFlow[1] | release>plant$designFlow[2])
      {
         obj<-ifelse(release<plant$designFlow[1],1e5*(plant$designFlow[1]-release),0)+
         ifelse(release>plant$designFlow[2],1e5*(release-plant$designFlow[2]),0)
      }
      if(any((E1-plant$turbineAxisElevation)<plant$designHead[1],
             (E2-plant$turbineAxisElevation)<plant$designHead[1],
             (E1-plant$turbineAxisElevation)>plant$designHead[2],
             (E2-plant$turbineAxisElevation)>plant$designHead[2]))
      {
         obj<-obj+
           ifelse((E1-plant$turbineAxisElevation-plant$designHead[1])<0,abs(E1-plant$turbineAxisElevation-plant$designHead[1])*1e5,0)+
           ifelse((E2-plant$turbineAxisElevation-plant$designHead[1])<0,abs(E2-plant$turbineAxisElevation-plant$designHead[1])*1e5,0)+
           ifelse((E1-plant$turbineAxisElevation-plant$designHead[2])>0,abs(E1-plant$turbineAxisElevation-plant$designHead[2])*1e5,0)+
           ifelse((E2-plant$turbineAxisElevation-plant$designHead[2])>0,abs(E2-plant$turbineAxisElevation-plant$designHead[2])*1e5,0)
      }
      return(abs(obj))
   }

   ####### Hydropower Reservoir #########
   if(type=='hydropower' && all(demand==0))
   {
      for (iter in 1:ifelse(is.na(initialStorage), 10, 1))
      {
         ####### First time step #########
         if (iter > 1) {storage[1] <- storage[length(dates)]}
         t<-1
         if((storage[1]+inflow[1]-geometry$deadStorage)>plant$designFlow[1]/conversionFactor)
         {
            opt<-isres(x0=plant$designFlow[1]/conversionFactor,f0Hydropower,
                       lower=plant$designFlow[1]/conversionFactor,
                       upper=min(plant$designFlow[2]/conversionFactor,ifelse(is.na(initialStorage),geometry$capacity,initialStorage)+inflow[1]-geometry$deadStorage),
                       maxeval=200,pop.size=50)
            if(abs(opt$value)<plant$installedCapacity) R[1]<-opt$par[1]
         }
         if((storage[1]+inflow[1]-R[1])>geometry$capacity)
         {
            E1<-storageElevationFunction(storage[1])
            seep1 <- storage[1]*seepageFraction
            spill[1]<-storage[1]+inflow[1]-R[1]-geometry$capacity
            A1<-storageAreaFunction(storage[1])
            storage[1]<-geometry$capacity
            seep2 <- storage[1]*seepageFraction
            A2<-storageAreaFunction(storage[1])
            loss[1]<-(A1+A2)*netEvaporation[1]/2
            seepage[1] <- (seep1+seep2)/2
            if ((storage[1]-seepage[1]-loss[1]) < 0)
            {
                seepage[1]<-seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
                loss[1]   <-loss[1]  -(seepage[1]+loss[1]-storage[1])*loss[1]   /(seepage[1]+loss[1])
            }
            storage[1]<-storage[1]-seepage[1]-loss[1]
            E2<-storageElevationFunction(storage[1])
         }
         if((storage[1]+inflow[1]-R[1])<geometry$deadStorage)
         { 
            E1<-storageElevationFunction(storage[1])
            spill[1]<-0
            seep1 <- storage[1]*seepageFraction
            A1<-storageAreaFunction(storage[1])
            storage[1]<-storage[1]+inflow[1]-R[1]
            seep2 <- storage[1]*seepageFraction
            A2<-storageAreaFunction(storage[1])
            loss[1]<-(A1+A2)*netEvaporation[1]/2
            seepage[1] <- (seep1+seep2)/2
            if ((storage[1]-seepage[1]-loss[1]) < 0)
            {
                seepage[1]<-seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
                loss[1]   <-loss[1]  -(seepage[1]+loss[1]-storage[1])*loss[1]   /(seepage[1]+loss[1])
            }
            storage[1]<-storage[1]-seepage[1]-loss[1]
            E2<-storageElevationFunction(storage[1])
         }
         if((storage[1]+inflow[1]-R[1])<geometry$capacity && (storage[1]+inflow[1]-R[1])>geometry$deadStorage)
         {
            E1<-storageElevationFunction(storage[1])
            spill[1]<-0
            seep1 <- storage[1]*seepageFraction
            A1<-storageAreaFunction(storage[1])
            storage[1]<-storage[1]+inflow[1]-R[1]
            seep2 <- storage[1]*seepageFraction
            A2<-storageAreaFunction(storage[1])
            loss[1]<-(A1+A2)*netEvaporation[1]/2
            seepage[1] <- (seep1+seep2)/2
            if ((storage[1]-seepage[1]-loss[1]) < 0)
            {
                seepage[1]<-seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
                loss[1]   <-loss[1]  -(seepage[1]+loss[1]-storage[1])*loss[1]   /(seepage[1]+loss[1])
            }
            storage[1]<-storage[1]-seepage[1]-loss[1]
            E2<-storageElevationFunction(storage[1])
         }
         if(abs(opt$value)<plant$installedCapacity)
         {
            RCMS[1]<-R[1]*conversionFactor
            phi<-ifelse(RCMS[1]==0,0,approxExtrap(x=plant$efficiency[,1],y=plant$efficiency[,2],xout=RCMS[1])$y)
            totalHeadLoss<-plant$loss+ifelse(penstock$length>0,(10.67*penstock$length/penstock$diameter^4.8704)*(RCMS[1]/penstock$roughness)^1.852,0)
            grossHead<-mean(c(E1,E2))-ifelse(plant$submerged,max(dischargeElevationFunction(R[1]),plant$turbineAxisElevation),plant$turbineAxisElevation)-totalHeadLoss
            power[1]<-9806*phi*RCMS[1]*grossHead/1e6
         }
         
         ####### second time step and beyond #########
         for (t in 2:length(dates))
         {
            if((storage[t-1]+inflow[t-1]-geometry$deadStorage)>plant$designFlow[1]/conversionFactor)
            {
               opt<-isres(x0=plant$designFlow[1]/conversionFactor,fHydropower,
                          lower=plant$designFlow[1]/conversionFactor,
                          upper=min(plant$designFlow[2]/conversionFactor,storage[t-1]+inflow[t-1]-geometry$deadStorage),
                          maxeval=200,pop.size=50)
               if(abs(opt$value)<plant$installedCapacity) R[t]<-opt$par[1]
            }
            if((storage[t-1]+inflow[t-1]-R[t])>geometry$capacity)
            {
               E1<-storageElevationFunction(storage[t-1])
               seep1 <- storage[t-1]*seepageFraction
               spill[t]<-storage[t-1]+inflow[t-1]-R[t]-geometry$capacity
               A1<-storageAreaFunction(storage[t-1])
               storage[t]<-geometry$capacity
               seep2 <- storage[t]*seepageFraction
               A2<-storageAreaFunction(storage[t])
               loss[t-1]<-(A1+A2)*netEvaporation[t-1]/2
               seepage[t]<-(seep1+seep2)/2
               if ((storage[t]-seepage[t]-loss[t]) < 0)
               {
                   seepage[t]<-seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
                   loss[t]   <-loss[t]  -(seepage[t]+loss[t]-storage[t])*loss[t]   /(seepage[t]+loss[t])
               }
               storage[t]<-storage[t]-seepage[t]-loss[t]
               E2<-storageElevationFunction(storage[t])
            }
            if((storage[t-1]+inflow[t-1]-R[t])<geometry$deadStorage)
            {
               E1<-storageElevationFunction(storage[t-1])
               spill[t]<-0
               seep1 <- storage[t-1]*seepageFraction
               A1<-storageAreaFunction(storage[t-1])
               storage[t]<-storage[t-1]+inflow[t-1]-R[t]
               seep2 <- storage[t]*seepageFraction
               A2<-storageAreaFunction(storage[t])
               loss[t-1]<-(A1+A2)*netEvaporation[t-1]/2
               seepage[t]<-(seep1+seep2)/2
               if ((storage[t]-seepage[t]-loss[t]) < 0)
               {
                   seepage[t]<-seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
                   loss[t]   <-loss[t]  -(seepage[t]+loss[t]-storage[t])*loss[t]   /(seepage[t]+loss[t])
               }
               storage[t]<-storage[t]-seepage[t]-loss[t]
               E2<-storageElevationFunction(storage[t])
            }
            if((storage[t-1]+inflow[t-1]-R[t])<geometry$capacity && (storage[t-1]+inflow[t-1]-R[t])>geometry$deadStorage)
            {
               E1<-storageElevationFunction(storage[t-1])
               spill[t]<-0
               seep1 <- storage[t-1]*seepageFraction
               A1<-storageAreaFunction(storage[t-1])
               storage[t]<-storage[t-1]+inflow[t-1]-R[t]
               seep2 <- storage[t]*seepageFraction
               A2<-storageAreaFunction(storage[t])
               loss[t-1]<-(A1+A2)*netEvaporation[t-1]/2
               seepage[t]<-(seep1+seep2)/2
               if ((storage[t]-seepage[t]-loss[t]) < 0)
               {
                   seepage[t]<-seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
                   loss[t]   <-loss[t]  -(seepage[t]+loss[t]-storage[t])*loss[t]   /(seepage[t]+loss[t])
               }
               storage[t]<-storage[t]-seepage[t]-loss[t]
               E2<-storageElevationFunction(storage[t])
            }
            if(abs(opt$value)<plant$installedCapacity)
            {
               RCMS[t]<-R[t]*conversionFactor
               phi<-ifelse(RCMS[t]==0,0,approxExtrap(x=plant$efficiency[,1],y=plant$efficiency[,2],xout=RCMS[t])$y)
               totalHeadLoss<-plant$loss+ifelse(penstock$length>0,(10.67*penstock$length/penstock$diameter^4.8704)*(RCMS[t]/penstock$roughness)^1.852,0)
               grossHead<-mean(c(E1,E2))-ifelse(plant$submerged,max(dischargeElevationFunction(R[t]),plant$turbineAxisElevation),plant$turbineAxisElevation)-totalHeadLoss
               power[t]<-9806*phi*RCMS[t]*grossHead/1e6
            }
            
         }
      }
      inflow<-as.data.frame(inflow);rownames(inflow)<-dates
      storage<-as.data.frame(storage);rownames(storage)<-dates
      spill<-as.data.frame(spill);rownames(spill)<-dates
      R<-as.data.frame(R);rownames(R)<-dates
      seepage<-as.data.frame(seepage);rownames(seepage)<-dates
      loss<-as.data.frame(loss);rownames(loss)<-dates
      power<-as.data.frame(power);rownames(power)<-dates
      sim<-list(inflow=inflow,storage=storage,spill=spill,release=R,seepage=seepage,loss=loss,power=power)
      return(sim)
   }

   ####### Storage Reservoir #########
   if(type=='storage')
   {
      for (iter in 1:ifelse(is.na(initialStorage), 10, 1))
      {
          ####### First time step simulation #########
          if (iter > 1) {storage[1] <- storage[length(dates)]}
          if ((inflow[1]+storage[1]-D[1]) > geometry$capacity)
          {
              seep1 <- storage[1]*seepageFraction
              A1 <- storageAreaFunction(storage[1])
              spill[1] <- inflow[1]+storage[1]-geometry$capacity
              R[1] <- D[1]
              storage[1] <- geometry$capacity
              seep2 <- storage[1]*seepageFraction
              A2 <- storageAreaFunction(storage[1])
              loss[1] <- netEvaporation[1]*(A1+A2)/2
              seepage[1] <- (seep1+seep2)/2
              if ((storage[1]-seepage[1]-loss[1]) < 0)
              {
                  seepage[1] <- seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
                  loss[1] <- loss[1]-(seepage[1]+loss[1]-storage[1])*loss[1]/(seepage[1]+loss[1])
              }
              storage[1] <- storage[1]-seepage[1]-loss[1]
          }
          if ((inflow[1]+storage[1]-D[1]) < geometry$deadStorage)
          {
              seep1 <- storage[1]*seepageFraction
              A1 <- storageAreaFunction(storage[1])
              if ((inflow[1]+storage[1]) < geometry$deadStorage)
              {
                 R[1] <- 0
              }else{
                 R[1] <- inflow[1]+storage[1]-geometry$deadStorage
                 storage[1] <- geometry$deadStorage
              }
              seep2 <- storage[1]*seepageFraction
              A2 <- storageAreaFunction(storage[1])
              seepage[1] <- (seep1+seep2)/2
              loss[1] <- netEvaporation[1]*(A1+A2)/2
              if ((storage[1]-seepage[1]-loss[1]) < 0)
              {
                  seepage[1] <- seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
                  loss[1] <- loss[1]-(seepage[1]+loss[1]-storage[1])*loss[1]/(seepage[1]+loss[1])
              }
              storage[1] <- storage[1]-seepage[1]-loss[1]
              spill[1] <- 0
          }
          if ((inflow[1]+storage[1]-D[1]) > geometry$deadStorage && (inflow[1]+storage[1]-D[1]) < geometry$capacity)
          {
              seep1 <- storage[1]*seepageFraction
              A1 <- storageAreaFunction(storage[1])
              R[1] <- D[1]
              storage[1] <- inflow[1]+storage[1]-R[1]
              seep2 <- storage[1]*seepageFraction
              A2 <- storageAreaFunction(storage[1])
              loss[1] <- netEvaporation[1]*(A1+A2)/2
              seepage[1] <- (seep1+seep2)/2
              if ((storage[1]-seepage[1]-loss[1]) < 0)
              {
                  seepage[1] <- seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
                  loss[1] <- loss[1]-(seepage[1]+loss[1]-storage[1])*loss[1]/(seepage[1]+loss[1])
              }
              storage[1] <- storage[1]-seepage[1]-loss[1]
              spill[1] <- 0
          }

          ####### second time step and beyond #########
          for (t in 2:length(dates))
          {
              if ((inflow[t]+storage[t-1]-D[t]) > geometry$capacity) 
              {
                  seep1 <- storage[t-1]*seepageFraction
                  A1 <- storageAreaFunction(storage[t-1])
                  spill[t] <- inflow[t]+storage[t-1]-geometry$capacity
                  R[t] <- D[t]
                  storage[t] <- geometry$capacity
                  seep2 <- storage[t]*seepageFraction
                  A2 <- storageAreaFunction(storage[t])
                  seepage[t] <- (seep1+seep2)/2
                  loss[t] <- (netEvaporation[t]+netEvaporation[t-1])*(A1+A2)/4
                  if ((storage[t]-seepage[t]-loss[t]) < 0)
                  {
                     seepage[t] <- seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
                     loss[t] <- loss[t]-(seepage[t]+loss[t]-storage[t])*loss[t]/(seepage[t]+loss[t])
                  }
                  storage[t] <- storage[t]-loss[t]-seepage[t]
              }
              if ((inflow[t]+storage[t-1]-D[t]) < geometry$deadStorage)
              {
                  seep1 <- storage[t-1]*seepageFraction
                  A1 <- storageAreaFunction(storage[t-1])
                  if ((inflow[t]+storage[t-1]) < geometry$deadStorage)
                  {
                     R[t] <- 0
                     storage[t] <- storage[t-1]
                  }else {
                     R[t] <- inflow[t]+storage[t-1]-geometry$deadStorage
                     storage[t] <- geometry$deadStorage
                  }
                  seep2 <- storage[t]*seepageFraction
                  A2 <- storageAreaFunction(storage[t])
                  seepage[t] <- (seep1+seep2)/2
                  loss[t] <- (netEvaporation[t]+netEvaporation[t-1])*(A1+A2)/4
                  if ((storage[t]-seepage[t]-loss[t]) < 0)
                  {
                     seepage[t] <- seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
                     loss[t] <- loss[t]-(seepage[t]+loss[t]-storage[t])*loss[t]/(seepage[t]+loss[t])
                  }
                  storage[t] <- storage[t]-loss[t]-seepage[t]
                  spill[t] <- 0
              }
              if ((inflow[t]+storage[t-1]-D[t]) > geometry$deadStorage && (inflow[t]+storage[t-1]-D[t]) < geometry$capacity)
              {
                 seep1 <- storage[t-1]*seepageFraction
                 A1 <- storageAreaFunction(storage[t-1])
                 R[t] <- D[t]
                 storage[t] <- inflow[t]+storage[t-1]-R[t]
                 seep2 <- storage[t]*seepageFraction
                 A2 <- storageAreaFunction(storage[t])
                 loss[t] <- (netEvaporation[t]+netEvaporation[t-1])*(A1+A2)/4
                 seepage[t] <- (seep1+seep2)/2
                 if ((storage[t]-seepage[t]-loss[t]) < 0) 
                 {
                    seepage[t] <- seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
                    loss[t] <- loss[t]-(seepage[t]+loss[t]-storage[t])*loss[t]/(seepage[t]+loss[t])
                 }
                 storage[t] <- storage[t]-loss[t]-seepage[t]
                 spill[t] <- 0
              }
          }
      }
      R<-splitter(demand, priority, R) 
      inflow<-as.data.frame(inflow);rownames(inflow)<-dates
      storage<-as.data.frame(storage);rownames(storage)<-dates
      spill<-as.data.frame(spill);rownames(spill)<-dates
      R<-as.data.frame(R);rownames(R)<-dates
      seepage<-as.data.frame(seepage);rownames(seepage)<-dates
      loss<-as.data.frame(loss);rownames(loss)<-dates
      power<-as.data.frame(power);rownames(power)<-dates
      sim<-list(inflow=inflow,storage=storage,spill=spill,release=R,seepage=seepage,loss=loss,power=power)
      return(sim)
   }

   ####### Storage-Hydropower Reservoir #########
   if(type=='hydropower' && !all(demand==0))
   {
      for (iter in 1:ifelse(is.na(initialStorage), 10, 1))
      {
          ####### First time step #########
          if (iter > 1) {storage[1] <- storage[length(dates)]}
          if ((inflow[1]+storage[1]-D[1]) > geometry$capacity)
          {
              E1<- storageElevationFunction(storage[1])
              seep1 <- storage[1]*seepageFraction
              A1 <- storageAreaFunction(storage[1])
              spill[1] <- inflow[1]+storage[1]-geometry$capacity
              R[1] <- D[1]
              storage[1] <- geometry$capacity
              seep2 <- storage[1]*seepageFraction
              A2 <- storageAreaFunction(storage[1])
              loss[1] <- netEvaporation[1]*(A1+A2)/2
              seepage[1] <- (seep1+seep2)/2
              if ((storage[1]-seepage[1]-loss[1]) < 0)
              {
                  seepage[1] <- seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
                  loss[1] <- loss[1]-(seepage[1]+loss[1]-storage[1])*loss[1]/(seepage[1]+loss[1])
              }
              storage[1] <- storage[1]-seepage[1]-loss[1]
              E2<- storageElevationFunction(storage[1])
          }
          if ((inflow[1]+storage[1]-D[1]) < geometry$deadStorage)
          {
              E1<- storageElevationFunction(storage[1])
              seep1 <- storage[1]*seepageFraction
              A1 <- storageAreaFunction(storage[1])
              if ((inflow[1]+storage[1]) < geometry$deadStorage)
              {
                 R[1] <- 0
              }else{
                 R[1] <- inflow[1]+storage[1]-geometry$deadStorage
                 storage[1] <- geometry$deadStorage
              }
              seep2 <- storage[1]*seepageFraction
              A2 <- storageAreaFunction(storage[1])
              seepage[1] <- (seep1+seep2)/2
              loss[1] <- netEvaporation[1]*(A1+A2)/2
              if ((storage[1]-seepage[1]-loss[1]) < 0)
              {
                  seepage[1] <- seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
                  loss[1] <- loss[1]-(seepage[1]+loss[1]-storage[1])*loss[1]/(seepage[1]+loss[1])
              }
              storage[1] <- storage[1]-seepage[1]-loss[1]
              E2<- storageElevationFunction(storage[1])
              spill[1] <- 0
          }
          if ((inflow[1]+storage[1]-D[1]) > geometry$deadStorage && (inflow[1]+storage[1]-D[1]) < geometry$capacity)
          {
              E1<- storageElevationFunction(storage[1])
              seep1 <- storage[1]*seepageFraction
              A1 <- storageAreaFunction(storage[1])
              R[1] <- D[1]
              storage[1] <- inflow[1]+storage[1]-R[1]
              seep2 <- storage[1]*seepageFraction
              A2 <- storageAreaFunction(storage[1])
              loss[1] <- netEvaporation[1]*(A1+A2)/2
              seepage[1] <- (seep1+seep2)/2
              if ((storage[1]-seepage[1]-loss[1]) < 0)
              {
                  seepage[1] <- seepage[1]-(seepage[1]+loss[1]-storage[1])*seepage[1]/(seepage[1]+loss[1])
                  loss[1] <- loss[1]-(seepage[1]+loss[1]-storage[1])*loss[1]/(seepage[1]+loss[1])
              }
              storage[1] <- storage[1]-seepage[1]-loss[1]
              E2<- storageElevationFunction(storage[1])
              spill[1] <- 0
          }

          if(R[1]*conversionFactor>plant$designFlow[1])
          {
             opt<-isres(x0=plant$designFlow[1]/conversionFactor,f0HydropowerStorage,
                        lower=plant$designFlow[1]/conversionFactor,
                        upper=min(R[1],ifelse(is.na(initialStorage),geometry$capacity,initialStorage)+inflow[1]-geometry$deadStorage,plant$designFlow[2]/conversionFactor),
                        maxeval=200,pop.size=50)
             if(abs(opt$value)<plant$installedCapacity) RCMS[1]<-opt$par[1]*conversionFactor
          }

          phi<-ifelse(RCMS[1]==0,0,approxExtrap(x=plant$efficiency[,1],y=plant$efficiency[,2],xout=RCMS[1])$y)
          totalHeadLoss<-plant$loss+ifelse(penstock$length>0,(10.67*penstock$length/penstock$diameter^4.8704)*(RCMS[1]/penstock$roughness)^1.852,0)
          grossHead<-mean(c(E1,E2))-totalHeadLoss-max(plant$turbineAxisElevation,ifelse(plant$submerged,dischargeElevationFunction(RCMS[1]),plant$turbineAxisElevation))
          power[1]<-9806*phi*RCMS[1]*grossHead/1e6
          
          ####### Second time step and beyond #########
          for (t in 2:length(dates))
          {
              if ((inflow[t]+storage[t-1]-D[t]) > geometry$capacity) 
              {
                  E1<-storageElevationFunction(storage[t-1])
                  seep1 <- storage[t-1]*seepageFraction
                  A1 <- storageAreaFunction(storage[t-1])
                  spill[t] <- inflow[t]+storage[t-1]-geometry$capacity
                  R[t] <- D[t]
                  storage[t] <- geometry$capacity
                  seep2 <- storage[t]*seepageFraction
                  A2 <- storageAreaFunction(storage[t])
                  seepage[t] <- (seep1+seep2)/2
                  loss[t] <- (netEvaporation[t]+netEvaporation[t-1])*(A1+A2)/4
                  if ((storage[t]-seepage[t]-loss[t]) < 0)
                  {
                     seepage[t] <- seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
                     loss[t] <- loss[t]-(seepage[t]+loss[t]-storage[t])*loss[t]/(seepage[t]+loss[t])
                  }
                  storage[t] <- storage[t]-loss[t]-seepage[t]
                  E2<-storageElevationFunction(storage[t])
              }
              if ((inflow[t]+storage[t-1]-D[t]) < geometry$deadStorage)
              {
                  E1<-storageElevationFunction(storage[t-1])
                  seep1 <- storage[t-1]*seepageFraction
                  A1 <- storageAreaFunction(storage[t-1])
                  if ((inflow[t]+storage[t-1]) < geometry$deadStorage)
                  {
                     R[t] <- 0
                     storage[t] <- storage[t-1]
                  }else {
                     R[t] <- inflow[t]+storage[t-1]-geometry$deadStorage
                     storage[t] <- geometry$deadStorage
                  }
                  seep2 <- storage[t]*seepageFraction
                  A2 <- storageAreaFunction(storage[t])
                  seepage[t] <- (seep1+seep2)/2
                  loss[t] <- (netEvaporation[t]+netEvaporation[t-1])*(A1+A2)/4
                  if ((storage[t]-seepage[t]-loss[t]) < 0)
                  {
                     seepage[t] <- seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
                     loss[t] <- loss[t]-(seepage[t]+loss[t]-storage[t])*loss[t]/(seepage[t]+loss[t])
                  }
                  storage[t] <- storage[t]-loss[t]-seepage[t]
                  E2<-storageElevationFunction(storage[t])
                  spill[t] <- 0
              }
              if ((inflow[t]+storage[t-1]-D[t]) > geometry$deadStorage && (inflow[t]+storage[t-1]-D[t]) < geometry$capacity)
              {
                 E1<-storageElevationFunction(storage[t-1])
                 seep1 <- storage[t-1]*seepageFraction
                 A1 <- storageAreaFunction(storage[t-1])
                 R[t] <- D[t]
                 storage[t] <- inflow[t]+storage[t-1]-R[t]
                 seep2 <- storage[t]*seepageFraction
                 A2 <- storageAreaFunction(storage[t])
                 loss[t] <- (netEvaporation[t]+netEvaporation[t-1])*(A1+A2)/4
                 seepage[t] <- (seep1+seep2)/2
                 if ((storage[t]-seepage[t]-loss[t]) < 0) 
                 {
                    seepage[t] <- seepage[t]-(seepage[t]+loss[t]-storage[t])*seepage[t]/(seepage[t]+loss[t])
                    loss[t] <- loss[t]-(seepage[t]+loss[t]-storage[t])*loss[t]/(seepage[t]+loss[t])
                 }
                 storage[t] <- storage[t]-loss[t]-seepage[t]
                 E2<-storageElevationFunction(storage[t])
                 spill[t] <- 0
              }

              if(R[t]*conversionFactor>plant$designFlow[1])
              {
                 opt<-isres(x0=plant$designFlow[1]/conversionFactor,fHydropowerStorage,
                            lower=plant$designFlow[1]/conversionFactor,
                            upper=min(R[t],storage[t-1]+inflow[t]-geometry$deadStorage,plant$designFlow[2]/conversionFactor),
                            maxeval=200,pop.size=50)
                 if(abs(opt$value)<plant$installedCapacity) RCMS[t]<-opt$par[1]*conversionFactor
              }

              phi<-ifelse(RCMS[t]==0,0,approxExtrap(x=plant$efficiency[,1],y=plant$efficiency[,2],xout=RCMS[t])$y)
              totalHeadLoss<-plant$loss+ifelse(penstock$length>0,(10.67*penstock$length/penstock$diameter^4.8704)*(RCMS[t]/penstock$roughness)^1.852,0)
              grossHead<-mean(c(E1,E2))-totalHeadLoss-max(plant$turbineAxisElevation,ifelse(plant$submerged,dischargeElevationFunction(RCMS[t]),plant$turbineAxisElevation))
              power[t]<-9806*phi*RCMS[t]*grossHead/1e6
          }
      }
      R<-splitter(demand,priority,R) 
      inflow<-as.data.frame(inflow);rownames(inflow)<-dates
      storage<-as.data.frame(storage);rownames(storage)<-dates
      spill<-as.data.frame(spill);rownames(spill)<-dates
      R<-as.data.frame(R);rownames(R)<-dates
      seepage<-as.data.frame(seepage);rownames(seepage)<-dates
      loss<-as.data.frame(loss);rownames(loss)<-dates
      power<-as.data.frame(power);rownames(power)<-dates
      demand<-as.data.frame(demand);rownames(demand)<-dates
      sim<-list(inflow=inflow,storage=storage,spill=spill,release=R,seepage=seepage,loss=loss,power=power)
      return(sim)
   }
}