addObjectToArea <-
function(area,object)
{
   if(missing(area))
   {
      stop("Area is missing !")
   }
   if(missing(object))
   {
      stop("object to be added to area is missing!")
   }
   
   simulationSteps<-length(area$operation$simulatio$dates)
   dates<-area$operation$simulatio$dates

   if(inherits(object,"createAquifer"))
   {
      object$operation$outflow<-data.frame(outflow=rep(0,simulationSteps))
      rownames(object$operation$outflow)<-dates
      if(!all(is.na(object$operation$rechargeTS)))
      {
         if(length(object$operation$rechargeTS)==simulationSteps)
         {
            rechargeTS<-data.frame(object$operation$rechargeTS)
            colnames(rechargeTS)<-'natural recharge'
            rownames(rechargeTS)<-dates
            object$operation$inflow<-rechargeTS
         }
         if(length(object$operation$rechargeTS)!=simulationSteps)
         {
            if(length(object$operation$rechargeTS)>simulationSteps) rechargeTS<-object$operation$rechargeTS[1:simulationSteps]
            if(length(object$operation$rechargeTS)<simulationSteps)
            {
               rechargeTS<-rep(0,simulationSteps)
               rechargeTS[1:length(object$operation$rechargeTS)]<-object$operation$rechargeTS
            }
            rechargeTS<-data.frame(rechargeTS)
            colnames(rechargeTS)<-'natural recharge'
            rownames(rechargeTS)<-dates
            object$operation$inflow<-rechargeTS
            warning('recharge flow vector is reshaped! miss matched dimenssion with the number of simulation steps!')
         }
      }else{
         rechargeTS<-data.frame(rep(0,simulationSteps))
         colnames(rechargeTS)<-'zero flow'
         rownames(rechargeTS)<-dates
         object$operation$inflow<-rechargeTS
      }
      object$operation$rechargeTS<-NULL
      i<-length(area$operation$aquifers)+1
      area$operation$aquifers[[i]]<-object
   }

   if(inherits(object,"createRiver"))
   {
      object$operation$outflow<-data.frame(outflow=rep(0,simulationSteps))
      rownames(object$operation$outflow)<-dates
      if(all(is.na(object$operation$discharge)))
      {
         object$operation$discharge<-rep(0,simulationSteps)
      }
      if(length(object$operation$discharge)==simulationSteps)
      {
          riverDischarge            <-data.frame(discharge=object$operation$discharge)
          rownames(riverDischarge)  <-dates
          object$operation$inflow   <-riverDischarge
      }else{
          if(length(object$operation$discharge)>simulationSteps) riverDischarge<-object$operation$discharge[1:simulationSteps]
          if(length(object$operation$discharge)<simulationSteps)
          {
             riverDischarge<-rep(0,simulationSteps)
             riverDischarge[1:length(object$operation$discharge)]<-object$operation$discharge
          }
          riverDischarge<-data.frame(discharge=riverDischarge)
          rownames(riverDischarge)<-dates
          object$operation$inflow<-riverDischarge
          warning('recharge flow vector is reshaped! miss matched dimenssion with the number of simulation steps!')
      }
      object$operation$discharge<-NULL
      i<-length(area$operation$rivers)+1
      area$operation$rivers[[i]]<-object
   }

   if(inherits(object,"createReservoir"))
   {
      if(!all(is.na(object$operation$netEvaporation)))
      {
         if(length(object$operation$netEvaporation)==simulationSteps)
         {
            NET<-object$operation$netEvaporation
         }else{
            if(length(object$operation$netEvaporation)>simulationSteps)
            {
               NET<-object$operation$netEvaporation[1:simulationSteps]
            }
            if(length(object$operation$netEvaporation)<simulationSteps)
            {
               temp<-rep(0,simulationSteps)
               temp[1:length(object$operation$netEvaporation)]<-object$operation$netEvaporation
               NET<-temp
            }
            warning('net evaporation vector is reshaped! miss matched dimenssion with the number of simulation steps!')
         }
      }else{
         NET<-rep(0,simulationSteps)
      }
      NET                                   <-data.frame(netEvaporation=NET)
      rownames(NET)                         <-dates
      object$operation$netEvaporation       <-NET
      object$operation$outflow              <-data.frame(outflow=rep(0,simulationSteps))
      object$operation$inflow               <-data.frame(inflow =rep(0,simulationSteps))
      rownames(object$operation$outflow)    <-dates
      rownames(object$operation$inflow)     <-dates
      i                                     <-length(area$operation$reservoirs)+1
      area$operation$reservoirs[[i]]        <-object
   }

   if(inherits(object,"createDiversion"))
   {
      object$operation$outflow           <-data.frame(outflow=rep(0,simulationSteps))
      object$operation$inflow            <-data.frame(inflow=rep(0,simulationSteps))
      rownames(object$operation$outflow) <-dates
      rownames(object$operation$inflow)  <-dates
      i                                  <-length(area$operation$diversions)+1
      area$operation$diversions[[i]]     <-object
   }

   if(inherits(object,"createJunction"))
   {
      object$operation$outflow           <-data.frame(outflow=rep(0,simulationSteps))
      object$operation$inflow            <-data.frame(inflow=rep(0,simulationSteps))
      rownames(object$operation$outflow) <-dates
      rownames(object$operation$inflow)  <-dates
      i                                  <-length(area$operation$junctions)+1
      area$operation$junctions[[i]]      <-object
   }

   if(inherits(object,"createDemandSite"))
   {
      if(all(is.na(object$operation$demandTS)))
      {
         demandTS<-rep(object$operation$demandParams$waterUseRate*
                       object$operation$demandParams$waterVariation*
                       object$operation$demandParams$cropArea/100,
                       floor(simulationSteps/length(object$operation$demandParams$waterVariation))+1)[1:simulationSteps]
      }else{
         demandTS<-object$operation$demandTS
         if(length(demandTS)!=simulationSteps)
          {
             if(length(demandTS)>simulationSteps)
             {
                demandTS-demandTS[1:simulationSteps]
             }
             if(length(demandTS)<simulationSteps)
             {
                demandTS<-rep(demandTS,floor(simulationSteps/length(demandTS))+1)[1:simulationSteps]
             }
             warning('demand vector is reshaped and replicated! miss matched dimenssion with the number of simulation steps!')
          }
      }
      demandTS<-data.frame(demand=demandTS)
      rownames(demandTS)                  <-dates
      object$operation$demandTS           <-demandTS
      object$operation$outflow            <-data.frame(outflow=rep(0,simulationSteps))
      object$operation$inflow             <-data.frame(inflow=rep(0,simulationSteps))
      rownames(object$operation$outflow)  <-dates
      rownames(object$operation$inflow)   <-dates
      i                                   <-length(area$operation$demands)+1
      area$operation$demands[[i]]         <-object
   }
   return(area)
}