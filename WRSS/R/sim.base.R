sim.base <-
function(object)
{
    #-----------Initialization-----------
    nRes<-length(object$operation$reservoirs)
    nRiv<-length(object$operation$rivers)
    nAuq<-length(object$operation$aquifers)
    nJun<-length(object$operation$junctions)
    nDiv<-length(object$operation$diversions)
    nDem<-length(object$operation$demands)
    simulation<-object$operation$simulation
    duration<-length(object$operation$simulation$dates)
    
    #-----------Creating a reference matrix -----------
    
    matCode<-matrix(NA,5,nRes+nRiv+nAuq+nJun+nDiv+nDem)
    lab<-c()
    i<-0;j<-0;k<-0;l<-0;m<-0;n<-0
    if(nRes>0){for(i in 1:nRes){matCode[1,i]          <-object$operation$reservoirs[[i]]$operation$label;matCode[2,i]          <-object$operation$reservoirs[[i]]$operation$downstream;   matCode   [3,i]          <-1;lab<-c(lab,object$operation$reservoirs[[i]]$operation$name);matCode[4,i]          <-object$operation$reservoirs[[i]]$operation$priority;matCode[5,i]          <-i}}
    if(nRiv>0){for(j in 1:nRiv){matCode[1,i+j]        <-object$operation$rivers    [[j]]$operation$label;matCode[2,i+j]        <-object$operation$rivers    [[j]]$operation$downstream;   matCode   [3,i+j]        <-2;lab<-c(lab,object$operation$rivers    [[j]]$operation$name);matCode[4,i+j]        <-object$operation$rivers    [[j]]$operation$priority;matCode[5,i+j]        <-j}}
    if(nAuq>0){for(k in 1:nAuq){matCode[1,i+j+k]      <-object$operation$aquifers  [[k]]$operation$label;matCode[2,i+j+k]      <-object$operation$aquifers  [[k]]$operation$leakageObject;matCode   [3,i+j+k]      <-3;lab<-c(lab,object$operation$aquifers  [[k]]$operation$name);matCode[4,i+j+k]      <-object$operation$aquifers  [[k]]$operation$priority;matCode[5,i+j+k]      <-k}}
    if(nDiv>0){for(l in 1:nDiv){matCode[1,i+j+k+l]    <-object$operation$diversions[[l]]$operation$label;matCode[2,i+j+k+l]    <-object$operation$diversions[[l]]$operation$downstream;   matCode   [3,i+j+k+l]    <-4;lab<-c(lab,object$operation$diversions[[l]]$operation$name);matCode[4,i+j+k+l]    <-object$operation$diversions[[l]]$operation$priority;matCode[5,i+j+k+l]    <-l}}
    if(nJun>0){for(m in 1:nJun){matCode[1,i+j+k+l+m]  <-object$operation$junctions [[m]]$operation$label;matCode[2,i+j+k+l+m]  <-object$operation$junctions [[m]]$operation$downstream;   matCode   [3,i+j+k+l+m]  <-5;lab<-c(lab,object$operation$junctions [[m]]$operation$name);matCode[4,i+j+k+l+m]  <-Inf                                                ;matCode[5,i+j+k+l+m]  <-m}}
    if(nDem>0){for(n in 1:nDem){matCode[1,i+j+k+l+m+n]<-object$operation$demands   [[n]]$operation$label;matCode[2,i+j+k+l+m+n]<-object$operation$demands   [[n]]$operation$downstream;   matCode   [3,i+j+k+l+m+n]<-0;lab<-c(lab,object$operation$demands   [[n]]$operation$name);matCode[4,i+j+k+l+m+n]<-object$operation$demands   [[n]]$operation$priority;matCode[5,i+j+k+l+m+n]<-n}}
    colnames(matCode)<-lab
    rownames(matCode)<-c('code','downstream','featureType','priority','address')
    resources<-which(!(matCode[3,]==0 | matCode[3,]==5))
    for(i in 1:length(resources))
    {
      currentResources<-matCode[,resources[i],drop=FALSE]
      if(nDem>0)
      {
        for (j in 1:nDem)
        {
          if(any(object$operation$demands[[j]]$operation$suppliers==currentResources[1]))
          {
            name<-colnames(matCode)[resources[i]]
            code<-matCode[1,resources[i]]
            downStream<-object$operation$demands[[j]]$operation$label
            type<-matCode[3,resources[i]]
            priority<-matCode[4,resources[i]]
            address<-matCode[5,resources[i]]
            codes<-as.matrix(c(code,downStream,type,priority,address))
            colnames(codes)<-name
            matCode<-cbind(matCode,codes)
          }
        }
      }
    }

    if(nRiv>0)
    {
       for(i in 1:nRiv)
       {
          if(!is.na(object$operation$rivers[[i]]$operation$seepageObject))
          {
             name<-object$operation$rivers[[i]]$operation$name
             code<-object$operation$rivers[[i]]$operation$label
             downStream<-object$operation$rivers[[i]]$operation$seepageObject
             type<-2
             priority<-Inf
             address<-i
             codes<-as.matrix(c(code,downStream,type,priority,address))
             colnames(codes)<-name
             matCode<-cbind(matCode,codes)
          }
       }
    }

    if(nRes>0)
    {
       for(i in 1:nRes)
       {
          if(!is.na(object$operation$reservoirs[[i]]$operation$seepageObject))
          {
             name<-object$operation$reservoirs[[i]]$operation$name
             code<-object$operation$reservoirs[[i]]$operation$label
             downStream<-object$operation$reservoirs[[i]]$operation$seepageObject
             type<-1
             priority<-object$operation$reservoirs[[i]]$operation$priority
             address<-i
             codes<-as.matrix(c(code,downStream,type,priority,address))
             colnames(codes)<-name
             matCode<-cbind(matCode,codes)
          }
       }
    }
    
    matCodeAllObjects<-matCode
    IDUP<-idUpstream<-which(is.na(match(matCode[1,],matCode[2,])))
    idUpstream<-idUpstream[which(!duplicated(matCode[1,idUpstream]))]
    names(idUpstream)<-colnames(matCode)[idUpstream]
    idUpstream<-idUpstream[sort(matCode[4,idUpstream],index.return = TRUE)$ix]
    
    #-----------Checking reference matrix -----------
    mat<-matCode
    repeat
    {
      ids<-which(is.na(match(mat[1,],mat[2,])))
      if((length(ids)==0) && (ncol(mat)>0)){stop('check objects connectivity!')}
      mat<-as.matrix(mat[,-ids])
      if(ncol(mat)==0){break}
    }
    junctions<-matCode[,which(matCode[3,]==5),drop=FALSE]
    if(ncol(junctions)>0){for(j in 1:ncol(junctions)){for (d in 1:nDem){if(any(object$operation$demands[[d]]$operation$suppliers==junctions[1,j])){stop('demands should be supplied by water resources else junctions!')}}}}
    
    #-----------defining functions for finding demands and setting time series to objects at downstream-----------
    collectDemands<-function(code)
    {
      demand<-data.frame(rep(NA,duration))
      priority<-c()
      name<-c()
      labelCode<-c()
      for(d in 1:nDem)
      {
        if(any(object$operation$demands[[d]]$operation$suppliers==code))
        {
          demand<-cbind(demand,object$operation$demands[[d]]$operation$demandTS-apply(object$operation$demands[[d]]$operation$inflow,1,sum))
          demand[which(demand<0,arr.ind=TRUE)]<-0
          priority<-c(priority,object$operation$demands[[d]]$operation$priority)
          name<-c(name,object$operation$demands[[d]]$operation$name)
          labelCode<-c(labelCode,object$operation$demands[[d]]$operation$label)
        }
      }
      if(ncol(demand)>1)
      {
        demand<-demand[,-1,drop=FALSE]
        colnames(demand)<-name
        return(list(demand=demand,priority=priority,label=labelCode))
      }else{
        demand<-data.frame(zero_demand=rep(0,duration))
        rownames(demand)<-object$operation$simulation$dates
        return(list(demand=demand,priority=Inf,label=NA))
      }
    }
    
    findANDset<-function(object,outflow,currentFeatureCodes,matCodeAllObjects)
    {
      if(!is.na(currentFeatureCodes[2]))
      {
        downstreamFeatureCodes<-matCodeAllObjects[,which(!is.na(match(matCodeAllObjects[1,],currentFeatureCodes[2])))]
        if(downstreamFeatureCodes[3]==0){object$operation$demands   [[downstreamFeatureCodes[5]]]$operation$inflow<-cbind(object$operation$demands   [[downstreamFeatureCodes[5]]]$operation$inflow,outflow)}
        if(downstreamFeatureCodes[3]==1){object$operation$reservoirs[[downstreamFeatureCodes[5]]]$operation$inflow<-cbind(object$operation$reservoirs[[downstreamFeatureCodes[5]]]$operation$inflow,outflow)}
        if(downstreamFeatureCodes[3]==2){object$operation$rivers    [[downstreamFeatureCodes[5]]]$operation$inflow<-cbind(object$operation$rivers    [[downstreamFeatureCodes[5]]]$operation$inflow,outflow)}
        if(downstreamFeatureCodes[3]==3){object$operation$aquifers  [[downstreamFeatureCodes[5]]]$operation$inflow<-cbind(object$operation$aquifers  [[downstreamFeatureCodes[5]]]$operation$inflow,outflow)}
        if(downstreamFeatureCodes[3]==4){object$operation$diversions[[downstreamFeatureCodes[5]]]$operation$inflow<-cbind(object$operation$diversions[[downstreamFeatureCodes[5]]]$operation$inflow,outflow)}
        if(downstreamFeatureCodes[3]==5){object$operation$junctions [[downstreamFeatureCodes[5]]]$operation$inflow<-cbind(object$operation$junctions [[downstreamFeatureCodes[5]]]$operation$inflow,outflow)}
      }
      return(object)
    }
    
    demandRouting<-function(inflow,demandTS,returnFlowFraction,currentFeatureCodes)
    {
      overflow<-as.matrix(ifelse(inflow>demandTS,inflow-demandTS,0))
      if(returnFlowFraction>0 | sum(overflow)>0)
      {
        currentName            <-object$operation$demands[[currentFeatureCodes[5]]]$operation$name
        downstreamName         <-lookupDownstreamName(currentFeatureCodes)
        if(returnFlowFraction>0) {returnFlow<-inflow*returnFlowFraction}else{returnFlow<-as.matrix(rep(0,duration))}
        totalOutFlow           <-as.matrix(apply(cbind(overflow,returnFlow),1,sum,na.rm=TRUE))
        colnames(totalOutFlow) <-paste('outflow (to: ',downstreamName,')',sep='')
        object$operation$demands[[currentFeatureCodes[5]]]$operation$outflow<-cbind(object$operation$demands[[currentFeatureCodes[5]]]$operation$outflow,totalOutFlow)
        if(!is.na(currentFeatureCodes[2]))
        {
          colnames(totalOutFlow)<-paste('inflow from(',currentName,')',sep='')
          object                <-findANDset(object,totalOutFlow,currentFeatureCodes,matCodeAllObjects)
        }
      }
      return(object)
    }
    
    lookupDownstreamName<-function(currentFeatureCodes)
    {
      if(!is.na(currentFeatureCodes[2]))
      {
        downstreamFeatureCodes<-matCodeAllObjects[,match(currentFeatureCodes[2],matCodeAllObjects[1,])]
        if(downstreamFeatureCodes[3]==0){name<-object$operation$demand    [[downstreamFeatureCodes[5]]]$operation$name}
        if(downstreamFeatureCodes[3]==1){name<-object$operation$reservoirs[[downstreamFeatureCodes[5]]]$operation$name}
        if(downstreamFeatureCodes[3]==2){name<-object$operation$rivers    [[downstreamFeatureCodes[5]]]$operation$name}
        if(downstreamFeatureCodes[3]==3){name<-object$operation$aquifers  [[downstreamFeatureCodes[5]]]$operation$name}
        if(downstreamFeatureCodes[3]==4){name<-object$operation$diversions[[downstreamFeatureCodes[5]]]$operation$name}
        if(downstreamFeatureCodes[3]==5){name<-object$operation$junctions [[downstreamFeatureCodes[5]]]$operation$name}
        return(name)
      }else{
        return(c('NA'))
      }
    }
    
    #-----------operation-----------
    repeat
    {
      for(r in 1:length(idUpstream))
      {
        #-----------finding demands for associated to current water resources-----------
        currentFeatureCodes<-matCode[,idUpstream[r]]
        if(!(currentFeatureCodes[3]==5 | currentFeatureCodes[3]==0))
        {
          demand  <-collectDemands(currentFeatureCodes[1])
          priority<-demand$priority
          label   <-demand$label
          demand  <-demand$demand
        }else{
          demand  <-NA
          priority<-NA
          label   <-NA
          demand  <-NA
        }
        
        #-----------demand operation-----------
        if(currentFeatureCodes[3]==0)
        {
          inflow             <-as.matrix(apply(object$operation$demands[[currentFeatureCodes[5]]]$operation$inflow,1,sum))
          demandTS           <-as.matrix(object$operation$demands[[currentFeatureCodes[5]]]$operation$demandTS)
          returnFlowFraction <-object$operation$demands[[currentFeatureCodes[5]]]$operation$returnFlowFraction
          object             <-demandRouting(inflow,demandTS,returnFlowFraction,currentFeatureCodes)
        }
        #-----------reservoir operation-----------
        if(currentFeatureCodes[3]==1)
        {
          inflow                <-apply(object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$inflow,1,sum)
          netEvaporation        <-object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$netEvaporation$netEvaporation
          geometry              <-object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$geometry
          initialStorage        <-object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$initialStorage
          seepageFraction       <-object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$seepageFraction
          currentName           <-object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$name
          downstreamName        <-lookupDownstreamName(currentFeatureCodes)
          type                  <-object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$type
          plant                 <-object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$plant
          penstock              <-object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$penstock
          sim_result            <-reservoirRouting(type,inflow,netEvaporation,demand,priority,seepageFraction,geometry,plant,penstock,initialStorage,simulation)
          object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$sim_result<-
                                       cbind(storage     = sim_result$storage             ,
                                             spill       = sim_result$spill               ,
                                             evaporation = sim_result$loss                ,
                                             release     = apply(sim_result$release,1,sum),
                                             seepage     = ifelse(seepageFraction>0,sim_result$seepage,0),
                                             power       = ifelse(type == 'hydropower',sim_result[[7]],0)[[1]])
          if(seepageFraction>0)
          {
            seepage<-as.matrix(sim_result$seepage)
            seepageObject         <-c(NA,object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$seepageObject,NA,NA,NA)
            seepageName           <-lookupDownstreamName(seepageObject)
            colnames(seepage)     <-paste('outflow (to: ',seepageName,')',sep='')
            object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$outflow<-cbind(outflow=object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$outflow,seepage)
            colnames(seepage)     <-paste('inflow from(',currentName,')',sep='')
            object                <-findANDset(object,seepage,seepageObject,matCodeAllObjects)
          }
          if(all(is.na(label)))
          {
            spill                 <-as.matrix(sim_result$spill)
            colnames(spill)       <-paste('outflow (to: ',downstreamName,')',sep='')
            object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$outflow<-cbind(object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$outflow,spill,sim_result$loss)
            colnames(spill)       <-paste('inflow from(',currentName,')',sep='')
            object                <-findANDset(object,spill,currentFeatureCodes,matCodeAllObjects)
          }else{
            spill               <-as.matrix(sim_result$spill)
            colnames(spill)     <-paste('outflow (to: ',downstreamName,')',sep='')
            object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$outflow<-cbind(object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$outflow,spill,sim_result$loss)
            colnames(spill)     <-paste('inflow from(',currentName,')',sep='')
            object              <-findANDset(object,spill,currentFeatureCodes,matCodeAllObjects)
            release             <-sim_result$release
            colnames(release)   <-paste('outflow (to: ',colnames(release),')',sep='')
            object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$outflow<-cbind(object$operation$reservoirs[[currentFeatureCodes[5]]]$operation$outflow,release)
            colnames(release)   <-rep(paste('inflow (from: ',currentName,')',sep=''),ncol(release))
            for(d in 1:ncol(release))
            {
              currentDemandCodes<-matCodeAllObjects[,match(label[d],matCodeAllObjects[1,])]
              object$operation$demands[[currentDemandCodes[5]]]$operation$inflow<-cbind(object$operation$demands[[currentDemandCodes[5]]]$operation$inflow,release[,d,drop=FALSE])
            }
          }  
        }
        
        #-----------river operation-----------
        if(currentFeatureCodes[3]==2)
        {
          currentName           <-object$operation$rivers[[currentFeatureCodes[5]]]$operation$name
          downstreamName        <-lookupDownstreamName(currentFeatureCodes)
          seepageFraction       <-object$operation$rivers[[currentFeatureCodes[5]]]$operation$seepageFraction
          seepageObject          <-c(NA,object$operation$rivers[[currentFeatureCodes[5]]]$operation$seepageObject,NA,NA,NA)
          seepageName           <-lookupDownstreamName(seepageObject)
          discharge             <-as.matrix(apply(object$operation$rivers[[currentFeatureCodes[5]]]$operation$inflow,1,sum))
          seepage               <-0 
          if(seepageFraction>0)
          {
            seepage<-seepageFraction*discharge
            discharge<-discharge-seepage
            colnames(seepage)    <-paste('outflow (to: ',seepageName,')',sep='')
            object$operation$rivers[[currentFeatureCodes[5]]]$operation$outflow<-cbind(object$operation$rivers[[currentFeatureCodes[5]]]$operation$outflow,seepage)
            colnames(seepage)    <-paste('inflow from(',currentName,')',sep='')
            object               <-findANDset(object,seepage,seepageObject,matCodeAllObjects)
          }
          if(all(is.na(label)))
          {
            colnames(discharge)    <-paste('outflow (to: ',downstreamName,')',sep='')
            object$operation$rivers[[currentFeatureCodes[5]]]$operation$outflow<-cbind(outflow=object$operation$rivers[[currentFeatureCodes[5]]]$operation$outflow,discharge)
            colnames(discharge)    <-paste('inflow from(',currentName,')',sep='')
            object                 <-findANDset(object,discharge,currentFeatureCodes,matCodeAllObjects)
          }else{
            sim_result          <-riverRouting(demand,priority,discharge-seepage,seepageFraction=0,simulation)
            release             <-sim_result$release
            outflow             <-sim_result$outflow
            colnames(release)   <-paste('outflow (to: ',colnames(release),')',sep='')
            colnames(outflow)   <-paste('outflow (to: ',downstreamName,')',sep='')
            object$operation$rivers[[currentFeatureCodes[5]]]$operation$outflow<-cbind(object$operation$rivers[[currentFeatureCodes[5]]]$operation$outflow,outflow,release)
            colnames(outflow)   <-paste('inflow from(',currentName,')',sep='')
            object              <-findANDset(object,outflow,currentFeatureCodes,matCodeAllObjects)
            colnames(release)   <-rep(paste('inflow(from: ',currentName,')',sep=''),ncol(release))
            for(d in 1:ncol(release))
            {
              currentDemandCodes<-matCodeAllObjects[,match(label[d],matCodeAllObjects[1,])]
              object$operation$demands[[currentDemandCodes[5]]]$operation$inflow<-cbind(object$operation$demands[[currentDemandCodes[5]]]$operation$inflow,release[,d,drop=FALSE])
            }
          }
        }   
        
        #-----------auqifer operation-----------
        if(currentFeatureCodes[3]==3)
        {
          inflow         <-apply(object$operation$aquifers[[currentFeatureCodes[5]]]$operation$inflow,1,sum)
          area           <-object$operation$aquifers[[currentFeatureCodes[5]]]$operation$area
          volume         <-object$operation$aquifers[[currentFeatureCodes[5]]]$operation$volume
          Sy             <-object$operation$aquifers[[currentFeatureCodes[5]]]$operation$Sy
          leakageFraction<-object$operation$aquifers[[currentFeatureCodes[5]]]$operation$leakageFraction
          initialStorage <-object$operation$aquifers[[currentFeatureCodes[5]]]$operation$initialStorage
          sim_result     <-aquiferRouting(demand,priority,area,volume,inflow,leakageFraction,initialStorage,Sy,simulation)
          object$operation$aquifers[[currentFeatureCodes[5]]]$operation$storage<-sim_result$storage
          currentName    <-object$operation$aquifers[[currentFeatureCodes[5]]]$operation$name
          downstreamName <-lookupDownstreamName(currentFeatureCodes)
          if(all(is.na(label)))
          {
            leakage          <-as.matrix(sim_result$leakage)
            colnames(leakage)<-paste('outflow (to: ',downstreamName,')',sep='')
            object$operation$aquifers[[currentFeatureCodes[5]]]$operation$outflow<-cbind(object$operation$aquifers[[currentFeatureCodes[5]]]$operation$outflow,leakage)
            colnames(leakage)<-paste('inflow from(',currentName,')',sep='')
            object           <-findANDset(object,leakage,currentFeatureCodes,matCodeAllObjects)
          }else{
            leakage          <-as.matrix(sim_result$leakage)
            colnames(leakage)<-paste('outflow (to: ',downstreamName,')',sep='')
            release<-Release <-sim_result$release
            colnames(release)<-paste('outflow (to: ',colnames(release),')',sep='')
            object$operation$aquifers[[currentFeatureCodes[5]]]$operation$outflow<-cbind(object$operation$aquifers[[currentFeatureCodes[5]]]$operation$outflow,leakage,release)
            colnames(leakage)<-paste('inflow from(',currentName,')',sep='')
            object           <-findANDset(object,leakage,currentFeatureCodes,matCodeAllObjects)
            colnames(release)<-rep(paste('inflow(from: ',currentName,')',sep=''),ncol(release))
            for(d in 1:ncol(release))
            {
              currentDemandCodes<-matCodeAllObjects[,match(label[d],matCodeAllObjects[1,])]
              object$operation$demands[[currentDemandCodes[5]]]$operation$inflow<-cbind(object$operation$demands[[currentDemandCodes[5]]]$operation$inflow,release[,d,drop=FALSE])
            }
          }  
        }
        
        #-----------diversion operation-----------
        if(currentFeatureCodes[3]==4)
        {
          inflow            <-apply(object$operation$diversions[[currentFeatureCodes[5]]]$operation$inflow,1,sum)
          capacity          <-object$operation$diversions[[currentFeatureCodes[5]]]$operation$capacity
          sim_result        <-diversionRouting(demand,priority,capacity,inflow,simulation)
          sim_result$release[which(is.nan(sim_result$release[,1])),]<-0
          object$operation$diversions[[currentFeatureCodes[5]]]$operation$sim_result<-sim_result
          currentName       <-object$operation$diversions[[currentFeatureCodes[5]]]$operation$name
          downstreamName    <-lookupDownstreamName(currentFeatureCodes)
          overflow          <-as.matrix(sim_result$overflow)
          colnames(overflow)<-paste('inflow from(',currentName,')',sep='') 
          object            <-findANDset(object,overflow,currentFeatureCodes,matCode)
          colnames(overflow)<-paste('outflow (to: ',downstreamName,')',sep='')
          object$operation$diversions[[currentFeatureCodes[5]]]$operation$outflow<-cbind(object$operation$diversions[[currentFeatureCodes[5]]]$operation$outflow,overflow)
          if(all(is.na(label)))
          {
            diverted          <-as.matrix(sim_result$diverted)
            divertObject          <-rep(NA,4)
            divertObject[2]       <-object$operation$diversions[[currentFeatureCodes[5]]]$operation$divertObject
            divertName        <-lookupDownstreamName(divertObject)
            colnames(diverted)<-paste('outflow (to: ',divertName,')',sep='')
            object$operation$diversions[[currentFeatureCodes[5]]]$operation$outflow<-cbind(object$operation$diversions[[currentFeatureCodes[5]]]$operation$outflow,diverted)
            colnames(diverted)<-paste('inflow from(',currentName,')',sep='')
            object            <-findANDset(object,diverted,divertObject,matCodeAllObjects)
          }else{
            release<-Release  <-sim_result$release
            colnames(release) <-paste('outflow (to: ',colnames(release),')',sep='')
            diverted          <-sim_result$diverted
            diverted          <-as.matrix(diverted-apply(release,1,sum))
            divertObject          <-rep(NA,4)
            divertObject[2]       <-object$operation$diversions[[currentFeatureCodes[5]]]$operation$divertObject
            divertName        <-lookupDownstreamName(divertObject)
            colnames(diverted)<-paste('outflow (to: ',divertName,')',sep='')
            object$operation$diversions[[currentFeatureCodes[5]]]$operation$outflow<-cbind(object$operation$diversions[[currentFeatureCodes[5]]]$operation$outflow,release,diverted)
            colnames(diverted)<-paste('inflow from(',currentName,')',sep='')
            object            <-findANDset(object,diverted,divertObject,matCodeAllObjects)
            colnames(Release) <-rep(paste('inflow(from: ',currentName,')',sep=''),ncol(release))
            for(d in 1:ncol(release))
            {
              currentDemandCodes<-matCodeAllObjects[,match(label[d],matCodeAllObjects[1,])]
              object$operation$demands[[currentDemandCodes[5]]]$operation$inflow<-cbind(object$operation$demands[[currentDemandCodes[5]]]$operation$inflow,release[,d,drop=FALSE])
            }
          }  
        }
        
        #-----------junction operation-----------
        if(currentFeatureCodes[3]==5)
        {
          currentName       <-object$operation$junctions[[currentFeatureCodes[5]]]$operation$name
          downstreamName    <-lookupDownstreamName(currentFeatureCodes)
          inflow            <-apply(object$operation$junctions[[currentFeatureCodes[5]]]$operation$inflow,1,sum)
          outflow           <-as.matrix(inflow)
          colnames(outflow) <-paste('outflow (to: ',downstreamName,')',sep='')
          object$operation$junctions[[currentFeatureCodes[5]]]$operation$outflow<-cbind(object$operation$junctions[[currentFeatureCodes[5]]]$operation$outflow,outflow)
          colnames(outflow) <-paste('inflow from(',currentName,')',sep='')
          object<-findANDset(object,outflow,currentFeatureCodes,matCodeAllObjects)
        } 
      }

      matCode<-as.matrix(matCode[,-IDUP,drop=FALSE])
      if(ncol(matCode)==0){break}
      if(ncol(matCode)==1)
      {
        idUpstream<-1
        IDUP<-1
      }else{
        IDUP<-idUpstream<-which(is.na(match(matCode[1,],matCode[2,])))
        idUpstream<-idUpstream[which(!duplicated(matCode[1,idUpstream]))]
        idUpstream<-idUpstream[sort(matCode[4,idUpstream],index.return = TRUE)$ix]
      }
      names(idUpstream)<-colnames(matCode)[idUpstream]
      idUpstream
    }
      
    if(nRes>0){for(i in 1:nRes){if(all(object$operation$reservoirs[[i]]$operation$inflow[,1]==0)){object$operation$reservoirs[[i]]$operation$inflow<-object$operation$reservoirs[[i]]$operation$inflow[,-1,drop=FALSE]};if(all(object$operation$reservoirs[[i]]$operation$outflow[,1]==0)){object$operation$reservoirs[[i]]$operation$outflow<-object$operation$reservoirs[[i]]$operation$outflow[,-1,drop=FALSE]}}}
    if(nRiv>0){for(i in 1:nRiv){if(all(object$operation$rivers    [[i]]$operation$inflow[,1]==0)){object$operation$rivers    [[i]]$operation$inflow<-object$operation$rivers    [[i]]$operation$inflow[,-1,drop=FALSE]};if(all(object$operation$rivers    [[i]]$operation$outflow[,1]==0)){object$operation$rivers    [[i]]$operation$outflow<-object$operation$rivers    [[i]]$operation$outflow[,-1,drop=FALSE]}}}
    if(nAuq>0){for(i in 1:nAuq){if(all(object$operation$aquifers  [[i]]$operation$inflow[,1]==0)){object$operation$aquifers  [[i]]$operation$inflow<-object$operation$aquifers  [[i]]$operation$inflow[,-1,drop=FALSE]};if(all(object$operation$aquifers  [[i]]$operation$outflow[,1]==0)){object$operation$aquifers  [[i]]$operation$outflow<-object$operation$aquifers  [[i]]$operation$outflow[,-1,drop=FALSE]}}}
    if(nDiv>0){for(i in 1:nDiv){if(all(object$operation$diversions[[i]]$operation$inflow[,1]==0)){object$operation$diversions[[i]]$operation$inflow<-object$operation$diversions[[i]]$operation$inflow[,-1,drop=FALSE]};if(all(object$operation$diversions[[i]]$operation$outflow[,1]==0)){object$operation$diversions[[i]]$operation$outflow<-object$operation$diversions[[i]]$operation$outflow[,-1,drop=FALSE]}}}
    if(nJun>0){for(i in 1:nJun){if(all(object$operation$junctions [[i]]$operation$inflow[,1]==0)){object$operation$junctions [[i]]$operation$inflow<-object$operation$junctions [[i]]$operation$inflow[,-1,drop=FALSE]};if(all(object$operation$junctions [[i]]$operation$outflow[,1]==0)){object$operation$junctions [[i]]$operation$outflow<-object$operation$junctions [[i]]$operation$outflow[,-1,drop=FALSE]}}}
    if(nDem>0){for(i in 1:nDem){if(all(object$operation$demands   [[i]]$operation$inflow[,1]==0)){object$operation$demands   [[i]]$operation$inflow<-object$operation$demands   [[i]]$operation$inflow[,-1,drop=FALSE]};if(ncol(object$operation$demands  [[i]]$operation$outflow     )>1){object$operation$demands   [[i]]$operation$outflow<-object$operation$demands   [[i]]$operation$outflow[,-1,drop=FALSE]}}}
      
    return(object)
}