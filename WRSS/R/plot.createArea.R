plot.createArea <-
function(x,...)
{
   if(missing(x))
   {
      stop("missing x!")
   }
   
   if(!inherits(x,"createArea"))
   {
      stop("bad class type!")
   }

   nRes<-length(x$operation$reservoirs)
   nRiv<-length(x$operation$rivers)
   nJun<-length(x$operation$junctions)
   nAuq<-length(x$operation$aquifers)
   nDiv<-length(x$operation$diversions)
   nDem<-length(x$operation$demands)

   matCode<-matrix(NA,3,nRes+nRiv+nAuq+nJun+nDiv+nDem)
   lab<-c()
   i<-0;j<-0;k<-0;l<-0;m<-0;n<-0
   if(nRes>0){for(i in 1:nRes){matCode[1,i]          <-x$operation$reservoirs[[i]]$operation$label;matCode[2,i]          <-x$operation$reservoirs[[i]]$operation$downstream   ;matCode [3,i]          <-1;lab<-c(lab,x$operation$reservoirs[[i]]$operation$name)}}
   if(nRiv>0){for(j in 1:nRiv){matCode[1,i+j]        <-x$operation$rivers    [[j]]$operation$label;matCode[2,i+j]        <-x$operation$rivers    [[j]]$operation$downstream   ;matCode [3,i+j]        <-2;lab<-c(lab,x$operation$rivers    [[j]]$operation$name)}}
   if(nAuq>0){for(k in 1:nAuq){matCode[1,i+j+k]      <-x$operation$aquifers  [[k]]$operation$label;matCode[2,i+j+k]      <-x$operation$aquifers  [[k]]$operation$leakageObject;matCode [3,i+j+k]      <-3;lab<-c(lab,x$operation$aquifers  [[k]]$operation$name)}}
   if(nDiv>0){for(l in 1:nDiv){matCode[1,i+j+k+l]    <-x$operation$diversions[[l]]$operation$label;matCode[2,i+j+k+l]    <-x$operation$diversions[[l]]$operation$downstream   ;matCode [3,i+j+k+l]    <-4;lab<-c(lab,x$operation$diversions[[l]]$operation$name)}}
   if(nJun>0){for(m in 1:nJun){matCode[1,i+j+k+l+m]  <-x$operation$junctions [[m]]$operation$label;matCode[2,i+j+k+l+m]  <-x$operation$junctions [[m]]$operation$downstream   ;matCode [3,i+j+k+l+m]  <-5;lab<-c(lab,x$operation$junctions [[m]]$operation$name)}}
   if(nDem>0){for(n in 1:nDem){matCode[1,i+j+k+l+m+n]<-x$operation$demands   [[n]]$operation$label;matCode[2,i+j+k+l+m+n]<-x$operation$demands   [[n]]$operation$downstream   ;matCode [3,i+j+k+l+m+n]<-0;lab<-c(lab,x$operation$demands   [[n]]$operation$name)}}

   colnames(matCode)<-lab
   rownames(matCode)<-c('code','downstream','featureType')
   resources<-which(!(matCode[3,]==0 | matCode[3,]==5))
   matCode<-matCode[-3,]
   for(i in 1:length(resources))
   {
      currentResources<-matCode[,resources[i],drop=FALSE]
      if(nDem>0)
      {
         for (j in 1:nDem)
         {
            if(any(x$operation$demands[[j]]$operation$suppliers==currentResources[1]))
            {
               name<-colnames(matCode)[resources[i]]
               code<-matCode[1,resources[i]]
               downStream<-x$operation$demands[[j]]$operation$label
               codes<-as.matrix(c(code,downStream))
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
         if(!is.na(x$operation$rivers[[i]]$operation$seepageObject))
         {
            name<-x$operation$rivers[[i]]$operation$name
            code<-x$operation$rivers[[i]]$operation$label
            downStream<-x$operation$rivers[[i]]$operation$seepageObject
            codes<-as.matrix(c(code,downStream))
            colnames(codes)<-name
            matCode<-cbind(matCode,codes)
         } 
      }
   }

   if(nRes>0)
   {
      for(i in 1:nRes)
      {
         if(!is.na(x$operation$reservoirs[[i]]$operation$seepageObject))
         {
            name<-x$operation$reservoirs[[i]]$operation$name
            code<-x$operation$reservoirs[[i]]$operation$label
            downStream<-x$operation$reservoirs[[i]]$operation$seepageObject
            codes<-as.matrix(c(code,downStream))
            colnames(codes)<-name
            matCode<-cbind(matCode,codes)
         }
      }
   }

   if(nDiv>0)
   {
      for(i in 1:nDiv)
      {
         if(!is.na(x$operation$diversions[[i]]$operation$divertObject))
         {
            name<-x$operation$diversions[[i]]$operation$name
            code<-x$operation$diversions[[i]]$operation$label
            downStream<-x$operation$diversions[[i]]$operation$divertObject
            codes<-as.matrix(c(code,downStream))
            colnames(codes)<-name
            matCode<-cbind(matCode,codes)
         }
      }
   }

   type<-c('Reservoir','River','Aquifer','Diversion','Junction','Demand')
   availableTypes<-c(ifelse(nRes>0,1,NA),ifelse(nRiv>0,1,NA),ifelse(nAuq>0,1,NA),ifelse(nDiv>0,1,NA),ifelse(nJun>0,1,NA),ifelse(nDem>0,1,NA))
   type<-type[which(!is.na(availableTypes))]
   types<-rep(type,c(nRes,nRiv,nAuq,nDiv,nJun,nDem)[which(!is.na(availableTypes))])
   color.palette<-c(5,1,2,3,4,6)[which(!is.na(availableTypes))]
   shape.palette <-c(17,1,15,3,10,19)[which(!is.na(availableTypes))]
   size.palette<-c(10,0.01,10,10,10,10)[which(!is.na(availableTypes))]
   names(size.palette)<-type
   names(shape.palette)<-type
   names(color.palette)<-type
   net<-matrix(0,ncol(matCode),ncol(matCode))
   for(n in 1:ncol(net))
   {
      con<-which(matCode[2,n]==matCode[1,])
      if(length(con)>0) {net[n,con]<-1}
   }
   colnames(net)<-colnames(matCode)
   rownames(net)<-colnames(matCode)

   if(sum(c(nRes,nRiv,nAuq,nJun,nDiv,nDem))<ncol(matCode))
   {
      for(i in 1:(sum(c(nRes,nRiv,nAuq,nJun,nDiv,nDem))+1))
      {
         net[i,]<-apply(net[which(matCode[1,i]==matCode[1,])[-1],,drop=FALSE],2,sum)+net[i,]
      }
      net<-net[,1:(sum(c(nRes,nRiv,nAuq,nJun,nDiv,nDem)))]
      net<-net[1:(sum(c(nRes,nRiv,nAuq,nJun,nDiv,nDem))),]
   }
   net<-network(net)
   set.vertex.attribute(net,"type",types)
   ggnet2(net,color='type',,size='type',shape='type',
          color.palette=color.palette,shape.palette=shape.palette,size.palette=size.palette,
          label=TRUE,arrow.size = 9, arrow.gap = 0.025)+guides(size = FALSE)
}