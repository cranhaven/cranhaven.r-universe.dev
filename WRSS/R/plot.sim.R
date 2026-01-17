plot.sim <-
function(x,...)
{
   nRes<-length(x$operation$operation$reservoirs)
   nRiv<-length(x$operation$operation$rivers)
   nAuq<-length(x$operation$operation$aquifers)
   nJun<-length(x$operation$operation$junctions)
   nDiv<-length(x$operation$operation$diversions)
   nDem<-length(x$operation$operation$demands)
   simulation<- x$operation$operation$simulation
   dates<-seq(as.Date(simulation$start),as.Date(simulation$end),simulation$interval)

   getCycleMean<-function(data)
   {
      if(simulation$interval=='month')
      {
         mat<-t(matrix(NA,(floor(length(data)/12)+1),12))
         m<-months(seq(as.Date('2000-01-01'),to=as.Date('2000-12-01'),'month'))
         start<-which(months(as.Date(simulation$start))==m)
         if(start==0) start<-1
         mat[start:(start+length(dates)-1)]<-data
         mat<-t(mat)
         out<-apply(mat,2,mean,na.rm=TRUE)
         names(out)<-month.abb
      }
      if(simulation$interval=='week')
      {
         start<-floor(as.numeric(as.Date(simulation$start)-as.Date(paste((strsplit(simulation$start,'-')[[1]])[1],'-','01','-','01',sep='')))/7)+1
         if(start==0) start<-1
         mat<-t(matrix(NA,(floor(length(data)/53)+1),53))
         mat[start:(start+length(dates)-1)]<-data
         mat<-t(mat)
         out<-apply(mat,2,mean,na.rm=TRUE)
         names(out)<-paste('week','-',1:length(out),sep='')
      }
      if(simulation$interval=='day')
      {
         start<-as.numeric(as.Date(simulation$start)-as.Date(paste((strsplit(simulation$start,'-')[[1]])[1],'-','01','-','01',sep='')))
         if(start==0) start<-1
         mat<-t(matrix(NA,(floor(length(data)/366)+1),366))
         mat[start:(start+length(dates)-1)]<-data
         mat<-t(mat)
         out<-apply(mat,2,mean,na.rm=TRUE)
         names(out)<-paste('day','-',1:length(out),sep='')
      }
      NANs<-which(is.nan(out))
      if(length(NANs)>0) out<-out[-NANs]
      return(out)
   }
   labelRemover<-function(data)
   {
      if(simulation$interval=='day') CF<-363
      if(simulation$interval=='week') CF<-51
      if(simulation$interval=='month') CF<-10
      intervals<-floor(length(Storage)*18/CF+2)+2
      names<-colnames(data)
      labels<-rep(NA,length(names))
      selctedLabels<-round(seq(1,ncol(data),length.out=intervals))
      labels[selctedLabels]<-names[selctedLabels]
      colnames(data)<-labels
      return(data)
   }

   if(nRes>0)
   {
      for(i in 1:nRes)
      {
         oask <- devAskNewPage(TRUE)
         on.exit(devAskNewPage(oask))
         inflow  <-x$operation$operation$reservoirs[[i]]$operation$inflow
         outflow <-x$operation$operation$reservoirs[[i]]$operation$outflow
         capacity<-x$operation$operation$reservoirs[[i]]$operation$geometry$capacity
         Storage    <-getCycleMean(x$operation$operation$reservoirs[[i]]$operation$sim_result$storage)
         Spill      <-getCycleMean(x$operation$operation$reservoirs[[i]]$operation$sim_result$spill)
         Evaporation<-getCycleMean(x$operation$operation$reservoirs[[i]]$operation$sim_result$loss)
         Release    <-getCycleMean(apply(x$operation$operation$reservoirs[[i]]$operation$outflow,1,sum))
         Inflow     <-getCycleMean(apply(x$operation$operation$reservoirs[[i]]$operation$inflow,1,sum))
         title<-x$operation$operation$reservoirs[[i]]$operation$name
         bars<-t(cbind(Evaporation,Release,Storage,Inflow,Spill))
         bars<-labelRemover(bars)
         Storage<-Storage-Inflow
         ylim<-c(0,max(apply(bars,2,sum),na.rm=TRUE))*(1+nrow(bars)*0.05)
         barplot(bars,las=2,col=1:5,ylab='Volume (MCM)',ylim=ylim,main=title)
         lines(0:length(dates),rep(capacity,length(dates)+1),col=6,typ='o',lwd=2,pch=19)
         legend('top',
                legend=c('evaporation','release','storage','inflow','spill','capacity'),
                ncol=3,
                fill=c(1:5,NA),
                box.lwd=0,
                box.col=NA,
                lty=c(rep(0,5),1),
                col=1:6,
                lwd=c(rep(0,5),2),
                border=c(rep(1,5),NA))
      }
   }

   if(nRiv>0)
   {
      for(i in 1:nRiv)
      {
         oask <- devAskNewPage(TRUE)
         on.exit(devAskNewPage(oask))
         inflow <-x$operation$operation$rivers[[i]]$operation$inflow
         outflow<-x$operation$operation$rivers[[i]]$operation$outflow
         I<-rep(NA,length(getCycleMean(inflow [,1])))
         O<-rep(NA,length(getCycleMean(inflow [,1])))
         for(j in 1:ncol(inflow)) {I<-cbind(I,getCycleMean(inflow [,j]))};I<-I[,-1,drop=FALSE]
         for(j in 1:ncol(outflow)){O<-cbind(O,getCycleMean(outflow[,j]))};O<-O[,-1,drop=FALSE]
         I<-t(I);O<-t(O)
         ylim<-c(0,max(apply(I,2,sum),apply(O,2,sum)))*(1+(ncol(inflow)+ncol(outflow))*0.1)
         title<-x$operation$operation$rivers[[i]]$operation$name
         I<-labelRemover(I)
         middleOfBars<-barplot(I,ylim=ylim,ylab='Volume (MCM)',las=2,main=title,col=gray((1:ncol(inflow))/(1.2*ncol(inflow))))
         for(j in 1:nrow(O)){lines(middleOfBars,O[j,],typ='o',pch=21,bg='white',col=j+1)}
         legend('top',
                legend=c(colnames(inflow),colnames(outflow)),
                ncol=2,
                fill=c(gray((1:ncol(inflow))/(1.2*ncol(inflow))),rep(NA,ncol(outflow))),
                lty=c(rep(0,ncol(inflow)),rep(1,ncol(outflow))),
                col=c(rep(1,ncol(inflow)),2:(ncol(outflow)+1)),
                border=c(rep(1,ncol(inflow)),rep(NA,ncol(outflow))),
                box.lwd=0,box.col=NA)
      }
   }

   if(nAuq>0)
   {
      for(i in 1:nAuq)
      {
         oask <- devAskNewPage(TRUE)
         on.exit(devAskNewPage(oask))
         inflow <-x$operation$operation$aquifers[[i]]$operation$inflow
         outflow<-x$operation$operation$aquifers[[i]]$operation$outflow
         storage<-x$operation$operation$aquifers[[i]]$operation$storage
         I<-rep(NA,length(getCycleMean(inflow [,1])))
         O<-rep(NA,length(getCycleMean(inflow [,1])))
         for(j in 1:ncol(inflow)) {I<-cbind(I,getCycleMean(inflow [,j]))};I<-I[,-1,drop=FALSE]
         for(j in 1:ncol(outflow)){O<-cbind(O,getCycleMean(outflow[,j]))};O<-O[,-1,drop=FALSE]
         I<-t(I);O<-t(O)
         ylim<-c(0,max(apply(I,2,sum),apply(O,2,sum)))*(1+(ncol(inflow)+ncol(outflow))*0.1)
         title<-x$operation$operation$aquifers[[i]]$operation$name
         I<-labelRemover(I)
         middleOfBars<-barplot(I,ylim=ylim,ylab='Volume (MCM)',las=2,main=title,col=gray((1:ncol(inflow))/(1.2*ncol(inflow))))
         for(j in 1:nrow(O)){lines(middleOfBars,O[j,],typ='o',pch=21,bg='white',col=j+1)}
         legend('top',
                legend=c(colnames(inflow),colnames(outflow)),
                ncol=2,
                fill=c(gray((1:ncol(inflow))/(1.2*ncol(inflow))),rep(NA,ncol(outflow))),
                lty=c(rep(0,ncol(inflow)),rep(1,ncol(outflow))),
                col=c(1:ncol(inflow),2:(ncol(outflow)+1)),
                border=c(rep(1,ncol(inflow)),rep(NA,ncol(outflow))),
                box.lwd=0,box.col=NA)
         inflow <-apply(inflow,1,sum)
         outflow<-apply(outflow,1,sum)
         ylim<-c(0,max(inflow+outflow))*1.2
         par(mar = c(5, 4, 4, 4) + 0.3)
         IO<-rbind(inflow,outflow)
         IO<-labelRemover(IO)
         middleOfBars<-barplot(IO,las=2,col=gray(c(1,3)/4),ylab='Volume of inflow & outflow (MCM)',ylim=ylim, main=title)
         par(new = TRUE)
         plot(middleOfBars,storage$storage,axes = FALSE, bty = "n", xlab = "", ylab = "",typ='o',col=2,pch=21,bg='white')
         axis(side=4, at = pretty(range(storage)))
         mtext("Aquifer storage (MCM)", side=4, line=3)
         legend('top',
                legend=c('inflow','outflow','storage'),
                ncol=3,
                fill=c(gray(c(1,3)/4),NA),
                lty=c(0,0,1),
                col=c(1,1,2),
                border=c(1,1,NA),
                box.lwd=0,box.col=NA)
      }
   }

   if(nDiv>0)
   {
      for(i in 1:nDiv)
      {
         oask <- devAskNewPage(TRUE)
         on.exit(devAskNewPage(oask))
         inflow  <-x$operation$operation$diversions[[i]]$operation$inflow
         outflow <-x$operation$operation$diversions[[i]]$operation$outflow
         diverted<-x$operation$operation$diversions[[i]]$operation$sim_result$diverted$diverted
         overflow<-x$operation$operation$diversions[[i]]$operation$sim_result$overflow$overflow
         I<-rep(NA,length(getCycleMean(inflow [,1])))
         O<-getCycleMean(overflow)
         D<-getCycleMean(diverted)
         for(j in 1:ncol(inflow)) {I<-cbind(I,getCycleMean(inflow [,j]))};I<-I[,-1,drop=FALSE]
         I<-apply(I,1,sum)
         ylim<-c(0,max(apply(rbind(I,O,D),2,sum)))*1.1
         title<-x$operation$operation$diversions[[i]]$operation$name
         IOD<-rbind(I,O,D)
         IOD<-labelRemover(IOD)         
         barplot(IOD,las=2, ylab=c('Volume (MCM)'),ylim=ylim,col=gray(1:3/4),main=title,)
         legend('top',
                legend=c('inflow','outflow','diverted'),
                ncol=3,
                fill=c(gray(c(1:3)/4),NA),
                box.lwd=0,box.col=NA)
         plot(ecdf(diverted)(seq(min(diverted)-0.01,max(diverted)+0.01,0.01)),
                             seq(min(diverted)-0.01,max(diverted)+0.01,0.01),
              typ='l',xlab='Probability',ylab='Diverted Volume (MCM)', main=title)
      }
   }

   if(nJun>0)
   {
      for(i in 1:nJun)
      {
         oask <- devAskNewPage(TRUE)
         on.exit(devAskNewPage(oask))
         inflow  <-x$operation$operation$junctions[[i]]$operation$inflow
         outflow <-x$operation$operation$junctions[[i]]$operation$outflow[,1]
         I<-rep(NA,length(getCycleMean(inflow [,1])))
         for(j in 1:ncol(inflow)) {I<-cbind(I,getCycleMean(inflow [,j]))};I<-I[,-1,drop=FALSE]
         O<-getCycleMean(outflow)
         ylim<-c(0,max(I,O))*(1+0.05*(ncol(I)+1))
         title<-x$operation$operation$junctions[[i]]$operation$name
         I<-t(labelRemover(t(I)))
         plot(I[,1],xaxt='n',ylab='Volume (MCM)',ylim=ylim,typ='o',xlab='',pch=0,main=title)
         axis(1, at=1:nrow(I),labels=rownames(I),las=2)
         if(ncol(inflow)>1)
         {
            for(j in 2:ncol(I))
            {
               lines(I[,j],col=j,pch=j-1,bg='white',typ='o')
            }
         }
         lines(O,typ='o',col=j+1,pch=j,bg='white')
         name<-c(colnames(inflow),colnames(outflow))
         legend('top',
                legend=name,
                ncol=2,
                col=1:(j+1),
                pch=0:j,
                box.lwd=0,box.col=NA)
      }
   }

   if(nDem>0)
   {
      for(i in 1:nDem)
      {
         oask <- devAskNewPage(TRUE)
         on.exit(devAskNewPage(oask))
         outflow <-x$operation$operation$demands[[i]]$operation$outflow
         demandTS<-getCycleMean(x$operation$operation$demands[[i]]$operation$demandTS$demand)
         if(ncol(outflow)>1)
         {
            inflow  <-x$operation$operation$demands[[i]]$operation$inflow
            out<-as.matrix(apply(outflow,1,sum))
            colnames(out)<-colnames(outflow)[1]
            O<-getCycleMean(out)
            I<-rep(NA,length(getCycleMean(inflow [,1])))
            for(j in 1:ncol(inflow)) {I<-cbind(I,getCycleMean(inflow [,j]))};I<-t(I[,-1,drop=FALSE])
            ylim<-c(0,max(apply(rbind(I,O),2,sum),demandTS))*1.1
            title<-x$operation$operation$demands[[i]]$operation$name
            col<-gray(1:(ncol(inflow)+1)/(ncol(inflow)+1))
            I<-labelRemover(I)
            middleOfBars<-barplot(I,las=2, ylab=c('Volume (MCM)'),ylim=ylim,main=title,col=col[1:ncol(inflow)])
            lines(middleOfBars,demandTS,typ='o',pch=21,bg='white',col=2)
            barplot(O,las=2, ylab=c('Volume (MCM)'),ylim=ylim,main=title,col=col[length(col)],add=TRUE)
            legend('top',
                   legend=c(colnames(inflow),colnames(outflow)[2],'demand'),
                   ncol=2,
                   fill=c(col,NA),
                   lty=c(rep(0,ncol(inflow)+1),1),
                   col=c(rep(0,ncol(inflow)+1),2),
                   border=c(rep(1,ncol(inflow)+1),NA),
                   box.lwd=0,box.col=NA)
         }else{
            inflow  <-x$operation$operation$demands[[i]]$operation$inflow
            I<-rep(NA,length(getCycleMean(inflow [,1])))
            for(j in 1:ncol(inflow)) {I<-cbind(I,getCycleMean(inflow [,j]))};I<-t(I[,-1,drop=FALSE])
            ylim<-c(0,max(apply(I,2,sum),demandTS))*1.1
            title<-x$operation$operation$demands[[i]]$operation$name
            col<-gray(1:ncol(inflow)/ncol(inflow))
            I<-labelRemover(I)
            middleOfBars<-barplot(I,las=2, ylab=c('Volume (MCM)'),ylim=ylim,main=title,col=col)
            lines(middleOfBars,demandTS,typ='o',pch=21,bg='white',col=2)
            legend('top',
                   legend=c(colnames(inflow),'demand'),
                   ncol=2,
                   fill=c(col,NA),
                   lty=c(rep(0,ncol(inflow)),1),
                   col=c(rep(0,ncol(inflow)),2),
                   border=c(rep(1,ncol(inflow)),NA),
                   box.lwd=0,box.col=NA)
         }
      }
   }
}