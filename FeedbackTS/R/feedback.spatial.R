 # # # # # # # # # # # # # # # # # #
# # #     spatial feedback          # # #
 # # #         functions            # # #
  # # # # # # # # # # # # # # # # # 


map.statistic=function(coordinates,statistic,region,cex.circles=c(3,0.2),legend,
	main=NULL,add=FALSE){
  	factmult=1/max(abs(statistic))*cex.circles[1]
  	factaddi=cex.circles[2]
  	z=statistic*factmult
	map('worldHires',region$border,xlim=region$xlim,ylim=region$ylim,col=1,add=add)
	if(length(main)>0){
		mtext(main)
	}
  	points(coordinates,cex=factaddi+abs(z),col=1+(z<0)+3*(z>0),lwd=1)
  	if(length(legend$x)>0){
  		points(legend$x,legend$y,col=c(1,1,1,4,2),
        cex=factaddi+round(c(0,max(abs(z))/2,max(abs(z)),rep(max(abs(z))/2,2))))
  		text(legend$xtext,legend$ytext,pos=4,
        labels=c(as.character(round(c(0,max(abs(z))/2,max(abs(z)))/factmult,
        legend$digits)),"Positive value","Negative value"))
    }
}

###################       FEEDBACK KRIGING       ####################


.map.shape=function(nodes,region,plots){
    MAP=map('worldHires', region$border,xlim=region$xlim,ylim=region$ylim,col="grey",
        fill=TRUE,plot=plots)
    k=length(MAP)+1
    j1=1
    j=1
    for(i in 1:length(MAP$x)){
    	if(is.na(MAP$x[j])){
            if(j-1-j1>5){
                MAP[[k]]=cbind(MAP$x[j1:(j-1)],MAP$y[j1:(j-1)])
                k=k+1
            }
            j1=j+1
    	}
    	j=j+1
    }
    in.region=0
    for(i in 5:length(MAP)){
    	in.region=in.region+point.in.polygon(nodes[,1],nodes[,2],MAP[[i]][,1],MAP[[i]][,2])
    }
    if(plots){
        points(nodes[in.region>0,],pch=".",col=3)
    }
    list(MAP=MAP,in.region=(in.region>0))
}


krige=function(coordinates,statistic,grid,krige.param=NULL,plots=TRUE,
    variog.param=list(npoints=50,nsim=99,plot.numbers=0.04)){
    statistic=as.vector(statistic)
    input=list(coordinates=coordinates,statistic=statistic,variog.param=variog.param,grid=grid,krige.param=krige.param)
    ## eventual projection to be made
    if(length(grid$proj)>0){
        coord.proj=project(coordinates,proj=grid$proj,degrees=grid$degrees)
        coord.proj=cbind(coord.proj$x,coord.proj$y)
    } else {
        coord.proj=coordinates
    }
    rownames(coord.proj)=NULL
    ## variography and kriging
    colnames(coord.proj)=c("x","y")
    DATA=SpatialPointsDataFrame(coord.proj,as.data.frame(cbind(statistic,coord.proj)))
    gr=expand.grid(grid$x,grid$y)
    gr1=.map.shape(gr,list(border=grid$border,xlim=range(grid$x),ylim=range(grid$y)),
        plots=FALSE)
    MAP=gr1$MAP
    in.region=gr1$in.region
    if(length(grid$proj)>0){
        pred.grid=project(gr,proj=grid$proj,degrees=grid$degrees)
        pred.grid=cbind(pred.grid$x,pred.grid$y)
    } else {
        pred.grid=gr
    }
    pred.grid=pred.grid[in.region,]
    colnames(pred.grid)=c("x","y")
    rownames(pred.grid)=NULL
    pred.grid1=SpatialPointsDataFrame(pred.grid,as.data.frame(cbind(pred.grid)))
    formula0=as.formula("statistic~1")
    if(length(krige.param)>0){
        formula0=as.formula(paste("statistic~",paste(krige.param,collapse="+")))
    }
    kk=autoKrige(formula0,input_data=DATA,new_data=pred.grid1)
    if(plots){
        ## variogram plotting
        if(is.null(variog.param$npoints)){ variog.param$npoints=50 }
        if(is.null(variog.param$nsim)){ variog.param$nsim=99 }
        if(is.null(variog.param$plot.numbers)){ variog.param$plot.numbers=0.04 }
        npoints=variog.param$npoints ## default is 50
        vgmfit=variogramLine(kk$var_model,maxdist=max(kk$exp_var$dist),n=npoints)
        nsim=variog.param$nsim  ## default is 99
        vgmfitStar=matrix(0,nsim,npoints)
        for(i in 1:nsim){
            permut=sample(1:length(statistic),size=length(statistic),replace=FALSE)
            coord.permut=coord.proj[permut,]
            rownames(coord.permut)=NULL
            DATAStar=SpatialPointsDataFrame(coord.permut,as.data.frame(cbind(statistic,coord.permut)))
            variogramStar = autofitVariogram(formula0,input_data=DATAStar)
            vgmfitStar[i,]=variogramLine(variogramStar$var_model,maxdist=max(variogramStar$exp_var$dist),n=npoints)[,2]
        }
        vgmfitStar=apply(vgmfitStar,2,range)
        plot.numbers=variog.param$plot.numbers ## default is 0.04
        plot(vgmfit,type="l",ylab="semi-variance",xlab="distance",xlim=c(0,1.1*max(variogramStar$exp_var$dist)),
             ylim=range(c(vgmfit[,2],kk$exp_var[,3],vgmfitStar)))
        points(kk$exp_var[,2:3])
        if(plot.numbers>0){
            text(kk$exp_var[,2]+plot.numbers*max(kk$exp_var[,2]),kk$exp_var[,3],kk$exp_var$np)
        }
        lines(vgmfit[,1],vgmfitStar[1,],lty="dashed")
        lines(vgmfit[,1],vgmfitStar[2,],lty="dashed")
        ## boxplots of kriging prediction and kriging standard deviation
        boxplot(cbind(kk$krige_output$var1.pred,kk$krige_output$var1.stdev),axes=FALSE,ylab="statistic")
        box()
        axis(1,1:2,labels=c("prediction","sd"))
        axis(2)
        ## kriging prediction plotting 
        prediction=rep(NA,nrow(gr))
        prediction[in.region]=kk$krige_output$var1.pred
        prediction=matrix(prediction,length(grid$x),length(grid$y))
        image(grid$x,grid$y,prediction,col=gray(seq(1,0.1,l=100)),xlab="latitude",ylab="longitude",
              main="kriging prediction")
        contour(grid$x,grid$y,prediction,add=TRUE)
        map('worldHires', grid$border,xlim=range(grid$x),ylim=range(grid$y),col=1,add=TRUE)
        ## kriging standard deviation plotting
        predictionSD=rep(NA,nrow(gr))
        predictionSD[in.region]=kk$krige_output$var1.stdev
        predictionSD=matrix(predictionSD,length(grid$x),length(grid$y))	
        image(grid$x,grid$y,predictionSD,col=gray(seq(1,0.1,l=100)),xlab="latitude",ylab="longitude",
              main="kriging standard error")
        contour(grid$x,grid$y,predictionSD,add=TRUE)
        map('worldHires', grid$border,xlim=range(grid$x),ylim=range(grid$y),col=1,add=TRUE)
    }    
    return(list(input=input,in.region=in.region,MAP=MAP,grid=gr,krige=kk))
}




krige.test=function(krige.output, subregion, alternative, nb.rand, subregion.coverage=0.8){
	## permute and krige
	coord=krige.output$input$coordinates
	in.region=krige.output$in.region
	grid=krige.output$grid
	in.subregion=(point.in.polygon(grid[,1],grid[,2],subregion$x,subregion$y)>0)
        predict=rep(NA,length(in.region))
        predict[in.region]=krige.output$krige$krige_output$var1.pred
	predict.permute=NULL
	j=0
	k=0
	while(j < nb.rand){
		k=k+1
		translation.index=sample(1:(length(predict)-1),1)
		permutation=c((translation.index+1):length(predict),
			1:translation.index)
		predict.permute.try=predict[permutation]
		if(mean(!is.na(predict.permute.try[in.region & in.subregion]))>subregion.coverage){
			predict.permute=cbind(predict.permute,predict.permute.try)
			j=j+1
			if(j/100==round(j/100)){ 
				print(paste("Number of permutations:",j,"/",nb.rand))
			}
		}
	}
	print(paste("Total number of permutations:",k,"(",k-nb.rand,"permutations led to under-coverage of the subregion under study and were discarded )"))
	## plot
      #  browser()
	count.local.nodes=sum(in.region & in.subregion)
	localpred=mean(predict[in.region & in.subregion])
	localPRED=as.numeric(colMeans(predict.permute[in.region & in.subregion,],
		na.rm=TRUE))
	if(alternative=="greater"){
		pval=mean(localPRED>localpred)
	} else {
		if(alternative=="less"){
			pval=mean(localPRED<localpred)
		} else {
			stop(paste("Alternative hypothesis",alternative,"is not available."))
		}
	}
    return(new("KT.output",krige.output=krige.output, subregion=subregion,
        averageKrigingPrediction.rand=localPRED,
        averageKrigingPrediction.obs=localpred,
        alternative=alternative, p.value=pval))
}


