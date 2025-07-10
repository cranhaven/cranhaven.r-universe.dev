# Project: BiclustGUI
# 
# Author: Gebruiker
###############################################################################


#which: all, range (from..to..), selection (vector)
#dim (row, col, both)


# DO SOME CHECKS HERE:
# 1: is there an active dataset?
# 2: is the dimension of the active dataset corresponding with the biclust result ?
ExtractBiclustersGUI <-function(result,which="all",from=1,to=1,selection=c(1),dim="both",save=FALSE,save.name="Extract"){
	

	# Which biclusters to select?
	if(which=="all"){
		selected_biclusters <- seq(1,result@Number,1)
	}
	if(which=="range"){
		selected_biclusters <- seq(from,to,1)
	}
	if(which=="selection"){
		selected_biclusters <- selection
	}
	
#	eval(parse(text=paste0("matrixdata <- as.matrix(",ActiveDataSet(),")")))
	matrixdata <- as.matrix(get(ActiveDataSet(),envir=.GlobalEnv))
	
	# Making the desired list object
	out <- list()
	names_temp <- c()
	
	for(i.bc in 1:length(selected_biclusters)){
		i.bc.sel <- selected_biclusters[i.bc]
		names_temp <- c(names_temp,paste0("bicluster_",i.bc.sel))
		
		out_temp <- list()
		
		if(dim=="row"|dim=="both"){
			out_temp$rows <- which(result@RowxNumber[,i.bc.sel]==TRUE)
			out_temp$rows.names <- rownames(matrixdata)[out_temp$rows]
		}
		if(dim=="col"|dim=="both"){
			out_temp$cols <- which(result@NumberxCol[i.bc.sel,]==TRUE)
			out_temp$cols.names <- colnames(matrixdata)[out_temp$cols]
		}
		out[[i.bc]] <- out_temp
	}
	names(out) <- names_temp
	
	
	if(save==TRUE){
		
		eval(parse(text=paste0(save.name," <- out")))
		path.temp <- getwd()
		path.temp <- paste0(path.temp,"/",save.name,".RData")
		eval(parse(text=paste0("save(",save.name,",file='",path.temp,"')")))
		temp.command <- paste0("warning('The result is saved in ",path.temp,"',call.=FALSE)")
		justDoIt(temp.command)

	}
	
return(out)
}


ExtractBiclustersGUI_general <- function(result,which="all",from=1,to=1,selection=c(1),dim="both",save=FALSE,save.name="Extract"){
	
	temp <- .tobiclust(result)
	return(ExtractBiclustersGUI(temp,which=which,from=from,to=to,selection=selection,dim=dim,save=FALSE,save.name="Extract"))
	
}



