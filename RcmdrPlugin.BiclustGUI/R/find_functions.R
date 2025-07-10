# Project: BiclustGUI
# 
# Author: lucp8394
###############################################################################


# FUNCTION ONLY WORKS ON BICLUST OBJECTS!

FindBiclusters <- function(results,cols=c(),rows=c(),data){
	check.results <- function(x){
		if(!(class(x)=="list")){return(FALSE)}
		
		for(i.list in 1:length(x)){
			if(!.isBiclustresult(x[[i.list]])){return(FALSE)}
		}
		return(TRUE)
	}
	
	
	if(!check.results(results)){
		if(class(results)=="list"){
			justDoIt("warning('One of the results is not of Biclust class.',call.=FALSE)")
		}
		else{
			justDoIt("warning('The results object is not a list object.',call.=FALSE)")
		}
	}
	
	else{
		# Making list names if necessary
		if(is.null(names(results))){
			numbers <- as.character(1:length(results))
			temp <-  sapply(numbers,FUN=function(x){return(paste0("BCResult",x))})
			names(temp) <- NULL
			names(results) <- temp
		}
		
		# Transform names to numeric if necessary
		if(class(cols)=="character"){
			cols <- sapply(cols,FUN=function(x){which(colnames(data)==x)})
			names(cols) <- NULL
		}
		if(class(rows)=="character"){
			rows <- sapply(rows,FUN=function(x){which(rownames(data)==x)})
			names(rows) <- NULL
		}
		
		# Prepare output object
		out <- list()
		for(i.name in names(results)){
			eval(parse(text=paste0("out$",i.name," <- list()")))
		}
#		eval(parse(text=paste0()))
		
		# Search for biclusters in results
		for(i.result in 1:length(results)){
			current.result <- results[[i.result]]
			chosenBC <- c()
			out[[i.result]]$InsideBC <- "placeholder" # empty, but just to make sure it is in the first place
			
			for(i.bc in 1:current.result@Number){
				# Checking Rows
				rowsBC <- which(current.result@RowxNumber[,i.bc]==TRUE)
				rowscheck <- all(rows %in% rowsBC)
				
				# Checking Columns
				colsBC <- which(current.result@NumberxCol[i.bc,]==TRUE)
				colscheck <- all(cols %in% colsBC)
				
				# Together + Save Information about BC
				if(rowscheck & colscheck){
					chosenBC <- c(chosenBC,i.bc)
					temp.info <- list()
					
					temp.info$rows <- rowsBC
					temp.info$rows.names <- rownames(data)[rowsBC]
					
					temp.info$cols <- colsBC
					temp.info$cols.names <- colnames(data)[colsBC]
							
					# Put all info in..
					eval(parse(text=paste0("out[[i.result]]$bicluster",i.bc," <- temp.info")))
					
					
				}
			}
			
		# Put info of which biclusters were okay inside the list object
		out[[i.result]]$InsideBC <- chosenBC
			
		}
		
		# Add Search Call to list object 
		out$Searched <- list(rows=rows,rows.names=rownames(data)[rows],cols=cols,cols.names=colnames(data)[cols])
		
		
		# Print Summary 
		
		cat("Total Number of found BC's: ")
		cat(sum(sapply(out,FUN=function(x){return(length(x$InsideBC))})))
		cat("\n\n")
		for(i.cat in 1:(length(out)-1)){
			cat(names(out)[i.cat],"\n")
			n.bc <- length(out[[i.cat]]$InsideBC)
			cat(n.bc,"BC's")
			if(n.bc > 0){
				cat(":\t")
				cat(out[[i.cat]]$InsideBC)
			}
			cat("\n")
		}
		
		# Return object
		return(out)
	}
		
	
}
