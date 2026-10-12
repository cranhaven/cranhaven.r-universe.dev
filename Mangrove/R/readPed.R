readPed <-
function(prefix){
	map <- read.table(paste(prefix,'.map',sep=""),colClasses="character")
	rsids <- map[,2]
	colClasses <- c("character","character","character","character","numeric","numeric",rep("character",2*length(map[,2])))
	colNames <- c("Family","ID","Father","Mother","Sex","Phenotype",paste(rep(rsids,rep(2,length(rsids))),rep(c(".1",".2"),length(rsids)),sep=""))
	ped <- read.table(paste(prefix,'.ped',sep=""),colClasses=colClasses,col.names=colNames)
	dimnames(ped)[[1]] <- ped$ID
	# set any parents that don't have entries to missing
	ped[!ped[,3] %in% ped[,2] & ped[,3] != 0,3] <- 0
	ped[!ped[,4] %in% ped[,2] & ped[,4] != 0,4] <- 0
	class(ped) <- c("MangrovePed","data.frame")
	return(ped)
}

