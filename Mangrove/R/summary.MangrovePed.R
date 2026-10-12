summary.MangrovePed <-
function(object,...){
	ped <- object
	cat("A Mangrove pedigree.\n")
	cat("Number of individuals: ")
	cat(dim(ped)[[1]])
	cat("\nNumber of genotyped individuals: ")
	nonmiss <- apply(ped,1,function(x) any(x[-c(1:6)] != 0,na.rm=T))
	cat(sum(nonmiss,na.rm=T))
	cat("\nNumber of affecteds: ")
	cat(sum(ped[,6] == 2))
	cat("\n")
	cat("Number of markers: ")
	cat((dim(ped)[[2]] - 6)/2)
	cat("\n")

	temp <- sapply(1:((dim(ped)[[2]] - 6)/2),function(i) paste(ped[,7+2*(i-1)],ped[,8+2*(i-1)],sep=""))
	cat("Allele counts:\n")
	k <- dimnames(ped)[[2]][7 + 2*0:((length(ped[1,]) - 6)/2 - 1)]
	dimnames(temp)[[2]] <- substr(k,1,nchar(k)-2)
	print(summary(temp[nonmiss,]))
	}

