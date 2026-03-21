MAX <-
function(vector, rank=1, value=FALSE, rank.adjust=TRUE, forceChoice=FALSE){
	max=sort(unique(vector),decreasing=TRUE)
	if(length(max)==1){
		if(forceChoice==TRUE){index=sample(length(vector), 1)}
		if(forceChoice==FALSE){index=1:length(vector)}
	}
	if(length(max) > 1){
		if(rank.adjust==TRUE){if(rank[length(rank)] > length(max)){rank=min(rank):length(max)}} #don't ask for more than there is
		max=max[rank] 
		index=1:length(vector)
		index=index[vector%in%max]
		if(length(index) > length(rank) & forceChoice==TRUE){
			if(length(rank)==1){index=index[sample(length(index), 1)]}
			if(length(rank) > 1){index=index[order(vector[index], decreasing=TRUE)[1:length(rank)]]}
	}	}	
	if(value==TRUE){out=vector[index]}
	if(value==FALSE){out=index}
out
}
