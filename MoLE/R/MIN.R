MIN <-
function(vector, rank=1, value=F, rank.adjust=T, forceChoice=F){
	min=sort(unique(vector),decreasing=F)
	if(length(min)==1){
		if(forceChoice==T){index=sample(length(vector), 1)}
		if(forceChoice==F){index=1:length(vector)}
	}
	if(length(min) > 1){
		if(rank.adjust==T){if(rank[length(rank)] > length(min)){rank=min(rank):length(min)}}
		min=min[rank]
		index=1:length(vector)
		index=index[vector%in%min]
		if(length(index) > length(rank) & forceChoice==T){
			if(length(rank)==1){index=index[sample(length(index), 1)]}
			if(length(rank) > 1){index=index[order(vector[index], decreasing=F)[1:length(rank)]]}
	}	}	
	if(value==T){out=vector[index]}
	if(value==F){out=index}
out
}
