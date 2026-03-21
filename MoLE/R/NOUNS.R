NOUNS <-
function(n=world$nNouns, local=world$local){
	cut=FALSE; if(n==1){n=2; cut=TRUE}	#minimally 2 entries for dataframe
	distinctions=world$distinctions; reductionRecencyThreshold=world$reductionRecencyThreshold
	cols=length(distinctions)
	nouns=as.data.frame(replicate(cols,rep(0,n)), stringsAsFactors=FALSE)
	names(nouns)=gsub('^V(\\d)','D\\1',names(nouns))
	for (i in 1:cols)
		nouns[,i]=sample(seq(0,1,length.out=distinctions[i]),n,replace=TRUE)
	nouns$person=3
	nouns$ID=1:n
	nouns$form=FORMS(n)
	nouns$frequency=0
	nouns$argument=0
	nouns$nounMarker=0
	nouns$verbMarker=0
	nouns$recency=reductionRecencyThreshold+1
	nouns$activation=0
	nouns$productionEffort=nchar(nouns$form)
	nouns$semanticWeight=1
	if(local==TRUE){
		maxs=MAX(VMATCH(rep(1, length(distinctions)), nouns[, grep('D\\d', names(nouns))]), 1:2, forceChoice=TRUE)
		nouns[maxs,]$person=1:2
	}
	if(cut==TRUE){nouns=nouns[1,]}
nouns
}
