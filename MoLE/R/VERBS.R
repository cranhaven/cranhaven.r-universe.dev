VERBS <-
function(n=world$nVerbs){
	cut=FALSE; if(n==1){n=2; cut=TRUE} #minimally 2 entries for dataframe
	distinctions=world$distinctions; linkingPreference=world$linkingPreference; proportionIntrans=world$proportionIntrans; reductionRecencyThreshold=world$reductionRecencyThreshold
	cols=length(distinctions)
	verbs=as.data.frame(replicate(cols,rep(0,n)),stringsAsFactors=FALSE)
	names(verbs)=gsub('^V(\\d)','D\\1',names(verbs))
	for (i in 1:cols){verbs[,i]=sample(seq(0,1,length.out=distinctions[i]),n,replace=TRUE)}
	external=as.data.frame(replicate(cols,rep(0,n)),stringsAsFactors=FALSE)
	for (i in 1:cols){external[,i]=sample(seq(1,0,length.out=distinctions[i]),n,replace=TRUE, prob=seq(linkingPreference, 1, length.out=distinctions[i]))}	#external role of two-place predicates are more prominent
	internal=as.data.frame(replicate(cols,rep(0,n)),stringsAsFactors=FALSE)
	for (i in 1:cols){internal[,i]=sample(seq(1,0,length.out=distinctions[i]),n,replace=TRUE, prob=seq(1, linkingPreference, length.out=distinctions[i]))}
	names(external)=gsub('^V(\\d)','Ext\\1',names(external))
	names(internal)=gsub('^V(\\d)','Int\\1',names(internal))
	if(proportionIntrans > 0){
		intrans=sample(n,ceiling(proportionIntrans*n),replace=FALSE)
		internal[intrans,]=NA
		for (i in 1:cols){external[intrans,i]=sample(seq(0,1,length.out=distinctions[i]),length(intrans),replace=TRUE)}	#external roles of intransitives can be anything
	}	
	verbs=cbind(verbs, external, internal)	
	verbs$type='twoPlace'
	if(proportionIntrans > 0){verbs[intrans,]$type='onePlace'}
	verbs$ID=1:n
	verbs$form=FORMS(n)
	verbs$frequency=0
	verbs$recency=reductionRecencyThreshold+1
	verbs$activation=0
	verbs$productionEffort=nchar(verbs$form)
	verbs$semanticWeight=1
	if(cut==TRUE){verbs=verbs[1,]}
verbs
}
