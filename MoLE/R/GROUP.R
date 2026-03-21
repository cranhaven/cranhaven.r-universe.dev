GROUP <-
function(hearerID, analysis){
	distinctiveness=world$distinctiveness; topicCopy=world$topicCopy
	hearer=population[[hearerID]]
	verb=grep(paste('verb','$',sep=''), analysis$role)
	if(length(verb)==0){verb=0}
	nounSuffixes=grep('nounSuffix',analysis$role)
	verbSuffixes=grep('verbSuffix',analysis$role)
	verbAdposition=verb + 1     
	while(verbAdposition%in%verbSuffixes){verbAdposition=verbAdposition + 1}   
	yangIndex=TRUE %in% c(hearer$usageHistory$index$no < ((hearer$usageHistory$index$yes + hearer$usageHistory$index$no)/log(hearer$usageHistory$index$yes + hearer$usageHistory$index$no)))
	if(yangIndex==TRUE & !'verbsuffix'%in%analysis$role){	
		person=analysis[verbAdposition,]$verbMarkerPerson
		data=hearer$usageHistory$index[hearer$usageHistory$index$person==person,]
		data=data[data$no < (data$yes + data$no)/log(data$yes + data$no),]
		if(nrow(data)!=0){verbAdposition=0}
	}
	words=list()
	for (i in 1:nrow(analysis)){    
		if(i==1 & i!=verb){words[[i]]='?'}
		if(i==verb){words[[i]]='verb'}
		if(i%in%nounSuffixes){words[[i]]='nounSuffix'}
		if(i%in%verbSuffixes){words[[i]]='verbSuffix'}	
		if(i==verbAdposition){words[[i]]=c('verbAdposition','?')}    
		if(i > 1 & i!=verb & i!=verbAdposition & !i%in%nounSuffixes & !i%in%verbSuffixes & analysis$nounPerson[i]==3){words[[i]]=c('nounAdposition','?')}	
		if(i > 1 & i!=verb & i!=verbAdposition & !i%in%nounSuffixes & !i%in%verbSuffixes & analysis$nounPerson[i]!=3){words[[i]]=c('?')}
	}                       
	nAnalyses=1
	for(i in 1:length(words)){nAnalyses=nAnalyses*length(words[[i]])}
	options=as.data.frame(replicate(length(words),rep('',nAnalyses)),stringsAsFactors=FALSE)
	if(nAnalyses==1){options=as.data.frame(t(replicate(length(words),'')), stringsAsFactors=FALSE)}
	for (i in 1:ncol(options)){
		options[,i]=rep(sort(rep(words[[i]],nrow(unique(options)))), length.out=nAnalyses)
	}
	if(ncol(options)>1){	
		impossible=vector()
		n=1
		for (i in 1:nrow(options)){
			for (j in 2:ncol(options)){
				if(options[i,j]=='nounSuffix' & options[i,j-1]!='?'){impossible[n]=i; n=n + 1; break}
				if(options[i,j]=='verbSuffix' & options[i,j-1]!='verb' & options[i,j-1]!='verbSuffix'){impossible[n]=i; n=n + 1; break}
				if(options[i,j]=='nounAdposition' & options[i,j-1]!='?' & options[i,j-1]!='nounSuffix'){impossible[n]=i; n=n + 1; break}
				if(options[i,j]=='verbAdposition' & options[i,j-1]!='verb' & options[i,j-1]!='verbSuffix'){impossible[n]=i; n=n + 1}
		}	}
		if(length(impossible)!=0){options=options[-impossible,]}
	}
	grouping=list(analysis)
	if(nrow(options)!=0){
		for (i in 1:nrow(options)){
			grouping[[i]]=analysis
			grouping[[i]]$role=as.character(options[i,])
	}	}
grouping
}
