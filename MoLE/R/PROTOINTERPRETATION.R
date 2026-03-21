PROTOINTERPRETATION <-
function(hearerID, analysis){
	#proto reflexive
	if(length(grep('\\?', analysis$role))==2 & length(unique(analysis[grep('\\?', analysis$role),]$nounID))==1 & !'internal'%in%analysis$role & !'external'%in%analysis$role){
		analysis[grep('\\?', analysis$role),]$role=sample(c('external','internal'))
	}
	if('?'%in%analysis$role){
		if(analysis[analysis$role=='verb',]$verbType=='onePlace'){
			if(!'external'%in%analysis$role){
				typeMatch=TYPEMATCH(hearerID, analysis)
				if('external'%in%typeMatch$role){analysis[grep('external', typeMatch$role),]$role='external'}
		}	}
		if(analysis[analysis$role=='verb',]$verbType=='twoPlace'){
			if(!'external'%in%analysis$role | !'internal'%in%analysis$role){
				typeMatch=TYPEMATCH(hearerID, analysis)
				if(!'external'%in%analysis$role & 'external'%in%typeMatch$role){analysis[grep('external',typeMatch$role),]$role='external'}
				if(!'internal'%in%analysis$role & 'internal'%in%typeMatch$role){analysis[grep('internal',typeMatch$role),]$role='internal'}
	}	}	}
	#one left
	if(length(grep('\\?', analysis$role))==1 & !'external'%in%analysis$role){analysis[grep('\\?', analysis$role),]$role='external'}
	if(length(grep('\\?', analysis$role))==1 & !'internal'%in%analysis$role){analysis[grep('\\?', analysis$role),]$role='internal'}
	#guess
	if(length(grep('\\?', analysis$role))>1){
		if(!'internal'%in%analysis$role & !'external'%in%analysis$role){analysis[sample(grep('\\?', analysis$role),2),]$role=c('external', 'internal')}
		if(!'internal'%in%analysis$role){analysis[sample(grep('\\?', analysis$role),1),]$role='internal'}
		if(!'external'%in%analysis$role){analysis[sample(grep('\\?', analysis$role),1),]$role='external'}
	}	
analysis
}
