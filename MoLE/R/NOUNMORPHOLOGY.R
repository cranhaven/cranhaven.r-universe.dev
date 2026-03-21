NOUNMORPHOLOGY <-
function(hearerID, analysis){
	distinctiveness=world$distinctiveness
	hearer=population[[hearerID]]
	nouns=hearer$nouns; verbs=hearer$verbs
	if('nounSuffix'%in%analysis$role | 'nounAdposition'%in%analysis$role){	
		analysis$nounSuffixExternalScore=0; analysis$nounSuffixInternalScore=0		
		for (i in grep('nounSuffix', analysis$role)){
			nounSuffixProfile=nouns[nouns$ID==analysis[i,]$nounMarkerID,grep('^D\\d',names(nouns))]
			external=verbs[verbs$ID==analysis[analysis$role=='verb',]$verbID,grep('^Ext\\d',names(verbs))]
			internal=verbs[verbs$ID==analysis[analysis$role=='verb',]$verbID,grep('^Int\\d',names(verbs))]
			analysis[i,]$nounSuffixExternalScore=VMATCH(nounSuffixProfile, external)
			analysis[i,]$nounSuffixInternalScore=VMATCH(nounSuffixProfile, internal)
		}
		if(length(MAX(analysis$nounSuffixExternalScore, forceChoice=FALSE))==1){
			external=MAX(analysis$nounSuffixExternalScore)
			if(analysis[external,]$nounSuffixExternalScore > (analysis[external,]$nounSuffixInternalScore + distinctiveness)){	#if nounSuffix is cleary external
				if(analysis[external,]$nounSuffixExternalScore > analysis[MAX(analysis$nounSuffixExternalScore, 2, forceChoice=TRUE),]$nounSuffixExternalScore + distinctiveness){	#and sufficiently better than second best
					if('external'%in%analysis$role){analysis[analysis$role=='external',]$role='?'}	#overwrite alternative analyses
					analysis[external-1,]$role='external'
		}	}	}	
		if(length(MAX(analysis$nounSuffixInternalScore, forceChoice=FALSE))==1){
			internal=MAX(analysis$nounSuffixInternalScore)
			if(analysis[internal,]$nounSuffixInternalScore > (analysis[internal,]$nounSuffixExternalScore + distinctiveness)){	
				if(analysis[internal,]$nounSuffixInternalScore > analysis[MAX(analysis$nounSuffixInternalScore, 2, forceChoice=TRUE),]$nounSuffixInternalScore + distinctiveness){	
					if('internal'%in%analysis$role){analysis[analysis$role=='internal',]$role='?'}
					analysis[internal-1,]$role='internal'
	}	}	}	}	
	if('nounAdposition'%in%analysis$role){	
		analysis$nounAdpositionExternalScore=0; analysis$nounAdpositionInternalScore=0	#future work: allow for oblique roles
		for (i in grep('nounAdposition', analysis$role)){
			nounAdpositionProfile=nouns[nouns$ID==analysis[i,]$nounID,grep('^D\\d',names(nouns))]
			external=verbs[verbs$ID==analysis[analysis$role=='verb',]$verbID,grep('^Ext\\d',names(verbs))]
			internal=verbs[verbs$ID==analysis[analysis$role=='verb',]$verbID,grep('^Int\\d',names(verbs))]
			analysis[i,]$nounAdpositionExternalScore=VMATCH(nounAdpositionProfile, external)
			analysis[i,]$nounAdpositionInternalScore=VMATCH(nounAdpositionProfile, internal)
		}
		if(length(MAX(analysis$nounAdpositionExternalScore, forceChoice=FALSE))==1){
			external=MAX(analysis$nounAdpositionExternalScore)
			if(analysis[external,]$nounAdpositionExternalScore > (analysis[external,]$nounAdpositionInternalScore + distinctiveness)){	
				if(analysis[external,]$nounAdpositionExternalScore > analysis[MAX(analysis$nounAdpositionExternalScore, 2, forceChoice=TRUE),]$nounAdpositionExternalScore + distinctiveness){	
					if('external'%in%analysis$role){analysis[analysis$role=='external',]$role='?'}
					if(analysis[external-1,]$role!='nounSuffix'){	
						analysis[external-1,]$role='external'	
					}
					if(analysis[external-1,]$role=='nounSuffix'){	#marker overrules suffix with same host
						analysis[external-2,]$role='external'		
		}	}	}	}	
		if(length(MAX(analysis$nounAdpositionInternalScore, forceChoice=FALSE))==1){
			internal=MAX(analysis$nounAdpositionInternalScore)
			if(analysis[internal,]$nounAdpositionInternalScore > (analysis[internal,]$nounAdpositionExternalScore + distinctiveness)){	
				if(analysis[internal,]$nounAdpositionInternalScore > analysis[MAX(analysis$nounAdpositionInternalScore, 2, forceChoice=TRUE),]$nounAdpositionInternalScore + distinctiveness){	
					if('internal'%in%analysis$role){analysis[analysis$role=='internal',]$role='?'}
					if(analysis[internal-1,]$role!='nounSuffix'){	
						analysis[internal-1,]$role='internal'	
					}
					if(analysis[internal-1,]$role=='nounSuffix'){	
						analysis[internal-2,]$role='internal'		
	}	}	}	}	}	
analysis
}
