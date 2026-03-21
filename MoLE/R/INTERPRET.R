INTERPRET <-
function(hearerID, utterance, situation){
	wordOrder=world$wordOrder; frequency=world$frequency
	hearer=population[[hearerID]]
	nEvents=nrow(situation) 
	interpretation=list()
	analysis=ANALYZE(hearerID, utterance, situation)
	grouping=GROUP(hearerID, analysis)
	for(j in 1:length(grouping)){
		#first use explicit role marking
		grouping[[j]]=NOUNMORPHOLOGY(hearerID, grouping[[j]])
		#then word order (if still necessary)
		if(wordOrder==TRUE){
			if('verb'%in%grouping[[j]]$role){
				grouping[[j]]=WORDORDER(hearerID, grouping[[j]])
		}	}
		#then verb morphology (idem)
		grouping[[j]]=VERBMORPHOLOGY(hearerID, grouping[[j]])
		solved=TRUE
		if('verb'%in%grouping[[j]]$role){
			if(grouping[[j]][grouping[[j]]$role=='verb',]$verbType=='twoPlace'){
				if(!'external'%in%grouping[[j]]$role | !'internal'%in%grouping[[j]]$role){
					solved=FALSE
			}	}
			if(grouping[[j]][grouping[[j]]$role=='verb',]$verbType=='onePlace'){
				if(!'external'%in%grouping[[j]]$role){
					solved=FALSE
			}	}
			if(solved==FALSE){	
				grouping[[j]]=PROTOINTERPRETATION(hearerID, grouping[[j]])	
		}	}
		interpretation.int=INTERPRET.INT(hearerID, grouping[[j]], situation)
		if(interpretation.int$target$totalMatch>0){interpretation[[length(interpretation) + 1]]=interpretation.int}
	}	
	if(length(interpretation)==1){scores=1}
	if(length(interpretation) > 1){
		if(nEvents > 1){
			scores=vector()
			for(i in 1:length(interpretation)){
				scores[i]=interpretation[[i]]$target$totalMatch
		}	}
		if(nEvents==1){
			scores=vector()
			for(i in 1:length(interpretation)){
				verb=interpretation[[i]]$verb
				scores[i]=0
					if('extMarkerID'%in%names(verb)){
					if(frequency=='relative'){scores[i]=scores[i] + interpretation[[i]]$verb$extMarkerFrequency/max(hearer$nouns$verbMarker)}
					if(frequency=='absolute'){scores[i]=scores[i] + interpretation[[i]]$verb$extMarkerFrequency/max(hearer$nouns$frequency)}
				}
				if('intMarkerID'%in%names(verb)){
					if(frequency=='relative'){scores[i]=scores[i] + interpretation[[i]]$verb$intMarkerFrequency/max(hearer$nouns$verbMarker)}
					if(frequency=='absolute'){scores[i]=scores[i] + interpretation[[i]]$verb$intMarkerFrequency/max(hearer$nouns$frequency)}
				}
				if(is.data.frame(interpretation[[i]]$external)){
					external=interpretation[[i]]$external
					collostruction=population[[hearerID]]$collostructions$SV[intersect(grep(paste('^',verb$ID,'$',sep=''), population[[hearerID]]$collostructions$SV$V), grep(paste('^',external$ID,'$',sep=''), population[[hearerID]]$collostructions$SV$S)),]$frequency
					if(length(collostruction)==0){collostruction=0}
					interpretation[[i]]$external$collostruction=collostruction
					if(frequency=='relative'){
						scores[i]=scores[i] + interpretation[[i]]$external$argument/max(hearer$nouns$argument) + interpretation[[i]]$external$collostruction/max(hearer$collostructions$SV$freq)
						if('marker'%in%names(external)){scores[i]=scores[i] + external$markerFrequency/max(hearer$nouns$nounMarker)}
					}
					if(frequency=='absolute'){
						scores[i]=scores[i] + interpretation[[i]]$internal$argument/max(hearer$nouns$frequency) + interpretation[[i]]$external$collostruction/max(hearer$collostructions$SV$freq)
						if('marker'%in%names(external)){scores[i]=scores[i] + external$markerFrequency/max(hearer$nouns$frequency)}
				}	}
				if(is.data.frame(interpretation[[i]]$internal)){
					internal=interpretation[[i]]$internal
					collostruction=population[[hearerID]]$collostructions$SV[intersect(grep(paste('^',verb$ID,'$',sep=''), population[[hearerID]]$collostructions$SV$V), grep(paste('^',internal$ID,'$',sep=''), population[[hearerID]]$collostructions$SV$S)),]$frequency
					if(length(collostruction)==0){collostruction=0}
					interpretation[[i]]$internal$collostruction=collostruction
					if(frequency=='relative'){
						scores[i]=scores[i] + interpretation[[i]]$internal$argument/max(hearer$nouns$argument) + interpretation[[i]]$internal$collostruction/max(hearer$collostructions$OV$freq)
						if('marker'%in%names(internal)){scores[i]=scores[i] + internal$markerFrequency/max(hearer$nouns$nounMarker)}
					}
					if(frequency=='absolute'){
						scores[i]=scores[i] + interpretation[[i]]$internal$argument/max(hearer$nouns$frequency) + interpretation[[i]]$internal$collostruction/max(hearer$collostructions$OV$freq)
						if('marker'%in%names(internal)){scores[i]=scores[i] + internal$markerFrequency/max(hearer$nouns$frequency)}
	}	}	}	}	}
	if(length(interpretation)!=0){
		interpretation=interpretation[[MAX(scores, forceChoice=TRUE)]]
	} else {interpretation='?'}	
interpretation<<-interpretation
interpretation
}
