REFCHECK <-
function(speakerID, proposition, situation){
	referenceThreshold=world$referenceThreshold; distinctions=world$distinctions; distinctiveness=world$distinctiveness; frequency=world$frequency
	speaker=population[[speakerID]]
	target=situation[situation$target==1,]
	distractors=situation[situation$target==0,]
	nounCandidates=speaker$nouns[speaker$nouns$productionEffort > referenceThreshold,]
	nounCandidates=nounCandidates[sample(nrow(nounCandidates)),]
	verb=proposition$verb
	external=proposition$external
	externalRole=ifelse(ACTOR(verb[,grep('^Ext', names(verb))], verb[, grep('^Int', names(verb))])==1, 'actor', 'undergoer')
	if(externalRole=='actor'){
		externalTarget=target[,grep('^A',names(target))]
		externalDistractors=unique(distractors[,grep('^A',names(distractors))])
	}
	if(externalRole=='undergoer'){
		externalTarget=target[,grep('^U',names(target))]
		externalDistractors=unique(distractors[,grep('^U',names(distractors))])
	}
	if(nrow(externalDistractors)!=0){externalDistractors=externalDistractors[!VMATCH(externalTarget, externalDistractors)==1,]}
	if(nchar(external$form) <= referenceThreshold){
		if(!'extMarkerID'%in%names(verb)){
			proposition$verb$extMarkerID=external$ID
			proposition$verb$extMarker=external$form
			proposition$verb$extMarkerFrequency=ifelse(frequency=='absolute',speaker$nouns[speaker$nouns$ID==external$ID,]$frequency, speaker$nouns[speaker$nouns$ID==external$ID,]$verbMarker)
		}
		topic=external$topic; person=external$person
		marker=0; if('markerID'%in%names(external)){marker=external[,grep('^marker', names(external))]}
		nouns=nounCandidates[nounCandidates$person==person,]	
		if(person==3){nouns$match=VMATCH(externalTarget, nouns)}	#match non-local arguments with real-world argument
		if(person!=3){
			if(nrow(nouns)>0){nouns$match=VMATCH(verb[,grep('^Ext', names(verb))], nouns); marker=0}	#match local pronoun with role and remove marker
			if(nrow(nouns)==0){	#if there's no local pronominal paradigm yet, select prominent noun for local ref
				nouns=nounCandidates[nounCandidates$person==3,]
				nouns$match=VMATCH(rep(1, length(distinctions)), nouns)	
		}	}
		nouns$collostruction=0
		collostructions=speaker$collostructions$SV[speaker$collostructions$SV$V==verb$ID,]
		nouns[nouns$ID%in%collostructions$S,]$collostruction=collostructions[na.omit(match(nouns$ID, collostructions$S)),]$frequency
		argumentOrder=order(CANDIDATESCORE(nouns), decreasing=TRUE)
		if(person!=3){argument=nouns[argumentOrder[1],]}
		if(person==3){		
			argument=''
			if(nrow(situation) > 1){
				if(nrow(externalDistractors)!=0){
					for (i in argumentOrder){
						distractorMatch=MAX(VMATCH(nouns[i,grep('^D\\d',names(nouns))], externalDistractors), value=TRUE, forceChoice=TRUE)
						if(nouns[i,]$match > (distractorMatch + distinctiveness)){
							argument=nouns[i,]
							break()
			}	}	}	}
			if(nrow(situation)==1){	
				argument=nouns[argumentOrder[1],]
			}
			if(!is.data.frame(argument)){argument=nouns[MAX(nouns$match, forceChoice=TRUE),]}	
		}
		argument$typing=VMATCH(argument[,grep('^D\\d',names(argument)),], verb[,grep('^Ext\\d',names(verb)),])
		argument$topic=topic; argument$person=person
		if(is.data.frame(marker)){argument=cbind(argument, marker)}
		proposition$external=argument
		proposition$verb$extMarkerTarget=proposition$external$ID
	}	
	if(verb$type=='twoPlace'){
		internal=proposition$internal
		if(externalRole=='actor'){
			internalTarget=target[,grep('^U',names(target))]
			internalDistractors=unique(distractors[,grep('^U',names(distractors))])
		}
		if(externalRole=='undergoer'){
			internalTarget=target[,grep('^A',names(target))]
			internalDistractors=unique(distractors[,grep('^A',names(distractors))])
		}
		if(nrow(internalDistractors)!=0){internalDistractors=internalDistractors[!VMATCH(internalTarget, internalDistractors)==1,]}
		if(nchar(internal$form) <= referenceThreshold){
			if(!'intMarkerID'%in%names(verb)){	
				proposition$verb$intMarkerID=internal$ID
				proposition$verb$intMarker=internal$form
				proposition$verb$intMarkerFrequency=ifelse(frequency=='absolute',speaker$nouns[speaker$nouns$ID==internal$ID,]$frequency, speaker$nouns[speaker$nouns$ID==internal$ID,]$verbMarker)
			}
			topic=internal$topic; person=internal$person
			marker=0; if('markerID'%in%names(internal)){marker=internal[,grep('^marker', names(internal))]}
			nouns=nounCandidates[nounCandidates$person==person,]	
			if(person==3){nouns$match=VMATCH(internalTarget, nouns)}	
			if(person!=3){
				if(nrow(nouns)>0){nouns$match=VMATCH(verb[,grep('^Int', names(verb))], nouns); marker=0}	
				if(nrow(nouns)==0){	
					nouns=nounCandidates[nounCandidates$person==3,]
					nouns$match=VMATCH(rep(1, length(distinctions)), nouns)	
			}	}
			nouns$collostruction=0
			collostructions=speaker$collostructions$OV[speaker$collostructions$OV$V==verb$ID,]
			nouns[nouns$ID%in%collostructions$O,]$collostruction=collostructions[na.omit(match(nouns$ID, collostructions$O)),]$frequency
			argumentOrder=order(CANDIDATESCORE(nouns), decreasing=TRUE)
			if(person!=3){argument=nouns[argumentOrder[1],]}
			if(person==3){		
				argument=''
				if(nrow(situation) > 1){
					if(nrow(internalDistractors)!=0){
						for (i in argumentOrder){
							distractorMatch=MAX(VMATCH(nouns[i,grep('^D\\d',names(nouns))], internalDistractors), value=TRUE, forceChoice=TRUE)
							if(nouns[i,]$match > (distractorMatch + distinctiveness)){
								argument=nouns[i,]
								break()
				}	}	}	}
				if(nrow(situation)==1){	
					argument=nouns[argumentOrder[1],]
				}
				if(!is.data.frame(argument)){argument=nouns[MAX(nouns$match, forceChoice=TRUE),]}	
			}
			argument$typing=VMATCH(argument[,grep('^D\\d',names(argument)),], verb[,grep('^Int\\d',names(verb)),])
			argument$topic=topic; argument$person=person
			if(is.data.frame(marker)){argument=cbind(argument, marker)}
			if(is.data.frame(marker)){argument=cbind(argument, marker)}
			proposition$internal=argument
			proposition$verb$intMarkerTarget=proposition$internal$ID		
	}	}
proposition
}
