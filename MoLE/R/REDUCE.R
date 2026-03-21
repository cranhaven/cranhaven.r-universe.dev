REDUCE <-
function(speakerID, proposition){
	erosionMax=world$erosionMax; reductionFrequencyThreshold=world$reductionFrequencyThreshold; blocking=world$formBlocking; reductionCollostructionThreshold=world$reductionCollostructionThreshold; reductionRecencyThreshold=world$reductionRecencyThreshold
	speaker=population[[speakerID]]
	reductionFrequencyThreshold=reductionFrequencyThreshold*speaker$age
	verb=proposition$verb
	if(nchar(verb$form)>erosionMax & (verb$frequency > reductionFrequencyThreshold | verb$recency <= reductionRecencyThreshold)){	
		form=gsub('(.*).','\\1',verb$form)
		if(blocking==FALSE){proposition$verb$form=form}
		if(blocking==TRUE){if(!form%in%speaker$verbs$form){proposition$verb$form=form}}
	}	
	if('extMarkerID'%in%names(verb)){
		#if frequent and recent or predictable (through collostruction)
		if(nchar(verb$extMarker)>erosionMax & (proposition$verb$extMarkerFrequency > reductionFrequencyThreshold | speaker$nouns[speaker$nouns$ID==proposition$verb$extMarkerID,]$recency <= reductionRecencyThreshold)){
			form=gsub('(.*).','\\1',verb$extMarker)
			if(blocking==FALSE){proposition$verb$extMarker=form}
			if(blocking==TRUE){if(!form%in%speaker$nouns$form){proposition$verb$extMarker=form}}
	}	}
	if('intMarkerID'%in%names(proposition$verb)){
		#if frequent and recent or predictable (through collostruction)
		if(nchar(verb$intMarker)>erosionMax & (proposition$verb$intMarkerFrequency > reductionFrequencyThreshold | speaker$nouns[speaker$nouns$ID==proposition$verb$intMarkerID,]$recency <= reductionRecencyThreshold)){
			form=gsub('(.*).','\\1',verb$intMarker)
			if(blocking==FALSE){proposition$verb$intMarker=form}
			if(blocking==TRUE){if(!form%in%speaker$nouns$form){proposition$verb$intMarker=form}}
	}	}
	external=proposition$external
	if(nchar(external$form)>erosionMax & (external$frequency > reductionFrequencyThreshold | external$recency <= reductionRecencyThreshold | external$collostruction > reductionCollostructionThreshold)){
		form=gsub('(.*).','\\1',external$form)
		if(blocking==FALSE){proposition$external$form=form}
		if(blocking==TRUE){if(!form%in%speaker$nouns$form){proposition$external$form=form}}
	}
	if('marker'%in%names(proposition$external)){
		if(nchar(external$marker)>erosionMax & (proposition$external$markerFrequency > reductionFrequencyThreshold | proposition$external$markerRecency <= reductionRecencyThreshold | proposition$external$markerCollostruction > reductionCollostructionThreshold)){
			form=gsub('(.*).','\\1',proposition$external$marker)
			if(blocking==FALSE){proposition$external$marker=form}
			if(blocking==TRUE){if(!form%in%speaker$nouns$form){proposition$external$marker=form}}
	}	}
	if(proposition$verb$type=='twoPlace'){
		internal=proposition$internal
		if(nchar(internal$form)>erosionMax & (internal$frequency > reductionFrequencyThreshold | internal$recency <= reductionRecencyThreshold | internal$collostruction > reductionCollostructionThreshold)){
			form=gsub('(.*).','\\1',internal$form)
			if(blocking==FALSE){proposition$internal$form=form}
			if(blocking==TRUE){if(!form%in%speaker$nouns$form){proposition$internal$form=form}}
		}
		if('marker'%in%names(proposition$internal)){
			if(nchar(internal$marker)>erosionMax & (proposition$internal$markerFrequency > reductionFrequencyThreshold | proposition$internal$markerRecency <= reductionRecencyThreshold | proposition$internal$markerCollostruction > reductionCollostructionThreshold)){
				form=gsub('(.*).','\\1',proposition$internal$marker)
				if(blocking==FALSE){proposition$internal$marker=form}
				if(blocking==TRUE){if(!form%in%speaker$nouns$form){proposition$internal$marker=form}}
	}	}	}
proposition
}
