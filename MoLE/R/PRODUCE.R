PRODUCE <-
function(speakerID, prep){
	suffixThreshold=world$suffixThreshold
	utterance=paste(names(prep), collapse=' '); utterance=gsub(' target', '', utterance)
	if('verb'%in%names(prep)){
		verb=prep$verb$form
		if('intMarkerID'%in%names(prep$verb)){		#internal marker closest to verb (cf. Dryer; not exploited by hearer!)
			if(nchar(prep$verb$intMarker) <= suffixThreshold){marker=prep$verb$intMarker; verb=paste(verb, marker, sep='')}	#first suffixes...
		}
		if('extMarkerID'%in%names(prep$verb)){
			if(nchar(prep$verb$extMarker) <= suffixThreshold){marker=prep$verb$extMarker; verb=paste(verb, marker, sep='')}
		}
		if('intMarkerID'%in%names(prep$verb)){																				
			if(nchar(prep$verb$intMarker) > suffixThreshold){marker=prep$verb$intMarker; verb=paste(verb, marker, sep=' ')}	#...then adpositions
		}
		if('extMarkerID'%in%names(prep$verb)){
			if(nchar(prep$verb$extMarker) > suffixThreshold){marker=prep$verb$extMarker; verb=paste(verb, marker, sep=' ')}
		}
	}
	if('internal'%in%names(prep)){
		internal=prep$internal$form
		if('marker'%in%names(prep$internal)){	
			if(nchar(prep$internal$marker) <= suffixThreshold){marker=prep$internal$marker; internal=paste(internal, marker, sep='')}
			if(nchar(prep$internal$marker) > suffixThreshold){marker=prep$internal$marker; internal=paste(internal, marker, sep=' ')}
		}
		if('verb'%in%names(prep)){
			if(nchar(internal)<suffixThreshold & !'intMarker'%in%names(prep$verb)){	#again, internal marker closest to verb
				verb=paste(verb, internal, sep='')
				utterance=gsub('\\s?internal', '', utterance)
	}	}	}
	if('external'%in%names(prep)){
		external=prep$external$form
		if('marker'%in%names(prep$external)){	
			if(nchar(prep$external$marker) <= suffixThreshold){marker=prep$external$marker; external=paste(external, marker, sep='')}
			if(nchar(prep$external$marker) > suffixThreshold){marker=prep$external$marker; external=paste(external, marker, sep=' ')}
		}
		if('verb'%in%names(prep)){
			if(nchar(external)<suffixThreshold & !'extMarker'%in%names(prep$verb)){
				verb=paste(verb, external, sep='')
				utterance=gsub('\\s?external', '', utterance)
	}	}	}
	if(grepl('verb', utterance)){utterance=gsub('verb', verb, utterance)}
	if(grepl('internal', utterance)){utterance=gsub('internal', internal, utterance)}
	if(grepl('external', utterance)){utterance=gsub('external', external, utterance)}
	utterance=gsub('^\\s','',utterance)
utterance<<-utterance
utterance
}
