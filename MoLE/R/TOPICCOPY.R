TOPICCOPY <-
function(speakerID, proposition){
	distinctiveness=world$distinctiveness; frequency=world$frequency
	speaker=population[[speakerID]]
	markerID=0
	if(proposition$verb$type=='onePlace' & proposition$external$recency!=0){	#only copy for reestablished/non-continuous topics (cf. Givon; idem for twoPlace below)
		markers=speaker$nouns[speaker$nouns$person==proposition$external$person,]	
		markers=markers[sample(nrow(markers)),]
		markers$match=VMATCH(proposition$external[,grep('^D\\d',names(proposition$external))], markers[,grep('^D\\d',names(markers))])
		if(!'extMarkerID'%in%names(proposition$verb)){
			markers$collostruction=0
			collostructions=speaker$collostructions$index[speaker$collostructions$index$N==proposition$external$ID,]
			markers[markers$ID%in%collostructions$marker,]$collostruction=collostructions[na.omit(match(markers$ID, collostructions$marker)),]$frequency
			markers=markers[order(CANDIDATESCORE(markers, type='verbMarker'), decreasing=TRUE),]
			for (i in 1:nrow(markers)){
				if(markers[i,]$match > (1-distinctiveness)){
					markerID=markers[i,]$ID
					break()
			}	}
			if(markerID==0){markerID=markers[MAX(markers$match, forceChoice=TRUE),]$ID}
			markerTarget=proposition$external$ID
	}	}
	if(proposition$verb$type=='twoPlace'){
		if(proposition$external$topic==1 & proposition$external$recency!=0 & !'extMarkerID'%in%names(proposition$verb)){
			markerTarget=proposition$external$ID
			markers=speaker$nouns[speaker$nouns$person==proposition$external$person,]	
			markers=markers[sample(nrow(markers)),]
			markers$match=VMATCH(proposition$external[,grep('^D\\d',names(proposition$external))], markers[,grep('^D\\d',names(markers))])
			markers$distractorMatch=0
			markers[markers$person==proposition$external$person, ]$distractorMatch=VMATCH(proposition$internal[,grep('^D\\d',names(proposition$internal))], markers[markers$person==proposition$external$person,grep('^D\\d',names(markers))])
			markers$collostruction=0
			collostructions=speaker$collostructions$index[speaker$collostructions$index$N==proposition$external$ID,]
			markers[markers$ID%in%collostructions$marker,]$collostruction=collostructions[na.omit(match(markers$ID, collostructions$marker)),]$frequency
			markers=markers[order(CANDIDATESCORE(markers, type='verbMarker'), decreasing=TRUE),]
			for (i in 1:nrow(markers)){
				if(markers[i,]$match > (markers[i,]$distractorMatch + distinctiveness)){
					markerID=markers[i,]$ID
					break()
				}
				if(markerID==0){markerID=markers[MAX(markers$match, forceChoice=TRUE),]$ID}
		}	}
		if(proposition$internal$topic==1 & proposition$internal$recency!=0 &!'intMarkerID'%in%names(proposition$verb)){
			markerTarget=proposition$internal$ID
			markers=speaker$nouns[speaker$nouns$person==proposition$internal$person,]	
			markers=markers[sample(nrow(markers)),]
			markers$match=VMATCH(proposition$internal[,grep('^D\\d',names(proposition$internal))], markers[,grep('^D\\d',names(markers))])
			markers$distractorMatch=0
			markers[markers$person==proposition$internal$person, ]$distractorMatch=VMATCH(proposition$external[,grep('^D\\d',names(proposition$external))], markers[markers$person==proposition$internal$person,grep('^D\\d',names(markers))])
			markers$collostruction=0
			collostructions=speaker$collostructions$index[speaker$collostructions$index$N==proposition$internal$ID,]
			markers[markers$ID%in%collostructions$marker,]$collostruction=collostructions[na.omit(match(markers$ID, collostructions$marker)),]$frequency
			markers=markers[order(CANDIDATESCORE(markers, type='verbMarker'), decreasing=TRUE),]
			for (i in 1:nrow(markers)){
				if(markers[i,]$match > (markers[i,]$distractorMatch + distinctiveness)){
					markerID=markers[i,]$ID
					break()
				}
				if(markerID==0){markerID=markers[MAX(markers$match, forceChoice=TRUE),]$ID}
		}	}
		if(proposition$internal$topic==1 & markerID!=0){
			proposition$verb$intMarkerID=markerID
			proposition$verb$intMarker=speaker$nouns[speaker$nouns$ID==markerID,]$form
			proposition$verb$intMarkerTarget=markerTarget
			if(frequency=='absolute'){proposition$verb$intMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
			if(frequency=='relative'){proposition$verb$intMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$verbMarker}
	}	}		
	if(proposition$external$topic==1  & markerID!=0){
		proposition$verb$extMarkerID=markerID
		proposition$verb$extMarker=speaker$nouns[speaker$nouns$ID==markerID,]$form
		proposition$verb$extMarkerTarget=markerTarget	
		if(frequency=='absolute'){proposition$verb$extMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
		if(frequency=='relative'){proposition$verb$extMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$verbMarker}
	}	
proposition
}
