GENERALIZE <-
function(speakerID, proposition, situation){
	distinctiveness=world$distinctiveness; frequency=world$frequency; suffixThreshold=world$suffixThreshold; distinctions=world$distinctions; topicCopy=world$topicCopy; wordOrder=world$wordOrder
	speaker=population[[speakerID]]
	yangIndex=TRUE %in% c(speaker$usageHistory$index$no < ((speaker$usageHistory$index$yes + speaker$usageHistory$index$no)/log(speaker$usageHistory$index$yes + speaker$usageHistory$index$no)))
	yangPerson=TRUE %in% c(speaker$usageHistory$flag$person$no < ((speaker$usageHistory$flag$person$yes + speaker$usageHistory$flag$person$no)/log(speaker$usageHistory$flag$person$yes + speaker$usageHistory$flag$person$no)))
	yangRole=TRUE %in% c(speaker$usageHistory$flag$actor$no < ((speaker$usageHistory$flag$actor$yes + speaker$usageHistory$flag$actor$no)/log(speaker$usageHistory$flag$actor$yes + speaker$usageHistory$flag$actor$no))) | TRUE %in% c(speaker$usageHistory$flag$undergoer$no < ((speaker$usageHistory$flag$undergoer$yes + speaker$usageHistory$flag$undergoer$no)/log(speaker$usageHistory$flag$undergoer$yes + speaker$usageHistory$flag$undergoer$no)))
	yangTopic=FALSE; yangWordOrder=FALSE
	if(wordOrder==TRUE & sum(speaker$wordOrder$success)>8){	
		yangTopic=TRUE %in% speaker$topicPosition$success[speaker$topicPosition$position=='other'] < (sum(speaker$topicPosition$success)/log(sum(speaker$topicPosition$success)))
		yangWordOrder=TRUE %in% 
			c((sum(speaker$wordOrder$success)-speaker$wordOrder$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) |
			c(sum(speaker$wordOrder[grep('^A', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) |
			c(sum(speaker$wordOrder[grep('V$', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) |
			c(sum(speaker$wordOrder[grep('UV', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) |
			c(sum(speaker$wordOrder[grep('VU', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) 
		if(yangWordOrder==TRUE){
			if(TRUE %in% c(sum(speaker$wordOrder[grep('^A', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success))))){
				proposition=AGENTFIRST(proposition)
			}
			if(TRUE %in% c(sum(speaker$wordOrder[grep('V$', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success))))){
				proposition=VERBFINAL(proposition)
			}
			if(proposition$verb$type=='twoPlace'){
				actor=ifelse(ACTOR(proposition$verb[,grep('^Ext\\d', names(proposition$verb))], proposition$verb[,grep('^Int\\d', names(proposition$verb))])==1, 'external', 'internal')
				undergoer=ifelse(actor=='external', 'internal', 'external')
				if(TRUE %in% c(sum(speaker$wordOrder[grep('UV', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success))))){
					if(grep(actor, names(proposition)) < grep('verb', names(proposition))){proposition=proposition[c(actor, undergoer, 'verb', 'target')]}
					if(grep(actor, names(proposition)) > grep('verb', names(proposition))){proposition=proposition[c(undergoer, 'verb', actor, 'target')]}				
				}
				if(TRUE %in% c(sum(speaker$wordOrder[grep('VU', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success))))){
					if(grep(actor, names(proposition)) < grep('verb', names(proposition))){proposition=proposition[c(actor, 'verb', undergoer, 'target')]}
					if(grep(actor, names(proposition)) > grep('verb', names(proposition))){proposition=proposition[c('verb', undergoer, actor, 'target')]}				
				}
				if(TRUE %in% c((sum(speaker$wordOrder$success)-speaker$wordOrder$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success))))){
					order=speaker$wordOrder$order[c((sum(speaker$wordOrder$success)-speaker$wordOrder$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success))))]
					order=unlist(strsplit(order, '')); order=gsub('A', actor, order); order=gsub('U', undergoer, order); order=gsub('V', 'verb', order)
					proposition=proposition[c(order, 'target')]
			}	}
			if(yangTopic==TRUE){	
				proposition=TOPICFIRST(speakerID, proposition)
	}	}	}
	if(TRUE %in% c(yangIndex, yangPerson, yangRole)){
		if(proposition$verb$type=='onePlace'){
			verbRole='external'
			person=proposition$external$person
			actorScore=VMATCH(proposition$verb[,grep('^Ext', names(proposition$verb))], rep(1, length(distinctions)))
			undergoerScore=VMATCH(proposition$verb[,grep('^Ext', names(proposition$verb))], rep(0, length(distinctions)))
			semRole=ifelse(undergoerScore<actorScore, 'actor', 'undergoer')
			firstArgument=list(person=person, verbRole=verbRole, semRole=semRole, props=proposition$external[,grep('^D\\d', names(proposition$external))])		
		}
		if(proposition$verb$type=='twoPlace'){
			actorPerspective=ifelse(ACTOR(proposition$verb[,grep('^Ext', names(proposition$verb))], proposition$verb[,grep('^Int', names(proposition$verb))])==1, 'external', 'internal')
			verbRole=names(proposition[names(proposition)%in%c('internal', 'external')])[1]
			person=proposition[[verbRole]]$person
			semRole=ifelse(actorPerspective==verbRole, 'actor', 'undergoer')
			firstArgument=list(person=person, verbRole=verbRole, semRole=semRole, props=proposition[[verbRole]][,grep('^D\\d', names(proposition[[verbRole]]))])		
			verbRole=names(proposition[names(proposition)%in%c('internal', 'external')])[2]
			person=proposition[[verbRole]]$person
			semRole=ifelse(actorPerspective==verbRole, 'actor', 'undergoer')
			secondArgument=list(person=person, verbRole=verbRole, semRole=semRole, props=proposition[[verbRole]][,grep('^D\\d', names(proposition[[verbRole]]))])		
	}	}
	if(yangIndex==TRUE){
		index=FALSE
		data=speaker$usageHistory$index[speaker$usageHistory$index$person==firstArgument$person & speaker$usageHistory$index$role==firstArgument$semRole,]
		if(TRUE %in% c(data$no < (data$yes + data$no)/log(data$yes + data$no))){index=TRUE}		
		if(firstArgument$verbRole=='external' & 'extMarker'%in%names(proposition$verb) | firstArgument$verbRole=='internal' & 'intMarker'%in%names(proposition$verb)){index=FALSE}
		if(index==TRUE){	
			markers=speaker$nouns[speaker$nouns$person==firstArgument$person & speaker$nouns$productionEffort<=suffixThreshold,]
			if(nrow(markers)==0){markers=speaker$nouns[speaker$nouns$person==firstArgument$person,]}
			if(nrow(markers)!=0){
				markers=markers[sample(nrow(markers)),]
				markers$collostruction=0
				if(firstArgument$verbRole=='external'){
					markers$match=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
					collostructions=speaker$collostructions$index[speaker$collostructions$index$N==proposition$external$ID,]
					markers[markers$ID%in%collostructions$marker,]$collostruction=collostructions[na.omit(match(markers$ID, collostructions$marker)),]$frequency
	
				}
				if(firstArgument$verbRole=='internal'){
					markers$match=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
					collostructions=speaker$collostructions$index[speaker$collostructions$index$N==proposition$internal$ID,]
					markers[markers$ID%in%collostructions$marker,]$collostruction=collostructions[na.omit(match(markers$ID, collostructions$marker)),]$frequency
				}
				markers=markers[order(CANDIDATESCORE(markers, type='verbMarker'), decreasing=TRUE),]
				markerID=markers$ID[1]
				markerTarget=proposition[[firstArgument$verbRole]]$ID
				if(firstArgument$verbRole=='external'){
					proposition$verb$extMarkerID=markerID
					proposition$verb$extMarker=speaker$nouns[speaker$nouns$ID==markerID,]$form
					proposition$verb$extMarkerTarget=markerTarget
					if(frequency=='absolute'){proposition$verb$extMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
					if(frequency=='relative'){proposition$verb$extMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$verbMarker}
				}	
				if(firstArgument$verbRole=='internal'){
					proposition$verb$intMarkerID=markerID
					proposition$verb$intMarker=speaker$nouns[speaker$nouns$ID==markerID,]$form
					proposition$verb$intMarkerTarget=markerTarget
					if(frequency=='absolute'){proposition$verb$intMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
					if(frequency=='relative'){proposition$verb$intMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$verbMarker}
		}	}	}
		if(proposition$verb$type=='twoPlace'){
			index=FALSE
			data=speaker$usageHistory$index[speaker$usageHistory$index$person==secondArgument$person & speaker$usageHistory$index$role==secondArgument$semRole,]
			if(TRUE %in% c(data$no < (data$yes + data$no)/log(data$yes + data$no))){index=TRUE}		
			if(secondArgument$verbRole=='external' & 'extMarker'%in%names(proposition$verb)){index=FALSE}
			if(secondArgument$verbRole=='internal' & 'intMarker'%in%names(proposition$verb)){index=FALSE}
			if(index==TRUE){	
				markers=speaker$nouns[speaker$nouns$person==secondArgument$person & speaker$nouns$productionEffort<=suffixThreshold,]
				if(nrow(markers)!=0){
					markers=markers[sample(nrow(markers)),]
					markers$collostruction=0
					if(secondArgument$verbRole=='external'){
						markers$match=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
						collostructions=speaker$collostructions$index[speaker$collostructions$index$N==proposition$external$ID,]
						markers[markers$ID%in%collostructions$marker,]$collostruction=collostructions[na.omit(match(markers$ID, collostructions$marker)),]$frequency
					}
					if(secondArgument$verbRole=='internal'){
						markers$match=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
						collostructions=speaker$collostructions$index[speaker$collostructions$index$N==proposition$internal$ID,]
						markers[markers$ID%in%collostructions$marker,]$collostruction=collostructions[na.omit(match(markers$ID, collostructions$marker)),]$frequency
					}
					markers=markers[order(CANDIDATESCORE(markers, type='verbMarker'), decreasing=TRUE),]
					markerID=markers$ID[1]
					markerTarget=proposition[[secondArgument$verbRole]]$ID
					if(secondArgument$verbRole=='external'){
						proposition$verb$extMarkerID=markerID
						proposition$verb$extMarker=speaker$nouns[speaker$nouns$ID==markerID,]$form
						proposition$verb$extMarkerTarget=markerTarget
						if(frequency=='absolute'){proposition$verb$extMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
						if(frequency=='relative'){proposition$verb$extMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$verbMarker}
					}	
					if(secondArgument$verbRole=='internal'){
						proposition$verb$intMarkerID=markerID
						proposition$verb$intMarker=speaker$nouns[speaker$nouns$ID==markerID,]$form
						proposition$verb$intMarkerTarget=markerTarget
						if(frequency=='absolute'){proposition$verb$intMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
						if(frequency=='relative'){proposition$verb$intMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$verbMarker}
	}	}	}	}	}	
	if(yangPerson==TRUE | yangRole==TRUE){
		data=speaker$usageHistory$flag$person[speaker$usageHistory$flag$person$role==firstArgument$semRole & speaker$usageHistory$flag$person$person==firstArgument$person,]
		flag=TRUE %in% c(data$no < (data$yes + data$no)/log(data$yes + data$no))	
		if(flag==FALSE){	
			values=unlist(rep(firstArgument$props, distinctions)); values[is.na(values)]=-1
			values=values[1:length(speaker$usageHistory$flag[[firstArgument$semRole]]$value)]		
			data=speaker$usageHistory$flag[[firstArgument$semRole]][speaker$usageHistory$flag[[firstArgument$semRole]]$value==values,]
			flag=TRUE %in% c(data$no < (data$yes + data$no)/log(data$yes + data$no))	
		}
		if(flag==TRUE){
			done=FALSE
			topic=proposition[[firstArgument$verbRole]]$topic
			pronouns=speaker$nouns[speaker$nouns$person==firstArgument$person & speaker$nouns$productionEffort>world$referenceThreshold,]
			if(nrow(pronouns)>1){	
				pronouns$collostruction=0
				if(firstArgument$verbRole=='internal'){
					pronouns$match=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], pronouns[,grep('^D\\d',names(pronouns))])
					pronouns$typing=pronouns$match
					collostructions=speaker$collostructions$OV[speaker$collostructions$OV$V==proposition$verb$ID,]
					pronouns[pronouns$ID%in%collostructions$O,]$collostruction=collostructions[na.omit(match(pronouns$ID, collostructions$O)),]$frequency
				}
				if(firstArgument$verbRole=='external'){
					pronouns$match=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], pronouns[,grep('^D\\d',names(pronouns))])
					pronouns$typing=pronouns$match
					collostructions=speaker$collostructions$SV[speaker$collostructions$SV$V==proposition$verb$ID,]
					pronouns[pronouns$ID%in%collostructions$S,]$collostruction=collostructions[na.omit(match(pronouns$ID, collostructions$S)),]$frequency
				}
				pronoun=pronouns[order(CANDIDATESCORE(pronouns), decreasing=TRUE)[1],]
				if(firstArgument$person!=3){
					proposition[[firstArgument$verbRole]]=pronoun
					proposition[[firstArgument$verbRole]]$topic=topic
					done=TRUE		
				}	
				if(firstArgument$person==3){		
					if(firstArgument$semRole=='actor'){distractors=situation$personA}
					if(firstArgument$semRole=='undergoer'){distractors=situation$personU}
					if(sum(distractors==firstArgument$person, na.rm=TRUE)==1){	
						proposition[[firstArgument$verbRole]]=pronoun
						proposition[[firstArgument$verbRole]]$topic=topic
						done=TRUE
			}	}	}
			if(done==FALSE){
				markers=speaker$nouns[sample(nrow(speaker$nouns)), ]
				markers=markers[markers$person==3,]
				markers$collostruction=0
				if(firstArgument$verbRole=='internal'){
					markers$match=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
					markers$distractorMatch=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
					collostructions=speaker$collostructions$OV[speaker$collostructions$OV$V==proposition$verb$ID,]
					markers[markers$ID%in%collostructions$O,]$collostruction=collostructions[na.omit(match(markers$ID, collostructions$O)),]$frequency
				}
				if(firstArgument$verbRole=='external'){
					markers$match=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
					if(proposition$verb$type=='onePlace'){					
						markers$distractorMatch=.5
					}
					if(proposition$verb$type=='twoPlace'){					
						markers$distractorMatch=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])		
					}
					collostructions=speaker$collostructions$SV[speaker$collostructions$SV$V==proposition$verb$ID,]
					markers[markers$ID%in%collostructions$S,]$collostruction=collostructions[na.omit(match(markers$ID, collostructions$S)),]$frequency
				}
				markers=markers[order(CANDIDATESCORE(markers, type='nounMarker'), decreasing=TRUE),]
				markerID=0
				for (i in 1:nrow(markers)){
					if(markers[i,]$match > (markers[i,]$distractorMatch + distinctiveness)){
					markerID=markers[i,]$ID
					markerCollostruction=markers[i,]$collostruction
						break()
				}	}
				if(markerID==0){
					markerID=markers[MAX(markers$match, forceChoice=TRUE),]$ID
					markerCollostruction=markers[MAX(markers$match, forceChoice=TRUE),]$collostruction
				}
				proposition[[firstArgument$verbRole]]$markerID=markerID
				proposition[[firstArgument$verbRole]]$marker=speaker$nouns[speaker$nouns$ID==markerID,]$form
				if(frequency=='absolute'){proposition[[firstArgument$verbRole]]$markerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
				if(frequency=='relative'){proposition[[firstArgument$verbRole]]$markerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$nounMarker}
				proposition[[firstArgument$verbRole]]$markerRecency=speaker$nouns[speaker$nouns$ID==markerID,]$recency
				proposition[[firstArgument$verbRole]]$markerCollostruction=markerCollostruction
		}	}
		if(proposition$verb$type=='twoPlace'){
			data=speaker$usageHistory$flag$person[speaker$usageHistory$flag$person$role==secondArgument$semRole & speaker$usageHistory$flag$person$person==secondArgument$person,]
			flag=TRUE %in% c(data$no < (data$yes + data$no)/log(data$yes + data$no))	
			if(flag==FALSE){	
				values=unlist(rep(secondArgument$props, distinctions)); values[is.na(values)]=-1
				values=values[1:length(speaker$usageHistory$flag[[secondArgument$semRole]]$value)]		
				data=speaker$usageHistory$flag[[secondArgument$semRole]][speaker$usageHistory$flag[[secondArgument$semRole]]$value==values,]
				flag=TRUE %in% c(data$no < (data$yes + data$no)/log(data$yes + data$no))	
			}
			if(flag==TRUE){
				done=FALSE
				topic=proposition[[secondArgument$verbRole]]$topic
				pronouns=speaker$nouns[speaker$nouns$person==secondArgument$person & speaker$nouns$productionEffort>world$referenceThreshold,]
				if(nrow(pronouns)>1){	
					pronouns$collostruction=0
					if(secondArgument$verbRole=='internal'){
						pronouns$match=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], pronouns[,grep('^D\\d',names(pronouns))])
						pronouns$typing=pronouns$match
						collostructions=speaker$collostructions$OV[speaker$collostructions$OV$V==proposition$verb$ID,]
						pronouns[pronouns$ID%in%collostructions$O,]$collostruction=collostructions[na.omit(match(pronouns$ID, collostructions$O)),]$frequency
					}
					if(secondArgument$verbRole=='external'){
						pronouns$match=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], pronouns[,grep('^D\\d',names(pronouns))])
						pronouns$typing=pronouns$match
						collostructions=speaker$collostructions$SV[speaker$collostructions$SV$V==proposition$verb$ID,]
						pronouns[pronouns$ID%in%collostructions$S,]$collostruction=collostructions[na.omit(match(pronouns$ID, collostructions$S)),]$frequency
					}
					pronoun=pronouns[order(CANDIDATESCORE(pronouns), decreasing=TRUE)[1],]
					if(secondArgument$person!=3){
						proposition[[secondArgument$verbRole]]=pronoun
						proposition[[secondArgument$verbRole]]$topic=topic
						done=TRUE		
					}	
					if(secondArgument$person==3){		
						if(secondArgument$semRole=='actor'){distractors=situation$personA}
						if(secondArgument$semRole=='undergoer'){distractors=situation$personU}
						if(sum(distractors==secondArgument$person, na.rm=TRUE)==1){	
							proposition[[secondArgument$verbRole]]=pronoun
							proposition[[secondArgument$verbRole]]$topic=topic
							done=TRUE
				}	}	}
				if(done==FALSE){
					markers=speaker$nouns[speaker$nouns$person==3, ]
					markers=markers[sample(nrow(markers)),]
					markers$collostruction=0
					if(secondArgument$verbRole=='internal'){
						markers$match=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
						markers$distractorMatch=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
						collostructions=speaker$collostructions$OV[speaker$collostructions$OV$V==proposition$verb$ID,]
						markers[markers$ID%in%collostructions$O,]$collostruction=collostructions[na.omit(match(markers$ID, collostructions$O)),]$frequency
					}
					if(secondArgument$verbRole=='external'){
						markers$match=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
						markers$distractorMatch=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])		
						collostructions=speaker$collostructions$SV[speaker$collostructions$SV$V==proposition$verb$ID,]
						markers[markers$ID%in%collostructions$S,]$collostruction=collostructions[na.omit(match(markers$ID, collostructions$S)),]$frequency
					}
					markers=markers[order(CANDIDATESCORE(markers, type='nounMarker'), decreasing=TRUE),]
					markerID=0
					for (i in 1:nrow(markers)){
						if(markers[i,]$match > (markers[i,]$distractorMatch + distinctiveness)){
						markerID=markers[i,]$ID
						markerCollostruction=markers[i,]$collostruction
							break()
					}	}
					if(markerID==0){
						markerID=markers[MAX(markers$match, forceChoice=TRUE),]$ID
						markerCollostruction=markers[MAX(markers$match, forceChoice=TRUE),]$collostruction
					}
					proposition[[secondArgument$verbRole]]$markerID=markerID
					proposition[[secondArgument$verbRole]]$marker=speaker$nouns[speaker$nouns$ID==markerID,]$form
					if(frequency=='absolute'){proposition[[secondArgument$verbRole]]$markerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
					if(frequency=='relative'){proposition[[secondArgument$verbRole]]$markerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$nounMarker}
					proposition[[secondArgument$verbRole]]$markerRecency=speaker$nouns[speaker$nouns$ID==markerID,]$recency
					proposition[[secondArgument$verbRole]]$markerCollostruction=markerCollostruction
	}	}	}	}
proposition	
}			


