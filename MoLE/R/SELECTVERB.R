SELECTVERB <-
function(speakerID, situation, actor=NULL, undergoer=NULL){
	roleNoise=world$roleNoise; distinctiveness=world$distinctiveness
	speaker=population[[speakerID]]
	target=situation[situation$target==1,]
	verbTarget=target[,grep('^V\\d',names(target))]
	targetTransitivity=ifelse(is.na(target$personU), 'onePlace', 'twoPlace')
	verbs=speaker$verbs[speaker$verbs$type==targetTransitivity,]
	verbs=verbs[sample(nrow(verbs)),]
	verbs$match=VMATCH(verbTarget, verbs[,grep('^D\\d',names(verbs))])
	verbs$collostruction=0
	if(!is.null(actor) & is.null(undergoer)){	#undergoer more important for verb selection
		collostructions=speaker$collostructions$SV[speaker$collostructions$SV$S==actor$ID,]
		verbs[verbs$ID%in%collostructions$V,]$collostruction=collostructions[na.omit(match(verbs$ID, collostructions$V)),]$frequency
	}
	if(!is.null(undergoer)){
		collostructions=speaker$collostructions$OV[speaker$collostructions$OV$O==undergoer$ID,]
		verbs[verbs$ID%in%collostructions$V,]$collostruction=collostructions[na.omit(match(verbs$ID, collostructions$V)),]$frequency
	}
	verbOrder=order(CANDIDATESCORE(verbs), decreasing=TRUE)
	verb=''
	if(nrow(situation) > 1){
		verbDistractors=situation[situation$target==0,]
		verbDistractors=unique(verbDistractors[,grep('^V\\d',names(verbDistractors))])
		verbDistractors=verbDistractors[!VMATCH(verbTarget, verbDistractors)==1,]
		if(nrow(verbDistractors)!=0){
			for (i in verbOrder){
				distractorMatch=MAX(VMATCH(verbs[i,grep('^D\\d',names(verbs))], verbDistractors), value=TRUE, forceChoice=TRUE)
				if(verbs[i,]$match > (distractorMatch + distinctiveness)){
					verb=verbs[i,]
					break()
	}	}	}	}
	if(nrow(situation)==1){	
		for (i in verbOrder){
			if(verbs[i,]$match > max(verbs[i,]$match)-distinctiveness){	#if preferred word comes close enough to best expression...
					verb=verbs[i,]
					break()
	}	}	}	
	if(!is.data.frame(verb)){verb=verbs[MAX(verbs$match, forceChoice=TRUE),]}
	verb$topic=ifelse(target$topic=='verb', 1, 0)
verb
}
