SELECTACTOR <-
function(speakerID, situation, verb=NULL){
	distinctiveness=world$distinctiveness
	speaker=population[[speakerID]]
	target=situation[situation$target==1,]
	distractors=situation[situation$target==0,]
	personA=target$personA
	nouns=speaker$nouns[speaker$nouns$person==personA,]
	nouns=nouns[sample(nrow(nouns)),]
	actorTarget=target[,grep('^A\\d',names(target))]
	actor=''
	nouns$match=VMATCH(actorTarget, nouns[,grep('^D\\d',names(nouns))])
	nouns$collostruction=0
	if(!is.null(verb)){
		if(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1){
			collostructions=speaker$collostructions$SV[speaker$collostructions$SV$V==verb$ID,]
			nouns[nouns$ID%in%collostructions$S,]$collostruction=collostructions[na.omit(match(nouns$ID, collostructions$S)),]$frequency
		} else {
			collostructions=speaker$collostructions$OV[speaker$collostructions$OV$V==verb$ID,]
			nouns[nouns$ID%in%collostructions$O,]$collostruction=collostructions[na.omit(match(nouns$ID, collostructions$O)),]$frequency
	}	}
	actorOrder=order(CANDIDATESCORE(nouns), decreasing=TRUE)
	if(personA==3){		
		if(nrow(situation) > 1){
			actorDistractors=unique(distractors[distractors$personA==3,grep('^A\\d',names(distractors))])
			if(nrow(actorDistractors)!=0){actorDistractors=actorDistractors[!VMATCH(actorTarget, actorDistractors)==1,]}
			if(nrow(actorDistractors)!=0){
				for (i in actorOrder){
					distractorMatch=MAX(VMATCH(nouns[i,grep('^D\\d',names(nouns))], actorDistractors), value=TRUE, forceChoice=TRUE)
					if(nouns[i,]$match > (distractorMatch + distinctiveness)){
						actor=nouns[i,]
						break()
		}	}	}	}
		if(nrow(situation)==1){	
			actor=nouns[1,]
	}	}	
	if(!is.data.frame(actor)){actor=nouns[MAX(nouns$match, forceChoice=TRUE),]}
	names(actor)[grep('match',names(actor))]='match'
	actor$topic=ifelse(target$topic=='actor', 1, 0)
actor
}
