SELECTUNDERGOER <-
function(speakerID, situation, verb=NULL){
	distinctiveness=world$distinctiveness
	speaker=population[[speakerID]]
	target=situation[situation$target==1,]
	distractors=situation[situation$target==0,]
	personU=target$personU
	nouns=speaker$nouns[speaker$nouns$person==personU,]
	nouns=nouns[sample(nrow(nouns)),]
	undergoerTarget=target[,grep('^U\\d',names(target))]
	undergoer=''
	nouns$match=VMATCH(undergoerTarget, nouns[,grep('^D\\d',names(nouns))])
	nouns$collostruction=0
	if(!is.null(verb)){
		if(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1){
			collostructions=speaker$collostructions$OV[speaker$collostructions$OV$V==verb$ID,]
			nouns[nouns$ID%in%collostructions$O,]$collostruction=collostructions[na.omit(match(nouns$ID, collostructions$O)),]$frequency
		} else {
			collostructions=speaker$collostructions$SV[speaker$collostructions$SV$V==verb$ID,]
			nouns[nouns$ID%in%collostructions$S,]$collostruction=collostructions[na.omit(match(nouns$ID, collostructions$S)),]$frequency
	}	}
	undergoerOrder=order(CANDIDATESCORE(nouns), decreasing=TRUE)
	if(personU==3){
		if(nrow(situation) > 1){
			undergoerDistractors=unique(distractors[distractors$personU==3,grep('^U\\d',names(distractors))])
			if(nrow(undergoerDistractors)!=0){undergoerDistractors=undergoerDistractors[!is.na(undergoerDistractors$U1),]}
			if(nrow(undergoerDistractors)!=0){undergoerDistractors=undergoerDistractors[!VMATCH(undergoerTarget, undergoerDistractors)==1,]}
			if(nrow(undergoerDistractors)!=0){
				for (i in undergoerOrder){
					distractorMatch=MAX(VMATCH(nouns[i,grep('^D\\d',names(nouns))], undergoerDistractors), value=TRUE, forceChoice=TRUE)
					if(nouns[i,]$match > (distractorMatch + distinctiveness)){
						undergoer=nouns[i,]
						break()
		}	}	}	}
		if(nrow(situation)==1){	
			undergoer=nouns[1,]
	}	}	
	if(!is.data.frame(undergoer)){undergoer=nouns[MAX(nouns$match, forceChoice=TRUE),]}	
	names(undergoer)[grep('match',names(undergoer))]='match'
	undergoer$topic=ifelse(target$topic=='undergoer', 1, 0)
undergoer
}
