PROPOSITION <-
function(speakerID, situation){
	distinctiveness=world$distinctiveness
	speaker=population[[speakerID]]
	target=situation[situation$target==1,]
	topic=target$topic
	targetTransitivity=ifelse(is.na(target$personU), 'onePlace', 'twoPlace')
	if(targetTransitivity=='onePlace'){
		if(topic=='actor'){external=SELECTACTOR(speakerID, situation); verb=SELECTVERB(speakerID, situation, actor=external)}	
		if(topic=='verb'){verb=SELECTVERB(speakerID, situation); external=SELECTACTOR(speakerID, situation, verb=verb)}
		externalCollostructionFrequency=speaker$collostructions$SV[intersect(grep(paste('^', external$ID, '$', sep=''), speaker$collostructions$SV$S),grep(paste('^', verb$ID, '$', sep=''), speaker$collostructions$SV$V)),]$frequency
		if(length(externalCollostructionFrequency)!=0){external$collostruction=externalCollostructionFrequency} else {external$collostruction=0}
		external$typing=VMATCH(external[,grep('^D\\d',names(external))], verb[,grep('^Ext\\d',names(verb))])
		proposition=list(verb=verb, external=external)
	}
	if(targetTransitivity=='twoPlace'){
		if(topic=='actor'){actor=SELECTACTOR(speakerID, situation); verb=SELECTVERB(speakerID, situation, actor=actor); undergoer=SELECTUNDERGOER(speakerID, situation, verb=verb)}	
		if(topic=='undergoer'){undergoer=SELECTUNDERGOER(speakerID, situation); verb=SELECTVERB(speakerID, situation, undergoer=undergoer); actor=SELECTACTOR(speakerID, situation, verb=verb)}	
		if(topic=='verb'){verb=SELECTVERB(speakerID, situation); actor=SELECTACTOR(speakerID, situation, verb=verb); undergoer=SELECTUNDERGOER(speakerID, situation, verb=verb)}	
		if(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1){
			external=actor; internal=undergoer} else {
			external=undergoer; internal=actor
		}
		internalCollostructionFrequency=speaker$collostructions$OV[intersect(grep(paste('^', internal$ID, '$', sep=''), speaker$collostructions$OV$O),grep(paste('^', verb$ID, '$', sep=''), speaker$collostructions$OV$V)),]$frequency
		if(length(internalCollostructionFrequency)!=0){internal$collostruction=internalCollostructionFrequency} else {internal$collostruction=0}
		externalCollostructionFrequency=speaker$collostructions$SV[intersect(grep(paste('^', external$ID, '$', sep=''), speaker$collostructions$SV$S),grep(paste('^', verb$ID, '$', sep=''), speaker$collostructions$SV$V)),]$frequency
		if(length(externalCollostructionFrequency)!=0){external$collostruction=externalCollostructionFrequency} else {external$collostruction=0}
		external$typing=VMATCH(external[,grep('^D\\d',names(external))], verb[,grep('^Ext\\d',names(verb))])
		internal$typing=VMATCH(internal[,grep('^D\\d',names(internal))], verb[,grep('^Int\\d',names(verb))])
		proposition=list(verb=verb, external=external, internal=internal)
	}
	proposition=proposition[sample(length(proposition))]
	proposition$target=target
proposition<<-proposition
proposition
}
