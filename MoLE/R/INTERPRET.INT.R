INTERPRET.INT <-
function(hearerID, analysis, situation){	
	interpretation=list()
	hearer=population[[hearerID]]
	verbs=hearer$verbs; nouns=hearer$nouns
	#result
	verb=''
	if('verb'%in%analysis$role){
		verb=verbs[verbs$ID==analysis[analysis$role=='verb',]$verbID,]
		verb$form=analysis[analysis$role=='verb',]$stem
		if('verbSuffix'%in%analysis$role){
			for(i in grep('verbSuffix', analysis$role)){
				if('internal'%in%analysis[analysis[i,]$verbMarkerTarget,]$role){	
					analysis[analysis$role=='internal',]$nounPerson=analysis[i,]$verbMarkerPerson
					verb$intMarkerID=analysis[i,]$verbMarkerID
					verb$intMarker=analysis[i,]$stem
					verb$intMarkerTarget=analysis[analysis[i,]$verbMarkerTarget,]$nounID
					verb$intMarkerFrequency=nouns[nouns$ID==verb$intMarkerID,]$verbMarker
					verb$intMarkerSemanticWeight=nouns[nouns$ID==verb$intMarkerID,]$semanticWeight
				}
				if('external'%in%analysis[analysis[i,]$verbMarkerTarget,]$role){	
					analysis[analysis$role=='external',]$nounPerson=analysis[i,]$verbMarkerPerson
					verb$extMarkerID=analysis[i,]$verbMarkerID
					verb$extMarker=analysis[i,]$stem
					verb$extMarkerTarget=analysis[analysis[i,]$verbMarkerTarget,]$nounID
					verb$extMarkerFrequency=nouns[nouns$ID==verb$extMarkerID,]$verbMarker
					verb$extMarkerSemanticWeight=nouns[nouns$ID==verb$extMarkerID,]$semanticWeight
		}	}	}
		if('verbAdposition'%in%analysis$role){	#NB verbAdpositions overrule verbSuffixes... 
			for(i in grep('verbAdposition', analysis$role)){
				if('external'%in%analysis[analysis[i,]$verbMarkerTarget,]$role){	
					analysis[analysis$role=='external',]$nounPerson=analysis[i,]$nounPerson
					verb$extMarkerID=analysis[i,]$nounID
					verb$extMarker=analysis[i,]$stem
					verb$extMarkerTarget=analysis[analysis[i,]$verbMarkerTarget,]$nounID
					verb$extMarkerFrequency=nouns[nouns$ID==verb$extMarkerID,]$verbMarker
					verb$extMarkerSemanticWeight=nouns[nouns$ID==verb$extMarkerID,]$semanticWeight
				}
				if('internal'%in%analysis[analysis[i,]$verbMarkerTarget,]$role){	
					analysis[analysis$role=='internal',]$nounPerson=analysis[i,]$nounPerson
					verb$intMarkerID=analysis[i,]$nounID
					verb$intMarker=analysis[i,]$stem
					verb$intMarkerTarget=analysis[analysis[i,]$verbMarkerTarget,]$nounID
					verb$intMarkerFrequency=nouns[nouns$ID==verb$intMarkerID,]$verbMarker
					verb$intMarkerSemanticWeight=nouns[nouns$ID==verb$intMarkerID,]$semanticWeight
	}	}	}	}
	internal=''
	if('internal'%in%analysis$role){
		if(!length(grep('internal',analysis$role)) > 1){
			internal=nouns[nouns$ID==analysis[analysis$role=='internal',]$nounID,]
			internal$form=analysis[analysis$role=='internal',]$stem
			internal$person=analysis[analysis$role=='internal',]$nounPerson
			internal$topic=0
			if(analysis$topic[1]!=1 & nouns[nouns$ID==internal$ID,]$recency==min(nouns$recency)){internal$topic=1}
			if(analysis$topic[1]==1 & grep('internal', analysis$role)==1){internal$topic=1}
			if(grep('internal', analysis$role) < nrow(analysis)){
				if(analysis$role[grep('internal',analysis$role) + 1]=='nounSuffix'){
					internal$markerID=analysis[grep('internal',analysis$role) + 1,]$nounMarkerID
					internal$marker=analysis[grep('internal',analysis$role) + 1,]$stem
					internal$markerFrequency=nouns[nouns$ID==internal$markerID,]$nounMarker
					internal$markerSemanticWeight=nouns[nouns$ID==internal$markerID,]$semanticWeight
					if((grep('internal',analysis$role) + 1) < nrow(analysis)){
						if(analysis$role[grep('internal',analysis$role) + 2]=='nounAdposition'){
							internal$markerID=analysis[grep('internal',analysis$role) + 2,]$nounID
							internal$marker=analysis[grep('internal',analysis$role) + 2,]$stem
							internal$markerFrequency=nouns[nouns$ID==internal$markerID,]$nounMarker
							internal$markerSemanticWeight=nouns[nouns$ID==internal$markerID,]$semanticWeight
				}	}	}
				if(analysis$role[grep('internal',analysis$role) + 1]=='nounAdposition'){
					internal$markerID=analysis[grep('internal',analysis$role) + 1,]$nounID
					internal$marker=analysis[grep('internal',analysis$role) + 1,]$stem
					internal$markerFrequency=nouns[nouns$ID==internal$markerID,]$nounMarker
					internal$markerSemanticWeight=nouns[nouns$ID==internal$markerID,]$semanticWeight
	}	}	}	}				
	external=''
	if('external'%in%analysis$role){
		if(!length(grep('external',analysis$role)) > 1){
			external=nouns[nouns$ID==analysis[analysis$role=='external',]$nounID,]
			external$form=analysis[analysis$role=='external',]$stem
			external$person=analysis[analysis$role=='external',]$nounPerson
			external$topic=0
			if(analysis$topic[1]!=1 & nouns[nouns$ID==external$ID,]$recency==min(nouns$recency)){external$topic=1}
			if(analysis$topic[1]==1 & grep('external', analysis$role)==1){external$topic=1}
			if(grep('external', analysis$role) < nrow(analysis)){
				if(analysis$role[grep('external',analysis$role) + 1]=='nounSuffix'){
					external$markerID=analysis[grep('external',analysis$role) + 1,]$nounMarkerID
					external$marker=analysis[grep('external',analysis$role) + 1,]$stem
					external$markerFrequency=nouns[nouns$ID==external$markerID,]$nounMarker
					external$markerSemanticWeight=nouns[nouns$ID==external$markerID,]$semanticWeight
					if((grep('external',analysis$role) + 1) < nrow(analysis)){
						if(analysis$role[grep('external',analysis$role) + 2]=='nounAdposition'){
							external$markerID=analysis[grep('external',analysis$role) + 2,]$nounID
							external$marker=analysis[grep('external',analysis$role) + 2,]$nounMarker
							external$marker=analysis[grep('external',analysis$role) + 2,]$stem
							external$markerFrequency=nouns[nouns$ID==external$markerID,]$nounMarker
							external$markerSemanticWeight=nouns[nouns$ID==external$markerID,]$semanticWeight
				}	}	}
				if(analysis$role[grep('external',analysis$role) + 1]=='nounAdposition'){
					external$markerID=analysis[grep('external',analysis$role) + 1,]$nounID
					external$marker=analysis[grep('external',analysis$role) + 1,]$nounMarker
					external$marker=analysis[grep('external',analysis$role) + 1,]$stem
					external$markerFrequency=nouns[nouns$ID==external$markerID,]$nounMarker
					external$markerSemanticWeight=nouns[nouns$ID==external$markerID,]$semanticWeight
	}	}	}	}		
	if(nrow(situation) > 1){
		if(is.data.frame(verb)){
			situation$verbMatch=VMATCH(verb[,grep('^D\\d',names(verb))], situation[,grep('^V\\d',names(situation))])
		} else {situation$verbMatch=-1}
		if(is.data.frame(external)){
			if(external$person==3){
				candidates=situation$personA
				if(sum(candidates==3, na.rm=TRUE)>1){situation$externalMatch=VMATCH(external[,grep('^D\\d',names(external))], situation[,grep('^A\\d',names(situation))])}
				if(sum(candidates==3, na.rm=TRUE)==1){situation$externalMatch=ifelse(external$person==situation$personA, 1, -1)}
				if(sum(candidates==3, na.rm=TRUE)==0){situation$externalMatch=-1}
			}
			if(external$person!=3){situation$externalMatch=ifelse(external$person==situation$personA, 1, -1)}
		} else {situation$externalMatch=-1}
		if(is.data.frame(internal)){	
			actor=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'external', 'internal')
			if(actor=='external'){
				if(internal$person==3){
					candidates=situation$personU
					if(sum(candidates==3, na.rm=TRUE)>1){situation$internalMatch=VMATCH(internal[,grep('^D\\d',names(internal))], situation[,grep('^U\\d',names(situation))])}
					if(sum(candidates==3, na.rm=TRUE)==1){situation$internalMatch=ifelse(internal$person==situation$personU, 1, -1)}
					if(sum(candidates==3, na.rm=TRUE)==0){situation$internalMatch=-1}
				}
				if(internal$person!=3){situation$internalMatch=ifelse(internal$person==situation$personU, 1, -1)}
			}
			if(actor=='internal'){
				if(internal$person==3){
					candidates=situation$personA
					if(sum(candidates==3, na.rm=TRUE)>1){situation$internalMatch=VMATCH(internal[,grep('^D\\d',names(internal))], situation[,grep('^A\\d',names(situation))])}
					if(sum(candidates==3, na.rm=TRUE)==1){situation$internalMatch=ifelse(internal$person==situation$personA, 1, -1)}
					if(sum(candidates==3, na.rm=TRUE)==0){situation$internalMatch=-1}
				}
				if(internal$person!=3){situation$internalMatch=ifelse(internal$person==situation$personA, 1, -1)}
				if(is.data.frame(external)){
					if(external$person==3){
						candidates=situation$personU
						if(sum(candidates==3, na.rm=TRUE)>1){situation$externalMatch=VMATCH(external[,grep('^D\\d',names(external))], situation[,grep('^U\\d',names(situation))])}
						if(sum(candidates==3, na.rm=TRUE)==1){situation$externalMatch=ifelse(external$person==situation$personU, 1, -1)}
						if(sum(candidates==3, na.rm=TRUE)==0){situation$externalMatch=-1}
					}
					if(external$person!=3){situation$externalMatch=ifelse(external$person==situation$personU, 1, -1)}
				} else {situation$externalMatch=-1}
			}
		} else {situation$internalMatch=-1; if(NA%in%situation$U1){situation[is.na(situation$U1),]$internalMatch=0}}	
		situation$penalty=length(grep('\\?', analysis$role))*-1 + length(grep('\\?', analysis[grep('verbSuffix', analysis$role),]$verbMarkerTarget))*-1
		situation$totalMatch=situation$verbMatch + situation$externalMatch + situation$internalMatch + situation$penalty
		target=situation[MAX(situation$totalMatch),]
		if(nrow(target) > 1){
			if(1%in%target$target){target=target[target$target==1,]} else {target=target[sample(nrow(target),1),]}
		}
	} else {target=situation}
	interpretationOrder=analysis$role
	interpretationOrder=interpretationOrder[interpretationOrder%in%c('external','internal','verb')]
	interpretationOrder=c(interpretationOrder, 'target')
	interpretation=list(verb=verb, external=external, internal=internal, target=target)
	interpretation=interpretation[interpretationOrder]
interpretation	
}
