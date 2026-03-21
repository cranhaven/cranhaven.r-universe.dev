FREQUPDATE <-
function(agentID, meaning, success){
	distinctions=world$distinctions; local=world$local; commonGround=world$useCommonGround; recencyDamper=world$recencyDamper; activationNoise=world$activationNoise
	agent=population[[agentID]]
	agent$nouns$recency=agent$nouns$recency + 1
	agent$verbs$recency=agent$verbs$recency + 1
	if(is.list(meaning)){
		if(is.data.frame(meaning$verb)){
			verbID=meaning$verb$ID
			actor=ifelse(ACTOR(meaning$verb[,grep('Ext\\d',names(meaning$verb))], meaning$verb[,grep('Int\\d',names(meaning$verb))])==1, 'external', 'internal')
			if(success==1){
				agent$verbs[agent$verbs$ID==verbID,]$frequency=agent$verbs[agent$verbs$ID==verbID,]$frequency + 1
				agent$verbs[agent$verbs$ID==verbID,]$recency=0
				agent$usageHistory$verbs[nrow(agent$usageHistory$verbs) + 1,]$verb=verbID
				agent$usageHistory$verbs[nrow(agent$usageHistory$verbs),grep('D\\d',names(agent$usageHistory$verbs))]=meaning$target[, grep('V\\d',names(meaning$target))]
				if('extMarker'%in%names(meaning$verb)){
					agent$nouns[agent$nouns$ID==meaning$verb$extMarkerID,]$frequency=agent$nouns[agent$nouns$ID==meaning$verb$extMarkerID,]$frequency + 1
					agent$nouns[agent$nouns$ID==meaning$verb$extMarkerID,]$recency=0
					agent$nouns[agent$nouns$ID==meaning$verb$extMarkerID,]$verbMarker=agent$nouns[agent$nouns$ID==meaning$verb$extMarkerID,]$verbMarker + 1
					if(is.data.frame(meaning$external)){
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns) + 1,]$noun=meaning$verb$extMarkerID
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$external[, grep('D\\d',names(meaning$external))]
						pair=intersect(grep(paste('^',meaning$verb$extMarkerID, '$', sep=''), agent$collostructions$index$marker),grep(paste('^',meaning$external$ID, '$', sep=''), agent$collostructions$index$N))
						if(length(pair)!=0){agent$collostructions$index$frequency[pair]=agent$collostructions$index$frequency[pair] + 1}
						if(length(pair)==0){
							agent$collostructions$index[nrow(agent$collostructions$index) + 1,]$marker=meaning$verb$extMarkerID
							agent$collostructions$index[nrow(agent$collostructions$index),]$N=meaning$external$ID
							agent$collostructions$index[nrow(agent$collostructions$index),]$frequency=1
				}	}	}
				if('intMarker'%in%names(meaning$verb)){
					agent$nouns[agent$nouns$ID==meaning$verb$intMarkerID,]$frequency=agent$nouns[agent$nouns$ID==meaning$verb$intMarkerID,]$frequency + 1
					agent$nouns[agent$nouns$ID==meaning$verb$intMarkerID,]$recency=0
					agent$nouns[agent$nouns$ID==meaning$verb$intMarkerID,]$verbMarker=agent$nouns[agent$nouns$ID==meaning$verb$intMarkerID,]$verbMarker + 1
					if(is.data.frame(meaning$internal)){
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns) + 1,]$noun=meaning$verb$intMarkerID
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$internal[, grep('D\\d',names(meaning$internal))]
						pair=intersect(grep(paste('^',meaning$verb$intMarkerID, '$', sep=''), agent$collostructions$index$marker),grep(paste('^',meaning$internal$ID, '$', sep=''), agent$collostructions$index$N))
						if(length(pair)!=0){agent$collostructions$index$frequency[pair]=agent$collostructions$index$frequency[pair] + 1}
						if(length(pair)==0){
							agent$collostructions$index[nrow(agent$collostructions$index) + 1,]$marker=meaning$verb$intMarkerID
							agent$collostructions$index[nrow(agent$collostructions$index),]$N=meaning$internal$ID
							agent$collostructions$index[nrow(agent$collostructions$index),]$frequency=1
				}	}	}
				if(is.data.frame(meaning$external)){
					person=grep(meaning$external$person, agent$usageHistory$index$person)
					actorScore=VMATCH(meaning$verb[,grep('^Ext', names(meaning$verb))], rep(1, length(distinctions)))
					undergoerScore=VMATCH(meaning$verb[,grep('^Ext', names(meaning$verb))], rep(0, length(distinctions)))
					semRole=ifelse(undergoerScore<actorScore, 'actor', 'undergoer')
					if(meaning$verb$type=='twoPlace'){
						if(actor=='external'){semRole='actor'}
						if(actor=='internal'){semRole='undergoer'}
					}	
					semRole2=grep(semRole,agent$usageHistory$index$role)
					if('extMarker'%in%names(meaning$verb)){
						agent$usageHistory$index[intersect(person, semRole2),]$yes=agent$usageHistory$index[intersect(person, semRole2),]$yes + 1
					}
					if(!'extMarker'%in%names(meaning$verb)){
						agent$usageHistory$index[intersect(person, semRole2),]$no=agent$usageHistory$index[intersect(person, semRole2),]$no + 1
					}
					values=unlist(rep(meaning$external[, grep('^D\\d', names(meaning$external))], distinctions)); values[is.na(values)]=-1
					if('markerID'%in%names(meaning$external)){
						agent$usageHistory$flag$person[intersect(person, semRole2),]$yes=agent$usageHistory$flag$person[intersect(person, semRole2),]$yes + 1
						if(meaning$external$person==3){
							if(semRole=='actor'){agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$yes=agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$yes+1}
							if(semRole=='undergoer'){agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$yes=agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$yes+1}
					}	}	
					if(!'markerID'%in%names(meaning$external)){
						agent$usageHistory$flag$person[intersect(person, semRole2),]$no=agent$usageHistory$flag$person[intersect(person, semRole2),]$no + 1				
						if(meaning$external$person==3){
							if(semRole=='actor'){agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$no=agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$no+1}
							if(semRole=='undergoer'){agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$no=agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$no+1}
				}	}	}	
				if(meaning$verb$type=='twoPlace' & is.data.frame(meaning$internal)){
					person=grep(meaning$internal$person, agent$usageHistory$index$person)
					semRole=ifelse(actor=='external', 'undergoer', 'actor')
 					semRole2=grep(semRole,agent$usageHistory$index$role)
					if('intMarker'%in%names(meaning$verb)){
						agent$usageHistory$index[intersect(person, semRole2),]$yes=agent$usageHistory$index[intersect(person, semRole2),]$yes + 1
					}
					if(!'intMarker'%in%names(meaning$verb)){
						agent$usageHistory$index[intersect(person, semRole2),]$no=agent$usageHistory$index[intersect(person, semRole2),]$no + 1
					}
					values=unlist(rep(meaning$internal[, grep('^D\\d', names(meaning$internal))], distinctions)); values[is.na(values)]=-1
					if('markerID'%in%names(meaning$internal)){
						agent$usageHistory$flag$person[intersect(person, semRole2),]$yes=agent$usageHistory$flag$person[intersect(person, semRole2),]$yes + 1
						if(meaning$internal$person==3){
							if(semRole=='actor'){agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$yes=agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$yes+1}
							if(semRole=='undergoer'){agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$yes=agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$yes+1}
					}	}	
					if(!'markerID'%in%names(meaning$internal)){
						agent$usageHistory$flag$person[intersect(person, semRole2),]$no=agent$usageHistory$flag$person[intersect(person, semRole2),]$no + 1				
						if(meaning$internal$person==3){
							if(semRole=='actor'){agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$no=agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$no+1}
							if(semRole=='undergoer'){agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$no=agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$no+1}
			}	}	}	}
			if(is.data.frame(meaning$external)){
				subjectID=meaning$external$ID
				macroRole=ifelse(actor=='external', 'actor', 'undergoer')
				if(meaning$external$topic==1){
					if(grep('external', names(meaning))==1){agent$topicPosition[agent$topicPosition$position=='first',]$freq=agent$topicPosition[agent$topicPosition$position=='first',]$freq+1} 
					if(grep('external', names(meaning))!=1){agent$topicPosition[agent$topicPosition$position=='other',]$freq=agent$topicPosition[agent$topicPosition$position=='other',]$freq+1} 
					if(success==1){
						if(grep('external', names(meaning))==1){agent$topicPosition[agent$topicPosition$position=='first',]$success=agent$topicPosition[agent$topicPosition$position=='first',]$success+1} 
						if(grep('external', names(meaning))!=1){agent$topicPosition[agent$topicPosition$position=='other',]$success=agent$topicPosition[agent$topicPosition$position=='other',]$success+1} 
						agent$topic[agent$topic$role==macroRole,]$topic=agent$topic[agent$topic$role==macroRole,]$topic+1
				}	}
				if(success==1){
					agent$nouns[agent$nouns$ID==subjectID,]$frequency=agent$nouns[agent$nouns$ID==subjectID,]$frequency + 1
					agent$nouns[agent$nouns$ID==subjectID,]$recency=0
					agent$nouns[agent$nouns$ID==subjectID,]$argument=agent$nouns[agent$nouns$ID==subjectID,]$argument + 1
					agent$usageHistory$nouns[nrow(agent$usageHistory$nouns) + 1,]$noun=subjectID
					if(meaning$external$person!=3){	
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$verb[, grep('^Ext\\d',names(meaning$verb))]
					}
					if(meaning$external$person==3){	
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$target[, grep('A\\d',names(meaning$target))] 
						if(meaning$verb$type=='twoPlace'){
							if(macroRole=='undergoer'){
								agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$target[, grep('U\\d',names(meaning$target))] 
					}	}	}
					SV=intersect(grep(paste('^',subjectID, '$', sep=''), agent$collostructions$SV$S),grep(paste('^',verbID, '$', sep=''), agent$collostructions$SV$V))
					if(length(SV)!=0){agent$collostructions$SV$frequency[SV]=agent$collostructions$SV$frequency[SV] + 1}
					if(length(SV)==0){
						agent$collostructions$SV[nrow(agent$collostructions$SV) + 1,]$S=subjectID
						agent$collostructions$SV[nrow(agent$collostructions$SV),]$V=verbID
						agent$collostructions$SV[nrow(agent$collostructions$SV),]$frequency=1
					}
					if('marker'%in%names(meaning$external)){
						markerID=as.numeric(meaning$external$markerID)
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns) + 1,]$noun=markerID
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$verb[, grep('Ext\\d',names(meaning$verb))]
						person=meaning$external$person
						agent$nouns[agent$nouns$ID==markerID,]$frequency=agent$nouns[agent$nouns$ID==markerID,]$frequency + 1
						agent$nouns[agent$nouns$ID==markerID,]$recency=0
						agent$nouns[agent$nouns$ID==markerID,]$nounMarker=agent$nouns[agent$nouns$ID==markerID,]$nounMarker + 1
						markerN=intersect(grep(paste('^',markerID, '$', sep=''), agent$collostructions$flag$marker),grep(paste('^',subjectID, '$', sep=''), agent$collostructions$flag$N))
						if(length(markerN)!=0){agent$collostructions$flag$frequency[markerN]=agent$collostructions$flag$frequency[markerN] + 1}
						if(length(markerN)==0){	
							agent$collostructions$flag[nrow(agent$collostructions$flag) + 1,]$marker=markerID
							agent$collostructions$flag[nrow(agent$collostructions$flag),]$N=subjectID
							agent$collostructions$flag[nrow(agent$collostructions$flag),]$frequency=1
						}	
						markerV=intersect(grep(paste('^',markerID, '$', sep=''), agent$collostructions$SV$S),grep(paste('^',verbID, '$', sep=''), agent$collostructions$SV$V))
						if(length(markerV)!=0){agent$collostructions$SV$frequency[markerV]=agent$collostructions$SV$frequency[markerV] + 1}
						if(length(markerV)==0){
							agent$collostructions$SV[nrow(agent$collostructions$SV) + 1,]$S=markerID
							agent$collostructions$SV[nrow(agent$collostructions$SV),]$V=verbID
							agent$collostructions$SV[nrow(agent$collostructions$SV),]$frequency=1
					}	}
					if(meaning$external$person==3 & commonGround==TRUE){
						if(!meaning$external$ID%in%agent$commonGround){
							agent$commonGround=c(agent$commonGround, meaning$external$ID)
			}	}	}	}
			if(meaning$verb$type=='twoPlace' & is.data.frame(meaning$internal)){
				wordOrder=names(meaning); wordOrder=wordOrder[wordOrder!='target']
				undergoer=ifelse(actor=='external', 'internal', 'external')
				wordOrder=gsub(actor,'A',wordOrder); wordOrder=gsub(undergoer,'U',wordOrder); wordOrder=gsub('verb','V',wordOrder)
				wordOrder=paste(wordOrder, collapse='')
				agent$wordOrder[agent$wordOrder$order==wordOrder,]$freq=agent$wordOrder[agent$wordOrder$order==wordOrder,]$freq + 1
				if(success==1){agent$wordOrder[agent$wordOrder$order==wordOrder,]$success=agent$wordOrder[agent$wordOrder$order==wordOrder,]$success + 1}
				objectID=meaning$internal$ID
				macroRole=ifelse(undergoer=='internal', 'undergoer', 'actor')
				if(meaning$internal$topic==1){
					if(grep('internal', names(meaning))==1){agent$topicPosition[agent$topicPosition$position=='first',]$freq=agent$topicPosition[agent$topicPosition$position=='first',]$freq+1} 
					if(grep('internal', names(meaning))!=1){agent$topicPosition[agent$topicPosition$position=='other',]$freq=agent$topicPosition[agent$topicPosition$position=='other',]$freq+1} 
					if(success==1){
						if(grep('internal', names(meaning))==1){agent$topicPosition[agent$topicPosition$position=='first',]$success=agent$topicPosition[agent$topicPosition$position=='first',]$success+1} 
						if(grep('internal', names(meaning))!=1){agent$topicPosition[agent$topicPosition$position=='other',]$success=agent$topicPosition[agent$topicPosition$position=='other',]$success+1} 
						agent$topic[agent$topic$role==macroRole,]$topic=agent$topic[agent$topic$role==macroRole,]$topic+1
				}	}
				if(success==1){
					agent$nouns[agent$nouns$ID==objectID,]$frequency=agent$nouns[agent$nouns$ID==objectID,]$frequency + 1
					agent$nouns[agent$nouns$ID==objectID,]$recency=0
					agent$nouns[agent$nouns$ID==objectID,]$argument=agent$nouns[agent$nouns$ID==objectID,]$argument + 1					
					agent$usageHistory$nouns[nrow(agent$usageHistory$nouns) + 1,]$noun=objectID
					if(meaning$internal$person!=3){	
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$verb[, grep('^Int\\d',names(meaning$verb))]
					}
					if(meaning$internal$person==3){	
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$target[, grep('U\\d',names(meaning$target))] 
						if(meaning$verb$type=='twoPlace'){
							if(macroRole=='actor'){
								agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$target[, grep('A\\d',names(meaning$target))] 
					}	}	}
					OV=intersect(grep(paste('^',objectID, '$', sep=''), agent$collostructions$OV$O),grep(paste('^',verbID, '$', sep=''), agent$collostructions$OV$V))
					if(length(OV)!=0){agent$collostructions$OV$frequency[OV]=agent$collostructions$OV$frequency[OV] + 1}
					if(length(OV)==0){	
						agent$collostructions$OV[nrow(agent$collostructions$OV) + 1,]$O=objectID
						agent$collostructions$OV[nrow(agent$collostructions$OV),]$V=verbID
						agent$collostructions$OV[nrow(agent$collostructions$OV),]$frequency=1
					}
					if('marker'%in%names(meaning$internal)){
						markerID=as.numeric(meaning$internal$markerID)
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns) + 1,]$noun=markerID
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$verb[, grep('Int\\d',names(meaning$verb))]
						person=meaning$internal$person
						agent$nouns[agent$nouns$ID==markerID,]$frequency=agent$nouns[agent$nouns$ID==markerID,]$frequency + 1
						agent$nouns[agent$nouns$ID==markerID,]$recency=0
						agent$nouns[agent$nouns$ID==markerID,]$nounMarker=agent$nouns[agent$nouns$ID==markerID,]$nounMarker + 1
						markerN=intersect(grep(paste('^',markerID, '$', sep=''), agent$collostructions$flag$marker),grep(paste('^',objectID, '$', sep=''), agent$collostructions$flag$N))
						if(length(markerN)!=0){agent$collostructions$flag$frequency[markerN]=agent$collostructions$flag$frequency[markerN] + 1}
						if(length(markerN)==0){	
							agent$collostructions$flag[nrow(agent$collostructions$flag) + 1,]$marker=markerID
							agent$collostructions$flag[nrow(agent$collostructions$flag),]$N=objectID
							agent$collostructions$flag[nrow(agent$collostructions$flag),]$frequency=1
						}
						markerV=intersect(grep(paste('^',markerID, '$', sep=''), agent$collostructions$OV$O),grep(paste('^',verbID, '$', sep=''), agent$collostructions$OV$V))
						if(length(markerV)!=0){agent$collostructions$OV$frequency[markerV]=agent$collostructions$OV$frequency[markerV] + 1}
						if(length(markerV)==0){
							agent$collostructions$OV[nrow(agent$collostructions$OV) + 1,]$O=markerID
							agent$collostructions$OV[nrow(agent$collostructions$OV),]$V=verbID
							agent$collostructions$OV[nrow(agent$collostructions$OV),]$frequency=1
					}	}
					if(meaning$internal$person==3 & commonGround==TRUE){
						if(!meaning$internal$ID%in%agent$commonGround){
							agent$commonGround=c(agent$commonGround, meaning$internal$ID)
	}	}	}	}	}	}
	if(success==1){
		agent$usageHistory$verbs=unique(agent$usageHistory$verbs)
		agent$usageHistory$nouns=unique(agent$usageHistory$nouns)
	}
	agent$nouns$activation=RESCALE(jitter(log((agent$nouns$frequency+1))/(agent$nouns$recency+1+recencyDamper), factor=activationNoise))
	agent$verbs$activation=RESCALE(jitter(log((agent$verbs$frequency+1))/(agent$verbs$recency+1+recencyDamper), factor=activationNoise))
	population[[agentID]]=agent
population<<-population
}
