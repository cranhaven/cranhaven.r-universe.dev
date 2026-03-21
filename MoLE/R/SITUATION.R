SITUATION <-
function(speakerID){
	useCommonGround=world$useCommonGround; commonGroundStart=world$commonGroundStart; dahlS=world$dahlS; dahlA=world$dahlA; dahlO=world$dahlO; oddsNewA=world$oddsNewA; oddsNewOther=world$oddsNewOther; distinctions=world$distinctions; proportionIntrans=world$proportionIntrans; nEvents=world$nEvents; roleNoise=world$roleNoise; referenceNoise=world$referenceNoise; local=world$local; personTopicality=world$personTopicality; roleTopicality=world$roleTopicality; topicContinuity=world$topicContinuity; oddsSg=world$oddsSg
	if(local==TRUE){	#local person always known: adjust oddsNew for Dahl numbers
		oddsNewA=oddsNewA/(sum(world$dahlA[3:4])/sum(world$dahlA))	
		oddsNewOther=oddsNewOther/(sum(world$dahlS[3:4]+world$dahlO[3:4])/sum(world$dahlS+world$dahlO))
	}
	speaker=population[[speakerID]]
	nEvents=sample(nEvents, 1, prob=nEvents)	#situations with multiple events are more likely than situations with single event
	#develop commonGround
	if(useCommonGround==TRUE){
		commonGround=speaker$commonGround	
		if(length(commonGround) < commonGroundStart){
			commonGround=c(commonGround, sample(speaker$nouns[!speaker$nouns$ID%in%commonGround,]$ID, commonGroundStart-length(commonGround))) 
	}	} 
	#preparation
	power=9*(1-roleNoise)	#more noise->lower power, reduces impact of VMATCH in type-match selection below
	intransVerbs=speaker$verbs[speaker$verbs$type=='onePlace',]	
	transVerbs=speaker$verbs[speaker$verbs$type=='twoPlace',]	
	nouns=speaker$nouns	
	#intrans events (external)
	intransExternals=vector(); intransActions=vector(); intransExternalsPerson=vector()	#will be overwritten if applicable, otherwise necessary for later
	nIntrans=round(proportionIntrans*nEvents,0)
	if(nIntrans==0){nIntrans=sample(0:1,1, prob=c(1-proportionIntrans, proportionIntrans))}	#otherwise, with low number of events and low prop of intransitives, never intransitive events
	if(nIntrans!=0){
		if(local==FALSE){	#count locals as animates
			intransExternals=sample(c('3A', '3I'), nIntrans, replace=TRUE, prob=c(sum(dahlS[1:3]),dahlS[4]))
			intransExternalsPerson=intransExternals
		}	
		if(local==TRUE){
			intransExternals=sample(c(1, 2, '3A', '3I'), nIntrans, replace=TRUE, prob=dahlS)
			intransExternalsPerson=intransExternals
			intransExternals[intransExternals%in%1:2]=nouns[MAX(VMATCH(rep(1, length(distinctions)), nouns[, grep('D\\d', names(nouns))]), 1, forceChoice=TRUE),]$ID
		}
		if(useCommonGround==TRUE){
			intransExternals[intransExternals%in%c('3A','3I')]=paste(intransExternals[intransExternals%in%c('3A','3I')],sample(c('old','new'), length(intransExternals[intransExternals%in%c('3A','3I')]), replace=TRUE, prob=c(1, oddsNewOther)), sep='')
			intransExternals[intransExternals=='3Aold']=sample(commonGround, sum(intransExternals=='3Aold'), replace=TRUE, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$ID%in%commonGround,]))	
			intransExternals[intransExternals=='3Iold']=sample(commonGround, sum(intransExternals=='3Iold'), replace=TRUE, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$ID%in%commonGround,]))	
			intransExternals[intransExternals=='3Anew']=sample(setdiff(nouns[nouns$person==3,]$ID, commonGround), sum(intransExternals=='3Anew'), replace=TRUE, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$person==3 & !nouns$ID%in%commonGround,]))	
			intransExternals[intransExternals=='3Inew']=sample(setdiff(nouns[nouns$person==3,]$ID, commonGround), sum(intransExternals=='3Inew'), replace=TRUE, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$person==3 & !nouns$ID%in%commonGround,]))	
		}
		if(useCommonGround==FALSE){
			intransExternals[intransExternals=='3A']=sample(nouns[nouns$person==3,]$ID, sum(intransExternals=='3A'), replace=TRUE, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$person==3,]))	
			intransExternals[intransExternals=='3I']=sample(nouns[nouns$person==3,]$ID, sum(intransExternals=='3I'), replace=TRUE, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$person==3,]))	
		}
		intransActions=intransVerbs[1:length(intransExternals),]
		for (i in 1:nrow(intransActions)){intransActions[i,]=intransVerbs[sample(nrow(intransVerbs), 1, prob=VMATCH(nouns[nouns$ID==intransExternals[i],],intransVerbs[,grep('^Ext\\d',names(intransVerbs))])^power),]}
	}
	#trans events
	transExternals=vector(); transActions=vector(); transExternalsPerson=vector(); internals=vector(); transActions=vector(); internalsPerson=vector()
	nTrans=(nEvents-nIntrans)
	if(nTrans!=0){
		if(local==FALSE){
			transExternals=sample(c('3A', '3I'), nTrans, replace=TRUE, prob=c(sum(dahlA[1:3]),dahlA[4]))
			internals=sample(c('3A', '3I'), nTrans, replace=TRUE, prob=c(sum(dahlO[1:3]),dahlO[4]))
			transExternalsPerson=transExternals
			internalsPerson=internals
		}
		if(local==TRUE){
			transExternals=sample(c(1, 2, '3A', '3I'), nTrans, replace=TRUE, prob=dahlA)
			transExternalsPerson=transExternals
			internals=sample(c(1, 2, '3A', '3I'), nTrans, replace=TRUE, prob=dahlO)
			internalsPerson=internals
			transExternals[transExternals%in%1:2]=nouns[MAX(VMATCH(rep(1, length(distinctions)), nouns[, grep('D\\d', names(nouns))]), 1, forceChoice=TRUE),]$ID
			internals[internals%in%1:2]=nouns[MAX(VMATCH(rep(1, length(distinctions)), nouns[, grep('D\\d', names(nouns))]), 1, forceChoice=TRUE),]$ID
		}
		if(useCommonGround==TRUE){
			transExternals[transExternals%in%c('3A','3I')]=paste(transExternals[transExternals%in%c('3A','3I')],sample(c('old','new'), length(transExternals[transExternals%in%c('3A','3I')]), replace=TRUE, prob=c(1, oddsNewA)), sep='')
			internals[internals%in%c('3A','3I')]=paste(internals[internals%in%c('3A','3I')],sample(c('old','new'), length(internals[internals%in%c('3A','3I')]), replace=TRUE, prob=c(1, oddsNewOther)), sep='')
			transExternals[transExternals=='3Aold']=sample(commonGround, 			sum(transExternals=='3Aold'), replace=TRUE, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$ID%in%commonGround,]))	
			transExternals[transExternals=='3Iold']=sample(commonGround, 			sum(transExternals=='3Iold'), replace=TRUE, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$ID%in%commonGround,]))	
			transExternals[transExternals=='3Anew']=sample(setdiff(nouns[nouns$person==3,]$ID, commonGround), 	sum(transExternals=='3Anew'), replace=TRUE, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$person==3 & !nouns$ID%in%commonGround,]))	
			transExternals[transExternals=='3Inew']=sample(setdiff(nouns[nouns$person==3,]$ID, commonGround), 	sum(transExternals=='3Inew'), replace=TRUE, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$person==3 & !nouns$ID%in%commonGround,]))	
			internals[internals=='3Aold']=sample(commonGround, 			sum(internals=='3Aold'), replace=TRUE, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$ID%in%commonGround,]))	
			internals[internals=='3Iold']=sample(commonGround, 			sum(internals=='3Iold'), replace=TRUE, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$ID%in%commonGround,]))	
			internals[internals=='3Anew']=sample(setdiff(nouns[nouns$person==3,]$ID, commonGround), 	sum(internals=='3Anew'), replace=TRUE, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$person==3 & !nouns$ID%in%commonGround,]))	
			internals[internals=='3Inew']=sample(setdiff(nouns[nouns$person==3,]$ID, commonGround), 	sum(internals=='3Inew'), replace=TRUE, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$person==3 & !nouns$ID%in%commonGround,]))	
		}
		if(useCommonGround==FALSE){
			transExternals[transExternals=='3A']=sample(nouns[nouns$person==3,]$ID, sum(transExternals=='3A'), replace=TRUE, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$person==3,]))	
			transExternals[transExternals=='3I']=sample(nouns[nouns$person==3,]$ID, sum(transExternals=='3I'), replace=TRUE, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$person==3,]))	
			internals[internals=='3A']=sample(nouns[nouns$person==3,]$ID, 	sum(internals=='3A'), replace=TRUE, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$person== 3,]))	
			internals[internals=='3I']=sample(nouns[nouns$person==3,]$ID, 	sum(internals=='3I'), replace=TRUE, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$person==3,]))	
		}
		transActions=transVerbs[1:length(transExternals),]
		for (i in 1:nrow(transActions)){transActions[i,]=transVerbs[sample(nrow(transVerbs), 1, prob=((VMATCH(nouns[nouns$ID==transExternals[i],],transVerbs[,grep('^Ext\\d',names(transVerbs))])+VMATCH(nouns[nouns$ID==internals[i],],transVerbs[,grep('^Int\\d',names(transVerbs))]))/2)^power),]}
	}	
	actors=nouns[match(c(intransExternals, transExternals),nouns$ID),c(grep('D\\d',names(nouns)),grep('person',names(nouns)))]
	names(actors)=gsub('D','A', names(actors))
	names(actors)=gsub('person','personA', names(actors))
	undergoers=nouns[match(c(intransExternals, internals),nouns$ID), c(grep('D\\d',names(nouns)),grep('person',names(nouns)))]	#include externals for equal-sized data frame
	names(undergoers)=gsub('D','U',names(undergoers))
	names(undergoers)=gsub('person','personU',names(undergoers))
	undergoers$personU=c(rep(NA, nIntrans), internalsPerson)
	actors$personA=c(intransExternalsPerson, transExternalsPerson)
	actions=rbind(intransActions, transActions)
	#make sure actor of action is indeed in actor position (verb selection is independent of this, only about match with V columns)
	for(i in 1:nrow(actions)){
		if(ACTOR(actions[i, grep('Ext\\d',names(actions))], actions[i, grep('Int\\d',names(actions))])==2){ff=actors[i,]; actors[i,]=undergoers[i,]; undergoers[i,]=ff}
	}
	intrans=grep('onePlace',actions$type)
	actions=actions[,grep('D\\d',names(actions))]
	names(actions)=gsub('D','V',names(actions))
	situation=cbind(actions, actors, undergoers)
	targetCols=sample((1:ncol(situation))[-grep('person',names(situation))], round(referenceNoise*length(grep('[^person]',names(situation)))), prob=rep(1:length(distinctions), 3))	#@prob: higher meaning cols more likely to vary
	nRows=round(referenceNoise*nrow(situation))
	if(nRows==0){nRows=sample(0:1,1, prob=c(1-referenceNoise, referenceNoise))}	#if number of rows is small, still allow for noise
	targetRows=sample(nrow(situation), nRows)
	if(length(targetCols) > 0 & length(targetRows) > 0){
		for(i in targetCols){
			situation[targetRows,i]=sample(seq(0, 1, length.out=distinctions[as.numeric(sub('.*?(\\d+)','\\1',names(situation[i])))]), replace=TRUE, length(targetRows))	
	}	}
	#make sure referents are fully specified
	for (j in grep('^V',names(situation))){	#fill V columns randomly
		if(NA%in%situation[,j]){
			situation[is.na(situation[,j]),j]=sample(seq(0, 1, length.out=distinctions[as.numeric(sub('.*?(\\d+)','\\1',names(situation[j])))]), sum(is.na(situation[,j])), replace=TRUE)
	}	}
	for (j in grep('^A',names(situation))){	#A columns with more prominent values	
		if(NA%in%situation[,j]){
			situation[is.na(situation[,j]),j]=sample(seq(0, 1, length.out=distinctions[as.numeric(sub('.*?(\\d+)','\\1',names(situation[j])))]), sum(is.na(situation[,j])), replace=TRUE, prob=seq(1,3,length.out=distinctions[as.numeric(sub('.*?(\\d+)','\\1',names(situation[j])))]))
	}	}
	for (j in grep('^U',names(situation))){	#and U columns with lower values	
		if(NA%in%situation[,j]){
			situation[is.na(situation[,j]),j]=sample(seq(1, 0, length.out=distinctions[as.numeric(sub('.*?(\\d+)','\\1',names(situation[j])))]), sum(is.na(situation[,j])), replace=TRUE, prob=seq(1,3,length.out=distinctions[as.numeric(sub('.*?(\\d+)','\\1',names(situation[j])))]))
	}	}
	continueTopic=sample(1:0, 1, prob=topicContinuity)
	situation$target=0
	situation$topic=''
	if(continueTopic==1){
		topicCandidates=nouns[nouns$recency==min(nouns$recency),]
		if(nrow(topicCandidates)>1){topicCandidates=topicCandidates[max(topicCandidates$argument, topicCandidates$verbMarker) >= topicCandidates$nounMarker,]}
		if(nrow(topicCandidates)==0){topicCandidates=nouns[nouns$recency==min(nouns$recency),]} #if none of the candidates qualifies argument criterium, only use recency
		topic=ifelse(nrow(topicCandidates)>1, sample(topicCandidates$ID,1), topicCandidates$ID)	
		if(nouns[nouns$ID==topic,]$person==3){	
			matchA=VMATCH(nouns[nouns$ID==topic,], situation[,grep('^A\\d', names(situation))]); matchA[grep(3, situation$personA, invert=TRUE)]=-1
			target=MAX(matchA, forceChoice=TRUE)
			situation[target, grep('^A\\d', names(situation))]=nouns[nouns$ID==topic,grep('^D\\d', names(nouns))]	#replace referential properties of topic
			situation$target[target]=1
			situation$topic[target]='actor'
			#DuBois: pref for actor topic..., otherwise:
			#matchU=VMATCH(nouns[nouns$ID==topic,], situation[,grep('^U\\d', names(situation))]); matchU[grep(3, situation$personU, invert=TRUE)]=-1
			#if(max(matchU)>max(matchA)){target=MAX(matchU, forceChoice=TRUE); situation[target, grep('^U\\d', names(situation))]=nouns[nouns$ID==topic,grep('^D\\d', names(nouns))]; situation$target[target]=1; situation$topic[target]='undergoer'}
			#if(max(matchA)>=max(matchU)){target=MAX(matchA, forceChoice=TRUE); situation[target, grep('^A\\d', names(situation))]=nouns[nouns$ID==topic,grep('^D\\d', names(nouns))]; situation$target[target]=1; situation$topic[target]='actor'}		
		}
		if(nouns[nouns$ID==topic,]$person!=3){
			personTopic=nouns[nouns$ID==topic,]$person
			targetCandidates=unique(grep(personTopic, situation$personA), grep(personTopic, situation$personU))
			if(length(targetCandidates)==0){targetCandidates=1:nrow(situation)}
			target=ifelse(length(targetCandidates)==1, targetCandidates, sample(targetCandidates, 1))
			situation$target[target]=1
			situation$topic[target]=ifelse(situation$personA[target]==personTopic, 'actor', 'undergoer')
	}	}
	if(continueTopic==0){
		personTopic=sample(c(1,2,'3A','3I'),1, prob=personTopicality)
		targetCandidates=unique(grep(personTopic, situation$personA), grep(personTopic, situation$personU))
		if(length(targetCandidates)==0){targetCandidates=1:nrow(situation)}
		target=ifelse(length(targetCandidates)==1, targetCandidates, sample(targetCandidates, 1))
		situation$target[target]=1
		situation$topic[target]=ifelse(situation$personA[target]==personTopic, 'actor', 'undergoer')
	}
	if(is.na(situation[target,]$personU) & situation[target,]$topic=='undergoer'){situation[target,]$topic='actor'}
	situation$personA=gsub('A|I','',situation$personA)
	situation$personU=gsub('A|I','',situation$personU)
	if(length(intrans)!=0){
		situation[intrans, grep('^U\\d',names(situation))]=NA
	}
situation<<-situation
unique(situation)
}
