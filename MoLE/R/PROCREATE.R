PROCREATE <-
function(speakerID, hearerID){
	procreationAge=world$procreationAge; deathAge=world$deathAge; desemanticization=world$desemanticization; crossover=world$crossover; replace=world$replace; distinctions=world$distinctions; wordLength=world$wordLength; vowels=world$vowels; consonants=world$consonants
	if(population[[speakerID]]$fertile=='yes' & population[[hearerID]]$fertile=='yes'){
		if((population[[speakerID]]$age + population[[hearerID]]$age)/2 > (procreationAge*deathAge)){
			#procreate
			offspring1=population[[speakerID]]
			if(crossover==FALSE){offspring2=population[[hearerID]]}
			if(crossover==TRUE){
				mix=sample(intersect(offspring1$nouns$ID, population[[hearerID]]$nouns$ID), .5*nrow(offspring1$nouns))
				offspring1$nouns[match(mix, offspring1$nouns$ID),]=population[[hearerID]]$nouns[match(mix,population[[hearerID]]$nouns$ID),]
				mix=sample(intersect(offspring1$verbs$ID, population[[hearerID]]$verbs$ID), .5*nrow(offspring1$verbs))
				offspring1$verbs[match(mix,offspring1$verbs$ID), ]=population[[hearerID]]$verbs[match(mix,population[[hearerID]]$verbs$ID), ]
				offspring2=offspring1
			}
			offspring1$age=0; offspring2$age=0
			offspring1$semupdate=0; offspring2$semupdate=0
			offspring1$generation=offspring1$generation + 1; offspring2$generation=offspring2$generation + 1
			if(replace==TRUE){
				zeros=offspring1$nouns[offspring1$nouns$frequency==0,]$ID
				if(length(zeros)!=0){
					cols=sample(1:length(distinctions), length(zeros), replace=TRUE)
					for (i in 1:length(zeros)){
					 	if(offspring1$nouns[offspring1$nouns$ID==zeros[i], ]$semanticWeight < 1){
							offspring1$nouns[offspring1$nouns$ID==zeros[i],grep('NA',offspring1$nouns[offspring1$nouns$ID==zeros[i],grep('^D\\d',names(offspring1$nouns))])[1]]=sample(seq(0,1,length.out=distinctions[grep('NA',offspring1$nouns[offspring1$nouns$ID==zeros[i],grep('^D\\d',names(offspring1$nouns))])[1]]),1)	#restores first meaning dimension with NA
							offspring1$nouns[offspring1$nouns$ID==zeros[i],]$semanticWeight=(length(grep('^D\\d',names(offspring1$nouns)))-sum(is.na(offspring1$nouns[offspring1$nouns$ID==zeros[i],grep('^D\\d',names(offspring1$nouns))])))/length(grep('^D\\d',names(offspring1$nouns)))	
                    				} else {offspring1$nouns[offspring1$nouns$ID==zeros[i],cols[i]]=sample(seq(0,1,length.out=distinctions[cols[i]]),1)}
                    				if(offspring1$nouns[offspring1$nouns$ID==zeros[i], ]$productionEffort < min(wordLength)){
                    					plus=gsub('.*(.)$', '\\1', offspring1$nouns[offspring1$nouns$ID==zeros[i], ]$form)
                    					plus=ifelse(plus%in%vowels, sample(consonants, 1), sample(vowels, 1))
                    					offspring1$nouns[offspring1$nouns$ID==zeros[i], ]$form=paste(offspring1$nouns[offspring1$nouns$ID==zeros[i], ]$form, plus, sep='')
                    					offspring1$nouns[offspring1$nouns$ID==zeros[i], ]$productionEffort=nchar(offspring1$nouns[offspring1$nouns$ID==zeros[i], ]$form)
                    			}	}
					offspring2$nouns[match(zeros, offspring1$nouns$ID),]=offspring1$nouns[match(zeros, offspring1$nouns$ID),]
			}	}
			offspring1$nouns$frequency=0; offspring2$nouns$frequency=0
			offspring1$nouns$recency=0; offspring2$nouns$recency=0
			offspring1$nouns$activation=0; offspring2$nouns$activation=0
			offspring1$nouns$nounMarker=0; offspring2$nouns$nounMarker=0
			offspring1$nouns$verbMarker=0; offspring2$nouns$verbMarker=0
			offspring1$nouns$argument=0; offspring2$nouns$argument=0
			offspring1$verbs$frequency=0; offspring2$verbs$frequency=0
			offspring1$verbs$recency=0; offspring2$verbs$recency=0
			offspring1$verbs$activation=0; offspring2$verbs$activation=0
			offspring1$usageHistory$nouns=offspring1$usageHistory$nouns[-(1:nrow(offspring1$usageHistory$nouns)),]; offspring2$usageHistory$nouns=offspring2$usageHistory$nouns[-(1:nrow(offspring2$usageHistory$nouns)),]
			offspring1$usageHistory$verbs=offspring1$usageHistory$verbs[-(1:nrow(offspring1$usageHistory$verbs)),]; offspring2$usageHistory$verbs=offspring2$usageHistory$verbs[-(1:nrow(offspring2$usageHistory$verbs)),]
			values=vector(); for(i in 1:length(distinctions)){values=c(values, seq(0,1,length.out=distinctions[i]))}
			offspring1$usageHistory$flag=list(
				person=data.frame(role=rep(c('actor', 'undergoer'), each=3), person=rep(1:3, 2), yes=0, no=4, stringsAsFactors=FALSE), 
				actor=data.frame(dimension=rep(1:length(distinctions), distinctions), value=values, yes=0, no=4, stringsAsFactors=FALSE),
				undergoer=data.frame(dimension=rep(1:length(distinctions), distinctions), value=values, yes=0, no=4, stringsAsFactors=FALSE)
			)	
			offspring2$usageHistory$flag=offspring1$usageHistory$flag
			offspring1$usageHistory$index=data.frame(role=rep(c('actor', 'undergoer'), each=3), person=rep(1:3, 2), yes=0, no=4, stringsAsFactors=FALSE); offspring2$usageHistory$index=offspring1$usageHistory$index	
			offspring1$collostructions$SV=offspring1$collostructions$SV[-(1:nrow(offspring1$collostructions$SV)),]; offspring2$collostructions$SV=offspring2$collostructions$SV[-(1:nrow(offspring2$collostructions$SV)),]
			offspring1$collostructions$OV=offspring1$collostructions$OV[-(1:nrow(offspring1$collostructions$OV)),]; offspring2$collostructions$OV=offspring2$collostructions$OV[-(1:nrow(offspring2$collostructions$OV)),]
			offspring1$collostructions$flag=offspring1$collostructions$flag[-(1:nrow(offspring1$collostructions$flag)),]; offspring2$collostructions$flag=offspring2$collostructions$flag[-(1:nrow(offspring2$collostructions$flag)),]
			offspring1$collostructions$index=offspring1$collostructions$index[-(1:nrow(offspring1$collostructions$index)),]; offspring2$collostructions$index=offspring2$collostructions$index[-(1:nrow(offspring2$collostructions$index)),]
			offspring1$topic=data.frame(role=c('actor','undergoer'),topic=0, stringsAsFactors=FALSE); offspring2$topic=offspring1$topic
			offspring1$wordOrder=data.frame(order=c('AVU','AUV', 'VAU', 'VUA', 'UAV', 'UVA'),freq=0, success=0, stringsAsFactors=FALSE); offspring2$wordOrder=offspring1$wordOrder
			offspring1$topicPosition=data.frame(position=c('first', 'other'), freq=0, success=0, stringsAsFactors=FALSE); offspring2$topicPosition=offspring1$topicPosition
			population[[length(population) + 1]] = offspring1; names(population)[length(population)] = toupper(FORMS(1, length=6))	
			population[[length(population) + 1]] = offspring2; names(population)[length(population)] = toupper(FORMS(1, length=6))	
			population[[speakerID]]$fertile = 'no'; population[[hearerID]]$fertile = 'no'
			cat(paste('\n', paste(names(population)[c(length(population)-1, length(population))], collapse=' and '),'were born', '\n\n'))
 	}	}
population<<-population 
}
