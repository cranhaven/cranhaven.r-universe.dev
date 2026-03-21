PERSONUPDATE <-
function(agent){
	personThreshold=world$semUpdateThreshold*agent$age; referenceThreshold=world$referenceThreshold; verbalRoleMarker=world$verbalRoleMarker
	nouns=agent$nouns
	index=agent$collostructions$index
	if(nrow(index)!=0){
		index=index[order(index$N),]
		index$person=nouns[match(index$marker, nouns$ID),]$person
		index=index[!is.na(index$person),]
		index2=unique(index[,c('N','person')]); index2$frequency=0	#in case of multiple verb markers with same person...
		for(person in unique(index2$person)){index2[index2$person==person,]$frequency=tapply(index[index$person==person, ]$frequency, index[index$person==person, ]$N, sum)}
		index2=index2[index2$frequency>personThreshold,]
		if(nrow(index2)!=0){
			for(i in 1:nrow(index2)){
				if(index2$person[i]!=nouns[nouns$ID==index2$N[i],]$person){
					nouns[nouns$ID==index2$N[i],]$person=index2$person[i]
					graveyard$history[nrow(graveyard$history) + 1,]=c(agent$generation, 'new pro', 'PERSONUPDATE', index2$N[i], index2$person[i], '', '')
		}	}	}
		#remove redundant local pronouns
		localPros=nouns[nouns$person!=3 & nouns$argument>nouns$verbMarker & nouns$productionEffort>referenceThreshold,]
		if(nrow(localPros)>1){
			localPros=localPros[sample(nrow(localPros)),]
			for(i in 1:(nrow(localPros)-1)){
				if(1%in%VMATCH(localPros[i,c(grep('D\\d', names(localPros)), grep('person', names(localPros)))], localPros[(i + 1):nrow(localPros),c(grep('D\\d', names(localPros)), grep('person', names(localPros)))])){	
					nouns=nouns[nouns$ID!=localPros$ID[i],]
					graveyard$history[nrow(graveyard$history) + 1,]=c(agent$generation, 'pro removed', 'PERSONUPDATE', localPros$ID[i], localPros$person[i], '', '')
		}	}	}
		#remove redundant indexes
		if(nrow(nouns[nouns$productionEffort<=referenceThreshold,])>1){
			indexes=nouns[nouns$productionEffort<=referenceThreshold,]
			indexes=indexes[indexes$verbMarker > indexes$nounMarker & indexes$verbMarker > indexes$argument,]	#clear indexes
			if(nrow(indexes)>1){
				indexes=indexes[order(indexes$verbMarker),]	#start removal with least popular index
				if(verbalRoleMarker==F){
					for(i in 1:(nrow(indexes)-1)){
						if(sum(grepl(indexes[i,]$person, indexes[(i+1):nrow(indexes),]$person))>0){
							nouns=nouns[nouns$ID!=indexes$ID[i],]
							graveyard$history[nrow(graveyard$history) + 1,]=c(agent$generation, 'index removed', 'PERSONUPDATE', indexes$ID[i], indexes$person[i], '', '')
				}	}	}
				if(verbalRoleMarker==TRUE){
					for(i in 1:(nrow(indexes)-1)){
						if(1%in%VMATCH(indexes[i,c(grep('D\\d', names(indexes)), grep('person', names(indexes)))], indexes[(i + 1):nrow(indexes),c(grep('D\\d', names(indexes)), grep('person', names(indexes)))])){	
							nouns=nouns[nouns$ID!=indexes$ID[i],]
							graveyard$history[nrow(graveyard$history) + 1,]=c(agent$generation, 'index removed', 'PERSONUPDATE', indexes$ID[i], indexes$person[i], '', '')
		}	}	}	}	}
		agent$nouns=nouns
	}
graveyard <<- graveyard
agent
}
