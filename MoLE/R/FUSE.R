FUSE <-
function(agent){
	threshold=world$semUpdateThreshold; wordLength=world$wordLength
	threshold=threshold*agent$age
	nouns=agent$nouns
	refs=nouns[nouns$frequency==0 | (nouns$verbMarker+nouns$nounMarker) < (nouns$frequency/log(nouns$frequency)),]	
	flag=agent$collostructions$flag
	flag=flag[flag$frequency>threshold,]
	if(nrow(flag)!=0){
		for(i in 1:nrow(flag)){
			if(nouns[nouns$ID==flag$marker[i],]$nounMarker > max(nouns[nouns$ID==flag$marker[i],]$argument, nouns[nouns$ID==flag$marker[i],]$verbMarker)){	
				new=nouns[nouns$ID==flag$N[i],]		
				new$ID=max(agent$nouns$ID) + 1
				vowels=unlist(strsplit(c(new$form, nouns[nouns$ID==flag$marker[i],]$form), '')); vowels=unique(vowels[vowels%in%world$vowels])
				consonants=unlist(strsplit(c(new$form, nouns[nouns$ID==flag$marker[i],]$form), '')); consonants=unique(consonants[consonants%in%world$consonants])
				length=nchar(new$form) + nchar(nouns[nouns$ID==flag$marker[i],]$form)
				if(length>max(wordLength)){length=max(wordLength)}
				new$form=FORMS(1, length, vowels=vowels, consonants=consonants)
				if(new$form%in%nouns$form){	
					new$form=FORMS(1, length, vowels=vowels, consonants=consonants)
					solution=sample(c('vowels','consonants','length'), 1)
					if(solution=='vowels'){vowels=c(vowels, sample(world$vowels, 1))}
					if(solution=='consonants'){consonants=c(consonants, sample(world$consonants, 1))}
					if(solution=='length' & length<max(wordLength)){length=length+1}
				}
				change=grep('NA', nouns[nouns$ID==flag$marker[i],grep('D\\d',names(nouns))], invert=TRUE)
				new[,change]=nouns[nouns$ID==flag$marker[i],change]	
				new$frequency=flag$frequency[i]
				new$argument=flag$frequency[i]
				new$verbMarker=0	
				new$nounMarker=0	
				new$productionEffort=nchar(new$form)
				new$semanticWeight=(length(grep('D\\d', names(agent$nouns)))-length(grep('NA', new)))/length(grep('D\\d', names(agent$nouns)))
				new$recency=0
				if(!1%in%VMATCH(new[,c(grep('D\\d', names(new)), grep('person', names(new)))], refs[,c(grep('D\\d', names(refs)), grep('person', names(refs)))])){	
					agent$nouns=rbind(agent$nouns,new)
					rownames(agent$nouns)=1:nrow(agent$nouns)
					graveyard$history[nrow(graveyard$history) + 1,]=c(agent$generation, 'new word', 'FUSE', new$ID, new$person, flag$N[i], flag$marker[i])
	}	}	}	}
graveyard <<- graveyard
agent
}
