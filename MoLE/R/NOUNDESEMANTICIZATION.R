NOUNDESEMANTICIZATION <-
function(agent){	#Cf. Heine en Kuteva p.39: freq is epiphenomenon of extension, not cause; extension by combinatorial flexibility
	distinctions=world$distinctions; minimalSpecification=world$minimalSpecification; desemanticizationCeiling=world$desemanticizationCeiling; power=world$desemanticizationPower
	dims=length(distinctions)	
	steps=dims-1-minimalSpecification	
	factor=(desemanticizationCeiling*agent$age-8)/steps^power	#8 is minimum freq from which Yang applies
	steps=round(factor*(0:steps)^power + 8)
	nouns=table(agent$usageHistory$nouns$noun)
	nouns=names(nouns[nouns>steps[1]])	#i.e. >8
	for(noun in nouns){
		if(!noun%in%agent$nouns$ID){break}
		nounTargets=agent$usageHistory$nouns[agent$usageHistory$nouns$noun==noun,grep('^D\\d',names(agent$usageHistory$nouns))]
		#change meaning values
		for(i in 1:ncol(nounTargets)){
			values=table(nounTargets[,i])
			change=MAX(values, forceChoice=TRUE)
			if(sum(values[-change])<(sum(values)/log(sum(values)))){
				agent$nouns[agent$nouns$ID==noun, i]=as.numeric(names(values)[change])
				graveyard$history[nrow(graveyard$history) + 1,]=c(agent$generation, '(pro)noun meaning changed', 'NOUNDESEMANTICIZATION', noun, '', '', '')
		}	}
		#remove meaning values
		if(agent$nouns[agent$nouns$ID==noun,]$semanticWeight>(minimalSpecification/dims)){
			if(nrow(nounTargets)>steps[sum(is.na(agent$nouns[agent$nouns$ID==noun,grep('^D\\d',names(agent$nouns))])) + 1]){		
				nounProfile=agent$nouns[agent$nouns$ID==noun,grep('^D\\d',names(agent$nouns))]
				vars=rep(0, ncol(nounProfile))
				for(i in 1:length(vars)){vars[i]=sum(nounProfile[,i]!=nounTargets[,i], na.rm=TRUE)}
				agent$nouns[agent$nouns$ID==noun, MAX(vars, forceChoice=TRUE)]=NA
				agent$nouns[agent$nouns$ID==noun,]$semanticWeight=(length(grep('^D\\d',names(agent$nouns)))-sum(is.na(agent$nouns[agent$nouns$ID==noun,grep('^D\\d',names(agent$nouns))])))/length(grep('^D\\d',names(agent$nouns)))
				graveyard$history[nrow(graveyard$history) + 1,]=c(agent$generation, '(pro)noun meaning dimension removed', 'NOUNDESEMANTICIZATION', noun, '', '', '')
	}	}	}
graveyard <<- graveyard
agent
}
