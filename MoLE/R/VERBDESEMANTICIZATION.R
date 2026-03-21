VERBDESEMANTICIZATION <-
function(agent){	#Cf. Heine en Kuteva p.39: freq is epiphenomenon of extension, not cause; extension by combinatorial flexibility
	distinctions=world$distinctions; minimalSpecification=world$minimalSpecification; desemanticizationCeiling=world$desemanticizationCeiling; power=world$desemanticizationPower
	dims=length(distinctions)	
	steps=dims-1-minimalSpecification	
	factor=(desemanticizationCeiling*agent$age-8)/steps^power
	steps=round(factor*(0:steps)^power + 8)
	verbs=xtabs(~agent$usageHistory$verbs$verb)
	verbs=names(verbs[verbs>steps[1]])
	for(verb in verbs){
		verbTargets=agent$usageHistory$verbs[agent$usageHistory$verbs$verb==verb,grep('^D\\d',names(agent$usageHistory$verbs))]
		#change meaning values
		for(i in 1:ncol(verbTargets)){
			values=table(verbTargets[,i])
			change=MAX(values, forceChoice=T)
			if(sum(values[-change])<(sum(values)/log(sum(values)))){
				agent$verbs[agent$verbs$ID==verb, i]=as.numeric(names(values)[change])
				graveyard$history[nrow(graveyard$history) + 1,]=c(agent$generation, 'verb meaning changed', 'VERBDESEMANTICIZATION', verb, '', '', '')
		}	}
		#remove meaning values
		if(agent$verbs[agent$verbs$ID==verb,]$semanticWeight>(minimalSpecification/dims)){
			if(nrow(verbTargets)>steps[sum(is.na(agent$verbs[agent$verbs$ID==verb,grep('^D\\d',names(agent$verbs))])) + 1]){		
				#action
				actionProfile=agent$verbs[agent$verbs$ID==verb,grep('^D\\d',names(agent$verbs))]
				vars=rep(0, ncol(verbTargets))
				for(i in 1:length(vars)){vars[i]=sum(actionProfile[,i]!=verbTargets[,i], na.rm=T)}
				agent$verbs[agent$verbs$ID==verb,  MAX(vars, forceChoice=T)]=NA
				agent$verbs[agent$verbs$ID==verb, ]$semanticWeight=(length(grep('^D\\d',names(agent$verbs)))-sum(is.na(agent$verbs[agent$verbs$ID==verb, grep('^D\\d',names(agent$verbs))])))/length(grep('^D\\d',names(agent$verbs)))
				#external
				extProfile=agent$verbs[agent$verbs$ID==verb, grep('^Ext\\d',names(agent$verbs))]
				vars=rep(0, ncol(extProfile))
				performerProfiles=agent$nouns[match(agent$collostructions$SV[grep(paste('^',verb, '$',sep=''),agent$collostructions$SV$V),]$S, agent$nouns$ID),grep('^D\\d',names(agent$nouns))]
				for(i in 1:length(vars)){vars[i]=sum(extProfile[,i]!=performerProfiles[,i], na.rm=T)}
				agent$verbs[agent$verbs$ID==verb,  grep('^Ext\\d',names(agent$verbs))[MAX(vars, forceChoice=T)]]=NA
				#internal
				intProfile=agent$verbs[agent$verbs$ID==verb, grep('^Int\\d',names(agent$verbs))]
				vars=rep(0, ncol(intProfile))
				performerProfiles=agent$nouns[match(agent$collostructions$OV[grep(paste('^',verb, '$',sep=''),agent$collostructions$OV$V),]$O, agent$nouns$ID),grep('^D\\d',names(agent$nouns))]
				for(i in 1:length(vars)){vars[i]=sum(intProfile[,i]!=performerProfiles[,i], na.rm=T)}
				agent$verbs[agent$verbs$ID==verb, grep('^Int\\d',names(agent$verbs))[MAX(vars, forceChoice=T)]]=NA
				graveyard$history[nrow(graveyard$history) + 1,]=c(agent$generation, 'verb meaning dimension removed', 'VERBDESEMANTICIZATION', verb, '', '', '')
	}	}	}	
graveyard <<- graveyard
agent
}
