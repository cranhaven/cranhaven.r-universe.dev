DIE <-
function(agentID){
	deathAge=world$deathAge; saveAll=world$saveAll
	agent=population[[agentID]]
	if(agent$age > deathAge){
		graveyard$summary[nrow(graveyard$summary) + 1,]$name = names(population)[[agentID]]
		graveyard$summary[nrow(graveyard$summary),]$generation = agent$generation
		graveyard$summary[nrow(graveyard$summary),]$successRate = sum(agent$verbs$frequency)/agent$age
		graveyard$summary[nrow(graveyard$summary),]$meanNounMarkerFrequency = sum(agent$collostructions$flag$frequency)/sum(agent$verbs$frequency)
		graveyard$summary[nrow(graveyard$summary),]$topnounMarkerFrequency = ifelse(nrow(agent$collostructions$flag)!=0, MAX(tapply(agent$collostructions$flag$frequency, agent$collostructions$flag$marker, sum),1, forceChoice=TRUE, value=TRUE)/sum(agent$verbs$frequency), 0)
		graveyard$summary[nrow(graveyard$summary),]$meanverbMarkerFrequency = sum(agent$collostructions$index$frequency)/sum(agent$verbs$frequency)
		graveyard$summary[nrow(graveyard$summary),]$topverbMarkerFrequency = ifelse(nrow(agent$collostructions$index)!=0, MAX(tapply(agent$collostructions$index$frequency, agent$collostructions$index$marker, sum),1, forceChoice=TRUE, value=TRUE)/sum(agent$verbs$frequency), 0)
		graveyard$brains[length(graveyard$brains) + 1] = population[agentID]
		if(saveAll==FALSE){
			graveyard$brains[[length(graveyard$brains)]]$usageHistory = graveyard$brains[[length(graveyard$brains)]]$usageHistory[-c(1,2)]	
			graveyard$brains[[length(graveyard$brains)]]$collostructions$SV = graveyard$brains[[length(graveyard$brains)]]$collostructions$SV[graveyard$brains[[length(graveyard$brains)]]$collostructions$SV$frequency>10,] 
			graveyard$brains[[length(graveyard$brains)]]$collostructions$OV = graveyard$brains[[length(graveyard$brains)]]$collostructions$OV[graveyard$brains[[length(graveyard$brains)]]$collostructions$OV$frequency>10,] 
			graveyard$brains[[length(graveyard$brains)]]$collostructions$index = graveyard$brains[[length(graveyard$brains)]]$collostructions$index[graveyard$brains[[length(graveyard$brains)]]$collostructions$index$frequency>10,] 
			graveyard$brains[[length(graveyard$brains)]]$collostructions$flag = graveyard$brains[[length(graveyard$brains)]]$collostructions$flag[graveyard$brains[[length(graveyard$brains)]]$collostructions$flag$frequency>10,] 
			save=table(graveyard$brains[[length(graveyard$brains)]]$usageHistory$flag$actor$dimension); save=names(save[save==2])
			graveyard$brains[[length(graveyard$brains)]]$usageHistory$flag$actor = graveyard$brains[[length(graveyard$brains)]]$usageHistory$flag$actor[graveyard$brains[[length(graveyard$brains)]]$usageHistory$flag$actor$dimension%in%save,]
			save=table(graveyard$brains[[length(graveyard$brains)]]$usageHistory$flag$undergoer$dimension); save=names(save[save==2])
			graveyard$brains[[length(graveyard$brains)]]$usageHistory$flag$undergoer = graveyard$brains[[length(graveyard$brains)]]$usageHistory$flag$undergoer[graveyard$brains[[length(graveyard$brains)]]$usageHistory$flag$undergoer$dimension%in%save,]
		}
		population = population[-agentID]
		cat(paste('\n', graveyard$summary[nrow(graveyard$summary),]$name,'died', '\n\n'))
		population <<- population
		graveyard <<- graveyard
}	}
