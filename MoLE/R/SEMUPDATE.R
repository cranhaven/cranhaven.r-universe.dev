SEMUPDATE <-
function(agentID){
	semUpdateAge=world$semUpdateAge; deathAge=world$deathAge; nNouns=world$nNouns; nVerbs=world$nVerbs; local=world$local
	agent=population[[agentID]]
	if(agent$semupdate==0){
		if(agent$age>semUpdateAge*deathAge){
			agent=VERBDESEMANTICIZATION(agent)
			agent=NOUNDESEMANTICIZATION(agent)
			agent=FUSE(agent)
			agent=PERSONUPDATE(agent)
			agent$semupdate=1
	}	}
	agent$nouns=agent$nouns[agent$nouns$semanticWeight>0,]
	if(nrow(agent$nouns)<nNouns){
		newNouns=NOUNS(nNouns-nrow(agent$nouns), local=FALSE)
		newNouns$ID=max(agent$nouns$ID)+(1:nrow(newNouns))
		agent$nouns=rbind(agent$nouns, newNouns)
	}
	if(local==TRUE){
		if(!1%in%agent$nouns$person){
			agent$nouns[MAX(VMATCH(rep(1, length(grep('D\\d', names(agent$nouns)))), agent$nouns[, grep('D\\d', names(agent$nouns))]), 1, forceChoice=TRUE),]$person=1
		} 
		if(!2%in%agent$nouns$person){
			agent$nouns[MAX(VMATCH(rep(1, length(grep('D\\d', names(agent$nouns)))), agent$nouns[, grep('D\\d', names(agent$nouns))]), 2, forceChoice=TRUE),]$person=2
	}	}
	agent$verbs=agent$verbs[agent$verbs$semanticWeight>0,]
	if(nrow(agent$verbs)<nVerbs){
		newNouns=VERBS(nNouns-nrow(agent$verbs))
		newNouns$ID=max(agent$verbs$ID)+(1:nrow(newNouns))
		agent$verbs=rbind(agent$verbs, newNouns)
	}
	population[[agentID]]=agent
population<<-population	
}
