TALK <-
function(nTurns){
	talkAge=world$talkAge; desemanticization=world$desemanticization; semUpdateAge=world$semUpdateAge; procreationAge=world$procreationAge; deathAge=world$deathAge; turnChange=world$turnChange
	speakerID=sample(length(population), 1)
	if(length(population)==2){hearerID=setdiff(1:length(population),speakerID)}
	if(length(population) > 2){hearerID=sample(setdiff(1:length(population),speakerID),1)}
	#make it less likely (not impossible) for young agents to talk with each other
	if(population[[speakerID]]$age < (talkAge*deathAge) & population[[hearerID]]$age < (talkAge*deathAge) & length(population)>2){
		speakerID=sample(setdiff(1:length(population),speakerID), 1)
		hearerID=sample(setdiff(1:length(population),speakerID),1)
	}	
	#reset common ground
	population[[speakerID]]$commonGround=vector()
	population[[hearerID]]$commonGround=vector()
	population<<-population
	for (i in 1:nTurns){
		if(population[[speakerID]]$age < (talkAge*deathAge) & population[[hearerID]]$age > (talkAge*deathAge)){
			changeID=speakerID
			speakerID=hearerID
			hearerID=changeID
		}
		utterance=TURN(speakerID, hearerID)
		cat(paste(names(population)[[speakerID]], ': ', utterance, '\n', sep='')); flush.console()
		changeTurn=sample(1:0, 1, prob=turnChange)
		if(changeTurn==1){
			changeID=speakerID
			speakerID=hearerID
			hearerID=changeID
	}	}
	#semupdate
	if(!is.na(semUpdateAge)){SEMUPDATE(speakerID); SEMUPDATE(hearerID)}
	PROCREATE(speakerID, hearerID)
	if(speakerID > hearerID){DIE(speakerID); DIE(hearerID)}	#without condition, ref to second participant may be wrong after death of first
	if(hearerID > speakerID){DIE(hearerID); DIE(speakerID)}
}
