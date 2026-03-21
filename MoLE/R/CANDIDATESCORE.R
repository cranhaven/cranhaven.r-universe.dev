CANDIDATESCORE <-
function(lexicon, type='referringExpression'){
	candidateScoring=world$candidateScoring; frequency=world$frequency; functionBlocking=world$functionBlocking; activationImpact=world$activationImpact; semanticWeightImpact=world$semanticWeightImpact; economyImpact=world$economyImpact; collostructionImpact=world$collostructionImpact
	if(candidateScoring=='match'){candidateScore=lexicon$match}
	if(candidateScoring=='semanticWeight'){candidateScore=1/lexicon$semanticWeight}	
	if(candidateScoring=='economy'){candidateScore=1/lexicon$productionEffort}
	if(candidateScoring=='recency'){candidateScore=1/(lexicon$recency + 1)}	
	if(candidateScoring=='collostruction'){candidateScore=order(lexicon$collostruction, decreasing=TRUE)}
	if(candidateScoring=='frequency'){
		if(frequency=='absolute' | !'argument'%in%names(lexicon)){candidateScore=lexicon$frequency}	
		if(frequency=='relative' & 'argument'%in%names(lexicon)){	
			if(type=='referringExpression'){
				if(functionBlocking==FALSE){candidateScore=lexicon$argument}
				if(functionBlocking==TRUE){candidateScore=lexicon$argument-lexicon$nounMarker-lexicon$verbMarker}
			}
			if(type=='nounMarker'){
				if(functionBlocking==FALSE){candidateScore=lexicon$nounMarker}
				if(functionBlocking==TRUE){candidateScore=lexicon$nounMarker-lexicon$argument-lexicon$verbMarker}
			}
			if(type=='verbMarker'){
				if(functionBlocking==FALSE){candidateScore=lexicon$verbMarker}
				if(functionBlocking==TRUE){candidateScore=lexicon$verbMarker-lexicon$argument-lexicon$nounMarker}
	}	}	}
	if(candidateScoring=='activation'){candidateScore=lexicon$activation}
	if(candidateScoring=='all'){
		candidateScore=lexicon$match + 
			activationImpact*lexicon$activation +  	
			collostructionImpact*RESCALE(lexicon$collostruction) +
			semanticWeightImpact*min(lexicon$semanticWeight)/lexicon$semanticWeight + 	
			economyImpact*min(lexicon$productionEffort)/lexicon$productionEffort	
	}		
candidateScore
}
