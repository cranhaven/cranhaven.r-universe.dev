FIRSTINFIRSTOUT <-
function(speakerID, proposition){
	candidateScoring=world$candidateScoring; frequency=world$frequency; functionBlocking=world$functionBlocking; activationImpact=world$activationImpact; semanticWeightImpact=world$semanticWeightImpact; economyImpact=world$economyImpact; collostructionImpact=world$collostructionImpact; minWeight=world$minimalSpecification; minProd=world$suffixThreshold
	target=proposition$target; proposition=proposition[-grep('target', names(proposition))]
	activations=rep(-1, length(proposition))	
	for (i in 1:length(activations)){	
		if(candidateScoring=='match'){activations[i]=proposition[[i]]$match}
		if(candidateScoring=='activation'){activations[i]=proposition[[i]]$activation}
		if(candidateScoring=='semanticWeight'){activations[i]=1/proposition[[i]]$semanticWeight}	
		if(candidateScoring=='economy'){activations[i]=1/proposition[[i]]$productionEffort}
		if(candidateScoring=='recency'){activations[i]=1/(proposition[[i]]$recency + 1)}	
		if(candidateScoring=='collostruction'){activations[i]=proposition[[i]]$collostruction}
		if(candidateScoring=='frequency'){
			if(frequency=='absolute' | !'argument'%in%names(proposition[[i]])){activations[i]=proposition[[i]]$frequency}	
			if(frequency=='relative' & 'argument'%in%names(proposition[[i]])){	
				if(functionBlocking==FALSE){activations[i]=proposition[[i]]$argument}
				if(functionBlocking==TRUE){activations[i]=proposition[[i]]$argument-proposition[[i]]$nounMarker-proposition[[i]]$verbMarker}
		}	}	
		if(candidateScoring=='all'){
			activations[i]=proposition[[i]]$match + 
				activationImpact*proposition[[i]]$activation +  	
				collostructionImpact*(proposition[[i]]$collostruction/(proposition[[i]]$frequency+1)) +
				semanticWeightImpact*(minWeight/proposition[[i]]$semanticWeight) + 	
				economyImpact*(minProd/proposition[[i]]$productionEffort)	
	}	}		
	proposition=proposition[order(activations, decreasing=TRUE)]
	proposition$target=target
proposition
}
