TOPICFIRST <-
function(speakerID, proposition){
	topicCopy=world$topicCopy
	target=proposition$target
	if(proposition$verb$type=='onePlace'){
		if(proposition$verb$topic==1){proposition=proposition[c('verb','external', 'target')]}
		if(proposition$external$topic==1){proposition=proposition[c('external','verb', 'target')]}
	}
	if(proposition$verb$type=='twoPlace'){
		if(proposition$verb$topic==1){proposition=proposition[c('verb',names(proposition)[names(proposition)!='verb'])]}
		if(proposition$external$topic==1){proposition=proposition[c('external',names(proposition)[names(proposition)!='external'])]}
		if(proposition$internal$topic==1){proposition=proposition[c('internal',names(proposition)[names(proposition)!='internal'])]}
	}
	proposition$target=target
	if(topicCopy==TRUE & proposition$verb$topic!=1){proposition=TOPICCOPY(speakerID, proposition)}
proposition
}
