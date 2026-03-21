SUCCESS <-
function(proposition, interpretation, situation){
	distinctiveness=world$distinctiveness
	success=0
	if(is.list(interpretation)){
		if(nrow(situation) > 1){
			success=ifelse(interpretation$target$target==1, 1, 0)
		} else {
			if(!'verb'%in%names(interpretation) | !'verb'%in%names(proposition)){
				intended=proposition$external[grep('D\\d',names(proposition$external))]
				understood=interpretation$external[grep('D\\d',names(interpretation$external))]
			}
			if('verb'%in%names(interpretation) & 'verb' %in%names(proposition)){
				if(proposition$verb$type=='onePlace'){intended=cbind(proposition$verb, proposition$external); intended=intended[grep('D\\d',names(intended))]}
				if(proposition$verb$type=='twoPlace'){intended=cbind(proposition$verb, proposition$external, proposition$internal); intended=intended[grep('D\\d',names(intended))]}
				if(interpretation$verb$type=='onePlace'){
					understood=interpretation$verb[grep('D\\d',names(interpretation$verb))]
					if('external'%in%names(interpretation)){understood=cbind(interpretation$verb, interpretation$external); understood=understood[grep('D\\d',names(understood))]}
				}
				if(interpretation$verb$type=='twoPlace'){
					understood=interpretation$verb[grep('D\\d',names(interpretation$verb))]
					if('external'%in%names(interpretation) & 'internal'%in%names(interpretation)){understood=cbind(understood, interpretation$external, interpretation$internal); understood=understood[grep('D\\d',names(understood))]}
					if('external'%in%names(interpretation) & !'internal'%in%names(interpretation)){understood=cbind(understood, interpretation$external); understood=understood[grep('D\\d',names(understood))]}
					if('internal'%in%names(interpretation) & !'external'%in%names(interpretation)){
						external=interpretation$internal
						external[grep('D\\d',names(external))]=NA
						understood=cbind(understood, external, interpretation$internal); understood=understood[grep('D\\d',names(understood))]
			}	}	}
			success=ifelse(VMATCH(intended, understood) > (1-distinctiveness), 1, 0)
	}	}
success
}
