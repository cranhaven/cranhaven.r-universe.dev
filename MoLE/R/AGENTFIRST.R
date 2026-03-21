AGENTFIRST <-
function(proposition){
	if(proposition$verb$type=='onePlace'){	
		proposition=proposition[c('external',names(proposition)[names(proposition)!='external'])]
	}
	if(proposition$verb$type=='twoPlace'){
		actor=ifelse(ACTOR(proposition$verb[,grep('^Ext\\d', names(proposition$verb))], proposition$verb[,grep('^Int\\d', names(proposition$verb))])==1, 'external', 'internal')
		proposition=proposition[c(actor,names(proposition)[names(proposition)!=actor])]
	}
proposition
}
