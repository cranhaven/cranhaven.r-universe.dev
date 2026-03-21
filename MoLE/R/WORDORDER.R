WORDORDER <-
function(hearerID, analysis){
	hearer=population[[hearerID]]
	positions=grep('\\?|^verb$', analysis$role)
	verb=hearer$verbs[hearer$verbs$ID==analysis[analysis$role=='verb',]$verbID,]
	actor=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'external', 'internal')
	undergoer=ifelse(actor=='external', 'internal', 'external')
	verbPosition=grep(grep('^verb$', analysis$role), positions)
	if(sum(hearer$wordOrder$success)>8){	#n exceptions should minimally be 4(=8/ln(8)) for Yang
		yangTopic=TRUE %in% hearer$topicPosition$success[hearer$topicPosition$position=='other'] < (sum(hearer$topicPosition$success)/log(sum(hearer$topicPosition$success)))
		yangWordOrder=TRUE %in% 
			#any particular order
			c((sum(hearer$wordOrder$success)-hearer$wordOrder$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success)))) |
			#A first
			c(sum(hearer$wordOrder[grep('^A', hearer$wordOrder$order, invert=TRUE), ]$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success)))) |
			#V final
			c(sum(hearer$wordOrder[grep('V$', hearer$wordOrder$order, invert=TRUE), ]$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success)))) |
			#UV
			c(sum(hearer$wordOrder[grep('UV', hearer$wordOrder$order, invert=TRUE), ]$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success)))) |
			#VU
			c(sum(hearer$wordOrder[grep('VU', hearer$wordOrder$order, invert=TRUE), ]$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success)))) 
		if(yangTopic==TRUE){analysis$topic[1]=1}
		if(yangWordOrder==TRUE){
			if(analysis[analysis$role=='verb',]$verbType=='twoPlace'){
				##no particular order
				if(!TRUE %in% c((sum(hearer$wordOrder$success)-hearer$wordOrder$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success))))){
					#A first
					if(TRUE %in% c(sum(hearer$wordOrder[grep('^A', hearer$wordOrder$order, invert=TRUE), ]$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success))))){
						if(analysis[1,]$role=='?' & !actor%in%analysis$role){analysis[1,]$role=actor}
					}
					#UV
					if(TRUE %in% c(sum(hearer$wordOrder[grep('UV', hearer$wordOrder$order, invert=TRUE), ]$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success))))){
						if(verbPosition!=1){if(analysis[positions[verbPosition-1],]$role=='?' & !undergoer%in%analysis$role){analysis[positions[verbPosition-1],]$role=undergoer}}
					}	
					#VU
					if(TRUE %in% c(sum(hearer$wordOrder[grep('VU', hearer$wordOrder$order, invert=TRUE), ]$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success))))){
						if((verbPosition+1)<=length(positions)){if(analysis[positions[verbPosition+1],]$role=='?' & !undergoer%in%analysis$role){analysis[positions[verbPosition+1],]$role=undergoer}}
				}	}
				##particular orders
				if(TRUE %in% c((sum(hearer$wordOrder$success)-hearer$wordOrder$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success))))){
					order=hearer$wordOrder$order[c((sum(hearer$wordOrder$success)-hearer$wordOrder$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success))))]
					order=unlist(strsplit(order, ''))
					actorPosition=grep('A', order)-grep('V', order)
					if((verbPosition+actorPosition)>0 & (verbPosition+actorPosition)<=length(positions)){
						if(!actor%in%analysis$role & analysis[positions[verbPosition+actorPosition],]$role=='?'){
							analysis[positions[verbPosition+actorPosition],]$role=actor
					}	}
					undergoerPosition=grep('U', order)-grep('V', order)
					if( (verbPosition+undergoerPosition)>0 & (verbPosition+undergoerPosition)<=length(positions) ){
						if(!undergoer%in%analysis$role & analysis[positions[verbPosition+undergoerPosition],]$role=='?'){
							analysis[positions[verbPosition+undergoerPosition],]$role=undergoer
	}	}	}	}	}	}
analysis			
}
