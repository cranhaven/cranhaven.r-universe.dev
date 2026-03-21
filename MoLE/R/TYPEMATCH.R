TYPEMATCH <-
function(hearerID, analysis){
	hearer=population[[hearerID]]
	if('?'%in%analysis$role){
		verb=analysis[analysis$role=='verb',]
		todo='external'; if(verb$verbType=='twoPlace'){todo=c(todo, 'internal')}
		todo=todo[!todo%in%analysis$role]
		verbSemantics=hearer$verbs[hearer$verbs$ID==verb$verbID,]
		analysis$externalScore=0
		analysis$externalScore=VMATCH(verbSemantics[,grep('^Ext\\d',names(verbSemantics))], hearer$nouns[match(analysis$nounID,hearer$nouns$ID), grep('^D\\d',names(hearer$nouns))])
		if(verb$verbType=='twoPlace'){	
			analysis$internalScore=0
			analysis[!is.na(analysis$nounID),]$internalScore=VMATCH(verbSemantics[,grep('^Int\\d',names(verbSemantics))], hearer$nouns[match(analysis[!is.na(analysis$nounID),]$nounID,hearer$nouns$ID), grep('^D\\d',names(hearer$nouns))])
		}
		if(length(todo)==2 & length(grep('\\?',analysis$role)) > 1){
			externalMax=MAX(analysis[grep('\\?',analysis$role),]$externalScore, value=TRUE)
			if(length(externalMax) > 1){externalMax=0}
			internalMax=MAX(analysis[grep('\\?',analysis$role),]$internalScore, value=TRUE)
			if(length(internalMax) > 1){internalMax=0}
			if(externalMax > internalMax){	
				analysis[grep('\\?',analysis$role)[MAX(analysis[grep('\\?',analysis$role),]$externalScore)],]$role='external'; todo=todo[todo!='external']
			if(internalMax > externalMax){	
				analysis[grep('\\?',analysis$role)[MAX(analysis[grep('\\?',analysis$role),]$internalScore)],]$role='internal'; todo=todo[todo!='internal']
		}	}
		if(length(todo)==2 & length(grep('\\?',analysis$role))==1){
			if(analysis[grep('\\?',analysis$role),]$externalScore > (analysis[grep('\\?',analysis$role),]$internalScore)){
				analysis[grep('\\?',analysis$role),]$role='external'; todo=todo[todo!='external']
			}
			if(analysis[grep('\\?',analysis$role),]$internalScore > (analysis[grep('\\?',analysis$role),]$externalScore)){
				analysis[grep('\\?',analysis$role),]$role='internal'; todo=todo[todo!='internal']
		}	}	
		if(length(todo)==1 & 'external'%in%todo){
			if(length(grep('\\?',analysis$role))==1){
				analysis[grep('\\?',analysis$role),]$role='external'; todo=todo[todo!='external']
			}
			if(length(grep('\\?',analysis$role)) > 1){
				externalMax=MAX(analysis[grep('\\?',analysis$role),]$externalScore)
				if(length(externalMax)==1){
					analysis[grep('\\?',analysis$role)[MAX(analysis[grep('\\?',analysis$role),]$externalScore)],]$role='external'; todo=todo[todo!='external']
		}	}	}
		if(length(todo)==1 & 'internal'%in%todo){
			if(length(grep('\\?',analysis$role))==1){
				analysis[grep('\\?',analysis$role),]$role='internal'; todo=todo[todo!='internal']
			}
			if(length(grep('\\?',analysis$role)) > 1){
				internalMax=MAX(analysis[grep('\\?',analysis$role),]$internalScore, value=TRUE)
				if(length(internalMax)==1){
					analysis[grep('\\?',analysis$role)[MAX(analysis[grep('\\?',analysis$role),]$internalScore)],]$role='internal'; todo=todo[todo!='internal']}
	}	}	}	}
analysis
}
