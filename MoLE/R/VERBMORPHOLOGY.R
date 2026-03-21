VERBMORPHOLOGY <-
function(hearerID, analysis){
	topicCopy=world$topicCopy; refCheck=world$refCheck; referenceThreshold=world$referenceThreshold; distinctiveness=world$distinctiveness; collostructionImpact=world$collostructionImpact
	hearer=population[[hearerID]]
	nouns=hearer$nouns
	verbs=hearer$verbs
	if('verbSuffix'%in%analysis$role | 'verbAdposition'%in%analysis$role){
		analysis$verbMarkerTarget='?'
		yangTopic=ifelse(sum(hearer$wordOrder$success)>8 & TRUE %in% hearer$topicPosition$success[hearer$topicPosition$position=='other'] < (sum(hearer$topicPosition$success)/log(sum(hearer$topicPosition$success))), TRUE, FALSE)
		yangIndex=TRUE %in% c(hearer$usageHistory$index$no < ((hearer$usageHistory$index$yes + hearer$usageHistory$index$no)/log(hearer$usageHistory$index$yes + hearer$usageHistory$index$no)))
	}
	if('verbSuffix'%in%analysis$role){	
		verbSuffixes=grep('verbSuffix', analysis$role)
		targets=grep('\\?|internal|external', analysis$role)
		verb=hearer$verbs[hearer$verbs$ID==analysis[grep('^verb$',analysis$role),]$verbID,]
		###reanalyze?
		if(verb$type=='onePlace'){
			if(length(verbSuffixes)==1 & length(targets)==0){analysis[verbSuffixes,]$role='?'}
		}
		if(verb$type=='twoPlace'){
			if(length(targets)<2 & 'verbAdposition'%in%analysis$role){	#if too few targets, first reanalyze verb adposition
				analysis[grep('verbAdposition',analysis$role), ]$role='?'
				targets=grep('\\?|internal|external', analysis$role)
			}
			if(length(targets)<2 & length(verbSuffixes)==1){analysis[verbSuffixes,]$role='?'}	#still too few targets, reanalyze verb suffix as pro-index
			if(length(targets)==1 & length(verbSuffixes)==2){analysis[verbSuffixes[2],]$role='?'}	
			if(length(targets)==0 & length(verbSuffixes)==2){analysis[verbSuffixes,]$role=c('internal', 'external')}	#if no targets, reanalyse as pro-indexes; inner verb suffix has internal role
		}
		verbSuffixes=grep('verbSuffix', analysis$role)
	}
	if('verbAdposition'%in%analysis$role){		
		if(analysis[1,]$role!='verb'){analysis[grep('verbAdposition',analysis$role), ]$verbMarkerTarget=1}
	}
	#check again
	if('verbSuffix'%in%analysis$role){	
		proto=PROTOINTERPRETATION(hearerID, analysis)	#only adds unknown roles, does not overwrite anything							
		###intransitives
		if(verb$type=='onePlace'){
			if(length(targets)==1 & length(verbSuffixes)==1){
				#simple person match
				if(analysis[verbSuffixes,]$verbMarkerPerson==analysis[targets,]$nounPerson){
					analysis[verbSuffixes,]$verbMarkerTarget=targets
				}
				#recruitment
				if(refCheck==TRUE & nchar(analysis[verbSuffixes,]$form)<=referenceThreshold){
					analysis[verbSuffixes,]$verbMarkerTarget=targets
		}	}	}	
		###transitives
		if(verb$type=='twoPlace'){
			#single suffix
			if(length(verbSuffixes)==1){
				v=verbSuffixes
				role='unclear'
				person=analysis[v,]$verbMarkerPerson
				data=hearer$usageHistory$index[hearer$usageHistory$index$person==person,]
				data=data[data$no < (data$yes + data$no)/log(data$yes + data$no),]
				if(nrow(data)!=0){
					role=data$role[MAX((data$yes + data$no)/log(data$yes + data$no), forceChoice=TRUE)]
					actor=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'external', 'internal')
					if(role=='actor'){role=ifelse(actor=='external', 'external', 'internal')}
					if(role=='undergoer'){role=ifelse(actor=='external', 'internal', 'external')}
				}
				#single person pair
				if(length(grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson))==1){	
					analysis[v,]$verbMarkerTarget=targets[grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson)]
				}
				#person pairs with multiple targets: use role?
				if(length(grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson))>1){
					if(role%in%analysis$role){
						analysis[v,]$verbMarkerTarget=grep(role, analysis$role)
					} else if(role%in%proto$role){
						analysis[v,]$verbMarkerTarget=grep(role, proto$role)
				}	}
				#no match, recruitment?
				if(refCheck==TRUE & !analysis[v,]$verbMarkerPerson %in% analysis[targets,]$nounPerson & nchar(analysis[v,]$form)<=referenceThreshold & length(targets)!=0){
					#single target
					if(length(grep(3, analysis[targets,]$nounPerson))==1){
						analysis[v,]$verbMarkerTarget=targets[grep(3, analysis[targets,]$nounPerson)]
					}
					#multiple targets
					if(length(grep(3, analysis[targets,]$nounPerson))>1){
						if(role%in%analysis$role){
							analysis[v,]$verbMarkerTarget=grep(role, analysis$role)
						} else if(role%in%proto$role){
							analysis[v,]$verbMarkerTarget=grep(role, proto$role)
						}
						#else, use prominence
						if(nrow(data)==0){
							analysis[v,]$verbMarkerTarget=targets[MAX(VMATCH(rep(1, length(grep('^D\\d', names(nouns)))), nouns[match(analysis[targets,]$nounID, nouns$ID), grep('^D\\d', names(nouns))]), forceChoice=TRUE)]
			}	}	}	}
			#multiple suffixes
			if(length(verbSuffixes)==2){
				for(v in verbSuffixes){
					#unique person pairs
					if(length(grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson))==1 & length(grep(analysis[v,]$verbMarkerPerson, analysis[verbSuffixes,]$verbMarkerPerson))==1){	
						analysis[v,]$verbMarkerTarget=targets[grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson)]
					}	
					#multiple person pairs with different targets: internal suffix internal role 
					if(length(grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson))>1){
						if(grep(v, verbSuffixes)==1){
							if('internal'%in%analysis$role){
								analysis[v,]$verbMarkerTarget=grep('internal', analysis$role)
							} else if('internal'%in%proto$role){
								analysis[v,]$verbMarkerTarget=grep('internal', proto$role)
						}	}
						if(grep(v, verbSuffixes)==2){
							if('external'%in%analysis$role){
								analysis[v,]$verbMarkerTarget=grep('external', analysis$role)
							} else if ('external'%in%proto$role){
								analysis[v,]$verbMarkerTarget=grep('external', proto$role)
					}	}	}
					#multiple person pairs with single target
					if(length(grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson))==1 & length(grep(analysis[v,]$verbMarkerPerson, analysis[verbSuffixes,]$verbMarkerPerson))==2){
						#first try role (if explicitly marked)
						if(grep(v, verbSuffixes)==1){
							if('internal'%in%analysis$role){analysis[v,]$verbMarkerTarget=grep('internal', analysis$role)}
						}
						if(grep(v, verbSuffixes)==2){
							if('external'%in%analysis$role){analysis[v,]$verbMarkerTarget=grep('external', analysis$role)}
						}
						#next try collostructions
						if(!'internal'%in%analysis$role & !'external'%in%analysis$role){
							collostructions=hearer$collostructions$index[hearer$collostructions$index$N==analysis[targets[grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson)],]$nounID,]
							collostruction=rep(0, 2)
							if(nrow(collostructions)!=0){collostruction[analysis[verbSuffixes,]$verbMarkerID%in%collostructions$marker]=collostructions[na.omit(match(analysis[verbSuffixes,]$verbMarkerID, collostructions$marker)),]$frequency}
							analysis[verbSuffixes[MAX(collostruction, forceChoice=TRUE)],]$verbMarkerTarget=targets[grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson)]
					}	}	
					#no match, recruitment?
					if(refCheck==TRUE & analysis[v,]$verbMarkerTarget=='?'){
						if(nchar(analysis[v,]$form)<=referenceThreshold & length(targets)!=0){
							if(length(grep(3,analysis[targets,]$nounPerson))==1){
								analysis[v,]$verbMarkerTarget=targets[grep(3,analysis[targets,]$nounPerson)]
							}
							if(length(grep(3,analysis[targets,]$nounPerson))>1){
								if(grep(v, verbSuffixes)==1){
									if('internal'%in%analysis$role){
										analysis[v,]$verbMarkerTarget=grep('internal', analysis$role)
									} else if ('internal'%in%proto$role){
								analysis[v,]$verbMarkerTarget=grep('internal', proto$role)
								}	}
								if(grep(v, verbSuffixes)==2){
									if('external'%in%analysis$role){
										analysis[v,]$verbMarkerTarget=grep('external', analysis$role)
									} else if ('external'%in%proto$role){
										analysis[v,]$verbMarkerTarget=grep('external', proto$role)
					}	}	}	}	}
					targets=targets[!targets%in%analysis[verbSuffixes,]$verbMarkerTarget]
		}	}	}						
		##role resolution
		targets=analysis[verbSuffixes,]$verbMarkerTarget
		if(!'?'%in%targets){
			if(length(verbSuffixes)==2){	
				if(analysis[targets[1],]$role=='?' & !'internal'%in%analysis$role){analysis[targets[1],]$role='internal'}
				if(analysis[targets[2],]$role=='?' & !'external'%in%analysis$role){analysis[targets[2],]$role='external'}
			}
			if(length(verbSuffixes)==1){
				if(analysis[targets,]$role=='?'){
					#if generalization is made
					if(yangIndex==TRUE){
						person=analysis[verbSuffixes,]$verbMarkerPerson
						data=hearer$usageHistory$index[hearer$usageHistory$index$person==person,]
						data=data[data$no < (data$yes + data$no)/log(data$yes + data$no),]
						#if relevant generalization is made
						if(nrow(data)!=0){
							role=data$role[MAX((data$yes + data$no)/log(data$yes + data$no), forceChoice=TRUE)]
							actor=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'external', 'internal')
							if(role=='actor'){role=ifelse(actor=='external', 'external', 'internal')}
							if(role=='undergoer'){role=ifelse(actor=='external', 'internal', 'external')}
							if(!role%in%analysis$role){analysis[targets,]$role==role}
					}	}
					#if no generalization is made and suffix is distinctive for role (e.g. in case of fusion of role-distinctive pronoun)
					if(analysis[targets,]$role=='?'){
						verbSuffix=analysis[verbSuffixes,]$verbMarkerID
						externalMatch=VMATCH(verb[,grep('^Ext\\d',names(verb))], nouns[nouns$ID==verbSuffix,grep('^D\\d',names(nouns))])
						internalMatch=VMATCH(verb[,grep('^Int\\d',names(verb))], nouns[nouns$ID==verbSuffix,grep('^D\\d',names(nouns))])
						externalCollostruction=sum(hearer$collostructions$SV[grep(paste('^', verbSuffix, '$', sep=''), hearer$collostructions$SV$S),]$frequency)
						internalCollostruction=sum(hearer$collostructions$OV[grep(paste('^', verbSuffix, '$', sep=''), hearer$collostructions$OV$O),]$frequency)
						externalCollostruction=externalCollostruction/(externalCollostruction+internalCollostruction); if(is.na(externalCollostruction)){externalCollostruction=0}
						internalCollostruction=internalCollostruction/(internalCollostruction+externalCollostruction); if(is.na(internalCollostruction)){internalCollostruction=0}
						role=ifelse((externalMatch+collostructionImpact*externalCollostruction)>=(internalMatch+collostructionImpact*internalCollostruction), 'external', 'internal')
						if(!role%in%analysis$role){analysis[targets,]$role==role}
	}	} 	}	}	}
analysis
}
