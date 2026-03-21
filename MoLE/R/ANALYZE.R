ANALYZE <-
function(hearerID, utterance, situation){
	refCheck=world$refCheck; referenceThreshold=world$referenceThreshold
	hearer=population[[hearerID]]
	actions=unique(situation[, grep('^V',names(situation))])
	As=situation[, grep('^A',names(situation))]; names(As)=gsub('A', 'D', names(As))
	Us=situation[, grep('^U',names(situation))]; names(Us)=gsub('U', 'D', names(As))
	objects=unique(rbind(As, Us))
	forms=unlist(strsplit(utterance,'\\s'))
	forms=forms[forms!='']
	analysis=data.frame(word=1, form='', option=1, stem='', role='', stringsAsFactors=FALSE)
	n=1
	for (i in 1:length(forms)){
		analysis[n,]$word=i
		analysis[n,]$form=forms[i]
		analysis[n,]$option=1
		analysis[n,]$stem=forms[i]
		analysis[n,]$role='?'
		n=n + 1
		decomposables=DECOMPOSE(hearerID, forms[i])
		if(length(decomposables)!=0){
			m=2
			for (word in decomposables){
				analysis[n,]$word=i
				analysis[n,]$form=forms[i]
				analysis[n,]$option=m
				analysis[n,]$stem=unlist(strsplit(word,'-'))[1]
				analysis[n,]$role='?'
				n=n + 1
				analysis[n,]$word=i
				analysis[n,]$form=unlist(strsplit(word,'-'))[2]
				analysis[n,]$option=m
				analysis[n,]$stem=unlist(strsplit(word,'-'))[2]
				analysis[n,]$role='suffix'
				n=n + 1
				if(length(unlist(strsplit(word,'-'))) > 2){
					analysis[n,]$word=i
					analysis[n,]$form=unlist(strsplit(word,'-'))[3]
					analysis[n,]$option=m
					analysis[n,]$stem=unlist(strsplit(word,'-'))[3]
					analysis[n,]$role='suffix'
					n=n + 1
				}		
				m=m + 1
	}	}	}
	analysis=analysis[!is.na(analysis$stem),]
	analysis$verbID=0; analysis$verb=''; analysis$verbType=''; analysis$verbMatch=0; analysis$actionMatch=0; analysis$verbScore=0
	analysis$nounID=0; analysis$noun=''; analysis$nounPerson=0; analysis$nounMatch=0; analysis$objectMatch=0; analysis$nounScore=0
	analysis$nounMarkerID=0; analysis$nounMarker=''; analysis$nounMarkerPerson=0; analysis$nounMarkerMatch=0
	analysis$verbMarkerID=0; analysis$verbMarker=''; analysis$verbMarkerPerson=0; analysis$verbMarkerMatch=0
	for (i in 1:nrow(analysis)){
		if(analysis$role[i]=='suffix'){
			candidates=hearer$nouns
			candidates$match=FMATCH(analysis$stem[i], candidates)
			entry=candidates[MAX(candidates$match, forceChoice=FALSE),]
			if(nrow(entry)>1){entry=entry[MAX(entry$activation, forceChoice=TRUE),]}
			analysis$nounMarkerID[i]=entry$ID
			analysis$nounMarkerPerson[i]=entry$person
			analysis$nounMarker[i]=entry$form
			analysis$nounMarkerMatch[i]=entry$match
			analysis$verbMarkerID[i]=entry$ID
			analysis$verbMarkerPerson[i]=entry$person
			analysis$verbMarker[i]=entry$form
			analysis$verbMarkerMatch[i]=entry$match
			analysis$nounID[i]=entry$ID	
			analysis$nounPerson[i]=entry$person
			analysis$noun[i]=entry$form
			analysis$nounMatch[i]=entry$match
			analysis$objectMatch[i]=MAX(VMATCH(hearer$nouns[hearer$nouns$ID==analysis$nounID[i],], objects), forceChoice=TRUE, value=TRUE)
		}
		if(analysis$role[i]=='?'){
			candidates=hearer$verbs	
			candidates$match=FMATCH(analysis$stem[i], candidates)
			entry=candidates[MAX(candidates$match, forceChoice=FALSE),]
			if(nrow(entry)>1){entry=entry[MAX(entry$activation, forceChoice=TRUE),]}
			analysis$verbID[i]=entry$ID
			analysis$verb[i]=entry$form
			analysis$verbType[i]=ifelse(entry$type=='onePlace','onePlace','twoPlace') 
			analysis$verbMatch[i]=entry$match
			analysis$actionMatch[i]=MAX(VMATCH(hearer$verbs[hearer$verbs$ID==analysis$verbID[i],], actions), forceChoice=TRUE, value=TRUE)
			candidates=hearer$nouns	
			candidates$match=FMATCH(analysis$stem[i], candidates)
			entry=candidates[MAX(candidates$match, forceChoice=FALSE),]
			if(nrow(entry)>1){entry=entry[MAX(entry$activation, forceChoice=TRUE),]}
			analysis$nounID[i]=entry$ID
			analysis$nounPerson[i]=entry$person
			analysis$noun[i]=entry$form
			analysis$nounMatch[i]=entry$match
			if(analysis$nounPerson[i]==3){analysis$objectMatch[i]=MAX(VMATCH(hearer$nouns[hearer$nouns$ID==analysis$nounID[i],], objects), forceChoice=TRUE, value=TRUE)}
			if(analysis$nounPerson[i]!=3){analysis$objectMatch[i]=ifelse(analysis$nounPerson[i]%in%unique(situation$personA, situation$personU), 1, -1)}
			if(nrow(analysis)>(i+2)){
				if(analysis$role[i+1]=='suffix' & analysis$role[i+2]=='suffix'){analysis$nounMatch[i]=-1}	
			}
			analysis$verbMarkerID[i]=entry$ID	
			analysis$verbMarker[i]=entry$form
			analysis$verbMarkerPerson[i]=entry$person
			analysis$verbMarkerMatch[i]=entry$match
			analysis$nounMarkerID[i]=entry$ID	
			analysis$nounMarkerPerson[i]=entry$person
			analysis$nounMarker[i]=entry$form
			analysis$nounMarkerMatch[i]=entry$match
	}	}
	analysis$verbScore=analysis$verbMatch * analysis$actionMatch
	analysis$nounScore=analysis$nounMatch * analysis$objectMatch
	if(nrow(analysis)==1){	
		analysis$role=ifelse(analysis$verbScore > analysis$nounScore, 'verb', '?')	
	} 
	if(nrow(analysis) > 1){
		candidates=grep('\\?',analysis$role)
		for (i in candidates){
			arguments=intersect(grep(analysis$word[i], analysis$word, invert=TRUE), grep('\\?', analysis$role))
			argumentScores=tapply(analysis[arguments, ]$nounScore, analysis[arguments,]$word, max)
			analysis$verbScore[i]=prod(analysis$verbScore[i], argumentScores)
		}
		verbIndex=MAX(analysis$verbScore)
		if(length(verbIndex) > 1){	
			suffixScores=rep(0, length(verbIndex))
			for(j in 1:length(verbIndex)){
				suffixes=intersect(grep(analysis$word[verbIndex[j]], analysis$word), grep(analysis$option[verbIndex[j]], analysis$option)); suffixes=intersect(suffixes, grep('suffix', analysis$role))
				suffixScores[j]=prod(analysis[suffixes, ]$verbMarkerMatch)
			}
			verbIndex=verbIndex[MAX(suffixScores)]
		}
		if(length(verbIndex) > 1){	
			verbIndex=verbIndex[MIN(analysis[verbIndex,]$nounScore)]
		}
		if(length(verbIndex) > 1){verbIndex=sample(verbIndex, 1)}
		analysis$role[verbIndex]='verb'
		suffixes=grep('suffix',analysis$role)
		if((verbIndex + 1)%in%suffixes){	
			analysis[verbIndex + 1,]$role='verbSuffix'
			if((verbIndex + 2)%in%suffixes){analysis[verbIndex + 2,]$role='verbSuffix'}
		}
		if('suffix'%in%analysis$role){analysis[analysis$role=='suffix',]$role='nounSuffix'}
		analysis=analysis[!(analysis$word==analysis$word[verbIndex] & analysis$option!=analysis$option[verbIndex]),]
		wrong=intersect(grep('nounSuffix', analysis$role), grep(3, analysis$nounMarkerPerson, invert=TRUE))
		wrong=unique(c(wrong, intersect(grep('nounSuffix', analysis$role),(grep('nounSuffix', analysis$role) + 1))))
		if(length(wrong)!=0){
			for (i in wrong){
				if(analysis$role[i-1]=='?'){analysis$nounScore[i-1]=0}
				if(analysis$role[i-1]=='nounSuffix'){analysis$nounScore[i-2]=0}
		}	}
		for (word in unique(analysis[grep('\\?',analysis$role),]$word)){
			option=MAX(analysis[intersect(grep('\\?', analysis$role), grep(word, analysis$word)),]$nounScore, forceChoice=TRUE)
			analysis=analysis[!(analysis$word==word & analysis$option!=option),]
		}		
		row.names(analysis)=1:nrow(analysis)
	}
	analysis$topic=0
analysis
}
