DECOMPOSE <-
function(hearerID, form){
	suffixThreshold=world$suffixThreshold; erosionMax=world$erosionMax
	if(erosionMax==0){erosionMax=1} 
	hearer=population[[hearerID]]
	lexicon=hearer$nouns
	suffixes=lexicon[lexicon$productionEffort <= (suffixThreshold + 1),]$form	
	suffixes=suffixes[nchar(suffixes)  >= erosionMax]		
	composition=vector()
	if(length(suffixes)!=0){
		suffixes[nchar(suffixes)>erosionMax]=gsub('.$','.?$',suffixes[nchar(suffixes)>erosionMax])
		suffixes[nchar(suffixes)==erosionMax]=gsub('.$','.$',suffixes[nchar(suffixes)==erosionMax])
		suffixes=unique(suffixes)
		for (i in 1:length(suffixes)){
			if(grepl(suffixes[i], form) & nchar(gsub(suffixes[i],'',form))  >=erosionMax){
				stem=gsub(suffixes[i], '', form)
				suffix=gsub(paste('.*(',suffixes[i],')',sep=''), '\\1', form)
				composition[length(composition) + 1]=paste(stem, suffix, sep='-')
				for (j in 1:length(suffixes)){ 
					if(grepl(suffixes[j], stem) & nchar(gsub(suffixes[j],'',stem))  >=erosionMax){
						suffix2=paste(gsub(paste('.*(',suffixes[j],')',sep=''), '\\1', stem), suffix, sep='-')
						stem2=gsub(suffixes[j], '', stem)
						composition[length(composition) + 1]=paste(stem2, suffix2, sep='-')
	}	}	}	}	}
unique(composition)
}
