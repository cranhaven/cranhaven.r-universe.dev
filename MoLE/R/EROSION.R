EROSION <-
function(hearerID, interpretation){
	hearer=population[[hearerID]]; reductionFrequencyThreshold=world$reductionFrequencyThreshold*hearer$age; erosionMax=world$erosionMax; formSetFrequency=world$formSetFrequency
	if(is.data.frame(interpretation$verb)){
		if(hearer$verbs[hearer$verbs$ID==interpretation$verb$ID,]$form!=interpretation$verb$form){
			if(nchar(interpretation$verb$form) >= erosionMax){
				ID=interpretation$verb$ID
				if(hearer$verbs[hearer$verbs$ID==ID,]$frequency < formSetFrequency){
					if(nchar(hearer$verbs[hearer$verbs$ID==ID,]$form)==nchar(interpretation$verb$form)){hearer$verbs[hearer$verbs$ID==ID,]$form=gsub('.$', substr(interpretation$verb$form, nchar(interpretation$verb$form), nchar(interpretation$verb$form)), hearer$verbs[hearer$verbs$ID==ID,]$form)}
					if(nchar(hearer$verbs[hearer$verbs$ID==ID,]$form)>nchar(interpretation$verb$form)){hearer$verbs[hearer$verbs$ID==ID,]$form=gsub('.$','',hearer$verbs[hearer$verbs$ID==ID,]$form)}
					if(nchar(hearer$verbs[hearer$verbs$ID==ID,]$form)<nchar(interpretation$verb$form)){hearer$verbs[hearer$verbs$ID==ID,]$form=paste(hearer$verbs[hearer$verbs$ID==ID,]$form, substr(interpretation$verb$form, nchar(hearer$verbs[hearer$verbs$ID==ID,]$form) + 1, nchar(hearer$verbs[hearer$verbs$ID==ID,]$form) + 1), sep='')}
				}
				hearer$verbs[hearer$verbs$ID==ID,]$productionEffort=nchar(hearer$verbs[hearer$verbs$ID==ID,]$form)
				if(hearer$verbs[hearer$verbs$ID==ID,]$productionEffort==0){
					hearer$verbs=hearer$verbs[hearer$verbs$ID!=ID,]
					graveyard$history[nrow(graveyard$history) + 1,]=c(hearer$generation, 'verb removed', 'EROSION', ID, '', '', '')
				}
		}	}
		if('extMarkerID'%in%names(interpretation$verb)){
			markerID=interpretation$verb$extMarkerID
			if(hearer$nouns[hearer$nouns$ID==markerID,]$form!=interpretation$verb$extMarker){
				if(nchar(interpretation$verb$extMarker) >= erosionMax){
					if(hearer$nouns[hearer$nouns$ID==markerID,]$frequency < formSetFrequency){
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)==nchar(interpretation$verb$extMarker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$', substr(interpretation$verb$extMarker, nchar(interpretation$verb$extMarker), nchar(interpretation$verb$extMarker)), hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)>nchar(interpretation$verb$extMarker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$','',hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)<nchar(interpretation$verb$extMarker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=paste(hearer$nouns[hearer$nouns$ID==markerID,]$form, substr(interpretation$verb$extMarker, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1), sep='')}
						hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort=nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)
						if(hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort==0){
							hearer$nouns=hearer$nouns[hearer$nouns$ID!=markerID,]
							graveyard$history[nrow(graveyard$history) + 1,]=c(hearer$generation, 'verb marker removed', 'EROSION', markerID, '', '', '')
		}	}	}	}	}
		if('intMarkerID'%in%names(interpretation$verb)){
			markerID=interpretation$verb$intMarkerID
			if(hearer$nouns[hearer$nouns$ID==markerID,]$form!=interpretation$verb$intMarker){
				if(nchar(interpretation$verb$intMarker) >= erosionMax){
					if(hearer$nouns[hearer$nouns$ID==markerID,]$frequency < formSetFrequency){
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)==nchar(interpretation$verb$intMarker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$', substr(interpretation$verb$intMarker, nchar(interpretation$verb$intMarker), nchar(interpretation$verb$intMarker)), hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)>nchar(interpretation$verb$intMarker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$','',hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)<nchar(interpretation$verb$intMarker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=paste(hearer$nouns[hearer$nouns$ID==markerID,]$form, substr(interpretation$verb$intMarker, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1), sep='')}
						hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort=nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)
						if(hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort==0){
							hearer$nouns=hearer$nouns[hearer$nouns$ID!=markerID,]
							graveyard$history[nrow(graveyard$history) + 1,]=c(hearer$generation, 'verb marker removed', 'EROSION', markerID, '', '', '')
	}	}	}	}	}	}
	if(is.data.frame(interpretation$external)){	
		if(hearer$nouns[hearer$nouns$ID==interpretation$external$ID, ]$form!=interpretation$external$form){
			if(nchar(interpretation$external$form) >= erosionMax){
				ID=interpretation$external$ID
				if(hearer$nouns[hearer$nouns$ID==ID,]$frequency < formSetFrequency){
					if(nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)==nchar(interpretation$external$form)){hearer$nouns[hearer$nouns$ID==ID,]$form=gsub('.$', substr(interpretation$external$form, nchar(interpretation$external$form), nchar(interpretation$external$form)), hearer$nouns[hearer$nouns$ID==ID,]$form)}
					if(nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)>nchar(interpretation$external$form)){hearer$nouns[hearer$nouns$ID==ID,]$form=gsub('.$','',hearer$nouns[hearer$nouns$ID==ID,]$form)}
					if(nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)<nchar(interpretation$external$form)){hearer$nouns[hearer$nouns$ID==ID,]$form=paste(hearer$nouns[hearer$nouns$ID==ID,]$form, substr(interpretation$external$form, nchar(hearer$nouns[hearer$nouns$ID==ID,]$form) + 1, nchar(hearer$nouns[hearer$nouns$ID==ID,]$form) + 1), sep='')}
					hearer$nouns[hearer$nouns$ID==ID,]$productionEffort=nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)
					if(hearer$nouns[hearer$nouns$ID==ID,]$productionEffort==0){
						hearer$nouns=hearer$nouns[hearer$nouns$ID!=ID,]
						graveyard$history[nrow(graveyard$history) + 1,]=c(hearer$generation, '(pro)noun removed', 'EROSION', ID, '', '', '')
		}	}	}	}
		if('marker'%in%names(interpretation$external)){
			markerID=interpretation$external$markerID
			if(hearer$nouns[hearer$nouns$ID==markerID,]$form!=interpretation$external$marker){
				if(nchar(interpretation$external$marker) >= erosionMax){
					if(hearer$nouns[hearer$nouns$ID==markerID,]$frequency < formSetFrequency){
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)==nchar(interpretation$external$marker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$', substr(interpretation$external$marker, nchar(interpretation$external$marker), nchar(interpretation$external$marker)), hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)>nchar(interpretation$external$marker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$','',hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)<nchar(interpretation$external$marker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=paste(hearer$nouns[hearer$nouns$ID==markerID,]$form, substr(interpretation$external$marker, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1), sep='')}
						hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort=nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)
						if(hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort==0){
							hearer$nouns=hearer$nouns[hearer$nouns$ID!=markerID,]
							graveyard$history[nrow(graveyard$history) + 1,]=c(hearer$generation, 'noun marker removed', 'EROSION', markerID, '', '', '')
	}	}	}	}	}	}
	if(is.data.frame(interpretation$internal)){	
		if(hearer$nouns[hearer$nouns$ID==interpretation$internal$ID,]$form!=interpretation$internal$form){
			if(nchar(interpretation$internal$form) >= erosionMax){
				ID=interpretation$internal$ID
				if(hearer$nouns[hearer$nouns$ID==ID,]$frequency < formSetFrequency){
					if(nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)==nchar(interpretation$internal$form)){hearer$nouns[hearer$nouns$ID==ID,]$form=gsub('.$', substr(interpretation$internal$form, nchar(interpretation$internal$form), nchar(interpretation$internal$form)), hearer$nouns[hearer$nouns$ID==ID,]$form)}
					if(nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)>nchar(interpretation$internal$form)){hearer$nouns[hearer$nouns$ID==ID,]$form=gsub('.$','',hearer$nouns[hearer$nouns$ID==ID,]$form)}
					if(nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)<nchar(interpretation$internal$form)){hearer$nouns[hearer$nouns$ID==ID,]$form=paste(hearer$nouns[hearer$nouns$ID==ID,]$form, substr(interpretation$internal$form, nchar(hearer$nouns[hearer$nouns$ID==ID,]$form) + 1, nchar(hearer$nouns[hearer$nouns$ID==ID,]$form) + 1), sep='')}
					hearer$nouns[hearer$nouns$ID==ID,]$productionEffort=nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)
					if(hearer$nouns[hearer$nouns$ID==ID,]$productionEffort==0){
						hearer$nouns=hearer$nouns[hearer$nouns$ID!=ID,]
						graveyard$history[nrow(graveyard$history) + 1,]=c(hearer$generation, '(pro)noun removed', 'EROSION', ID, '', '', '')
		}	}	}	}
		if('marker'%in%names(interpretation$internal)){
			markerID=interpretation$internal$markerID
			if(hearer$nouns[hearer$nouns$ID==markerID,]$form!=interpretation$internal$marker){
				if(nchar(interpretation$internal$marker) >= erosionMax){
					if(hearer$nouns[hearer$nouns$ID==markerID,]$frequency < formSetFrequency){
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)==nchar(interpretation$internal$marker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$', substr(interpretation$internal$marker, nchar(interpretation$internal$marker), nchar(interpretation$internal$marker)), hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)>nchar(interpretation$internal$marker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$','',hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)<nchar(interpretation$internal$marker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=paste(hearer$nouns[hearer$nouns$ID==markerID,]$form, substr(interpretation$internal$marker, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1), sep='')}
						hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort=nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)
						if(hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort==0){
							hearer$nouns=hearer$nouns[hearer$nouns$ID!=markerID,]
							graveyard$history[nrow(graveyard$history) + 1,]=c(hearer$generation, 'noun marker removed', 'EROSION', markerID, '', '', '')
	}	}	}	}	}	}
	population[[hearerID]]=hearer
population <<- population
graveyard <<- graveyard
}
