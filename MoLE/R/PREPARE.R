PREPARE <-
function(speakerID, proposition, situation){
	generalization=world$generalization; refCheck=world$refCheck; checkSuccess=world$checkSuccess; firstInFirstOut=world$firstInFirstOut; erosion=world$erosion
	prep=proposition
	if(refCheck==TRUE){prep=REFCHECK(speakerID, prep, situation)}	#first check whether referential expressions are strong enough
	if(firstInFirstOut==TRUE){prep=FIRSTINFIRSTOUT(speakerID, prep)}	#order ingredients of proposition by activation before other principles apply
	if(generalization==TRUE & population[[speakerID]]$generation>1){prep=GENERALIZE(speakerID, prep, situation)}		#apply observed rules (overrules FIFO)
	if(checkSuccess==TRUE){prep=CHECKSUCCESS(speakerID, prep, situation)}
	if(erosion==TRUE){prep=REDUCE(speakerID, prep)}
prep<<-prep
prep
}
