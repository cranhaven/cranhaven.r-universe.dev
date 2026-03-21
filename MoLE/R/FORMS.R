FORMS <-
function(n, length=world$wordLength, vowels=world$vowels, consonants=world$consonants){
	NEWFORM=function(length){
		if(length(length)>1){length=sample(length, 1)}
		form=rep(c('C','V'),length)
		start=sample(2,1)	
		form=form[start:(length + start-1)]
		form[form=='V']=sample(vowels, sum(form=='V'), replace=TRUE)
		form[form=='C']=sample(consonants, sum(form=='C'), replace=TRUE)
		form=paste(form, collapse='')
	}	
	forms=NEWFORM(length)
	ADDFORM=function(length){
		word=NEWFORM(length)
		if(!(word%in%forms)){
			forms[length(forms) + 1]=word
		}
		forms
	}
	while(length(forms) < n){forms=ADDFORM(length)}
forms
}
