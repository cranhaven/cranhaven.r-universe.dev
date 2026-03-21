FMATCH <-
function(target, lexicon){	
	forms=data.frame(matrix(0, nrow=nrow(lexicon), ncol=nchar(target)), stringsAsFactors=FALSE)
	forms$remainder=nchar(lexicon$form)-nchar(target)
	forms$remainder[forms$remainder<0]=0
	forms$form=lexicon$form
	for (i in 1:nchar(target)){
		forms[,i]=gsub(paste('.{', i-1,'}(.).*', sep=''),'\\1',target)!=gsub(paste('.{', i-1,'}(.).*', sep=''),'\\1',forms$form)
	}
	max=nchar(target)+1
	forms[,1:max]=forms[,1:max]*t(replicate(nrow(forms), max:1))	
	forms$distance=rowSums(forms[,1:max])
	forms$fmatch=1/(1+forms$distance)
forms$fmatch
}
