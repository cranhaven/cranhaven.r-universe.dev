FIRSTSPEAKER <-
function(){
	local=world$local; distinctions=world$distinctions
	verbs=VERBS()
	nouns=NOUNS()
	values=vector(); for(i in 1:length(distinctions)){values=c(values, seq(0,1,length.out=distinctions[i]))}
	flag=list(
		person=data.frame(role=rep(c('actor', 'undergoer'), each=3), person=rep(1:3, 2), yes=0, no=4, stringsAsFactors=FALSE), 
		actor=data.frame(dimension=rep(1:length(distinctions), distinctions), value=values, yes=0, no=4, stringsAsFactors=FALSE),
		undergoer=data.frame(dimension=rep(1:length(distinctions), distinctions), value=values, yes=0, no=4, stringsAsFactors=FALSE)
	)
	usageHistory=list(
		verbs=data.frame(matrix(0, nrow=0, ncol=length(grep('D\\d',names(verbs))) + 1, dimnames=list(NULL,c('verb',paste('D',1:length(distinctions), sep='')))), stringsAsFactors=FALSE),
		nouns=data.frame(matrix(0, nrow=0, ncol=length(grep('D\\d',names(nouns))) + 1, dimnames=list(NULL,c('noun',paste('D',1:length(distinctions), sep='')))), stringsAsFactors=FALSE),
		index=data.frame(role=rep(c('actor', 'undergoer'), each=3), person=rep(1:3, 2), yes=0, no=4, stringsAsFactors=FALSE),	
		flag=flag
	)
	flag=data.frame(marker=1, N=1, frequency=0, stringsAsFactors=FALSE); flag=flag[-1,]
	index=data.frame(marker=1, N=1, frequency=0, stringsAsFactors=FALSE); index=index[-1,]
	SV=data.frame(S=1, V=1, frequency=0, stringsAsFactors=FALSE); SV=SV[-1,]
	OV=data.frame(O=1,V=1, frequency=0, stringsAsFactors=FALSE); OV=OV[-1,]
	collostructions=list(SV=SV, OV=OV, index=index, flag=flag)
	wordOrder=data.frame(order=c('AVU', 'AUV', 'VAU', 'VUA', 'UAV', 'UVA'), freq=0, success=0, stringsAsFactors=FALSE)
	topic=data.frame(role=c('actor','undergoer'), topic=0, stringsAsFactors=FALSE)
	topicPosition=data.frame(position=c('first', 'other'), freq=0, success=0, stringsAsFactors=FALSE)
	commonGround=vector()
	list(age=0, generation=1, fertile='yes', semupdate=0, verbs=verbs, nouns=nouns, usageHistory=usageHistory, commonGround=commonGround, collostructions=collostructions, topic=topic, wordOrder=wordOrder, topicPosition=topicPosition)
}
