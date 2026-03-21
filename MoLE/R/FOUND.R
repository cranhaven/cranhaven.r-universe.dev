FOUND <-
function(nAgents=world$nAgents){
	firstspeaker=FIRSTSPEAKER()
	population=list()
	for (i in 1:nAgents){population[[i]]=firstspeaker}
	names(population)=toupper(FORMS(nAgents, length=6))
	graveyard=list(summary=data.frame(name=as.character(vector()), generation=as.numeric(vector()), successRate=as.numeric(vector()), meanNounMarkerFrequency=as.numeric(vector()), topnounMarkerFrequency=as.numeric(vector()), meanverbMarkerFrequency=as.numeric(vector()), topverbMarkerFrequency=as.numeric(vector()), stringsAsFactors=F), brains=list(), history=data.frame(generation=vector(), change=vector(), principle=vector(), ID=vector(), person=vector(), N=vector(), marker=vector()))
	graveyard$brains[[length(graveyard$brains) + 1]]=population[[1]]
	graveyard$brains[[length(graveyard$brains)]]$generation=0
population<<-population
graveyard<<-graveyard
}
