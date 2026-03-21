RUN <-
function(nHours=1){
	nTurns=world$nTurns
	start=proc.time()[3]
	stop=proc.time()[3]
	while(((stop-start)/60/60)<nHours){
		TALK(sample(nTurns,1))
		stop=proc.time()[3]
}	}
