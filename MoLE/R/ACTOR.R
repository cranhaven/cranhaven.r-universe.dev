ACTOR <-
function(x, y){
	x=VMATCH(rep(1, length(x)), x)
	y=VMATCH(rep(1, length(y)), y)
	actor=ifelse(x<y,2,1)	
actor
}
