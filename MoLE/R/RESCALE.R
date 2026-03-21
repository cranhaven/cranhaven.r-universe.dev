RESCALE <-
function(x){ 
	if(max(x)==min(x)){x=rep(0, length(x))}
	if(max(x)!=min(x)){max=max(abs(x)); x=x/max}
x
}
