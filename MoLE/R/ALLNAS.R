ALLNAS <-
function(x){
	if(is.data.frame(x)){allnas=rowSums(is.na(x))==ncol(x)}
	if(is.vector(x)){allnas=sum(is.na(x))==length(x)}
as.vector(allnas)
}
