##
expit <- function(x) exp(x)/(1 + exp(x))

##
logit <- function(p) log(p/(1-p))

##
strReverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

##
expandCatX <- function(X, expandX="all")
{
	##
	if(expandX == "all")
		colIndex <- 1:ncol(X)
	if(expandX != "all")
		colIndex <- c(1:ncol(X))[is.element(colnames(X), expandX)]
	
	## Assumes a check has been performed to make sure the columns that are to be expanded
	## adhere to the {0,1,2,...} coding convention
	##
	value <- matrix(X[,1], ncol=1, dimnames=list(1:nrow(X), "Int"))
	##
	if(ncol(X) > 1)
	{
		n.lev <- unlist(lapply(apply(X, 2, unique), FUN=length))
		for(i in 2:ncol(X))
		{
			if(!is.element(i, colIndex))
			{
				value <- cbind(value, X[,i])
				colnames(value)[ncol(value)] <- colnames(X)[i]
			}
			else
			{
				for(j in 1:(n.lev[i]-1))
				{
					value <- cbind(value, as.numeric(X[,i] == j))
					if(n.lev[i] == 2) colnames(value)[ncol(value)] <- colnames(X)[i]
					if(n.lev[i] > 2) colnames(value)[ncol(value)] <- paste(colnames(X)[i], ".", j, sep="")
				}
			}
		}
	}
	##
	return(value)
}

##
stratify <- function(X, strata=NULL)
{
	##
	if(is.null(strata))
		value <- 1:nrow(X)
	##
	if(!is.null(strata) & length(strata) == 1)
		value <- X[,strata]
	##
	if(!is.null(strata) & length(strata) > 1)
	{
		if(is.element(1, strata))
			strata <- sort(strata)[-1]
		##
		temp <- apply(as.matrix(X[,strata]), 2, unique)
		if(is.matrix(temp))
			n.lev <- apply(apply(as.matrix(X[,strata]), 2, unique), 2, FUN=length)
		if(is.list(temp))
			n.lev <- unlist(lapply(apply(as.matrix(X[,strata]), 2, unique), FUN=length))
		##
		base10 <- cumsum(ceiling(log10(n.lev))) - 1
		base10 <- matrix(10^base10, nrow=nrow(X), ncol=length(strata), byrow=TRUE)
		value  <- apply(X[,strata]*base10, 1, sum)
	}
	##
	temp   <- value
  Slvls  <- unique(sort(temp))
  for(k in 1:length(Slvls)) value[temp == Slvls[k]] <- k
	##
	return(value)
}
