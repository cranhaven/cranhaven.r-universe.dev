###############################################################################
# Common functions used internally
###############################################################################
len = function(x) length(x)

mlast = function(m, n=1) if( is.matrix(m) ) m[(nrow(m)-n+1):nrow(m), ,drop=FALSE] else m[(len(m)-n+1):len(m)]

mlag = function(m, nlag=1) 
  if( is.matrix(m) ) {
    n = nrow(m)
    if(nlag > 0) {
      m[(nlag+1):n,] = m[1:(n-nlag),]
      m[1:nlag,] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag),] = m[(1-nlag):n,]
      m[(n+nlag+1):n,] = NA
    } 
	m
  } else { # vector
    n = len(m)
    if(nlag > 0) {
      m[(nlag+1):n] = m[1:(n-nlag)]
      m[1:nlag] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag)] = m[(1-nlag):n]
      m[(n+nlag+1):n] = NA
    }
	m	
  }

spl = function(s, delim = ',') unlist(strsplit(s,delim))

trim = function(s) sub('\\s+$', '', sub('^\\s+', '', s))

make.copy = function(x, default) { out = x; out[] = default; out }
	  
	  
iif = function(cond, truepart, falsepart) 
	if(len(cond) == 1) { 
		if(cond) truepart else falsepart 
	} else {  
		if(length(falsepart) == 1) falsepart = make.copy(cond, falsepart)
    	
		cond[is.na(cond) | is.nan(cond) | is.infinite(cond)] = FALSE
			
		if(length(truepart) == 1) 
			falsepart[cond] = truepart 
		else
			falsepart[cond] = zoo::coredata(truepart)[cond]
		falsepart
	}
    
ifnull = function(x, y) iif(is.null(x), y, x)

ifna = function(x, y) iif(is.na(x) | is.nan(x) | is.infinite(x), y, x)

to.date = function(x) if(class(x)[1] != 'Date') as.Date(x, format='%Y-%m-%d') else x

# set global options
set.options = function(key, ..., overwrite=TRUE) {
	values = list(...)
	if( len(values) == 1 && is.null(names(values))) values = values[[1]]
	temp = ifnull(options()[[key]], list())
	
	for(i in names(values))
		if(overwrite)
			temp[[i]] = values[[i]]
		else {
			if( is.null(temp[[i]]) )
				temp[[i]] = values[[i]]
		}
	
	options(make.list(key, temp))
}

# make list
make.list = function(key, value) {
	out = list()
	out[[key]] = value
	out
}

