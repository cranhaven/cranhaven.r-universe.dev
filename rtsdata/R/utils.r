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

# shortcut for xts index
indexts = function(x) {
	temp = xts::.index(x)
	class(temp) = c('POSIXct', 'POSIXt')
  
	type = xts::tclass(x)[1]
	if( type == 'Date' || type == 'yearmon' || type == 'yearqtr')
		temp = as.Date(temp)
	temp
}

# capture cat function output to string
cat2str = function(..., sep=',') {
	file = rawConnection(raw(0L), open='w')

	cat(..., file = file, sep = sep)	
	out = rawToChar(rawConnectionValue(file))
	
	close(file)
	file = NULL
	
	out
}

# convert last record of xts data to string
last2str = function(x) {
	temp = mlast(x)
	cat2str(indexts(temp), temp)
}
first2str = function(x) {
	temp = x[1,]
	cat2str(indexts(temp), temp)
}


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


# --------------------------------
# load default option
# 
# use following waterfall
# 1. options
# 2. environment
# 3. default option
# 
# more details at
# http://blog.revolutionanalytics.com/2015/11/how-to-store-and-use-authentication-details-with-r.html
# --------------------------------
get.default.option = function(name, default) {
	# try options first
	if (is.null(getOption(name))) { 
	
		# try environment variables next
		value = Sys.getenv(name, unset=NA)
		
		# use default is missing
		if (is.na(value)) 
			options(make.list(name, default))
		else
			options(make.list(name, value))
	}
	getOption(name)
}	


###############################################################################
#' Read csv
#'
#' @param filename CSV filename
#' @param sep delimiter
#' @param ... other parameters
#'
#' @examples
#'  # generate csv file
#'  filename = file.path(tempdir(), 'dummy.csv')
#'  cat('x1,x2,x3\n1,2,3\n', file = filename)
#'  ds.load.csv(filename)
#'  
#' @export
###############################################################################
ds.load.csv = function(filename, sep=',', ...)
	tryCatch({ 
		if( requireNamespace('data.table', quietly = TRUE) )
			suppressWarnings(data.table::fread(filename, stringsAsFactors=FALSE, sep=sep, ...))
		else
			warning('"data.table" package could not be loaded')
	}, error = function(ex) 
		suppressWarnings(data.table::data.table(utils::read.csv(filename, check.names=FALSE, stringsAsFactors=FALSE, sep=sep, ...)))
	)

		
###############################################################################
#' Load data from URL
#'
#' @param url url
#' @param h curl handle
#' @param useragent user agent
#' @param referer referer
#'
#' @examples
#'  \donttest{ 
#'  ds.get.url('https://finance.yahoo.com/')
#'  }
#'
#' @export
###############################################################################
ds.get.url = function
(
	url,
	h = curl::new_handle(),
	useragent = 'Mozilla/5.0 (Windows NT 6.1; Win64; rv:62.0) Gecko/20100101',
	referer = NULL
)
{
	if( requireNamespace('curl', quietly = TRUE) ) {
	
	if( !is.null(useragent) ) curl::handle_setopt(h, useragent = useragent)
	if( !is.null(referer) ) curl::handle_setopt(h, referer=referer)

	req = curl::curl_fetch_memory(url, h)
	if(req$status_code != 200) 
		warning('error getting data, status_code:', req$status_code, 'for url:', url, 'content:', rawToChar(req$content))
	
    txt = rawToChar(req$content)
	txt
	} else
		warning('"curl" package could not be loaded')
}
