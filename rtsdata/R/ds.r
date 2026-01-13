###############################################################################
# Efficient Data Storage system
#
# Download only new data when possible instead of downloading
# complete history. The historical data can be stored in the Rdata files or
# mongo database.
#
# The mongo database functionality is not developed yet.
###############################################################################


###############################################################################
# Query or set package options
# @param value Optionally passed parameter to set package options
# @return Returns invisibly the currently set package options
###############################################################################
#ds.storage.mode <- function(value) {
#	if (!missing(value))
#		set.options('rtsdata.options', storage.mode = value)
#	invisible(options()$rtsdata.options$storage.mode)
#}





###############################################################################
#' Download historical data
#'
#' Overwrite the getSymbols function from 'quantmod' package to efficiently 
#' load historical data
#'
#' @param Symbols list symbols to download historical data
#' @param env environment to hold historical data, \strong{defaults to parent.frame()}
#' @param reload.Symbols flag, not used, inherited from the getSymbols function from 'quantmod' package, \strong{defaults to FALSE}
#' @param verbose flag, inherited from the getSymbols function from 'quantmod' package, \strong{defaults to FALSE}
#' @param warnings flag, not used, inherited from the getSymbols function from 'quantmod' package, \strong{defaults to TRUE}
#' @param src source of historical data, \strong{defaults to 'yahoo'}
#' @param symbol.lookup flag, not used, inherited from the getSymbols function from 'quantmod' package, \strong{defaults to TRUE}
#' @param auto.assign flag to store data in the given environment, \strong{defaults to TRUE}
#'
#' @param from start date, expected in yyyy-mm-dd format, \strong{defaults to 1900-01-01}
#' @param to end date, expected in yyyy-mm-dd format, \strong{defaults to today's date}
#'
#' @param calendar RQuantLib's holiday calendar, for example: calendar = 'UnitedStates/NYSE', \strong{defaults to NULL}
#' @param check.update flag to check for updates, \strong{defaults to NULL}
#' @param full.update flag to force full update, \strong{defaults to NULL}
#'
#' @return xts object with data
#'
#' @examples
#'  # small toy example
#'
#'  # register data source to generate fake stock data for use in rtsdata examples
#'  register.data.source(src = 'sample', data = ds.getSymbol.fake.stock.data)
#'  
#'  # Full Update till '2018-02-13'
#'  data = getSymbols('test', src = 'sample', from = '2018-01-01', to = '2018-02-13', 
#'						auto.assign=FALSE, verbose=TRUE)
#'  
#'  # No updated needed, data is loaded from file
#'  data = getSymbols('test', src = 'sample', from = '2018-01-01', to = '2018-02-13', 
#'						auto.assign=FALSE, verbose=TRUE)
#' 
#'  # Incremental update from '2018-02-13' till today
#'  data = getSymbols('test', src = 'sample', from = '2018-01-01', 
#'						auto.assign=FALSE, verbose=TRUE)
#'
#'  # No updated needed, data is loaded from file
#'  data = getSymbols('test', src = 'sample', from = '2018-01-01', 
#'						auto.assign=FALSE, verbose=TRUE)
#'
#' 	# data is stored in the 'sample_Rdata' folder at the following location
#' 	ds.default.location()
#'  
#'  \donttest{ 
#'  ds.getSymbol.yahoo('AAPL',from='2018-02-13')
#'  }
#' 
#' @export 
###############################################################################
getSymbols = function 
(
	# general parameters
	Symbols = NULL, 
	env=parent.frame(), 
	reload.Symbols = FALSE,
    verbose = FALSE, 
	warnings = TRUE, 
	src = 'yahoo', 
	symbol.lookup = TRUE,
    auto.assign = TRUE, 
	from = '1990-01-01', 
	to = Sys.time(), 
	
	# additional parameters
	calendar = NULL,
	check.update = NULL,
	full.update = NULL
) {
	Symbols = spl(Symbols)
	#from = to.date(from)
	#to = to.date(to)
	# date.range = paste(format(from,'%Y%m%d'),'::',format(to,'%Y%m%d'),sep='')
		
	# make sure that given source is avalible in the registered data sources
	data.source = data.sources()[[src]]
	if(is.null(data.source)) stop('Data source:', src, ' is not found. Following are registered data sources:', names(data.sources()))
	
	data = ds.getSymbols(Symbols, from, to, env, calendar, check.update, full.update, data.source, verbose)
	
	if (!auto.assign) return(data)
	
	invisible()
}



# load and update data
ds.getSymbols = function
(
	Symbols, 
	from, 
	to, 
	env, 
	
	calendar,
	check.update,
	full.update,
	ds,
	verbose	
) {
	# setup
	fn.download = ds$data	
	check.update = ifnull(check.update, ds$functionality$check.update)
	force.full.update = ifnull(full.update, FALSE)
	fn.update.required = ds$functionality$update.required
	ds = ds$storage	
	
	# no updates if download function is not provided
	if(is.null(fn.download)) check.update = FALSE

	# check internet
	if(check.update)
		if(!curl::has_internet())
			stop('No internet connection.')	 
	
	# date range
	date.range = paste0(from, '::', to)
	
	# loop over each symbol
	for (ticker in Symbols) {
		ticker.raw = trim(toupper(ticker))
		# remove symbols cannot be part of filename
		ticker = trim(gsub('/','_', gsub('\\^', '', ticker.raw)))
	
		full.update.required = check.update
			
		if(!force.full.update && ds$exists(ticker, ds)) {
			# ds.data / xtsAttributes(ds.data)$stamp - time stamp of last update
			ds.data = ds$load(ticker, ds)
						
			if(check.update) {
				ds.stamp = ifnull( xtsAttributes(ds.data)$stamp, indexts(mlast(ds.data)) )
				
				if(!fn.update.required(ds.stamp, to, calendar)) {
					full.update.required = FALSE
				} else if(!ds.getSymbols.empty(ds.data)) {
					ds.stamp = indexts(mlast(ds.data))
					if(verbose) cat('***Incremental Update for', ticker.raw, '\n\n')
					temp = ds.getSymbol.load(fn.download, ticker.raw, ds.stamp, to)
					
					if(is.null(temp)) {
						full.update.required = FALSE
						xtsAttributes(ds.data) = list(stamp = to)
						ds$save(ds.data, ticker, ds)						
					} else {
						# last record is different from first update record, do full update
						if(first2str(temp) == last2str(ds.data)) {
							full.update.required = FALSE
							ds.data = rbind(ds.data, temp[-1,,drop=FALSE])
							
							xtsAttributes(ds.data) = list(stamp = to)
							ds$save(ds.data, ticker, ds)
						}									
					}
				}
			}
			# if null ds.data, do full update
			if(!ds.getSymbols.empty(ds.data)) env[[ ticker.raw ]] = ds.data[date.range]
		} 
		
		
		# do full update if
		# 1. file does not exists or
		# 2. last stored record is different from first update record or
		# 3. save data is null and update is required
		if(force.full.update || full.update.required) {
			if(verbose) cat('***Full Update for', ticker.raw, '\n\n')
			temp = ds.getSymbol.load(fn.download, ticker.raw, from, to)
			
			if(is.null(temp)) {
				ds.data = xts(NA, order.by = Sys.time())
			} else {
				ds.data = temp
			}
			
			xtsAttributes(ds.data) = list(stamp = to)
			ds$save(ds.data, ticker, ds)
			
			if(!ds.getSymbols.empty(ds.data)) env[[ ticker.raw ]] = ds.data[date.range]			
		}
	}
	env[[ ticker.raw ]]
}


ds.getSymbols.empty = function(x) is.null(x) | is.null(nrow(x)) | len(x)==0 | (len(x)==1 & is.na(x)[1])



# wrapper for the load function to capture errors
ds.getSymbol.load = function(fn, Symbol, from = '1900-01-01', to = Sys.time())
	tryCatch({
		# download
		out = fn(Symbol, from, to)
		
		# remove any duplicates
		out = out[!duplicated(indexts(out)),,drop=FALSE]
		
		# date range
		date.range = paste0(from, '::', to)
		out[date.range]	
	}, error = function(ex) {
		utils::str(ex)
		cat('Error:\n\t', as.character(ex), '\n')
		NULL
	})			



###############################################################################
# Internal function to check saved data in Rdata format
###############################################################################
ds.test.load = function(
	ticker = 'IBM',
	src = 'yahoo' 
){
	ds = data.sources()[[src]]$storage
	filename = ds$ticker(ticker, ds)
	
	ds.data = NULL
	load(filename)	
	ds.stamp = xtsAttributes(ds.data)$stamp
	
	cat('ticker:', ticker, '\n')
	cat('filename:', filename, '\n')
	cat('ds.stamp:', format(ds.stamp,'%Y-%m-%d'), '\n')
	print(mlast(ds.data))
	ds.data
}
