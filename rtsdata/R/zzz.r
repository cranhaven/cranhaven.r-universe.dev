.onLoad <- function(libname, pkgname) {
	# disable quantmod warning messages
	options("getSymbols.warning4.0"=FALSE)
	options("getSymbols.yahoo.warning"=FALSE)	
	
	# register data sources, do not overwrite if already registered
	register.data.source('yahoo', ds.getSymbol.yahoo, overwrite=FALSE)
	register.data.source('FRED', ds.getSymbol.FRED, overwrite=FALSE)
	register.data.source('Quandl', ds.getSymbol.Quandl, overwrite=FALSE)
	register.data.source('av', ds.getSymbol.av, overwrite=FALSE)
	register.data.source('tiingo', ds.getSymbol.tiingo, overwrite=FALSE)

	# options
	#if( is.null(options()$rtsdata.options) ) options(rtsdata.options = list())
	#set.options('rtsdata.options',
	#	storage.mode = 'file', # 'database'
	#	storage.location = ds.default.location() # 'dbname	
	#)	
	
	invisible()
}


.onUnload <- function(libpath) {
	gc() # Force garbage collection of connections
}