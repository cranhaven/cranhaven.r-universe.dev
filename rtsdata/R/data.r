###############################################################################
# Data Sources
#
# Functions to download historical data for supported Data Sources.
#
# All functions follow the same interface: download historical data given:
# - Symbol, 
# - start date (from)
# - end date (to)
###############################################################################


###############################################################################
#' Data Sources
#'
#' List available data sources and Register new ones
#'
#' @param src data source name, \strong{defaults to 'yahoo'}
#' @param data data source to download historical data, function must take Symbol, from, to parameters, \strong{defaults to ds.getSymbol.yahoo}
#' @param storage storage model configuration, \strong{defaults to ds.storage.file.rdata(src)}
#' @param functionality functionality configuration, \strong{defaults to ds.functionality.default()}
#' @param overwrite flag to overwrite data source if already registered in the list of plugins, \strong{defaults to True}
#'
#' @return None
#'
#' @examples
#'  # register data source to generate fake stock data for use in rtsdata examples
#'  register.data.source(src = 'sample', data = ds.getSymbol.fake.stock.data)
#'
#'  # print allregistered data sources
#'  names(data.sources())
#'
#' @export
#' @rdname DataSources
###############################################################################
register.data.source = function
(
	src = 'yahoo',
	data = ds.getSymbol.yahoo,
	storage = ds.storage.file.rdata(),
	functionality = ds.functionality.default(),
	overwrite = TRUE
)
	set.options('rtsdata.plugins', make.list(src, 
		list(
			data = data, 
			storage = storage$setup(src, storage), 
			functionality = functionality 
		)
	), overwrite = overwrite)


#' @export
#' @rdname DataSources
data.sources = function() ifnull(options()$rtsdata.plugins, list())


###############################################################################
#' Get quotes from Yahoo Finance
#'
#' Download historical data from Yahoo Finance using `getSymbols.yahoo` function from `quantmod` package.
#'
#' @param Symbol symbol
#' @param from start date, expected in yyyy-mm-dd format, \strong{defaults to 1900-01-01}
#' @param to end date, expected in yyyy-mm-dd format, \strong{defaults to today's date}
#'
#' @return xts object with data
#'
#' @export
#' @rdname DataSource
###############################################################################
ds.getSymbol.yahoo = function
(
	Symbol, 
	from = '1900-01-01',
	to = Sys.Date()
){
	from = to.date(from)
	to = to.date(to)
	if( requireNamespace('quantmod', quietly = TRUE) )
		suppressWarnings(quantmod::getSymbols(Symbol, src='yahoo', from=from, to=to, auto.assign=FALSE))
	else
		warning('"quantmod" package could not be loaded')
}


###############################################################################
#' Get quotes from FRED
#'
#' Download historical data from FRED using `get_fred_series` function from `alfred` package.
#'
#' @export 
#' @rdname DataSource
###############################################################################
ds.getSymbol.FRED = function
(
	Symbol, 
	from = '1900-01-01',
	to = Sys.Date()
){
	from = to.date(from)
	to = to.date(to)
	if( requireNamespace('quantmod', quietly = TRUE) ) {
		suppressWarnings(quantmod::getSymbols(Symbol, src='FRED', from=from, to=to, auto.assign=FALSE)[paste0(from,'::',to)])
	} else
		warning('"quantmod" package could not be loaded')
}


###############################################################################
#' Get quotes from Quandl
#'
#' Download historical data from Quandl using `Quandl` function from `Quandl` package.
#'
#' Quandl recommends getting an API key
#' Add following code options(Quandl.api_key = api_key)  to your .Rprofile file
#'
#' @export 
#' @rdname DataSource
###############################################################################
ds.getSymbol.Quandl = function
(
	Symbol, 
	from = '1900-01-01',
	to = Sys.Date()
){
	from = to.date(from)
	to = to.date(to)
	if( requireNamespace('Quandl', quietly = TRUE) )
		suppressWarnings(Quandl::Quandl(Symbol, type='xts', start_date=from, end_date = to))
	else
		warning('"Quandl" package could not be loaded')
}


###############################################################################
#' Get quotes from AlphaVantage
#'
#' Download historical data from AlphaVantage using `getSymbols.av` function from `quantmod` package.
#'
#' You need an API key from www.alphavantage.co
#' Add following code options(getSymbols.av.Default = api_key) to your .Rprofile file
#'
#' @export 
#' @rdname DataSource
###############################################################################
ds.getSymbol.av = function
(
	Symbol, 
	from = '1900-01-01',
	to = Sys.Date()
){
	from = to.date(from)
	to = to.date(to)
	if( requireNamespace('quantmod', quietly = TRUE) )
		suppressWarnings(quantmod::getSymbols(Symbol, src='av', from=from, to=to, auto.assign=FALSE))
	else
		warning('"quantmod" package could not be loaded')
}


###############################################################################
#' Get quotes from Tiingo
#'
#' Download historical data from Tiingo using `getSymbols.tiingo` function from `quantmod` package.
#'
#' You need an API key from api.tiingo.com
#' Add following code options(getSymbols.av.Default = api_key) to your .Rprofile file
#'
#' @export 
#' @rdname DataSource
###############################################################################
ds.getSymbol.tiingo = function
(
	Symbol, 
	from = '1900-01-01',
	to = Sys.Date()
){
	from = to.date(from)
	to = to.date(to)
	if( requireNamespace('quantmod', quietly = TRUE) )
		suppressWarnings(quantmod::getSymbols(Symbol, src='tiingo', from=from, to=to, auto.assign=FALSE))
	else
		warning('"quantmod" package could not be loaded')
}


###############################################################################
#' Generate fake stock data
#'
#' Generate fake stock data for use in rtsdata examples
#'
#' @examples
#'  # get sample of the fake stock data
#'  ds.getSymbol.fake.stock.data('dummy', from = '2018-02-01', to = '2018-02-13')
#'
#' @export 
#' @rdname DataSource
###############################################################################
ds.getSymbol.fake.stock.data = function
(
	Symbol, 
	from = '1900-01-01',
	to = Sys.Date()
){
	from = anytime::anydate(from); to = anytime::anydate(to)
	x =  seq(from, to, by = 'day')
	set.seed(42) # make sure the fake stock data is persistent, Date(1970-01-01) is mapped to 0
	y = 10 + cumsum( stats::rnorm(to, sd = 0.1) )
	xts::xts(y[from:to], x)
}
