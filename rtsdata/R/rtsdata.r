#' @title `rtsdata` - Efficient Data Storage system for R Time Series.
#' 
#' @description The `rtsdata` package simplifies the management of Time Series in R. This package 
#' overwrites the `getSymbols` function from `quantmod` package to allow for minimal changes
#' to get started. The `rtsdata` package provides functionality to **download** and **store** historical time series. 
#' 
#' The **download** functionality will intelligently update historical data as needed.
#' The incremental data is downloaded first to updated historical data. The full 
#' history is **only** downloaded if incremental data is not consistent. I.e.
#' the last saved record is different from the first downloaded record.
#' 
#' The following download plugins are currently available:
#' * Yahoo Finance - based on `quantmod` package.
#' * FRED - based on `quantmod` package.
#' * Quandl - based on `Quandl` package.
#' 	Quandl recommends getting an API key.
#' 	Add following code options(Quandl.api_key = api_key)  to your .Rprofile file.
#' * AlphaVantage(av) - based on `quantmod` package.
#' 	You need an API key from www.alphavantage.co.
#' 	Add following code options(getSymbols.av.Default = api_key) to your .Rprofile file.
#' * Tiingo - based on `quantmod` package
#' 	You need an API key from api.tiingo.com.
#' 	Add following code options(getSymbols.av.Default = api_key) to your .Rprofile file.
#' 
#' The download functionality plugins are easily created. The user needs to provide a 
#' function to download historical data with ticker, start, and end dates parameters 
#' to create new download plugin.
#' 
#' The **storage** functionality provides a consistent interface to store historical time series.  
#' The following storage plugins are currently available:
#' * Rdata - store historical time series data in the Rdata files.
#' * CSV - store historical time series data in the CSV files. The CSV storage is not 
#' 	efficient because CSV files will have to be parsed every time the data is loaded.
#' 	The advantage of this format is ease of access to the stored historical data by external programs.
#' 	For example the CSV files can be opened in Notepad or Excel.
#' * MongoDB - store historical time series data in the MongoDB GridFS system. The MongoDB
#' 	storage provides optional authentication. The MongoDB storage functionality is currently only 
#' 	available in the development version at bitbucket.
#' 
#' The storage functionality plugins are easily created. The user needs to provide 
#' a functions to load and save data to create new storage plugin.
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
#' 
#' @import xts
#' 
#' @name rtsdata
#' @docType package
#' 
"_PACKAGE"
