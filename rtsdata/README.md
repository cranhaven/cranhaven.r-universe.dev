rtsdata
====


Efficient Data Storage system for R Time Series.
===


The `rtsdata` package simplifies the management of Time Series in R. This package 
overwrites the `getSymbols` function from `quantmod` package to allow for minimal code changes
to get started. The `rtsdata` package provides functionality to **download** and **store** historical time series. 

The **download** functionality will intelligently update historical data as needed.
The incremental data is downloaded first to updated historical data. The full 
history is **only** downloaded if incremental data is not consistent. I.e.
the last saved record is different from the first downloaded record.

The following download plugins are currently available:

* Yahoo Finance - based on `quantmod` package.
* FRED - based on `alfred` package.
* Quandl - based on `Quandl` package.
	Quandl recommends getting an API key.
	Add following code options(Quandl.api_key = api_key)  to your .Rprofile file.
* AlphaVantage(av) - based on `quantmod` package.
	You need an API key from www.alphavantage.co.
	Add following code options(getSymbols.av.Default = api_key) to your .Rprofile file.
* Tiingo - based on `quantmod` package
	You need an API key from api.tiingo.com.
	Add following code options(getSymbols.av.Default = api_key) to your .Rprofile file.

The download functionality plugins are easily created. The user needs to provide a 
function to download historical data with ticker, start, and end dates parameters 
to create new download plugin.




The **storage** functionality provides a consistent interface to store historical time series.  
The following storage plugins are currently available:

* Rdata - store historical time series data in the Rdata files.
* CSV - store historical time series data in the CSV files. The CSV storage is not 
	efficient because CSV files will have to be parsed every time the data is loaded.
	The advantage of this format is ease of access to the stored historical data by external programs.
	For example the CSV files can be opened in Notepad or Excel.
* MongoDB - store historical time series data in the MongoDB GridFS system. The MongoDB
	storage provides optional authentication.


The storage functionality plugins are easily created. The user needs to provide 
a functions to load and save data to create new storage plugin.


Installation:
===

The current release is available on [CRAN](https://CRAN.R-project.org/package=rtsdata),
which you can install via:

```r
install.packages("rtsdata")
```

To install the development version run following code:

```R
remotes::install_bitbucket("rtsvizteam/rtsdata")
```
	

Example : Basic usage
===

```R
	# load `rtsdata` package
	library(rtsdata)

	# tickers to load data
	env = new.env()
	Symbols = c('spy','aapl','ibm')
	
	# download data
	getSymbols(Symbols, env, src = 'yahoo', from = '2018-01-01', to = '2018-02-13', verbose=TRUE)

	print(env$SPY)

	# update data - only the missing, recent, data is downloaded
	getSymbols(Symbols, env, src = 'yahoo', from = '2018-01-01', verbose=TRUE)

	print(env$SPY)

	# data is stored in the 'yahoo_Rdata' folder at the following location
	# defaults to the temp directory
	#
	# you can overwrite default location by setting 'RTSDATA_FOLDER' option
	#	
	# good practice is not to store this setting inside the script files,
	# for example, add options(RTSDATA_FOLDER='C:/Data') line to the .Rprofile to 
	# use 'C:/Data' folder.
	ds.default.location()
	
	
	# the historical data is currently starts in 2018 because we initially set from = '2018-01-01'
	getSymbols(Symbols, env, src = 'yahoo', verbose=TRUE)
	print(env$SPY[1])

	# set full.update flag to force full data update
	getSymbols(Symbols, env, src = 'yahoo', full.update = TRUE, verbose=TRUE)
	print(env$SPY[1])
```


Example : use CSV storage
===

```R
	# load `rtsdata` package
	library(rtsdata)

	# tickers to load data
	env = new.env()
	Symbols = c('spy','aapl','ibm')

	# change the 'yahoo' data source to use CSV files to store historical data
	# data is stored in the 'yahoo_csv' folder
	register.data.source(src = 'yahoo', storage = ds.storage.file.csv())

	getSymbols(Symbols, env, src = 'yahoo', from = '2018-01-01')
	
	print(env$SPY)

	# CSV files are stored in the 'yahoo_csv' folder at the following location	
	ds.default.location()
```



Example : use external data in CSV format
===
Suppose there is an external stock downloader that stores data at the 'C:/Data/stocks' folder in the the CSV format. The updates are done by the external stock downloader.

```R
	# load `rtsdata` package
	library(rtsdata)

	# tickers to load data
	env = new.env()
	Symbols = c('spy','aapl','ibm')

	# change the 'yahoo' data source to use CSV files to store historical data
	# data is stored in the 'C:/Data/stocks' folder 
	# and disable check for updates
	register.data.source(src = 'custom', 
		storage = ds.storage.file.csv('C:/Data/stocks', custom.folder = TRUE),
		functionality = ds.functionality.default(check.update = FALSE)
	)

	getSymbols(Symbols, env, src = 'custom', from = '2018-01-01')
	
	print(env$SPY)
```


	
Example : use holiday calendar to skip checking for data updates on holidays
===

```R
	# load `rtsdata` package
	library(rtsdata)

	# tickers to load data
	env = new.env()
	Symbols = c('spy','aapl','ibm')

	
	# The `RQuantLib` package must be available for this functionality
	# please specify `RQuantLib`'s holiday calendar
	getSymbols(Symbols, env, src = 'yahoo', from = '2018-01-01', calendar = 'UnitedStates/NYSE')
	
	print(env$SPY)
```
	


Example : get data from FRED
===

```R
	# load `rtsdata` package
	library(rtsdata)

	# tickers to load data
	env = new.env()
	Symbols = 'DTB3'

	# get data from FRED
	# data is stored in the 'FRED_Rdata' folder
	getSymbols(Symbols, env, src = 'FRED', from = '2018-01-01')

	print(env$DTB3)
```



Example : use MongoDB storage
===

If you do not have MongoDB installed, the good tutorial to start using MongoDB on Windows: [Install, setup and start MongoDB on Windows](https://yitdeveloper.wordpress.com/2014/06/19/install-setup-and-start-mongodb-on-windows/)
	
* Create database folder
	mkdir c:\mongodb\data
* Start MongoDB server	
	`mongod.exe --dbpath "c:\mongodb\data"`
* Test MongoDB setup
	`mongo.exe`

```
	show dbs
	# In the clean install, you expected to see 
	# admin  0.000GB
	# local  0.000GB

	use data_storage
	show collections
```



```R
	# load `rtsdata` package
	library(rtsdata)

	# tickers to load data
	env = new.env()
	Symbols = c('spy','aapl','ibm')
	
	
	# data is stored in the 'data_storage' database at the following location
	# defaults to the 'mongodb://localhost' URI
	#
	# you can overwrite default location by setting 'RTSDATA_DB' option
	#		
	# good practice is not to store this setting inside the script files. 
	# add options(RTSDATA_DB='mongodb://localhost') line to the .Rprofile to use 'mongodb://localhost' URI.
	

	
	# change the 'yahoo' data source to use MongoDB to store historical data
	register.data.source(src = 'yahoo', storage = ds.storage.database())
	
	# download data and save in MongoDB
	getSymbols(Symbols, env, src = 'yahoo', from = '2018-01-01')

	print(env$SPY)
```



Example : use MongoDB storage with authentication
===

It is a good idea to secure your database. Sample steps to add authentication to MongoDB:

* Connect to MongoDB
	`mongo.exe`
```	
	# For example, create a superuser with username 'user12' and password 'secret12'
	use admin
	db.createUser({user:"user12",pwd:"secret12", roles:[{role:"root",db:"admin"}]})
```
	
* Re-start MongoDB server with authentication
	`mongod.exe --auth --dbpath "c:\mongodb\data"`

* Test MongoDB setup
	`mongo.exe -u "user12" -p "secret12" --authenticationDatabase "admin"`

```
	show dbs
	# In the clean install, you expected to see 
	# admin  0.000GB
	# data_storage  0.000GB
	# local  0.000GB
	
	use data_storage
	show collections
```


```R	
	# load `rtsdata` package
	library(rtsdata)

	# tickers to load data
	env = new.env()
	Symbols = c('spy','aapl','ibm')


	# change the 'yahoo' data source to use MongoDB to store historical data
	register.data.source(src = 'yahoo', storage = ds.storage.database('mongodb://user12:secret12@localhost'))

	# download data and save in MongoDB
	getSymbols(Symbols, env, src = 'yahoo', from = '2018-01-01', to = '2018-02-13')

	print(env$SPY)
	
	# update data - only the missing, recent, data is downloaded
	getSymbols(Symbols, env, src = 'yahoo', from = '2018-01-01')
	
	print(env$SPY)	
```	
	
