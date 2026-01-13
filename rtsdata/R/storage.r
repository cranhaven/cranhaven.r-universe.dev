###############################################################################
# Storage Models
#
# Define data storage models. I.e. file or database / Rdata / csv.
#
# All models follow the same interface: 
# - load function = load data for given ticker
# - save function = save data for given ticker
# - location = folder / collection where data is saved
# - exists function = check if data is saved for given ticker
# - extension = file extension
# - ticker function = construct data filename for given ticker
#
# In the code, the s variable stands for storage model; t variable stands for ticker.
###############################################################################


###############################################################################
#' Default location to save data
#'
#' The following waterfall is used to lookup the default location: 
#' 1. options
#' 2. environment
#' 3. default option
#' 
#' Good practice is not to store this setting inside the script files. 
#' Add options(RTSDATA_FOLDER='C:/Data') line to the .Rprofile to use 'C:/Data' folder.
#'
#' @return default location to save data
#'
#' @examples
#' 	# Default location to save data
#' 	ds.default.location()
#'
#' @export
#' @rdname DefaultStorageOptions
###############################################################################
ds.default.location = function() {
	# location for Rdata files
	#folder = system.file("extdata", package="rtsdata")
	#if(file.access(folder, mode = 2) == -1){
		## No access to the root folder by this user, take tempfolder - will not persist across R sessions
		#folder = tempdir()
	#	folder = getwd()
	#}	

	
	# Location for data files
	# use 'RTSDATA_FOLDER' folder if provided
	# otherwise use tempfolder - will not persist
	#get.default.option( 'RTSDATA_FOLDER', file.path(getwd(), 'data') )
	#get.default.option( 'RTSDATA_FOLDER', file.path(dirname(tempdir()), 'data') )
	get.default.option( 'RTSDATA_FOLDER', tempdir() )
}


###############################################################################
#' Default database to save data
#'
#' Good practice is not to store this setting inside the script files. 
#' Add options(RTSDATA_DB='mongodb://localhost') line to the .Rprofile to use 'mongodb://localhost' database.
#'
#' @return default database to save data
#'
#' @export
#' @rdname DefaultStorageOptions
###############################################################################
ds.default.database = function() {
	url = 'mongodb://localhost'
	get.default.option('RTSDATA_DB', url)
}


###############################################################################
#' Rdata file Storage model
#'
#' @param location storage location, \strong{defaults to ds.default.location folder}
#' @param extension file extension, \strong{defaults to 'Rdata'}
#' @param custom.folder custom folder flag, \strong{defaults to False}
#' 	if flag is False \strong{default}, the data is stored at the \code{"location\src_extnsion"} folder.
#'	if flag is True, the data is stored at the \strong{location} folder.
#'
#' @return list with storage options
#'
#' @examples
#'  # change the 'yahoo' data source to use Rdata files to store historical data
#'  register.data.source(src = 'yahoo', storage = ds.storage.file.rdata())
#'
#' @export
###############################################################################
ds.storage.file.rdata = function
(
	location = ds.default.location(),
	extension = 'Rdata',
	custom.folder = FALSE
)
	list(
		load = function(t, s) { 
			ds.data = NULL; 
			# original - load(s$ticker(t,s)); return(ds.data);
			filename = s$ticker(t,s)
			tryCatch({
				temp = readBin(filename, raw(), file.info(filename)$size)
				unserialize(brotli::brotli_decompress( temp ))
			}, error=function(e) {load(filename); ds.data})
		},
		save = function(ds.data, t, s) {
			# original - save(ds.data, file=s$ticker(t,s)); return();			
			filename = s$ticker(t,s)
			writeBin(brotli::brotli_compress(serialize(ds.data, NULL), 2), filename);
		},
		exists = function(t, s) ds.storage.file.exists(t,s),
		ticker = function(t, s) ds.storage.file.ticker(t,s),
		
		# called once storage is registered
		setup = function(src, s) setup.location(src, s),
		
		location = location,
		extension = extension,
		custom.folder = custom.folder
	)

	
#' File with historical data for given ticker
#'
#' @param t ticker
#' @param s storage model
#'
#' @return filename with historical data for given ticker
#'
#' @examples
#'  ds.storage.file.ticker('dummy', ds.storage.file.rdata())
#'
#' @export
ds.storage.file.ticker = function(t,s) file.path(s$location, paste0(t, '.', s$extension))
	

#' Check if file exists with historical data for given ticker
#'
#' @param t ticker
#' @param s storage model
#'
#' @return boolean indicating if file exists with historical data for given ticker
#'
#' @examples
#'  ds.storage.file.exists('dummy', ds.storage.file.rdata())
#'
#' @export
ds.storage.file.exists = function(t,s) {
	if(!dir.exists(s$location)) dir.create(s$location, TRUE, TRUE) 
	file.exists(s$ticker(t,s)) 
}

	
# internal function to setup location and create storage folder
setup.location = function(src, s) {
	# setup location
	if(!s$custom.folder) s$location = file.path(s$location, paste0(src, '_', s$extension)) 
	s
}


###############################################################################
#' CSV file Storage model
#'
#' @param location storage location, \strong{defaults to ds.default.location folder}
#' @param extension file extension, \strong{defaults to 'csv'}
#' @param date.format date format, \strong{defaults to "\%Y-\%m-\%d"}
#'  use \strong{"\%Y-\%m-\%d \%H:\%M:\%S"} for storing intra day data
#' @param custom.folder custom folder flag, \strong{defaults to False}
#' 	if flag is False \strong{default}, the data is stored at the \code{"location\src_extnsion"} folder.
#'	if flag is True, the data is stored at the \strong{location} folder.
#'
#' @return list with storage options
#'
#' @examples
#'  # change the 'yahoo' data source to use CSV files to store historical data
#'  register.data.source(src = 'yahoo', storage = ds.storage.file.csv())
#'
#' @export
###############################################################################
ds.storage.file.csv = function
(
	location = ds.default.location(),
	extension = 'csv',
	date.format = '%Y-%m-%d',
	custom.folder = FALSE	
)
	list(
		load = function(t, s) ds.storage.file.csv.load(s$ticker(t,s), date.format = date.format),
		save = function(ds.data, t, s) ds.storage.file.csv.save(ds.data, s$ticker(t,s), date.format),
		exists = function(t, s) ds.storage.file.exists(t,s),
		ticker = function(t, s) ds.storage.file.ticker(t,s),		
		
		# called once storage is registered
		setup = function(src, s) setup.location(src, s),
		
		location = location,
		extension = extension,
		custom.folder = custom.folder
	)
	

#' Load data from CSV file into `xts` object
#'
#' @param file CSV file
#' @param date.col date column
#' @param date.format date format
#'
#' @return xts object with loaded data
#'
#' @examples
#'  # get sample of the fake stock data
#'  data = ds.getSymbol.fake.stock.data('dummy', from = '2018-02-01', to = '2018-02-13')
#'  filename = file.path(tempdir(), 'dummy.csv')
#'  ds.storage.file.csv.save(data, filename)
#'  ds.storage.file.csv.load(filename)
#'
#' @export
ds.storage.file.csv.load = function
(
	file, 
	date.col=NULL,
	date.format = '%Y-%m-%d'
) {
	data = ds.load.csv(file)			
	date.col = ifnull(date.col, colnames(data)[1])
			
	data.table::set(data, j=date.col, value=as.POSIXct(strptime(data[[date.col]], date.format)))
	data.table::as.xts.data.table(data)
}


#' Save `xts` object into CSV file
#'
#' @param ds.data `xts` object
#' @param file filename to save `xts` object
#' @param date.format date format
#'
#' @return nothing
#'
#' @examples
#'  # get sample of the fake stock data
#'  data = ds.getSymbol.fake.stock.data('dummy', from = '2018-02-01', to = '2018-02-13')
#'  filename = file.path(tempdir(), 'dummy.csv')
#'  ds.storage.file.csv.save(data, filename)
#'
#' @export
ds.storage.file.csv.save = function
(
	ds.data,
	file, 
	date.format = '%Y-%m-%d'
) {
	cat('Date', file = file, append = FALSE)	
	suppressWarnings(utils::write.table(ds.data, sep=',',  
		row.names = format(indexts(ds.data), date.format), 
		col.names = NA, file = file, append = TRUE, quote = FALSE))
}


###############################################################################
#' MongoDB GridFS Storage model
#'
#' @param url address of the mongodb server in mongo connection string URI format, 
#' \strong{defaults to ds.default.database database}. 
#' 
#' For local mongodb server, use 'mongodb://localhost' URI.
#' For local authenticated mongodb server, use 'mongodb://user:password@localhost' URI.
#' @param db name of database, \strong{defaults to 'data_storage'}
#'
#' @return list with storage options
#'
#' @examples
#'  \donttest{ 
#'   # change the 'yahoo' data source to use MongoDB to store historical data
#'   # register.data.source(src = 'yahoo', storage = ds.storage.database())
#'  }
#'
#' @export
###############################################################################
ds.storage.database = function
(
	url = ds.default.database(),
	db = 'data_storage'	
) {
	# gridfs functionality is available since mongolite 1.5.9 version
	if (utils::packageVersion('mongolite') < '1.5.9')
		stop('mongolite >= 1.5.9 needed for this function.', call. = FALSE)

	# connect to db; if db is not setup, mongo will create empty one
	fs = mongolite::gridfs(db = db, url = url)
	
	# check saved files and remove all duplicates
	index = fs$find()$name
	index = index[duplicated(index)]
	
	# remove all duplicates
	if( len(index) > 0 )
		for(i in index)
			fs$remove(i)
	
	# MongoDB storage definition
	list(
		fs = fs,
		load = function(t, s) {
			# original - unserialize(s$fs$read(s$ticker(t,s))$data),
			temp = s$fs$read(s$ticker(t,s))$data
			tryCatch(unserialize(brotli::brotli_decompress(temp)), error=function(e) unserialize(temp))
		},
		save = function(ds.data, t, s) {
			i = s$ticker(t,s)	
			# the only way to update data is to remove the old file
			try(s$fs$remove(i), silent = TRUE)
			# original - s$fs$write(serialize(ds.data, NULL), i)
			s$fs$write(brotli::brotli_compress(serialize(ds.data, NULL), 2), i)
		},
		exists = function(t, s) sum(s$fs$find()$name == s$ticker(t,s)) > 0,
		ticker = function(t, s) paste0(s$location, '|', t),

		# called once storage is registered
		setup = function(src, s) { s$location = src; s },
		
		location = ''		
	)
}
