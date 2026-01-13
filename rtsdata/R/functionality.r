###############################################################################
# Functionality
#
# Define **custom** functionality to process data.
#
# Currently only following options are supported:
# - check.update flag to check for updates, \strong{defaults to TRUE}
###############################################################################


###############################################################################
#' Default Functionality
#'
#' Default functionality configuration
#'
#' @param check.update flag to check for updates, \strong{defaults to TRUE}
#' @param update.required.fn function to check if update is required given stored historical data, 
#'  \strong{defaults to update.required}. The update.required function takes 
#'  last update stamp, current date/time, holiday calendar name.
#' 
#' @return list with options
#'
#' @examples
#'  # disable check for updates for the 'yahoo' data source
#'  register.data.source(src = 'yahoo', functionality = ds.functionality.default(FALSE))
#'
#' @export
#' @rdname Functionality
###############################################################################
ds.functionality.default = function
(
	check.update = TRUE,
	update.required.fn = update.required
)
	list(check.update = check.update, update.required = update.required.fn)


	
	
###############################################################################
# Internal function to check if data needs updating
# The function is build to work with daily data only
#
# Optionally can provide holiday calendar. For example RQuantLib's calendars:
# - calendar = 'UnitedStates/NYSE'
# - calendar = 'Canada/TSX'
###############################################################################
update.required = function
(
	stamp,
	current = Sys.time(),
	calendar = NULL	
) {
	stamp = to.date(stamp)
	current = to.date(current)
	
	dayofweek = as.POSIXlt(current)$wday
	   	
	date.lag = 1
	
	# Sunday is 0
	if(dayofweek == 0) date.lag = 2
	
	# Monday is 1
	if(dayofweek == 1) date.lag = 3

	update = (current - stamp) > date.lag
	
	# getHolidayList
	if(update)
		if(!is.null(calendar))
			if(requireNamespace('RQuantLib', quietly=TRUE)) {
				holidays = RQuantLib::getHolidayList(calendar, stamp, current) 	
				if(!is.null(holidays)) date.lag = date.lag + len(holidays)
				update = (current - stamp) > date.lag
			} else
				warning('"RQuantLib" package could not be loaded')
		   	
	update
}	

