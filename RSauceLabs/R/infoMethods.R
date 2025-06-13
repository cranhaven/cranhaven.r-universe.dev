#' Get Sauce Labs Status
#'
#' Get the current status of Sauce Labs services
#' @template account
#' @template ellipsis
#' @example /inst/examples/docs/infoMethods.R
#' @family infoMethods
#' @export

getSauceLabsStatus <-function (account, ...) {
	obj <- list()
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/info/status", data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getSauceLabsStatus", ...)
	res
}


#' Get Supported Platforms
#'
#' Get a list of objects describing all the OS and browser platforms currently supported on Sauce Labs. Choose the automation API you need, bearing in mind that WebDriver and Selenium RC are each compatible with a different set of platforms.
#' @template account
#' @param autoAPI Accepted Values for autoAPI "all", "appium", "webdriver". Defaults to "webdriver"
#' @template ellipsis
#' @example /inst/examples/docs/infoMethods.R
#' @family infoMethods
#' @export

getSupportedPlatforms <-function (account, autoAPI = "webdriver", ...) {
  obj <- list()
  obj$automation_api <- autoAPI
  pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/info/platforms/{{automation_api}}",
		data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getSupportedPlatforms", ...)
	sbv <- lapply(res, function(x){unlist(x[names(x) %in% c("supported_backend_versions")], use.names = FALSE)})
	res <- lapply(res, function(x){
	  x[sapply(x,length) == 0] <- NA
	  aD <- x[!names(x) %in% c("supported_backend_versions")]
	  aD
	})
	res <- rbindlist(res, fill = TRUE)
	res[, supported_backend_version:= sbv]
	res
}


#' Get Appium EOL dates
#'
#' Get a list of Appium end-of-life dates. Dates are displayed in Unix time.
#' @template account
#' @template ellipsis
#' @example /inst/examples/docs/infoMethods.R
#' @family infoMethods
#' @export

getAppiumEolDates <-function (account, ...) {
	obj <- list()
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/info/platforms/appium/eol", data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getAppiumEolDates", ...)
	lapply(res, function(x){
	  if(!is.null(x)){
	    as.POSIXct(x, origin="1970-01-01")
	  }else{
	    NA
	  }
	})
}


