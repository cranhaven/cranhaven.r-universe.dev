#' Start JS Unit Tests
#'
#' Start your JavaScript unit tests on as many browsers as you like with a single request
#' @template account
#' @param username SauceLabs username
#' @param platforms A list of platforms (see example)
#' @param url should point to the page that hosts your tests
#' @param framework can be "qunit", "jasmine", "YUI Test", "mocha", or "custom"
#' @template ellipsis
#' @example /inst/examples/docs/jsUnitTestMethods.R
#' @family jsUnitTestMethods
#' @export

startJsUnitTests <-function (account, username = Sys.getenv("SLUSER"), platforms, url, framework, ...) {
  obj <- list()
  obj$username <- username
  pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/{{username}}/js-tests", data = obj)
	pathURL <- parse_url(pathTemplate)
	body <- toJSON(
	  list(
	    platforms = platforms,
	    url = unbox(url),
	    framework = unbox(framework)
	  )
	)
	res <- queryAPI(verb = POST, account = account, url = build_url(pathURL), source = "startJsUnitTests",
		body = body, ...)
	res
}


#' Get JS Unit Test Status
#'
#' Get the status of your JS unit tests
#' @template account
#' @param username SauceLabs username
#' @param js_tests a vector of job ids.
#' @template ellipsis
#' @example /inst/examples/docs/jsUnitTestMethods.R
#' @family jsUnitTestMethods
#' @export

getJsUnitTestStatus <-function (account, username = Sys.getenv("SLUSER"), js_tests, ...) {
	obj <- list()
	obj$username <- username
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/{{username}}/js-tests/status", data = obj)
	pathURL <- parse_url(pathTemplate)
	body <- toJSON(
	  list("js tests" = js_tests)
	)
	res <- queryAPI(verb = POST, account = account, url = build_url(pathURL), source = "getJsUnitTestStatus",
		body = body, ...)
	res
}


