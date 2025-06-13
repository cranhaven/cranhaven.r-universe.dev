#' Get Real-Time Job Activity
#'
#' Get information about concurrency, minutes and jobs used by the user over a specific duration (default 90 days). Concurrency is separated in mean and peak concurrency.
#' @template account
#' @param username SauceLabs username
#' @template ellipsis
#' @example /inst/examples/docs/actUsageMethods.R
#' @family actUsageMethods
#' @export

getRealTimeJobActivity <-function (account, username = Sys.getenv("SLUSER"), ...) {
	obj <- list()
	obj$username = username
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1.1/users/{{username}}/concurrency",
		data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getRealTimeJobActivity", ...)
	res
}


#' Get User Activity
#'
#' Get currently running job counts broken down by account and job status.
#' @template account
#' @param username SauceLabs username
#' @template ellipsis
#' @example /inst/examples/docs/actUsageMethods.R
#' @family actUsageMethods
#' @export

getUserActivity <-function (account, username = Sys.getenv("SLUSER"), ...) {
	obj <- list()
	obj$username = username
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/{{username}}/activity", data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getUserActivity", ...)
	res
}


#' Get User Account Usage
#'
#' Access historical account usage data
#' @template account
#' @param username SauceLabs username
#' @template ellipsis
#' @example /inst/examples/docs/actUsageMethods.R
#' @family actUsageMethods
#' @return The result is a breakdown summarizing the total number of jobs and VM time used, in seconds, by day.
#' @export

getUserAccountUsage <-function (account, username = Sys.getenv("SLUSER"), ...) {
	obj <- list()
	obj$username = username
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/users/{{username}}/usage", data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getUserAccountUsage", ...)
	res <- lapply(res$usage, function(x){
	  data.frame(user_name = res$username, date = x[[1]], no_of_jobs = x[[2]][[1]]
	             , vm_minutes = x[[2]][[2]], stringsAsFactors = FALSE)
	})
	rbindlist(res)
}


