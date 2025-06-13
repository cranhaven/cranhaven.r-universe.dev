#' Get Jobs
#'
#' List recent jobs belonging to a specific user
#' @template account
#' @param username SauceLabs username
#' @param limit Specifies the number of jobs to return. Default is 100 and max is 500.
#' @param getFullJobs Get full job information, rather than just IDs. Default is FALSE.
#' @param skipJobs Skips the specified number of jobs. Default is 0.
#' @param to Get jobs until the specified time (POSIXct)
#' @param from Get jobs since the specified time (POSIXct)
#' @template ellipsis
#'
#' @return returns a named list. "data" is the job data minus the tags and custom-data.
#' tagsAndCD are a list of tags and custom-data for each job. If getFullJobs = FALSE then data only contains the job ids and
#' tagsAndCD contains empty lists for each job.
#' @export
#'
#' @example /inst/examples/docs/jobMethods.R
#' @family jobMethods
getJobs <- function(account, username = Sys.getenv("SLUSER"), limit = 100L, getFullJobs = FALSE
                    , skipJobs = 0L, to = NULL, from = NULL, ...){
  obj <- list()
  obj$username <- username
  if(!is.null(to)){
    if(!inherits(to, "POSIXct")){stop("to should be a POSIXct object")}
    to <- as.integer(to)
  }
  if(!is.null(from)){
    if(!inherits(from, "POSIXct")){stop("from should be a POSIXct object")}
    from <- as.integer(from)
  }
  query <- list(limit = limit,
                full = list(NULL, "true")[[getFullJobs + 1]],
                skip= skipJobs,
                to = to,
                from = from,
                format = "json"
  )
  pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/{{username}}/jobs",
                                 data = obj)
  pathURL <- parse_url(pathTemplate)
  res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getJobAssetNames",
                  query = query, ...)
  mainData <- rbindlist(lapply(res, function(x){
    aD <- x[!names(x) %in% c("custom-data", "tags")]
    aD[sapply(aD, is.null)] <- NA
    aD
  }
  ), fill = TRUE)
  tagsAndCD <- lapply(res, function(x){x[names(x) %in% c("custom-data", "tags")]})
  list(data = mainData, tagsAndCD = tagsAndCD)
}

#' Update Job
#'
#' Edit an existing job
#' @template account
#' @param username SauceLabs username
#' @param jobID Id of the job to edit
#' @param name Change the job name
#' @param tags Change the job tags
#' @param public Set job visibility to "public", "public restricted", "share" (true), "team" (false) or "private"
#' @param passed Set whether the job passed or not on the user end
#' @param build The build number tested by this test
#' @param custom_data A  set of key-value pairs with any extra info that a user would like to add to the job. Note that the max data allowed is 64KB
#' @template ellipsis
#' @example /inst/examples/docs/jobMethods.R
#' @family jobMethods
#' @export

updateJob <-function (account, username = Sys.getenv("SLUSER"), jobID, name = NULL, tags = NULL, public = NULL, passed = NULL
                      , build = NULL, custom_data = NULL, ...) {
  obj <- list()
  obj$job_id <- jobID
  obj$username <- username
  body <- list()
  body$name = unbox(name)
  body$tags = tags
  body$public = unbox(public)
  body$passed = unbox(passed)
  body$build = unbox(build)
  body$`custom-data` = custom_data
  body <- toJSON(body)
  pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/{{username}}/jobs/{{job_id}}", data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = PUT, account = account, url = build_url(pathURL), source = "updateJob"
	                , body = body, ...)
	res
}


#' Delete Job
#'
#' Removes the job from the system with all the linked assets
#' @template account
#' @param username SauceLabs username
#' @param jobID Id of the job to delete
#' @template ellipsis
#' @example /inst/examples/docs/jobMethods.R
#' @family jobMethods
#' @export

deleteJob <-function (account, username = Sys.getenv("SLUSER"), jobID, ...) {
	obj <- list()
	obj$job_id <- jobID
	obj$username <- username
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/{{username}}/jobs/{{job_id}}", data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = DELETE, account = account, url = build_url(pathURL), source = "deleteJob",
		json = body, ...)
	res
}


#' Stop Job
#'
#' Terminates a running job
#' @template account
#' @param username SauceLabs username
#' @param jobID Id of the job to stop
#' @template ellipsis
#' @example /inst/examples/docs/jobMethods.R
#' @family jobMethods
#' @export

stopJob <-function (account, username = Sys.getenv("SLUSER"), jobID, ...) {
	obj <- list()
	obj$username <- username
	obj$job_id <- jobID
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/{{username}}/jobs/{{job_id}}/stop",
		data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = PUT, account = account, url = build_url(pathURL), source = "stopJob", json = body,
		...)
	res
}


#' Get Job Asset Names
#'
#' Get details about the static assets collected for a specific job
#' @template account
#' @param username SauceLabs username
#' @param jobID Id of the job to stop
#' @template ellipsis
#' @example /inst/examples/docs/jobMethods.R
#' @family jobMethods
#' @export

getJobAssetNames <-function (account, username = Sys.getenv("SLUSER"), jobID, ...) {
	obj <- list()
	obj$username <- username
	obj$job_id <- jobID
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/{{username}}/jobs/{{job_id}}/assets",
		data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getJobAssetNames",
		json = body, ...)
	res
}


#' Get Job Asset Files
#'
#' Download job assets. After a job completes, all assets created during the job are available via this API. These include the screencast recording, logs, and screenshots taken on crucial steps.
#' The job assests will be deleted from the test page after 30 days. Thus, after 30 days all your test commands, logs, screenshots and the screencast recording will be gone. This is the reason why we strongly recommend to download your job assets if this is an information that you must keep in your records.
#' @template account
#' @param username SauceLabs username
#' @param jobID Id of the job to get assets from
#' @param fileName Accepted Values for fileName "selenium-server.log" "video.flv" "XXXXscreenshot.png" (where XXXX is a number between 0000 and 9999) "final_screenshot.png"
#' @template ellipsis
#' @example /inst/examples/docs/jobMethods.R
#' @family jobMethods
#' @export

getJobAssetFiles <-function (account, username = Sys.getenv("SLUSER"), jobID, fileName = "selenium-server.log", ...) {
	obj <- list()
	obj$username <- username
	obj$job_id <- jobID
	obj$file_name <- fileName
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/{{username}}/jobs/{{job_id}}/assets/{{file_name}}",
		data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getJobAssetFiles",
		json = body, ...)
	res
}


#' Delete Job Assets
#'
#' Delete all the assets captured during a test run. This includes the screencast recording, logs, and all screenshots.
#' @template account
#' @param username SauceLabs username
#' @param jobID Id of the job to delete assests from
#' @template ellipsis
#' @example /inst/examples/docs/jobMethods.R
#' @family jobMethods
#' @export

deleteJobAssets <-function (account, username = Sys.getenv("SLUSER"), jobID, ...) {
	obj <- list()
	obj$username <- username
	obj$job_id <- jobID
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/{{username}}/jobs/{{job_id}}/assets",
		data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = DELETE, account = account, url = build_url(pathURL), source = "deleteJobAssets",
		json = body, ...)
	res
}


