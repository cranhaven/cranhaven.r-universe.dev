## tempStorageMethods.R - compiled by RoxygenReady, a package by @vertesy


#' Upload File
#'
#' Uploads a file to the temporary sauce storage. The storage will only retain the files for seven days.
#' @template account
#' @param username SauceLabs username
#' @param file file to upload
#' @param SLfileName name to give the file on SauceLabs. (Defaults to the current name of the file)
#' @template ellipsis
#' @example /inst/examples/docs/tempStorageMethods.R
#' @family tempStorageMethods
#' @export

uploadFile <-function (account, username = Sys.getenv("SLUSER"), file, SLfileName = basename(file), ...) {
  obj <- list()
  obj$username <- username
  obj$your_file_name <- SLfileName
  pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/storage/{{username}}/{{your_file_name}}",
		data = obj)
	pathURL <- parse_url(pathTemplate)
	body <- upload_file(file)
	res <- queryAPI(verb = POST, account = account, url = build_url(pathURL), source = "uploadFile",
		body = body, ...)
	res
}


#' Get Stored Files
#'
#' Check which files are in your temporary storage
#' @template account
#' @param username SauceLabs username
#' @template ellipsis
#' @example /inst/examples/docs/tempStorageMethods.R
#' @family tempStorageMethods
#' @export

getStoredFiles <-function (account, username = Sys.getenv("SLUSER"), ...) {
	obj <- list()
	obj$username <- username
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/storage/{{username}}", data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getStoredFiles", ...)
	res
}


