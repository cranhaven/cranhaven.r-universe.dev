#' Access basic account information
#'
#' Access basic account information
#' @template account
#' @param username SauceLabs username
#' @template ellipsis
#' @example /inst/examples/docs/accountMethods.R
#' @family accountMethods
#' @export

getUser <-function (account, username = Sys.getenv("SLUSER"), ...) {
  obj <- list()
  obj$username <- username
  pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/users/{{username}}", data = obj)
  pathURL <- parse_url(pathTemplate)
  res <- queryAPI(verb = GET, url = build_url(pathURL), account = account, source = "getUser", body = NULL, ...)
  res
}


#' Create a sub account
#'
#' Create a sub account
#' @template account
#' @param username SauceLabs username
#' @param newUsername The username of the new user you wish to create
#' @param password The password for the new user you wish to create
#' @param name The name of the new user you wish to create
#' @param email The email of the new user you wish to create
#' @template ellipsis
#' @example /inst/examples/docs/accountMethods.R
#' @family accountMethods
#' @export

createUser <-function (account, username = Sys.getenv("SLUSER"), newUsername, password, name, email, ...) {
	obj <- list()
	obj$username = username
	body <- toJSON(list(
	  username = newUsername,
	  password = password,
	  name = name,
	  email = email
	), auto_unbox = TRUE)
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/users/{{username}}", data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = POST, account = account, url = build_url(pathURL), source = "createUser",
		body = body, ...)
	res
}


#' Check account concurrency limits
#'
#' Check account concurrency limits
#' @template account
#' @param username SauceLabs username
#' @template ellipsis
#' @example /inst/examples/docs/accountMethods.R
#' @family accountMethods
#' @export

getUserConcurrency <-function (account, username = Sys.getenv("SLUSER"), ...) {
	obj <- list()
	obj$username = username
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1.1/users/{{username}}/concurrency",
		data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getUserConcurrency", ...)
	res
}


#' Get a list of sub accounts
#'
#' Get a list of sub accounts associated with a parent account
#' @template account
#' @param username SauceLabs username
#' @param from Get user from this user number. Defaults to NULL
#' @param limit The limit on users returned. Defaults to 50L (50L is the max).
#' @template ellipsis
#' @example /inst/examples/docs/accountMethods.R
#' @family accountMethods
#' @export

getListOfSubAccounts <-function (account, username = Sys.getenv("SLUSER"), from = NULL, limit = 100L, ...) {
	obj <- list()
	obj$username = username
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/users/{{username}}/list-subaccounts",
		data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getListOfSubAccounts",
		query = list(from = from, limit = limit), ...)
	res
}


#' Get a list of sibling accounts
#'
#' Get a list of sibling accounts associated with provided account
#' @template account
#' @param username SauceLabs username
#' @param page optional defaults to NULL
#' @param per_page results per page (max 50L). Defaults to 50L
#' @template ellipsis
#' @example /inst/examples/docs/accountMethods.R
#' @family accountMethods
#' @export

getListOfSiblingAccounts <-function (account, username = Sys.getenv("SLUSER"), page = NULL, per_page = 50L, ...) {
	obj <- list()
	obj$username = username
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1.1/users/{{username}}/siblings", data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getListOfSiblingAccounts",
		query = list(page = page, per_page = per_page), ...)
	res
}


#' Get information about a sub account
#'
#' Get information about a sub account
#' @template account
#' @param username SauceLabs username
#' @template ellipsis
#' @example /inst/examples/docs/accountMethods.R
#' @family accountMethods
#' @export

getSubAccountInformation <-function (account, username = Sys.getenv("SLUSER"), ...) {
	obj <- list()
	obj$username = username
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/users/{{username}}/subaccounts",
		data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getSubAccountInformation", ...)
	res
}


#' Change access key
#'
#' Change access key of an account
#' @template account
#' @param username SauceLabs username
#' @template ellipsis
#' @example /inst/examples/docs/accountMethods.R
#' @family accountMethods
#' @export

changeAccessKey <-function (account, username = Sys.getenv("SLUSER"), ...) {
	obj <- list()
	obj$username = username
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/users/{{username}}/accesskey/change",
		data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = POST, account = account, url = build_url(pathURL), source = "changeAccessKey",
		json = body, ...)
	res
}


