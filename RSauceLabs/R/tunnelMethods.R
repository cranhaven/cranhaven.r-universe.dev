#' Get Tunnels
#'
#' Retrieves all running tunnels for a specific user
#' @template account
#' @param username SauceLabs username
#' @template ellipsis
#' @example /inst/examples/docs/tunnelMethods.R
#' @family tunnelMethods
#' @export

getTunnels <-function (account, username = Sys.getenv("SLUSER"), ...) {
	obj <- list()
	obj$username <- username
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/{{username}}/tunnels", data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getTunnels", ...)
	res
}


#' getTunnel
#'
#' Get information for a tunnel given its ID
#' @template account
#' @param username SauceLabs username
#' @param tunnelID id of the tunnel to get more information on
#' @template ellipsis
#' @example /inst/examples/docs/tunnelMethods.R
#' @family tunnelMethods
#' @export

getTunnel <-function (account, username = Sys.getenv("SLUSER"), tunnelID, ...) {
	obj <- list()
	obj$username <- username
	obj$tunnel_id <- tunnelID
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/{{username}}/tunnels/{{tunnel_id}}",
		data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = GET, account = account, url = build_url(pathURL), source = "getTunnel", ...)
	res
}


#' Delete Tunnel
#'
#' Shuts down a tunnel given its ID
#' @template account
#' @param username SauceLabs username
#' @param tunnelID id of the tunnel to shutdown
#' @template ellipsis
#' @example /inst/examples/docs/tunnelMethods.R
#' @family tunnelMethods
#' @export

deleteTunnel <-function (account, username = Sys.getenv("SLUSER"), tunnelID, ...) {
	obj <- list()
	obj$username <- username
	obj$tunnel_id <- tunnelID
	pathTemplate <- whisker.render("https://saucelabs.com/rest/v1/{{username}}/tunnels/{{tunnel_id}}",
		data = obj)
	pathURL <- parse_url(pathTemplate)
	res <- queryAPI(verb = DELETE, account = account, url = build_url(pathURL), source = "deleteTunnel", ...)
	res
}


