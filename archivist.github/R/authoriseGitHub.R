##  archivist package for R
##  archivist.github package for R
##
#' @title Authorise with GitHub API
#'
#' @description
#' 
#' \code{authoriseGitHub} is function that performes OAuth authorisation with GitHub API 
#' and stores resulting token in the \code{github_token} variable.
#' In order to authorise your app you need ClinetID and ClientSecret.
#' They can be found here: https://github.com/settings/applications/new
#' 
#' @references 
#' More about \pkg{archivist.github} can be found on 
#' \href{http://marcinkosinski.github.io/archivist.github/}{marcinkosinski.github.io/archivist.github/} 
#' and about \pkg{archivist} in posts' history on \href{https://pbiecek.github.io/archivist/articles/posts.html}{https://pbiecek.github.io/archivist/articles/posts.html}
#' 
#' @param ClientID A 20 characters long string with Client ID. See https://github.com/settings/applications/ for more details.
#' 
#' @param ClientSecret A 40 characters long string with Client Secret. See https://github.com/settings/applications/ for more details.
#' 
#' @param scope A character vector with the list of availables scopes for the GitHub API token. See \href{https://developer.github.com/v3/oauth/#scopes}{https://developer.github.com/v3/oauth/#scopes}.
#' For repository deletion you will need to add \code{"delete_repo"} scope.
#' 
#' @author 
#' Przemyslaw Biecek, \email{przemyslaw.biecek@@gmail.com}
#'
#' @note 
#' Bug reports and feature requests can be sent to \href{https://github.com/MarcinKosinski/archivist.github/issues}{https://github.com/MarcinKosinski/archivist.github/issues}
#' 
#'
#' @examples
#' \dontrun{
#' ## GitHub version
#' authoriseGitHub(ClientID, ClientSecret) -> github_token
#' }
#' @family archivist.github
#' @rdname authoriseGitHub
#' @export
authoriseGitHub <- function(ClientID, ClientSecret, scope = c("public_repo")) {
	myapp <- oauth_app("github",
	                    key = ClientID,
	                    secret = ClientSecret)
	github_token <- oauth2.0_token(oauth_endpoints("github"),
	                                 myapp,
	                                 scope = scope)
	aoptions("github_token", github_token)
	github_token
}
