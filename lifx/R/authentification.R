#' retrieve lifx_token from R environment
#'
#' @template details_token
#' @return the 'LIFX' api token found in environmental variables
#' @seealso \code{\link{lx_has_token}}, \code{\link{lx_save_token}}
#' @examples
#' \dontrun{
#' lx_get_token()
#' }
#' @export
lx_get_token <- function() {
    token <- Sys.getenv("LIFX_TOKEN")
    if (identical(token, "")) {
        stop("Please supply a token or add a token to your R environment file with lifx::lx_save_token()", call. = FALSE)
    }

    return(token)
}

#' save a lifx API token in your r environment file
#' @template details_token
#' @template param_token
#' @seealso \code{\link{lx_has_token}}, \code{\link{lx_get_token}}
#' @return logical TRUE if saving token has been successful
#' @examples
#' \dontrun{
#' lx_save_token("asodfjawea9499fao8u4a09fw0s8fu439wfrsud7") # put your token here
#' }
#' @export
lx_save_token <- function(token) {
    assertthat::assert_that(assertthat::is.string(token))
    Sys.setenv(LIFX_TOKEN = token)
}

#' check whether a lifx api token is stored in the R environment file.
#'
#' @template details_token
#' @seealso \code{\link{lx_save_token}}, \code{\link{lx_get_token}}
#' @return logical TRUE if a token was found
#' @examples lx_has_token()
#' @export
lx_has_token <- function() {
    Sys.getenv("LIFX_TOKEN") != ""
}
