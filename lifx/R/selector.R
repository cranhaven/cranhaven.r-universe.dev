
#' select lights
#'
#' use this function to select lights that you want to communicate with
#' @param id the id of the lamp(s) to select
#' @param label the label of the lamp(s) to select
#' @param group_id the group_id of the lamp(s) to select
#' @param group the group of the lamp(s) to select
#' @param location_id the location_id of the lamp(s) to select
#' @param location the location of the lamp(s) to select
#' @param zones the zones of the lamp(s) to select
#' @details this creates strings to select lamps in the format that the 'LIFX' api expects (see https://api.developer.lifx.com/docs/selectors). This function is intended to be used to create a 'selector' that is then passed to a function that changes the state of the lamps.
#' @examples
#' lx_selector(id = '1234')
#' lx_selector(label = "my_light")
#' lx_selector(location = 'kitchen', zone = 3)
#' lx_selector(location = 'kitchen', group = 'ceiling')
#' @return a character string in the format expected by the 'LIFX' API for selectors. It has it's own class and printing style, but a regular character string can be used just as well.
#' @export
lx_selector <- function(id = NULL, label = NULL, group_id = NULL, group = NULL, location_id = NULL, location = NULL, zones = NULL) {
    args <- as.list(match.call())
    # remove fun name from args
    args <- args[-1]

    # must add |1|2|3 if zones = c(1,2,3) at the end of each selector:
    if (!is.null(zones)) {
        # remove 'zones' from argument list:
        args <- args[-length(args)]
        zones_string <- paste0("|", paste0(zones, collapse = "|"))
    } else {
        zones_string <- ""
    }

    if (length(args) == 0) {
        return(paste0("all", zones_string))
    }

    selector <- paste0(paste0(paste0(names(args), ":", args), zones_string), collapse = ",")
    class(selector) <- unique(c("lx_selector_string", class(selector)))
    selector
}

#' @method print lx_selector_string
#' @export
print.lx_selector_string <- function(x, ...) {
    cat(crayon::silver("lx selector:"))
    cat(crayon::italic(x))
    invisible(x)
}

