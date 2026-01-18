#' Analysing the results of simulations ran with \code{simER}
#'
#' Analysing the results of simulations ran with \code{simER}.
#' It computes the average sample number (ASN) at which the boundary is attained
#' (either the lower or the upper one), the percentage of hits of the lower
#' boundary as well as hits of the upper boundary, and the percentage of
#' trajectories that did not hit none of the boundaries.
#'
#' @param sim A \code{simER} or a \code{compER} object.
#'
#' @return An object of class \code{data.frame}, which contains the average
#' sample number (ASN) at which the boundary is attained (either the lower or
#' the upper one), the percentage of hits of the lower boundary as well as hits
#' of the upper boundary, and the percentage of trajectories that did not hit
#' none of the boundaries (and thus end at nmax).
#'
#' @importFrom stats sd na.omit
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' library(ESTER)
#' sim <- simER(cohensd = 0.8, nmin = 20, nmax = 100, boundary = 10, nsims = 100, ic = bic)
#' analysER(sim)
#' }
#'
#' @author Ladislas Nalborczyk <\email{ladislas.nalborczyk@@gmail.com}>
#'
#' @seealso \code{\link{simER}}
#'
#' @export

analysER <- function(sim) {

    UseMethod("analysER")

}

#' @export

analysER.simER <- function(sim) {

    if (!any(class(sim) == "simER") ) {

        stop("sim should be a simER object")

    }

    sim <- na.omit(sim)

    bound_hit <- function(x, boundary) {

        if (any(x > boundary) ) {

            first <- which(x > boundary)[1]
            x[first:length(x)] <- boundary

        } else if (any(x < (1 / boundary) ) ){

            first <- which(x < (1 / boundary) )[1]
            x[first:length(x)] <- 1 / boundary

        } else{

            x <- x

        }

        return(x)

    }

    bound_na <- function(x, boundary) {

        if (any(x == boundary) ) {

            first <- which(x == boundary)[1]

            if (first < length(x) ) {

                x[(first + 1):length(x)] <- NA

            }

        } else if (any(x == 1 / boundary) ) {

            first <- which(x == 1 / boundary)[1]

            if (first < length(x) ) {

                x[(first + 1):length(x)] <- NA

            }

        } else {

            x <- x

        }

        return(x)

    }

    sim2 <-
        sim %>%
        group_by_("id", "true.ES", "boundary") %>%
        mutate_("ER" = "bound_na(bound_hit(ER, boundary), boundary )" ) %>%
        ungroup()

    nmax.hit <-
        sim2 %>%
        group_by_("id", "true.ES", "boundary") %>%
        mutate_("log_ER" = "log(ER)" ) %>%
        filter_(.dots = list("n == max(n) & ER < boundary & ER > 1 / boundary") ) %>%
        mutate(hitCondition = "nmax") %>%
        na.omit()

    boundary.hit <-
        sim2 %>%
        group_by_("id", "true.ES", "boundary") %>%
        mutate_("log_ER" = "log(ER)" ) %>%
        filter_(.dots = list(~ER %in% c(boundary, 1 / boundary) ) ) %>%
        mutate(hitCondition = "boundary") %>%
        na.omit()

    endpoint <- bind_rows(nmax.hit, boundary.hit)
    total <- length(unique(sim$id) ) * length(unique(sim$true.ES) ) * length(unique(sim$boundary) )

    if(!total == nrow(endpoint) ) {

        stop("number of detected endpoints is not equal to number of trajectories...")

    }

    res <-
        endpoint %>%
        group_by_("true.ES", "boundary") %>%
        summarise_(
            "ASN" = "mean(n)",
            "Lower_hit" = "sum(ER == 1 / boundary) / n_distinct(sim$id)",
            "Upper_hit" = "sum(ER == boundary) / n_distinct(sim$id)",
            "Inconclusive" = "1 - (Lower_hit + Upper_hit)" ) %>%
        data.frame %>%
        mutate_at(.vars = 4:6, .funs = funs(paste0(. * 100, "%") ) )

    return(res)

}
