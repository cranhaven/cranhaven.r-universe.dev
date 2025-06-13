#' Generate spells for binary variables
#'
#' @param df a data frame
#' @param event character string naming the binary variable identifying the
#' event. Note \code{1} must signify events and \code{0} non-events.
#' @param t_var character string with the name of the time variable.
#' @param cs_unit character string with the name of the cross-sectional unit.
#' @param pad_ts logical indicating whether or not to fill in the time-series
#' if panels are unbalanced.
#'
#' @return The original (\code{df}) data frame with an additional
#' \code{spell_time} value identifying the number of observed periods in the spell,
#' i.e. time points since the last period.
#'
#' @examples
#' data('negative')
#'
#' neg_set <- btscs(df = negative, event = 'y', t_var = 'tim', cs_unit = 'group')
#'
#' @source This function is a port of Dave Armstrong's \code{btscs} function
#' from:
#'
#' Dave Armstrong (2015). DAMisc: Dave Armstrong's Miscellaneous Functions.
#' R package version 1.3. \url{https://CRAN.R-project.org/package=DAMisc}.
#'
#' It was ported largely to reduce the dependencies needed for the examples.
#' There are also internal improvements, largely to handle single period spells
#' and to start the spell time counter from 1.
#'
#' David's package implemented the Stata function from:
#'
#' Beck, N.. J. Katz and R. Tucker. 1998. "Beyond Ordinary Logit: Taking Time
#' Seriously in Binary-Time-Series-Cross-Section Models". American Journal of
#' Political Science 42(4): 1260-1288.
#'
#' @export

btscs <- function (df, event, t_var, cs_unit, pad_ts = FALSE)
{
    if (!is.data.frame(df)) stop('df must be a data frame.', call. = FALSE)

    if (!all(c(event, t_var, cs_unit) %in% names(df)))
        stop('Unable to locate all of the required variables in the supplied data frame.',
             call. = FALSE)

    df$orig_order <- 1:nrow(df)
    df <- df[order(df[[cs_unit]], df[[t_var]]), ]
    spells <- function(x) {
        tmp <- rep(0, length(x))
        runcount <- 0
        if (length(x) == 1) tmp <- runcount
        else
            for (j in 2:length(x)) {
                if (x[j] == 0 & x[(j - 1)] == 0) {
                    tmp[j] <- runcount <- runcount + 1
                }
                if (x[j] != 0 & x[(j - 1)] == 0) {
                    tmp[j] <- runcount + 1
                    runcount <- 0
                }
                if (x[j] == 0 & x[(j - 1)] != 0) {
                    tmp[j] <- runcount <- 0
                }
            }
        tmp
    }
    sp <- split(df, df[[cs_unit]])
    if (pad_ts) {
        sp <- lapply(sp, function(x) x[match(seq(min(x[[t_var]], na.rm = TRUE),
                                                 max(x[[t_var]], na.rm = TRUE)),
                                             x[[t_var]]),
                                       ])
        for (i in seq_along(sp)) {
            if (any(is.na(sp[[i]][[event]]))) {
                sp[[i]][[event]][which(is.na(sp[[i]][[event]]))] <- 1
            }
            if (any(is.na(sp[[i]][[t_var]]))) {
                sp[[i]][[t_var]] <- seq(min(sp[[i]][[t_var]], na.rm = TRUE),
                                       max(sp[[i]][[t_var]], na.rm = TRUE))
            }
            if (any(is.na(sp[[i]][[cs_unit]]))) {
                sp[[i]][[cs_unit]][which(is.na(sp[[i]][[cs_unit]]))] <-
                    mean(sp[[i]][[cs_unit]], na.rm = TRUE)
            }
        }
    }
    sp <- lapply(seq_along(sp), function(x) {
        cbind(sp[[x]], data.frame(spell_time = spells(sp[[x]][[event]])),
              row.names = NULL)
    })
    df <- do.call(rbind, sp)
    if (!pad_ts) {
        if (any(is.na(df$orig_order))) {
            df <- df[-which(is.na(df$orig_order)), ]
        }
        df <- df[df$orig_order, ]
    }
    else {
        df <- df[order(df[[cs_unit]], df[[t_var]]), ]
    }
    df$orig_order <- NULL
    df$spell_time <- df$spell_time + 1
    return(df)
}
