#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# #################### #
# working with locales #
# #################### #


.try_locale <- function(locale)
{
    if (!is.character(locale) || length(locale) != 1L) {
        mes0 <- gettextf("invalid %s argument", sQuote("locale"))
        mes1 <- gettextf("character string expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }

    suppressWarnings(res <- Sys.setlocale("LC_TIME", locale))

    if (res == "") {
        msg <- gettextf("operating system did not accept locale %s", dQuote(locale))
        hint <- gettextf("Hint: on majority of modern operating systems locale is of the form %s (%s for language, %s for country) optionally followed by a dot and a character set identifier, for example, %s",
                         dQuote("xx_XX"), dQuote("xx"), dQuote("XX"),
                         dQuote("pl_PL.UTF-8"))
        stop(paste0(msg, "\n", hint), call. = FALSE, domain = NA)
    }
}


.calendar_names <- function(locale)
{
    if (!is.null(locale)) {
        locale_t <- Sys.getlocale(category = "LC_TIME")
        if (is.null(locale_t)) {
            mes0 <- gettextf("locale information not available")
            mes1 <- gettextf("using default settings")
            warning(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
        } else {
            on.exit(Sys.setlocale(category = "LC_TIME", locale = locale_t))
            .try_locale(locale)
        }
    }
    dd <- c("2000-01-03", "2000-02-01", "2000-03-01", "2000-04-06",
            "2000-05-05", "2000-06-03", "2000-07-02", "2000-08-01",
            "2000-09-01", "2000-10-01", "2000-11-01", "2000-12-01")
    dd <- as.Date(dd)
    res <- c(format(dd, "%b"), format(dd, "%B"),
             format(dd[1L:7L], "%a"), format(dd[1L:7L], "%A"))
    ampm <- as.POSIXct(c("2000-01-01 06:00", "2000-01-01 18:00"),
                                    format = "%F %R", tz = "UTC")
    ampm <- tryCatch(format(ampm, format = "%p"), error = function(e) c("", ""))
    res <- c(res, ampm)
    return (res)
}

