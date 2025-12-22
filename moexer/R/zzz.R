.onLoad <- function(libname, pkgname) {
    options(
        moexer.iss.base_url = 'http://iss.moex.com/iss',
        moexer.debug = FALSE,
        moexer.candle.intervals = list(
            per_minute = 1,
            per_10_minutes = 10,
            hourly = 60,
            daily = 24,
            weekly = 7,
            monthly = 31,
            quarterly = 4
        )
    )
}

if (getRversion() >= '2.15.1')
    utils::globalVariables(c(
        'is_primary', 'begin', 'direction', 'high', 'low', 'secid'
    ))
