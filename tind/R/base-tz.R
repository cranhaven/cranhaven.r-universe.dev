#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ####################### #
# working with time zones #
# ####################### #


# check time zone name
# ###################################################################

.check_tz <- function(tz)
{
    if (!is.null(tz) && (!is.character(tz) || (length(tz) != 1L))) {
        mes0 <- gettextf("invalid %s argument", sQuote("tz"))
        mes1 <- gettextf("character string expected")
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    if (is.null(tz) || (tz == "")) {
        tz <- Sys.timezone()
        return (if (is.na(tz) || tz == "") "UTC" else tz)
    }

    tzs <- OlsonNames()
    if (tz %in% tzs) return (tz)

    pm <- agrep(tz, tzs, fixed = TRUE, value = TRUE)
    if (length(pm) == 1L) {
        # unambiguous city names (Continent/City)
        splt <- strsplit(pm, split = "/")[[1L]]
        ok <- length(splt) == 2L
        if (ok) {
            city <- splt[2L]
            ok <- (tz == city) || (gsub(" ", "_", tz) == city)
        }
        if (!ok) {
            mes <- gettextf("assuming %s refers to time zone %s", dQuote(tz),
                            dQuote(pm))
            warning(mes, call. = FALSE, domain = NA)
        }
        return (pm)
    }

    if (length(pm) > 0L) {
        collapse_nm <- function(n) {
            nn <- length(n)
            if (nn <= 4L) return (toString(dQuote(n)))
            return (toString(c(dQuote(n[1L:3L]), paste0("... [+", nn - 3L, "]"))))
        }
        mes0 <- gettextf("ambiguous time zone name: %s", dQuote(tz))
        mes1 <- gettextf("possible matches: %s", collapse_nm(pm))
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }

    mes0 <- gettextf("unrecognised time zone: %s", dQuote(tz))
    mes1 <- gettextf("you can find the list of time zones by running %s",
                     sQuote("OlsonNames()"))

    stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
}


# warn on different time zones of arguments
# ###################################################################

.warn_diff_tz <- function(tz1, tz2 = NULL, first = FALSE)
{
    if (!.tind.getOption("warn.diff.tz")) return (invisible(NULL))
    mes0 <- gettextf("different time zones of arguments")
    if (!is.null(tz2)) {
        mes0 <- paste0(mes0, ": ", dQuote(tz1), ", ", dQuote(tz2))
    }
    if (first) {
        mes1 <- gettextf("assuming: %s", dQuote(tz1))
        mes <- paste0(mes0, "; ", mes1)
    } else mes <- mes0
    warning(mes, call. = FALSE, domain = NA)
}


# time zone peculiarities
# ###################################################################

# UTC/GMT time zones
.tz_utc <- function()
{
    c("UTC", "UCT", "Etc/UTC", "Etc/UCT", "Etc/Universal", "Etc/Zulu",
      "Zulu", "GMT", "GMT0", "GMT+0", "GMT-0", "Etc/GMT", "Etc/GMT0",
      "Etc/GMT-0", "Etc/GMT+0", "Etc/Greenwich")
}


# time zones with fixed UTC offset
.tz_fixed <- function()
{
    c("Etc/GMT-14", "Etc/GMT-13", "Etc/GMT-12", "Etc/GMT-11", "Etc/GMT-10",
      "Etc/GMT-9", "Etc/GMT-8", "Etc/GMT-7", "Etc/GMT-6", "Etc/GMT-5",
      "Etc/GMT-4", "Etc/GMT-3", "Etc/GMT-2", "Etc/GMT-1", "Etc/GMT+1",
      "Etc/GMT+2", "Etc/GMT+3", "Etc/GMT+4", "Etc/GMT+5", "Etc/GMT+6",
      "Etc/GMT+7", "Etc/GMT+8", "Etc/GMT+9", "Etc/GMT+10", "Etc/GMT+11",
      "Etc/GMT+12")
}


.tz_fixed_offset <- function(tz) as.numeric(gsub("Etc/GMT", "", tz)) * -3600


# time zones with days missing
.tz_missing_days <- function()
{
    list("Pacific/Kwajalein"  = 8633L,  # 1993-08-21
         "Kwajalein"          = 8633L,  # 1993-08-21
         "Pacific/Enderbury"  = 9130L,  # 1994-12-31
         "Pacific/Kanton"     = 9130L,  # 1994-12-31
         "Pacific/Kiritimati" = 9130L,  # 1994-12-31
         "Pacific/Apia"       = 15338L, # 2011-12-30
         "Pacific/Fakaofo"    = 15338L) # 2011-12-30
}


# time zones with double midnights
.tz_dblmidnight <- function(y)
{
    # 1994-
    dbl94 <- c("Antarctica/Davis", "Antarctica/Casey", "Antarctica/Vostok",
               "Asia/Amman", "Asia/Chita", "Asia/Colombo", "Asia/Gaza",
               "Asia/Hebron", "Asia/Jerusalem", "Asia/Magadan",
               "Asia/Tel_Aviv", "Israel")
    # 1953-1993
    dbl53 <- c("Africa/Tunis", "Asia/Tehran", "Atlantic/Madeira", "Europe/Athens",
               "Asia/Ho_Chi_Minh", "Asia/Saigon",
               "Europe/Bucharest", "Europe/Budapest", "Europe/Lisbon",
               "Europe/Madrid", "Europe/Malta", "Europe/Monaco", "Europe/Paris",
               "Europe/Rome", "Europe/San_Marino", "Europe/Sofia",
               "Europe/Uzhgorod", "Europe/Vatican", "Iran", "Portugal")
    # 1923-1952
    dbl23 <- c("Africa/Algiers", "Africa/Ceuta", "Asia/Taipei", "Asia/Tokyo",
               "Europe/Chisinau", "Europe/Helsinki", "Europe/Luxembourg",
               "Europe/Mariehamn", "Europe/Tiraspol", "Japan", "ROC")

    dbl <- dbl94
    if (y < 1994L) dbl <- c(dbl, dbl53)
    if (y < 1953L) dbl <- c(dbl, dbl23)
    return (dbl)
}


# time zones with UTC offsets that are/were not full hours 1988-
.tz_nfh88 <- function()
{
    c("America/Caracas", "America/St_Johns", "Asia/Calcutta", "Asia/Colombo",
      "Asia/Kabul", "Asia/Kathmandu", "Asia/Katmandu", "Asia/Kolkata",
      "Asia/Pyongyang", "Asia/Rangoon", "Asia/Tehran", "Asia/Yangon",
      "Australia/Adelaide", "Australia/Broken_Hill", "Australia/Darwin",
      "Australia/Eucla", "Australia/LHI", "Australia/Lord_Howe",
      "Australia/North", "Australia/South", "Australia/Yancowinna",
      "Canada/Newfoundland", "Indian/Cocos", "Iran", "NZ-CHAT",
      "Pacific/Chatham", "Pacific/Marquesas", "Pacific/Norfolk",
      "Pacific/Pitcairn", "Pacific/Rarotonga")
}


# time zones with UTC offsets that are/were not multiples of 30min 1961-
.tz_n30m61 <- function()
{
    c("Africa/Monrovia", "America/Guyana", "Asia/Kathmandu", "Asia/Katmandu",
      "Australia/Eucla", "NZ-CHAT", "Pacific/Chatham", "Pacific/Kiritimati")
}


# short time zone names and corresponding UTC offsets
# ###################################################################

# If invoked without 'tzs' argument, this returns character vector
# of tz abbreviations. If 'tzs' argument is provided, a list with numeric vector
# of UTC offsets (in seconds) and time zone is returned.

.tzshort <- function(tzs, tz = NULL, d = NA_integer_)
{
    if (missing(tzs)) return (names(.shorttzs))

    .mk_shortoff <- function(tzs, d)
    {
        t0 <- max(-1483120800, min(d) * 86400, na.rm = TRUE)
        t1 <- min(as.numeric(Sys.time()) + 31557600,
                    max(d) * 86400, na.rm = TRUE)
        # we are going from latest to earliest
        tt <- (round(t1 / 15778800) + 1):(round(t0 / 15778800) - 1) * 15778800
        shortoff <- data.frame(name = character(), short = character(), off = double(),
                               stringsAsFactors = FALSE)
        for (tz in tzs) {
            plt <- as.POSIXlt(tt, origin = "1970-01-01", tz = tz)
            shortoff0 <- data.frame(short = plt$zone, off = as.double(plt$gmtoff),
                                    stringsAsFactors = FALSE)
            shortoff0 <- unique(shortoff0)
            shortoff0 <- shortoff0[nchar(shortoff0$short) > 0L, ]
            shortoff0 <- shortoff0[grepl("^[^-+0-9]", shortoff0$short), ]
            if (nrow(shortoff0))
                shortoff <- rbind(shortoff, data.frame(name = tz, shortoff0))
        }
        return (shortoff)
    }

    if (!is.null(tz) && tz != "UTC") {
        nmoff <- .mk_shortoff(tz, d)
        if (nrow(nmoff)) {
            if (anyDuplicated(nmoff$short)) {
                dupl <- duplicated(nmoff$short, fromLast = TRUE)
                mes1 <- gettextf("ambiguous abbreviation %s for time zone %s",
                                 dQuote(nmoff$short[which.max(dupl)]), tz)
                mes2 <- gettextf("results for earlier dates may be incorrect")
                warning(paste0(mes1, "; ", mes2), call. = FALSE, domain = NA)
                nmoff <- nmoff[!dupl, ]
            }
            res <- match(tzs, nmoff$short)
            if (anyNA(res)) {
                unknown <- setdiff(tzs, nmoff$short)
                mes0 <- gettextf("NAs introduced")
                mes1 <- gettextf("unrecognised abbreviations for time zone %s: %s",
                                 tz, toString(dQuote(unknown)))
                mes2 <- gettextf("recognised abbreviations: %s",
                                 toString(dQuote(nmoff$short)))
                warning(paste0(mes0, "; ", mes1, "; ", mes2), call. = FALSE, domain = NA)
            }
            return (list(nmoff$off[res], tz))
        } else {
            mes1 <- gettextf("time zone abbreviations and/or UTC offsets not available")
            mes2 <- gettextf("using default values")
            warning(paste0(mes1, "; ", mes2), call. = FALSE, domain = NA)
        }
    }

    gmtoff <- sapply(unname(.shorttzs[tzs]), function(x) x[[1L]])
    ambg <- sapply(unname(.shorttzs[tzs]), function(x) x[[3L]])
    onms <- OlsonNames()
    def <- lapply(unname(.shorttzs[tzs]), function(x) intersect(x[[4L]], onms))
    alts <- lapply(unname(.shorttzs[tzs]), function(x) intersect(x[[5L]], onms))
    havedef <- as.logical(sapply(def, length))
    havealt <- as.logical(sapply(alts, length))
    ok0 <- !ambg
    ok1 <- ambg & !is.na(gmtoff) & havedef
    nok <- !ok0 & !ok1

    # guess tz
    if (any(ok0 & havedef)) {
        tz <- def[ok0 & havedef][[1L]]
    }
    if (is.null(tz) && any(ok0 & havealt)) {
        tz <- alts[ok0 & havealt][[1L]][1L]
    }
    if (is.null(tz) && any(ok1)) {
        tz <- def[ok1][[1L]]
    }
    tz <- .check_tz(tz)

    # handle ambiguities
    if (any(nok)) {
        ambgs <- tzs[nok]
        mes0 <- gettextf("NAs introduced")
        mes1 <- gettextf("ambiguous abbreviations: %s", toString(dQuote(ambgs)))
        mes2 <- gettextf("provide %s argument", sQuote("tz"))
        warning(paste0(mes0, "; ", mes1, "; ", mes2), call. = FALSE, domain = NA)
    }
    if (any(ok1)) {
        ambgs <- tzs[ok1]
        mes0 <- gettextf("ambiguous abbreviations: %s", toString(dQuote(ambgs)))
        off <- sapply(unname(.shorttzs[ambgs]), function(x) x[[2L]])
        map <- toString(paste0(dQuote(ambgs), " - ", off, " (",
                               dQuote(unlist(def[ok1])), ")"))
        mes1 <- gettextf("assuming: %s", map)
        mes2 <- gettextf("consider providing %s argument", sQuote("tz"))
        warning(paste0(mes0, "; ", mes1, "; ", mes2), call. = FALSE, domain = NA)
    }

    return (list(gmtoff, tz))
}



## NOTE: this list was automatically generated by the commented-out code underneath
.shorttzs <- list(
    ACDT = list(37800L, "+1030", FALSE, NA_character_,
                c("Australia/Adelaide", "Australia/Broken_Hill", "Australia/Darwin",
                  "Australia/North", "Australia/South", "Australia/Yancowinna")),
    ACST = list(34200L, "+0930", FALSE, NA_character_,
                c("Australia/Adelaide", "Australia/Broken_Hill", "Australia/Darwin",
                  "Australia/North", "Australia/South", "Australia/Yancowinna")),
    ADDT = list(-7200L, "-0200", FALSE, NA_character_,
                c("America/Goose_Bay")),
    ADT  = list(-10800L, "-0300", FALSE, NA_character_,
                c("America/Barbados", "America/Glace_Bay", "America/Goose_Bay",
                  "America/Halifax", "America/Martinique", "America/Moncton",
                  "America/Thule", "Atlantic/Bermuda", "Canada/Atlantic")),
    AEDT = list(39600L, "+1100", FALSE, "Australia/Sydney",
                c("Antarctica/Macquarie", "Australia/ACT", "Australia/Brisbane",
                  "Australia/Canberra", "Australia/Currie", "Australia/Hobart",
                  "Australia/Lindeman", "Australia/Melbourne", "Australia/NSW",
                  "Australia/Queensland", "Australia/Sydney", "Australia/Tasmania",
                  "Australia/Victoria")),
    AEST = list(36000L, "+1000", FALSE, "Australia/Sydney",
                c("Antarctica/Macquarie", "Australia/ACT", "Australia/Brisbane",
                  "Australia/Canberra", "Australia/Currie", "Australia/Hobart",
                  "Australia/LHI", "Australia/Lindeman", "Australia/Lord_Howe",
                  "Australia/Melbourne", "Australia/NSW", "Australia/Queensland",
                  "Australia/Sydney", "Australia/Tasmania", "Australia/Victoria")),
    AHDT = list(-32400L, "-0900", FALSE, NA_character_,
                c("America/Anchorage")),
    AHST = list(-36000L, "-1000", FALSE, NA_character_,
                c("America/Anchorage")),
    AKDT = list(-28800L, "-0800", FALSE, NA_character_,
                c("America/Anchorage", "America/Juneau", "America/Metlakatla",
                  "America/Nome", "America/Sitka", "America/Yakutat")),
    AKST = list(-32400L, "-0900", FALSE, NA_character_,
                c("America/Anchorage", "America/Juneau", "America/Metlakatla",
                  "America/Nome", "America/Sitka", "America/Yakutat")),
    AMT  = list(-13840L, "-0350", FALSE, NA_character_,
                c("America/Asuncion")),
    AST  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("America/Anchorage", "America/Anguilla", "America/Antigua",
                  "America/Aruba", "America/Barbados", "America/Blanc-Sablon",
                  "America/Curacao", "America/Dominica", "America/Glace_Bay",
                  "America/Goose_Bay", "America/Grand_Turk", "America/Grenada",
                  "America/Guadeloupe", "America/Halifax", "America/Kralendijk",
                  "America/Lower_Princes", "America/Marigot", "America/Martinique",
                  "America/Miquelon", "America/Moncton", "America/Montserrat",
                  "America/Port_of_Spain", "America/Puerto_Rico",
                  "America/Santo_Domingo", "America/St_Barthelemy", "America/St_Kitts",
                  "America/St_Lucia", "America/St_Thomas", "America/St_Vincent",
                  "America/Thule", "America/Tortola", "America/Virgin",
                  "Atlantic/Bermuda", "Canada/Atlantic")),
    AWDT = list(32400L, "+0900", FALSE, "Australia/Perth",
                c("Australia/Perth", "Australia/West")),
    AWST = list(28800L, "+0800", FALSE, "Australia/Perth",
                c("Australia/Perth", "Australia/West")),
    AWT  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("America/Anchorage", "America/Anguilla", "America/Antigua",
                  "America/Aruba", "America/Blanc-Sablon", "America/Curacao",
                  "America/Dominica", "America/Glace_Bay", "America/Grenada",
                  "America/Guadeloupe", "America/Halifax", "America/Kralendijk",
                  "America/Lower_Princes", "America/Marigot", "America/Moncton",
                  "America/Montserrat", "America/Port_of_Spain", "America/Puerto_Rico",
                  "America/St_Barthelemy", "America/St_Kitts", "America/St_Lucia",
                  "America/St_Thomas", "America/St_Vincent", "America/Tortola",
                  "America/Virgin", "Canada/Atlantic")),
    BDST = list(7200L, "+0200", FALSE, NA_character_,
                c("Europe/Belfast", "Europe/Gibraltar", "Europe/Guernsey",
                  "Europe/Isle_of_Man", "Europe/Jersey", "Europe/London", "GB-Eire",
                  "GB")),
    BDT  = list(-36000L, "-1000", FALSE, NA_character_,
                c("America/Adak", "America/Atka", "America/Nome")),
    BMT  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("Asia/Jakarta", "Atlantic/Bermuda", "Europe/Bucharest",
                  "Europe/Chisinau", "Europe/Tiraspol")),
    BST  = list(3600L, "+0100", TRUE, "Europe/London",
                c("America/Adak", "America/Atka", "America/La_Paz", "America/Nome",
                  "Europe/Belfast", "Europe/Gibraltar", "Europe/Guernsey",
                  "Europe/Isle_of_Man", "Europe/Jersey", "Europe/London", "GB-Eire",
                  "GB")),
    CAST = list(10800L, "+0300", FALSE, NA_character_,
                c("Africa/Juba", "Africa/Khartoum")),
    CAT  = list(7200L, "+0200", FALSE, NA_character_,
                c("Africa/Blantyre", "Africa/Bujumbura", "Africa/Gaborone",
                  "Africa/Harare", "Africa/Juba", "Africa/Khartoum", "Africa/Kigali",
                  "Africa/Lubumbashi", "Africa/Lusaka", "Africa/Maputo",
                  "Africa/Windhoek")),
    CDT  = list(-18000L, "-0500", TRUE, "America/Chicago",
                c("America/Bahia_Banderas", "America/Belize", "America/Cambridge_Bay",
                  "America/Cancun", "America/Chicago", "America/Chihuahua",
                  "America/Ciudad_Juarez", "America/El_Salvador", "America/Fort_Wayne",
                  "America/Guatemala", "America/Havana",
                  "America/Indiana/Indianapolis", "America/Indiana/Knox",
                  "America/Indiana/Marengo", "America/Indiana/Petersburg",
                  "America/Indiana/Tell_City", "America/Indiana/Vincennes",
                  "America/Indiana/Winamac", "America/Indianapolis", "America/Iqaluit",
                  "America/Kentucky/Louisville", "America/Kentucky/Monticello",
                  "America/Knox_IN", "America/Louisville", "America/Managua",
                  "America/Matamoros", "America/Menominee", "America/Merida",
                  "America/Mexico_City", "America/Monterrey",
                  "America/North_Dakota/Beulah", "America/North_Dakota/Center",
                  "America/North_Dakota/New_Salem", "America/Ojinaga",
                  "America/Pangnirtung", "America/Rainy_River", "America/Rankin_Inlet",
                  "America/Resolute", "America/Tegucigalpa", "America/Winnipeg",
                  "Asia/Beijing", "Asia/Chongqing", "Asia/Chungking", "Asia/Harbin",
                  "Asia/Macao", "Asia/Macau", "Asia/Shanghai", "Asia/Taipei",
                  "Canada/Central", "CST6CDT", "Cuba")),
    CEMT = list(10800L, "+0300", FALSE, NA_character_,
                c("Arctic/Longyearbyen", "Atlantic/Jan_Mayen", "Europe/Berlin",
                  "Europe/Copenhagen", "Europe/Oslo", "Europe/Stockholm")),
    CEST = list(7200L, "+0200", FALSE, "Europe/Warsaw",
                c("Africa/Algiers", "Africa/Ceuta", "Africa/Tripoli", "Africa/Tunis",
                  "Arctic/Longyearbyen", "Atlantic/Jan_Mayen", "CET",
                  "Europe/Amsterdam", "Europe/Andorra", "Europe/Athens",
                  "Europe/Belgrade", "Europe/Berlin", "Europe/Bratislava",
                  "Europe/Brussels", "Europe/Budapest", "Europe/Busingen",
                  "Europe/Chisinau", "Europe/Copenhagen", "Europe/Gibraltar",
                  "Europe/Kaliningrad", "Europe/Kiev", "Europe/Kyiv", "Europe/Lisbon",
                  "Europe/Ljubljana", "Europe/Luxembourg", "Europe/Madrid",
                  "Europe/Malta", "Europe/Minsk", "Europe/Monaco", "Europe/Oslo",
                  "Europe/Paris", "Europe/Podgorica", "Europe/Prague", "Europe/Riga",
                  "Europe/Rome", "Europe/San_Marino", "Europe/Sarajevo",
                  "Europe/Simferopol", "Europe/Skopje", "Europe/Sofia",
                  "Europe/Stockholm", "Europe/Tallinn", "Europe/Tirane",
                  "Europe/Tiraspol", "Europe/Uzhgorod", "Europe/Vaduz",
                  "Europe/Vatican", "Europe/Vienna", "Europe/Vilnius", "Europe/Warsaw",
                  "Europe/Zagreb", "Europe/Zaporozhye", "Europe/Zurich")),
    CET  = list(3600L, "+0100", FALSE, "Europe/Warsaw",
                c("Africa/Algiers", "Africa/Ceuta", "Africa/Tripoli", "Africa/Tunis",
                  "Arctic/Longyearbyen", "Atlantic/Jan_Mayen", "CET",
                  "Europe/Amsterdam", "Europe/Andorra", "Europe/Athens",
                  "Europe/Belgrade", "Europe/Berlin", "Europe/Bratislava",
                  "Europe/Brussels", "Europe/Budapest", "Europe/Busingen",
                  "Europe/Chisinau", "Europe/Copenhagen", "Europe/Gibraltar",
                  "Europe/Kaliningrad", "Europe/Kiev", "Europe/Kyiv", "Europe/Lisbon",
                  "Europe/Ljubljana", "Europe/Luxembourg", "Europe/Madrid",
                  "Europe/Malta", "Europe/Minsk", "Europe/Monaco", "Europe/Oslo",
                  "Europe/Paris", "Europe/Podgorica", "Europe/Prague", "Europe/Riga",
                  "Europe/Rome", "Europe/San_Marino", "Europe/Sarajevo",
                  "Europe/Simferopol", "Europe/Skopje", "Europe/Sofia",
                  "Europe/Stockholm", "Europe/Tallinn", "Europe/Tirane",
                  "Europe/Tiraspol", "Europe/Uzhgorod", "Europe/Vaduz",
                  "Europe/Vatican", "Europe/Vienna", "Europe/Vilnius", "Europe/Warsaw",
                  "Europe/Zagreb", "Europe/Zaporozhye", "Europe/Zurich")),
    CMT  = list(-16356L, "-0432", FALSE, NA_character_,
                c("America/La_Paz")),
    CST  = list(-21600L, "-0600", TRUE, "America/Chicago",
                c("America/Bahia_Banderas", "America/Belize", "America/Cambridge_Bay",
                  "America/Cancun", "America/Chicago", "America/Chihuahua",
                  "America/Ciudad_Juarez", "America/Costa_Rica", "America/El_Salvador",
                  "America/Fort_Wayne", "America/Guatemala", "America/Havana",
                  "America/Hermosillo", "America/Indiana/Indianapolis",
                  "America/Indiana/Knox", "America/Indiana/Marengo",
                  "America/Indiana/Petersburg", "America/Indiana/Tell_City",
                  "America/Indiana/Vevay", "America/Indiana/Vincennes",
                  "America/Indiana/Winamac", "America/Indianapolis", "America/Iqaluit",
                  "America/Kentucky/Louisville", "America/Kentucky/Monticello",
                  "America/Knox_IN", "America/Louisville", "America/Managua",
                  "America/Matamoros", "America/Mazatlan", "America/Menominee",
                  "America/Merida", "America/Mexico_City", "America/Monterrey",
                  "America/North_Dakota/Beulah", "America/North_Dakota/Center",
                  "America/North_Dakota/New_Salem", "America/Ojinaga",
                  "America/Pangnirtung", "America/Rainy_River", "America/Rankin_Inlet",
                  "America/Regina", "America/Resolute", "America/Swift_Current",
                  "America/Tegucigalpa", "America/Winnipeg", "Asia/Beijing",
                  "Asia/Chongqing", "Asia/Chungking", "Asia/Harbin", "Asia/Macao",
                  "Asia/Macau", "Asia/Shanghai", "Asia/Taipei", "Canada/Central",
                  "Canada/Saskatchewan", "CST6CDT", "Cuba")),
    CWT  = list(-18000L, "-0500", FALSE, "America/Chicago",
                c("America/Belize", "America/Chicago", "America/Fort_Wayne",
                  "America/Indiana/Indianapolis", "America/Indiana/Knox",
                  "America/Indiana/Marengo", "America/Indiana/Petersburg",
                  "America/Indiana/Tell_City", "America/Indiana/Vevay",
                  "America/Indiana/Vincennes", "America/Indiana/Winamac",
                  "America/Indianapolis", "America/Kentucky/Louisville",
                  "America/Kentucky/Monticello", "America/Knox_IN",
                  "America/Louisville", "America/Menominee", "America/Mexico_City",
                  "America/Rainy_River", "America/Winnipeg", "Canada/Central",
                  "CST6CDT")),
    EAT  = list(10800L, "+0300", FALSE, NA_character_,
                c("Africa/Addis_Ababa", "Africa/Asmara", "Africa/Asmera",
                  "Africa/Dar_es_Salaam", "Africa/Djibouti", "Africa/Juba",
                  "Africa/Kampala", "Africa/Khartoum", "Africa/Mogadishu",
                  "Africa/Nairobi")),
    EDT  = list(-14400L, "-0400", FALSE, "America/New_York",
                c("America/Cancun", "America/Detroit", "America/Fort_Wayne",
                  "America/Grand_Turk", "America/Indiana/Indianapolis",
                  "America/Indiana/Marengo", "America/Indiana/Petersburg",
                  "America/Indiana/Tell_City", "America/Indiana/Vevay",
                  "America/Indiana/Vincennes", "America/Indiana/Winamac",
                  "America/Indianapolis", "America/Iqaluit", "America/Jamaica",
                  "America/Kentucky/Louisville", "America/Kentucky/Monticello",
                  "America/Louisville", "America/Montreal", "America/Nassau",
                  "America/New_York", "America/Nipigon", "America/Pangnirtung",
                  "America/Port-au-Prince", "America/Santo_Domingo",
                  "America/Thunder_Bay", "America/Toronto", "Canada/Eastern",
                  "EST5EDT")),
    EEST = list(10800L, "+0300", FALSE, NA_character_,
                c("Africa/Cairo", "Asia/Amman", "Asia/Beirut", "Asia/Damascus",
                  "Asia/Famagusta", "Asia/Gaza", "Asia/Hebron", "Asia/Istanbul",
                  "Asia/Nicosia", "EET", "Egypt", "Europe/Athens", "Europe/Bucharest",
                  "Europe/Chisinau", "Europe/Helsinki", "Europe/Istanbul",
                  "Europe/Kaliningrad", "Europe/Kiev", "Europe/Kyiv",
                  "Europe/Mariehamn", "Europe/Minsk", "Europe/Moscow",
                  "Europe/Nicosia", "Europe/Riga", "Europe/Simferopol", "Europe/Sofia",
                  "Europe/Tallinn", "Europe/Tiraspol", "Europe/Uzhgorod",
                  "Europe/Vilnius", "Europe/Zaporozhye")),
    EET  = list(7200L, "+0200", FALSE, NA_character_,
                c("Africa/Cairo", "Africa/Tripoli", "Asia/Amman", "Asia/Beirut",
                  "Asia/Damascus", "Asia/Famagusta", "Asia/Gaza", "Asia/Hebron",
                  "Asia/Istanbul", "Asia/Nicosia", "EET", "Egypt", "Europe/Athens",
                  "Europe/Bucharest", "Europe/Chisinau", "Europe/Helsinki",
                  "Europe/Istanbul", "Europe/Kaliningrad", "Europe/Kiev",
                  "Europe/Kyiv", "Europe/Mariehamn", "Europe/Minsk", "Europe/Moscow",
                  "Europe/Nicosia", "Europe/Riga", "Europe/Simferopol", "Europe/Sofia",
                  "Europe/Tallinn", "Europe/Tiraspol", "Europe/Uzhgorod",
                  "Europe/Vilnius", "Europe/Zaporozhye")),
    EMT  = list(-26248L, "-0717", FALSE, NA_character_,
                c("Chile/EasterIsland")),
    EST  = list(-18000L, "-0500", FALSE, "America/New_York",
                c("America/Atikokan", "America/Cancun", "America/Cayman",
                  "America/Chicago", "America/Coral_Harbour", "America/Detroit",
                  "America/Fort_Wayne", "America/Grand_Turk",
                  "America/Indiana/Indianapolis", "America/Indiana/Knox",
                  "America/Indiana/Marengo", "America/Indiana/Petersburg",
                  "America/Indiana/Tell_City", "America/Indiana/Vevay",
                  "America/Indiana/Vincennes", "America/Indiana/Winamac",
                  "America/Indianapolis", "America/Iqaluit", "America/Jamaica",
                  "America/Kentucky/Louisville", "America/Kentucky/Monticello",
                  "America/Knox_IN", "America/Louisville", "America/Managua",
                  "America/Menominee", "America/Merida", "America/Montreal",
                  "America/Nassau", "America/New_York", "America/Nipigon",
                  "America/Panama", "America/Pangnirtung", "America/Port-au-Prince",
                  "America/Rankin_Inlet", "America/Resolute", "America/Santo_Domingo",
                  "America/Thunder_Bay", "America/Toronto", "Canada/Eastern", "EST",
                  "EST5EDT")),
    EWT  = list(-14400L, "-0400", FALSE, NA_character_,
                c("America/Detroit", "America/Iqaluit", "America/Montreal",
                  "America/Nassau", "America/New_York", "America/Nipigon",
                  "America/Pangnirtung", "America/Thunder_Bay", "America/Toronto",
                  "Canada/Eastern", "EST5EDT")),
    GMT  = list(0L, "+0000", FALSE, "Europe/London",
                c("Africa/Abidjan", "Africa/Accra", "Africa/Bamako", "Africa/Banjul",
                  "Africa/Bissau", "Africa/Conakry", "Africa/Dakar", "Africa/Freetown",
                  "Africa/Lome", "Africa/Monrovia", "Africa/Nouakchott",
                  "Africa/Ouagadougou", "Africa/Sao_Tome", "Africa/Timbuktu",
                  "America/Danmarkshavn", "Atlantic/Reykjavik", "Atlantic/St_Helena",
                  "Eire", "Etc/GMT-0", "Etc/GMT", "Etc/GMT+0", "Etc/GMT0",
                  "Etc/Greenwich", "Europe/Belfast", "Europe/Bratislava",
                  "Europe/Dublin", "Europe/Gibraltar", "Europe/Guernsey",
                  "Europe/Isle_of_Man", "Europe/Jersey", "Europe/London",
                  "Europe/Prague", "GB-Eire", "GB")),
    HDT  = list(-32400L, "-0900", FALSE, NA_character_,
                c("America/Adak", "America/Atka")),
    HKST = list(32400L, "+0900", FALSE, NA_character_,
                c("Asia/Hong_Kong")),
    HKT  = list(28800L, "+0800", FALSE, NA_character_,
                c("Asia/Hong_Kong")),
    HMT  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("America/Havana", "Asia/Dacca", "Asia/Dhaka", "Cuba")),
    HST  = list(-36000L, "-1000", FALSE, NA_character_,
                c("America/Adak", "America/Atka")),
    IDDT = list(14400L, "+0400", FALSE, NA_character_,
                c("Asia/Jerusalem", "Asia/Tel_Aviv")),
    IDT  = list(10800L, "+0300", FALSE, NA_character_,
                c("Asia/Gaza", "Asia/Hebron", "Asia/Jerusalem", "Asia/Tel_Aviv")),
    IST  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("Asia/Calcutta", "Asia/Gaza", "Asia/Hebron", "Asia/Jerusalem",
                  "Asia/Kolkata", "Asia/Tel_Aviv", "Eire", "Europe/Dublin")),
    JDT  = list(36000L, "+1000", FALSE, NA_character_,
                c("Asia/Tokyo")),
    JST  = list(32400L, "+0900", FALSE, "Asia/Tokyo",
                c("Asia/Hong_Kong", "Asia/Manila", "Asia/Pyongyang", "Asia/Seoul",
                  "Asia/Taipei", "Asia/Tokyo")),
    KDT  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("Asia/Seoul", "Asia/Seoul")),
    KMT  = list(7324L, "+0202", FALSE, NA_character_,
                c("Europe/Kiev", "Europe/Kyiv", "Europe/Uzhgorod", "Europe/Zaporozhye")),
    KST  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("Asia/Pyongyang", "Asia/Pyongyang", "Asia/Seoul", "Asia/Seoul")),
    LMT  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("Africa/El_Aaiun", "Africa/Juba", "Africa/Khartoum",
                  "Antarctica/Syowa", "Antarctica/Vostok", "Asia/Aden", "Asia/Almaty",
                  "Asia/Amman", "Asia/Anadyr", "Asia/Aqtau", "Asia/Aqtobe",
                  "Asia/Ashgabat", "Asia/Ashkhabad", "Asia/Atyrau", "Asia/Baku",
                  "Asia/Bishkek", "Asia/Brunei", "Asia/Dushanbe", "Asia/Jayapura",
                  "Asia/Kashgar", "Asia/Kuching", "Asia/Kuwait", "Asia/Magadan",
                  "Asia/Novokuznetsk", "Asia/Oral", "Asia/Qostanay", "Asia/Qyzylorda",
                  "Asia/Riyadh", "Asia/Samarkand", "Asia/Srednekolymsk",
                  "Asia/Tashkent", "Asia/Thimbu", "Asia/Thimphu", "Asia/Urumqi",
                  "Asia/Yerevan", "Europe/Astrakhan")),
    MDT  = list(-21600L, "-0600", FALSE, NA_character_,
                c("America/Bahia_Banderas", "America/Boise", "America/Cambridge_Bay",
                  "America/Chihuahua", "America/Ciudad_Juarez", "America/Creston",
                  "America/Denver", "America/Edmonton", "America/Hermosillo",
                  "America/Inuvik", "America/Mazatlan", "America/Mexico_City",
                  "America/North_Dakota/Beulah", "America/North_Dakota/Center",
                  "America/North_Dakota/New_Salem", "America/Ojinaga",
                  "America/Phoenix", "America/Regina", "America/Shiprock",
                  "America/Swift_Current", "America/Yellowknife", "Canada/Mountain",
                  "Canada/Saskatchewan")),
    MMT  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("Africa/Monrovia", "America/Managua", "Asia/Makassar",
                  "Asia/Ujung_Pandang", "Europe/Minsk")),
    MSD  = list(14400L, "+0400", FALSE, NA_character_,
                c("Europe/Chisinau", "Europe/Kaliningrad", "Europe/Kiev",
                  "Europe/Kirov", "Europe/Kyiv", "Europe/Minsk", "Europe/Moscow",
                  "Europe/Riga", "Europe/Simferopol", "Europe/Tallinn",
                  "Europe/Tiraspol", "Europe/Uzhgorod", "Europe/Vilnius",
                  "Europe/Volgograd", "Europe/Zaporozhye")),
    MSK  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("Europe/Chisinau", "Europe/Kaliningrad", "Europe/Kiev",
                  "Europe/Kirov", "Europe/Kirov", "Europe/Kyiv", "Europe/Minsk",
                  "Europe/Moscow", "Europe/Moscow", "Europe/Riga", "Europe/Simferopol",
                  "Europe/Simferopol", "Europe/Tallinn", "Europe/Tiraspol",
                  "Europe/Uzhgorod", "Europe/Vilnius", "Europe/Volgograd",
                  "Europe/Volgograd", "Europe/Zaporozhye")),
    MST  = list(-25200L, "-0700", FALSE, NA_character_,
                c("America/Bahia_Banderas", "America/Boise", "America/Cambridge_Bay",
                  "America/Chihuahua", "America/Ciudad_Juarez", "America/Creston",
                  "America/Dawson_Creek", "America/Dawson", "America/Denver",
                  "America/Edmonton", "America/Ensenada", "America/Fort_Nelson",
                  "America/Hermosillo", "America/Inuvik", "America/Mazatlan",
                  "America/Mexico_City", "America/North_Dakota/Beulah",
                  "America/North_Dakota/Center", "America/North_Dakota/New_Salem",
                  "America/Ojinaga", "America/Phoenix", "America/Regina",
                  "America/Santa_Isabel", "America/Shiprock", "America/Swift_Current",
                  "America/Tijuana", "America/Whitehorse", "America/Yellowknife",
                  "Canada/Mountain", "Canada/Saskatchewan", "Canada/Yukon")),
    MWT  = list(-21600L, "-0600", FALSE, NA_character_,
                c("America/Boise", "America/Cambridge_Bay", "America/Creston",
                  "America/Denver", "America/Edmonton", "America/North_Dakota/Beulah",
                  "America/North_Dakota/Center", "America/North_Dakota/New_Salem",
                  "America/Phoenix", "America/Regina", "America/Shiprock",
                  "America/Swift_Current", "America/Yellowknife", "Canada/Mountain",
                  "Canada/Saskatchewan")),
    NDDT = list(-5400L, "-0130", FALSE, NA_character_,
                c("America/St_Johns", "Canada/Newfoundland")),
    NDT  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("America/Goose_Bay", "America/St_Johns", "America/St_Johns",
                  "Canada/Newfoundland", "Canada/Newfoundland")),
    NST  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("America/Adak", "America/Atka", "America/Goose_Bay",
                  "America/Goose_Bay", "America/Nome", "America/St_Johns",
                  "America/St_Johns", "Canada/Newfoundland", "Canada/Newfoundland")),
    NWT  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("America/Adak", "America/Atka", "America/Goose_Bay", "America/Nome",
                  "America/St_Johns", "Canada/Newfoundland")),
    NZDT = list(46800L, "+1300", FALSE, NA_character_,
                c("Antarctica/McMurdo", "Antarctica/South_Pole")),
    NZMT = list(41400L, "+1130", FALSE, NA_character_,
                c("Antarctica/McMurdo", "Antarctica/South_Pole")),
    NZST = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("Antarctica/McMurdo", "Antarctica/McMurdo", "Antarctica/South_Pole",
                  "Antarctica/South_Pole")),
    PDT  = list(-25200L, "-0700", TRUE, "America/Los_Angeles",
                c("America/Dawson_Creek", "America/Dawson", "America/Ensenada",
                  "America/Fort_Nelson", "America/Inuvik", "America/Juneau",
                  "America/Los_Angeles", "America/Metlakatla", "America/Santa_Isabel",
                  "America/Sitka", "America/Tijuana", "America/Vancouver",
                  "America/Whitehorse", "Asia/Manila", "Canada/Pacific",
                  "Canada/Yukon")),
    PKST = list(21600L, "+0600", FALSE, NA_character_,
                c("Asia/Karachi")),
    PKT  = list(18000L, "+0500", FALSE, NA_character_,
                c("Asia/Karachi")),
    PMT  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("America/Paramaribo", "America/Paramaribo", "Asia/Pontianak")),
    PST  = list(-28800L, "-0800", TRUE, "America/Los_Angeles",
                c("America/Bahia_Banderas", "America/Boise", "America/Dawson_Creek",
                  "America/Dawson", "America/Ensenada", "America/Fort_Nelson",
                  "America/Hermosillo", "America/Inuvik", "America/Juneau",
                  "America/Los_Angeles", "America/Mazatlan", "America/Metlakatla",
                  "America/Santa_Isabel", "America/Sitka", "America/Tijuana",
                  "America/Vancouver", "America/Whitehorse", "Asia/Manila",
                  "Canada/Pacific", "Canada/Yukon")),
    PWT  = list(-25200L, "-0700", FALSE, NA_character_,
                c("America/Dawson_Creek", "America/Ensenada", "America/Fort_Nelson",
                  "America/Juneau", "America/Los_Angeles", "America/Metlakatla",
                  "America/Santa_Isabel", "America/Sitka", "America/Tijuana",
                  "America/Vancouver", "Canada/Pacific")),
    QMT  = list(-18840L, "-0514", FALSE, NA_character_,
                c("America/Guayaquil")),
    RMT  = list(5794L, "+0136", FALSE, NA_character_,
                c("Europe/Riga")),
    SAST = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("Africa/Johannesburg", "Africa/Johannesburg", "Africa/Maseru",
                  "Africa/Maseru", "Africa/Mbabane", "Africa/Mbabane",
                  "Africa/Windhoek", "Africa/Windhoek")),
    SDMT = list(-16800L, "-0440", FALSE, NA_character_,
                c("America/Santo_Domingo")),
    SMT  = list(NA_integer_, NA_character_, TRUE, NA_character_,
                c("America/Punta_Arenas", "America/Santiago", "Chile/Continental",
                  "Europe/Simferopol")),
    TBMT = list(10751L, "+0259", FALSE, NA_character_,
                c("Asia/Tbilisi")),
    TMT  = list(12344L, "+0325", FALSE, NA_character_,
                c("Asia/Tehran")),
    UTC  = list(0L, "+0000", FALSE, "UTC",
                c("Etc/UCT", "Etc/Universal", "Etc/UTC", "Etc/Zulu")),
    WAST = list(7200L, "+0200", FALSE, NA_character_,
                c("Africa/Ndjamena")),
    WAT  = list(3600L, "+0100", FALSE, NA_character_,
                c("Africa/Bangui", "Africa/Brazzaville", "Africa/Douala",
                  "Africa/Kinshasa", "Africa/Lagos", "Africa/Libreville",
                  "Africa/Luanda", "Africa/Malabo", "Africa/Ndjamena", "Africa/Niamey",
                  "Africa/Porto-Novo", "Africa/Sao_Tome", "Africa/Windhoek")),
    WEMT = list(7200L, "+0200", FALSE, NA_character_,
                c("Europe/Lisbon", "Europe/Madrid", "Europe/Monaco", "Europe/Paris")),
    WEST = list(3600L, "+0100", FALSE, NA_character_,
                c("Africa/Algiers", "Africa/Ceuta", "Atlantic/Canary",
                  "Atlantic/Faeroe", "Atlantic/Faroe", "Atlantic/Madeira",
                  "Europe/Amsterdam", "Europe/Brussels", "Europe/Lisbon",
                  "Europe/Luxembourg", "Europe/Madrid", "Europe/Monaco",
                  "Europe/Paris")),
    WET  = list(0L, "+0000", FALSE, NA_character_,
                c("Africa/Algiers", "Africa/Ceuta", "Atlantic/Azores",
                  "Atlantic/Canary", "Atlantic/Faeroe", "Atlantic/Faroe",
                  "Atlantic/Madeira", "Europe/Amsterdam", "Europe/Andorra",
                  "Europe/Brussels", "Europe/Lisbon", "Europe/Luxembourg",
                  "Europe/Madrid", "Europe/Monaco", "Europe/Paris")),
    WIB  = list(25200L, "+0700", FALSE, NA_character_,
                c("Asia/Jakarta", "Asia/Pontianak")),
    WIT  = list(32400L, "+0900", FALSE, NA_character_,
                c("Asia/Jayapura")),
    WITA = list(28800L, "+0800", FALSE, NA_character_,
                c("Asia/Makassar", "Asia/Pontianak", "Asia/Ujung_Pandang")),
    YDDT = list(-25200L, "-0700", FALSE, NA_character_,
                c("America/Dawson", "America/Whitehorse", "Canada/Yukon")),
    YDT  = list(-28800L, "-0800", FALSE, NA_character_,
                c("America/Juneau", "America/Yakutat")),
    YST  = list(-32400L, "-0900", FALSE, NA_character_,
                c("America/Dawson", "America/Whitehorse", "America/Yakutat",
                  "Canada/Yukon")),
    YWT  = list(-28800L, "-0800", FALSE, NA_character_,
                c("America/Dawson", "America/Whitehorse", "America/Yakutat",
                  "Canada/Yukon"))
)


# tt <- seq(-1483120800,
#           (1 + ceiling(as.numeric(Sys.time()) / 15778800)) * 15778800,
#           by = 15778800)
#
# tzinfo <- data.frame(name = character(), short = character(),
#                      off = character(), gmtoff = integer())
# for (tz in OlsonNames()) {
#     tzinfo0 <- data.frame(
#         short = format(as.POSIXct(tt, origin = "1970-01-01", tz = tz), "%Z"),
#         off = format(as.POSIXct(tt, origin = "1970-01-01", tz = tz), "%z"),
#         gmtoff = as.POSIXlt(tt, origin = "1970-01-01", tz = tz)$gmtoff,
#         stringsAsFactors = FALSE)
#     tzinfo0 <- unique(tzinfo0)
#     tzinfo0 <- data.frame(name = tz, tzinfo0, stringsAsFactors = FALSE)
#     tzinfo <- rbind(tzinfo, tzinfo0)
# }
#
# (tzinfo <- tzinfo[grepl("^[^-+0-9]", tzinfo$short),])
# (tzinfo <- tzinfo[order(tzinfo$short, tzinfo$gmtoff), ])
#
# # discretionary tz selection
# short2tz <- c(
#     AEDT  = "Australia/Sydney",     AEST  = "Australia/Sydney",
#     AWDT  = "Australia/Perth",      AWST  = "Australia/Perth",
#     CEST  = "Europe/Warsaw",        CET   = "Europe/Warsaw",
#     CWT   = "America/Chicago",
#     EDT   = "America/New_York",     EST   = "America/New_York",
#     GMT   = "Europe/London",
#     JST   = "Asia/Tokyo",
#     UTC   = "UTC",
#     # ambiguities
#     BST  =   "Europe/London",
#     CST  =   "America/Chicago",     CDT  =   "America/Chicago",
#     PDT  =   "America/Los_Angeles", PST  =   "America/Los_Angeles"
# )
# short2tz <- data.frame(short = names(short2tz), default = short2tz)
# short2tz <-merge(short2tz, tzinfo[, 1L:4L], by.x = rev(names(short2tz)),
#                  by.y = names(tzinfo)[1L:2L], all.x = TRUE)
# names(short2tz)[3:4] <- paste0("def.", names(short2tz)[3:4])
# short2tz
#
# ushortoff <- unique(tzinfo[, c("short", "gmtoff")])
# duplsh <- unique(ushortoff$short[duplicated(ushortoff$short)])
# (tzinfo <- data.frame(tzinfo, ambg = tzinfo$short %in% duplsh))
# (tzinfo <- merge(tzinfo, short2tz, all.x = TRUE))
#
# tzinfo[150:200, ]
# tzinfo[150:200 + 750, ]
#
# #
# ina <- tzinfo$ambg & is.na(tzinfo$default)
# tzinfo
# tzinfo$off[ina] <- NA_character_
# tzinfo$gmtoff[ina] <- NA_integer_
# tzinfo[150:200, ]
# tzinfo[150:200 + 750, ]
# #
# tzinfo
# inna <- !is.na(tzinfo$def.off)
# tzinfo$off[inna] <- tzinfo$def.off[inna]
# tzinfo$gmtoff[inna] <- tzinfo$def.gmtoff[inna]
# tzinfo[150:200, ]
# tzinfo[150:200 + 750, ]
#
# # "aggregate"
# uso <- tapply(tzinfo$name, tzinfo$short, function(x) toString(dQuote(x, "\"")))
# uso
# tzinfo_agg <- tzinfo[, c(1L, 4L:3L, 5L:6L)]
# str(tzinfo_agg)
# (tzinfo_agg <- unique(tzinfo_agg))
# (tzinfo_agg <- cbind(tzinfo_agg, tzs = uso[tzinfo_agg$short]))
#
#
#
# tzinfo_agg$short <- format(tzinfo_agg$short)
# .prettytz <- function(tab)
# {
#     res1 <- paste0(tab$short, " = list(")
#     res1 <- paste0(res1, if (is.na(tab$gmtoff)) "NA_integer_" else paste0(tab$gmtoff, "L"))
#     res1 <- paste0(res1, ", ", if (is.na(tab$off)) "NA_character_" else dQuote(tab$off, "\""))
#     res1 <- paste0(res1, ", ", as.character(tab$ambg))
#     res1 <- paste0(res1, ", ", if (is.na(tab$default)) "NA_character_" else dQuote(tab$default, "\""))
#     res1 <- paste0(res1, ",")
#     res20 <- tab[, "tzs"]
#     res20 <- sort(strsplit(res20, ", ")[[1L]])
#     res2 <- character()
#     while(length(res20)) {
#         nc <- nchar(res20) + 2L
#         cnc <- cumsum(nc)
#         ok <- cnc <= 70L
#         res2 <- c(res2, paste0(if (length(res2)) "  " else "c(", toString(res20[ok]),
#                                if (all(ok)) "))," else ","))
#         res20 <- res20[!ok]
#     }
#     res2 <- paste0("             ", res2)
#     res <- c(res1, res2)
#     res <- paste0("   ", res, "\n")
#     return (paste(res, collapse = ""))
# }
#
#
# cat(sapply(seq_len(nrow(tzinfo_agg)), function(i) .prettytz(tzinfo_agg[i, ])))

