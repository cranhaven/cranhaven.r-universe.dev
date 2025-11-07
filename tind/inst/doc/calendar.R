## ----echo=FALSE---------------------------------------------------------------
options(crayon.enabled = TRUE)
knitr::knit_hooks$set(output = function(x, options) {
    paste0(
    '<pre class="r-output"><code>',
    fansi::sgr_to_html(x = htmltools::htmlEscape(x), warn = FALSE),
    '</code></pre>'
    )
})

## ----color--------------------------------------------------------------------
library("tind")

## -----------------------------------------------------------------------------
today()
now()

## -----------------------------------------------------------------------------
now("Asia/Tokyo")
today("Asia/Tokyo")
now("Europe/Warsaw")
today("Europe/Warsaw")
now("Europe/London")
today("Europe/London")
now("America/New_York")
today("America/New_York")

## -----------------------------------------------------------------------------
(nw <- now())
year(nw)
quarter(nw)
month(nw)
month(nw, labels = TRUE)
month(nw, labels = TRUE, abbreviate = FALSE)
week(nw)
day(nw)
day_of_week(nw)
day_of_week(nw, labels = TRUE)
day_of_week(nw, labels = TRUE, abbreviate = FALSE)
day_of_year(nw)
hour(nw)
am(nw)
pm(nw)
minute(nw)
second(nw)

## -----------------------------------------------------------------------------
(tm <- tind(y = 2023:2024, m = c(10, 2), d = 29, H = 1, tz = "Europe/Warsaw"))
is.leap_year(tm)
days_in_year(tm)
weeks_in_year(tm)
days_in_quarter(tm)
days_in_month(tm)
hours_in_day(tm)
is.dst(tm)

## -----------------------------------------------------------------------------
(x <- today())
x + 0:3
x - 0:3
x - 3:0
x - as.date("2000-01-01")

## -----------------------------------------------------------------------------
(x <- today())
x %+y% -1:1
x + years(-1:1)
x %+m% -1:1
x + mnths(-1:1)
x %+d% -1:1 # same as x + -1:1
(x <- now())
x %-h% 3:0
x - hours(3:0)
x %-min% 3:0
x - mins(3:0)
x %-s% 3:0 # same as x - 3:0
x - secs(3:0) # same as x - 3:0

## -----------------------------------------------------------------------------
seq(as.month("2023-11"), as.month("2025-04"), by = 2)

## -----------------------------------------------------------------------------
(m <- as.month("2025-03"))
seq(as.date(m), as.date(m + 1) - 1)

## -----------------------------------------------------------------------------
(td <- today())
seq(as.quarter(td), td)
seq(td, as.quarter(td))

## -----------------------------------------------------------------------------
(x <- tind(y = 2025, m = 3, d = 30))
floor_t(x, "w")
ceiling_t(x, "w")
round_t(x, "w")
floor_t(x, "m")
ceiling_t(x, "m")
round_t(x, "m")
floor_t(x, "2m")
ceiling_t(x, "2m")
round_t(x, "2m")
floor_t(x, "q")
ceiling_t(x, "q")
round_t(x, "q")
floor_t(x, "y")
ceiling_t(x, "y")
round_t(x, "y")

## -----------------------------------------------------------------------------
(x <- date_time(x, H = "13:13:13.13"))
trunc_t(x, "s")
trunc_t(x, "min")
trunc_t(x, "h")
trunc_t(x, "d")
trunc_t(x, "w")
trunc_t(x, "m")
trunc_t(x, "q")
trunc_t(x, "y")

## -----------------------------------------------------------------------------
nth_dw_in_month(4, 4, 201911)

## -----------------------------------------------------------------------------
nth_dw_in_month(4, 4, 201911) + 1

## -----------------------------------------------------------------------------
last_dw_in_month(7, 201903)
last_dw_in_month(7, 201910)

## -----------------------------------------------------------------------------
hours_in_day(last_dw_in_month(7, 201903), "Europe/Warsaw")
hours_in_day(last_dw_in_month(7, 201910), "Europe/Warsaw")

## -----------------------------------------------------------------------------
easter(2020:2025)

## -----------------------------------------------------------------------------
calendar_US <- function(dd)
{
    dd <- as.tind(dd)
    y <- year(dd)
    m <- month(dd)
    d <- day(dd)
    newyear <- (m == 1) & (d == 1)
    martinlking <- (y >= 2000) & (m == 1) & (dd == nth_dw_in_month(3, 1, dd))
    presidentsday <- (m == 2) & (dd == nth_dw_in_month(3, 1, dd))
    memorialday <- (m == 5) & (dd == last_dw_in_month(1, dd))
    juneteenth <- (y >= 2021) & (m == 6) & (d == 19)
    independence <- (m == 7) & (d == 4)
    labor <- (m == 9) & (dd == nth_dw_in_month(1, 1, dd))
    columbus <- (m == 10) & (dd == nth_dw_in_month(2, 1, dd))
    veterans <- (m == 11) & (d == 11)
    thanksgiving <- (m == 11) & (dd == nth_dw_in_month(4, 4, dd))
    christmas <- (m == 12) & (d == 25)
    holiday <- newyear | martinlking | presidentsday |
               memorialday | juneteenth | independence |
               labor | columbus | veterans | thanksgiving |
               christmas
    # holiday names - a programming trick
    # names of holnms should be the same as names of logical vectors above
    names(holiday) <- rep("", length(holiday))
    holnms <- c(newyear = "New Year's Day",
                martinlking = "Birthday of Martin Luther King, Jr.",
                presidentsday = "Washington's Birthday",
                memorialday = "Memorial Day",
                juneteenth = "Juneteenth National Independence Day",
                independence = "Independence Day",
                labor = "Labor Day",
                columbus = "Columbus Day",
                veterans = "Veterans Day",
                thanksgiving = "Thanksgiving Day",
                christmas = "Christmas Day")
    lapply(names(holnms), function(nm) names(holiday)[get(nm)] <<- holnms[nm])
    # business days
    business <- !holiday & (day_of_week(dd) %in% 1:5)
    return (list(business = business, holiday = holiday))
}

## -----------------------------------------------------------------------------
calendar(2020, calendar_US)
calendar(as.year(today()), calendar_US)
calendar("2020-01", calendar_US)
calendar(calendar = calendar_US)

## -----------------------------------------------------------------------------
calendar_PL <- function(dd)
{
    dd <- as.tind(dd)
    y <- year(dd)
    m <- month(dd)
    d <- day(dd)
    # public holidays
    newyear <- (m == 1L) & (d == 1L)
    epiphany <- (y >= 2011L) & (m == 1L) & (d == 6L)
    easterd <- easter(dd) == dd
    eastermon <- easter(dd) + 1L == dd
    labour <- (m == 5L) & (d == 1L)
    constitution <- (m == 5L) & (d == 3L)
    pentecost <- easter(dd) + 49L == dd
    corpuschristi <- easter(dd) + 60L == dd
    assumption <- (m == 8L) & (d == 15L)
    allsaints <- (m == 11L) & (d == 1L)
    independence <- (m == 11L) & (d == 11L)
    christmaseve <- (m == 12L) & (d == 24L) & (y >= 2025)
    christmas <- (m == 12L) & (d == 25L)
    christmas2 <- (m == 12L) & (d == 26L)
    holiday <- newyear | epiphany |
               easterd | eastermon |
               labour | constitution |
               pentecost | corpuschristi |
               assumption |
               allsaints | independence |
               christmaseve | christmas | christmas2
    # holiday names
    names(holiday) <- rep("", length(holiday))
    holnms <- c(newyear = "New Year", epiphany = "Epiphany",
                easterd = "Easter", eastermon = "Easter Monday",
                labour = "Labour Day", constitution = "Constitution Day",
                pentecost = "Pentecost", corpuschristi = "Corpus Christi",
                assumption = "Assumption of Mary",
                allsaints = "All Saints Day", independence = "Independence Day",
                christmaseve = "Christmas Eve",
                christmas = "Christmas", christmas2 = "Christmas (2nd day)")
    lapply(names(holnms), function(nm) names(holiday)[get(nm)] <<- holnms[nm])
    # working/business days
    work <- !holiday & (day_of_week(dd) <= 5L)
    # other observances
    fatthursday <- easter(dd) - 52L == dd
    shrovetuesday <- easter(dd) - 47L == dd
    ashwednesday <- easter(dd) - 46L == dd
    goodfriday <- easter(dd) - 2L == dd
    primaaprilis <- (m == 4L) & (d == 1L)
    flagday <- (m == 5L) & (d == 2L)
    mothersday <- (m == 5L) & (d == 26L)
    childrensday <- (m == 6L) & (d == 1L)
    saintjohnseve <- (m == 6L) & (d == 23L)
    allsoulsday <- (m == 11L) & (d == 2L)
    saintandrewseve <- (m == 11L) & (d == 29L)
    saintnicholasday <- (m == 12L) & (d == 6L)
    christmaseve <- (m == 12L) & (d == 24L) & (y < 2025)
    newyeareve <- (m == 12L) & (d == 31L)
    other <- fatthursday | shrovetuesday | ashwednesday |
             goodfriday |
             primaaprilis |
             flagday |
             mothersday | childrensday | saintjohnseve |
             allsoulsday |
             saintandrewseve |
             saintnicholasday | christmaseve |
             newyeareve
    names(other) <- rep("", length(other))
    othernms <- c(fatthursday = "Fat Thursday",
                  shrovetuesday = "Shrove Tuesday",
                  ashwednesday = "Ash Wednesday",
                  goodfriday = "Good Friday",
                  primaaprilis = "All Fool's Day",
                  flagday = "Flag Day",
                  mothersday = "Mother's Day",
                  childrensday = "Children's Day",
                  saintjohnseve = "Saint John's Eve",
                  allsoulsday = "All Souls' Day",
                  saintandrewseve = "Saint Andrew's Eve",
                  saintnicholasday = "Saint Nicholas Day",
                  christmaseve = "Christmas Eve",
                  newyeareve = "New Year's Eve")
    lapply(names(othernms), function(nm) names(other)[get(nm)] <<- othernms[nm])

    return (list(work = work, holiday = holiday, other = other))
}

## -----------------------------------------------------------------------------
calendar(2020, calendar_PL)
calendar(as.year(today()), calendar_PL)
calendar("2020-06", calendar_PL)
calendar(calendar = calendar_PL)

## -----------------------------------------------------------------------------
calendar("2023-01", calendar = calendar_US)
bizday("2023-01-15", "p", calendar_US)
bizday("2023-01-15", "f", calendar_US)
bizday("2023-01-15", "mf2", calendar_US)
calendar("2023-08", calendar = calendar_PL)
bizday("2023-08-15", "p", calendar_PL)
bizday("2023-08-15", "f", calendar_PL)
bizday("2023-08-15", "mf2", calendar_PL)

## -----------------------------------------------------------------------------
m <- as.month("2023-01") + 0:11
data.frame(month = m, PL = bizdays_in_month(m, calendar_PL),
                      US = bizdays_in_month(m, calendar_US))

## -----------------------------------------------------------------------------
(d <- as.date("2024-05-05"))
year_frac(d)
year(d) + (day_of_year(d) - 1) / days_in_year(d)
as.quarter(d)
year_frac(as.quarter(d))
year(d) + (quarter(d) - 1) / 4
as.month(d)
year_frac(as.month(d))
year(d) + (month(d) - 1) / 12
as.week(d)
year_frac(as.week(d))
year(d) + (week(d) - 1) / weeks_in_year(d)

## -----------------------------------------------------------------------------
(imm <- nth_dw_in_month(3, 3, tind(y = 2025, m = 12) + 3 * (0:4)))
daycount_frac(imm[1L], imm[-1L], "30/360")
daycount_frac(imm[1L], imm[-1L], "30E/360")
daycount_frac(imm[1L], imm[-1L], "ACT/ACT")
daycount_frac(imm[1L], imm[-1L], "ACT/365F")
daycount_frac(imm[1L], imm[-1L], "ACT/360")

