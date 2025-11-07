context("custom calendars")
# ###################################################################

# test sample size
NN <- 100L


# test samples
y <- sample(1990L:2020L, NN, replace = TRUE)
m <- sample.int(12L, size = NN, replace = TRUE)
d <- sample.int(31L, size = NN, replace = TRUE)
dd <- suppressWarnings(tind(y = y, m = m, d = d))




# Polish calendar for tests
.calendar_PL <- function(dd)
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
    christmas <- (m == 12L) & (d == 25L)
    christmas2 <- (m == 12L) & (d == 26L)
    holiday <- newyear | epiphany |
               easterd | eastermon |
               labour | constitution |
               pentecost | corpuschristi |
               assumption |
               allsaints | independence |
               christmas | christmas2
    # holiday names
    names(holiday) <- rep("", length(holiday))
    holnms <- c(newyear = "New Year", epiphany = "Epiphany",
                easterd = "Easter", eastermon = "Easter Monday",
                labour = "Labour Day", constitution = "Constitution Day",
                pentecost = "Pentecost", corpuschristi = "Corpus Christi",
                assumption = "Assumption of Mary",
                allsaints = "All Saints Day", independence = "Independence Day",
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
    christmaseve <- (m == 12L) & (d == 24L)
    newyeareve <- (m == 12L) & (d == 31L)
    other <- fatthursday | shrovetuesday | ashwednesday |
             goodfriday |
             primaaprilis |
             flagday |
             mothersday |childrensday | saintjohnseve |
             allsoulsday |
             christmaseve |
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
                  christmaseve = "Christmas Eve",
                  newyeareve = "New Year's Eve")
    lapply(names(othernms), function(nm) names(other)[get(nm)] <<- othernms[nm])

    return (list(work = work, holiday = holiday, other = other))
}


.calendar_PL1 <- function(d) .calendar_PL(d)[[1L]]
.calendar_PL2 <- function(d) .calendar_PL(d)[1L:2L]
.calendar_PLa <- function(d)
{
    res <- .calendar_PL(d)
    names(res[[2L]]) <- NULL
    return (res)
}
.calendar_PLb <- function(d)
{
    res <- .calendar_PL(d)
    names(res[[3L]]) <- NULL
    return (res)
}


test_that("'.eval_clndr' works correctly", {
    expect_equal(.eval_clndr(dd, .calendar_PL, bd.only = TRUE),
                     as.vector(.calendar_PL1(dd)))
    expect_equal(.eval_clndr(dd, .calendar_PL1, bd.only = TRUE),
                     as.vector(.calendar_PL1(dd)))
    expect_equal(.eval_clndr(dd, .calendar_PL2, bd.only = TRUE),
                     as.vector(.calendar_PL1(dd)))
    res0 <- .eval_clndr(dd, .calendar_PL)
    res1 <- .eval_clndr(dd, .calendar_PL1)
    res2 <- .eval_clndr(dd, .calendar_PL2)
    expect_true(is.list(res0) && (length(res0) == 3L))
    expect_true(is.list(res1) && (length(res1) == 3L))
    expect_true(is.list(res2) && (length(res2) == 3L))
    expect_equal(res0[[1L]], res1[[1L]])
    expect_equal(res0[[1L]], res2[[1L]])
    expect_equal(res0[[2L]], res2[[2L]])
    expect_equal(res1[[3L]], rep(FALSE, NN))
    expect_equal(res2[[3L]], rep(FALSE, NN))
    expect_equal(res1[[2L]], rep(FALSE, NN))
    errinvfun <- paste0("invalid ", sQuote("calendar"),
                        " argument; expected a function")
    expect_error(.eval_clndr(dd, 4), errinvfun)
    errerr <- paste0("^error while evaluating function given as ", sQuote("calendar"), ": ")
    expect_error(.eval_clndr(dd, function(d) (day_of_week__(d) <= 5L)), errerr)
    errinvret <- paste0("invalid return from function given as ", sQuote("calendar"))
    .calendar_PL_err <- function(d) head(.calendar_PL(d)[[1L]], -1L)
    expect_error(.eval_clndr(dd, .calendar_PL_err), errinvret, fixed = TRUE)
    .calendar_PL_err <- function(d) as.integer(.calendar_PL(d)[[1L]])
    expect_error(.eval_clndr(dd, .calendar_PL_err), errinvret, fixed = TRUE)
    .calendar_PL_err <- function(d)
    {
        res <- .calendar_PL(d)
        res[[1L]] <- head(res[[1L]], -1L)
        return (res)
    }
    expect_error(.eval_clndr(dd, .calendar_PL_err), errinvret, fixed = TRUE)
    .calendar_PL_err <- function(d)
    {
        res <- .calendar_PL(d)
        res[[2L]] <- head(res[[2L]], -1L)
        return (res)
    }
    expect_error(.eval_clndr(dd, .calendar_PL_err), errinvret, fixed = TRUE)
})


test_that("'eval_calendar' works correctly", {
    dd <- today() + 0:100
    res0 <- eval_calendar(dd, .calendar_PL)
    res1 <- eval_calendar(dd, .calendar_PL1)
    res2 <- eval_calendar(dd, .calendar_PL2)
    expect_equal(names(res0), c("bizdays", "holidays", "otherobs"))
    expect_equal(names(res1), c("bizdays", "holidays", "otherobs"))
    expect_equal(names(res2), c("bizdays", "holidays", "otherobs"))
    expect_true(all(sapply(res0, is.tind)))
    expect_true(all(sapply(res0, function(r) all(r %in% dd))))
    expect_equal(res0[[1L]], res1[[1L]])
    expect_equal(res0[[1L]], res2[[1L]])
    expect_equal(res0[[2L]], res2[[2L]])
    expect_equal(res1[[2L]], tind(type = "d"))
    expect_equal(res1[[3L]], tind(type = "d"))
    expect_equal(res2[[3L]], tind(type = "d"))
    res0. <- .eval_clndr(dd, .calendar_PL)
    expect_equal(res0[[1L]], dd[res0.[[1L]]])
    expect_true(all(res0[[2L]] == dd[res0.[[2L]]]))
    expect_true(all(res0[[3L]] == dd[res0.[[3L]]]))
    expect_equal(names(res0[[2L]]), names(res0.[[2L]])[res0.[[2L]]])
    expect_equal(names(res0[[3L]]), names(res0.[[3L]])[res0.[[3L]]])
    err <- ".*calendar.* missing"
    expect_error(eval_calendar(dd), err)
    err <- paste0("invalid ", sQuote("d"), " argument; ",
                  "expected a sequence of consecutive dates")
    expect_error(eval_calendar(dd[-2L], .calendar_PL), err, fixed = TRUE)
    expect_error(eval_calendar(tind(length = 1, type = "d"), err))
    err <- paste0("^invalid time index type: ", dQuote("[a-z]"), " \\([- a-z]+\\); ",
                  "expected: ", dQuote("d"), " \\(date\\)$")
    expect_error(eval_calendar(now(), .calendar_PL), err)
})


test_that("'calendar' works correctly", {
    have_crayon <- requireNamespace("crayon", quietly = TRUE) && crayon::has_color()
    # years
    paty <- "^ +[0-9]{4}( \\[[- _[:alnum:]]+\\])? +$"
    patm <- "^( +[[:alpha:]]{3,5} +)+$"
    patw <- "^([ ]{4}( [[:alpha:]]{2}){7})([ ]{5}( [[:alpha:]]{2}){7})*$"
    patd <- paste0("^((  [0-9]{2}(   |  [0-9]| [0-9]{2}){7})|[ ]{25})",
                "((   [0-9]{2}(   |  [0-9]| [0-9]{2}){7})|[ ]{26})*$")
    pattd <- "\\[( [0-9]|[0-9]{2})\\]"
    pattd0 <- "\\[( [0-9]|[0-9]{2})\\]$"
    # 1
    cap <- capture_output_lines(calendar(as.year(today()), name = "this year"))
    if (have_crayon) cap <- crayon::strip_style(cap)
    iy <- grepl(paty, cap)
    im <- grepl(patm, cap)
    iw <- grepl(patw, cap)
    itd <- grepl(pattd, cap)
    expect_true(sum(itd) == 1L)
    cap <- sub(pattd0, " \\1", cap)
    cap <- sub(pattd, " \\1 ", cap)
    expect_true(all(diff(nchar(cap)) == 0L))
    id <- grepl(patd, cap)
    expect_equal(which(iy), 1L)
    expect_equal(which(iw), which(im) + 1L)
    expect_true(all(diff(which(im)) >= 5L))
    expect_true(all((iy + im + iw + id) == 1L))
    # 2
    cap <- capture_output_lines(calendar(2020, .calendar_PL))
    if (have_crayon) cap <- crayon::strip_style(cap)
    expect_true(all(diff(nchar(cap)) == 0L))
    iy <- grepl(paty, cap)
    im <- grepl(patm, cap)
    iw <- grepl(patw, cap)
    id <- grepl(patd, cap)
    expect_equal(which(iy), 1L)
    expect_equal(which(iw), which(im) + 1L)
    expect_true(all(diff(which(im)) >= 5L))
    expect_true(all((iy + im + iw + id) == 1L))
    # months
    patym <- "^ +[[:alpha:]]{3,5} [0-9]{4}( \\[[- _[:alnum:]]+\\])? +$"
    patw <- "^([ ]{4}( [[:alpha:]]{2}){7})$"
    patd <- "^((  [0-9]{2}(   |  [0-9]| [0-9]{2}){7})|[ ]{25})(  \\| .+)?$"
    # 1
    cap <- capture_output_lines(calendar("2022-12"))
    if (have_crayon) cap <- crayon::strip_style(cap)
    expect_true(all(diff(nchar(cap)) == 0L))
    iym <- grepl(patym, cap)
    iw <- grepl(patw, cap)
    id <- grepl(patd, cap)
    expect_equal(which(iym)[1L], 1L)
    expect_equal(which(iw), which(iym) + 1L)
    expect_equal(length(which(iym)), 1L)
    expect_true(all((iym + iw + id) == 1L))
    # 2
    cap <- capture_output_lines(calendar(calendar = .calendar_PL))
    if (have_crayon) cap <- crayon::strip_style(cap)
    iym <- grepl(patym, cap)
    iw <- grepl(patw, cap)
    itd <- grepl(pattd, cap)
    expect_true(sum(itd) == 1L)
    cap <- sub(pattd0, " \\1", cap)
    cap <- sub(pattd, " \\1 ", cap)
    id <- grepl(patd, cap)
    expect_equal(which(iym)[1L], 1L)
    expect_equal(which(iw), which(iym) + 1L)
    expect_equal(length(which(iym)), 2L)
    expect_true(all((iym + iw + id) == 1L))
    # 3
    cap <- capture_output_lines(calendar(now(), calendar = .calendar_PL))
    if (have_crayon) cap <- crayon::strip_style(cap)
    iym <- grepl(patym, cap)
    iw <- grepl(patw, cap)
    itd <- grepl(pattd, cap)
    expect_true(sum(itd) == 1L)
    cap <- sub(pattd0, " \\1", cap)
    cap <- sub(pattd, " \\1 ", cap)
    id <- grepl(patd, cap)
    expect_equal(which(iym)[1L], 1L)
    expect_equal(which(iw), which(iym) + 1L)
    expect_equal(length(which(iym)), 1L)
    expect_true(all((iym + iw + id) == 1L))
    # 4
    cap <- capture_output_lines(calendar("2022-06", calendar = .calendar_PLa))
    if (have_crayon) cap <- crayon::strip_style(cap)
    iym <- grepl(patym, cap)
    iw <- grepl(patw, cap)
    cap <- sub(pattd0, " \\1", cap)
    cap <- sub(pattd, " \\1 ", cap)
    id <- grepl(patd, cap)
    expect_equal(which(iym)[1L], 1L)
    expect_equal(which(iw), which(iym) + 1L)
    expect_equal(length(which(iym)), 1L)
    expect_true(all((iym + iw + id) == 1L))
    # 5
    cap <- capture_output_lines(calendar("2022-06", calendar = .calendar_PLb))
    if (have_crayon) cap <- crayon::strip_style(cap)
    iym <- grepl(patym, cap)
    iw <- grepl(patw, cap)
    cap <- sub(pattd0, " \\1", cap)
    cap <- sub(pattd, " \\1 ", cap)
    id <- grepl(patd, cap)
    expect_equal(which(iym)[1L], 1L)
    expect_equal(which(iw), which(iym) + 1L)
    expect_equal(length(which(iym)), 1L)
    expect_true(all((iym + iw + id) == 1L))
    # too long title
    patym <- "^ +[[:alpha:]]{3,5} [0-9]{4}( \\[[- _[:alnum:]]+\\.\\.\\.\\])?$"
    cap <- capture_output_lines(calendar("2022-12", name = "12345678901234"))[1L]
    if (have_crayon) cap <- crayon::strip_style(cap)
    expect_true(grepl(patym, cap))
    cap <- capture_output_lines(calendar("2022-12", name = "1234567890"))[1L]
    if (have_crayon) cap <- crayon::strip_style(cap)
    expect_false(grepl(patym, cap))
    # invalid name
    err <- paste0("invalid ", sQuote("name"), " argument; character string expected")
    expect_error(calendar("2022-12", name = letters[1L:2L]), err)
    # invalid type or ym arg
    err <- paste0("invalid ", sQuote("ym"), " argument")
    expect_error(calendar(as.time(now()), name = letters[1L:2L]), err)
})

