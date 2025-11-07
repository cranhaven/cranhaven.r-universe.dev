context("base - locale")
# ###################################################################

test_that(".try_locale works correctly", {
    skip_on_cran()
    cl <- Sys.getlocale("LC_TIME")
    if (is.null(cl)) skip("locale information not available")

    err <- paste0("invalid ", sQuote("locale"),
                  " argument; character string expected")
    expect_error(.try_locale(123), err, fixed = TRUE)
    expect_error(.try_locale(c("Hsdfasdfs_Qwerty", "Hsdfasdfs_Qwerty")), err,
                 fixed = TRUE)
    expect_invisible(.try_locale(cl))
    expect_error(.try_locale("Hsdfasdfs_Qwerty"),
                 paste0("^operating system did not accept locale ",
                        dQuote("Hsdfasdfs_Qwerty")))
})


test_that("'.calendar_names' works correctly", {
    skip_on_cran()
    cl <- Sys.getlocale("LC_TIME")
    if (is.null(cl)) skip("locale information not available")

    en_locales <- c("C", "en_US", "en_GB", "English_United States",
                    "English_United Kingdom")
    for (lc in en_locales) {
        suppressWarnings(en_locale <- Sys.setlocale("LC_TIME", lc))
        if (en_locale != "") break
    }
    Sys.setlocale("LC_TIME", cl)

    if (en_locale != "") {
        cnames <- .calendar_names(en_locale)
        mn_ena <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                    "Oct", "Nov", "Dec")
        mn_enf <- c("January", "February", "March", "April", "May", "June", "July",
                    "August", "September", "October", "November", "December")
        wn_ena <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
        wn_enf <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                    "Saturday",  "Sunday")
        ap_en <- c("am", "pm")
        expect_equal(length(cnames), 40L)
        expect_equal(cnames[1L:12L], mn_ena)
        expect_equal(cnames[13L:24L], mn_enf)
        expect_equal(cnames[25L:31L], wn_ena)
        expect_equal(cnames[32L:38L], wn_enf)
        expect_equal(tolower(cnames[39L:40L]), ap_en)
    }

    pl_locales <- c("pl", "pl_PL", "Polish", "Polish_Poland")
    for (lc in pl_locales) {
        suppressWarnings(pl_locale <- Sys.setlocale("LC_TIME", lc))
        if (pl_locale != "") break
    }
    Sys.setlocale("LC_TIME", cl)

    if (pl_locale != "") {
        cnames <- .calendar_names(pl_locale)
        mn_pla <- c("sty", "lut", "mar", "kwi", "maj", "cze", "lip", "sie", "wrz",
                    "paź", "lis", "gru")
        mn_plf <- c("styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec",
                    "lipiec", "sierpień", "wrzesień", "październik", "listopad",
                    "grudzień")
        wn_pla <- c("pon", "wto", "śro", "czw", "pią", "sob", "nie")
        wn_plf <- c("poniedziałek", "wtorek", "środa", "czwartek", "piątek",
                    "sobota", "niedziela")
        expect_equal(length(cnames), 40L)
        # limit check to names with ASCII-only characters
        allascii <- c(1L:9L, 11:12L)
        expect_equal(cnames[allascii], mn_pla[allascii])
        allascii <- c(2L:3L, 5L:7L, 11L)
        # subtr to handle different gramatical forms
        expect_equal(substr(cnames[12L + allascii], 0L, 3L),
                         substr(mn_plf[allascii], 0L, 3L))
        allascii <- c(1L:2L, 4L, 6L:7L)
        expect_equal(cnames[24L + allascii], wn_pla[allascii])
        allascii <- c(2L, 4L, 6L:7L)
        expect_equal(cnames[31L + allascii], wn_plf[allascii])
        expect_equal(cnames[39L:40L], c("", ""))
    }
})

