\dontrun{
# use test example from
#https://wiki.saucelabs.com/display/DOCS/JavaScript+Unit+Testing+Methods
platforms <- list(c("Windows 7", "firefox", "27"),
                    c("Linux", "googlechrome", "")
                    )
appUrl <- "https://saucelabs.com/test_helpers/front_tests/index.html"
framework <- "jasmine"
myAcc <- account()
myTest <- startJsUnitTests(myAcc, platforms = platforms, url = appUrl, framework = framework)

#> unlist(myTest, use.names = FALSE)
#[1] "bc8b9ef6e6184ed8a7e5270344115999" "bf43cef30bca429eaa2ed08da09dbdce"
testIds <- unlist(myTest, use.names = FALSE)
testRes <- getJsUnitTestStatus(myAcc,js_tests = testIds)

}
