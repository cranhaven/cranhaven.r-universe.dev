\dontrun{
myAcc <- account()
getSauceLabsStatus(myAcc)
#$wait_time
#[1] 1580.536
#
#$service_operational
#[1] TRUE
#
#$status_message
#[1] "Basic service status checks passed."
supportedPlatforms <- getSupportedPlatforms(myAcc)
supportedPlatforms[os == "Linux" & api_name == "chrome" & short_version > 44
                   , .(api_name, long_version)]
#api_name  long_version
#1:   chrome 45.0.2454.85.
#2:   chrome  46.0.2490.71
#3:   chrome  47.0.2526.73
#4:   chrome  48.0.2564.97
getAppiumEolDates(myAcc)
#$`1.4.0`
#[1] "2016-04-09 PDT"
#
#$`1.4.3`
#[1] "2016-04-09 PDT"
#....
}
