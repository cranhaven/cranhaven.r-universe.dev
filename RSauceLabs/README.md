RSauceLabs
==========================
| CRAN version       | Travis build status    | Coverage |
|:-------------:|:-------------:|:-------------:|
|  | [![Build Status](https://travis-ci.org/johndharrison/RSauceLabs.svg?branch=master)](https://travis-ci.org/johndharrison/RSauceLabs) | [![codecov](https://codecov.io/gh/johndharrison/RSauceLabs/branch/master/graph/badge.svg)](https://codecov.io/gh/johndharrison/RSauceLabs) |


 
##### *An R Wrapper for SauceLabs REST API*

### Introduction

RSauceLabs is an R Wrapper for SauceLabs REST API.

### Install


To install the current developement version of RSauceLabs run:

```
devtools::install_github("johndharrison/RSauceLabs")
```


### Getting started

The SauceLabs API requires an authenticated user for most API calls. RSauceLabs provides an `account` function which returns an authentication profile which other functions in the package utilise:

```
SLAccount <- account()
> SLAccount
Sauce Labs Account:
SauceLabs username: seleniumPipes
```

By default the function looks for the username and password for SauceLabs authentication as environment variables "SLUSER" and "SLPASS" respectively. Using the account credentials other functions can be called:

```
myJobs <- getJobs(SLAccount, limit = 2, getFullJobs = TRUE)
> myJobs$data
   browser_short_version                                                             video_url
1:                    27 https://saucelabs.com/jobs/8b06fed2992d4c6781b947f59ae1a58e/video.flv
2:                    48 https://saucelabs.com/jobs/144641aa1c594dcab6680f3e69103b15/video.flv
   creation_time browser_version         owner                               id record_screenshots
1:    1474738526           27.0. seleniumPipes 8b06fed2992d4c6781b947f59ae1a58e               TRUE
2:    1474738526    48.0.2564.97 seleniumPipes 144641aa1c594dcab6680f3e69103b15               TRUE
   record_video build passed public   end_time   status
1:         TRUE    NA   TRUE public 1474738551 complete
2:         TRUE    NA  FALSE public 1474738541 complete
                                                                           log_url start_time
1: https://saucelabs.com/jobs/8b06fed2992d4c6781b947f59ae1a58e/selenium-server.log 1474738526
2: https://saucelabs.com/jobs/144641aa1c594dcab6680f3e69103b15/selenium-server.log 1474738526
   proxied modification_time          name commands_not_successful consolidated_status
1:   FALSE        1474738551 jasmine tests                       2              passed
2:   FALSE        1474738541 jasmine tests                       1              failed
   assigned_tunnel_id error           os breakpointed      browser
1:                 NA    NA Windows 2008           NA      firefox
2:                 NA    NA        Linux           NA googlechrome
```



