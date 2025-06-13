\dontrun{
myAcc <- account()
jobActivity <- getRealTimeJobActivity(myAcc)
#> jobActivity$concurrency$self$allowed
#$manual
#[1] 5
#
#$mac
#[1] 5
#
#$overall
#[1] 5
#
#$real_device
#[1] 0
userActivity <- getUserActivity(myAcc)
#> userActivity$subaccounts$rsaucelabs
#$`in progress`
#[1] 0
#
#$all
#[1] 0
#
#$queued
#[1] 0
userAccUsage <- getUserAccountUsage(myAcc)
#> userAccUsage
#user_name      date no_of_jobs vm_minutes
#1: seleniumPipes 2016-8-12          2        239
#2: seleniumPipes 2016-8-13         65       6399
#3: seleniumPipes 2016-8-15         36       7235
#4: seleniumPipes 2016-8-16          7       1101
}
