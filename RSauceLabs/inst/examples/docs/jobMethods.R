\dontrun{
myAcc <- account()
myJobs <- getJobs(myAcc)
#> myJobs$data[1,]
#id
#1: 4152e0a185f945bfa43e091eef1e7c30
myJobs <- getJobs(myAcc, getFullJobs = TRUE)
#> myJobs$data[1,.(id, browser)]
#id      browser
#1: 4152e0a185f945bfa43e091eef1e7c30 googlechrome
testId <- myJobs$data[1, id]

#> myJobs$data[1,.(build, passed)]
#build passed
#1:    24  FALSE

# update this job
updateJob(myAcc, jobID = testId, passed = TRUE, build = 20)
myJobs <- getJobs(myAcc, getFullJobs = TRUE)
#> myJobs$data[1,.(build, passed)]
#build passed
#1:    20   TRUE
# deleteJob(myAcc, jobID = testId)
stopJob(myAcc, jobID = testId)

jobAssets <- getJobAssetNames(myAcc, jobID = testId)
#> jobAssets[["selenium-log"]]
#[1] "selenium-server.log"

jobLog <- getJobAssetFiles(myAcc, jobID = testId)
# deleteJobAssets(myAcc, jobID = testId)
}

