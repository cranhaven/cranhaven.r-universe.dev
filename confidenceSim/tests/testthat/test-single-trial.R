inputs <- list(
 outcome.type = "BINARY", # binary outcome data
 estimator.type = 'risk diff', # primary outcome is risk difference
 lmb.threshold = 0.1, # risk difference < 0.1 lacks meaningful benefit
 multiarm.mode='MONITOR FUTILITY', # only monitor for futility
 alpha = 0.0125, # fixed alpha threshold to determine treatment efficacy
 alloc.ratio = c(1,1), # allocation ratio
 num.per.block = c(1,1), # number per block for blocked allocation
 final.visit = 0, # time in days after which follow-up data becomes available
 ppm = rep(25, 15), # patients accrued each month for the entire trial period.
 looks =  214, # number of patients accrued at each look time, nmax = 214.
 perpetual=FALSE, # not a perpetual trial.
 resprate =  c(0.5, 0.6), # response rate for each arm
 lmb.conf.thres=0.95, # treatment arm is futility is the confidence in LMB is greater than 0.95
 special = paste0(0.5, '_', 0.6) # passing the response rates to special to add to the output
)
# run a single simulation with these settings

conf <- runSingleTrial(input=inputs, save.plot=FALSE, verbose=FALSE, save.text = FALSE)

testthat::expect_equal(length(conf), 17)
