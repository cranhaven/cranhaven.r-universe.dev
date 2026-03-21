## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = F, echo = F, results = "hide"------------------------------------
#  # When I use knitr to run this whole file, I need to
#  # "require" all the packages that are needed to run the code
#  # since knitr uses a new environment.  This is in contrast to
#  # running the code chunks separately which use all of the packages
#  # which are listed in the factorial2x2 package's DESCRIPTION file
#  # on the DEPENDS line
#  
#  # require(factorial2x2)
#  # require(survival)
#  # require(mvtnorm)

## ---- eval =   F--------------------------------------------------------------
#  time <- simdata[, 'time']    # follow-up time
#  event <- simdata[, 'event']  # event indicator
#  indA <- simdata[, 'indA']    # treatment A indicator
#  indB <- simdata[, 'indB']    # treatment B indicator
#  covmat <- simdata[, 6:10]    # baseline covariates
#  fac2x2analyze(time, event, indA, indB, covmat, alpha = 0.05, dig = 2, niter = 5)
#  # simdata[, 6:10] corresponds to the baseline covariates which include
#  # a history of cardiovascular disease (yes/no) and four indicator
#  # variables which correspond to which of 5 clinical centers enrolled each of the participants
#  $loghrAoverall
#  [1] -0.1170805         # overall A effect log hazard ratio (HR)
#  
#  $seAoverall
#  [1] 0.06258749         # standard error of overall A effect log HR
#  
#  $ZstatAoverall
#  [1] -1.87067           # Z-statistic for overall A effect
#  
#  $pvalAoverall
#  [1] 0.06139083         # nominal p-value for overall A effect
#  
#  $hrAoverall
#  [1] 0.8895135          # HR for overall A effect
#  
#  $ciAoverall
#  [1] 0.786823 1.005607  # 95% confidence interval for overall A effect HR
#  
#  $loghrAsimple
#  [1] -0.2112048         # simple A effect log HR
#  
#  $seAsimple
#  [1] 0.08655462         # standard error of simple A effect log HR
#  
#  $ZstatAsimple
#  [1] -2.440133          # Z-statistic for simple A effect
#  
#  $pvalAsimple
#  [1] 0.01468184         # nominal p-value for simple A effect
#  
#  $hrAsimple
#  [1] 0.8096082          # HR for simple A effect
#  
#  $ciAsimple
#  [1] 0.6832791 0.9592939 # 95% confidence interval for simple A effect HR
#  
#  $loghrABsimple
#  [1] -0.2766681         # simple AB effect log HR
#  
#  $seABsimple
#  [1] 0.08738966         # standard error of simple AB effect log HR
#  
#  $ZstatABsimple
#  [1] -3.165914          # Z-statistic for simple AB effect
#  
#  $pvalABsimple
#  [1] 0.001545967        # nominal p-value for simple AB effect
#  
#  $hrABsimple
#  [1] 0.7583061          # HR for simple AB effect
#  
#  $ciABsimple
#  [1] 0.6389355 0.8999785 # 95% confidence interval for simple AB effect HR
#  
#  $critEA3_A
#  [1] -2.31              # critical value for Equal Allocation 3 procedure for Family 1 hypotheses
#  
#  $sigEA3_A
#  [1] 0.02088815         # significance level corresponding to the critical value
#  
#  $resultEA3_A
#  [1] "accept overall A" "reject simple A"  "reject simple AB"  # hypothesis tests results
#  
#  $critPA2_A
#  [1] -2.13              # critical value for overall A effect for Proportional Allocation 2
#  
#  $sigPA2_A
#  [1] 0.03317161         # significance level corresponding to the critical value
#  
#  $critPA2_ab
#  [1] -2.24              # critical value for simple AB effect for Proportional Allocation 2
#  
#  $sigPA2_ab
#  [1] 0.02509092         # significance level corresponding to the critical value
#  
#  $resultPA2_A
#  [1] "accept overall A" "reject simple AB"  # hypothesis tests results
#  
#  $critEA2_A
#  [1] -2.22              # critical value for Equal Allocation 2 procedure
#  
#  $sigEA2_A
#  [1] 0.02641877         # significance level corresponding to critical value
#  
#  $resultEA2_A
#  [1] "reject simple A"  "reject simple AB"  # hypothesis tests results
#  
#  $corAa
#  [1] 0.7274961          # correlation between overall A and simple A logrank statistics
#  
#  $corAab
#  [1] 0.7164075          # correlation between overall A and simple AB logrank statistics
#  
#  $coraab
#  [1] 0.4572905          # correlation between simple A and simple AB logrank statistics
#  
#  $loghrBoverall
#  [1] -0.1664725         # overall B effect log hazard ratio (HR)
#  
#  $seBoverall
#  [1] 0.06273747         # standard error of overall B effect log HR
#  
#  $ZstatBoverall
#  [1] -2.653478          # Z-statistic for overall B effect
#  
#  $pvalBoverall
#  [1] 0.007966693        # nominal p-value for overll B effect
#  
#  $hrBoverall
#  [1] 0.8466461          # HR for overall B effect
#  
#  $ciBoverall
#  [1] 0.7486842 0.9574258 # 95% confidence interval for overall B effect HR
#  
#  $loghrBsimple
#  [1] -0.2673257         # simple B effect log HR
#  
#  $seBsimple
#  [1] 0.08711762         # standard error of simple B effect log HR
#  
#  $ZstatBsimple
#  [1] -3.068561          # Z-statistic for simple B effect log HR
#  
#  $pvalBsimple
#  [1] 0.002150925        # nominal p-value for simple B effect
#  
#  $hrBsimple
#  [1] 0.7654237          # HR for simple B effect
#  
#  $ciBsimple
#  [1] 0.6452766 0.9079416 # 95% confidence interval for simple B effect HR
#  
#  $critEA3_B
#  [1] -2.32              # critical value for Equal Allocation 3 procedure for Family 2 hypotheses
#  
#  $sigEA3_B
#  [1] 0.02034088         # significance level corresponding to the critical value
#  
#  $resultEA3_B
#  [1] "reject overall B" "reject simple B"  # hypothesis tests results
#  
#  $critPA2_B
#  [1] -2.13              # critical value for overall B effect for Proportional Allocation 2
#  
#  $sigPA2_B
#  [1] 0.03317161         # significance level corresponding to critical value
#  
#  $resultPA2_B
#  [1] "reject overall B" # hypothesis test result
#  
#  $critEA2_B
#  [1] -2.22              # critical value for Equal Allocation 2 procedure
#  
#  $sigEA2_B
#  [1] 0.02641877         # significance level corresponding to the critical value
#  
#  $resultEA2_B
#  [1] "reject simple B"  # hypothesis test result
#  
#  $corBb
#  [1] 0.7149066          # correlation between overall B and simple B logrank statistics
#  
#  $corBab
#  [1] 0.7171287          # correlation between overall B and simple AB logrank statistics
#  
#  $corbab
#  [1] 0.4575956          # correlation between simple B and simple AB logrank statistics
#  

## ---- eval =   F--------------------------------------------------------------
#  # read the COMBINE data into an R data frame
#  Combine <- read.table("c:\\combine_data.txt", header = T, nrows = 1226, na.strings ="",
#                        stringsAsFactors= T)
#  dim(Combine)
#  [1] 1226    9
#  
#  dimnames(Combine)[[2]]
#  [1] "ID"         "AGE"        "GENDER"     "T0_PDA"     "NALTREXONE"
#  [6] "THERAPY"    "site"       "relapse"    "futime"
#  
#  # create the baseline covariate variables
#  T0_PDA <- Combine[,"T0_PDA"]            # baseline percentage of days abstinent
#  site_1 <- Combine[,"site"] == "site_1"  # research site indicator variables
#  site_2 <- Combine[,"site"] == "site_2"
#  site_3 <- Combine[,"site"] == "site_3"
#  site_4 <- Combine[,"site"] == "site_4"
#  site_5 <- Combine[,"site"] == "site_5"
#  site_6 <- Combine[,"site"] == "site_6"
#  site_7 <- Combine[,"site"] == "site_7"
#  site_8 <- Combine[,"site"] == "site_8"
#  site_9 <- Combine[,"site"] == "site_9"
#  site_10 <- Combine[,"site"] == "site_10"
#  
#  # combine the covariates into a single covariate matrix
#  CombineCovMat <- cbind(T0_PDA, site_1, site_2, site_3, site_4, site_5, site_6,
#                           site_7, site_8, site_9, site_10)
#  
#  # define the other required variables
#  relapse <- Combine[,"relapse"]         # heavy drinking relapse indicator
#  futime <- Combine[,"futime"]           # time to first heavy drinking day or censoring
#  NALTREXONE <- Combine[,"NALTREXONE"]   # received naltrexone indicator
#  THERAPY <- Combine[,"THERAPY"]         # received cognitive behavioral intervention (CBI) indicator
#  
#  # reproduce the COMBINE analysis using fac2x2analyze
#  fac2x2analyze(futime, relapse, NALTREXONE, THERAPY, CombineCovMat, alpha = 0.025, dig = 4)
#  
#  $loghrAoverall
#  [1] -0.0847782              # log hazard rato estimate for the overall effect of naltrexone
#  
#  $seAoverall
#  [1] 0.06854294              # std error of the log HR estimate for the overall effect of naltrexone
#  
#  $ZstatAoverall
#  [1] -1.236863               # Z-statistic for the overall effect of naltrexone
#  
#  $pvalAoverall
#  [1] 0.2161381               # p-value for the overall effect of naltrexone
#  
#  $hrAoverall
#  [1] 0.918716                # hazard ratio estimate for the overall effect of naltrexone
#  
#  $ciAoverall
#  [1] 0.8032234 1.0508149     # corresponding 95% confidence interval
#  
#  $loghrAsimple
#  [1] -0.2517618              # log hazard rato estimate for the simple effect of naltrexone
#  
#  $seAsimple
#  [1] 0.09786137              # std error of the log HR estimate for the simple effect of naltrexone
#  
#  $ZstatAsimple
#  [1] -2.572637               # Z-statistic for the simple effect of naltrexone
#  
#  $pvalAsimple
#  [1] 0.0100927               # p-value for the simple effect of naltrexone
#  
#  $hrAsimple
#  [1] 0.7774299               # hazard ratio estimate for the simple effect of naltrexone
#  
#  $ciAsimple
#  [1] 0.6417413 0.9418083     # corresponding 95% confidence interval
#  
#  $loghrABsimple
#  [1] -0.09132675             # log hazard ratio estimate for the simple effect of naltrexone and CBI
#  
#  $seABsimple
#  [1] 0.09553005              # std error of the log HR estimate for the simple effect of naltrexone
#                              # and CBI
#  
#  $ZstatABsimple
#  [1] -0.9560003              # Z-statistic for the simple effect of naltrexone and CBI
#  
#  $pvalABsimple
#  [1] 0.3390721               # p-value for the simple effect of naltrexone and CBI
#  
#  $hrABsimple
#  [1] 0.9127194               # hazard ratio estimate for the simple effect of naltrexone and CBI
#  
#  $ciABsimple
#  [1] 0.7568686 1.1006624     # corresponding 95% confidence interval
#  
#  $critEA3_A
#  [1] -2.5811                 # critical value for the three tests in Table 4 to provide
#                              # two-sided 0.025 familywise error;
#                              # slightly larger in absolute terms than the
#                              # critical value -2.573 reported on p.1083 of Lin, Gong, et al.
#  $sigEA3_A
#  [1] 0.009848605
#  
#  $resultEA3_A
#  [1] "accept overall A" "accept simple A"  "accept simple AB"
#  

## ---- eval =   F--------------------------------------------------------------
#    n <- 4600          # total sample size
#    rateC <- 0.0445    # one year event rate in the control group
#    hrA <- 0.80        # simple A effect hazard ratio
#    hrB <- 0.80        # simple B effect hazard ratio
#    hrAB <- 0.72       # simple AB effect hazard ratio
#    mincens <- 4.0     # minimum censoring time in years
#    maxcens <- 8.4     # maximum censoring time in years
#    fac2x2design(n, rateC, hrA, hrB, hrAB, mincens, maxcens, dig = 2, alpha = 0.05)
#  
#  $events
#  [1] 954.8738         # expected number of events
#  
#  $evtprob             # event probabilities for the C, A, B, and AB groups, respectively
#      probC     probA     probB    probAB
#  0.2446365 0.2012540 0.2012540 0.1831806
#  
#  $powerEA3overallA
#  [1] 0.5861992        # Equal Allocation 3's power to detect the overall A effect
#  
#  $powerEA3simpleA
#  [1] 0.5817954        # Equal Allocation 3's power to detect the simple A effect
#  
#  $powerEA3simplAB
#  [1] 0.9071236        # Equal Allocation 3's power to detect the simple AB effect
#  
#  $powerEA3anyA
#  [1] 0.7060777        # Equal Allocation 3's power to detect either the overall A or simple A effects
#  
#  $powerPA2overallA
#  [1] 0.6582819        # Proportional Allocation 2's power to detect the overall A effect
#  
#  $powerPA2simpleAB
#  [1] 0.9197286        # Proportional Allocation 2's power to detect the simple AB effect
#  
#  $powerEA2simpleA
#  [1] 0.6203837        # Equal Allocation 2's power to detect the simple A effect
#  
#  $powerEA2simpleAB
#  [1] 0.9226679        # Equal Allocation 2's power to detect the simple AB effect
#  
#  $powerA
#  [1] 0.7182932        # power to detect the overall A effect at the two-sided 0.05 level
#  

