## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fwildclusterboot)

## ---- error = FALSE, warning = FALSE, message = FALSE-------------------------
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(2352342)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(23325)

# load data set voters included in fwildclusterboot
data(voters)

# estimate the regression model via lm
lm_fit <- lm(
  proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
  data = voters
)

# model with interaction
lm_fit_interact <- lm(
  proposition_vote ~ treatment + ideology1 + log_income:Q1_immigration , 
  data = voters
)


## -----------------------------------------------------------------------------
# boottest on an object of type lm
boot_lm <- boottest(
  lm_fit, 
  clustid = "group_id1",
  param = "treatment",
  B = 9999
)

## -----------------------------------------------------------------------------
#names(coef(lm_fit_interact))
boot_lm_interact <- boottest(
  lm_fit_interact,
  clustid = "group_id1",
  param = "log_income:Q1_immigration1",
  B = 9999
)

## -----------------------------------------------------------------------------
boot_multi <- boottest(
  lm_fit, 
  clustid = "group_id1",
  param = c("treatment", "ideology1"),
  R = c(0.6, 0.2), 
  r = 0.02, 
  B = 9999
)

## -----------------------------------------------------------------------------
# fwildclusterboot's internal summary() method
summary(boot_lm)
summary(boot_multi)

if(requireNamespace("modelsummary")){
  # summary via the modelsummary package
  library(modelsummary)
  msummary(list(boot_lm, boot_lm_interact), 
            estimate = "{estimate} ({p.value})", 
           statistic = "[{conf.low}, {conf.high}]")  
}



## -----------------------------------------------------------------------------
plot(boot_lm)

## -----------------------------------------------------------------------------
boot_lm <- boottest(
  lm_fit,
  clustid = c("group_id1", "group_id2"), 
  param = "treatment",
  B = 9999
)
summary(boot_lm)

## -----------------------------------------------------------------------------
boot_lm <- boottest(
  lm_fit,
  param = "treatment",
  B = 9999
)
summary(boot_lm)
boot_lm$engine

## -----------------------------------------------------------------------------
boot_lm_rade <- boottest(
  lm_fit, 
  clustid = c("group_id1", "group_id2"), 
  param = "treatment", 
  B = 999,
  type = "rademacher")
boot_lm_webb <- boottest(
  lm_fit, 
  clustid = c("group_id1", "group_id2"), 
  param = "treatment", 
  B = 999,
  type = "webb"
)

if(requireNamespace("modelsummary")){
  library(modelsummary)
  msummary(list(boot_lm_rade, boot_lm_webb), 
          estimate = "{estimate} ({p.value})", 
         statistic = "[{conf.low}, {conf.high}]")
}

## -----------------------------------------------------------------------------
boot_lm_5 <- boottest(
  lm_fit, 
  clustid = c("group_id1"),
  param = "treatment", B = 9999, 
  sign_level = 0.05
)
boot_lm_10 <- boottest(
  lm_fit, 
  clustid = c("group_id1"),
  param = "treatment", B = 9999, 
  sign_level = 0.10
)

if(requireNamespace("modelsummary")){
  library(modelsummary)
  msummary(list(boot_lm_5, boot_lm_10), 
          estimate = "{estimate} ({p.value})", 
         statistic = "[{conf.low}, {conf.high}]")
}

## -----------------------------------------------------------------------------
boot_lm1 <- boottest(
  lm_fit, 
  clustid = c("group_id1", "group_id2"), 
  param = "treatment",
  B = 9999, 
  bootcluster = "min"
)

boot_lm2 <- boottest(
  lm_fit, 
  clustid = c("group_id1", "group_id2"), 
  param = "treatment",
  B = 9999, 
  bootcluster = "group_id1"
)

if(requireNamespace("modelsummary")){
  library(modelsummary)
  msummary(list(boot_lm1, boot_lm2), 
          estimate = "{estimate} ({p.value})", 
         statistic = "[{conf.low}, {conf.high}]")
}

## -----------------------------------------------------------------------------

if(requireNamespace("fixest")){
  # estimate the regression model via feols
  library(fixest)
  feols_fit <- feols(
    proposition_vote ~ treatment + ideology1 + log_income | 
      Q1_immigration , 
    data = voters
  )
  boot_feols <- boottest(
    feols_fit, 
    clustid = "group_id1", 
    param = "treatment", 
    B = 9999, 
    fe = "Q1_immigration"
  )
}


## -----------------------------------------------------------------------------
boot_min <- boottest(
  lm_fit,
  clustid = c("group_id1", "group_id2"), 
  param = "treatment", 
  B = 9999, 
  bootcluster = "min"
)
boot_var <- boottest(
  lm_fit,
  clustid = c("group_id1", "group_id2"), 
  param = "treatment", 
  B = 9999, 
  bootcluster = "group_id1"
)
boot_2var <- boottest(
  lm_fit,
  clustid = c("group_id1", "group_id2"), 
  param = "treatment", 
  B = 9999, 
  bootcluster = c("group_id1", "Q1_immigration")
)

if(requireNamespace("modelsummary")){
  library(modelsummary)
  msummary(model = list(boot_min, boot_2var), 
         estimate = "{estimate} ({p.value})", 
         statistic = "[{conf.low}, {conf.high}]")
}



## -----------------------------------------------------------------------------
# regression with weights / WLS
lm_w_fit <- lm(
  proposition_vote ~ treatment + ideology1 + log_income,
  weights = voters$weights,
  data = voters
)

boot_w_lm <- boottest(
  lm_w_fit, 
  clustid = "group_id1", 
  param = "treatment", 
  B = 9999
)


## ---- eval = FALSE------------------------------------------------------------
#  boot_lm <- boottest(
#    lm_fit,
#    clustid = "group_id1",
#    param = "treatment",
#    B = 9999,
#    nthreads = 2
#  )

## ---- eval = FALSE, message = FALSE, warning = FALSE--------------------------
#  boot_lm <- boottest(
#    lm_fit,
#    clustid = "group_id1",
#    param = "treatment",
#    B = 9999,
#    engine = "WildBootTests.jl"
#  )
#  tidy(boot_lm)
#  #             term   estimate statistic    p.value   conf.low conf.high
#  #1 1*treatment = 0 0.07290769  3.709435 0.00060006 0.03326969 0.1134117

## ---- eval = FALSE, message = FALSE, warning = FALSE--------------------------
#  setBoottest_engine("WildBootTests.jl")

## ---- eval = FALSE, message = FALSE, warning = FALSE--------------------------
#  library(ivreg)
#  
#  data("SchoolingReturns", package = "ivreg")
#  
#  # drop all NA values from SchoolingReturns
#  SchoolingReturns <- na.omit(SchoolingReturns)
#  ivreg_fit <- ivreg(
#    log(wage) ~ education + age + ethnicity + smsa + south + parents14 |
#                nearcollege + age  + ethnicity + smsa + south + parents14,
#    data = SchoolingReturns)
#  
#  
#  boot_ivreg <- boottest(
#    object = ivreg_fit,
#    B = 999,
#    param = "education",
#    clustid = "kww",
#    type = "mammen",
#    impose_null = TRUE
#  )
#  tidy(boot_ivreg)
#  #              term  estimate statistic   p.value    conf.low conf.high
#  # 1 1*education = 0 0.0638822  1.043969 0.2482482 -0.03152655 0.2128746

## ---- eval = FALSE, message = FALSE, warning = FALSE--------------------------
#  library(clubSandwich)
#  R <- clubSandwich::constrain_zero(2:3, coef(lm_fit))
#  wboottest <-
#  mboottest(
#    object = lm_fit,
#    clustid = "group_id1",
#    B = 999,
#    R = R
#  )
#  tidy(wboottest)
#  #   teststat p_val
#  # 1 8.469086     0

## -----------------------------------------------------------------------------
data <- 
fwildclusterboot:::create_data(
  N = 1000, 
  N_G1 = 20, 
  icc1 = 0.81,
  N_G2 = 10,
  icc2 = 0.01, 
  numb_fe1 = 10,
  numb_fe2 = 10, 
  seed = 8769
)

# oneway clustering 
feols_fit <- fixest::feols(
  proposition_vote ~ treatment + ideology1 + log_income,
  data = data, 
  cluster = ~group_id1, 
  ssc = fixest::ssc(adj = TRUE, 
                    cluster.adj = TRUE, 
                    cluster.df = 'conventional')
  )
        
feols_tstats <- fixest::coeftable(
  feols_fit
  )[c("treatment", "log_income", "ideology1"), 3]

boot_tstats <- 
lapply(c("treatment", "log_income", "ideology1"), function(x){
  boot1 <- fwildclusterboot::boottest(
    feols_fit, 
    clustid = c("group_id1"),
    B = 999, 
    param = x, 
    ssc = fwildclusterboot::boot_ssc(
      adj = TRUE, 
      cluster.adj = TRUE, 
      cluster.df = 'conventional'),  
      impose_null = TRUE)$t_stat
})        

df <- cbind(feols_tstats, unlist(boot_tstats))
colnames(df) <- c("feols tstat", "boottest tstat")
df


## ---- eval = FALSE------------------------------------------------------------
#  library(bench)
#  
#  dt <- fwildclusterboot:::create_data(
#    N = 10000,
#    N_G1 = 250,
#    icc1 = 0.01,
#    N_G2 = 10,
#    icc2 = 0.01,
#    numb_fe1 = 10,
#    numb_fe2 = 10,
#    seed = 7645
#  )
#  
#  lm_fit <- lm(
#    proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
#    data = dt
#  )
#  
#  res <-
#  bench::mark(
#    "R" = boottest(lm_fit,
#             clustid = "group_id1",
#             param = "treatment",
#             B = 9999,
#             engine = "R",
#             nthreads = 4),
#    "R-lean" = boottest(lm_fit,
#             clustid = "group_id1",
#             param = "treatment",
#             B = 9999,
#             engine = "R-lean",
#             nthreads = 4),
#    "WildBootTests.jl" =
#      boottest(lm_fit,
#             clustid = "group_id1",
#             param = "treatment",
#             B = 9999,
#             engine = "WildBootTests.jl"),
#    iterations = 1,
#    check = FALSE
#  )
#  
#  res
#  

