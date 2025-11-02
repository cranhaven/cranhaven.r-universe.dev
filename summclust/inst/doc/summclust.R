## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(summclust)

## ----example, warning = FALSE, message = FALSE, out.width="50%", out.height="50%"----
library(summclust)
library(lmtest)
library(haven)

url <- "http://www.stata-press.com/data/r9/nlswork.dta"
if (httr::http_error(url)) {
  stop("No internet connection or data source broken. Sorry about that!")
  return(NULL)
} else {
  message("downloading the 'nlswork' dataset.")
  nlswork <- read_dta(url)
}



# drop NAs at the moment
nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr", "union", "race", "msp", "ind_code")]
nlswork <- na.omit(nlswork)

lm_fit <- lm(
  ln_wage ~ as.factor(grade) + as.factor(age) + as.factor(birth_yr) + union +  race + msp,
  data = nlswork)

summclust_res <- summclust(
  obj = lm_fit,
  cluster = ~ind_code,
  params = c("msp", "union")
)

# CRV3-based inference - exactly matches output of summclust-stata
tidy(summclust_res)

summary(summclust_res)

## ---- warning = FALSE, message = FALSE, out.width="50%"-----------------------
plot(summclust_res)

## ---- warning = FALSE, message=FALSE------------------------------------------
library(lmtest)

vcov3J <- 
  vcov_CR3J(
    lm_fit, 
    cluster = ~ ind_code
  )

all.equal(
  vcov3J, 
  summclust_res$vcov
)

df <- length(summclust_res$cluster) - 1

# with lmtest
CRV1 <- lmtest::coeftest(lm_fit, sandwich::vcovCL(lm_fit, ~ind_code), df = df)
CRV3 <- lmtest::coeftest(lm_fit, vcov3J, df = df)

CRV1[c("union", "race", "msp"),]
CRV3[c("union", "race", "msp"),]

stats::confint(CRV1)[c("union", "race", "msp"),]
stats::confint(CRV3)[c("union", "race", "msp"),]

## -----------------------------------------------------------------------------
library(fixest)

feols_fit <- feols(
  ln_wage ~ i(grade) + i(age) + i(birth_yr) + union +  race + msp,
  data = nlswork
)

# Store vcov into the fixest object
feols_fit <- summary(
  feols_fit, 
  vcov = vcov_CR3J(feols_fit, cluster = ~ ind_code)
)

# Now it just works with fixest functions
fixest::coeftable(feols_fit, keep = c("msp", "union", "race"))

