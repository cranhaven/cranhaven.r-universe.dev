## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)
library(parallel)

## -----------------------------------------------------------------------------
names <- c('a','b','c','d')
term_n <- c(0,1,1,2)
tform <- c("loglin","lin","lin","plin")
modelform <- "M"
fir <- 0

a_n <- c(0.1, 0.1, 0.1, 0.1)

## -----------------------------------------------------------------------------
df <- data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
           "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
             "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
          "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0),
                      "a"=c(0,   1,   1,   0,   1,   0,   1),
                      "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
                      "c"=c(10,  11,  10,  11,  12,  9,   11),
                      "d"=c(0,   0,   0,   1,   1,   1,   1))
# For the interval case
time1 <- "Starting_Age"
time2 <- "Ending_Age"
event <- "Cancer_Status"

# Supposing we had left truncated data the following would change
time1 <- "Starting_Age"
time2 <- "%trunc%"

# and with right truncated data the following is used
time1 <- "%trunc%"
time2 <- "Ending_Age"

#setting back to normal
time1 <- "Starting_Age"
time2 <- "Ending_Age"

## -----------------------------------------------------------------------------
df$Person_Years <- df$Ending_Age -df$Starting_Age
pyr <- "Person_Years"
event <- "Cancer_Status"

## -----------------------------------------------------------------------------
keep_constant <- c(0,0,0,0)
der_iden <- 0

control=list("ncores"=2,'lr' = 0.75,'maxiter' = 100,'halfmax' = 5,'epsilon' = 1e-9,
             'dbeta_max' = 0.5,'deriv_epsilon' = 1e-9, 'abs_max'=1.0,'change_all'=TRUE,
             'dose_abs_max'=100.0,'verbose'=FALSE, 'ties'='breslow','double_step'=1)

## -----------------------------------------------------------------------------
# assuming the table of covariates is stored in a data.table "df"

e <- RunCoxRegression(df, time1, time2, event, names, term_n, tform, keep_constant,
                      a_n, modelform, fir, der_iden, control)
print(e)

#or a Poisson model regression
a_n <- c(0.1, 0.1, 0.1, 0.1) #a_n is updated when either regression is called
e <- RunPoissonRegression(df, pyr, event, names, term_n, tform, keep_constant, a_n,
                          modelform, fir, der_iden, control)
print(e)

