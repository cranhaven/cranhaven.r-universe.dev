## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(knitr)
library(kableExtra)

## ----setup, echo=FALSE--------------------------------------------------------
library(qacBase)
df <- data.frame(sex=c(1,2,1,2,2,2),
                
                 race=c("b", "w", "a", "b", "w", "h"),
                 outcome=c("better", "worse", "same", "same", "better", "worse"),
                 Q1=c(20, 30, 44, 15, 50, 99),
                 Q2=c(15, 23, 18, 86, 99, 35),
                 age=c(12, 20, 33, 55, 30, 100),
                 rating =c(1,2,5,3,4,5))

## ---- echo=FALSE--------------------------------------------------------------
kbl(df) %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

## -----------------------------------------------------------------------------
df <- recodes(data=df, vars="sex", 
               from=c(1,2), to=c("Male", "Female"))

## ---- echo=FALSE--------------------------------------------------------------
kbl(df) %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

## -----------------------------------------------------------------------------
df <- recodes(data=df, vars="race", 
              from=c("w", "b", "a", "h"), 
              to=c("White", "Other", "Other", "Other"))

## ---- echo=FALSE--------------------------------------------------------------
kbl(df) %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

## -----------------------------------------------------------------------------
df <- recodes(data=df, vars="outcome", 
              from=c("better", "same", "worse"), 
              to=c(1, 0, 0))

## ---- echo=FALSE--------------------------------------------------------------
kbl(df) %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

## -----------------------------------------------------------------------------
df <- recodes(data=df, vars=c("Q1", "Q2"), 
              from=c(86, 99), to=NA)

## ---- echo=FALSE--------------------------------------------------------------
kbl(df) %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

## -----------------------------------------------------------------------------
df <- recodes(data=df, vars="age", 
              from=c("$ <   20 | $ >  90", 
                     "$ >=  20 & $ <= 30",
                     "$ >   30 & $ <= 50",
                     "$ >   50 & $ <= 90"), 
              to=c(NA, "Younger", "Middle Aged", "Older"))

## ----eval=FALSE---------------------------------------------------------------
#  df <- recodes(data=df, vars="age",
#                from=c("$ < 20", "$ <= 30", "$ <= 50", "$ <= 90", "$ > 90"),
#                to=  c(NA, "Younger", "Middle Aged", "Older", "NA"))

## ---- echo=FALSE--------------------------------------------------------------
kbl(df) %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

## -----------------------------------------------------------------------------
df <- recodes(data=df, vars="rating", from=1:5, to=5:1)

## ---- echo=FALSE--------------------------------------------------------------
kbl(df) %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

