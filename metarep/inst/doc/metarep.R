## ----instalation, eval=F, include=F , echo=F----------------------------------
#  devtools::install_github( "guido-s/meta"    , force=T )
#  devtools::install_github( "IJaljuli/metarep", force=T )

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = " "
)

## ----example_CD002943---------------------------------------------------------
library(metarep)
library(meta)
data(CD002943_CMP001)

m2943 <- metabin( event.e = N_EVENTS1, n.e = N_TOTAL1, 
                     event.c = N_EVENTS2, n.c = N_TOTAL2,
                     studlab = STUDY, comb.fixed = T , comb.random = F,
                     method = 'Peto', sm = CD002943_CMP001$SM[1],
                     data = CD002943_CMP001)

m2943

summary(m2943)

## ----RA Zaykin and bounds22---------------------------------------------------
(m2943.ra <- metarep(x = m2943 , u = 2 , common.effect = F ,t = 0.05 ,report.u.max = T))

## ----rvalue extraction--------------------------------------------------------
m2943.ra$r.value

## ----forest, fig.width=8.5, fig.height=3--------------------------------------
forest(m2943.ra, layout='revman5',digits.pval = 2 , test.overall = T )

## ----u_L bound----------------------------------------------------------------
find_umax(x = m2943 , common.effect = F,alternative = 'less',t = 0.05,confidence = 0.975)

## ----u bounds-----------------------------------------------------------------
find_umax(x = m2943 , common.effect = F,alternative = 'two-sided',t = 0.05,confidence = 0.95)

## ----RA_with_FE---------------------------------------------------------------
find_umax(x = m2943 , common.effect = T,alternative = 'two-sided', confidence = 0.95)

(m2943.raFE <- metarep(x = m2943 , u = 2 , common.effect = T ,report.u.max = T))



## ----RA_with_FE_forest, fig.width=8.5, fig.height=3---------------------------
find_umax(x = m2943 , common.effect = T,alternative = 'two-sided', confidence = 0.95)

(m2943.raFE <- metarep(x = m2943 , u = 2 , common.effect = T ,report.u.max = T))

forest(m2943.raFE, layout='revman5',digits.pval = 2 , test.overall = T )

