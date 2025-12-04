## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(knitr)
library(ggplot2)
library(htmlTable)

## ---- message = FALSE,include = TRUE------------------------------------------
library(scipub)
data(psydat)

## -----------------------------------------------------------------------------
apastat(stats::cor.test(psydat$Age, psydat$Height), ci = TRUE)

## -----------------------------------------------------------------------------
apastat(stats::t.test(Height ~ Sex, data = psydat))

## -----------------------------------------------------------------------------
apastat(stats::lm(data = psydat, Height ~ Age + Sex))

## -----------------------------------------------------------------------------
apastat(stats::lm(data = psydat, Height ~ Age + Sex), var = "Age")

## ----results="asis"-----------------------------------------------------------
correltable(data = psydat, vars = c("Age", "Height", "iq"),tri="lower",html=TRUE)

## ----results="asis"-----------------------------------------------------------
correltable(data = psydat, vars = c("Age", "Height", "iq"), var_names = c("Age (months)", "Height (inches)", "IQ"), tri = "upper", colnum = TRUE,html=TRUE)

## ----results="asis"-----------------------------------------------------------
correltable(data = psydat, vars = c("Age", "Height", "iq"), var_names = c("Age (months)", "Height (inches)", "IQ"), tri = "upper", method = "spearman", use = "complete", cutempty = TRUE, colnum = TRUE,html=TRUE)

## ----results="asis"-----------------------------------------------------------
correltable(data = psydat, vars = c("Age", "Height", "iq"), var_names = c("Age (months)", "Height (inches)", "IQ"), vars2 = c("depressT", "anxT"), var_names2 = c("Depression T", "Anxiety T"),html=TRUE)

## ----results="asis"-----------------------------------------------------------
correltable(data = psydat, html=TRUE)

## ----results="asis"-----------------------------------------------------------
partial_correltable(data = psydat, vars = c("Age", "Height", "iq"), partialvars = c("Sex", "Income"), tri = "lower", html = TRUE)

## ----results="asis"-----------------------------------------------------------
partial_correltable(data = psydat, vars = c("Age", "Height", "iq"), var_names = c("Age (months)", "Height (inches)", "IQ"), partialvars = c("Sex", "Income"),tri = "upper", colnum = TRUE, html = TRUE)

## ----results="asis"-----------------------------------------------------------
partial_correltable(data = psydat, vars = c("Age", "Height", "iq"), var_names = c("Age (months)", "Height (inches)", "IQ"), partialvars = c("Sex", "Income"),tri = "upper", method = "spearman", use = "complete", cutempty = TRUE, colnum = TRUE, html = TRUE)

## ----results="asis"-----------------------------------------------------------
FullTable1(data = psydat, vars = c("Age", "Sex","Height", "depressT"),  html=TRUE)

## ----results="asis"-----------------------------------------------------------
FullTable1(data = psydat, vars = c("Age", "Height", "depressT"), strata = "Sex", html=TRUE)

## ----results="asis"-----------------------------------------------------------
FullTable1(data = psydat, vars = c("Age", "Sex","Height", "depressT"), var_names = c("Age (months)", "Sex","Height (inches)", "Depression T"), strata = "Income", stars = "stat",p_col = FALSE, html=TRUE)

## ----results="asis"-----------------------------------------------------------
FullTable1(data = psydat, strata = "Sex",stars = "name",p_col = FALSE, html=TRUE)

## ----results="asis"-----------------------------------------------------------
tmp <- FullTable1(data = psydat,
   vars = c("Age", "Height", "depressT"), strata = "Sex")
   tmp$caption <- "Write your own caption"
   print(htmlTable::htmlTable(tmp$table, useViewer=T, rnames=F,caption=tmp$caption, pos.caption="bottom"))

## ----results="asis"-----------------------------------------------------------
temp <- data.frame(iq=psydat$iq, iq_winsor=winsorZ(psydat$iq), iq_outlier = winsorZ_find(psydat$iq))
summary(temp)
ggplot(temp[!is.na(temp$iq),], aes(x=iq, y=iq_winsor)) + geom_point(aes(color=iq_outlier),alpha=.7) + geom_line() + theme_bw()

## ----results="asis"-----------------------------------------------------------
gg_groupplot(data=psydat, x=Sex, y=depressT)

## ----results="asis"-----------------------------------------------------------
gg_groupplot(data=psydat, x=Income, y=depressT) + facet_wrap(~Sex) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

