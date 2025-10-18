## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(DescrTab2)
library(tidyverse)
options(print_format="html")

## ----wilcox.test 1 sample, results='asis'-------------------------------------

x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
dat_wilcox.test_1_sample <- tibble(diff = x-y)

descr(dat_wilcox.test_1_sample, test_options = c(nonparametric=TRUE))

## ----wilcox.test 1 sample SAS, results='asis', echo=FALSE---------------------
URS_ID <- "wilcox.test_1_sample"
sas_html <- here::here(
    "vignettes",
    "validation_report",
    URS_ID,
    paste0(URS_ID, "_example_", 1, ".html")
  )

if(file.exists(sas_html)){
  shiny::includeHTML(sas_html)
}

## ----wilcox.test 2 sample, results='asis'-------------------------------------

x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
group <- c(rep("Trt", length(x)), rep("Ctrl", length(y)))
dat_wilcox.test_2_sample <- tibble(var=c(x,y), group=group)

descr(dat_wilcox.test_2_sample, "group", test_options = c(nonparametric=TRUE))

## ----wilcox.test 2 sample SAS, results='asis', echo=FALSE---------------------
URS_ID <- "wilcox.test_2_sample"
sas_html <- here::here(
    "vignettes",
    "validation_report",
    URS_ID,
    paste0(URS_ID, "_example_", 1, ".html")
  )

if(file.exists(sas_html)){
  shiny::includeHTML(sas_html)
}

## ----kruskal.test, results='asis'---------------------------------------------

x <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
group <- c(rep("Trt", length(x)), rep("Ctrl", length(y)), rep("Placebo", length(z)))
dat_kruskal.test <- tibble(var=c(x,y,z), group=group)

descr(dat_kruskal.test, "group", test_options = c(nonparametric=TRUE)) 

## ----kruskal.test SAS, echo=FALSE, results='asis'-----------------------------
URS_ID <- "kruskal.test"
sas_html <- here::here(
    "vignettes",
    "validation_report",
    URS_ID,
    paste0(URS_ID, "_example_", 1, ".html")
  )


if(file.exists(sas_html)){
  shiny::includeHTML(sas_html)
}

## ----friedman.test, results='asis'--------------------------------------------

RoundingTimes <-
matrix(c(5.40, 5.50, 5.55,
         5.85, 5.70, 5.75,
         5.20, 5.60, 5.50,
         5.55, 5.50, 5.40,
         5.90, 5.85, 5.70,
         5.45, 5.55, 5.60,
         5.40, 5.40, 5.35,
         5.45, 5.50, 5.35,
         5.25, 5.15, 5.00,
         5.85, 5.80, 5.70,
         5.25, 5.20, 5.10,
         5.65, 5.55, 5.45,
         5.60, 5.35, 5.45,
         5.05, 5.00, 4.95,
         5.50, 5.50, 5.40,
         5.45, 5.55, 5.50,
         5.55, 5.55, 5.35,
         5.45, 5.50, 5.55,
         5.50, 5.45, 5.25,
         5.65, 5.60, 5.40,
         5.70, 5.65, 5.55,
         6.30, 6.30, 6.25),
       nrow = 22,
       byrow = TRUE,
       dimnames = list(1 : 22,
                       c("Round Out", "Narrow Angle", "Wide Angle")))

idx <- rep(1:22, 3)
dat <- tibble(var = c(RoundingTimes[,1], RoundingTimes[,2], RoundingTimes[,3]),
              group = c(rep("Round Out", 22), rep("Narrow Angle", 22), rep("Wide Angle", 22)))


descr(dat, "group", test_options = list(nonparametric=TRUE, indices=idx, paired=TRUE))


## ----friedman.test SAS, echo=FALSE, results='asis'----------------------------
URS_ID <- "friedman.test"
sas_html <- here::here(
    "vignettes",
    "validation_report",
    URS_ID,
    paste0(URS_ID, "_example_", 1, ".html")
  )


if(file.exists(sas_html)){
  shiny::includeHTML(sas_html)
}

## ----CochranQTest, results='asis'---------------------------------------------

d.frm <- DescTools::Untable(xtabs(c(6,2,2,6,16,4,4,6) ~ ., 
    expand.grid(rep(list(c("F","U")), times=3))), 
    colnames = LETTERS[1:3])

# rearrange to long shape    
d.long <- reshape(d.frm, varying=1:3, times=names(d.frm)[c(1:3)], 
                  v.names="resp", direction="long")
idx <- d.long$id
dat <- d.long[, 1:2] %>% mutate(time=as.character(time), resp=as.character(resp))

descr(dat, "time", test_options = list(indices=idx, paired=TRUE))


## ----CochranQTest SAS, echo=FALSE, results='asis'-----------------------------
URS_ID <- "CochraneQTest"
sas_html <- here::here(
    "vignettes",
    "validation_report",
    URS_ID,
    paste0(URS_ID, "_example_", 1, ".html")
  )


if(file.exists(sas_html)){
  shiny::includeHTML(sas_html)
}

## ----mcnemar.test, results='asis'---------------------------------------------
dat <- tibble::tibble(var = c(rep("Approve", 794), rep("Approve", 150), rep("Disapprove", 86), rep("Disapprove", 570),
                      rep("Approve", 794), rep("Disapprove", 150), rep("Approve", 86), rep("Disapprove", 570)),
              group= c(rep("first", 1600), rep("second",1600)))
              
descr(dat, "group", test_options = list(paired=TRUE, indices=c(1:1600, 1:1600)))
descr(dat, "group", test_options = list(paired=TRUE, exact=TRUE, indices=c(1:1600, 1:1600)))

dat <-
  tibble::tibble(x = c(
    rep("Approve", 794),
    rep("Approve", 150),
    rep("Disapprove", 86),
    rep("Disapprove", 570)
  ),
  y = c(
    rep("Approve", 794),
    rep("Disapprove", 150),
    rep("Approve", 86),
    rep("Disapprove", 570)
  ))



## ----mcnemar.test2------------------------------------------------------------
mcnemar.test(dat$x, dat$y, correct = FALSE)

## ----mcnemar.test SAS, echo=FALSE, results='asis'-----------------------------
URS_ID <- "mcnemar.test"
sas_html <- here::here(
    "vignettes",
    "validation_report",
    URS_ID,
    paste0(URS_ID, "_example_", 1, ".html")
  )


if(file.exists(sas_html)){
  shiny::includeHTML(sas_html)
}

## ----chisq.test1, results='asis'----------------------------------------------

dat <- tibble(gender=c(rep("F",sum(c(762, 327, 468)) ), rep("M", sum( c(484, 239, 477)))),
              party=c(rep("Democrat", 762), rep("Independent", 327), rep("Republican", 468),
                      rep("Democrat", 484), rep("Independent", 239), rep("Republican", 477)))
              
descr(dat, "gender")
descr(dat)

## ----chisq.test2--------------------------------------------------------------
chisq.test(dat$gender, dat$party)
chisq.test(table(dat$gender))
chisq.test(table(dat$party))

## ----chisq.test3, results='asis'----------------------------------------------
dat <- tibble(
  
  a = factor(c(0,
               0,
               1,
               1,
               0,
               0,
               0,
               0,
               0,
               0,
               1)),
  b = factor(c(1,
               1,
               1,
               1,
               1,
               1,
               1,
               0,
               0,
               1,
               0))

)
descr(dat, "b")

## ----chisq.test SAS, echo=FALSE, results='asis'-------------------------------
URS_ID <- "chisq.test"
sas_html <- here::here(
    "vignettes",
    "validation_report",
    URS_ID,
    paste0(URS_ID, "_example_", 1, ".html")
  )


if(file.exists(sas_html)){
  shiny::includeHTML(sas_html)
}

## ----t.test, results='asis'---------------------------------------------------

dat <- sleep[, c("extra", "group")]
              

descr(dat[, "extra"]) 
descr(dat, "group")
descr(dat, "group", test_options = list(paired=TRUE, indices=rep(1:10, 2)))


## ----t.test SAS, echo=FALSE, results='asis'-----------------------------------
URS_ID <- "t.test"
sas_html <- here::here(
    "vignettes",
    "validation_report",
    URS_ID,
    paste0(URS_ID, "_example_", 1, ".html")
  )


if(file.exists(sas_html)){
  shiny::includeHTML(sas_html)
}

## ----aov, results='asis'------------------------------------------------------

dat <- data.frame(
  y = npk$yield,
  P = ordered(gl(3, 24)),
  N = ordered(gl(3, 1, 24))
)
              
descr(dat[, c("y", "P")], "P") 
descr(dat[, c("y", "N")], "N") 


## ----aov SAS, echo=FALSE, results='asis'--------------------------------------
URS_ID <- "aov"
sas_html <- here::here(
    "vignettes",
    "validation_report",
    URS_ID,
    paste0(URS_ID, "_example_", 1, ".html")
  )


if(file.exists(sas_html)){
  shiny::includeHTML(sas_html)
}

## ----mixed, results='asis'----------------------------------------------------

dat <- nlme::Orthodont
dat2 <- nlme::Orthodont[1:64,]
dat2$Sex <- "Divers"
dat2$distance <- dat2$distance + c(rep(0.1*c(1,4,3,2), 10), 0.1*rep(c(0.4,2,1.5, 2.3), 6) )
dat2$Subject <- str_replace_all(dat2$Subject, "M", "D")
dat <- bind_rows(dat, dat2)
dat <- as_tibble(dat)


descr(dat[, c("Sex", "distance")], "Sex", test_options = list(paired=TRUE, indices=dat$Subject))


## ----mixed SAS, echo=FALSE, results='asis'------------------------------------
URS_ID <- "mixed"
sas_html <- here::here(
    "vignettes",
    "validation_report",
    URS_ID,
    paste0(URS_ID, "_example_", 1, ".html")
  )


if(file.exists(sas_html)){
  shiny::includeHTML(sas_html)
}

## ----boschloo1, results='asis'------------------------------------------------
dat <- tibble(gender=factor(c("M", "M", "M", "M", "M", "M", "F", "F", "F", "F", "F")),
              party=factor(c("A", "A", "B", "B", "B", "B", "A", "A", "A", "B", "B")))
descr(dat, "gender", test_options = c(exact=TRUE))

## ----boschloo2----------------------------------------------------------------
exact2x2::boschloo(3, 5, 2, 6, tsmethod="central")

## ----boschloo3----------------------------------------------------------------
exact2x2::boschloo(3, 5, 2, 6, tsmethod="minlike")
Exact::exact.test(table(dat), method="boschloo", to.plot = FALSE)

