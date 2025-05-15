## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

getInfo <- function(what = "Suggests") {
  text <- packageDescription("rgnoisefilt")[what][[1]]
  text <- gsub("\n", ", ", text, fixed = TRUE)
  text <- gsub(">=", "$\\\\ge$", text, fixed = TRUE)
  eachPkg <- strsplit(text, ", ", fixed = TRUE)[[1]]
  eachPkg <- gsub(",", "", eachPkg, fixed = TRUE)
  #out <- paste("\\\**", eachPkg[order(tolower(eachPkg))], "}", sep = "")
  #paste(out, collapse = ", ")
  length(eachPkg)
}

## ----install1-----------------------------------------------------------------
#install.packages("rgnoisefilt")

## ----install2-----------------------------------------------------------------
library(rgnoisefilt)

## ----document1----------------------------------------------------------------
help(regIPF)

## ----example 1----------------------------------------------------------------
data(rock)
head(rock)
# Using the default method:
set.seed(9)
out.def <- regCNN(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
# Using the formula method:
set.seed(9)
out.frm <- regCNN(formula = perm ~ ., data = rock)
# Check the match of noisy indices:
all(out.def$idnoise == out.frm$idnoise)

## ----example 2----------------------------------------------------------------
str(out.def)

## ----example 3----------------------------------------------------------------
print(out.def)

## ----example 4----------------------------------------------------------------
summary(out.frm, showid = TRUE)

