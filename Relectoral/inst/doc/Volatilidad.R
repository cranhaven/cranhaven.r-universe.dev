## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
if (!require("readxl")) install.packages("readxl");require("readxl")
dat1<-read_xlsx("../inst/data_raw/volatilidad/abril_19.xlsx",col_names = TRUE)
dat2<-read_xlsx("../inst/data_raw/volatilidad/noviembre_19.xlsx",col_names = TRUE)

## -----------------------------------------------------------------------------
head(dat1)

## -----------------------------------------------------------------------------
dat1_electoral<-dat1[,c(1,3)]
dat2_electoral<-dat2[,c(1,3)]

## -----------------------------------------------------------------------------
dat1_parlamentaria<-dat1[,c(1,7)]
dat2_palamentaria<-dat2[,c(1,7)]

## -----------------------------------------------------------------------------
enlace <- read.csv("../inst/data_raw/volatilidad/abril_noviembre_2019.csv",
                   header = TRUE,sep=";", stringsAsFactors = FALSE)
enlace

## -----------------------------------------------------------------------------
# Loading Relectoral package
if (!require("Relectoral")) install.packages("Relectoral"); require("Relectoral")
s<-volatilidad(dat1_electoral,dat2_electoral,enlace)

## -----------------------------------------------------------------------------
s$Total

## -----------------------------------------------------------------------------
s$Entre

## -----------------------------------------------------------------------------
s1<-volatilidad(dat1_parlamentaria,dat2_palamentaria,enlace)
s1$Total
s1$Entre

