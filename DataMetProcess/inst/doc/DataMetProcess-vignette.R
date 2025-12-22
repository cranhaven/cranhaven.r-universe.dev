## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(DataMetProcess)

## -----------------------------------------------------------------------------
file.down <- tempfile()

info.inmet <- DataMetProcess::list_inmet(
  year="2000",
  filename = file.down
)

head(info.inmet)

## ----eval=FALSE---------------------------------------------------------------
# 
# file.save <- tempfile()
# 
# unzip.file <-
#    utils::unzip(
#      zipfile = file.down, #or info.inmet$Saved
#      exdir = file.save
#    )
# 
# #specific file
# unzip.file <-
#    utils::unzip(
#      zipfile = file.down, #or info.inmet$Saved
#      files = info.inmet$Adresses[2,],
#      exdir = file.save
#    )

## -----------------------------------------------------------------------------
address <-
 base::system.file("extdata",
                    "ex1_inmet.CSV",
                    package = "DataMetProcess")

df <-
  read.table(
    address,
    h=TRUE,
    sep = ";",
    dec = ",",
    skip = 8, 
    na.strings = -9999,
    check.names = FALSE
  ) #see ?read.table for more details...

#Converting to R standard (when necessary)
df$Data = as.Date(df$Data,format = "%d/%m/%Y")

head(df[1:3]) #We are only viewing a part of it.

df <-
  adjustDate(df,
             colnames(df)[1],
             colnames(df)[2],
             fuso = "America/Bahia")

#date and time are now in a single column
head(df[1:2]) #We are only viewing a part of it.

## -----------------------------------------------------------------------------
df.new <- df
df.new$Date_Hour <- as.Date(df$Date_Hour)

## -----------------------------------------------------------------------------
df.daily <-
  calculateDMY(
    data = df.new,
    col_date = colnames(df)[c(1)],
    col_sum = colnames(df)[c(2,6)], #simplest way to pass column names as string
    col_mean = colnames(df)[-c(1,2,6)], #remove the previous steps in the parameter above
    type = "Daily"
  )

head(df.daily[1:2]) #We are only viewing a part of it.


## -----------------------------------------------------------------------------
df.monthly <-
  calculateDMY(
    data = df.daily,
    col_date = colnames(df.daily)[c(1)],
    col_sum = colnames(df.daily)[c(2)],
    col_mean = colnames(df.daily)[-c(1,2)],
    type = "Monthly"
  )

head(df.monthly[1:2]) #We are only viewing a part of it.


## -----------------------------------------------------------------------------
df.yearly <-
  calculateDMY(
    data = df.monthly,
    col_date = colnames(df.monthly)[c(1)],
    col_sum = colnames(df.monthly)[c(2)],
    col_mean = colnames(df.monthly)[-c(1,2)],
    type = "Yearly"
  )

head(df.yearly[1:2]) #We are only viewing a part of it.

## -----------------------------------------------------------------------------
 address <-
  base::system.file("extdata",
                     "ex2_daily.CSV",
                     package = "DataMetProcess")

 df <- read.table(
 address,
 h = TRUE,
 sep = ";"
 )

 #converting to Mj/m
 df$radiacao_global_kj_m <- df$radiacao_global_kj_m/1000
 colnames(df)[3] <- "radiacao_global_mj_m"

df.Eto <-
 calculateETrefPM(
   data = df,
   lat = -21.980353,
   alt = 859.29,
   za = 10,
   DAP = 1,
   date = colnames(df)[1],
   Ta = colnames(df)[7],
   G = NULL,
   RH = colnames(df)[15],
   Rg = colnames(df)[3],
   AP = colnames(df)[4],
   WS = colnames(df)[18],
   Kc = NULL
 )
 
 head(df.Eto)
 

