## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(stxplore)
library(dplyr)
library(tidyr)

## ----lookatdata---------------------------------------------------------------
data("NOAA_df_1990")
head(NOAA_df_1990)

## ----filterdata---------------------------------------------------------------
Tmax <- filter(NOAA_df_1990,
  proc == "Tmax" &
  month %in% 5:9 &
  year == 1993)
Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
Tmax_days <- subset(Tmax, t %in% c(1, 15, 30))

## ----ssnap1-------------------------------------------------------------------
spatial_snapshots(Tmax_days, 
         lat_col = 'lat', 
         lon_col = 'lon', 
         t_col = 't', 
         z_col = 'z',  
         title = "Maximum Temperature for 3 days",
         legend_title = 'Temp')


## ----ssnap2-------------------------------------------------------------------
Tmax_days <- subset(Tmax, t %in% c(1:12))

spatial_snapshots(Tmax_days, 
         lat_col = 'lat', 
         lon_col = 'lon', 
         t_col = 't', 
         z_col = 'z',  
         title = "Maximum Temperature for 12 days",
         legend_title = 'Temp')


## ----tsnap0-------------------------------------------------------------------
set.seed(148)                                          
Tmax_ID <- unique(Tmax$id)                               
ids <- sample(Tmax_ID, 12)
ids

## ----tsnap1-------------------------------------------------------------------
temporal_snapshots(Tmax, 
         t_col = 't', 
         z_col = 'z', 
         id_col = 'id', 
         id_sample = ids,  
         xlab = "Days", 
         ylab="Temperature",
         title = "Temperature Selected Days")

## ----sem1, message=FALSE------------------------------------------------------
sem <- spatial_means(Tmax, 
       lat_col = "lat", 
       lon_col = "lon", 
       t_col = "t", 
       z_col = "z" 
       )
autoplot(sem, ylab="Mean Max Temp")

## ----tem1---------------------------------------------------------------------

tem <- temporal_means(Tmax,
                      t_col = 'date',
                      z_col = 'z',
                      id_col = 'id')

autoplot(tem,
         ylab = "Mean Maximum Temperature")

## ----hovm1--------------------------------------------------------------------
hov <- hovmoller(lat_or_lon = "lat",
                 x = Tmax,
                 lat_or_lon_col = 'lat',
                 t_col = 't',
                 z_col = 'z')
autoplot(hov, legend_title = "Temperature")

## ----hovm2--------------------------------------------------------------------
hov <- hovmoller (lat_or_lon = "lon",
                  x = Tmax,
                  lat_or_lon_col = 'lon',
                  t_col = 't',
                  z_col = 'z')
autoplot(hov, legend_title = "Temperature")

## ----ridgeline----------------------------------------------------------------

ridgeline(Tmax, group_col = 'lat', z_col = 'z' )


## ----stecov1------------------------------------------------------------------
esv <- emp_spatial_cov(Tmax,
                       lat_col = "lat",
                       lon_col = "lon",
                       t_col ="t",
                       z_col = "z",
                       num_strips = 4,
                       quadratic_space = FALSE,
                       quadratic_time = TRUE,
                       lat_or_lon_strips = "lon"
)

autoplot(esv)

## ----stecov2------------------------------------------------------------------
esv <- emp_spatial_cov(Tmax,
                       lat_col = "lat",
                       lon_col = "lon",
                       t_col ="t",
                       z_col = "z",
                       num_strips = 4,
                       quadratic_space = FALSE,
                       quadratic_time = TRUE,
                       lat_or_lon_strips = "lat"
)
autoplot(esv)


## ----stecov3------------------------------------------------------------------
# longitudinal strips
esv1 <- emp_spatial_cov(Tmax,
                        lat_col = "lat",
                        lon_col = "lon",
                        t_col ="t",
                        z_col = "z",
                        num_strips = 4,
                        quadratic_space = FALSE,
                        quadratic_time = TRUE,
                        lat_or_lon_strips = "lon",
                        lag = 1
)
autoplot(esv1)

# latitude strips
esv2 <- emp_spatial_cov(Tmax,
                        lat_col = "lat",
                        lon_col = "lon",
                        t_col ="t",
                        z_col = "z",
                        num_strips = 4,
                        quadratic_space = FALSE,
                        quadratic_time = TRUE,
                        lat_or_lon_strips = "lat",
                        lag = 1
)
autoplot(esv2)

## ----stsemiv1-----------------------------------------------------------------
# Location data
data(locs)
head(locs)
dim(locs)

# Timestamp data
data(Times)
head(Times)
dim(Times)

# Spatio-temporal data
data(Tmax)
dim(Tmax) 

## ----stsemiv2-----------------------------------------------------------------
temp_part <- with(Times, paste(year, month, day, sep = "-"))
# Selecting the dates from 0992-07-01 to 1992-07-31
temp_part <- data.frame(date = as.Date(temp_part)[913:943])
Tmax2 <- Tmax[913:943, ]

## ----stsemiv3-----------------------------------------------------------------
semiv <- semivariogram(locs,
        temp_part,
        Tmax2,
        latitude_linear = FALSE,
        longitude_linear = FALSE,
        missing_value = -9999,
        width = 50,
        cutoff = 1000,
        tlagmax = 7
)
autoplot(semiv)

## ----stsemiv4-----------------------------------------------------------------
semiv<- semivariogram(locs,
        temp_part,
        Tmax2,
        latitude_linear = TRUE,
        longitude_linear = FALSE,
        missing_value = -9999,
        width = 50,
        cutoff = 1000,
        tlagmax = 7)
autoplot(semiv)

## ----stsemiv5-----------------------------------------------------------------
semiv <- semivariogram(locs,
        temp_part,
        Tmax2,
        latitude_linear = TRUE,
        longitude_linear = TRUE,
        missing_value = -9999,
        width = 50,
        cutoff = 1000,
        tlagmax = 7)
autoplot(semiv)

## ----eof1---------------------------------------------------------------------
data(SSTlonlatshort)
data(SSTdatashort)
data(SSTlandmaskshort)
# Take first 396 months (33 years) and delete land
delete_rows <- which(SSTlandmaskshort  ==  1)
SSTdatashort   <- SSTdatashort[-delete_rows, 1:396]
eoff <- emp_orth_fun(SSTlonlatshort[-delete_rows,  ],
                     SSTdatashort)
autoplot(eoff)



## ----eof2---------------------------------------------------------------------
autoplot(eoff, EOF_num = 2)

## ----eof3---------------------------------------------------------------------
pcs <- eoff$pcts %>% 
  select(t, EOF, nPC) %>% 
  pivot_wider(names_from = EOF, values_from = nPC) %>%
  select(-t)

cormat <- cor(pcs)[1:3, 1:3]
cormat

## ----cancor-------------------------------------------------------------------
data(SSTlonlatshort)
data(SSTdatashort)
cc1 <- cancor_eof(x = SSTlonlatshort,
           lag = 7,
           n_eof = 8,
           values_df = SSTdatashort)
autoplot(cc1)


