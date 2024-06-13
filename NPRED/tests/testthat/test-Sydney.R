skip('skip')

library(NPRED)
library(zoo)
library(SPEI)

data("Sydney")


rain.daily <- Sydney$Rain; obs.daily <- Sydney$NCEP
dates <- time(rain.daily)
atm.vars <- colnames(obs.daily)

#plot(SPI.36[,11:12])
#-------------------------------------------------
###Study Index and Period
sc = 30 #30 days moving window

flag.knn <- switch(1, "NPRED","KNN")
start.yr = as.Date("1950-01-01"); end.yr = as.Date("1959-12-31")  #study period

###knn predictive model
k=0 # The number of nearest neighbours used: 0=ceiling(sqrt(n))
alpha=0.1 # selection stop criteria

###aggregated predictor
SPI <- zoo(spi(rain.daily,sc=sc)$fitted, dates)
period.ind <- which(!is.na(SPI[,1]))
stn.sub <- 1:5 

data.atmos.ts <- window(obs.daily[period.ind, ], start=start.yr, end=end.yr)
data.SPI.ts <- window(SPI[period.ind, stn.sub], start=start.yr, end=end.yr)
data.rain.ts <- window(rain.daily[period.ind, stn.sub], start=start.yr, end=end.yr)


# create paired response and predictors dataset for each station
data.list <- list()
for(id in 1:ncol(data.SPI.ts)){
  x <- data.SPI.ts[,id]
  dp <- data.atmos.ts[,]
  x.n<- data.rain.ts[,id]
  data.list[[id]] <- list(x=as.numeric(x), dp=matrix(dp, nrow=nrow(dp)),
                          x.n=as.numeric(x.n))
}

pic1 <- lapply(data.list, function(df) NPRED::stepwise.PIC(df$x.n, df$dp, nvarmax=4, alpha=0.2))
pic2 <- lapply(data.list, function(df) NPRED::stepwise.PIC(df$x, df$dp, nvarmax=4, alpha=0.2))













