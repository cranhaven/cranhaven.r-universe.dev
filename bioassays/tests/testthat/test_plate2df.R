
# data set
data(rawdata24,rawdata96,rawdata384)

## eg:1 spectrophotometer reading from 24 well plate in dataframe format
datamatrix<- data2plateformat(rawdata24, platetype = 24)
eg1<-head(plate2df(datamatrix))
exp1<-data.frame(row=c("A","A","A","A","A","A"),
                 col=c(1,2,3,4,5,6),
                 position=c("A01","A02","A03","A04","A05","A06"),
                 value=c(0.659,0.649,0.598,0.601,0.541,0.553))

## eg:2 spectrophotometer reading from 96 well plate in dataframe format
datamatrix<- data2plateformat(rawdata96, platetype = 96)
eg2<-head(plate2df(datamatrix))
exp2<-data.frame(row=c("A","A","A","A","A","A"),
                 col=c(1,2,3,4,5,6),
                 position=c("A01","A02","A03","A04","A05","A06"),
                 value=c(0.659,0.649,0.598,0.601,0.541,0.553))

## eg:3 spectrophotometer reading from 384 well plate in dataframe format
datamatrix<- data2plateformat(rawdata384, platetype = 384)
eg3<-head(plate2df(datamatrix))
exp3<-data.frame(row=c("A","A","A","A","A","A"),
                 col=c(1,2,3,4,5,6),
                 position=c("A01","A02","A03","A04","A05","A06"),
                 value=c(0.1,0.1,0.2,0.3,0.2,0.2))

context("plate_metadata")

test_that("examples plate_metadata are working", {
  expect_that(eg1, equals(exp1))
  expect_that(eg2, equals(exp2))
  expect_that(eg3, equals(exp3))
  })

