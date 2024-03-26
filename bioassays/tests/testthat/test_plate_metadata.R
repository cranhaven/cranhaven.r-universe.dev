
# data set
data(metafile96)
plate_details <- list("compound" = "Taxol",
                      "concentration" = c(0.00,0.01,0.02,0.05,0.10,1.00,5.00,10.00),
                      "type" = c("S1","S2","S3","S4","S5","S6","S7","S8"),
                      "dilution" = 1)

## eg:1 filling metadata96 using plate_details
plate_meta<-plate_metadata(plate_details,metafile96,mergeby="type")
eg1<-head(plate_meta)

exp1<-data.frame(row=c("A","A","A","A","A","A"),
                 col=c(1,2,3,4,5,6),
                 type=c("STD1","STD1","S1","S1","S1","S1"),
                 position=c("A01","A02","A03","A04","A05","A06"),
                 id=c("STD","STD","Sample","Sample","Sample","Sample"),
                 dilution=c(NA,NA,1,1,1,1),
                 concentration=c(25,25,0,0,0,0),
                 compound=c(NA,NA,"Taxol","Taxol","Taxol","Taxol"))
row.names(exp1)<-c("1","2","3","4","5","6")


context("pplate_metadata")

test_that("examples plate_metadata are working", {
  expect_that(eg1, equals(exp1))
  })

