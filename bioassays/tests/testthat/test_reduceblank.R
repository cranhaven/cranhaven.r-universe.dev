
# data set
data(metafile384, rawdata384)
rawdata<-plate2df(data2plateformat(rawdata384,platetype = 384))
data_DF2<- dplyr::inner_join(rawdata,metafile384,by=c("row","col","position"))

eg1<-bioassays::reduceblank(data_DF2,
                      x_vector=c("drug1","drug2","drug3","drug4"),
                      blank_vector = c("blank1","blank2","blank3","blank4"), "value")
eg1<-head(eg1)
exp1<-data.frame(row=c("A","A","A","A","A","A"),
                 col=c(1,2,3,4,5,6),
                 position=c("A01","A02","A03","A04","A05","A06"),
                 value=c(0.1,0.1,0.2,0.3,0.2,0.2),
                 cell=c("hepg2","hepg2","hepg2","hepg2","hepg2","hepg2"),
                 compound=c("drug1","drug1","drug1","drug1","drug1","drug1"),
                 concentration=c("B","C1","C1","C1","C1","C1"),
                 type=c("blank1","treated","treated","treated","treated","treated"),
                 dilution=c(NA,10,10,10,10,10),
                 blankminus=c(-0.4,-0.4,-0.3,-0.2,-0.3,-0.3))
row.names(exp1)<-c("1","2","3","4","5","6")


context("reduceblank")

test_that("examples blankminus are working", {
  expect_that(eg1, equals(exp1))
  })
