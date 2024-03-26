
# data set
data(metafile384, rawdata384)
rawdata<-plate2df(data2plateformat(rawdata384,platetype = 384))
data_DF2<- dplyr::inner_join(rawdata,metafile384,by=c("row","col","position"))
result3 <- dfsummary(data_DF2,y = "value",
                     grp_vector = c("cell","compound","concentration"),
                     rm_vector = c("B", "drug2", "huh7"),
                     nickname = "",
                     rm = "FALSE", param = c(strict = "FALSE", cutoff = 40,n = 12))
eg1<- pvalue(result3,"C3",sigval=0.05)
eg1<-head(eg1)

exp1<-data.frame(cell=c("hepg2","hepg2","hepg2","hepg2","hepg2","hepg2"),
                 compound=c("drug1","drug1","drug1","drug3","drug3","drug3"),
                 concentration=c("C3","C1","C2","C3","C1","C2"),
                 label=c("","","","","",""),
                 N=c(16,15,16,16,15,16),
                 Mean=c(0.475,0.227,0.262,0.425,0.607,0.612),
                 SD=c(0.262,0.059,0.096,0.191,0.070,0.072),
                 CV=c(55.17,26.19,36.47,45.06,11.60,11.74),
                 pvalue=c("control ","< 0.001","0.0024","control ","< 0.001","< 0.001"),
                 significance=c(" ","Yes","Yes"," ","Yes","Yes"))
row.names(exp1)<-c("1","2","3","4","5","6")


context("pvalue")

test_that("examples pvalue are working", {
  expect_that(eg1, equals(exp1))
  })

test_that("examples pvalue are notworking", {
  expect_error(pvalue(result3,"drug1",sigval=0.05), "Error: Control is not the approprite one for the analysis")
})
