
data(metafile384, rawdata384)
rawdata<-bioassays::plate2df(bioassays::data2plateformat(rawdata384,platetype = 384))
data_DF2<- dplyr::inner_join(rawdata,metafile384,by=c("row","col","position"))

result2 <- bioassays::dfsummary(data_DF2,y = "value",
                     grp_vector = c("cell","compound","concentration","type"),
                     rm_vector = c("blank1","blank2","blank3","blank4"),
                     nickname = "384well",
                     rm = "FALSE",param = c(strict="FALSE",cutoff=40,n=12))
eg1<-head(result2)

exp1<-data.frame(cell=c("hepg2","hepg2","hepg2","hepg2","hepg2","hepg2"),
                 compound=c("drug1","drug1","drug1","drug1","drug1","drug1"),
                 concentration=c("C1","C1","C2","C2","C3","C3"),
                 type=c("treated","untreated","treated","untreated","treated","untreated"),
                 label=c("384well","384well","384well","384well","384well","384well"),
                 N=c(7,8,8,8,8,8),
                 Mean=c(0.200,0.250,0.262,0.262,0.650,0.300),
                 SD=c(0.058,0.053,0.130,0.052,0.278,0.000),
                 CV=c(28.87,21.38,49.62,19.72,42.73,0.00))


context("dfsummary")

test_that("examples dfsummary are working",{
  expect_that(eg1, equals(exp1))
  })


