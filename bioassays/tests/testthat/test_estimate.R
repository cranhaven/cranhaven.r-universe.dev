
# data set
data(data_DF1)

## Filtering standards
std<- dplyr::filter(data_DF1, data_DF1$id=="STD")
std <- aggregate(std$blankminus ~ std$concentration, FUN = mean )
colnames (std) <-c("con", "OD")

## 3-parametric regression curve fitting
fit1<-nplr::nplr(std$con,std$OD,npars=3,useLog = FALSE)

## Linear regression curve fitting
fit2<- stats::lm(formula = con ~ OD,data = std)

## eg:1 Based on nonparametric logistic regression fitting
estimated_nplr <- estimate(data_DF1,colname = "blankminus",fitformula = fit1,method = "nplr")
eg1<-head(estimated_nplr)
eg1$estimated<-round(eg1$estimated,2)

exp1<-data.frame(row=c("A","A","A","A","A","A"),
                 col=c(1,2,3,4,5,6),
                 position=c("A01","A02","A03","A04","A05","A06"),
                 value=c(0.659,0.649,0.598,0.601,0.541,0.553),
                 type=c("STD1","STD1","S1","S1","S1","S1"),
                 id=c("STD","STD","Sample","Sample","Sample","Sample"),
                 dilution=c(NA,NA,1,1,1,1),
                 concentration=c(25,25,0,0,0,0),
                 compound=c(NA,NA,"Taxol","Taxol","Taxol","Taxol"),
                 blankminus=c(0.5545,0.5445,0.4935,0.4965,0.4365,0.4485),
                 estimated=c(26.39687,24.01751,18.68524,18.88785,15.70869,16.23867))
exp1$compound<-as.factor(exp1$compound)
exp1$estimated<-round(exp1$estimated,2)
#row.names(exp1)<-c("1","2","3","4","5","6")

## eg:2 Based on linear regression fitting
estimated_lr<-estimate(data_DF1,colname="blankminus",fitformula=fit2,method="linear")
eg2<-head(estimated_lr)
eg2$estimated<-round(eg2$estimated,2)

exp2<-data.frame(row=c("A","A","A","A","A","A"),
                 col=c(1,2,3,4,5,6),
                 position=c("A01","A02","A03","A04","A05","A06"),
                 value=c(0.659,0.649,0.598,0.601,0.541,0.553),
                 type=c("STD1","STD1","S1","S1","S1","S1"),
                 id=c("STD","STD","Sample","Sample","Sample","Sample"),
                 dilution=c(NA,NA,1,1,1,1),
                 concentration=c(25,25,0,0,0,0),
                 compound=c(NA,NA,"Taxol","Taxol","Taxol","Taxol"),
                 blankminus=c(0.5545,0.5445,0.4935,0.4965,0.4365,0.4485),
                 estimated=c(23.96838,23.51493,21.20234,21.33838,18.61769,19.16183))
exp2$compound<-as.factor(exp2$compound)
exp2$estimated<-round(exp2$estimated,2)
#row.names(exp2)<-c("1","2","3","4","5","6")


context("reduceblank")

test_that("examples blankminus are working", {
  expect_that(eg1, equals(exp1))
  expect_that(eg2, equals(exp2))
  })
