
test_that("Output Tests", {
  idvar<- "id"
  timevar<- "time"
  Yn<- "Y"
  An<- "A"

  datas<- dataexamples(n = 1000, seed = 123, Censoring = FALSE)
  data<- datas$datagestmult
  data<-FormatData(data=data,idvar="id",timevar="time",An="A",
                   varying=c("Y","A","L"),GenerateHistory=TRUE,
                   GenerateHistoryMax=1)

  outcomemodels=list("Y~A+U+L+Lag1A","Y~A+U+L+Lag1A",
                     "Y~A+U+L+Lag1A")
  propensitymodel=c("A~L+U+as.factor(time)+Lag1A")

  results1<-gestMultiple(data,  idvar, timevar, Yn, An, Cn=NA, outcomemodels,
            propensitymodel, censoringmodel=NULL, type=1, EfmVar=NA, cutoff=2)

  expect_equal(as.numeric(results1$psi), 1.082906,tolerance=0.001)

  datas<- dataexamples(n = 1000, seed = 123, Censoring = TRUE)
  data<-datas$datagestmultcat
  data<-FormatData(data=data,idvar="id",timevar="time",An="A",Cn="C",
                   varying=c("Y","A","L"),GenerateHistory=TRUE,
                   GenerateHistoryMax=1)
  outcomemodels=list("Y~A+U+L+Lag1A+A:L","Y~A+U+L+Lag1A+A:L",
                     "Y~A+U+L+Lag1A+A:L")
  censoringmodel<-c("C~L+U+as.factor(time)")

  results2<-gestMultiple(data,  idvar, timevar, Yn, An, Cn="C", outcomemodels,
                         propensitymodel, censoringmodel=censoringmodel,
                         type, EfmVar="L", type=2, cutoff=2)

  expect_equal(as.numeric(results2$psi), c(0.876480967,-0.003753011,
                                           2.002430202, -0.003016636),
                                          tolerance=0.001)

  #Test gestSingle

  datas<- dataexamples(n = 1000, seed = 123, Censoring = FALSE)
  data<- datas$datagest
  data<-FormatData(data=data,idvar="id",timevar="time",An="A",
                   varying=c("A","L"),GenerateHistory=TRUE,
                   GenerateHistoryMax=1)
  outcomemodels=list("Y~A+U+L","Y~A+U+L+Lag1A",
                     "Y~A+U+L+Lag1A")
  results3<-gestSingle(data,  idvar, timevar, Yn, An, Cn=NA, outcomemodels,
                      propensitymodel, censoringmodel=NULL, type=3, EfmVar=NA)

  expect_equal(as.numeric(results3$psi), c(1.2259292,0.9939270,0.9453069),
               tolerance=0.001)

  datas<- dataexamples(n = 1000, seed = 123, Censoring = TRUE)
  data<-datas$datagestcat
  data<-FormatData(data=data,idvar="id",timevar="time",An="A",Cn="C",
                   varying=c("A","L"),GenerateHistory=TRUE,
                   GenerateHistoryMax=1)
  outcomemodels=list("Y~A+U+L+A:L","Y~A+U+L+Lag1A+A:L",
                     "Y~A+U+L+Lag1A+A:L")
  censoringmodel<-c("C~L+U+as.factor(time)")

  results4<-gestSingle(data,  idvar, timevar, Yn, An, Cn="C", outcomemodels,
                         propensitymodel, censoringmodel=censoringmodel,
                         type, EfmVar="L", type=4)

  expect_equal(as.numeric(results4$psi), c(0.07020690,0.13677498,1.39091745,
                                          -0.03459920, 1.15979601, -0.03707152,
                                          2.36564693, -0.04804065, 1.48066441,
                                          -0.08357030, 2.49482780, -0.09849481),
               tolerance=0.01)

})



