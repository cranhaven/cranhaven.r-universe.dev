context("reg")

test_that("reg_x for lm, glm and coxph model", {

  # basic models
  fit<-reg_x(data = diabetes, x = c(1:4, 6), y ="diabetes" , factors = c(1, 3, 4), model = 'lm')
  plot(fit)
  fit<-reg_x(data = diabetes, x = c(1:4, 6), y ="diabetes" , factors = c(1, 3, 4), model = 'glm')
  plot(fit)
  fit<-reg_x(data = diabetes, x = c(3:4, 6), y ="diabetes",time=2,factors = c(1, 3, 4), model = 'coxph')
  plot(fit)

  #cov_show
  fit<-reg_x(data = diabetes, x = c(1:4, 6), y ="diabetes" ,cov=13:14, factors = c(1, 3, 4), model = 'lm',cov_show = TRUE)
  expect_message(plot(fit))
  fit<-reg_x(data = diabetes, x = c(1:4, 6), y ="diabetes" ,cov=13:14, factors = c(1, 3, 4), model = 'glm',cov_show = TRUE)
  expect_message(plot(fit))
  fit<-reg_x(data = diabetes, x = c(3:4, 6), y ="diabetes",cov=13:14,time=2,factors = c(1, 3, 4), model = 'coxph',cov_show = TRUE)
  expect_message(plot(fit))

  #detail_show
  fit<-reg_x(data = diabetes, x = c(1:4, 6), y ="diabetes" , factors = c(1, 3, 4), model = 'lm',detail_show = TRUE)
  expect_error(plot(fit))
  fit<-reg_x(data = diabetes, x = c(1:4, 6), y ="diabetes" , factors = c(1, 3, 4), model = 'glm',detail_show = TRUE)
  expect_error(plot(fit))
  fit<-reg_x(data = diabetes, x = c(3:4, 6), y ="diabetes",time=2,factors = c(1, 3, 4), model = 'coxph',detail_show = TRUE)
  expect_error(plot(fit))

  # y is also in x
  expect_warning(fit<-reg_x(data = diabetes, x = c(1:4, 5), y ="diabetes" , factors = c(1, 3, 4), model = 'lm'))

  })


test_that("reg_y for lm, glm and coxph model", {

  # basic models
  fit<-reg_y(data = diabetes, x = c(1:4, 6), y =c("diabetes","C2rs9332739","CFBrs641153") , factors = c(1, 3, 4), model = 'lm')
  expect_message(plot(fit))
  fit<-reg_y(data = diabetes, x = c(1:4, 6), y =c("diabetes","C2rs9332739","CFBrs641153") , factors = c(1, 3, 4), model = 'glm')
  expect_message(plot(fit))
  fit<-reg_y(data = diabetes, x = c(3:4, 6), y =c("diabetes","C2rs9332739","CFBrs641153"),time=2,factors = c(1, 3, 4), model = 'coxph')
  expect_message(plot(fit))

  #cov_show
  fit<-reg_y(data = diabetes, x = c(1:3, 6), y =c("diabetes","C2rs9332739","CFBrs641153") ,cov=c(11,14), factors = c(1, 3, 4), model = 'lm',cov_show = TRUE)
  expect_message(plot(fit))
  fit<-reg_y(data = diabetes, x = c(1:3, 6), y =c("diabetes","C2rs9332739","CFBrs641153") ,cov=c(11,14), factors = c(1, 3, 4), model = 'glm',cov_show = TRUE)
  expect_message(plot(fit))
  fit<-reg_y(data = diabetes, x = c(1,3, 6), y =c("diabetes","C2rs9332739","CFBrs641153"),time=2,cov=c(11,14),factors = c(1, 3, 4), model = 'coxph',cov_show = TRUE)
  expect_message(plot(fit))

  # y is also in x
  expect_warning(fit<-reg_y(data = diabetes, x = c(1:4, 5), y =c("diabetes","C2rs9332739","CFBrs641153") , factors = c(1, 3, 4), model = 'lm'))


})


test_that("reg for lm, glm and coxph model", {

  ## subgroup analysis for reg
  fit<-reg(data = diabetes, x = c(2:4),group=c(1,13,14), y ="diabetes" , factors = c(1, 3, 4), model = 'glm')
  expect_message(plot(fit))
  fit<-reg(data = diabetes, x = c(2:4),group=c(1,13,14), y ="diabetes" , factors = c(1, 3, 4), model = 'glm',group_combine = TRUE)
  expect_message(plot(fit))
  fit<-reg(data = diabetes, x = c(2:4),group=c(1,13,14),cov=c(11,12), y ="diabetes" , factors = c(1, 3, 4), model = 'glm',cov_show = TRUE)
  expect_message(plot(fit))
  fit<-reg(data = diabetes, x = c(2:4),group=c(1,14),cov=c(11,12),y ="diabetes" , factors = c(1, 3, 4), model = 'glm',group_combine = TRUE)
  expect_message(plot(fit))
  fit<-reg(data = diabetes, x = c(2:4),group=c(1,14),cov=c(11,12), y =c("diabetes","CFBrs641153"), factors = c(1, 3, 4), model = 'glm',group_combine = TRUE,cov_show = TRUE)
  expect_message(plot(fit))
})



test_that("display_table", {

  dd<-display_table(data=diabetes,variables =c(1:4,6:10),group="diabetes")
  expect_warning(dd<-display_table(data=diabetes,group="diabetes"))
  dd<-display_table(data=diabetes,variables =c(1:4,6:10),group="diabetes",mean_or_median="median")
  dd<-display_table(data=diabetes,variables =c(1:4,6:10),group="diabetes",mean_or_median="median",addNA = FALSE)
  dd<-display_table(data=diabetes,variables =c(1:4,6:10),group="diabetes",mean_or_median="median",addNA = FALSE,discrete_limit = 3)

  dd<-display_table(data=diabetes,variables =c(1:4,6:10),group="diabetes",mean_or_median="mean",addNA = TRUE,discrete_limit = 3)

  expect_warning({
    data<-diabetes
    data$CFHrs2230199<-as.factor(data$CFHrs2230199)
    dd<-display_table(data=data,variables =c(1:4,6:14),group="diabetes",mean_or_median="mean",addNA = TRUE,discrete_limit = 2,exclude_discrete = TRUE)
    dd<-display_table(data=data,variables =c(1:4,6:14),group="diabetes",mean_or_median="mean",addNA = TRUE,discrete_limit = 2,exclude_discrete = FALSE)
  })

  dd<-display_table(data=diabetes,variables =c(1:4,6:10),group="diabetes",normtest = "shapiro.test")
  dd<-display_table(data=diabetes,variables =c(1:4,6:10),group="diabetes",normtest = "lillie.test")


})


test_that("display_table_group", {

  result_1<-display_table_group(data=diabetes,variables=c("age","smoking","education"),group="CFHrs2230199",super_group = "sex")

  result_2<-display_table_group(data=diabetes,variables=c("age","education"),group=c("smoking"),super_group = c("CFHrs2230199","sex"))
  result_3<-display_table_group(data=diabetes,variables=c("age","education"),group=c("smoking"),super_group = c("CFHrs2230199","sex"),group_combine=TRUE)


})

