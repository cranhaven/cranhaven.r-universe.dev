# Test Start ###
test_that("test when bak.vir is NULL",{
  data(en.vir)
  data(LappetMoths)
  barcode.identi.result<-LappetMoths$barcode.identi.result
  ref.infor<-LappetMoths$ref.infor
  que.infor<-LappetMoths$que.infor

  expect_output(NBSI2(ref.infor=ref.infor,que.infor=que.infor,
                      barcode.identi.result=barcode.identi.result,
                      model="RF",variables="ALL",
                      en.vir=en.vir,bak.vir=NULL),
                "Background extracting ... ")
})

test_that("test when ref and que are NULL",{
  data(LappetMoths)
  barcode.identi.result<-LappetMoths$barcode.identi.result

  expect_error(NBSI2(ref.infor=NULL,que.infor=NULL,
                     ref.env=NULL,que.env=NULL,
                     barcode.identi.result=barcode.identi.result,
                     model="RF",variables="ALL",
                     en.vir=NULL,bak.vir=NULL),
               "Please check the input data!")
})

test_that("tests of different parameter combinations",{
  data(en.vir)
  data(bak.vir)
  data(LappetMoths)
  barcode.identi.result<-LappetMoths$barcode.identi.result
  ref.infor<-LappetMoths$ref.infor
  que.infor<-LappetMoths$que.infor
  NBSI2.out<-NBSI2(ref.infor=ref.infor,que.infor=que.infor,
                   barcode.identi.result=barcode.identi.result,
                   model="RF",variables="ALL",
                   en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI2.out),7)

  NBSI2.out<-NBSI2(ref.infor=ref.infor,que.infor=que.infor,
                   barcode.identi.result=barcode.identi.result,
                   model="RF",variables="SELECT",
                   en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI2.out),7)

  ref.env<-LappetMoths$ref.env
  que.env<-LappetMoths$que.env
  NBSI2.out<-NBSI2(ref.env=ref.env,que.env=que.env,
                   barcode.identi.result=barcode.identi.result,
                   model="RF",variables="ALL",
                   en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI2.out),7)

  NBSI2.out<-NBSI2(ref.env=ref.env,que.env=que.env,
                   barcode.identi.result=barcode.identi.result,
                   model="RF",variables="SELECT",
                   en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI2.out),7)
})

test_that("tests of different parameter combinations",{
  data(en.vir)
  data(bak.vir)
  data(LappetMoths)
  barcode.identi.result<-LappetMoths$barcode.identi.result

  ref.infor<-LappetMoths$ref.infor
  que.infor<-LappetMoths$que.infor
  NBSI2.out<-NBSI2(ref.infor=ref.infor,que.infor=que.infor,
                   barcode.identi.result=barcode.identi.result,
                   model="MAXENT",variables="ALL",
                   en.vir=en.vir,bak.vir=bak.vir)
  expect_equal(ncol(NBSI2.out),7)

  NBSI2.out<-NBSI2(ref.infor=ref.infor,que.infor=que.infor,
                   barcode.identi.result=barcode.identi.result,
                   model="MAXENT",variables="SELECT",
                   en.vir=en.vir,bak.vir=bak.vir)
  expect_equal(ncol(NBSI2.out),7)

  ref.env<-LappetMoths$ref.env
  que.env<-LappetMoths$que.env
  NBSI2.out<-NBSI2(ref.env=ref.env,que.env=que.env,
                   barcode.identi.result=barcode.identi.result,
                   model="MAXENT",variables="ALL",
                   en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI2.out),7)

  NBSI2.out<-NBSI2(ref.env=ref.env,que.env=que.env,
                   barcode.identi.result=barcode.identi.result,
                   model="MAXENT",variables="SELECT",
                   en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI2.out),7)
})

test_that("tests for abnormal conditions",{
  data(en.vir)
  data(bak.vir)
  data(LappetMoths)
  barcode.identi.result<-LappetMoths$barcode.identi.result
  ref.infor<-LappetMoths$ref.infor
  que.infor<-LappetMoths$que.infor
  NBSI2.out<-NBSI2(ref.infor=ref.infor,que.infor=que.infor[1,],
                   barcode.identi.result=barcode.identi.result,
                   model="RF",variables="ALL",
                   en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI2.out),7)

  ref.env<-LappetMoths$ref.env
  que.env<-LappetMoths$que.env
  NBSI2.out<-NBSI2(ref.env=ref.env,que.env=que.env[1,],
                     barcode.identi.result=barcode.identi.result,
                     model="RF",variables="ALL",
                     en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI2.out),7)
})

test_that("tests for abnormal conditions",{
  data(en.vir)
  data(bak.vir)
  data(LappetMoths)
  barcode.identi.result<-LappetMoths$barcode.identi.result
  ref.infor<-LappetMoths$ref.infor
  que.env<-LappetMoths$que.env

  expect_error(NBSI2(ref.infor=ref.infor,que.env=que.env,
                     barcode.identi.result=barcode.identi.result,
                     model="RF",variables="ALL",
                     en.vir=en.vir,bak.vir=bak.vir),
               "There is no matching ecological variable information")
})

test_that("tests for abnormal conditions",{
  data(en.vir)
  data(bak.vir)
  data(LappetMoths)
  barcode.identi.result<-LappetMoths$barcode.identi.result
  ref.infor<-LappetMoths$ref.infor
  que.infor<-LappetMoths$que.infor

  expect_warning(NBSI2(ref.infor=ref.infor[1:50,],que.infor=que.infor,
                       barcode.identi.result=barcode.identi.result,
                       model="RF",variables="ALL",
                       en.vir=en.vir,bak.vir=bak.vir),
                 "doesn't exist in ref.infor!")
})

