# Test Start ###
test_that("test when bak.vir is NULL",{
  data(en.vir)
  library(ape)
  data(LappetMoths)
  ref.seq<-LappetMoths$ref.seq
  que.seq<-LappetMoths$que.seq

  expect_output(NBSI(ref.seq,que.seq,ref.add=NULL,
                     independence=TRUE,
                     model="RF",variables="ALL",
                     en.vir=en.vir,bak.vir=NULL),
                "Background extracting ... ")
})

test_that("tests of different parameter combinations",{
  data(en.vir)
  data(bak.vir)
  library(ape)
  data(LappetMoths)
  ref.seq<-LappetMoths$ref.seq
  que.seq<-LappetMoths$que.seq
  NBSI.out<-NBSI(ref.seq,que.seq,ref.add=NULL,
                 independence=TRUE,
                 model="RF",variables="ALL",
                 en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI.out),7)

  NBSI.out<-NBSI(ref.seq,que.seq,ref.add=NULL,
                 independence=TRUE,
                 model="RF",variables="SELECT",
                 en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI.out),7)

  ref.add<-LappetMoths$ref.add
  NBSI.out<-NBSI(ref.seq,que.seq,ref.add=ref.add,
                 independence=TRUE,
                 model="RF",variables="ALL",
                 en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI.out),7)

  NBSI.out<-NBSI(ref.seq,que.seq,ref.add=ref.add,
                 independence=TRUE,
                 model="RF",variables="SELECT",
                 en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI.out),7)
})

test_that("tests of different parameter combinations",{
  data(en.vir)
  data(bak.vir)
  library(ape)
  data(LappetMoths)
  ref.seq<-LappetMoths$ref.seq
  que.seq<-LappetMoths$que.seq
  NBSI.out<-NBSI(ref.seq,que.seq,ref.add=NULL,
                 independence=TRUE,
                 model="MAXENT",variables="ALL",
                 en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI.out),7)

  NBSI.out<-NBSI(ref.seq,que.seq,ref.add=NULL,
                 independence=TRUE,
                 model="MAXENT",variables="SELECT",
                 en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI.out),7)

  ref.add<-LappetMoths$ref.add
  NBSI.out<-NBSI(ref.seq,que.seq,ref.add=ref.add,
                 independence=TRUE,
                 model="MAXENT",variables="ALL",
                 en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI.out),7)

  NBSI.out<-NBSI(ref.seq,que.seq,ref.add=ref.add,
                 independence=TRUE,
                 model="MAXENT",variables="SELECT",
                 en.vir=en.vir,bak.vir=bak.vir)

  expect_equal(ncol(NBSI.out),7)
})

test_that("tests for abnormal conditions",{
  data(en.vir)
  data(bak.vir)
  library(ape)
  data(LappetMoths)
  ref.seq<-LappetMoths$ref.seq
  que.seq<-LappetMoths$que.seq

  NBSI.out<-NBSI(ref.seq,que.seq=que.seq[1,],ref.add=NULL,
                 independence=TRUE,
                 model="RF",variables="ALL",
                 en.vir=en.vir,bak.vir=bak.vir)
  expect_equal(ncol(NBSI.out),7)

  NBSI.out<-NBSI(ref.seq,que.seq=que.seq[1,],ref.add=NULL,
                 independence=FALSE,
                 model="RF",variables="ALL",
                 en.vir=en.vir,bak.vir=bak.vir)
  expect_equal(ncol(NBSI.out),7)

  NBSI.out<-NBSI(ref.seq,que.seq=que.seq,ref.add=NULL,
                 independence=FALSE,
                 model="RF",variables="ALL",
                 en.vir=en.vir,bak.vir=bak.vir)
  expect_equal(ncol(NBSI.out),7)
})

test_that("tests for abnormal conditions",{
  data(en.vir)
  data(bak.vir)
  library(ape)
  data(LappetMoths)
  ref.seq<-LappetMoths$ref.seq
  que.seq<-LappetMoths$que.seq
  rownames(que.seq)<-gsub("[0-9\\.\\ \\-]*$","0 0",rownames(que.seq))

  expect_error(NBSI(ref.seq,que.seq,ref.add=NULL,
                    independence=TRUE,
                    model="RF",variables="ALL",
                    en.vir=en.vir,bak.vir=bak.vir),
               "No variables can be extracted from que.infor!")

  expect_error(NBSI(ref.seq,que.seq[1,],ref.add=NULL,
                    independence=TRUE,
                    model="RF",variables="ALL",
                    en.vir=en.vir,bak.vir=bak.vir),
               "No variables can be extracted from que.infor!")
})


