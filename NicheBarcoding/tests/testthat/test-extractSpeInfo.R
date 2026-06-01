# Test Start ###
test_that("run the function using different input parameters",{
  que.seq<-LappetMoths$que.seq
  seqID.full<-gsub("unknown,","",rownames(que.seq))

  expect_equal(ncol(extractSpeInfo(seqID.full)),5)
})


test_that("check the message when matching the format",{
  que.seq<-LappetMoths$que.seq
  seqID.full<-gsub(",unknown,.*","",rownames(que.seq))

  expect_output(extractSpeInfo(seqID.full),"bad format")
})
