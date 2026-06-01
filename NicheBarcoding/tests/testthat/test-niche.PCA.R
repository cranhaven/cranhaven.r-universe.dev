# Test Start ###
test_that("message test",{
  data(en.vir)
  data(LappetMoths)
  ref.infor<-LappetMoths$ref.infor
  que.infor<-LappetMoths$que.infor
  ref.lonlat=ref.infor[1:3,3:5]
  que.lonlat=que.infor[,c(2,4:5)]

  expect_message(niche.PCA(ref.lonlat,que.lonlat,en.vir=en.vir))
})





