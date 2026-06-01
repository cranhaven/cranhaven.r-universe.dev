# Test Start ###
test_that("check whether the matrices are matched",{
  data(en.vir)
  library(ape)
  data(LappetMoths)
  ref.seq<-LappetMoths$ref.seq
  spe.mantel<-spe.mantel.test(fas=ref.seq,en.vir=en.vir)

  expect_equal(nrow(spe.mantel$genet.matrix),ncol(spe.mantel$genet.matrix))
  expect_equal(nrow(spe.mantel$ecol.matrix),ncol(spe.mantel$ecol.matrix))
  expect_equal(dim(spe.mantel$genet.matrix),dim(spe.mantel$ecol.matrix))
})

test_that("warning test",{
  data(en.vir)
  library(ape)
  data(LappetMoths)
  ref.seq<-LappetMoths$ref.seq
  tmp1<-as.DNAbin(gsub("t|c|g","a",ref.seq[1,]))
  tmp2<-as.DNAbin(gsub("t|c|a","g",ref.seq[2,]))
  fas<-rbind(tmp1,tmp1,tmp2,tmp2)

  expect_message(spe.mantel.test(fas=fas,dna.model="JC69",en.vir=en.vir))
})


