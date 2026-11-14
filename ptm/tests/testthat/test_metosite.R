library(ptm)
context("MetOsite Database Search")

## ---------------------------------------------- ##
#             Testing meto.search                  #
## ---------------------------------------------- ##
test_that('meto.search() works properly', {

  m1 <- meto.search(highthroughput.group = F, bodyguard.group = F, gain.activity = 1)
  m2 <- meto.search(organism = 'Homo sapiens', oxidant = 'HClO')

  if (!is.null(m1)){
    expect_is(m1, "data.frame")
    expect_length(m1, 7)
    expect_gt(nrow(m1), 26)
    expect_true('hypochlorite (HClO)' %in% m1$org_oxidant)
    expect_true('Mus musculus' %in% m1$org_sp)
    expect_true('Calmodulin' %in% m1$prot_name)
    expect_true('P04275' %in% m1$prot_id)
  }

  if (!is.null(m2)){
    expect_is(m2, "data.frame")
    expect_length(m2, 7)
    expect_gt(nrow(m2), 20)
    expect_equal(nrow(m2), length(which(m2$org_sp == 'Homo sapiens')))
  }

})


## ---------------------------------------------- ##
#               Testing meto.scan                  #
## ---------------------------------------------- ##
test_that('meto.scan() works properly', {

  m1 <- meto.scan('P01009')
  m2 <- meto.scan('P01009', report = 2)
  m3 <- meto.scan('P01009', report = 3)

  if (!is.null(m1)){
    expect_is(m1, "list")
    expect_length(m1, 2)
    expect_equal(dim(m1$Metosites), c(3,7))
  }

  if (!is.null(m2)){
    expect_is(m2, "list")
    expect_length(m2, 18)
    expect_equal(dim(m1$Metosites), c(3,7))
  }

  if (!is.null(m3)){
    expect_is(m2, "list")
    expect_length(m3, 2)
    expect_equal(dim(m3$Metosites), c(3,7))
    expect_true(file.exists('report_scan_P01009.txt'))
  }
})
file.remove('report_scan_P01009.txt')


## ---------------------------------------------- ##
#               Testing meto.list                  #
## ---------------------------------------------- ##
test_that('meto.list() works properly', {

  ca <- meto.list('calcium')

  if (!is.null(ca)){
    expect_is(ca, "data.frame")
    expect_gt(nrow(ca), 19)
    expect_equal(ncol(ca), 3)
    expect_gt(sum(grepl("Calcium/calmodulin", ca$prot_name, ignore.case = T)), 2)
  }
})



