context("elfdata")
library(elfgen)

test_that("Checking length of input watershed.code",
          {
            expect_error(elfdata(watershed.code = "02080",ichthy.localpath = tempdir()), "Invalid length of hydrologic unit code")
          })

##test_that("Checking for valid watershed.code",
##          {
##            expect_error(elfdata("999999999999"), "No IchthyMap Data for Hydrologic Unit Code")
##          })

  # test_that("Function returns a dataframe", {
  #   expect_equal(is.data.frame(elfdata("020700080403")), TRUE)
  # })


test.ichthy.dataframe <- data.frame(
  ID = c(132114,462514,370400,113072,113252),
  Source = c('Fish_Virginia','Fish_United States','Fish_United States','Fish_Virginia','Fish_Virginia'),
  State = c('VA','VA','VA','VA','VA'),
  Name_Taxa = c('Ameiurus catus','Percina notogramma','Notropis analostanus','Clinostomus funduloides','Percina notogramma'),
  Genus = c('Ameiurus','Percina','Notropis','Clinostomus','Percina'),
  Species = c('catus','notogramma','analostanus','funduloides','notogramma'),
  Level_Taxa = c('species','species','species','species','species'),
  ITIS_TSN = c(164037,168473,163766,163371,168473),
  COMID_NHDv2 = c(8508050,8508132,8508148,8508662,8508662),
  HUC12 = c(20801061101,20801060301,20801060903,20801060102,20801060102),
  HUC8 = c(2080106,2080106,2080106,2080106,2080106),
  HUC4 = c(208,208,208,208,208),
  Time_frame = c('1968-1987','1950-1980','1950-1980','1968-1987','1968-1987')
)

test_that("Function returns a dataframe",
          {
            expect_equal(is.data.frame(elfdata(watershed.code = test.ichthy.dataframe,ichthy.localpath = tempdir())), TRUE)
          })
