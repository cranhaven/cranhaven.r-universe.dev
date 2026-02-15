library(testthat)
library(shinytest)
library(EventDetectGUI)

context("Shiny GUI Tests")

test_that("Application works", {
    skip_on_appveyor()
    skip_on_cran()
    expect_error(testApp(system.file("appTests",package = "EventDetectGUI")), regexp = NA)
})

test_that("general functionality",{
    xGood <- stationBData[1000:2000,-1]
    eds <- EventDetectR::detectEvents(xGood,windowSize = 100,nIterationsRefit = 50,ignoreVarianceWarning = T)
    expect_equal(nrow(eds$classification), nrow(xGood))
})
