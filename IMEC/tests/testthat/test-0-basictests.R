context("testing basic IMEC functionality")

test_that("analytic way of calculating coherence works", {
  #Comparison of Oxygen and Phlogiston Theory of Combustion
  Phenomena <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
  Oxygen <- c("OH1", "OH2", "OH3", "OH4", "OH5", "OH6")
  Phlogiston <- c("PH1", "PH2", "PH3", "PH4", "PH5", "PH6")
  evidence <- c(rep(1,8))
  ## oxygen and phlogiston
  explanations <- initializeNetwork(Phenomena, Oxygen, Phlogiston)
  #oygen explanations
  explanations <- explain(c("OH1", "OH2", "OH3"), "E1", explanations)
  explanations <- explain(c("OH1", "OH3"), "E3", explanations)
  explanations <- explain(c("OH1", "OH3", "OH4"), "E4", explanations)
  explanations <- explain(c("OH1", "OH5"), "E5", explanations)
  explanations <- explain(c("OH1", "OH4", "OH5"), "E6", explanations)
  explanations <- explain(c("OH1", "OH5"), "E7", explanations)
  explanations <- explain(c("OH1", "OH6"), "E8", explanations)
  #phlogiston explanations
  explanations <- explain(c("PH1", "PH2", "PH3"), "E1", explanations)
  explanations <- explain(c("PH1", "PH3", "PH4"), "E2", explanations)
  explanations <- explain(c("PH5", "PH6"), "E5", explanations)
  #contradictions
  explanations <- contradict("PH3", "OH3", explanations)
  explanations <- contradict("PH6", "OH5", explanations)
  IMEC <- computeIMEC(explanations,evidence, Phenomena, Oxygen, Phlogiston)
  plot(IMEC)
  summary(IMEC)
  expect(0 < mean(IMEC$ExplanatoryCoherenceT1[[2]]))
  expect(mean(IMEC$ExplanatoryCoherenceT1[[2]]) < 1)
  expect(0 < mean(IMEC$ExplanatoryCoherenceT2[[2]]))
  expect(mean(IMEC$ExplanatoryCoherenceT2[[2]]) < 1)
  expect(mean(IMEC$ExplanatoryCoherenceT1[[2]]) > mean(IMEC$ExplanatoryCoherenceT2[[2]]))
})
#> Test passed 🥳


# test one theory case
test_that("analytic way of calculating coherence works for 1 theory", {
  #Comparison of Oxygen and Phlogiston Theory of Combustion
  Phenomena <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
  Oxygen <- c("OH1", "OH2", "OH3", "OH4", "OH5", "OH6")
  evidence <- c(rep(1,8))
  ## oxygen and phlogiston
  explanations <- initializeNetwork(Phenomena, Oxygen)
  #oygen explanations
  explanations <- explain(c("OH1", "OH2", "OH3"), "E1", explanations)
  explanations <- explain(c("OH1", "OH3"), "E3", explanations)
  explanations <- explain(c("OH1", "OH3", "OH4"), "E4", explanations)
  explanations <- explain(c("OH1", "OH5"), "E5", explanations)
  explanations <- explain(c("OH1", "OH4", "OH5"), "E6", explanations)
  explanations <- explain(c("OH1", "OH5"), "E7", explanations)
  explanations <- explain(c("OH1", "OH6"), "E8", explanations)
  #contradictions
  explanations <- contradict("PH3", "OH3", explanations)
  explanations <- contradict("PH6", "OH5", explanations)
  IMEC <- computeIMEC(explanations,evidence, Phenomena, Oxygen)
  plot(IMEC)
  summary(IMEC)
  expect(0 < mean(IMEC$ExplanatoryCoherenceT1[[2]]))
  expect(mean(IMEC$ExplanatoryCoherenceT1[[2]]) < 1)
})
#> Test passed 🥳

test_that("intialize network gives error when more than two theories are compared", {
  #Comparison of Oxygen and Phlogiston Theory of Combustion
  Phenomena <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
  Oxygen <- c("OH1", "OH2", "OH3", "OH4", "OH5", "OH6")
  test <- c("bla")
  Phlogiston <- c("PH1", "PH2", "PH3", "PH4", "PH5", "PH6")
  evidence <- c(rep(1,8))
  ## oxygen and phlogiston
  expect_error(initializeNetwork(Phenomena, Oxygen, Phlogiston, test))

})

test_that("error message when there is more evidence than phenomena", {
  #Comparison of Oxygen and Phlogiston Theory of Combustion
  Phenomena <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
  Oxygen <- c("OH1", "OH2", "OH3", "OH4", "OH5", "OH6")
  Phlogiston <- c("PH1", "PH2", "PH3", "PH4", "PH5", "PH6")
  evidence <- c(rep(1,7))
  ## oxygen and phlogiston
  explanations <- initializeNetwork(Phenomena, Oxygen, Phlogiston)
  #oygen explanations
  explanations <- explain(c("OH1", "OH2", "OH3"), "E1", explanations)
  explanations <- explain(c("OH1", "OH3"), "E3", explanations)
  explanations <- explain(c("OH1", "OH3", "OH4"), "E4", explanations)
  explanations <- explain(c("OH1", "OH5"), "E5", explanations)
  explanations <- explain(c("OH1", "OH4", "OH5"), "E6", explanations)
  explanations <- explain(c("OH1", "OH5"), "E7", explanations)
  explanations <- explain(c("OH1", "OH6"), "E8", explanations)
  #phlogiston explanations
  explanations <- explain(c("PH1", "PH2", "PH3"), "E1", explanations)
  explanations <- explain(c("PH1", "PH3", "PH4"), "E2", explanations)
  explanations <- explain(c("PH5", "PH6"), "E5", explanations)
  #contradictions
  explanations <- contradict("PH3", "OH3", explanations)
  explanations <- contradict("PH6", "OH5", explanations)
  expect_error(computeIMEC(explanations,evidence, Phenomena, Oxygen, Phlogiston))

})


skip_on_cran()

test_that("simulation based way of calculating coherence works for one theory", {
    #Comparison of Oxygen and Phlogiston Theory of Combustion
    Phenomena <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
    Oxygen <- c("OH1", "OH2", "OH3", "OH4", "OH5", "OH6")
    evidence <- c(rep(1,8))
    ## oxygen and phlogiston
    explanations <- initializeNetwork(Phenomena, Oxygen)
    #oygen explanations
    explanations <- explain(c("OH1", "OH2", "OH3"), "E1", explanations)
    explanations <- explain(c("OH1", "OH3"), "E3", explanations)
    explanations <- explain(c("OH1", "OH3", "OH4"), "E4", explanations)
    explanations <- explain(c("OH1", "OH5"), "E5", explanations)
    explanations <- explain(c("OH1", "OH4", "OH5"), "E6", explanations)
    explanations <- explain(c("OH1", "OH5"), "E7", explanations)
    explanations <- explain(c("OH1", "OH6"), "E8", explanations)
    #contradictions
    explanations <- contradict("PH3", "OH3", explanations)
    explanations <- contradict("PH6", "OH5", explanations)
    IMEC <- computeIMEC(explanations,evidence, Phenomena, Oxygen, analytic = F)
    plot(IMEC)
    summary(IMEC)
    expect(0 < mean(IMEC$ExplanatoryCoherenceT1[[2]]))
    expect(mean(IMEC$ExplanatoryCoherenceT1[[2]]) < 1)
})

test_that("simulation based way of calculating coherence works", {
    #Comparison of Oxygen and Phlogiston Theory of Combustion
    Phenomena <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
    Oxygen <- c("OH1", "OH2", "OH3", "OH4", "OH5", "OH6")
    Phlogiston <- c("PH1", "PH2", "PH3", "PH4", "PH5", "PH6")
    evidence <- c(rep(1,8))
    ## oxygen and phlogiston
    explanations <- initializeNetwork(Phenomena, Oxygen, Phlogiston)
    #oygen explanations
    explanations <- explain(c("OH1", "OH2", "OH3"), "E1", explanations)
    explanations <- explain(c("OH1", "OH3"), "E3", explanations)
    explanations <- explain(c("OH1", "OH3", "OH4"), "E4", explanations)
    explanations <- explain(c("OH1", "OH5"), "E5", explanations)
    explanations <- explain(c("OH1", "OH4", "OH5"), "E6", explanations)
    explanations <- explain(c("OH1", "OH5"), "E7", explanations)
    explanations <- explain(c("OH1", "OH6"), "E8", explanations)
    #phlogiston explanations
    explanations <- explain(c("PH1", "PH2", "PH3"), "E1", explanations)
    explanations <- explain(c("PH1", "PH3", "PH4"), "E2", explanations)
    explanations <- explain(c("PH5", "PH6"), "E5", explanations)
    #contradictions
    explanations <- contradict("PH3", "OH3", explanations)
    explanations <- contradict("PH6", "OH5", explanations)
    IMEC <- computeIMEC(explanations,evidence, Phenomena, Oxygen, Phlogiston, analytic = F)
    plot(IMEC)
    summary(IMEC)
    expect(0 < mean(IMEC$ExplanatoryCoherenceT1[[2]]))
    expect(mean(IMEC$ExplanatoryCoherenceT1[[2]]) < 1)
    expect(0 < mean(IMEC$ExplanatoryCoherenceT2[[2]]))
    expect(mean(IMEC$ExplanatoryCoherenceT2[[2]]) < 1)
    expect(mean(IMEC$ExplanatoryCoherenceT1[[2]]) > mean(IMEC$ExplanatoryCoherenceT2[[2]]))
})
