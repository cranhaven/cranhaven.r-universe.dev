library(pipenostics)

test_that("*dropg* errs in flow rate drop", {
  d <- as.double(c(800, 900, 1000, 1400, 1200))
  flow_rate <- .125*d
  adj <- c(450, -400, 950, -255, 1152)
  expect_equal(
    flow_rate - dropg(adj, d, flow_rate),
    c(75.96439, 134.72222, 65.70302,180.80580,  78.05995),
    tolerance = 1e-5
  )

  adjp <- list(
    c(100, 175, 175, -65, 125, -60),  # diameters of 4 discharge pipes and 2 recharge pipes, [mm]
    c(-300, -100, -65, 125, -60),  # diameter of 1 discharge pipe and 4 recharge pipes, [mm]
    c(950),  # diameter of 1 discharge pipe, [mm]
    c(-255), # diameter of 1 recharge pipe, [mm]
    c(50, 70, 1000, 32)  # diameter of 4 discharge pipes, [mm]
  )
  d <- c(800, 900, 1000, 1400, 1200)
  flow_rate <- .125*d

  expect_equal(
    flow_rate - dropg(adjp, d, flow_rate),
    c(75.96439, 134.72222, 65.70302, 180.80580, 78.05995),
    tolerance = 1e-5
  )
})


test_that("*dropp* errs in pressure drop", {
  expect_equal(
    dropp(len = c(200, 300)),
    c(0.0007000666, 0.0010500999)
  )
})


test_that("*dropt* errs in temperature drop", {
  pipeline <- list(
    year = 1968,
    laying = "channel",
    d = 700,
    l = 1000
  )
  operation_temperature <- c(130, 150)  # [°C]

  temperature_drop <- dropt(
      temperature = operation_temperature,
      loss_power = do.call(
        m325nhl,
        c(pipeline, temperature = list(operation_temperature))
      )
  )
  expect_equal(
    temperature_drop,
    c(1.366806, 1.433840),
    tolerance = 1e-6
  )
})