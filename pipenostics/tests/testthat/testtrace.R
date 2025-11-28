library(pipenostics)

test_that("*traceline* errs in tracing regime parameters", {
  regime_fw <- traceline(
    130, .588399, 250, seq(0, 30, 10),
    loss = c(348, 347.1389, 346.3483, 345.8610), forward = TRUE
  )

  expect_equal(
    names(regime_fw),
    c("temperature", "pressure", "flow_rate", "loss", "flux", "Q")
  )
  expect_equal(
   regime_fw[["temperature"]],
   c(129.1799, 128.4269, 127.9628, 127.3367),
   tolerance = 1e-4
  )
  expect_equal(
    regime_fw[["pressure"]],
    c(0.5878607, 0.5874226, 0.5872143, 0.5870330),
    tolerance = 1e-7
  )
  expect_equal(
    regime_fw[["flow_rate"]],
    c(250, 240, 220, 190)
  )
  expect_equal(
    regime_fw[["loss"]],
    c(348, 347.138912477, 346.348251588, 345.860965187),
    tolerance = 1e-7
  )
  expect_equal(
    regime_fw[["flux"]],
    c(181.959958158, 181.509718360, 181.096302779, 180.841513660),
    tolerance = 1e-7
  )
  expect_equal(
    regime_fw[["Q"]],
    c(5011200, 4415607.97, 2493707.41, 2905232.11),
    tolerance = 1e-1
  )

  regime_bw <- traceline(
    127.3367, .5870330, 190, seq(0, 30, 10),
    loss = c(348, 347.1389, 346.3483, 345.8610), forward = FALSE
  )
  expect_equal(
    names(regime_bw),
    c("temperature", "pressure", "flow_rate", "loss", "flux", "Q")
  )
  expect_equal(
    regime_bw[["temperature"]],
    c(130.000893685, 129.180497939, 128.427226907, 127.963046346),
    tolerance = 1e-4
  )
  expect_equal(
    regime_bw[["pressure"]],
    c(0.588399833660, 0.587861095778, 0.587422779315, 0.587214377798),
    tolerance = 1e-7
  )
  expect_equal(
    regime_bw[["flow_rate"]],
    c(250, 250, 240, 220)
  )
  expect_equal(
    regime_bw[["loss"]],
    c(348, 347.1389, 346.3483, 345.8610),
    tolerance = 1e-7
  )
  expect_equal(
    regime_bw[["flux"]],
    c(181.959958158, 181.509711836, 181.096328092, 180.841531863),
    tolerance = 1e-7
  )
  expect_equal(
    regime_bw[["Q"]],
    c(5011200, 4415606.808, 2493707.76, 2905232.4),
    tolerance = 1e-3
  )
})


DHN <- list(
  sender = c(
    "N04", "N04", "N05", "N08", "N06", 
    "N08", "N06", "N11", "N06", "N11", "N13", "N00", "N12", "N13", 
    "N14", "N13", "N16", "N16", "N20", "N18", "N20", "N18", "N20", 
    "N22", "N22", "N23"
  ),
  acceptor = c(
    "N01", "N02", "N03", "N04", 
    "N05", "N06", "N07", "N08", "N09", "N10", "N11", "N12", "N13", 
    "N14", "N15", "N16", "N17", "N18", "N19", "N20", "N21", "N22", 
    "N23", "N24", "N25", "N26"
  ), 
  temperature = c(
    69.3, 69.4, 68.6, 
    NA, NA, NA, 70, NA, 69.2, 71.3, NA, 70.4942576977863, NA, NA, 
    70, NA, 71.4, NA, 69.3, NA, 68.8, NA, NA, 69.2, 69.3, 68.5
  ), 
  pressure = c(
    0.588399, 0.588399, 0.588399, NA, NA, NA, 0.588399, 
    NA, 0.588399, 0.588399, NA, 0.613560201407145, NA, NA, 0.588399, 
    NA, 0.588399, NA, 0.588399, NA, 0.588399, NA, NA, 0.588399, 
    0.588399, 0.588399
  ), 
  flow_rate = c(
    30, 30, 16, NA, NA, NA, 
    20, NA, 16, 10, NA, 274, NA, NA, 30, NA, 10, NA, 16, NA, 
    20, NA, NA, 30, 30, 16
  ), 
  d = c(
    150, 150, 80, 200, 80, 150, 
    100, 200, 80, 50, 200, 300, 300, 150, 150, 200, 50, 200, 
    80, 150, 100, 200, 80, 150, 150, 80
  ), 
  len = c(
    39.845, 39.845, 
    77.274, 14.275, 36.168, 60.483, 72.446, 30, 30.079, 21.893, 
    30, 89.522, 98.782, 68.32, 79.54, 30, 21.893, 30.81, 51.972, 
    66.762, 72.446, 30.81, 51.972, 25.455, 25.455, 77.274
  ), 
  year = c(
    1986L, 
    1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 
    1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 
    1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 1986L
  ), 
  insulation = c(
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 2L, 1L, 1L, 0L, 
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
  ), 
  laying = c(
    "channel", "channel", "channel", "channel", "channel", "channel", "tunnel", 
    "channel", "room", "room", "channel", "underground", "underground", 
    "channel", "channel", "channel", "room", "channel", "room", 
    "channel", "tunnel", "channel", "channel", "channel", "channel", 
    "channel"
  ), 
  beta = c(
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    FALSE, FALSE
  ), 
  exp5k = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
  ), 
  roughness = c(
    0.0015, 0.0015, 8e-04, 0.002, 8e-04, 0.0015, 
    0.001, 0.002, 8e-04, 5e-04, 0.002, 0.003, 0.003, 0.0015, 
    0.0015, 0.002, 5e-04, 0.002, 8e-04, 0.0015, 0.001, 0.002, 
    8e-04, 0.0015, 0.0015, 8e-04), inlet = c(0, 0, 0.5, 0, 0.3, 
    0, 0.5, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0.5
  ), 
  outlet = c(
    0, 0, 0.5, 0, 0.5, 0.3, 1, 0, 0.5, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0.5
  )
)
m325_tracefw_ensample <- do.call("m325tracefw", c(DHN, verbose = FALSE, elev_tol = .5))

DHN[c("year", "insulation", "laying", "beta", "exp5k")] <- NULL
n <- length(DHN[["sender"]])

root_node <- 12
DHN[["temperature"]] <- append(rep.int(NA_real_, n - 1), 70.4942576978, root_node - 1)
DHN[["pressure"]]    <- append(rep.int(NA_real_, n - 1), 0.6135602014, root_node - 1)
DHN[["flow_rate"]]   <- append(rep.int(NA_real_, n - 1), 274.0, root_node - 1)

actual_loss <- c(
  96.7797507566675, 96.7797507566675,  71.1808264048755,     116.65263776791  ,
  71.2923787993057, 96.7931935872254,  78.5007768719699,     116.676286487434 ,
  28.6262356192016, 24.5482097144085, 116.698548270144 , 0, 153.175635850318 ,
  96.8283016183455, 96.7711148826053, 116.698548270144 ,      24.5482097144085,
  116.676286487434 , 28.6172734296851,  96.7927053107223,      78.4922751902151,
  116.651999252483 , 71.2845855910305,  96.7636915531738,      96.7636915531738,
  71.1243060409466
)

test_that("*tracefw* errs in calculation without execution parallelization", {
  tracefw_report <- do.call(
    "tracefw", c(as.list(DHN), list(loss = actual_loss), verbose = FALSE, elev_tol = .5)
  )

  expect_equal(
    all(colnames(tracefw_report) == colnames(m325_tracefw_ensample)),
    TRUE
  )

  expect_equal(
    tracefw_report[["temperature"]], m325_tracefw_ensample[["temperature"]]
  )

  expect_equal(
    tracefw_report[["pressure"]], m325_tracefw_ensample[["pressure"]]
  )

  expect_equal(
    tracefw_report[["flow_rate"]], m325_tracefw_ensample[["flow_rate"]]
  )

  expect_equal(
    tracefw_report[["loss"]], m325_tracefw_ensample[["loss"]]
  )
  rm(tracefw_report)
})

test_that("*tracefw* errs in calculation utilizing parallel execution (if possible)", {
  tracefw_report <- do.call(
    "tracefw", c(as.list(DHN), 
    list(loss = actual_loss, use_cluster = !nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))), verbose = FALSE, elev_tol = .5)
  )

  expect_equal(
    all(colnames(tracefw_report) == colnames(m325_tracefw_ensample)),
    TRUE
  )

  expect_equal(
    tracefw_report[["temperature"]], m325_tracefw_ensample[["temperature"]]
  )

  expect_equal(
    tracefw_report[["pressure"]], m325_tracefw_ensample[["pressure"]]
  )

  expect_equal(
    tracefw_report[["flow_rate"]], m325_tracefw_ensample[["flow_rate"]]
  )

  expect_equal(
    tracefw_report[["loss"]], m325_tracefw_ensample[["loss"]]
  )
  rm(tracefw_report)
})

rm(actual_loss, root_node, n, m325_tracefw_ensample, DHN)

test_that("*tracefw* does not write csv-file", {
  file_name <- tempfile()
  tracefw(csv = TRUE, file = file_name)
  expect_equal(
    file.exists(file_name),
    TRUE
  )
   unlink(file_name)
})

test_that("*tracebw* does not write csv-file", {
  file_name <- tempfile()
  tracebw(csv = TRUE, file = file_name)
  expect_equal(
    file.exists(file_name),
    TRUE
  )
  unlink(file_name)
})

test_that("*tracebw* does not produce ensample results (formed by `m325tracebw`)", {
  DHN <- pipenostics::m325testbench
  DHN[["d"]] <- 1e3*DHN[["d"]]  # convert [m] to [mm]

  DHN[["sender"]]   <- sprintf("N%02i", DHN[["sender"]])
  DHN[["acceptor"]] <- sprintf("N%02i", DHN[["acceptor"]])

  m325_tracebw_ensample <- do.call("m325tracebw", DHN)

  DHN[c("year", "insulation", "laying", "beta", "exp5k")] <- NULL
  
  actual_loss <- c(
    96.236,           # 1
    96.288,           # 2
    70.584,           # 3
    116.0449431258,   # 4
    70.7340165868,    # 5
    96.2114863151   , # 6
    78.4,             # 7
    116.0158814785,   # 8
    28.1152,          # 9
    24.9182,          #10
    116.6790502491,   #11
    0,                #12 
    153.1336828932,   #13 
    96.7331745004,    #14 
    96.6,             #15 
    116.6668282889,   #16
    24.9596,          #17
    115.9228229505,   #18
    28.1658,          #19
    96.1226072816,    #20
    77.824,           #21
    115.9455144868,   #22
    70.6899252509,    #23
    96.184,           #24
    96.236,           #25
    70.54             #26
  )

  tracebw_report     <- do.call("tracebw", c(as.list(DHN), list(loss = actual_loss)))

  expect_equal(all(colnames(tracebw_report)        == colnames(m325_tracebw_ensample)), TRUE)
  expect_equal(all(tracebw_report[["node"]]        == m325_tracebw_ensample[["node"]])       , TRUE)
  expect_equal(all(tracebw_report[["tracing"]]     == m325_tracebw_ensample[["tracing"]])    , TRUE)
  expect_equal(all(tracebw_report[["backward"]]    == m325_tracebw_ensample[["backward"]])   , TRUE)
  expect_equal(all(tracebw_report[["aggregation"]] == m325_tracebw_ensample[["aggregation"]]), TRUE)
  expect_equal(all(tracebw_report[["job"]]         == m325_tracebw_ensample[["job"]])        , TRUE)

  expect_equal(all.equal(tracebw_report[["temperature"]], m325_tracebw_ensample[["temperature"]], tolerance = 1e-4), TRUE)
  expect_equal(all.equal(tracebw_report[["pressure"]]   , m325_tracebw_ensample[["pressure"]]   , tolerance = 1e-4), TRUE)
  expect_equal(all.equal(tracebw_report[["flow_rate"]]  , m325_tracebw_ensample[["flow_rate"]]  , tolerance = 1e-4), TRUE)
  expect_equal(all.equal(tracebw_report[["Q"]]     , m325_tracebw_ensample[["Q"]]   , tolerance = 1e-4), TRUE)
  expect_equal(all.equal(tracebw_report[["loss"]]  , m325_tracebw_ensample[["loss"]], tolerance = 1e-4), TRUE)
  expect_equal(all.equal(tracebw_report[["flux"]]  , m325_tracebw_ensample[["flux"]], tolerance = 1e-4), TRUE)
})


