library(formula.tools)

## all tests are wrapped in single function to make it easier to call on different designs,
## which is done at the end of this script

comprehensive_design_test <- function(nosim, resps, designtype, designpath, ul, bcoeff, decisiongroups = c(0, 1)) {
  # Test cases related to sim_all function
  test_that("u is not a list of lists", {
    expect_error(
      sim_all(
        nosim = nosim, resps = resps, designtype = destype,
        designpath = designpath, u = data.frame(u = " alp"), bcoeff = bcoeff
      ),
      "must be provided and must be a list containing at least one list"
    )
  })

  test_that("no value provided for  utility", {
    expect_error(
      sim_all(
        nosim = nosim, resps = resps, designtype = destype,
        designpath = designpath, bcoeff = bcoeff
      ),
      "must be provided and must be a list containing at least one list"
    )
  })

  test_that("wrong designtype", {
    expect_error(sim_all(
      nosim = nosim, resps = resps, designtype = "ng",
      designpath = designpath, u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups
    ), "Invalid value for design. Please provide either NULL, 'ngene', 'spdesign'or 'idefix',  or do not use the argument 'designtype'. NULL lets us to guess the design.")
  })


  test_that("folder does not exist", {
    expect_error(
      sim_all(
        nosim = nosim, resps = resps, designtype = destype,
        designpath = system.file("da/bullshit", package = "simulateDCE"), u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups
      ),
      "The folder where your designs are stored does not exist."
    )
  })

  test_that("seed setting makes code reproducible", {
    set.seed(3333)
    result1 <- sim_all(nosim = nosim, resps = resps, designtype = destype, designpath = designpath, u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups)

    set.seed(3333)
    result2 <- sim_all(nosim = nosim, resps = resps, designtype = destype, designpath = designpath, u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups)

    expect_identical(result1[["summaryall"]], result2[["summaryall"]])
  })

  test_that("No seed setting makes code results different", {
    result1 <- sim_all(nosim = nosim, resps = resps, designtype = destype, designpath = designpath, u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups)


    result2 <- sim_all(nosim = nosim, resps = resps, designtype = destype, designpath = designpath, u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups)

    expect_failure(expect_identical(result1[["summaryall"]], result2[["summaryall"]]))
  })

  test_that("exact and simple produce same results", {
    set.seed(3333)
    result1 <- sim_all(nosim = nosim, resps = resps, designtype = destype, designpath = designpath, u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups, utility_transform_type = "simple")

    set.seed(3333)
    result2 <- sim_all(nosim = nosim, resps = resps, designtype = destype, designpath = designpath, u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups, , utility_transform_type = "exact")

    expect_identical(result1[["summaryall"]], result2[["summaryall"]])
  })


  test_that("Length of utility functions matches number of decision groups", {
    # Define test inputs
    badbcoeff <- list(
      basc = 0.2,
      bcow = 0.3,
      badv = 0.3,
      bvet = 0.3,
      bfar = 0.3,
      bmet = 0.3,
      bbon = 0.3,
      bbon2 = 1.9,
      basc2 = 2
    )

    badlist <- list(
      u1 = list(
        v1 = V.1 ~ bcow * alt1.cow + badv * alt1.adv + bvet * alt1.vet + bfar * alt1.far + bmet * alt1.met + bbon * alt1.bon,
        v2 = V.2 ~ bcow * alt2.cow + badv * alt2.adv + bvet * alt2.vet + bfar * alt2.far + bmet * alt2.met + bbon * alt2.bon,
        v3 = V.3 ~ basc
      ),
      u2 = list(
        v1 = V.1 ~ bcow * alt1.cow + badv * alt1.adv + bvet * alt1.vet + bfar * alt1.far + bbon * alt1.bon,
        v2 = V.2 ~ bcow * alt2.cow + badv * alt2.adv + bvet * alt2.vet + bfar * alt2.far + bbon * alt2.bon,
        v3 = V.3 ~ basc
      ),
      u3 = list(
        v1 = V.1 ~ bbon2 * alt1.bon,
        v2 = V.2 ~ bbon2 * alt2.bon,
        v3 = V.3 ~ basc
      ),
      u4 = list(
        v1 = V.1 ~ basc2 + bcow * alt1.cow + badv * alt1.adv + bvet * alt1.vet + bfar * alt1.far + bmet * alt1.met + bbon * alt1.bon,
        v2 = V.2 ~ bcow * alt2.cow + badv * alt2.adv + bvet * alt2.vet + bfar * alt2.far + bmet * alt2.met + bbon * alt2.bon,
        v3 = V.3 ~ basc
      )
    )
    baddecisiongroups <- c(0, 0.3, 0.6, 1)

    # Test that the function throws an error when lengths don't match
    expect_error(sim_all(nosim = nosim, resps = resps, designtype = destype, designpath = designpath, u = badlist, bcoeff = badbcoeff, decisiongroups = baddecisiongroups), "Number of decision groups must equal number of utility functions!")

    # Define test inputs where lengths match
    gooddecisiongroups <- c(0, 0.3, 0.6, 0.8, 1)

    # Test that the function does not throw an error when lengths match (assumed true in input)
    expect_no_error(sim_all(nosim = nosim, resps = resps, designtype = destype, designpath = designpath, u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups))
  })


  ########### Additional Tests ##############
  test_that("bcoeff is provided", {
    expect_error(sim_all(
      nosim = nosim, resps = resps, designtype = destype,
      designpath = designpath, u = ul
    ))
  })

  test_that("bcoeff contains valid values", {
    expect_error(sim_all(
      nosim = nosim, resps = resps, designtype = destype,
      designpath = designpath, u = ul, bcoeff = list(bsq = "invalid")
    ))
  })

  test_that("bcoeff is a list", {
    expect_error(sim_all(
      nosim = nosim, resps = resps, designtype = destype,
      designpath = designpath, u = ul, bcoeff = "not a list"
    ))
  })

  test_that("B coefficients in the utility functions dont match those in the bcoeff list", {
    expect_error(sim_all(
      nosim = nosim, resps = resps, designtype = destype,
      designpath = designpath, u = ul, bcoeff = list(bWRONG = 0.00)
    ))
  })

  test_that("Utility functions are valid", {
    expect_no_error(eval(ul$u1$v1))
    expect_no_error(eval(ul$u1$v2))
  })

  test_that("Design path must be a valid directory", {
    # Test case: designpath is not a character string
    expect_error(sim_all(nosim = nosim, resps = resps, designtype = destype, designpath = 123, u = ul, bcoeff = bcoeff))

    # Test case: designpath does not exist
    expect_error(sim_all(nosim = nosim, resps = resps, designtype = destype, designpath = "/nonexistent/path", u = ul, bcoeff = bcoeff))

    # Test case: designpath is not a directory
    expect_error(sim_all(nosim = nosim, resps = resps, designtype = destype, designpath = "path/to/a/file.txt", u = ul, bcoeff = bcoeff))
  })

  test_that("Resps must be an integer", {
    # Test case: resps is missing
    expect_error(sim_all(nosim = nosim, designtype = destype, designpath = designpath, u = ul, bcoeff = bcoeff))

    # Test case: resps is not an integer
    expect_error(sim_all(nosim = nosim, resps = "abc", designtype = destype, designpath = designpath, u = ul, bcoeff = bcoeff))

    # Test case: resps is a numeric but not an integer
    expect_error(sim_all(nosim = nosim, resps = 1.5, designtype = destype, designpath = designpath, u = ul, bcoeff = bcoeff))
  })

  test_that("Function exists in simulateDCE", {
    expect_true("sim_all" %in% ls("package:simulateDCE"))
  })

  test_that("Simulation results are reasonable", {
    result1 <- sim_all(nosim = nosim, resps = resps, designtype = destype, designpath = designpath, u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups)

    # obtain the names of the design files (without extensions)
    designs <- tools::file_path_sans_ext(list.files(designpath, full.names = FALSE))

    ## Now access the summary data. this is tricky because a different table is made for each design
    ## to address this, loop through all designs and run tests on each design
    for (design in designs) {
      # Access the summary data frame for the current design
      summaryTable <- result1[[design]][["summary"]]

      ## nested loop looks at each row starting with est_ (estimated bcoeffs)
      for (row_name in rownames(summaryTable)) {
        if (startsWith(row_name, "est_")) {
          betaCoeff <- sub("^est_", "", row_name) # simpkly strip prefix
          meanBeta <- summaryTable[row_name, "mean"]
          inputBeta <- as.integer(bcoeff[betaCoeff]) # access beta coefficient list by specific coefficient name

          # Perform tests for rows starting with "est_"
          expect_true(
            betaCoeff %in% names(bcoeff),
            sprintf("Variable est_%s does not exist in summary data frame", variable)
          )


          expect_gt(meanBeta, inputBeta - 1)
          expect_lt(meanBeta, inputBeta + 1)
        }
      }
    }
  })
}

###################
## FROM RBOOK #####
###################

designpath <- system.file("extdata", "Rbook", package = "simulateDCE")

# notes <- "This design consists of different heuristics. One group did not attend the methan attribute, another group only decided based on the payment"

notes <- "No Heuristics"

resps <- 40 # number of respondents
nosim <- 2 # number of simulations to run (about 500 is minimum)

# betacoefficients should not include "-"

bcoeff <- list(
  bsq = 0.00,
  bredkite = -0.05,
  bdistance = 0.50,
  bcost = -0.05,
  bfarm2 = 0.25,
  bfarm3 = 0.50,
  bheight2 = 0.25,
  bheight3 = 0.50
)

destype <- "spdesign"


# place your utility functions here
ul <- list(u1 = list(
  v1 = V.1 ~ bsq * alt1.sq,
  v2 = V.2 ~ bfarm2 * alt2.farm2 + bfarm3 * alt2.farm3 + bheight2 * alt2.height2 + bheight3 * alt2.height3 + bredkite * alt2.redkite + bdistance * alt2.distance + bcost * alt2.cost,
  v3 = V.3 ~ bfarm2 * alt3.farm2 + bfarm3 * alt3.farm3 + bheight2 * alt3.height2 + bheight3 * alt3.height3 + bredkite * alt3.redkite + bdistance * alt3.distance + bcost * alt3.cost
))

comprehensive_design_test(nosim = nosim, resps = resps, designtype = destype, designpath = designpath, ul = ul, bcoeff = bcoeff)

###############################
#### From feedadditives #######
###############################

designpath <- system.file("extdata", "feedadditives", package = "simulateDCE")

# notes <- "This design consists of different heuristics. One group did not attend the methan attribute, another group only decided based on the payment"

notes <- "Three heuristics"

resps <- 30 # number of respondents
nosim <- 2 # number of simulations to run (about 500 is minimum)
destype <- "ngene"

# betacoefficients should not include "-"
bcoeff <- list(
  basc = 0.2,
  bcow = 0.3,
  badv = 0.3,
  bvet = 0.3,
  bfar = 0.3,
  bmet = 0.3,
  bbon = 0.3,
  bbon2 = 1.9,
  basc2 = 2
)

dgFeed <- c(0, 0.3, 0.6, 0.8, 1)

# place your utility functions here
ul <- list(
  u1 = list(
    v1 = V.1 ~ bcow * alt1.cow + badv * alt1.adv + bvet * alt1.vet + bfar * alt1.far + bmet * alt1.met + bbon * alt1.bon,
    v2 = V.2 ~ bcow * alt2.cow + badv * alt2.adv + bvet * alt2.vet + bfar * alt2.far + bmet * alt2.met + bbon * alt2.bon,
    v3 = V.3 ~ basc
  ),
  u2 = list(
    v1 = V.1 ~ bcow * alt1.cow + badv * alt1.adv + bvet * alt1.vet + bfar * alt1.far + bbon * alt1.bon,
    v2 = V.2 ~ bcow * alt2.cow + badv * alt2.adv + bvet * alt2.vet + bfar * alt2.far + bbon * alt2.bon,
    v3 = V.3 ~ basc
  ),
  u3 = list(
    v1 = V.1 ~ bbon2 * alt1.bon,
    v2 = V.2 ~ bbon2 * alt2.bon,
    v3 = V.3 ~ basc
  ),
  u4 = list(
    v1 = V.1 ~ basc2 + bcow * alt1.cow + badv * alt1.adv + bvet * alt1.vet + bfar * alt1.far + bmet * alt1.met + bbon * alt1.bon,
    v2 = V.2 ~ bcow * alt2.cow + badv * alt2.adv + bvet * alt2.vet + bfar * alt2.far + bmet * alt2.met + bbon * alt2.bon,
    v3 = V.3 ~ basc
  )
)

## comprehensive_design_test(nosim=nosim, resps=resps, destype=destype, designpath=designpath, ul = ul, bcoeff = bcoeff, decisiongroups = dgFeed)
