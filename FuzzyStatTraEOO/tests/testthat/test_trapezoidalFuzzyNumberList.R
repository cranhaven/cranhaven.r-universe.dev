
test_that("TrapezoidalTrapezoidalFuzzyNumberList instance works (initialize)",
          {
            ## CONSTRUCTOR ERRORS
            ## more parameters than needed
            expect_error(TrapezoidalTrapezoidalFuzzyNumberList$new(
              c(
                TrapezoidalFuzzyNumber$new(1, 2, 3, 4),
                TrapezoidalFuzzyNumber$new(-8, -6, -4, -2),
                TrapezoidalFuzzyNumber$new(-1, -1, 2, 3),
                TrapezoidalFuzzyNumber$new(1, 2, 3, 3)
              ),
              "a"
            ))
            ## invalid parameter: not a list
            expect_error(TrapezoidalTrapezoidalFuzzyNumberList$new())
            expect_error(TrapezoidalFuzzyNumberList$new(3))
            expect_error(TrapezoidalFuzzyNumberList$new("a"))
            expect_error(TrapezoidalFuzzyNumberList$new(c(1, 2, 3)))
            ## invalid parameter: list that does not contain TrapezoidalFuzzyNumbers
            c <-
              c(FuzzyNumber$new(array(c(
                0.0, 1.0, -1.5, -1.0, 2, 1
              ), dim = c(2, 3))), FuzzyNumber$new(array(
                c(0.0, 0.5, 1.0, -1.5, -1.0, -1.0, 2.0, 1.5, 1.0), dim = c(3, 3)
              )))
            expect_error(TrapezoidalFuzzyNumberList$new(c))
            ## invalid parameter: list does not only contain TrapezoidalFuzzyNumbers
            c <-
              c(TrapezoidalFuzzyNumber$new(1, 2, 3, 4),
                FuzzyNumber$new(array(
                  c(0.0, 0.5, 1.0, -1.5, -1.0, -1.0, 2.0, 1.5, 1.0), dim = c(3, 3)
                )))
            expect_error(TrapezoidalFuzzyNumberList$new(c))


            # valid parameter: list of valid TrapezoidalFuzzyNumbers
            # attributes rows, dimensions, columns and numbers correctly saved
            c <-
              c(
                TrapezoidalFuzzyNumber$new(1, 2, 3, 4),
                TrapezoidalFuzzyNumber$new(-8, -6, -4, -2),
                TrapezoidalFuzzyNumber$new(-1, -1, 2, 3),
                TrapezoidalFuzzyNumber$new(1, 2, 3, 3)
              )
            array <- TrapezoidalFuzzyNumberList$new(c)

            expect_equal(class(array$getDimension(1L))[[1]], "TrapezoidalFuzzyNumber")
            expect_equal(array$getLength(), length(c))

          })

test_that("TrapezoidalFuzzyNumberList dthetaphi function", {
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-2.015641,-1.094014,  1.2814199,  1.8966197),
      TrapezoidalFuzzyNumber$new(-4.829141,-1.605457,-0.4205031,-0.3803774)
    ))

  ## not all mandatory parameters
  expect_error(array$dthetaphi())

  ## more parameters than needed
  expect_error(array$dthetaphi("a", 1, 1, 1, 1))

  ## INVALID PARAMETERS
  ## s is not a TrapezoidalFuzzyNumberList
  expect_error(array$dthetaphi("a", 1, 1, 1))
  expect_error(array$dthetaphi(1, 1, 1, 1))
  expect_error(array$dthetaphi(1.5, 1, 1, 1))
  expect_error(array$dthetaphi(1L, 1, 1, 1))
  expect_error(array$dthetaphi(c(1, 2, 3, 4), 1, 1, 1))
  expect_error(array$dthetaphi(list(), 1, 1, 1))

  s <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
      TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
    ))

  ## invalid a, b or theta
  expect_error(array$dthetaphi(s, 0, 0, 0))
  expect_error(array$dthetaphi(s, "a", 1, 1))
  expect_error(array$dthetaphi(s, 1, 0L, 1))
  expect_error(array$dthetaphi(s, 1, -1.5, 1))
  expect_error(array$dthetaphi(s, 1 / 8, 1.9876, c(1, 2, 3, 4)))
  expect_error(array$dthetaphi(s, 1 / 8, 1.9876, list()))
  expect_error(array$dthetaphi(s, Inf, 1.9876, 1))
  expect_error(array$dthetaphi(s, 1 / 8, 1.9876,-Inf))

  ## all conditions fulfilled
  s <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
      TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
    ))

  expect_equal(array$dthetaphi(s), matrix(
    c(
      1.3820283732353056,
      2.5109358211914543,
      2.8244145900576512,
      3.1226035776940817
    ),
    nrow = 2,
    ncol = 2
  ))
  expect_equal(array$dthetaphi(s, 1), matrix(
    c(
      1.3820283732353056,
      2.5109358211914543,
      2.8244145900576512,
      3.1226035776940817
    ),
    nrow = 2,
    ncol = 2
  ))
  expect_equal(array$dthetaphi(s, 1, 1, 1), matrix(
    c(
      1.3820283732353056,
      2.5109358211914543,
      2.8244145900576512,
      3.1226035776940817
    ),
    nrow = 2,
    ncol = 2
  ))

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)

  set.seed(1234)
  dataTra2 <- Simulation$new()$simulCase4(5L)

  expect_equal(dataTra1$dthetaphi(dataTra2), matrix(
    c(
      1.98903699142224144,
      0.65103226857132857,
      0.30421635567767563,
      2.0087820093496189,
      0.6668008312690179,
      0.2843941899460920,
      1.83170169322028209,
      0.47633978008078892,
      0.37009413273750180,
      1.89599304438611060,
      0.54212396263371165,
      0.31867015418821776,
      1.64027261862807050,
      0.28806909711972234,
      0.54382475700158817
    ),
    nrow = 3,
    ncol = 5
  ))

})

test_that("TrapezoidalFuzzyNumberList dwablphi function", {
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-2.015641,-1.094014,  1.2814199,  1.8966197),
      TrapezoidalFuzzyNumber$new(-4.829141,-1.605457,-0.4205031,-0.3803774)
    ))

  ## not all mandatory parameters
  expect_error(array$dwablphi())

  ## more parameters than needed
  expect_error(array$dwablphi("a", 1, 1, 1, 1))

  ## INVALID PARAMETERS
  ## s is not a TrapezoidalFuzzyNumberList
  expect_error(array$dwablphi("a", 1, 1, 1))
  expect_error(array$dwablphi(1, 1, 1, 1))
  expect_error(array$dwablphi(1.5, 1, 1, 1))
  expect_error(array$dwablphi(1L, 1, 1, 1))
  expect_error(array$dwablphi(c(1, 2, 3, 4), 1, 1, 1))
  expect_error(array$dwablphi(list(), 1, 1, 1))
  expect_error(array$dwablphi(1L, Inf, 1, 1))
  expect_error(array$dwablphi(1L, 1, 1,-Inf))

  s <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
      TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
    ))

  ## invalid a, b or theta
  expect_error(array$dwablphi(s, 0, 0, 0))
  expect_error(array$dwablphi(s, "a", 1, 1))
  expect_error(array$dwablphi(s, 1, 0L, 1))
  expect_error(array$dwablphi(s, 1, -1.5, 1))
  expect_error(array$dwablphi(s, 1 / 8, 1.9876, c(1, 2, 3, 4)))
  expect_error(array$dwablphi(s, 1 / 8, 1.9876, list()))

  ## all conditions fulfilled
  s <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
      TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
    ))

  expect_equal(array$dwablphi(s), matrix(
    c(1.659267750, 3.328956966, 3.4620668, 3.9779599),
    nrow = 2,
    ncol = 2
  ))
  expect_equal(array$dwablphi(s, 1, 1), matrix(
    c(1.659267750, 3.328956966, 3.4620668, 3.9779599),
    nrow = 2,
    ncol = 2
  ))
  expect_equal(array$dwablphi(s, 1, 1, 1), matrix(
    c(1.659267750, 3.328956966, 3.4620668, 3.9779599),
    nrow = 2,
    ncol = 2
  ))

  expect_equal(array$dwablphi(s, 5, 1, 1), matrix(
    c(1.509121050, 2.404240217, 3.457647733, 3.925708967),
    nrow = 2,
    ncol = 2
  ))

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)

  set.seed(1234)
  dataTra2 <- Simulation$new()$simulCase4(5L)

  expect_equal(dataTra1$dwablphi(dataTra2), matrix(
    c(
      2.121130664700480,
      0.829923724294429,
      0.424901375196048,
      2.140485927797220,
      0.838380525431005,
      0.394359945883828,
      1.972939587563330,
      0.562506565098849,
      0.457846941063138,
      2.009668217365710,
      0.649466438567776,
      0.420714654544369,
      1.758104378978400,
      0.354362674669675,
      0.630022765053419
    ),
    nrow = 3,
    ncol = 5
  ))

})

test_that("TrapezoidalFuzzyNumberList hyperI function", {
  arrayR <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
      TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774)
    ))

  ## more parameters than needed
  expect_error(arrayR$hyperI("a"))

  ## not all trapezoidal fuzzy numbers are positive
  arrayR <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
      TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774)
    ))
  expect_output(arrayR$hyperI(), "all the fuzzy numbers should be positive")

  arrayS <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(0.1311532,  0.3400337, 0.3976215, 0.7372827),
      TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
    ))
  expect_output(arrayS$hyperI(), "all the fuzzy numbers should be positive")

  ## all trapezoidal fuzzy numbers are positive and valid
  arrayR <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(2.015641, 5.094014,  5.2814199, 5.8966197),
      TrapezoidalFuzzyNumber$new(4.829141, 6.605457, 7.4205031, 8.3803774)
    ))
  expect_equal(arrayR$hyperI(), 0.1668356458733955)
  expect_equal(arrayR$hyperI(0.265897), 0.1668356458733955)

  arrayS <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(0.1311532,  0.3400337, 0.3976215, 0.7372827),
      TrapezoidalFuzzyNumber$new(1.4843364, 2.5494522, 3.4790804, 3.6759589)
    ))
  expect_equal(arrayS$hyperI(), 2.2572456011203132)
  expect_equal(arrayS$hyperI(0.5), 2.2572456011203132)

  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(1, 1, 2, 2),
        TrapezoidalFuzzyNumber$new(1, 1, 2, 3),
        TrapezoidalFuzzyNumber$new(1, 2, 3, 3),
        TrapezoidalFuzzyNumber$new(1, 2, 2, 3)
      )
    )
  expect_equal(array$hyperI(), 0.32180611)

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase4(5L)

  expect_equal(dataTra1$hyperI(), 0.33760923351909189)
})

test_that("TrapezoidalFuzzyNumberList mean function", {
  ## all trapezoidal fuzzy numbers are valid
  arrayR <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
      TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774)
    ))

  ## more parameters than needed
  expect_error(arrayR$mean("a"))

  expect_equal(arrayR$mean(), TrapezoidalFuzzyNumberList$new(c(
    TrapezoidalFuzzyNumber$new(
      -3.4223910000000002,-1.3497355,
      0.43045839999999996,
      0.75812115000000002
    )
  )))

  arrayS <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
      TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
    ))
  expect_equal(arrayS$mean(), TrapezoidalFuzzyNumberList$new(c(
    TrapezoidalFuzzyNumber$new(-2.8077448,-2.10470925, 1.93835095, 2.2066208)
  )))

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)

  expect_equal(dataTra1$mean(), TrapezoidalFuzzyNumberList$new(c(
    TrapezoidalFuzzyNumber$new(
      -0.20069420379579875,
      -0.099738824121607825,
      0.20294193706047123,
      0.46582489545668493
    )
  )))
})

test_that("TrapezoidalFuzzyNumberList gsi function", {
  arrayR <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
      TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197)
    ))

  ## more parameters than needed
  expect_error(arrayR$gsi("a"))

  ## ALL TRAPEZOIDAL FUZZY NUMBERS ARE VALID
  ## same trapezoidal fuzzy numbers
  expect_equal(arrayR$gsi(), 0)

  arrayR <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774)
      )
    )
  expect_equal(arrayR$gsi(), 0.5)

  arrayR <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774)
      )
    )
  expect_equal(arrayR$gsi(), 0.44444444444444442)

  arrayR <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
        TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589),
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197)
      )
    )
  expect_equal(arrayR$gsi(), 0.625)

  arrayR <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
        TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589),
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197)
      )
    )
  expect_equal(arrayR$gsi(), 0.72)

  ## different trapezoidal fuzzy numbers
  arrayR <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197)
    ))
  expect_equal(arrayR$gsi(), 0)

  arrayR <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
      TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774)
    ))
  expect_equal(arrayR$gsi(), 0.5)

  arrayS <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
      TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
    ))
  expect_equal(arrayS$gsi(), 0.5)

  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
        TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
      )
    )
  expect_equal(array$gsi(), 0.75)

  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
        TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589),
        TrapezoidalFuzzyNumber$new(-1.0737003,-0.05878986,  0.6253104, 1.77892339),
        TrapezoidalFuzzyNumber$new(-13.3980115,-5.75576494,  2.4305524, 3.43680774),
        TrapezoidalFuzzyNumber$new(0.1299523,  0.13269168,  2.0643605, 4.43796608),
        TrapezoidalFuzzyNumber$new(1.7508453,  1.75805494,  2.0415862, 2.18392221)
      )
    )
  expect_equal(array$gsi(), 0.875)

  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
        TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589),
        TrapezoidalFuzzyNumber$new(-1.0737003,-0.05878986,  0.6253104, 1.77892339),
        TrapezoidalFuzzyNumber$new(-13.3980115,-5.75576494,  2.4305524, 3.43680774),
        TrapezoidalFuzzyNumber$new(0.1299523,  0.13269168,  2.0643605, 4.43796608),
        TrapezoidalFuzzyNumber$new(1.7508453,  1.75805494,  2.0415862, 2.18392221),
        TrapezoidalFuzzyNumber$new(-7.0842980,-4.41079508,  2.9970012, 3.03942180),
        TrapezoidalFuzzyNumber$new(-2.1544108,-1.05061570,  1.7329192, 2.13557275)
      )
    )
  expect_equal(array$gsi(), 0.90000000000000002)

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)

  expect_equal(dataTra1$gsi(), 0.66666666666666674)

})

test_that("TrapezoidalFuzzyNumberList rho1 function", {
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-2.015641,-1.094014,  1.2814199,  1.8966197),
      TrapezoidalFuzzyNumber$new(-4.829141,-1.605457,-0.4205031,-0.3803774)
    ))

  ## not all mandatory parameters
  expect_error(array$rho1())

  ## more parameters than needed
  expect_error(array$rho1("a", 1))

  ## INVALID PARAMETERS
  ## s is not a TrapezoidalFuzzyNumberList
  expect_error(array$rho1("a"))
  expect_error(array$rho1(1))
  expect_error(array$rho1(1.5))
  expect_error(array$rho1(1L))
  expect_error(array$rho1(c(1, 2, 3, 4)))
  expect_error(array$rho1(list()))

  ## all conditions fulfilled
  s <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
      TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
    ))

  expect_equal(array$rho1(s), matrix(
    c(
      1.3404177249999998,
      2.1448157999999999,
      2.7252833250000137,
      2.8887776000000001
    ),
    nrow = 2,
    ncol = 2
  ))

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)

  set.seed(1234)
  dataTra2 <- Simulation$new()$simulCase4(5L)

  expect_equal(dataTra1$rho1(dataTra2), matrix(
    c(
      1.982595687242530,
      0.614200284137185,
      0.242513335630809,
      2.002094802466930,
      0.633699399361583,
      0.231471021542988,
      1.823348070963940,
      0.454952667858595,
      0.342186031143459,
      1.890755370241320,
      0.522359967135980,
      0.276547538292689,
      1.633778730159900,
      0.265383327054553,
      0.531204996647869
    ),
    nrow = 3,
    ncol = 5
  ))

})

test_that("TrapezoidalFuzzyNumberList medianWabl function", {
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-3.3293414, -2.51760323,  1.32707594,  6.2201171)
    ))

  ## more parameters than needed
  expect_error(array$medianWabl(0, 0, 0, 0))

  ## INVALID PARAMETERS
  ## invalid nl
  expect_error(array$medianWabl(0, 0, 0))
  expect_error(array$medianWabl(0, 0, 0))
  expect_error(array$medianWabl("a", 1, 1))
  expect_error(array$medianWabl(0L, 0, 0))
  expect_error(array$medianWabl(1L, 0, 0))

  ## invalid a and/or b
  expect_error(array$medianWabl(2L, 0L, 1))
  expect_error(array$medianWabl(2L, -1.5, 1))
  expect_error(array$medianWabl(2L, 1.9876, c(1, 2, 3, 4)))
  expect_error(array$medianWabl(2L, 1.9876, list()))
  expect_error(array$medianWabl(2L, Inf, 1))
  expect_error(array$medianWabl(2L, 1,-Inf))

  ## valid parameters and valid TrapezoidalFuzzyNumbers
  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-3.3293414, -2.51760323,  1.32707594,  6.2201171),
        TrapezoidalFuzzyNumber$new(-0.9261896,  0.05988146,  0.47780304,  1.7574181),
        TrapezoidalFuzzyNumber$new(-2.6192779, -1.32427456, -1.12769469,  1.2941994),
        TrapezoidalFuzzyNumber$new(-4.4067658, -0.89778268,  0.08638159,  0.5807197),
        TrapezoidalFuzzyNumber$new(-0.2516410, -0.25020798,  1.40040190,  1.6762925),
        TrapezoidalFuzzyNumber$new(1.7360987,  1.74229238,  2.03007900,  2.0557024),
        TrapezoidalFuzzyNumber$new(-2.3597156, -0.85490726,  1.25757245,  3.4584208),
        TrapezoidalFuzzyNumber$new(-1.6963781, -0.92574816, -0.71187987,  4.7376725),
        TrapezoidalFuzzyNumber$new(-1.4707440, -0.85529186, -0.41129714, -0.3987298),
        TrapezoidalFuzzyNumber$new(-0.8977640, -0.59381095, -0.57915858, -0.2247244)
      )
    )

  expect_equal(array$medianWabl()$getDimension(1L)$getAlphaLevels(),
               matrix(c(seq(0, 1, len = 101)),
                      ncol = 1))
  expect_equal(array$medianWabl()$getDimension(1L)$getInfimums(),
               matrix(
                 c(
                   -1.125144485,-1.113739113,-1.102333741,-1.090928369,-1.079522997,-1.068117625,-1.056712253,-1.045306881,-1.033901509,-1.022496137,-1.011090765,-0.999685393,-0.988280021,-0.976874649,-0.965469277,-0.954063905,-0.942658533,-0.931253161,-0.919847789,-0.908442417,-0.897037045,-0.885631673,-0.874226301,-0.862820929,-0.851415557,-0.840010185,-0.828604813,-0.817199441,-0.805794069,-0.794388697,-0.782983325,-0.771577953,-0.760172581,-0.748767209,-0.737361837,-0.725956465,-0.714551093,-0.703145721,-0.692288829,-0.685806647,-0.679324465,-0.672842284,-0.666360102,-0.65987792,-0.653395738,-0.646913557,-0.640431375,-0.633949193,-0.627467011,-0.620984829,-0.614502648,-0.608020466,-0.601538284,-0.595056102,-0.58857392,-0.582091738,-0.575609557,-0.569127375,-0.562645193,-0.556163011,-0.549680829,-0.543198648,-0.536716466,-0.530234284,-0.523752102,-0.51726992,-0.510787739,-0.504305557,-0.481282165,-0.457262233,-0.4332423,-0.409222368,-0.385202436,-0.361182503,-0.337162571,-0.313142639,-0.289122706,-0.265102774,-0.241082842,-0.224120097,-0.212714725,-0.201309353,-0.189903981,-0.178498609,-0.167093237,-0.155687865,-0.144282493,-0.132877121,-0.121471749,-0.110066377,-0.098661005,-0.087255633,-0.075850261,-0.064444889,-0.053039517,-0.041634145,-0.030228773,-0.018823401,-0.007418029,
                   0.003987343,
                   0.015392715
                 ),
                 ncol = 1
               ))
  expect_equal(array$medianWabl()$getDimension(1L)$getSupremums(),
               matrix(
                 c(
                   1.924208095,
                   1.915338329,
                   1.906468563,
                   1.897598797,
                   1.888729032,
                   1.879859266,
                   1.870989500,
                   1.862119734,
                   1.853249968,
                   1.844380202,
                   1.835510437,
                   1.826640671,
                   1.817770905,
                   1.808901139,
                   1.800031373,
                   1.791161607,
                   1.782291841,
                   1.773422076,
                   1.764552310,
                   1.755682544,
                   1.746812778,
                   1.737943012,
                   1.729073246,
                   1.720203480,
                   1.711333715,
                   1.702463949,
                   1.692201825,
                   1.673694280,
                   1.655186734,
                   1.636679188,
                   1.618171642,
                   1.599664097,
                   1.581156551,
                   1.562649005,
                   1.544141460,
                   1.525633914,
                   1.507126368,
                   1.488618822,
                   1.470111277,
                   1.456025648,
                   1.442536725,
                   1.429047801,
                   1.415558878,
                   1.402069954,
                   1.388581031,
                   1.375092107,
                   1.361603184,
                   1.348114260,
                   1.334625337,
                   1.321136413,
                   1.307647490,
                   1.294158567,
                   1.280669643,
                   1.267180720,
                   1.253691796,
                   1.240202873,
                   1.226713949,
                   1.213225026,
                   1.199736102,
                   1.186247179,
                   1.172758256,
                   1.159269332,
                   1.145780409,
                   1.132291485,
                   1.118802562,
                   1.100960944,
                   1.061603712,
                   1.022246480,
                   0.982889247,
                   0.943532015,
                   0.904174783,
                   0.864817550,
                   0.840854721,
                   0.822347175,
                   0.803839629,
                   0.785332084,
                   0.766824538,
                   0.749320072,
                   0.742859160,
                   0.736398248,
                   0.729937336,
                   0.723476424,
                   0.717015512,
                   0.710554600,
                   0.704093688,
                   0.697632776,
                   0.691171864,
                   0.684710952,
                   0.678250040,
                   0.671789128,
                   0.665328216,
                   0.658867304,
                   0.652406392,
                   0.645945480,
                   0.639484568,
                   0.633023656,
                   0.626562744,
                   0.620101832,
                   0.613640920,
                   0.607180008,
                   0.602449641
                 ),
                 ncol = 1
               ))

  expect_equal(array$medianWabl(101L)$getDimension(1L)$getAlphaLevels(),
               matrix(c(seq(0, 1, len = 101)),
                      ncol = 1))
  expect_equal(array$medianWabl(101L)$getDimension(1L)$getInfimums(),
               matrix(
                 c(
                   -1.125144485,-1.113739113,-1.102333741,-1.090928369,-1.079522997,-1.068117625,-1.056712253,-1.045306881,-1.033901509,-1.022496137,-1.011090765,-0.999685393,-0.988280021,-0.976874649,-0.965469277,-0.954063905,-0.942658533,-0.931253161,-0.919847789,-0.908442417,-0.897037045,-0.885631673,-0.874226301,-0.862820929,-0.851415557,-0.840010185,-0.828604813,-0.817199441,-0.805794069,-0.794388697,-0.782983325,-0.771577953,-0.760172581,-0.748767209,-0.737361837,-0.725956465,-0.714551093,-0.703145721,-0.692288829,-0.685806647,-0.679324465,-0.672842284,-0.666360102,-0.65987792,-0.653395738,-0.646913557,-0.640431375,-0.633949193,-0.627467011,-0.620984829,-0.614502648,-0.608020466,-0.601538284,-0.595056102,-0.58857392,-0.582091738,-0.575609557,-0.569127375,-0.562645193,-0.556163011,-0.549680829,-0.543198648,-0.536716466,-0.530234284,-0.523752102,-0.51726992,-0.510787739,-0.504305557,-0.481282165,-0.457262233,-0.4332423,-0.409222368,-0.385202436,-0.361182503,-0.337162571,-0.313142639,-0.289122706,-0.265102774,-0.241082842,-0.224120097,-0.212714725,-0.201309353,-0.189903981,-0.178498609,-0.167093237,-0.155687865,-0.144282493,-0.132877121,-0.121471749,-0.110066377,-0.098661005,-0.087255633,-0.075850261,-0.064444889,-0.053039517,-0.041634145,-0.030228773,-0.018823401,-0.007418029,
                   0.003987343,
                   0.015392715
                 ),
                 ncol = 1
               ))
  expect_equal(array$medianWabl(101L)$getDimension(1L)$getSupremums(),
               matrix(
                 c(
                   1.924208095,
                   1.915338329,
                   1.906468563,
                   1.897598797,
                   1.888729032,
                   1.879859266,
                   1.870989500,
                   1.862119734,
                   1.853249968,
                   1.844380202,
                   1.835510437,
                   1.826640671,
                   1.817770905,
                   1.808901139,
                   1.800031373,
                   1.791161607,
                   1.782291841,
                   1.773422076,
                   1.764552310,
                   1.755682544,
                   1.746812778,
                   1.737943012,
                   1.729073246,
                   1.720203480,
                   1.711333715,
                   1.702463949,
                   1.692201825,
                   1.673694280,
                   1.655186734,
                   1.636679188,
                   1.618171642,
                   1.599664097,
                   1.581156551,
                   1.562649005,
                   1.544141460,
                   1.525633914,
                   1.507126368,
                   1.488618822,
                   1.470111277,
                   1.456025648,
                   1.442536725,
                   1.429047801,
                   1.415558878,
                   1.402069954,
                   1.388581031,
                   1.375092107,
                   1.361603184,
                   1.348114260,
                   1.334625337,
                   1.321136413,
                   1.307647490,
                   1.294158567,
                   1.280669643,
                   1.267180720,
                   1.253691796,
                   1.240202873,
                   1.226713949,
                   1.213225026,
                   1.199736102,
                   1.186247179,
                   1.172758256,
                   1.159269332,
                   1.145780409,
                   1.132291485,
                   1.118802562,
                   1.100960944,
                   1.061603712,
                   1.022246480,
                   0.982889247,
                   0.943532015,
                   0.904174783,
                   0.864817550,
                   0.840854721,
                   0.822347175,
                   0.803839629,
                   0.785332084,
                   0.766824538,
                   0.749320072,
                   0.742859160,
                   0.736398248,
                   0.729937336,
                   0.723476424,
                   0.717015512,
                   0.710554600,
                   0.704093688,
                   0.697632776,
                   0.691171864,
                   0.684710952,
                   0.678250040,
                   0.671789128,
                   0.665328216,
                   0.658867304,
                   0.652406392,
                   0.645945480,
                   0.639484568,
                   0.633023656,
                   0.626562744,
                   0.620101832,
                   0.613640920,
                   0.607180008,
                   0.602449641
                 ),
                 ncol = 1
               ))

  expect_equal(array$medianWabl(101L, 1)$getDimension(1L)$getAlphaLevels(),
               matrix(c(seq(0, 1, len = 101)),
                      ncol = 1))
  expect_equal(array$medianWabl(101L, 1)$getDimension(1L)$getInfimums(),
               matrix(
                 c(
                   -1.125144485,-1.113739113,-1.102333741,-1.090928369,-1.079522997,-1.068117625,-1.056712253,-1.045306881,-1.033901509,-1.022496137,-1.011090765,-0.999685393,-0.988280021,-0.976874649,-0.965469277,-0.954063905,-0.942658533,-0.931253161,-0.919847789,-0.908442417,-0.897037045,-0.885631673,-0.874226301,-0.862820929,-0.851415557,-0.840010185,-0.828604813,-0.817199441,-0.805794069,-0.794388697,-0.782983325,-0.771577953,-0.760172581,-0.748767209,-0.737361837,-0.725956465,-0.714551093,-0.703145721,-0.692288829,-0.685806647,-0.679324465,-0.672842284,-0.666360102,-0.65987792,-0.653395738,-0.646913557,-0.640431375,-0.633949193,-0.627467011,-0.620984829,-0.614502648,-0.608020466,-0.601538284,-0.595056102,-0.58857392,-0.582091738,-0.575609557,-0.569127375,-0.562645193,-0.556163011,-0.549680829,-0.543198648,-0.536716466,-0.530234284,-0.523752102,-0.51726992,-0.510787739,-0.504305557,-0.481282165,-0.457262233,-0.4332423,-0.409222368,-0.385202436,-0.361182503,-0.337162571,-0.313142639,-0.289122706,-0.265102774,-0.241082842,-0.224120097,-0.212714725,-0.201309353,-0.189903981,-0.178498609,-0.167093237,-0.155687865,-0.144282493,-0.132877121,-0.121471749,-0.110066377,-0.098661005,-0.087255633,-0.075850261,-0.064444889,-0.053039517,-0.041634145,-0.030228773,-0.018823401,-0.007418029,
                   0.003987343,
                   0.015392715
                 ),
                 ncol = 1
               ))
  expect_equal(array$medianWabl(101L, 1)$getDimension(1L)$getSupremums(),
               matrix(
                 c(
                   1.924208095,
                   1.915338329,
                   1.906468563,
                   1.897598797,
                   1.888729032,
                   1.879859266,
                   1.870989500,
                   1.862119734,
                   1.853249968,
                   1.844380202,
                   1.835510437,
                   1.826640671,
                   1.817770905,
                   1.808901139,
                   1.800031373,
                   1.791161607,
                   1.782291841,
                   1.773422076,
                   1.764552310,
                   1.755682544,
                   1.746812778,
                   1.737943012,
                   1.729073246,
                   1.720203480,
                   1.711333715,
                   1.702463949,
                   1.692201825,
                   1.673694280,
                   1.655186734,
                   1.636679188,
                   1.618171642,
                   1.599664097,
                   1.581156551,
                   1.562649005,
                   1.544141460,
                   1.525633914,
                   1.507126368,
                   1.488618822,
                   1.470111277,
                   1.456025648,
                   1.442536725,
                   1.429047801,
                   1.415558878,
                   1.402069954,
                   1.388581031,
                   1.375092107,
                   1.361603184,
                   1.348114260,
                   1.334625337,
                   1.321136413,
                   1.307647490,
                   1.294158567,
                   1.280669643,
                   1.267180720,
                   1.253691796,
                   1.240202873,
                   1.226713949,
                   1.213225026,
                   1.199736102,
                   1.186247179,
                   1.172758256,
                   1.159269332,
                   1.145780409,
                   1.132291485,
                   1.118802562,
                   1.100960944,
                   1.061603712,
                   1.022246480,
                   0.982889247,
                   0.943532015,
                   0.904174783,
                   0.864817550,
                   0.840854721,
                   0.822347175,
                   0.803839629,
                   0.785332084,
                   0.766824538,
                   0.749320072,
                   0.742859160,
                   0.736398248,
                   0.729937336,
                   0.723476424,
                   0.717015512,
                   0.710554600,
                   0.704093688,
                   0.697632776,
                   0.691171864,
                   0.684710952,
                   0.678250040,
                   0.671789128,
                   0.665328216,
                   0.658867304,
                   0.652406392,
                   0.645945480,
                   0.639484568,
                   0.633023656,
                   0.626562744,
                   0.620101832,
                   0.613640920,
                   0.607180008,
                   0.602449641
                 ),
                 ncol = 1
               ))

  expect_equal(
    array$medianWabl(101L, 1, 1)$getDimension(1L)$getAlphaLevels(),
    matrix(c(seq(0, 1, len = 101)),
           ncol = 1)
  )
  expect_equal(array$medianWabl(101L, 1, 1)$getDimension(1L)$getInfimums(),
               matrix(
                 c(
                   -1.125144485,-1.113739113,-1.102333741,-1.090928369,-1.079522997,-1.068117625,-1.056712253,-1.045306881,-1.033901509,-1.022496137,-1.011090765,-0.999685393,-0.988280021,-0.976874649,-0.965469277,-0.954063905,-0.942658533,-0.931253161,-0.919847789,-0.908442417,-0.897037045,-0.885631673,-0.874226301,-0.862820929,-0.851415557,-0.840010185,-0.828604813,-0.817199441,-0.805794069,-0.794388697,-0.782983325,-0.771577953,-0.760172581,-0.748767209,-0.737361837,-0.725956465,-0.714551093,-0.703145721,-0.692288829,-0.685806647,-0.679324465,-0.672842284,-0.666360102,-0.65987792,-0.653395738,-0.646913557,-0.640431375,-0.633949193,-0.627467011,-0.620984829,-0.614502648,-0.608020466,-0.601538284,-0.595056102,-0.58857392,-0.582091738,-0.575609557,-0.569127375,-0.562645193,-0.556163011,-0.549680829,-0.543198648,-0.536716466,-0.530234284,-0.523752102,-0.51726992,-0.510787739,-0.504305557,-0.481282165,-0.457262233,-0.4332423,-0.409222368,-0.385202436,-0.361182503,-0.337162571,-0.313142639,-0.289122706,-0.265102774,-0.241082842,-0.224120097,-0.212714725,-0.201309353,-0.189903981,-0.178498609,-0.167093237,-0.155687865,-0.144282493,-0.132877121,-0.121471749,-0.110066377,-0.098661005,-0.087255633,-0.075850261,-0.064444889,-0.053039517,-0.041634145,-0.030228773,-0.018823401,-0.007418029,
                   0.003987343,
                   0.015392715
                 ),
                 ncol = 1
               ))
  expect_equal(array$medianWabl(101L, 1, 1)$getDimension(1L)$getSupremums(),
               matrix(
                 c(
                   1.924208095,
                   1.915338329,
                   1.906468563,
                   1.897598797,
                   1.888729032,
                   1.879859266,
                   1.870989500,
                   1.862119734,
                   1.853249968,
                   1.844380202,
                   1.835510437,
                   1.826640671,
                   1.817770905,
                   1.808901139,
                   1.800031373,
                   1.791161607,
                   1.782291841,
                   1.773422076,
                   1.764552310,
                   1.755682544,
                   1.746812778,
                   1.737943012,
                   1.729073246,
                   1.720203480,
                   1.711333715,
                   1.702463949,
                   1.692201825,
                   1.673694280,
                   1.655186734,
                   1.636679188,
                   1.618171642,
                   1.599664097,
                   1.581156551,
                   1.562649005,
                   1.544141460,
                   1.525633914,
                   1.507126368,
                   1.488618822,
                   1.470111277,
                   1.456025648,
                   1.442536725,
                   1.429047801,
                   1.415558878,
                   1.402069954,
                   1.388581031,
                   1.375092107,
                   1.361603184,
                   1.348114260,
                   1.334625337,
                   1.321136413,
                   1.307647490,
                   1.294158567,
                   1.280669643,
                   1.267180720,
                   1.253691796,
                   1.240202873,
                   1.226713949,
                   1.213225026,
                   1.199736102,
                   1.186247179,
                   1.172758256,
                   1.159269332,
                   1.145780409,
                   1.132291485,
                   1.118802562,
                   1.100960944,
                   1.061603712,
                   1.022246480,
                   0.982889247,
                   0.943532015,
                   0.904174783,
                   0.864817550,
                   0.840854721,
                   0.822347175,
                   0.803839629,
                   0.785332084,
                   0.766824538,
                   0.749320072,
                   0.742859160,
                   0.736398248,
                   0.729937336,
                   0.723476424,
                   0.717015512,
                   0.710554600,
                   0.704093688,
                   0.697632776,
                   0.691171864,
                   0.684710952,
                   0.678250040,
                   0.671789128,
                   0.665328216,
                   0.658867304,
                   0.652406392,
                   0.645945480,
                   0.639484568,
                   0.633023656,
                   0.626562744,
                   0.620101832,
                   0.613640920,
                   0.607180008,
                   0.602449641
                 ),
                 ncol = 1
               ))

  expect_equal(array$medianWabl(3L, 1)$getDimension(1L)$getAlphaLevels(),
               matrix(c(0,
                        0.5,
                        1),
                      ncol = 1))
  expect_equal(array$medianWabl(3L, 1)$getDimension(1L)$getInfimums(),
               matrix(c(
                 -1.125144485, -0.614502648,
                 0.015392715
               ),
               ncol = 1))
  expect_equal(array$medianWabl(3L, 1)$getDimension(1L)$getSupremums(),
               matrix(c(1.9242080995,
                        1.307647490,
                        0.602449641),
                      ncol = 1))

  expect_equal(array$medianWabl(3L, 1, 1)$getDimension(1L)$getAlphaLevels(),
               matrix(c(0,
                        0.5,
                        1),
                      ncol = 1))
  expect_equal(array$medianWabl(3L, 1, 1)$getDimension(1L)$getInfimums(),
               matrix(c(
                 -1.125144485, -0.614502648,
                 0.015392715
               ),
               ncol = 1))
  expect_equal(array$medianWabl(3L, 1, 1)$getDimension(1L)$getSupremums(),
               matrix(c(1.9242080995,
                        1.307647490,
                        0.602449641),
                      ncol = 1))

  expect_equal(
    array$medianWabl(3L, 2.5, 5 / 9)$getDimension(1L)$getAlphaLevels(),
    matrix(c(0,
             0.5,
             1),
           ncol = 1)
  )
  expect_equal(
    array$medianWabl(3L, 2.5, 5 / 9)$getDimension(1L)$getInfimums(),
    matrix(c(
      -1.571165934, -1.158450945, -0.535681819
    ),
    ncol = 1)
  )
  expect_equal(
    array$medianWabl(3L, 2.5, 5 / 9)$getDimension(1L)$getSupremums(),
    matrix(c(
      1.0523376907,
      0.6669579228, -0.0793067457
    ),
    ncol = 1)
  )

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)

  expect_equal(dataTra1$medianWabl()$getDimension(1L)$getAlphaLevels(),
               matrix(c(seq(0, 1, len = 101)),
                      ncol = 1))
  expect_equal(dataTra1$medianWabl()$getDimension(1L)$getInfimums(),
               matrix(
                 c(
                   -0.01758629332022421110,-0.0168771488903196,-0.0161680044604151,-0.0154588600305105,-0.0147497156006060,-0.0140405711707014,-0.0133314267407969,-0.0126222823108923,-0.0119131378809878,-0.0112039934510832,-0.0104948490211786,-0.0097857045912741,-0.0090765601613696,-0.0083674157314650,-0.0076582713015605,-0.0069491268716559,-0.0062399824417514,-0.0055308380118468,-0.0048216935819423,-0.0041125491520377,-0.0034034047221332,-0.0026942602922286,-0.0019851158623241,-0.0012759714324195,-0.0005668270025150,
                   0.0001423174273896,
                   0.0008514618572941,
                   0.0015606062871987,
                   0.0022697507171032,
                   0.0029788951470078,
                   0.0036880395769123,
                   0.0043971840068169,
                   0.0051063284367214,
                   0.0058154728666260,
                   0.0065246172965305,
                   0.0072337617264351,
                   0.0079429061563396,
                   0.0086520505862442,
                   0.0093611950161487,
                   0.0100703394460532,
                   0.0107794838759578,
                   0.0114886283058623,
                   0.0121977727357669,
                   0.0129069171656714,
                   0.0136160615955760,
                   0.0143252060254805,
                   0.0150343504553851,
                   0.0157434948852896,
                   0.0164526393151942,
                   0.0171617837450988,
                   0.0178709281750033,
                   0.0185800726049079,
                   0.0192892170348124,
                   0.0199983614647170,
                   0.0207075058946215,
                   0.0214166503245261,
                   0.0221257947544306,
                   0.0228349391843352,
                   0.0235440836142397,
                   0.0242532280441442,
                   0.0249623724740488,
                   0.0256715169039533,
                   0.0263806613338579,
                   0.0270898057637624,
                   0.0277989501936670,
                   0.0285080946235715,
                   0.0292172390534761,
                   0.0299263834833806,
                   0.0306355279132852,
                   0.0313446723431897,
                   0.0320538167730943,
                   0.0327629612029988,
                   0.0334721056329034,
                   0.0341812500628079,
                   0.0348903944927125,
                   0.0355995389226170,
                   0.0363086833525216,
                   0.0370178277824261,
                   0.0377269722123307,
                   0.0384361166422352,
                   0.0391452610721398,
                   0.0398544055020443,
                   0.0405635499319489,
                   0.0412726943618534,
                   0.0419818387917580,
                   0.0426909832216625,
                   0.0434001276515671,
                   0.0441092720814716,
                   0.0448184165113762,
                   0.0455275609412807,
                   0.0462367053711853,
                   0.0469458498010899,
                   0.0476549942309944,
                   0.0487909493157098,
                   0.0501330403997626,
                   0.0514751314838155,
                   0.0528172225678683,
                   0.0541593136519211,
                   0.0555014047359740,
                   0.0568434958200269,
                   0.0581855869040797
                 ),
                 ncol = 1
               ))
  expect_equal(dataTra1$medianWabl()$getDimension(1L)$getSupremums(),
               matrix(
                 c(
                   0.6310610593435880,
                   0.6299256499407240,
                   0.6287902405378600,
                   0.6276548311349960,
                   0.6265194217321320,
                   0.6253840123292680,
                   0.6242486029264040,
                   0.6231131935235400,
                   0.6219777841206760,
                   0.6208423747178120,
                   0.6197069653149480,
                   0.6185715559120830,
                   0.6174361465092190,
                   0.6163007371063560,
                   0.6151653277034910,
                   0.6140299183006270,
                   0.6128945088977630,
                   0.6117590994948990,
                   0.6106236900920350,
                   0.6094882806891710,
                   0.6083528712863070,
                   0.6072174618834430,
                   0.6060820524805790,
                   0.6049466430777150,
                   0.6038112336748510,
                   0.5989578493891440,
                   0.5931261772236360,
                   0.5872945050581280,
                   0.5814628328926210,
                   0.5756311607271130,
                   0.5697994885616050,
                   0.5649694302135990,
                   0.5640500230300840,
                   0.5631306158465690,
                   0.5622112086630550,
                   0.5612918014795400,
                   0.5603723942960260,
                   0.5594529871125110,
                   0.5585335799289960,
                   0.5576141727454820,
                   0.5566947655619670,
                   0.5557753583784520,
                   0.5548559511949380,
                   0.5539365440114230,
                   0.5530171368279080,
                   0.5520977296443940,
                   0.5511783224608790,
                   0.5502589152773650,
                   0.5493395080938500,
                   0.5484201009103350,
                   0.5475006937268210,
                   0.5465812865433060,
                   0.5456618793597910,
                   0.5447424721762770,
                   0.5438230649927620,
                   0.5429036578092470,
                   0.5419842506257330,
                   0.5410648434422180,
                   0.5401454362587040,
                   0.5392260290751890,
                   0.5383066218916740,
                   0.5373872147081600,
                   0.5364678075246450,
                   0.5355484003411300,
                   0.5346289931576160,
                   0.5337095859741010,
                   0.5327901787905860,
                   0.5318707716070720,
                   0.5309513644235570,
                   0.5300319572400430,
                   0.5291125500565280,
                   0.5281931428730130,
                   0.5272737356894990,
                   0.5263543285059840,
                   0.5254349213224690,
                   0.5245155141389550,
                   0.5235961069554400,
                   0.5226766997719250,
                   0.5217572925884110,
                   0.5208378854048960,
                   0.5199184782213810,
                   0.5189990710378670,
                   0.5180796638543520,
                   0.5171602566708380,
                   0.5162408494873230,
                   0.5153214423038080,
                   0.5144020351202940,
                   0.5134826279367790,
                   0.5125632207532640,
                   0.5116438135697500,
                   0.5107244063862350,
                   0.5098049992027200,
                   0.5088855920192060,
                   0.5079661848356910,
                   0.5070467776521760,
                   0.5061273704686620,
                   0.5052079632851470,
                   0.5042885561016330,
                   0.5033691489181180,
                   0.5024497417346030,
                   0.5015303345510890
                 ),
                 ncol = 1
               ))

})

test_that("TrapezoidalFuzzyNumberList transfTra function", {
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-3.3293414, -2.51760323,  1.32707594,  6.2201171)
    ))

  ## more parameters than needed
  expect_error(array$transfTra(1L, 1L))

  ## invalid parameter nl
  expect_error(array$transfTra(0))
  expect_error(array$transfTra(0))
  expect_error(array$transfTra("a"))
  expect_error(array$transfTra(0L))
  expect_error(array$transfTra(1L))

  ## valid parameter and valid TrapezoidalFuzzyNumbers
  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(
          -2.8725561532634476,-2.7270717405921969,
          3.0172070494187917,
          3.0234982317229750
        ),
        TrapezoidalFuzzyNumber$new(
          -5.6643506202171681,-1.4888469198997507,-1.4865352121417219,-1.3613130089675001
        ),
        TrapezoidalFuzzyNumber$new(
          -6.0131525080715074,-5.9150548017590321,
          4.7751478325491714,
          4.9006428907665756
        )
      )
    )

  expect_equal(array$transfTra(3L)$getDimension(1L)$getAlphaLevels(),
               matrix(c(0,
                        0.5,
                        1),
                      ncol = 1))
  expect_equal(array$transfTra(3L)$getDimension(1L)$getInfimums(),
               matrix(
                 c(
                   -2.8725561532634476,-2.7998139469278223,-2.7270717405921969
                 ),
                 ncol = 1
               ))
  expect_equal(array$transfTra(3L)$getDimension(1L)$getSupremums(),
               matrix(
                 c(3.0234982317229750,
                   3.0203526405708834,
                   3.0172070494187917),
                 ncol = 1
               ))

  expect_equal(array$transfTra(3L)$getDimension(2L)$getAlphaLevels(),
               matrix(c(0,
                        0.5,
                        1),
                      ncol = 1))
  expect_equal(array$transfTra(3L)$getDimension(2L)$getInfimums(),
               matrix(c(-5.66435062,-3.57659877,-1.48884692),
                      ncol = 1))
  expect_equal(array$transfTra(3L)$getDimension(2L)$getSupremums(),
               matrix(c(
                 -1.361313009,-1.423924111,-1.486535212
               ),
               ncol = 1))

  expect_equal(array$transfTra(3L)$getDimension(3L)$getAlphaLevels(),
               matrix(c(0,
                        0.5,
                        1),
                      ncol = 1))
  expect_equal(array$transfTra(3L)$getDimension(3L)$getInfimums(),
               matrix(c(
                 -6.013152508,-5.964103655,-5.915054802
               ),
               ncol = 1))
  expect_equal(array$transfTra(3L)$getDimension(3L)$getSupremums(),
               matrix(c(4.900642891,
                        4.837895362,
                        4.775147833),
                      ncol = 1))

})

test_that("TrapezoidalFuzzyNumberList wablphi function", {
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-3.3293414, -2.51760323,  1.32707594,  6.2201171)
    ))

  ## more paramters than needed
  expect_error(array$wablphi(0L, 1, 1))

  ## invalid parameters a and/or b
  expect_error(array$wablphi(0L, 1))
  expect_error(array$wablphi(1, "a"))
  expect_error(array$wablphi(-1.5, 1))
  expect_error(array$wablphi(1.9876, c(1, 2, 3, 4)))
  expect_error(array$wablphi(1.9876, list()))
  expect_error(array$wablphi(1.9876, Inf))
  expect_error(array$wablphi(1.9876,-Inf))

  ## valid parameters and valid TrapezoidalFuzzyNumbers
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
      TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774)
    ))

  expect_equal(array$wablphi(),
               c(0.017096149999999959, -1.808869625000000037))
  expect_equal(array$wablphi(1),
               c(0.017096149999999959, -1.808869625000000037))
  expect_equal(array$wablphi(1, 1),
               c(0.017096149999999959, -1.808869625000000037))
  expect_equal(array$wablphi(2, 1),
               c(0.042631749999999947, -1.543573099999999698))
  expect_equal(array$wablphi(2.2, 1.1),
               c(0.042631750520862655, -1.543573077185075082))

  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
      TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
    ))

  expect_equal(array$wablphi(),
               c(0.33594617500000001, -0.71968732500000010))
  expect_equal(array$wablphi(2, 1),
               c(0.34690664999999998,-0.65818685000000010))
  expect_equal(array$wablphi(2.2, 1.1),
               c(0.34690664734464199, -0.65818684207965783))

  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827)
      )
    )

  expect_equal(
    array$wablphi(),
    c(
      0.017096149999999959,-1.808869625000000037,
      0.335946175000000014
    )
  )
  expect_equal(
    array$wablphi(2, 1),
    c(
      0.042631749999999947,-1.543573099999999698,
      0.346906649999999983
    )
  )
  expect_equal(
    array$wablphi(2),
    c(
      0.042631749999999947,-1.543573099999999698,
      0.346906649999999983
    )
  )
  expect_equal(
    array$wablphi(2.2, 1.1),
    c(
      0.042631750520862655,-1.543573077185075082,
      0.346906647344641994
    )
  )

  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
        TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
      )
    )

  expect_equal(
    array$wablphi(),
    c(
      0.017096149999999959,-1.808869625000000037,
      0.335946175000000014,-0.719687325000000100
    )
  )
  expect_equal(
    array$wablphi(2, 1),
    c(
      0.042631749999999947,-1.543573099999999698,
      0.346906649999999983,-0.658186850000000101
    )
  )
  expect_equal(
    array$wablphi(2.2, 1.1),
    c(
      0.042631750520862655,-1.543573077185075082,
      0.346906647344641994,-0.658186842079657830
    )
  )

})

test_that("TrapezoidalFuzzyNumberList var function", {
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-3.3293414, -2.51760323,  1.32707594,  6.2201171)
    ))

  ## more paramters than needed
  expect_error(array$var(0L, 1, 1))

  ## invalid parameters a, b and/or theta
  expect_error(array$var(0L, 1))
  expect_error(array$var(-1.5, 1))
  expect_error(array$var(1, "a"))
  expect_error(array$var(1.9876, c(1, 2, 3, 4)))
  expect_error(array$var(1.9876, list()))
  expect_error(array$var(1.9876, 2, 1L))
  expect_error(array$var(1.9876, 2, 0))
  expect_error(array$var(1.9876, 2, "blabla"))
  expect_error(array$var(1.9876, Inf,-Inf))
  expect_error(array$var(1.9876, Inf, Inf))

  ## valid parameters and valid TrapezoidalFuzzyNumbers
  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
        TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
      )
    )

  expect_equal(array$var(),
               c(2.9495212262687587))
  expect_equal(array$var(1, 1),
               c(2.9495212262687587))
  expect_equal(array$var(1, 1, 1),
               c(2.9495212262687587))
  expect_equal(array$var(2),
               c(2.8130968426319773))
  expect_equal(array$var(2, 1),
               c(2.8130968426319773))
  expect_equal(array$var(2, 1, 1),
               c(2.8130968426319773))
  expect_equal(array$var(1 / 3, 1 / 8, 1),
               c(2.8262341219892413))

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)
  expect_equal(dataTra1$var(),
               c(0.80422410712410453))

})

test_that("TrapezoidalFuzzyNumberList tn function", {
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-3.3293414, -2.51760323,  1.32707594,  6.2201171)
    ))

  ## not all mandatory parameters
  expect_error(array$tn())

  ## more parameters than needed
  expect_error(array$tn(0, 0, 0, 0, 0))

  ## INVALID PARAMETERS
  ## invalid type
  expect_error(array$tn(0L))
  expect_error(array$tn(0))
  expect_error(array$tn(4L))
  expect_error(array$tn(0.99999))
  expect_error(array$tn(3.000001))
  expect_error(array$tn("a"))
  expect_error(array$tn(list()))

  ## invalid a, b and/or theta
  expect_error(array$tn(1L, 0L, 1))
  expect_error(array$tn(1L,-1.5, 1))
  expect_error(array$tn(1L, 1, "a"))
  expect_error(array$tn(2L, 1.9876, c(1, 2, 3, 4)))
  expect_error(array$tn(2L, 1.9876, list()))
  expect_error(array$tn(2L, 1.9876, 2, 1L))
  expect_error(array$tn(3L, 1.9876, 2, 0))
  expect_error(array$tn(3L, 1.9876, 2, "blabla"))
  expect_error(array$tn(3L, 1.9876, Inf,-Inf))
  expect_error(array$tn(3L, 1.9876, Inf, Inf))

  ## valid parameters and valid TrapezoidalFuzzyNumbers
  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
        TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
      )
    )

  expect_equal(array$tn(1L),
               c(2.0385324583333331))
  expect_equal(array$tn(1L, 1),
               c(2.0385324583333331))
  expect_equal(array$tn(1L, 1, 1, 1),
               c(2.0385324583333331))
  expect_equal(array$tn(2L),
               c(2.3060148840748242))
  expect_equal(array$tn(2L, 1, 1),
               c(2.3060148840748242))
  expect_equal(array$tn(3L, 5, 1, 0.5),
               c(1.8751136258744443))

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(100L)

  expect_equal(dataTra1$tn(1L), 1.2505052660910376)
  expect_equal(dataTra1$tn(2L), 1.4502655551393968)

})

test_that("TrapezoidalFuzzyNumberList sn function", {
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-3.3293414, -2.51760323,  1.32707594,  6.2201171)
    ))

  ## not all mandatory parameters
  expect_error(array$sn())

  ## more parameters than needed
  expect_error(array$sn(0, 0, 0, 0, 0))

  ## INVALID PARAMETERS
  ## invalid type
  expect_error(array$sn(0L))
  expect_error(array$sn(0))
  expect_error(array$sn(4L))
  expect_error(array$sn(0.99999))
  expect_error(array$sn(3.000001))
  expect_error(array$sn("a"))
  expect_error(array$sn(list()))

  ## invalid a, b and/or theta
  expect_error(array$sn(1L, 0L, 1))
  expect_error(array$sn(1L,-1.5, 1))
  expect_error(array$sn(1L, 1, "a"))
  expect_error(array$sn(2L, 1.9876, c(1, 2, 3, 4)))
  expect_error(array$sn(2L, 1.9876, list()))
  expect_error(array$sn(2L, 1.9876, 2, 1L))
  expect_error(array$sn(3L, 1.9876, 2, 0))
  expect_error(array$sn(3L, 1.9876, 2, "blabla"))
  expect_error(array$sn(3L, 1.9876, Inf,-Inf))
  expect_error(array$sn(3L, 1.9876, Inf, Inf))

  ## valid parameters and valid TrapezoidalFuzzyNumbers
  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
        TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
      )
    )

  expect_equal(array$sn(1L),
               c(2.1448157999999999))
  expect_equal(array$sn(1L, 1),
               c(2.1448157999999999))
  expect_equal(array$sn(1L, 1, 1, 1),
               c(2.1448157999999999))
  expect_equal(array$sn(2L),
               c(2.5109358211914543))
  expect_equal(array$sn(2L, 1, 1),
               c(2.5109358211914543))
  expect_equal(array$sn(3L, 5, 1, 0.5),
               c(2.0201919583333332))

  #dato de ejemplo real para aplicar funciones de FuzzyStatTra
  data <- Utils$new()$convertTra(M1)

  expect_equal(data$sn(1L), 2.4005208333333337)
  expect_equal(data$sn(2L), 2.7778138886541699)

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)

  expect_equal(dataTra1$sn(1L), 0.79658832370242361)
  expect_equal(dataTra1$sn(2L), 0.79715558203480918)

})

test_that("TrapezoidalFuzzyNumberList qn function", {
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-3.3293414, -2.51760323,  1.32707594,  6.2201171)
    ))

  ## not all mandatory parameters
  expect_error(array$tn())

  ## more parameters than needed
  expect_error(array$tn(0, 0, 0, 0, 0))

  ## INVALID PARAMETERS
  ## invalid type
  expect_error(array$qn(0L))
  expect_error(array$qn(0))
  expect_error(array$qn(4L))
  expect_error(array$qn(0.99999))
  expect_error(array$qn(3.000001))
  expect_error(array$qn("a"))
  expect_error(array$qn(list()))

  ## invalid a, b and/or theta
  expect_error(array$qn(1L, 0L, 1))
  expect_error(array$qn(1L,-1.5, 1))
  expect_error(array$qn(1L, 1, "a"))
  expect_error(array$qn(2L, 1.9876, c(1, 2, 3, 4)))
  expect_error(array$qn(2L, 1.9876, list()))
  expect_error(array$qn(2L, 1.9876, 2, 1L))
  expect_error(array$qn(3L, 1.9876, 2, 0))
  expect_error(array$qn(3L, 1.9876, 2, "blabla"))
  expect_error(array$qn(3L, 1.9876, Inf,-Inf))
  expect_error(array$qn(3L, 1.9876, Inf, Inf))

  ## valid parameters and valid TrapezoidalFuzzyNumbers
  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
        TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
      )
    )

  expect_equal(array$qn(1L),
               c(2.1448157999999999))
  expect_equal(array$qn(1L, 1, 1),
               c(2.1448157999999999))
  expect_equal(array$qn(1L, 1, 1, 1),
               c(2.1448157999999999))
  expect_equal(array$qn(2L),
               c(2.5109358211914543))
  expect_equal(array$qn(2L, 1),
               c(2.5109358211914543))
  expect_equal(array$qn(3L, 5, 1, 0.5),
               c(2.0201919583333332))

  #dato de ejemplo real para aplicar funciones de FuzzyStatTra
  data <- Utils$new()$convertTra(M1)

  expect_equal(data$qn(1L), 1.425)
  expect_equal(data$qn(2L), 1.5960889699512375)

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(100L)

  expect_equal(dataTra1$qn(1L), 1.0582920955747563)
  expect_equal(dataTra1$qn(2L), 1.2290261273287957)

})

test_that("TrapezoidalFuzzyNumberList median1Norm function", {
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-3.3293414, -2.51760323,  1.32707594,  6.2201171)
    ))

  ## more parameters than needed
  expect_error(array$median1Norm(0, 0))

  ## invalid parameter nl
  expect_error(array$median1Norm(0))
  expect_error(array$median1Norm("a"))
  expect_error(array$median1Norm(0L))
  expect_error(array$median1Norm(1L))
  expect_error(array$median1Norm(c(1, 2, 3, 4)))
  expect_error(array$median1Norm(list()))

  ## valid parameters and valid TrapezoidalFuzzyNumbers
  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641,-1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141,-1.605457,-0.4205031,-0.3803774),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
        TrapezoidalFuzzyNumber$new(-5.4843364,-4.5494522, 3.4790804, 3.6759589)
      )
    )

  expect_equal(array$median1Norm()$getDimension(1L)$getAlphaLevels(),
               matrix(c(seq(0, 1, len = 101)), ncol = 1))

  expect_equal(array$median1Norm()$getDimension(1L)$getInfimums(),
               matrix(
                 c(
                   -3.4223910000000002,-3.401664445,-3.38093789,-3.360211335,-3.33948478,-3.318758225,-3.29803167,-3.277305115,-3.25657856,-3.235852005,-3.21512545,-3.194398895,-3.17367234,-3.152945785,-3.13221923,-3.111492675,-3.09076612,-3.070039565,-3.04931301,-3.028586455,-3.0078599,-2.987133345,-2.96640679,-2.945680235,-2.92495368,-2.904227125,-2.88350057,-2.862774015,-2.84204746,-2.821320905,-2.80059435,-2.779867795,-2.75914124,-2.738414685,-2.71768813,-2.696961575,-2.67623502,-2.655508465,-2.63478191,-2.614055355,-2.5933288,-2.572602245,-2.55187569,-2.531149135,-2.51042258,-2.489696025,-2.46896947,-2.448242915,-2.42751636,-2.406789805,-2.38606325,-2.365336695,-2.34461014,-2.323883585,-2.30315703,-2.282430475,-2.26170392,-2.240977365,-2.22025081,-2.199524255,-2.1787977,-2.158071145,-2.13734459,-2.116618035,-2.09589148,-2.075164925,-2.05443837,-2.033711815,-2.01298526,-1.992258705,-1.97153215,-1.950805595,-1.93007904,-1.909352485,-1.88862593,-1.867899375,-1.84717282,-1.826446265,-1.80571971,-1.784993155,-1.7642666,-1.743540045,-1.72281349,-1.702086935,-1.68136038,-1.660633825,-1.63990727,-1.619180715,-1.59845416,-1.577727605,-1.55700105,-1.536274495,-1.51554794,-1.494821385,-1.47409483,-1.453368275,-1.43264172,-1.411915165,-1.39118861,-1.370462055,-1.3497355
                 ),
                 ncol = 1
               ))
  expect_equal(array$median1Norm()$getDimension(1L)$getSupremums(),
               matrix(
                 c(
                   1.3169512,
                   1.312176895,
                   1.30740259,
                   1.302628285,
                   1.29785398,
                   1.293079675,
                   1.28830537,
                   1.283531065,
                   1.27875676,
                   1.273982455,
                   1.26920815,
                   1.264433845,
                   1.25965954,
                   1.254885235,
                   1.25011093,
                   1.245336625,
                   1.24056232,
                   1.235788015,
                   1.23101371,
                   1.226239405,
                   1.2214651,
                   1.216690795,
                   1.21191649,
                   1.207142185,
                   1.20236788,
                   1.197593575,
                   1.19281927,
                   1.188044965,
                   1.18327066,
                   1.178496355,
                   1.17372205,
                   1.168947745,
                   1.16417344,
                   1.159399135,
                   1.15462483,
                   1.149850525,
                   1.14507622,
                   1.140301915,
                   1.13552761,
                   1.130753305,
                   1.125979,
                   1.121204695,
                   1.11643039,
                   1.111656085,
                   1.10688178,
                   1.102107475,
                   1.09733317,
                   1.092558865,
                   1.08778456,
                   1.083010255,
                   1.07823595,
                   1.073461645,
                   1.06868734,
                   1.063913035,
                   1.05913873,
                   1.054364425,
                   1.04959012,
                   1.044815815,
                   1.04004151,
                   1.035267205,
                   1.0304929,
                   1.025718595,
                   1.02094429,
                   1.016169985,
                   1.01139568,
                   1.006621375,
                   1.00184707,
                   0.997072765,
                   0.99229846,
                   0.987524155,
                   0.98274985,
                   0.977975545,
                   0.97320124,
                   0.968426935,
                   0.96365263,
                   0.958878325,
                   0.95410402,
                   0.949329715,
                   0.94455541,
                   0.939781105,
                   0.9350068,
                   0.930232495,
                   0.92545819,
                   0.920683885,
                   0.91590958,
                   0.911135275,
                   0.90636097,
                   0.901586665,
                   0.89681236,
                   0.892038055,
                   0.88726375,
                   0.882489445,
                   0.87771514,
                   0.872940835,
                   0.86816653,
                   0.863392225,
                   0.85861792,
                   0.853843615,
                   0.84906931,
                   0.844295005,
                   0.8395207
                 ),
                 ncol = 1
               ))

  expect_equal(array$median1Norm(101L)$getDimension(1L)$getAlphaLevels(),
               matrix(c(seq(0, 1, len = 101)), ncol = 1))

  expect_equal(array$median1Norm(101L)$getDimension(1L)$getInfimums(),
               matrix(
                 c(
                   -3.4223910000000002,-3.401664445,-3.38093789,-3.360211335,-3.33948478,-3.318758225,-3.29803167,-3.277305115,-3.25657856,-3.235852005,-3.21512545,-3.194398895,-3.17367234,-3.152945785,-3.13221923,-3.111492675,-3.09076612,-3.070039565,-3.04931301,-3.028586455,-3.0078599,-2.987133345,-2.96640679,-2.945680235,-2.92495368,-2.904227125,-2.88350057,-2.862774015,-2.84204746,-2.821320905,-2.80059435,-2.779867795,-2.75914124,-2.738414685,-2.71768813,-2.696961575,-2.67623502,-2.655508465,-2.63478191,-2.614055355,-2.5933288,-2.572602245,-2.55187569,-2.531149135,-2.51042258,-2.489696025,-2.46896947,-2.448242915,-2.42751636,-2.406789805,-2.38606325,-2.365336695,-2.34461014,-2.323883585,-2.30315703,-2.282430475,-2.26170392,-2.240977365,-2.22025081,-2.199524255,-2.1787977,-2.158071145,-2.13734459,-2.116618035,-2.09589148,-2.075164925,-2.05443837,-2.033711815,-2.01298526,-1.992258705,-1.97153215,-1.950805595,-1.93007904,-1.909352485,-1.88862593,-1.867899375,-1.84717282,-1.826446265,-1.80571971,-1.784993155,-1.7642666,-1.743540045,-1.72281349,-1.702086935,-1.68136038,-1.660633825,-1.63990727,-1.619180715,-1.59845416,-1.577727605,-1.55700105,-1.536274495,-1.51554794,-1.494821385,-1.47409483,-1.453368275,-1.43264172,-1.411915165,-1.39118861,-1.370462055,-1.3497355
                 ),
                 ncol = 1
               ))
  expect_equal(array$median1Norm(101L)$getDimension(1L)$getSupremums(),
               matrix(
                 c(
                   1.3169512,
                   1.312176895,
                   1.30740259,
                   1.302628285,
                   1.29785398,
                   1.293079675,
                   1.28830537,
                   1.283531065,
                   1.27875676,
                   1.273982455,
                   1.26920815,
                   1.264433845,
                   1.25965954,
                   1.254885235,
                   1.25011093,
                   1.245336625,
                   1.24056232,
                   1.235788015,
                   1.23101371,
                   1.226239405,
                   1.2214651,
                   1.216690795,
                   1.21191649,
                   1.207142185,
                   1.20236788,
                   1.197593575,
                   1.19281927,
                   1.188044965,
                   1.18327066,
                   1.178496355,
                   1.17372205,
                   1.168947745,
                   1.16417344,
                   1.159399135,
                   1.15462483,
                   1.149850525,
                   1.14507622,
                   1.140301915,
                   1.13552761,
                   1.130753305,
                   1.125979,
                   1.121204695,
                   1.11643039,
                   1.111656085,
                   1.10688178,
                   1.102107475,
                   1.09733317,
                   1.092558865,
                   1.08778456,
                   1.083010255,
                   1.07823595,
                   1.073461645,
                   1.06868734,
                   1.063913035,
                   1.05913873,
                   1.054364425,
                   1.04959012,
                   1.044815815,
                   1.04004151,
                   1.035267205,
                   1.0304929,
                   1.025718595,
                   1.02094429,
                   1.016169985,
                   1.01139568,
                   1.006621375,
                   1.00184707,
                   0.997072765,
                   0.99229846,
                   0.987524155,
                   0.98274985,
                   0.977975545,
                   0.97320124,
                   0.968426935,
                   0.96365263,
                   0.958878325,
                   0.95410402,
                   0.949329715,
                   0.94455541,
                   0.939781105,
                   0.9350068,
                   0.930232495,
                   0.92545819,
                   0.920683885,
                   0.91590958,
                   0.911135275,
                   0.90636097,
                   0.901586665,
                   0.89681236,
                   0.892038055,
                   0.88726375,
                   0.882489445,
                   0.87771514,
                   0.872940835,
                   0.86816653,
                   0.863392225,
                   0.85861792,
                   0.853843615,
                   0.84906931,
                   0.844295005,
                   0.8395207
                 ),
                 ncol = 1
               ))

  expect_equal(array$median1Norm(5L)$getDimension(1L)$getAlphaLevels(),
               matrix(c(0,
                        0.25,
                        0.5,
                        0.75,
                        1),
                      ncol = 1))

  expect_equal(array$median1Norm(5L)$getDimension(1L)$getInfimums(),
               matrix(
                 c(
                   -3.4223910000000002,-2.9042271250000002,-2.3860632499999999,-1.8678993749999999,-1.3497355000000000
                 ),
                 ncol = 1
               ))

  expect_equal(array$median1Norm(5L)$getDimension(1L)$getSupremums(),
               matrix(
                 c(
                   1.31695120000000010,
                   1.19759357499999997,
                   1.07823595000000005,
                   0.95887832499999992,
                   0.83952070000000001
                 ),
                 ncol = 1
               ))

  expect_equal(array$median1Norm(2L)$getDimension(1L)$getAlphaLevels(),
               matrix(c(0,
                        1),
                      ncol = 1))
  expect_equal(array$median1Norm(2L)$getDimension(1L)$getInfimums(),
               matrix(c(
                 -3.4223910000000002,-1.3497355000000000
               ),
               ncol = 1))
  expect_equal(array$median1Norm(2L)$getDimension(1L)$getSupremums(),
               matrix(c(
                 1.31695120000000010,
                 0.83952070000000001
               ),
               ncol = 1))

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)

  expect_equal(dataTra1$median1Norm()$getDimension(1L)$getAlphaLevels(),
               matrix(c(seq(0, 1, len = 101)),
                      ncol = 1))
  expect_equal(dataTra1$median1Norm()$getDimension(1L)$getInfimums(),
               matrix(
                 c(
                   -0.0175862933202241,-0.0168771488903196,-0.0161680044604150,-0.0154588600305105,-0.0147497156006059,-0.0140405711707014,-0.0133314267407968,-0.0126222823108923,-0.0119131378809877,-0.0112039934510832,-0.0104948490211786,-0.0097857045912741,-0.0090765601613696,-0.0083674157314650,-0.0076582713015605,-0.0069491268716559,-0.0062399824417514,-0.0055308380118468,-0.0048216935819423,-0.0041125491520377,-0.0034034047221332,-0.0026942602922286,-0.0019851158623241,-0.0012759714324195,-0.0005668270025150,
                   0.0001423174273896,
                   0.0008514618572941,
                   0.0015606062871987,
                   0.0022697507171032,
                   0.0029788951470078,
                   0.0036880395769123,
                   0.0043971840068169,
                   0.0051063284367214,
                   0.0058154728666260,
                   0.0065246172965305,
                   0.0072337617264351,
                   0.0079429061563396,
                   0.0086520505862442,
                   0.0093611950161487,
                   0.0100703394460532,
                   0.0107794838759578,
                   0.0114886283058623,
                   0.0121977727357669,
                   0.0129069171656714,
                   0.0136160615955760,
                   0.0143252060254805,
                   0.0150343504553851,
                   0.0157434948852896,
                   0.0164526393151942,
                   0.0171617837450987,
                   0.0178709281750033,
                   0.0185800726049078,
                   0.0192892170348124,
                   0.0199983614647169,
                   0.0207075058946215,
                   0.0214166503245260,
                   0.0221257947544306,
                   0.0228349391843351,
                   0.0235440836142397,
                   0.0242532280441442,
                   0.0249623724740488,
                   0.0256715169039533,
                   0.0263806613338579,
                   0.0270898057637624,
                   0.0277989501936670,
                   0.0285080946235715,
                   0.0292172390534761,
                   0.0299263834833806,
                   0.0306355279132852,
                   0.0313446723431897,
                   0.0320538167730943,
                   0.0327629612029988,
                   0.0334721056329034,
                   0.0341812500628079,
                   0.0348903944927125,
                   0.0355995389226170,
                   0.0363086833525216,
                   0.0370178277824261,
                   0.0377269722123307,
                   0.0384361166422352,
                   0.0391452610721398,
                   0.0398544055020443,
                   0.0405635499319489,
                   0.0412726943618534,
                   0.0419818387917580,
                   0.0426909832216625,
                   0.0434001276515671,
                   0.0441092720814716,
                   0.0448184165113762,
                   0.0455275609412807,
                   0.0462367053711853,
                   0.0469458498010898,
                   0.0476549942309944,
                   0.0483641386608989,
                   0.0490732830908035,
                   0.0497824275207080,
                   0.0504915719506126,
                   0.0512007163805171,
                   0.0519098608104217,
                   0.0526190052403262,
                   0.0533281496702308
                 ),
                 ncol = 1
               ))
  expect_equal(dataTra1$median1Norm()$getDimension(1L)$getSupremums(),
               matrix(
                 c(
                   0.5934710529025530,
                   0.5925516457190380,
                   0.5916322385355230,
                   0.5907128313520090,
                   0.5897934241684940,
                   0.5888740169849790,
                   0.5879546098014650,
                   0.5870352026179500,
                   0.5861157954344360,
                   0.5851963882509210,
                   0.5842769810674060,
                   0.5833575738838920,
                   0.5824381667003770,
                   0.5815187595168620,
                   0.5805993523333480,
                   0.5796799451498330,
                   0.5787605379663180,
                   0.5778411307828040,
                   0.5769217235992890,
                   0.5760023164157750,
                   0.5750829092322600,
                   0.5741635020487450,
                   0.5732440948652310,
                   0.5723246876817160,
                   0.5714052804982010,
                   0.5704858733146870,
                   0.5695664661311720,
                   0.5686470589476570,
                   0.5677276517641430,
                   0.5668082445806280,
                   0.5658888373971130,
                   0.5649694302135990,
                   0.5640500230300840,
                   0.5631306158465690,
                   0.5622112086630550,
                   0.5612918014795400,
                   0.5603723942960260,
                   0.5594529871125110,
                   0.5585335799289960,
                   0.5576141727454820,
                   0.5566947655619670,
                   0.5557753583784520,
                   0.5548559511949380,
                   0.5539365440114230,
                   0.5530171368279080,
                   0.5520977296443940,
                   0.5511783224608790,
                   0.5502589152773650,
                   0.5493395080938500,
                   0.5484201009103350,
                   0.5475006937268210,
                   0.5465812865433060,
                   0.5456618793597910,
                   0.5447424721762770,
                   0.5438230649927620,
                   0.5429036578092470,
                   0.5419842506257330,
                   0.5410648434422180,
                   0.5401454362587040,
                   0.5392260290751890,
                   0.5383066218916740,
                   0.5373872147081600,
                   0.5364678075246450,
                   0.5355484003411300,
                   0.5346289931576160,
                   0.5337095859741010,
                   0.5327901787905860,
                   0.5318707716070720,
                   0.5309513644235570,
                   0.5300319572400430,
                   0.5291125500565280,
                   0.5281931428730130,
                   0.5272737356894990,
                   0.5263543285059840,
                   0.5254349213224690,
                   0.5245155141389550,
                   0.5235961069554400,
                   0.5226766997719250,
                   0.5217572925884110,
                   0.5208378854048960,
                   0.5199184782213810,
                   0.5189990710378670,
                   0.5180796638543520,
                   0.5171602566708380,
                   0.5162408494873230,
                   0.5153214423038080,
                   0.5144020351202940,
                   0.5134826279367790,
                   0.5125632207532640,
                   0.5116438135697500,
                   0.5107244063862350,
                   0.5098049992027200,
                   0.5088855920192060,
                   0.5079661848356910,
                   0.5070467776521760,
                   0.5061273704686620,
                   0.5052079632851470,
                   0.5042885561016330,
                   0.5033691489181180,
                   0.5024497417346030,
                   0.5015303345510890
                 ),
                 ncol = 1
               ))

})

test_that("FuzzyNumberList add function", {
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-2.015641, -1.094014, 1.2814199, 1.8966197),
      TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, 0.4205031, 0.5803774)
    ))

  ## not all mandatory parameters
  expect_error(array$add())
  expect_error(array$add("a", NULL))
  expect_error(array$add(NULL, "a"))

  ## more parameters than needed
  expect_error(array$add("a", 1, 1, 1, 1, 1))

  ## invalid parameter s
  expect_error(array$add("a", 1, 1, 1, 1))
  expect_error(array$add(4L, 1, 1, 1, 1))
  expect_error(array$add(4, 1, 1, 1, 1))
  expect_error(array$add(4.5, 1, 1, 1, 1))
  expect_error(array$add(c(1, 2, 3, 4), 1, 1, 1, 1))

  s <-
    FuzzyNumberList$new(c(FuzzyNumber$new(array(
      c(0, 1, 0.6623698, 0.9403180, 1.564024, 1.185641), dim = c(2, 3)
    ))))

  ## invalid parameter type
  expect_error(array$add(s, "a", 1, 1, 1))
  expect_error(array$add(s, 4L, 1, 1, 1))
  expect_error(array$add(s, 4, 1, 1, 1))
  expect_error(array$add(s, 4.5, 1, 1, 1))
  expect_error(array$add(s, c(1, 2, 3, 4), 1, 1, 1))
  expect_error(array$add(s, 4L, Inf, -Inf, 1))

  ## invalid parameter a, b or theta
  expect_error(array$add(s, 1L, 0, 0, 0))
  expect_error(array$add(s, 2L, "a", 0L,-1.5))
  expect_error(array$add(s, 3L, 1, list(), 1))
  expect_error(array$add(s, 3L, 1, 2, c()))
  expect_error(array$add(s, 3L, Inf,-Inf,-Inf))
  expect_error(array$add(s, 3L, Inf, Inf, Inf))

  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
        TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
      )
    )

  array3 <-
    FuzzyNumberList$new(c(FuzzyNumber$new(array(
      c(0, 1, 0.2, 0.5, 0.7, 0.6), dim = c(2, 3)
    )),
    FuzzyNumber$new(array(
      c(0, 1, 0.3, 0.6, 0.6, 0.6), dim = c(2, 3)
    ))))

  expect_equal(array$add(array3, 1L), NA)

  ## all conditions fulfilled
  array2 <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(
        -0.30614214796881267,
        -0.17429571018685452,
        0.41001586290808789,
        1.4885363965873308
      )
    ))

  expect_equal(array$add(array2, 2L), 1.9296252454859624)
  expect_equal(array$add(array2, 2L, 1, 1), 1.9296252454859624)
  expect_equal(array$add(array2, 2L, 1, 1, 1), 1.9296252454859624)

  array3 <-
    FuzzyNumberList$new(c(FuzzyNumber$new(array(
      c(
        0,
        0.25,
        0.5,
        0.75,
        1,-5.7835504819939967,-5.100726176047438,-4.4179018701008808,-3.7350775641543232,-3.0522532582077657,
        2.57316107511803693,
        2.05738398915402954,
        1.54160690319002169,
        1.02582981722601407,
        0.51005273126200645
      ),
      dim = c(5, 3)
    ))))

  expect_equal(array$add(array3, 2L), 2.1731857612915326)

  array4 <-
    FuzzyNumberList$new(c(FuzzyNumber$new(array(
      c(
        seq(0, 1, len = 101),
        -1.125144453,
        -1.113739082,
        -1.10233371,
        -1.090928338,
        -1.079522967,
        -1.068117595,
        -1.056712223,
        -1.045306852,
        -1.03390148,
        -1.022496109,
        -1.011090737,
        -0.999685365,
        -0.988279994,
        -0.976874622,
        -0.96546925,
        -0.954063879,
        -0.942658507,
        -0.931253136,
        -0.919847764,
        -0.908442392,
        -0.897037021,
        -0.885631649,
        -0.874226277,
        -0.862820906,
        -0.851415534,
        -0.840010162,
        -0.828604791,
        -0.817199419,
        -0.805794048,
        -0.794388676,
        -0.782983304,
        -0.771577933,
        -0.760172561,
        -0.748767189,
        -0.737361818,
        -0.725956446,
        -0.714551075,
        -0.703145703,
        -0.692288814,
        -0.685806632,
        -0.679324451,
        -0.672842269,
        -0.666360087,
        -0.659877906,
        -0.653395724,
        -0.646913543,
        -0.640431361,
        -0.633949179,
        -0.627466998,
        -0.620984816,
        -0.614502635,
        -0.608020453,
        -0.601538271,
        -0.59505609,
        -0.588573908,
        -0.582091726,
        -0.575609545,
        -0.569127363,
        -0.562645182,
        -0.556163,
        -0.549680818,
        -0.543198637,
        -0.536716455,
        -0.530234274,
        -0.523752092,
        -0.51726991,
        -0.510787729,
        -0.504305547,
        -0.48128216,
        -0.457262228,
        -0.433242296,
        -0.409222363,
        -0.385202431,
        -0.361182499,
        -0.337162566,
        -0.313142634,
        -0.289122702,
        -0.26510277,
        -0.241082837,
        -0.224120094,
        -0.212714723,
        -0.201309351,
        -0.189903979,
        -0.178498608,
        -0.167093236,
        -0.155687865,
        -0.144282493,
        -0.132877121,
        -0.12147175,
        -0.110066378,
        -0.098661006,
        -0.087255635,
        -0.075850263,
        -0.064444892,
        -0.05303952,
        -0.041634148,
        -0.030228777,
        -0.018823405,
        -0.007418033,
        0.003987338,
        0.01539271,
        1.924208103,
        1.915338338,
        1.906468572,
        1.897598806,
        1.88872904,
        1.879859274,
        1.870989508,
        1.862119742,
        1.853249976,
        1.84438021,
        1.835510444,
        1.826640678,
        1.817770912,
        1.808901146,
        1.80003138,
        1.791161614,
        1.782291848,
        1.773422083,
        1.764552317,
        1.755682551,
        1.746812785,
        1.737943019,
        1.729073253,
        1.720203487,
        1.711333721,
        1.702463955,
        1.692201814,
        1.673694268,
        1.655186722,
        1.636679177,
        1.618171631,
        1.599664086,
        1.58115654,
        1.562648994,
        1.544141449,
        1.525633903,
        1.507126357,
        1.488618812,
        1.470111266,
        1.456025635,
        1.442536712,
        1.429047789,
        1.415558866,
        1.402069943,
        1.38858102,
        1.375092097,
        1.361603174,
        1.348114251,
        1.334625328,
        1.321136404,
        1.307647481,
        1.294158558,
        1.280669635,
        1.267180712,
        1.253691789,
        1.240202866,
        1.226713943,
        1.21322502,
        1.199736097,
        1.186247174,
        1.172758251,
        1.159269327,
        1.145780404,
        1.132291481,
        1.118802558,
        1.100960944,
        1.061603712,
        1.02224648,
        0.982889248,
        0.943532016,
        0.904174784,
        0.864817552,
        0.840854715,
        0.822347169,
        0.803839623,
        0.785332078,
        0.766824532,
        0.749320068,
        0.742859156,
        0.736398244,
        0.729937331,
        0.723476419,
        0.717015507,
        0.710554595,
        0.704093683,
        0.697632771,
        0.691171859,
        0.684710947,
        0.678250034,
        0.671789122,
        0.66532821,
        0.658867298,
        0.652406386,
        0.645945474,
        0.639484562,
        0.633023649,
        0.626562737,
        0.620101825,
        0.613640913,
        0.607180001,
        0.602449632
      ),
      dim = c(101, 3)
    ))))

  expect_equal(array$add(array4, 1L), 1.7159723384199999)
  expect_equal(array$add(array4, 3L), 2.2059097333890874)
  expect_equal(array$add(array4, 3L, 5, 5, 9 / 8), 2.3353544967579181)

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)
  data1 <- dataTra1$mean()
  expect_equal(dataTra1$add(data1, 2L), 0.78862504661728794)

  data1 <- dataTra1$medianWabl()
  expect_equal(dataTra1$add(data1, 2L), 0.72824325422717551)

  data1 <- dataTra1$median1Norm()
  expect_equal(dataTra1$add(data1, 2L), 0.72415686528281631)

})

test_that("FuzzyNumberList mdd function", {
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(-2.015641, -1.094014, 1.2814199, 1.8966197),
      TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, 0.4205031, 0.5803774)
    ))

  ## not all mandatory parameters
  expect_error(array$mdd())
  expect_error(array$mdd("a", NULL))
  expect_error(array$mdd(NULL, "a"))

  ## more parameters than needed
  expect_error(array$mdd("a", 1, 1, 1, 1, 1))

  ## invalid parameter s
  expect_error(array$mdd("a", 1, 1, 1, 1))
  expect_error(array$mdd(4L, 1, 1, 1, 1))
  expect_error(array$mdd(4, 1, 1, 1, 1))
  expect_error(array$mdd(4.5, 1, 1, 1, 1))
  expect_error(array$mdd(c(1, 2, 3, 4), 1, 1, 1, 1))

  s <-
    FuzzyNumberList$new(c(FuzzyNumber$new(array(
      c(0, 1, 0.6623698, 0.9403180, 1.564024, 1.185641), dim = c(2, 3)
    ))))

  ## invalid parameter type
  expect_error(array$mdd(s, "a", 1, 1, 1))
  expect_error(array$mdd(s, 4L, 1, 1, 1))
  expect_error(array$mdd(s, 4, 1, 1, 1))
  expect_error(array$mdd(s, 4.5, 1, 1, 1))
  expect_error(array$mdd(s, c(1, 2, 3, 4), 1, 1, 1))

  ## invalid parameter a, b or theta
  expect_error(array$mdd(s, 1L, 0, 0, 0))
  expect_error(array$mdd(s, 2L, "a", 0L,-1.5))
  expect_error(array$mdd(s, 3L, 1, list(), 1))
  expect_error(array$mdd(s, 3L, 1, 2, c()))
  expect_error(array$mdd(s, 3L, Inf, Inf, 1))

  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
        TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
      )
    )

  array3 <-
    FuzzyNumberList$new(c(FuzzyNumber$new(array(
      c(0, 1, 0.2, 0.5, 0.7, 0.6), dim = c(2, 3)
    )),
    FuzzyNumber$new(array(
      c(0, 1, 0.3, 0.6, 0.6, 0.6), dim = c(2, 3)
    ))))

  expect_equal(array$mdd(array3, 1L), NA)

  ## all conditions fulfilled
  array2 <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(
        -0.30614214796881267,
        -0.17429571018685452,
        0.41001586290808789,
        1.4885363965873308
      )
    ))

  expect_equal(array$mdd(array2, 2L), 1.7279426087739591)

  array3 <-
    FuzzyNumberList$new(c(FuzzyNumber$new(array(
      c(
        0,
        0.25,
        0.5,
        0.75,
        1,-5.7835504819939967,-5.100726176047438,-4.4179018701008808,-3.7350775641543232,-3.0522532582077657,
        2.57316107511803693,
        2.05738398915402954,
        1.54160690319002169,
        1.02582981722601407,
        0.51005273126200645
      ),
      dim = c(5, 3)
    ))))

  expect_equal(array$mdd(array3, 2L), 1.8811202135945368)
  expect_equal(array$mdd(array3, 2L, 1, 1), 1.8811202135945368)
  expect_equal(array$mdd(array3, 2L, 1, 1, 1), 1.8811202135945368)

  array4 <-
    FuzzyNumberList$new(c(FuzzyNumber$new(array(
      c(
        seq(0, 1, len = 101),
        -1.125144453,
        -1.113739082,
        -1.10233371,
        -1.090928338,
        -1.079522967,
        -1.068117595,
        -1.056712223,
        -1.045306852,
        -1.03390148,
        -1.022496109,
        -1.011090737,
        -0.999685365,
        -0.988279994,
        -0.976874622,
        -0.96546925,
        -0.954063879,
        -0.942658507,
        -0.931253136,
        -0.919847764,
        -0.908442392,
        -0.897037021,
        -0.885631649,
        -0.874226277,
        -0.862820906,
        -0.851415534,
        -0.840010162,
        -0.828604791,
        -0.817199419,
        -0.805794048,
        -0.794388676,
        -0.782983304,
        -0.771577933,
        -0.760172561,
        -0.748767189,
        -0.737361818,
        -0.725956446,
        -0.714551075,
        -0.703145703,
        -0.692288814,
        -0.685806632,
        -0.679324451,
        -0.672842269,
        -0.666360087,
        -0.659877906,
        -0.653395724,
        -0.646913543,
        -0.640431361,
        -0.633949179,
        -0.627466998,
        -0.620984816,
        -0.614502635,
        -0.608020453,
        -0.601538271,
        -0.59505609,
        -0.588573908,
        -0.582091726,
        -0.575609545,
        -0.569127363,
        -0.562645182,
        -0.556163,
        -0.549680818,
        -0.543198637,
        -0.536716455,
        -0.530234274,
        -0.523752092,
        -0.51726991,
        -0.510787729,
        -0.504305547,
        -0.48128216,
        -0.457262228,
        -0.433242296,
        -0.409222363,
        -0.385202431,
        -0.361182499,
        -0.337162566,
        -0.313142634,
        -0.289122702,
        -0.26510277,
        -0.241082837,
        -0.224120094,
        -0.212714723,
        -0.201309351,
        -0.189903979,
        -0.178498608,
        -0.167093236,
        -0.155687865,
        -0.144282493,
        -0.132877121,
        -0.12147175,
        -0.110066378,
        -0.098661006,
        -0.087255635,
        -0.075850263,
        -0.064444892,
        -0.05303952,
        -0.041634148,
        -0.030228777,
        -0.018823405,
        -0.007418033,
        0.003987338,
        0.01539271,
        1.924208103,
        1.915338338,
        1.906468572,
        1.897598806,
        1.88872904,
        1.879859274,
        1.870989508,
        1.862119742,
        1.853249976,
        1.84438021,
        1.835510444,
        1.826640678,
        1.817770912,
        1.808901146,
        1.80003138,
        1.791161614,
        1.782291848,
        1.773422083,
        1.764552317,
        1.755682551,
        1.746812785,
        1.737943019,
        1.729073253,
        1.720203487,
        1.711333721,
        1.702463955,
        1.692201814,
        1.673694268,
        1.655186722,
        1.636679177,
        1.618171631,
        1.599664086,
        1.58115654,
        1.562648994,
        1.544141449,
        1.525633903,
        1.507126357,
        1.488618812,
        1.470111266,
        1.456025635,
        1.442536712,
        1.429047789,
        1.415558866,
        1.402069943,
        1.38858102,
        1.375092097,
        1.361603174,
        1.348114251,
        1.334625328,
        1.321136404,
        1.307647481,
        1.294158558,
        1.280669635,
        1.267180712,
        1.253691789,
        1.240202866,
        1.226713943,
        1.21322502,
        1.199736097,
        1.186247174,
        1.172758251,
        1.159269327,
        1.145780404,
        1.132291481,
        1.118802558,
        1.100960944,
        1.061603712,
        1.02224648,
        0.982889248,
        0.943532016,
        0.904174784,
        0.864817552,
        0.840854715,
        0.822347169,
        0.803839623,
        0.785332078,
        0.766824532,
        0.749320068,
        0.742859156,
        0.736398244,
        0.729937331,
        0.723476419,
        0.717015507,
        0.710554595,
        0.704093683,
        0.697632771,
        0.691171859,
        0.684710947,
        0.678250034,
        0.671789122,
        0.66532821,
        0.658867298,
        0.652406386,
        0.645945474,
        0.639484562,
        0.633023649,
        0.626562737,
        0.620101825,
        0.613640913,
        0.607180001,
        0.602449632
      ),
      dim = c(101, 3)
    ))))

  expect_equal(array$mdd(array4, 1L), 1.4237460135225)
  expect_equal(array$mdd(array4, 3L), 1.8440370093831748)
  expect_equal(array$mdd(array4, 3L, 5, 5, 9 / 8), 1.8496140811856641)

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)
  data1 <- dataTra1$mean()
  expect_equal(dataTra1$mdd(data1, 2L), 0.98891444467999567)

  data1 <- dataTra1$medianWabl()
  expect_equal(dataTra1$mdd(data1, 2L), 0.79217611470200144)

  data1 <- dataTra1$median1Norm()
  expect_equal(dataTra1$mdd(data1, 2L), 0.79715560541368446)

})

test_that("FuzzyNumberList mEstimator function", {
  array <-
    TrapezoidalFuzzyNumberList$new(c(
      TrapezoidalFuzzyNumber$new(1.015641, 1.094014,  1.2814199,  1.8966197),
      TrapezoidalFuzzyNumber$new(-4.829141,-1.605457, 0.4205031, 1.3803774)
    ))

  ## not all mandatory parameters
  expect_error(array$mEstimator())
  expect_error(array$mEstimator("Huber", 0.5, 1, 1, 1))

  ## more parameters than needed
  expect_error(array$mEstimator("uber", 0.5, 0.5, 0.5, 1L, 1, 1, 1, 1))

  ## INVALID PARAMETERS
  ## invalid f
  expect_error(array$mEstimator("uber", 0.5, 0.5, 0.5, 1L, 1, 1, 1))
  expect_error(array$mEstimator("ukey",  0.5, 0.5, 0.5, 1L, 1, 1, 1))
  expect_error(array$mEstimator("auchy", 0.5, 0.5, 0.5, 1L, 1, 1, 1))
  expect_error(array$mEstimator("huber",  0.5, 0.5, 0.5, 1L, 1, 1, 1))
  expect_error(array$mEstimator("tukey",  0.5, 0.5, 0.5, 1L, 1, 1, 1))

  ## invalid estInitial
  expect_error(array$mEstimator("Huber", "a", 1, 1, 1, 1))
  expect_error(array$mEstimator("Tukey", 4L, 1, 1, 1, 1))
  expect_error(array$mEstimator("Cauchy", 0, 1, 1, 1, 1))
  expect_error(array$mEstimator("Huber", -1.56, 1, 1, 1, 1))
  expect_error(array$mEstimator("Tukey", c(1, 2, 3, 4), 1, 1, 1, 1))
  expect_error(array$mEstimator("Tukey", Inf, 1, 1, 1, 1))
  expect_error(array$mEstimator("Tukey",-Inf, 1, 1, 1, 1))

  ## invalid delta
  expect_error(array$mEstimator("Huber", 0.5, 0, 1, 1, 1))
  expect_error(array$mEstimator("Tukey", 4, 1, 1, 1, 1))
  expect_error(array$mEstimator("Cauchy", 1, 1L, 1, 1, 1))
  expect_error(array$mEstimator("Huber", 1.56, 1.01, 1, 1, 1))
  expect_error(array$mEstimator("Tukey", 10,-0.1, 1, 1, 1))

  ## invalid epsilon
  expect_error(array$mEstimator("Huber", 0.5, 0.1, 0, 1, 1, 1))
  expect_error(array$mEstimator("Tukey", 4, 0.1, 0L, 1, 1, 1))
  expect_error(array$mEstimator("Cauchy", 1, 0.1,  1L, 1, 1, 1))
  expect_error(array$mEstimator("Huber", 1.56, 0.1, "a", 1, 1, 1, 1))
  expect_error(array$mEstimator("Tukey", 10, 0.1, -0.1, 1, 1, 1))

  ## invalid parameter type
  expect_error(array$mEstimator("Huber", 0.5, 0.5, "a", 1, 1, 1))
  expect_error(array$mEstimator("Tukey", 4, 0.9, 4L, 1, 1, 1))
  expect_error(array$mEstimator("Cauchy", 1, 0.5555, 1, 1, 1))
  expect_error(array$mEstimator("Huber", 1.56, 0.999, 4.5, 1, 1, 1))
  expect_error(array$mEstimator("Tukey", 10, 0.1, c(1, 2, 3, 4), 1, 1, 1))

  ## invalid parameter a, b or theta
  expect_error(array$mEstimator("Huber", 0.5, 0.5, 1L, 0, 0, 0, 0))
  expect_error(array$mEstimator("Tukey", 4, 0.9, 2L, "a", 0L,-1.5, Inf))
  expect_error(array$mEstimator("Cauchy", 1, 0.5555, 3L, 1, list(), 1, 1))
  expect_error(array$mEstimator("Tukey", 10, 0.1, 3L, 1, 2, c()))
  expect_error(array$mEstimator("Tukey", 10, 0.1, 1, 3L,-Inf,-Inf, Inf))
  expect_error(array$mEstimator("Tukey", 10, 0.1, 1, 3L,-Inf,-Inf, Inf))
  expect_error(array$mEstimator("Tukey", 10, 0.1, 1, 3L, Inf, Inf, Inf))

  ## all conditions fulfilled
  array <-
    TrapezoidalFuzzyNumberList$new(
      c(
        TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197),
        TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774),
        TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827),
        TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)
      )
    )

  expect_equal(array$mEstimator("Huber", 0.123, 0.5, (10) ^ (-5), 1L),
               2.422456209500139)
  expect_equal(array$mEstimator("Huber", 0.123, 0.5, (10) ^ (-5), 1L, 1),
               2.422456209500139)

  expect_equal(array$mEstimator("Tukey", 0.123, 0.5, (10) ^ (-5), 1L),
               3.7848582452283828)
  expect_equal(array$mEstimator("Tukey", 0.123, 0.5, (10) ^ (-5), 1L, 1, 1),
               3.7848582452283828)

  expect_equal(array$mEstimator("Cauchy", 0.123, 0.5, (10) ^ (-5), 1L),
               1.5719476037439555)
  expect_equal(array$mEstimator("Cauchy", 0.123, 0.5, (10) ^ (-5), 1L, 1, 1, 1),
               1.5719476037439555)

  expect_equal(array$mEstimator("Huber", 0.123, 0.5, (10) ^ (-5), 2L),
               2.8968091220593397)

  expect_equal(array$mEstimator("Tukey", 0.123, 0.5, (10) ^ (-5), 2L),
               4.4514520457954561)

  expect_equal(array$mEstimator("Cauchy", 0.123, 0.5, (10) ^ (-5), 2L),
               1.7753664569153211)

  expect_equal(array$mEstimator("Huber", 0.123, 0.5, (10) ^ (-5), 3L),
               3.6385727153232144)

  expect_equal(array$mEstimator("Tukey", 0.123, 0.5, (10) ^ (-5), 3L),
               5.4054947799200157)

  expect_equal(array$mEstimator("Cauchy", 0.123, 0.5, (10) ^ (-5), 3L),
               2.0753281732635096)

  # x antes de llegar a la función privada weight es 0
  t <-
    TrapezoidalFuzzyNumberList$new(c(TrapezoidalFuzzyNumber$new(0, 0, 0, 0)))

  expect_equal(t$mEstimator("Cauchy", 0.123, 0.5, (10) ^ (-5), 3L), 0)

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)
  data1 <- dataTra1$var()

  expect_equal(dataTra1$mEstimator("Huber", data1, 0.5, 10 ^ (-5), 1L),
               1.271415712500048)
  expect_equal(dataTra1$mEstimator("Huber", data1, 0.5, 10 ^ (-5), 2L),
               1.3226616237437916)
  expect_equal(dataTra1$mEstimator("Huber", data1, 0.5, 10 ^ (-5), 3L),
               1.5893034750871069)
  expect_equal(dataTra1$mEstimator("Tukey", data1, 0.5, 10 ^ (-5), 1L),
               1.8506190715641004)
  expect_equal(dataTra1$mEstimator("Tukey", data1, 0.5, 10 ^ (-5), 2L),
               1.949716437828086)
  expect_equal(dataTra1$mEstimator("Tukey", data1, 0.5, 10 ^ (-5), 3L),
               2.3630534703094748)
  expect_equal(dataTra1$mEstimator("Cauchy", data1, 0.5, 10 ^ (-5), 1L),
               0.73466824422383548)
  expect_equal(dataTra1$mEstimator("Cauchy", data1, 0.5, 10 ^ (-5), 2L),
               0.80559277794598727)
  expect_equal(dataTra1$mEstimator("Cauchy", data1, 0.5, 10 ^ (-5), 3L),
               1.0001001996414791)
})

test_that("TrapezoidalFuzzyNumberList getDimension method", {
  tra1 <-
    TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197)
  tra2 <-
    TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774)
  tra3 <-
    TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827)
  tra4 <-
    TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)

  array <-
    TrapezoidalFuzzyNumberList$new(c(tra1, tra2, tra3, tra4))

  ## invalid type parameter
  expect_error(array$getDimension("a"))
  expect_error(array$getDimension(0.55))
  expect_error(array$getDimension(list()))

  ## invalid dimension selected
  expect_error(array$getDimension(-1))
  expect_error(array$getDimension(0))
  expect_error(array$getDimension(3))

  ## valid dimension selected
  expect_equal(array$getDimension(1L), tra1)
  expect_equal(array$getDimension(2L), tra2)
  expect_equal(array$getDimension(3L), tra3)
  expect_equal(array$getDimension(4L), tra4)

})

test_that(
  "TrapezoidalFuzzyNumberList addTrapezoidalFuzzyNumber and removeTrapezoidalFuzzyNumber methods",
  {
    tra1 <-
      TrapezoidalFuzzyNumber$new(-2.015641, -1.094014,  1.2814199,  1.8966197)
    tra2 <-
      TrapezoidalFuzzyNumber$new(-4.829141, -1.605457, -0.4205031, -0.3803774)
    tra3 <-
      TrapezoidalFuzzyNumber$new(-0.1311532,  0.3400337, 0.3976215, 0.7372827)
    tra4 <-
      TrapezoidalFuzzyNumber$new(-5.4843364, -4.5494522, 3.4790804, 3.6759589)

    array <-
      TrapezoidalFuzzyNumberList$new(c(tra1, tra2, tra3, tra4))

    ## not all mandatory parameters
    expect_error(array$addTrapezoidaFuzzyNumber())
    expect_error(array$removeTrapezoidaFuzzyNumber())

    ## more parameters than needed
    expect_error(array$addTrapezoidaFuzzyNumber(1, 1))
    expect_error(array$removeTrapezoidaFuzzyNumber(1, 1))

    ## ADDTRAPEZOIDALFUZZYNUMBER
    ## invalid parameter
    expect_error(array$addTrapezoidaFuzzyNumber("a"))
    expect_error(array$addTrapezoidalFuzzyNumber(0.55))
    expect_error(array$addTrapezoidalFuzzyNumber(list()))
    expect_error(array$addTrapezoidalFuzzyNumber(-1))
    expect_error(array$addTrapezoidalFuzzyNumber(0))
    expect_error(array$addTrapezoidalFuzzyNumber(3L))

    ## valid parameter
    tra <- TrapezoidalFuzzyNumber$new(1, 2, 3, 4)
    array$addTrapezoidalFuzzyNumber(tra)
    expect_equal(array$getLength(), 5)
    expect_equal(array$getDimension(5L), tra)

    ## REMOVETRAPEZOIDALFUZZYNUMBER
    ## invalid parameter
    expect_error(array$removeTrapezoidalFuzzyNumber("a"))
    expect_error(array$removeTrapezoidalFuzzyNumber(0.55))
    expect_error(array$removeTrapezoidalFuzzyNumber(list()))
    expect_error(array$removeTrapezoidalFuzzyNumber(-1))
    expect_error(array$removeTrapezoidalFuzzyNumber(0))
    expect_error(array$removeTrapezoidalFuzzyNumber(6L))

    ## valid parameter
    array$removeTrapezoidalFuzzyNumber(2L)
    expect_equal(array$getLength(), 4)
    expect_equal(array$getDimension(2L), tra3)

    array$removeTrapezoidalFuzzyNumber(1L)
    expect_equal(array$getLength(), 3)
    expect_equal(array$getDimension(1L), tra3)

    array$removeTrapezoidalFuzzyNumber(3L)
    expect_equal(array$getLength(), 2)
    expect_equal(array$getDimension(2L), tra4)

    array$removeTrapezoidalFuzzyNumber(1L)
    expect_equal(array$getLength(), 1)
    expect_equal(array$getDimension(1L), tra4)

  }
)

test_that(
  "TrapezoidalFuzzyNumberList plot method with a list of fuzzy numbers and blue color specifief",
  {
    p1 <-
      TrapezoidalFuzzyNumberList$new(c(
        TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),
        TrapezoidalFuzzyNumber$new(-6, 0, 1, 4)
      ))$plot("blue")
    vdiffr::expect_doppelganger("TrapezoidalFuzzyNumber", p1)

  }
)

test_that(
  "TrapezoidalFuzzyNumberList plot method with a list of fuzzy numbers and several color specified",
  {
    p1 <-
      TrapezoidalFuzzyNumberList$new(c(
        TrapezoidalFuzzyNumber$new(-4,-3,-2,-1),
        TrapezoidalFuzzyNumber$new(-6, 0, 1, 4)
      ))$plot(palette()[2:8])
    vdiffr::expect_doppelganger("TrapezoidalFuzzyNumber", p1)

  }
)
