
test_that("FuzzyNumberList instance works (initialize and checking method)",
          {
            ## CONSTRUCTOR ERRORS
            ## invalid parameter: not a list
            expect_error(FuzzyNumberList$new())
            expect_error(FuzzyNumberList$new(3))
            expect_error(FuzzyNumberList$new("a"))
            expect_error(FuzzyNumberList$new(c(1, 2, 3)))
            ## more parameters than needed
            expect_error(FuzzyNumberList$new(c(1, 2, 3), 1))
            ## invalid parameter: list that does not contain FuzzyNumbers
            c <-
              c(TrapezoidalFuzzyNumber$new(1, 2, 3, 4),
                TrapezoidalFuzzyNumber$new(-1, 2, 3.3, 8.0))
            expect_error(FuzzyNumberList$new(c))
            ## invalid parameter: list does not only contain FuzzyNumbers
            c <-
              c(TrapezoidalFuzzyNumber$new(1, 2, 3, 4),
                FuzzyNumber$new(array(
                  c(0.0, 0.5, 1.0, -1.5, -1.0, -1.0, 2.0, 1.5, 1.0), dim = c(3, 3)
                )))
            expect_error(FuzzyNumberList$new(c))

            ## invalid numbers: valid FuzzyNumbers but all does not have the same alpha-levels
            c <-
              c(FuzzyNumber$new(array(c(
                0.0, 1.0, -1.5, -1.0, 2, 1
              ), dim = c(2, 3))), FuzzyNumber$new(array(
                c(0.0, 0.5, 1.0, -1.5, -1.0, -1.0, 2.0, 1.5, 1.0), dim = c(3, 3)
              )))

            expect_error(FuzzyNumberList$new(c),
                         "all fuzzy numbers must have the same alpha-levels")

            ## invalid numbers: valid FuzzyNumbers but all does not have the same alpha-levels
            c <-
              c(
                FuzzyNumber$new(array(c(
                  0.0, 1.0,-1.5,-1.0, 2, 1
                ), dim = c(2, 3))),
                FuzzyNumber$new(array(
                  c(0.0, 0.5, 1.0,-1.5,-1.0,-1.0, 2.0, 1.5, 1.0), dim = c(3, 3)
                )),
                FuzzyNumber$new(array(
                  c(
                    0.0,
                    0.25,
                    0.5,
                    0.75,
                    1.0,
                    -1.5,
                    -1.4,
                    -1.3,
                    -1.2,
                    -1.1,
                    1.4,
                    1.3,
                    1.2,
                    1.1,
                    1.0
                  ),
                  dim = c(5, 3)
                )),
                FuzzyNumber$new(array(c(
                  0.0, 1.0,-1.5,-1.0, 2, 1
                ), dim = c(2, 3))),
                FuzzyNumber$new(array(
                  c(0.0, 0.6, 1.0,-1.5,-1.0,-1.0, 2.0, 1.5, 1.0), dim = c(3, 3)
                ))
              )
            expect_error(FuzzyNumberList$new(c),
                         "all fuzzy numbers must have the same alpha-levels")

            ## valid numbers
            c <-
              c(FuzzyNumber$new(array(
                c(
                  0.0,
                  0.25,
                  0.5,
                  0.75,
                  1.0,
                  -1.5,
                  -1.4,
                  -1.3,
                  -1.2,
                  -1.1,
                  1.4,
                  1.3,
                  1.2,
                  1.1,
                  1.0
                ),
                dim = c(5, 3)
              )),
              FuzzyNumber$new(array(
                c(
                  0.0,
                  0.25,
                  0.5,
                  0.75,
                  1.0,
                  -1.5,
                  -1.5,
                  -1.5,
                  -1.0,
                  -1.0,
                  2.0,
                  1.5,
                  1.5,
                  1.0,
                  1.0
                ),
                dim = c(5, 3)
              )),
              FuzzyNumber$new(array(
                c(
                  0.0,
                  0.25,
                  0.5,
                  0.75,
                  1.0,-1.5,-1.25,-1.15,-1.1,-1.0,
                  2.0,
                  1.75,
                  1.5,
                  1.25,
                  1.0
                ),
                dim = c(5, 3)
              )))

            fuzzy <- FuzzyNumberList$new(c)
            expect_equal(fuzzy$getLength(), 3)
            expect_equal(nrow(fuzzy$getDimension(3L)$getAlphaLevels()), 5)

          })

test_that("FuzzyNumberList dthetaphi method", {
  array <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 1, 0.6623698, 0.9403180, 1.564024, 1.185641), dim = c(2, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 1, -0.30671609, -0.09722635, 4.49222404, 0.02745836),
    dim = c(2, 3)
  ))))

  ## not all mandatory parameters
  expect_error(array$dthetaphi())

  ## more parameters than needed
  expect_error(array$dthetaphi("a", 1, 1, 1, 1))

  ## invalid parameter s
  expect_error(array$dthetaphi("a", 1, 1, 1))
  expect_error(array$dthetaphi(4L, 1, 1, 1))
  expect_error(array$dthetaphi(4, 1, 1, 1))
  expect_error(array$dthetaphi(4.5, 1, 1, 1))
  expect_error(array$dthetaphi(c(1, 2, 3, 4), 1, 1, 1))

  s <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 1, -0.9202838, -0.6402973, -0.03652915, -0.42350253),
    dim = c(2, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 1, -1.0307571, -0.9230092, 3.413532, 1.627589), dim = c(2, 3)
  ))))

  ## invalid parameter a, b or theta
  expect_error(array$dthetaphi(s, 0, 0, 0))
  expect_error(array$dthetaphi(s, "a", 0L,-1.5))
  expect_error(array$dthetaphi(s, 1, list(), 1))
  expect_error(array$dthetaphi(s, 1, 2, c(1, 2, 3, 4)))
  expect_error(array$dthetaphi(s, 1, Inf,-Inf))

  ## not the same alpha-levels
  array <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 0.75, 1, 0.5, 0.6623698, 0.9403180, 1.6, 1.564024, 1.185641),
    dim = c(3, 3)
  )),
  FuzzyNumber$new(array(
    c(
      0,
      0.75,
      1,-0.4,-0.30671609,-0.09722635,
      5,
      4.49222404,
      0.02745836
    ),
    dim = c(3, 3)
  ))))

  s <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(
      0,
      0.5,
      1.0,-1,-0.9202838,-0.6402973,-0.01,-0.03652915,-0.42350253
    ),
    dim = c(3, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 0.5, 1.0, -1.5, -1.0307571, -0.9230092, 4, 3.413532, 1.627589),
    dim = c(3, 3)
  ))))

  expect_output(
    array$dthetaphi(s, 1, 1, 1),
    "the fuzzy numbers of the two FuzzyNumberLists must have the same alpha-levels"
  )

  ## all conditions fulfilled
  array <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 1, 0.6623698, 0.9403180, 1.564024, 1.185641), dim = c(2, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 1, -0.30671609, -0.09722635, 4.49222404, 0.02745836),
    dim = c(2, 3)
  ))))

  s <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 1, -0.9202838, -0.6402973, -0.03652915, -0.42350253),
    dim = c(2, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 1, -1.0307571, -0.9230092, 3.413532, 1.627589), dim = c(2, 3)
  ))))

  expect_equal(array$dthetaphi(s, 1, 5, 1), matrix(
    c(2.516585684, 5.109558541, 2.80342615, 1.45250297),
    nrow = 2,
    ncol = 2
  ))

  expect_equal(array$dthetaphi(s, 2, 1, 1 / 3), matrix(
    c(1.5949006772, 0.4977266787, 0.9736208296, 0.8002030868),
    nrow = 2,
    ncol = 2
  ))

  expect_equal(array$dthetaphi(s, 2, 1, 1), matrix(
    c(1.5949432007, 0.4991451598, 1.354124494, 1.273250836),
    nrow = 2,
    ncol = 2
  ))

  expect_equal(array$dthetaphi(s, 2), matrix(
    c(1.5949432007, 0.4991451598, 1.354124494, 1.273250836),
    nrow = 2,
    ncol = 2
  ))

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)

  set.seed(1234)
  dataTra2 <- Simulation$new()$simulCase4(5L)

  data1 <- dataTra1$transfTra()
  data2 <- dataTra2$transfTra()

  expect_equal(data1$dthetaphi(data2), matrix(
    c(
      1.98903753677428807,
      0.65103227846685230,
      0.30421651000074174,
      2.00878273840907751,
      0.66680100434263401,
      0.28439446014128239,
      1.83170325535917966,
      0.47634342552039383,
      0.37009799037003088,
      1.89599405848185421,
      0.54212523286170300,
      0.31867175631660227,
      1.64027379556863107,
      0.28807258411205533,
      0.54382616440734521
    ),
    nrow = 3,
    ncol = 5
  ))

})

test_that("FuzzyNumberList dwablphi method", {
  array <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 1, 0.6623698, 0.9403180, 1.564024, 1.185641), dim = c(2, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 1, -0.30671609, -0.09722635, 4.49222404, 0.02745836),
    dim = c(2, 3)
  ))))

  ## not all mandatory parameters
  expect_error(array$dwablphi())

  ## more parameters than needed
  expect_error(array$dwablphi(1, 1, 1, 1, 1))

  ## invalid parameter s
  expect_error(array$dwablphi("a", 1, 1, 1))
  expect_error(array$dwablphi(4L, 1, 1, 1))
  expect_error(array$dwablphi(4, 1, 1, 1))
  expect_error(array$dwablphi(4.5, 1, 1, 1))
  expect_error(array$dwablphi(c(1, 2, 3, 4), 1, 1, 1))

  s <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 1, -0.9202838, -0.6402973, -0.03652915, -0.42350253),
    dim = c(2, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 1, -1.0307571, -0.9230092, 3.413532, 1.627589), dim = c(2, 3)
  ))))

  ## invalid parameter a, b or theta
  expect_error(array$dwablphi(s, 0, 0, 0))
  expect_error(array$dwablphi(s, "a", 0L,-1.5))
  expect_error(array$dwablphi(s, 1, list(), 1))
  expect_error(array$dwablphi(s, 1, 2, c(1, 2, 3, 4)))
  expect_error(array$dwablphi(s, 1, Inf, Inf))

  ## not the same alpha-levels
  array <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 0.75, 1, 0.5, 0.6623698, 0.9403180, 1.6, 1.564024, 1.185641),
    dim = c(3, 3)
  )),
  FuzzyNumber$new(array(
    c(
      0,
      0.75,
      1,-0.4,-0.30671609,-0.09722635,
      5,
      4.49222404,
      0.02745836
    ),
    dim = c(3, 3)
  ))))

  s <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(
      0,
      0.5,
      1.0,-1,-0.9202838,-0.6402973,-0.01,-0.03652915,-0.42350253
    ),
    dim = c(3, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 0.5, 1.0, -1.5, -1.0307571, -0.9230092, 4, 3.413532, 1.627589),
    dim = c(3, 3)
  ))))

  expect_output(
    array$dwablphi(s, 1, 1, 1),
    "the fuzzy numbers of the two FuzzyNumberLists must have the same alpha-levels"
  )

  ## all conditions fulfilled
  array <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 1, 0.6623698, 0.9403180, 1.564024, 1.185641), dim = c(2, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 1, -0.30671609, -0.09722635, 4.49222404, 0.02745836),
    dim = c(2, 3)
  ))))

  s <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 1, -0.9202838, -0.6402973, -0.03652915, -0.42350253),
    dim = c(2, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 1, -1.0307571, -0.9230092, 3.413532, 1.627589), dim = c(2, 3)
  ))))

  expect_equal(array$dwablphi(s, 2, 1, 1), matrix(
    c(1.60914353, 0.54307095, 1.86332720, 1.60013064),
    nrow = 2,
    ncol = 2
  ))

  expect_equal(array$dwablphi(s, 2), matrix(
    c(1.60914353, 0.54307095, 1.86332720, 1.60013064),
    nrow = 2,
    ncol = 2
  ))

  expect_equal(array$dwablphi(s), matrix(
    c(1.604848340, 3.031420687, 1.778227050, 1.185709792),
    nrow = 2,
    ncol = 2
  ))

  expect_equal(array$dwablphi(s, 1, 1, 1), matrix(
    c(1.604848340, 3.031420687, 1.778227050, 1.185709792),
    nrow = 2,
    ncol = 2
  ))

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)

  set.seed(1234)
  dataTra2 <- Simulation$new()$simulCase4(5L)

  data1 <- dataTra1$transfTra()
  data2 <- dataTra2$transfTra()

  expect_equal(data1$dwablphi(data2), matrix(
    c(
      2.12113078925648,
      0.829923724294429,
      0.424901375196048,
      2.140487202586770,
      0.838380525431005,
      0.394359945883828,
      1.972938123870130,
      0.562510306702771,
      0.457853295212778,
      2.009678825468280,
      0.649469526487654,
      0.420718640302279,
      1.758112793154720,
      0.354365678595483,
      0.630023088608773
    ),
    nrow = 3,
    ncol = 5
  ))

})

test_that("FuzzyNumberList rho1 method", {
  array <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 1, 0.6623698, 0.9403180, 1.564024, 1.185641), dim = c(2, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 1, -0.30671609, -0.09722635, 4.49222404, 0.02745836),
    dim = c(2, 3)
  ))))

  ## not all mandatory parameters
  expect_error(array$rho1())

  ## more parameters than needed
  expect_error(array$rho1(1, 1))

  ## invalid parameter s
  expect_error(array$rho1("a"))
  expect_error(array$rho1(4L))
  expect_error(array$rho1(4))
  expect_error(array$rho1(4))
  expect_error(array$rho1(c(1, 2, 3, 4)))

  s <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 1, -0.9202838, -0.6402973, -0.03652915, -0.42350253),
    dim = c(2, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 1, -1.0307571, -0.9230092, 3.413532, 1.627589), dim = c(2, 3)
  ))))

  ## not the same alpha-levels
  array <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 0.75, 1, 0.5, 0.6623698, 0.9403180, 1.6, 1.564024, 1.185641),
    dim = c(3, 3)
  )),
  FuzzyNumber$new(array(
    c(
      0,
      0.75,
      1,-0.4,-0.30671609,-0.09722635,
      5,
      4.49222404,
      0.02745836
    ),
    dim = c(3, 3)
  ))))

  s <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(
      0,
      0.5,
      1.0,-1,-0.9202838,-0.6402973,-0.01,-0.03652915,-0.42350253
    ),
    dim = c(3, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 0.5, 1.0, -1.5, -1.0307571, -0.9230092, 4, 3.413532, 1.627589),
    dim = c(3, 3)
  ))))

  expect_output(
    array$rho1(s),
    "the fuzzy numbers of the two FuzzyNumberLists must have the same alpha-levels"
  )

  ## all conditions fulfilled
  array <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 1, 0.6623698, 0.9403180, 1.564024, 1.185641), dim = c(2, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 1, -0.30671609, -0.09722635, 4.49222404, 0.02745836),
    dim = c(2, 3)
  ))))

  s <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
    c(0, 1, -0.9202838, -0.6402973, -0.03652915, -0.42350253),
    dim = c(2, 3)
  )),
  FuzzyNumber$new(array(
    c(0, 1, -1.0307571, -0.9230092, 3.413532, 1.627589), dim = c(2, 3)
  ))))

  expect_equal(array$rho1(s), matrix(
    c(1.593241395, 1.534088185, 1.4619775250, 0.6975976217),
    nrow = 2,
    ncol = 2
  ))

  #fijar semilla de aleatorización
  set.seed(1234)
  dataTra1 <- Simulation$new()$simulCase1(3L)

  set.seed(1234)
  dataTra2 <- Simulation$new()$simulCase4(5L)

  data1 <- dataTra1$transfTra()
  data2 <- dataTra2$transfTra()

  expect_equal(data1$rho1(data2), matrix(
    c(
      1.982595687242530,
      0.614200284137185,
      0.242513335630809,
      2.002094802466930,
      0.633699399361583,
      0.231471021542988,
      1.823348070963940,
      0.454952667858595,
      0.342185756775459,
      1.890755370241320,
      0.522359967135980,
      0.276547891470030,
      1.633778730159900,
      0.265383327054553,
      0.531204996647870
    ),
    nrow = 3,
    ncol = 5
  ))

})

test_that("FuzzyNumberList getDimension method", {
  f1 <- FuzzyNumber$new(array(c(
    0, 1, 0.6623698, 0.9403180, 1.564024, 1.185641
  ), dim = c(2, 3)))

  f2 <- FuzzyNumber$new(array(
    c(0, 1, -0.30671609, -0.09722635, 4.49222404, 0.02745836),
    dim = c(2, 3)
  ))

  array <- FuzzyNumberList$new(c(f1, f2))

  ## not all mandatory parameters
  expect_error(array$getDimension())

  ## more parameters than needed
  expect_error(array$getDimension("a", 1))

  ## invalid type parameter
  expect_error(array$getDimension("a"))
  expect_error(array$getDimension(0.55))
  expect_error(array$getDimension(list()))

  ## invalid dimension selected
  expect_error(array$getDimension(-1))
  expect_error(array$getDimension(0))
  expect_error(array$getDimension(3))

  ## valid dimension selected
  expect_equal(array$getDimension(1L), f1)
  expect_equal(array$getDimension(2L), f2)

})

test_that("FuzzyNumberList addFuzzyNumber, removeFuzzyNumber and getLength methods",
          {
            array <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
              c(0, 1, 0.6623698, 0.9403180, 1.564024, 1.185641), dim = c(2, 3)
            )),
            FuzzyNumber$new(array(
              c(0, 1, -0.30671609, -0.09722635, 4.49222404, 0.02745836),
              dim = c(2, 3)
            ))))

            ## not all mandatory parameters
            expect_error(array$addFuzzyNumber())
            expect_error(array$removeFuzzyNumber())

            ## more parameters than needed
            expect_error(array$addFuzzyNumber(1, 1))
            expect_error(array$removeFuzzyNumber(1, 1))

            ## ADDFUZZYNUMBER
            ## invalid parameter
            expect_error(array$addFuzzyNumber("a"))
            expect_error(array$addFuzzyNumber(0.55))
            expect_error(array$addFuzzyNumber(list()))
            expect_error(array$addFuzzyNumber(-1))
            expect_error(array$addFuzzyNumber(0))
            expect_error(array$addFuzzyNumber(3L))

            ## valid parameter
            f <-
              FuzzyNumber$new(array(
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
              ))
            array$addFuzzyNumber(f)
            expect_equal(array$getLength(), 3)
            expect_equal(array$getDimension(3L), f)

            ## REMOVEFUZZYNUMBER
            ## invalid parameter
            expect_error(array$removeFuzzyNumber("a"))
            expect_error(array$removeFuzzyNumber(0.55))
            expect_error(array$removeFuzzyNumber(list()))
            expect_error(array$removeFuzzyNumber(-1))
            expect_error(array$removeFuzzyNumber(0))
            expect_error(array$removeFuzzyNumber(4L))

            ## valid parameter
            array$removeFuzzyNumber(2L)
            expect_equal(array$getLength(), 2)
            expect_equal(array$getDimension(2L), f)

            array$removeFuzzyNumber(1L)
            expect_equal(array$getLength(), 1)
            expect_equal(array$getDimension(1L), f)

          })

test_that("FuzzyNumberList plot method with a list of fuzzy numbers and the default color",
          {
            p1 <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
              c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3)
            )),
            FuzzyNumber$new(array(
              c(0.0, 0.5, 1.0, 1, 1.25, 1.5, 2, 1.75, 1.5), dim = c(3, 3)
            ))))$plot()
            vdiffr::expect_doppelganger("FuzzyNumberList", p1)

          })

test_that("FuzzyNumberList plot method with a list of fuzzy numbers and palette colors",
          {
            p1 <- FuzzyNumberList$new(c(FuzzyNumber$new(array(
              c(0.0, 0.5, 1.0, -1, -0.5, 0, 1.5, 1.25, 1), dim = c(3, 3)
            )),
            FuzzyNumber$new(array(
              c(0.0, 0.5, 1.0, 1, 1.25, 1.5, 2, 1.75, 1.5), dim = c(3, 3)
            ))))$plot(palette())
            vdiffr::expect_doppelganger("FuzzyNumberList", p1)

          })
