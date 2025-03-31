# Function to access private variables and functions
get_private <- function(x) {
  x[['.__enclos_env__']]$private
}

test_that("singleStructureGenerator initialization",{
  # Test my_combiStructure initialization
  single_obj <- singleStructureGenerator$new("U",100)
  expect_true(is.null(get_private(single_obj)$my_combiStructure),
              info = "direct singleStructureGenerator initialization assigns not null $my_combiStructure")

  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState= c("M", "U", "M"))

    combi_obj <- combiStructureGenerator$new(infoStr)
  for (i in 1:nrow(infoStr)){
    expect_false(is.null(get_private(combi_obj$get_singleStr(i))$my_combiStructure))
    expect_equal(class(get_private(combi_obj$get_singleStr(i))$my_combiStructure)[1], "combiStructureGenerator",
                 info ="single Str unit initialization by combiStructureGenerator assigns incorrect $my_combiStructure")
    expect_equal(length(get_private(combi_obj)$singleStr[[i]]$get_seq()), 100,
                 info ="single Str unit initialization by combiStructureGenerator generates incorrect sequence length")
    expect_equal(length(get_private(get_private(combi_obj)$singleStr[[1]])$siteR), 100,
                 info ="single Str unit initialization by combiStructureGenerator generates incorrect siteR length")
    expect_equal(length(get_private(get_private(combi_obj)$singleStr[[1]])$eqFreqs), 3,
                 info ="single Str unit initialization by combiStructureGenerator generates incorrect eqFreqs length")
    expect_equal(get_private(combi_obj$get_singleStr(i))$combiStructure_index, i,
                 info ="single Str unit initialization by combiStructureGenerator assigns incorrect combiStructure_index")
  }
})

test_that("combiStructureGenerator initialization", {
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState= c("M", "U", "M"))

  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_equal(length(get_private(combi_obj)$singleStr), nrow(infoStr),
               info = "length of $singleStr not equal to row number in input dataframe")
  expect_equal(length(get_private(combi_obj)$singleStr_globalState), nrow(infoStr),
               info = "length of $singleStr_globalState not equal to row number in input dataframe")
  for (i in 1:nrow(infoStr)){
    expect_equal(class(combi_obj$get_singleStr(i))[1], "singleStructureGenerator",
                 info = "$singleStr elements not of 'singleStructureGenerator' class")
  }
})


test_that("singleStructureGenerator get_seqFirstPos() from combiStructureGenerator instance", {

  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  for (i in 1:nrow(infoStr)){
    expect_equal(combi_obj$get_singleStr(i)$get_seqFirstPos(), combi_obj$get_singleStr(i)$get_seq()[1],
                 info = "output not consistent with get_seq()[1]")
  }
})

test_that("singleStructureGenerator get_seqLastPos() from combiStructureGenerator instance", {
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  for (i in 1:nrow(infoStr)){
    expect_equal(combi_obj$get_singleStr(i)$get_seqLastPos(), combi_obj$get_singleStr(i)$get_seq()[length(combi_obj$get_singleStr(i)$get_seq())],
                 info = "output not consistent with get_seq()[length(..)]")
  }
})



test_that("singleStructureGenerator get_nextStr()", {
  infoStr <- data.frame(n = c(10, 1, 10),
                        globalState= c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_equal(length(get_private(combi_obj$get_singleStr(1))$get_nextStr()$get_seq()), 1)
  expect_equal(length(get_private(combi_obj$get_singleStr(2))$get_nextStr()$get_seq()), 10)
  expect_null(get_private(combi_obj$get_singleStr(3))$get_nextStr())
  single_obj <- singleStructureGenerator$new("U",100)
  expect_null(get_private(single_obj)$get_nextStr())

})


test_that("singleStructureGenerator get_prevStr()", {
  infoStr <- data.frame(n = c(10, 1, 10),
                        globalState= c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_null(get_private(combi_obj$get_singleStr(1))$get_prevStr())
  expect_equal(length(get_private(combi_obj$get_singleStr(2))$get_prevStr()$get_seq()), 10)
  expect_equal(length(get_private(combi_obj$get_singleStr(3))$get_prevStr()$get_seq()), 1)
  single_obj <- singleStructureGenerator$new("U",100)
  expect_null(get_private(single_obj)$get_prevStr())
})

test_that("singleStructureGenerator init_neighbSt()", {

  # Test case 2: "long" single singleStructure instance initiated from combiStructure instance
  mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3)
  infoStr <- data.frame(n = c(5),
                        globalState = c("M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  # position 1
  leftN <- combi_obj$get_singleStr(1)$get_seq()[2]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[2]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "incorrect initialization test case 2, position 1")
  # position 2
  leftN <- combi_obj$get_singleStr(1)$get_seq()[1]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[3]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "incorrect initialization test case 2, position 2")
  # position 3
  leftN <- combi_obj$get_singleStr(1)$get_seq()[2]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[4]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[3], exp_neighbSt,
               info = "incorrect initialization test case 2, position 3")
  # position 4
  leftN <- combi_obj$get_singleStr(1)$get_seq()[3]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[5]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[4], exp_neighbSt,
               info = "incorrect initialization test case 2, position 4")
  # position 5
  leftN <- combi_obj$get_singleStr(1)$get_seq()[4]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[4]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[5], exp_neighbSt,
               info = "incorrect initialization test case 2, position 5")

  # Test case 3: "short" single singleStructure instance initiated from combiStructure instance
  infoStr <- data.frame(n = c(2),
                        globalState = c("M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  # position 1
  leftN <- combi_obj$get_singleStr(1)$get_seq()[2]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[2]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "incorrect initialization test case 3, position 1")
  # position 2
  leftN <- combi_obj$get_singleStr(1)$get_seq()[1]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[1]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "incorrect initialization test case 3, position 2")

  # Test case 4: "short" singleStructure instances within combiStructure instances with more than 1 singleStructure
  ## Test case 4.1: "short" singleStructure instance is first one
  infoStr <- data.frame(n = c(2, 5, 5),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  # singleStr1: position 1
  leftN <- combi_obj$get_singleStr(1)$get_seq()[2]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[2]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "incorrect initialization test case 4.1, singleStr1: position 1")
  # singleStr1: position 2
  leftN <- combi_obj$get_singleStr(1)$get_seq()[1]
  rightN <- combi_obj$get_singleStr(2)$get_seq()[1]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "incorrect initialization test case 4.1, singleStr1: position 2")
  # singleStr2: position 1
  leftN <- combi_obj$get_singleStr(1)$get_seq()[2]
  rightN <- combi_obj$get_singleStr(2)$get_seq()[2]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "incorrect initialization test case 4.1, singleStr2: position 1")
  # singleStr2: position 2
  leftN <- combi_obj$get_singleStr(2)$get_seq()[1]
  rightN <- combi_obj$get_singleStr(2)$get_seq()[3]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[2], exp_neighbSt,
               info = "incorrect initialization test case 4.1, singleStr2: position 2")
  # singleStr2: position 3
  leftN <- combi_obj$get_singleStr(2)$get_seq()[2]
  rightN <- combi_obj$get_singleStr(2)$get_seq()[4]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[3], exp_neighbSt,
               info = "incorrect initialization test case 4.1, singleStr2: position 3")
  # singleStr2: position 4
  leftN <- combi_obj$get_singleStr(2)$get_seq()[3]
  rightN <- combi_obj$get_singleStr(2)$get_seq()[5]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[4], exp_neighbSt,
               info = "incorrect initialization test case 4.1, singleStr2: position 4")
  # singleStr2: position 5
  leftN <- combi_obj$get_singleStr(2)$get_seq()[4]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[1]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[5], exp_neighbSt,
               info = "incorrect initialization test case 4.1, singleStr2: position 5")
  # singleStr3: position 1
  leftN <- combi_obj$get_singleStr(2)$get_seq()[5]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[2]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(3))$neighbSt[1], exp_neighbSt,
               info = "incorrect initialization test case 4.1, singleStr3: position 1")
  # singleStr3: position 2
  leftN <- combi_obj$get_singleStr(3)$get_seq()[1]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[3]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(3))$neighbSt[2], exp_neighbSt,
               info = "incorrect initialization test case 4.1, singleStr3: position 2")
  # singleStr3: position 3
  leftN <- combi_obj$get_singleStr(3)$get_seq()[2]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[4]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(3))$neighbSt[3], exp_neighbSt,
               info = "incorrect initialization test case 4.1, singleStr3: position 3")
  # singleStr3: position 4
  leftN <- combi_obj$get_singleStr(3)$get_seq()[3]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[5]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(3))$neighbSt[4], exp_neighbSt,
               info = "incorrect initialization test case 4.1, singleStr3: position 4")
  # singleStr3: position 5
  leftN <- combi_obj$get_singleStr(3)$get_seq()[4]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[4]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(3))$neighbSt[5], exp_neighbSt,
               info = "incorrect initialization test case 4.1, singleStr3: position 5")

  ## Test case 4.2: "short" singleStructure instance is second one
  infoStr <- data.frame(n = c(5, 2, 5),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  # singleStr1: position 1
  leftN <- combi_obj$get_singleStr(1)$get_seq()[2]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[2]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "incorrect initialization test case 4.2, singleStr1: position 1")
  # singleStr1: position 2
  leftN <- combi_obj$get_singleStr(1)$get_seq()[1]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[3]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "incorrect initialization test case 4.2, singleStr1: position 2")
  # singleStr1: position 3
  leftN <- combi_obj$get_singleStr(1)$get_seq()[2]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[4]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[3], exp_neighbSt,
               info = "incorrect initialization test case 4.2, singleStr1: position 3")
  # singleStr1: position 4
  leftN <- combi_obj$get_singleStr(1)$get_seq()[3]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[5]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[4], exp_neighbSt,
               info = "incorrect initialization test case 4.2, singleStr1: position 4")
  # singleStr1: position 5
  leftN <- combi_obj$get_singleStr(1)$get_seq()[4]
  rightN <- combi_obj$get_singleStr(2)$get_seq()[1]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[5], exp_neighbSt,
               info = "incorrect initialization test case 4.2, singleStr1: position 5")
  # singleStr2: position 1
  leftN <- combi_obj$get_singleStr(1)$get_seq()[5]
  rightN <- combi_obj$get_singleStr(2)$get_seq()[2]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "incorrect initialization test case 4.2, singleStr2: position 1")
  # singleStr2: position 2
  leftN <- combi_obj$get_singleStr(2)$get_seq()[1]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[1]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[2], exp_neighbSt,
               info = "incorrect initialization test case 4.2, singleStr2: position 2")
  # singleStr3: position 1
  leftN <- combi_obj$get_singleStr(2)$get_seq()[2]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[2]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(3))$neighbSt[1], exp_neighbSt,
               info = "incorrect initialization test case 4.2, singleStr3: position 1")
  # singleStr3: position 2
  leftN <- combi_obj$get_singleStr(3)$get_seq()[1]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[3]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(3))$neighbSt[2], exp_neighbSt,
               info = "incorrect initialization test case 4.2, singleStr3: position 2")
  # singleStr3: position 3
  leftN <- combi_obj$get_singleStr(3)$get_seq()[2]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[4]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(3))$neighbSt[3], exp_neighbSt,
               info = "incorrect initialization test case 4.2, singleStr3: position 3")
  # singleStr3: position 4
  leftN <- combi_obj$get_singleStr(3)$get_seq()[3]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[5]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(3))$neighbSt[4], exp_neighbSt,
               info = "incorrect initialization test case 4.2, singleStr3: position 4")
  # singleStr3: position 5
  leftN <- combi_obj$get_singleStr(3)$get_seq()[4]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[4]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(3))$neighbSt[5], exp_neighbSt,
               info = "incorrect initialization test case 4.2, singleStr3: position 5")

  ## Test case 4.3: "short" singleStructure instance is last one
  infoStr <- data.frame(n = c(5, 5, 2),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  # singleStr1: position 1
  leftN <- combi_obj$get_singleStr(1)$get_seq()[2]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[2]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "incorrect initialization test case 4.3, singleStr1: position 1")
  # singleStr1: position 2
  leftN <- combi_obj$get_singleStr(1)$get_seq()[1]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[3]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "incorrect initialization test case 4.3, singleStr1: position 2")
  # singleStr1: position 3
  leftN <- combi_obj$get_singleStr(1)$get_seq()[2]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[4]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[3], exp_neighbSt,
               info = "incorrect initialization test case 4.3, singleStr1: position 3")
  # singleStr1: position 4
  leftN <- combi_obj$get_singleStr(1)$get_seq()[3]
  rightN <- combi_obj$get_singleStr(1)$get_seq()[5]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[4], exp_neighbSt,
               info = "incorrect initialization test case 4.3, singleStr1: position 4")
  # singleStr1: position 5
  leftN <- combi_obj$get_singleStr(1)$get_seq()[4]
  rightN <- combi_obj$get_singleStr(2)$get_seq()[1]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[5], exp_neighbSt,
               info = "incorrect initialization test case 4.3, singleStr1: position 5")
  # singleStr2: position 1
  leftN <- combi_obj$get_singleStr(1)$get_seq()[5]
  rightN <- combi_obj$get_singleStr(2)$get_seq()[2]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "incorrect initialization test case 4.3, singleStr2: position 1")
  # singleStr2: position 2
  leftN <- combi_obj$get_singleStr(2)$get_seq()[1]
  rightN <- combi_obj$get_singleStr(2)$get_seq()[3]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[2], exp_neighbSt,
               info = "incorrect initialization test case 4.3, singleStr2: position 2")
  # singleStr2: position 3
  leftN <- combi_obj$get_singleStr(2)$get_seq()[2]
  rightN <- combi_obj$get_singleStr(2)$get_seq()[4]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[3], exp_neighbSt,
               info = "incorrect initialization test case 4.3, singleStr2: position 3")
  # singleStr2: position 4
  leftN <- combi_obj$get_singleStr(2)$get_seq()[3]
  rightN <- combi_obj$get_singleStr(2)$get_seq()[5]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[4], exp_neighbSt,
               info = "incorrect initialization test case 4.3, singleStr2: position 4")
  # singleStr2: position 5
  leftN <- combi_obj$get_singleStr(2)$get_seq()[4]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[1]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[5], exp_neighbSt,
               info = "incorrect initialization test case 4.3, singleStr2: position 5")
  # singleStr3: position 1
  leftN <- combi_obj$get_singleStr(2)$get_seq()[5]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[2]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(3))$neighbSt[1], exp_neighbSt,
               info = "incorrect initialization test case 4.3, singleStr3: position 1")
  # singleStr3: position 2
  leftN <- combi_obj$get_singleStr(3)$get_seq()[1]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[1]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(3))$neighbSt[2], exp_neighbSt,
               info = "incorrect initialization test case 4.3, singleStr3: position 2")

})

test_that("init_neighbSt() in structure of length 1", {
  mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3)
  # Test case 1: structure of length 1 in singleStructure instance
  obj <- singleStructureGenerator$new("M", 1)
  expect_equal(get_private(obj)$neighbSt, mapNeighbSt_matrix[obj$get_seq(), obj$get_seq()],
               info ="assigns incorrect $neighbSt in singleStructure instance of length 1")

  # Test case 2: intermediate structure of length 1 in combiStructure instance
  infoStr <- data.frame(n = c(10, 1, 10),
                        globalState= c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  # Check lengths according to expected
  expect_equal(length(combi_obj$get_singleStr(1)$get_seq()), 10)
  expect_equal(length(combi_obj$get_singleStr(2)$get_seq()), 1)
  expect_equal(length(combi_obj$get_singleStr(3)$get_seq()), 10)
  # Check neighbSt accordint to expected
  leftN <- combi_obj$get_singleStr(1)$get_seq()[10]
  rightN <- combi_obj$get_singleStr(3)$get_seq()[1]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt, exp_neighbSt,
               info = "intermediate singleStr of length 1 gets wrong assignment of neighbSt")

  # Test case 3: first structure of length 1 in combiStructure instance
  infoStr <- data.frame(n = c(1, 10, 10),
                        globalState= c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  # Check lengths according to expected
  expect_equal(length(combi_obj$get_singleStr(1)$get_seq()), 1)
  expect_equal(length(combi_obj$get_singleStr(2)$get_seq()), 10)
  expect_equal(length(combi_obj$get_singleStr(3)$get_seq()), 10)
  # Check neighbSt accordint to expected (first position next structure counts as both neighbors)
  leftN <- combi_obj$get_singleStr(2)$get_seq()[1]
  rightN <- combi_obj$get_singleStr(2)$get_seq()[1]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt, exp_neighbSt,
               info = "first singleStr of length 1 gets wrong assignment of neighbSt")
  # Test case 3.1: first combiStr instance with only singleStr instance of length 1
  infoStr <- data.frame(n = c(1),
                        globalState= c("M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  leftN <- combi_obj$get_singleStr(1)$get_seq()
  rightN <- combi_obj$get_singleStr(1)$get_seq()
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt, exp_neighbSt,
               info = "3.1. first and only singleStr of length 1 gets wrong assignment of neighbSt")


  # Test case 4: last structure of length 1 in combiStructure instance
  infoStr <- data.frame(n = c(10, 10, 1),
                        globalState= c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  # Check lengths according to expected
  expect_equal(length(combi_obj$get_singleStr(1)$get_seq()), 10)
  expect_equal(length(combi_obj$get_singleStr(2)$get_seq()), 10)
  expect_equal(length(combi_obj$get_singleStr(3)$get_seq()), 1)
  # Check neighbSt accordint to expected (last position in previous structure counts as both neighbors)
  leftN <- combi_obj$get_singleStr(2)$get_seq()[length(combi_obj$get_singleStr(2)$get_seq())]
  rightN <- combi_obj$get_singleStr(2)$get_seq()[length(combi_obj$get_singleStr(2)$get_seq())]
  exp_neighbSt <- mapNeighbSt_matrix[leftN, rightN]
  expect_equal(get_private(combi_obj$get_singleStr(3))$neighbSt, exp_neighbSt,
               info = "last singleStr of length 1 gets wrong assignment of neighbSt")
})

test_that("singleStructureGenerator get_seq2ndPos() from combiStructureGenerator instance", {
  
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  # expected under seq length not = 1, $seq second position
  for ( i in 1: nrow(infoStr)){
    expect_equal(combi_obj$get_singleStr(i)$get_seq2ndPos(), combi_obj$get_singleStr(i)$get_seq()[2],
                 info = "returns $seq second position different from expected for length not = 1")
  }
  # expected under seq length = 1 followed by another structure
  infoStr <- data.frame(n = c(13, 1, 4),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_equal(combi_obj$get_singleStr(2)$get_seq2ndPos(), combi_obj$get_singleStr(3)$get_seq()[1],
               info ="returns $sec second position different expected under seq length = 1 followed by another structure")
  # expected under seq length = 1 NOT followed by another structure
  infoStr <- data.frame(n = c(13, 5, 1),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_null(combi_obj$get_singleStr(3)$get_seq2ndPos())
})


test_that("singleStructureGenerator get_seq2ndButLastPos() from combiStructureGenerator instance", {
  
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  # expected under seq length not = 1, $seq second position
  for ( i in 1: nrow(infoStr)){
    expect_equal(combi_obj$get_singleStr(i)$get_seq2ndButLastPos(), combi_obj$get_singleStr(i)$get_seq()[length(combi_obj$get_singleStr(i)$get_seq())-1],
                 info = "returns $seq second but last position different from expected for length not = 1")
  }
  # expected under seq length = 1 preceded by another structure
  infoStr <- data.frame(n = c(13, 1, 4),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_equal(combi_obj$get_singleStr(2)$get_seq2ndButLastPos(), combi_obj$get_singleStr(1)$get_seq()[length(combi_obj$get_singleStr(1)$get_seq())],
               info ="returns $sec second but lastposition different expected under seq length = 1 preceded by another structure")
  # expected under seq length = 1 NOT preceded by another structure
  infoStr <- data.frame(n = c(1, 14, 4),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_null(combi_obj$get_singleStr(1)$get_seq2ndButLastPos())
})


test_that("singleStructureGenerator update_interStr_firstNeighbSt()", {
  single_obj <- singleStructureGenerator$new("U",10)
  
  # Test cases: incorrect leftNeighb_seqSt argument
  expect_error(single_obj$update_interStr_firstNeighbSt(leftNeighb_seqSt = "i", rightNeighb_seqSt = 1),
               info = "method fails to trow error with non-numeric 'leftNeighb_seqSt' argument")
  expect_error(single_obj$update_interStr_firstNeighbSt(leftNeighb_seqSt = NaN, rightNeighb_seqSt = 1),
               info = "method fails to trow error with NaN 'leftNeighb_seqSt' argument")
  expect_error(single_obj$update_interStr_firstNeighbSt(leftNeighb_seqSt = NA, rightNeighb_seqSt = 1),
               info = "method fails to trow error with NA 'leftNeighb_seqSt' argument")
  expect_error(single_obj$update_interStr_firstNeighbSt(leftNeighb_seqSt = 0, rightNeighb_seqSt = 1),
               info = "method fails to trow error with 0 'leftNeighb_seqSt' argument")
  expect_error(single_obj$update_interStr_firstNeighbSt(leftNeighb_seqSt = NULL, rightNeighb_seqSt = 1),
               info = "method fails to trow error with NULL 'leftNeighb_seqSt' argument")
  
  # Test cases: incorrect rightNeighb_seqSt argument
  expect_error(single_obj$update_interStr_firstNeighbSt(leftNeighb_seqSt = 1, rightNeighb_seqSt = "i"),
               info = "method fails to trow error with non-numeric 'rightNeighb_seqSt' argument")
  expect_error(single_obj$update_interStr_firstNeighbSt(leftNeighb_seqSt = 1, rightNeighb_seqSt = NaN),
               info = "method fails to trow error with NaN 'rightNeighb_seqSt' argument")
  expect_error(single_obj$update_interStr_firstNeighbSt(leftNeighb_seqSt = 1, rightNeighb_seqSt = NA),
               info = "method fails to trow error with NA 'rightNeighb_seqSt' argument")
  expect_error(single_obj$update_interStr_firstNeighbSt(leftNeighb_seqSt = 1, rightNeighb_seqSt = 0),
               info = "method fails to trow error with 0 'rightNeighb_seqSt' argument")
  # Expect no error when both are correct
  expect_no_error(single_obj$update_interStr_firstNeighbSt(leftNeighb_seqSt = 1, rightNeighb_seqSt = NULL))
  expect_no_error(single_obj$update_interStr_firstNeighbSt(leftNeighb_seqSt = 1, rightNeighb_seqSt = 1))
  
  # Test cases: update neighbSt first position
  single_obj <- singleStructureGenerator$new("U",10)
  single_obj$update_interStr_firstNeighbSt(3,2)
  expect_equal(get_private(single_obj)$neighbSt[1], 8,
               info = "method fails to correctly update neighbSt given 2 correct non NULL arguments")
  single_obj$update_interStr_firstNeighbSt(2, NULL)
  expect_equal(get_private(single_obj)$neighbSt[1], 5,
               info = "method fails to correctly update neighbSt given 2 correct arguments")
})


test_that("singleStructureGenerator update_interStr_lastNeighbSt()", {
  single_obj <- singleStructureGenerator$new("U",10)
  
  # Test cases: incorrect leftNeighb_seqSt argument
  expect_error(single_obj$update_interStr_lastNeighbSt(leftNeighb_seqSt = "i", rightNeighb_seqSt = 1),
               info = "method fails to trow error with non-numeric 'leftNeighb_seqSt' argument")
  expect_error(single_obj$update_interStr_lastNeighbSt(leftNeighb_seqSt = NaN, rightNeighb_seqSt = 1),
               info = "method fails to trow error with NaN 'leftNeighb_seqSt' argument")
  expect_error(single_obj$update_interStr_lastNeighbSt(leftNeighb_seqSt = NA, rightNeighb_seqSt = 1),
               info = "method fails to trow error with NA 'leftNeighb_seqSt' argument")
  expect_error(single_obj$update_interStr_lastNeighbSt(leftNeighb_seqSt = 0, rightNeighb_seqSt = 1),
               info = "method fails to trow error with 0 'leftNeighb_seqSt' argument")
  
  
  # Test cases: incorrect rightNeighb_seqSt argument
  expect_error(single_obj$update_interStr_lastNeighbSt(leftNeighb_seqSt = 1, rightNeighb_seqSt = "i"),
               info = "method fails to trow error with non-numeric 'rightNeighb_seqSt' argument")
  expect_error(single_obj$update_interStr_lastNeighbSt(leftNeighb_seqSt = 1, rightNeighb_seqSt = NaN),
               info = "method fails to trow error with NaN 'rightNeighb_seqSt' argument")
  expect_error(single_obj$update_interStr_lastNeighbSt(leftNeighb_seqSt = 1, rightNeighb_seqSt = NA),
               info = "method fails to trow error with NA 'rightNeighb_seqSt' argument")
  expect_error(single_obj$update_interStr_lastNeighbSt(leftNeighb_seqSt = 1, rightNeighb_seqSt = 0),
               info = "method fails to trow error with 0 'rightNeighb_seqSt' argument")
  expect_error(single_obj$update_interStr_lastNeighbSt(leftNeighb_seqSt = 1, rightNeighb_seqSt = NULL),
               info = "method fails to trow error with NULL 'leftNeighb_seqSt' argument")
  
  # Expect no error when both are correct
  expect_no_error(single_obj$update_interStr_lastNeighbSt(leftNeighb_seqSt = NULL, rightNeighb_seqSt = 1))
  expect_no_error(single_obj$update_interStr_lastNeighbSt(leftNeighb_seqSt = 1, rightNeighb_seqSt = 1))
  
  # Test cases: update neighbSt first position
  single_obj <- singleStructureGenerator$new("U",10)
  single_obj$update_interStr_lastNeighbSt(3,3)
  expect_equal(get_private(single_obj)$neighbSt[10], 9,
               info = "method fails to correctly update neighbSt given 2 correct non NULL arguments")
  single_obj$update_interStr_lastNeighbSt(NULL, 1)
  expect_equal(get_private(single_obj)$neighbSt[10], 1,
               info = "method fails to correctly update neighbSt given 2 correct arguments")
})


test_that("singleStructureGenerator update_intraStr_neighbSt()", {
  single_obj <- singleStructureGenerator$new("U",13)

  # Test cases: incorrect input
  expect_error(get_private(single_obj)$update_intraStr_neighbSt("i"),
               info = "method fails to trow error with non-numeric 'position' argument")
  expect_error(get_private(single_obj)$update_intraStr_neighbSt(c(1,2)),
               info = "method fails to trow error with non-numeric 'position' argument")
  expect_error(get_private(single_obj)$update_intraStr_neighbSt(14),
               info = "method fails to trow error with 'position' argument bigger than sequence length")
  expect_error(get_private(single_obj)$update_intraStr_neighbSt(0),
               info = "method fails to trow error with 'position' argument 0")
  expect_error(get_private(single_obj)$update_intraStr_neighbSt(1.5),
               info = "method fails to trow error with 'position' argument non-index (integer)")

  # Test cases: change of methylation state in different positions within singleStructure instances
  if (! "modify_seqPos"%in% names(singleStructureGenerator$public_methods)){
    singleStructureGenerator$set("public", "modify_seqPos", function(position, newState) {
      private$seq[position] <-newState
    })
  }
  mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3)

  ## Structure of length 1
  single_obj <- singleStructureGenerator$new("U", 1)
  single_obj$modify_seqPos(position = 1, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  expect_equal(get_private(single_obj)$neighbSt, 1,
               info = "singleStr of length 1 assigns wrong neighbSt for newState 1")
  single_obj$modify_seqPos(position = 1, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  expect_equal(get_private(single_obj)$neighbSt, 5,
               info = "singleStr of length 1 assigns wrong neighbSt for newState 2")
  single_obj$modify_seqPos(position = 1, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  expect_equal(get_private(single_obj)$neighbSt, 9,
               info = "singleStr of length 1 assigns wrong neighbSt for newState 3")
  
  ## Structure of length 2: change seq state position 1, update neighbSt position 2
  single_obj <- singleStructureGenerator$new("U", 2)
  single_obj$modify_seqPos(position = 1, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  expect_equal(get_private(single_obj)$neighbSt[2], 1,
               info = "singleStr of length 2 assigns wrong neighbSt to position 2 for newState 1")
  single_obj$modify_seqPos(position = 1, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  expect_equal(get_private(single_obj)$neighbSt[2], 5,
               info = "singleStr of length 2 assigns wrong neighbSt to position 2 for newState 2")
  single_obj$modify_seqPos(position = 1, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  expect_equal(get_private(single_obj)$neighbSt[2], 9,
               info = "singleStr of length 2 assigns wrong neighbSt to position 2 for newState 3")
  
  ## Structure of length 2: change seq state position 2, update neighbSt position 1
  single_obj <- singleStructureGenerator$new("U", 2)
  single_obj$modify_seqPos(position = 2, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(2)
  expect_equal(get_private(single_obj)$neighbSt[1], 1,
               info = "singleStr of length 2 assigns wrong neighbSt to position 1 for newState 1")
  single_obj$modify_seqPos(position = 2, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(2)
  expect_equal(get_private(single_obj)$neighbSt[1], 5,
               info = "singleStr of length 2 assigns wrong neighbSt to position 1 for newState 2")
  single_obj$modify_seqPos(position = 2, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(2)
  expect_equal(get_private(single_obj)$neighbSt[1], 9,
               info = "singleStr of length 2 assigns wrong neighbSt to position 1 for newState 3")
 
  ## Structure of length 3: change seq state position 1, update neighbSt position 2
  single_obj <- singleStructureGenerator$new("U", 3)
  single_obj$modify_seqPos(position = 1, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[1, single_obj$get_seq()[3]]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 3 assigns wrong neighbSt to position 2 for newState 1 position 1")
  single_obj$modify_seqPos(position = 1, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[2, single_obj$get_seq()[3]]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 3 assigns wrong neighbSt to position 2 for newState 2 position 1")
  single_obj$modify_seqPos(position = 1, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[3, single_obj$get_seq()[3]]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 3 assigns wrong neighbSt to position 2 for newState 3 position 1")
  
  ## Structure of length 3: change seq state position 2, update neighbSt position 1 and 3
  single_obj <- singleStructureGenerator$new("U", 3)
  single_obj$modify_seqPos(position = 2, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(2)
  expect_equal(get_private(single_obj)$neighbSt[1], 1,
               info = "singleStr of length 3 assigns wrong neighbSt to position 1 for newState 1 position 2")
  expect_equal(get_private(single_obj)$neighbSt[3], 1,
               info = "singleStr of length 3 assigns wrong neighbSt to position 3 for newState 1 position 2")
  single_obj$modify_seqPos(position = 2, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(2)
  expect_equal(get_private(single_obj)$neighbSt[1], 5,
               info = "singleStr of length 3 assigns wrong neighbSt to position 1 for newState 2 position 2")
  expect_equal(get_private(single_obj)$neighbSt[3], 5,
               info = "singleStr of length 3 assigns wrong neighbSt to position 3 for newState 2 position 2")
  single_obj$modify_seqPos(position = 2, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(2)
  expect_equal(get_private(single_obj)$neighbSt[1], 9,
               info = "singleStr of length 3 assigns wrong neighbSt to position 1 for newState 3 position 2")
  expect_equal(get_private(single_obj)$neighbSt[3], 9,
               info = "singleStr of length 3 assigns wrong neighbSt to position 3 for newState 3 position 2")
  
  ## Structure of length 3: change seq state position 3, update neighbSt position 2
  single_obj <- singleStructureGenerator$new("U", 3)
  single_obj$modify_seqPos(position = 3, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[1], 1]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 3 assigns wrong neighbSt to position 2 for newState 1 position 3")
  single_obj$modify_seqPos(position = 3, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[1], 2]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 3 assigns wrong neighbSt to position 2 for newState 2 position 3")
  single_obj$modify_seqPos(position = 3, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[1], 3]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 3 assigns wrong neighbSt to position 2 for newState 3 position 3")
  
  
  ## Structure of length 4: change seq state position 1, update neighbSt position 2
  single_obj <- singleStructureGenerator$new("U", 4)
  single_obj$modify_seqPos(position = 1, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[1, single_obj$get_seq()[3]]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 4 assigns wrong neighbSt to position 2 for newState 1 position 1")
  single_obj$modify_seqPos(position = 1, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[2, single_obj$get_seq()[3]]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 4 assigns wrong neighbSt to position 2 for newState 2 position 1")
  single_obj$modify_seqPos(position = 1, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[3, single_obj$get_seq()[3]]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 4 assigns wrong neighbSt to position 2 for newState 3 position 1")
  
  
  ## Structure of length 4: change seq state position 2, update neighbSt position 1 and 3
  single_obj <- singleStructureGenerator$new("U", 4)
  single_obj$modify_seqPos(position = 2, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(2)
  expect_equal(get_private(single_obj)$neighbSt[1], 1,
               info = "singleStr of length 4 assigns wrong neighbSt to position 1 for newState 1 position 2")
  exp_neighbSt <- mapNeighbSt_matrix[1, single_obj$get_seq()[4]]
  expect_equal(get_private(single_obj)$neighbSt[3], exp_neighbSt,
               info = "singleStr of length 4 assigns wrong neighbSt to position 3 for newState 1 position 2")
  single_obj$modify_seqPos(position = 2, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(2)
  expect_equal(get_private(single_obj)$neighbSt[1], 5,
               info = "singleStr of length 4 assigns wrong neighbSt to position 1 for newState 2 position 2")
  exp_neighbSt <- mapNeighbSt_matrix[2, single_obj$get_seq()[4]]
  expect_equal(get_private(single_obj)$neighbSt[3], exp_neighbSt,
               info = "singleStr of length 4 assigns wrong neighbSt to position 3 for newState 2 position 2")
  single_obj$modify_seqPos(position = 2, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(2)
  expect_equal(get_private(single_obj)$neighbSt[1], 9,
               info = "singleStr of length 4 assigns wrong neighbSt to position 1 for newState 3 position 2")
  exp_neighbSt <- mapNeighbSt_matrix[3, single_obj$get_seq()[4]]
  expect_equal(get_private(single_obj)$neighbSt[3], exp_neighbSt,
               info = "singleStr of length 4 assigns wrong neighbSt to position 3 for newState 3 position 2")
  
  ## Structure of length 4: change seq state position 3, update neighbSt position 2 and 4
  single_obj <- singleStructureGenerator$new("U", 4)
  single_obj$modify_seqPos(position = 3, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[1], 1]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 4 assigns wrong neighbSt to position 2 for newState 1 position 3")
  expect_equal(get_private(single_obj)$neighbSt[4], 1,
               info = "singleStr of length 4 assigns wrong neighbSt to position 4 for newState 1 position 3")
  single_obj$modify_seqPos(position = 3, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[1], 2]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 4 assigns wrong neighbSt to position 2 for newState 2 position 3")
  expect_equal(get_private(single_obj)$neighbSt[4], 5,
               info = "singleStr of length 4 assigns wrong neighbSt to position 4 for newState 2 position 3")
  single_obj$modify_seqPos(position = 3, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[1], 3]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 4 assigns wrong neighbSt to position 2 for newState 3 position 3")
  expect_equal(get_private(single_obj)$neighbSt[4], 9,
               info = "singleStr of length 4 assigns wrong neighbSt to position 4 for newState 3 position 3")
  
  ## Structure of length 4: change seq state position 4, update neighbSt position 3
  single_obj <- singleStructureGenerator$new("U", 4)
  single_obj$modify_seqPos(position = 4, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(4)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[2], 1]
  expect_equal(get_private(single_obj)$neighbSt[3], exp_neighbSt,
               info = "singleStr of length 4 assigns wrong neighbSt to position 3 for newState 1 position 4")
  single_obj$modify_seqPos(position = 4, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(4)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[2], 2]
  expect_equal(get_private(single_obj)$neighbSt[3], exp_neighbSt,
               info = "singleStr of length 4 assigns wrong neighbSt to position 3 for newState 2 position 4")
  single_obj$modify_seqPos(position = 4, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(4)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[2], 3]
  expect_equal(get_private(single_obj)$neighbSt[3], exp_neighbSt,
               info = "singleStr of length 4 assigns wrong neighbSt to position 3 for newState 3 position 4")
  
  
  ## Structure of length 5: change seq state position 1, update neighbSt position 2
  single_obj <- singleStructureGenerator$new("U", 5)
  single_obj$modify_seqPos(position = 1, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[1, single_obj$get_seq()[3]]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 2 for newState 1 position 1")
  single_obj$modify_seqPos(position = 1, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[2, single_obj$get_seq()[3]]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 2 for newState 2 position 1")
  single_obj$modify_seqPos(position = 1, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[3, single_obj$get_seq()[3]]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 2 for newState 3 position 1")
  
  ## Structure of length 5: change seq state position 2, update neighbSt position 1 and 3
  single_obj <- singleStructureGenerator$new("U", 5)
  single_obj$modify_seqPos(position = 2, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(2)
  expect_equal(get_private(single_obj)$neighbSt[1], 1,
               info = "singleStr of length 5 assigns wrong neighbSt to position 1 for newState 1 position 2")
  exp_neighbSt <- mapNeighbSt_matrix[1, single_obj$get_seq()[4]]
  expect_equal(get_private(single_obj)$neighbSt[3], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 3 for newState 1 position 2")
  single_obj$modify_seqPos(position = 2, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(2)
  expect_equal(get_private(single_obj)$neighbSt[1], 5,
               info = "singleStr of length 5 assigns wrong neighbSt to position 1 for newState 2 position 2")
  exp_neighbSt <- mapNeighbSt_matrix[2, single_obj$get_seq()[4]]
  expect_equal(get_private(single_obj)$neighbSt[3], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 3 for newState 2 position 2")
  single_obj$modify_seqPos(position = 2, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(2)
  expect_equal(get_private(single_obj)$neighbSt[1], 9,
               info = "singleStr of length 5 assigns wrong neighbSt to position 1 for newState 3 position 2")
  exp_neighbSt <- mapNeighbSt_matrix[3, single_obj$get_seq()[4]]
  expect_equal(get_private(single_obj)$neighbSt[3], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 3 for newState 3 position 2")
  
  ## Structure of length 5: change seq state position 3, update neighbSt position 2 and 4
  single_obj <- singleStructureGenerator$new("U", 5)
  single_obj$modify_seqPos(position = 3, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[1], 1]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 2 for newState 1 position 3")
  exp_neighbSt <- mapNeighbSt_matrix[1, single_obj$get_seq()[5]]
  expect_equal(get_private(single_obj)$neighbSt[4], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 4 for newState 1 position 3")
  single_obj$modify_seqPos(position = 3, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[1], 2]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 2 for newState 2 position 3")
  exp_neighbSt <- mapNeighbSt_matrix[2, single_obj$get_seq()[5]]
  expect_equal(get_private(single_obj)$neighbSt[4], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 4 for newState 2 position 3")
  single_obj$modify_seqPos(position = 3, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[1], 3]
  expect_equal(get_private(single_obj)$neighbSt[2], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 2 for newState 3 position 3")
  exp_neighbSt <- mapNeighbSt_matrix[3, single_obj$get_seq()[5]]
  expect_equal(get_private(single_obj)$neighbSt[4], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 4 for newState 3 position 3")
  
  ## Structure of length 5: change seq state position 4, update neighbSt positions 3 and 5
  single_obj <- singleStructureGenerator$new("U", 5)
  single_obj$modify_seqPos(position = 4, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(4)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[2], 1]
  expect_equal(get_private(single_obj)$neighbSt[3], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 3 for newState 1 position 4")
  expect_equal(get_private(single_obj)$neighbSt[5], 1,
               info = "singleStr of length 5 assigns wrong neighbSt to position 5 for newState 1 position 4")
  single_obj$modify_seqPos(position = 4, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(4)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[2], 2]
  expect_equal(get_private(single_obj)$neighbSt[3], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 3 for newState 2 position 4")
  expect_equal(get_private(single_obj)$neighbSt[5], 5,
               info = "singleStr of length 5 assigns wrong neighbSt to position 5 for newState 2 position 4")
  single_obj$modify_seqPos(position = 4, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(4)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[2], 3]
  expect_equal(get_private(single_obj)$neighbSt[3], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 3 for newState 3 position 4")
  expect_equal(get_private(single_obj)$neighbSt[5], 9,
               info = "singleStr of length 5 assigns wrong neighbSt to position 5 for newState 3 position 4")
  
  ## Structure of length 5: change seq state position 5, update neighbSt position 4
  single_obj <- singleStructureGenerator$new("U", 5)
  single_obj$modify_seqPos(position = 5, newState = 1)
  get_private(single_obj)$update_intraStr_neighbSt(5)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[3], 1]
  expect_equal(get_private(single_obj)$neighbSt[4], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 4 for newState 1 position 5")
  single_obj$modify_seqPos(position = 5, newState = 2)
  get_private(single_obj)$update_intraStr_neighbSt(5)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[3], 2]
  expect_equal(get_private(single_obj)$neighbSt[4], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 4 for newState 2 position 5")
  single_obj$modify_seqPos(position = 5, newState = 3)
  get_private(single_obj)$update_intraStr_neighbSt(5)
  exp_neighbSt <- mapNeighbSt_matrix[single_obj$get_seq()[3], 3]
  expect_equal(get_private(single_obj)$neighbSt[4], exp_neighbSt,
               info = "singleStr of length 5 assigns wrong neighbSt to position 4 for newState 3 position 5")
  
})

test_that("singleStructureGenerator $update_neighbSt() incorrect input", {
  single_obj <- singleStructureGenerator$new("U",13)
  
  # Test cases: incorrect input
  expect_error(get_private(single_obj)$update_neighbSt("i"),
               info = "method fails to trow error with non-numeric 'position' argument")
  expect_error(get_private(single_obj)$update_neighbSt(c(1,2)),
               info = "method fails to trow error with non-numeric 'position' argument")
  expect_error(get_private(single_obj)$update_neighbSt(14),
               info = "method fails to trow error with 'position' argument bigger than sequence length")
  expect_error(get_private(single_obj)$update_neighbSt(0),
               info = "method fails to trow error with 'position' argument 0")
  expect_error(get_private(single_obj)$update_neighbSt(1.5),
               info = "method fails to trow error with 'position' argument non-index (integer)")
})

test_that("singleStructureGenerator $update_neighbSt() instance of length 1", {
  
  if (! "modify_seqPos"%in% names(singleStructureGenerator$public_methods)){
    singleStructureGenerator$set("public", "modify_seqPos", function(position, newState) {
      private$seq[position] <-newState
    })
  }
  mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3)
  
  ## Structure of length 1: change seq state position 1, update neighbSt position 1
  ## Isolated
  infoStr <- data.frame(n = c(1),
                        globalState = c("M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 1, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(1)
  exp_neighbSt <- 5 #first position only singleStr counts as both neighbors
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "isolated singleStr of length 1 assigns wrong neighbSt to self")
  # Check that when there is no neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(1))$neighbSt), 1,
               info = "isolated structure. No neighbor. length of neighbSt not equal to $seq length")
  
  ## First structure. One right singleStr of length 1
  infoStr <- data.frame(n = c(1, 1),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 1, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(1)
  exp_neighbSt <- 5 
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "first singleStr of length 1 one right singleStr of length 1 assigns wrong neighbSt to right neighb")
  # Check that when there is no left neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(1))$neighbSt), 1,
               info = "First structure. No left neighbor. length of neighbSt not equal to $seq length")
  
  ## First structure. One right singleStr of length 2
  infoStr <- data.frame(n = c(1, 2),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 1, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(2)$get_seq()[2]]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "first singleStr of length 1 one right singleStr of length 2 assigns wrong neighbSt to right neighb")
  # Check that when there is no left neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(1))$neighbSt), 1,
               info = "First structure. No left neighbor. length of neighbSt not equal to $seq length")
  
  ## First structure. Two right singleStr of length 1 each
  infoStr <- data.frame(n = c(1,1,1),
                        globalState = c("M", "M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 1, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(3)$get_seq()[1]]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "first singleStr of length 1 Two right singleStr of length 1 each assigns wrong neighbSt to right neighb")
  # Check that when there is no left neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(1))$neighbSt), 1,
               info = "First structure. No left neighbor. length of neighbSt not equal to $seq length")
  
  
  ## Second structure. One left singleStr of length 1
  infoStr <- data.frame(n = c(1, 1),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(2)$modify_seqPos(position = 1, newState = 2)
  get_private(combi_obj$get_singleStr(2))$update_neighbSt(1)
  exp_neighbSt <- 5 
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "second singleStr of length 1 one left singleStr of length 1 assigns wrong neighbSt to left neighb")
  # Check that when there is no right neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(2))$neighbSt), 1,
               info = "Second structure. No right neighbor. length of neighbSt not equal to $seq length")
  
  ## Second structure. One left singleStr of length 2
  infoStr <- data.frame(n = c(2, 1),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(2)$modify_seqPos(position = 1, newState = 2)
  get_private(combi_obj$get_singleStr(2))$update_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "second singleStr of length 1 one left singleStr of length 2 assigns wrong neighbSt to left neighb")
  # Check that when there is no right neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(2))$neighbSt), 1,
               info = "Second structure. No right neighbor. length of neighbSt not equal to $seq length")
})

test_that("singleStructureGenerator $update_neighbSt() instance of length 2 change seq state position 1", {
  
  if (! "modify_seqPos"%in% names(singleStructureGenerator$public_methods)){
    singleStructureGenerator$set("public", "modify_seqPos", function(position, newState) {
      private$seq[position] <-newState
    })
  }
  mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3)
  
  ## Isolated: change seq state position 1, update neighbSt position 2
  infoStr <- data.frame(n = c(2),
                        globalState = c("M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 1, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(1)
  exp_neighbSt <- 5 
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "isolated singleStr assigns wrong right neighbSt")
  # Check that when there is no left neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(1))$neighbSt), 2,
               info = "isolated structure. No left neighbor. length of neighbSt not equal to $seq length")
  
  ## First structure. One right singleStr of length 1
  infoStr <- data.frame(n = c(2, 1),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 1, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(2)$get_seq()[1]]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "First structure. One right singleStr of length 1")
  # Check that when there is no left neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(1))$neighbSt), 2,
               info = "First structure. No left neighbor. length of neighbSt not equal to $seq length")
  
  ## Second structure. One left singleStr of length 1
  infoStr <- data.frame(n = c(1, 2),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(2)$modify_seqPos(position = 1, newState = 2)
  get_private(combi_obj$get_singleStr(2))$update_neighbSt(1)
  exp_neighbSt <- 5 
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "Second structure. One left singleStr of length 1, update left")
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[2], exp_neighbSt,
               info = "Second structure. One left singleStr of length 1, update right")
  
  ## Second structure. One left singleStr of length 2
  infoStr <- data.frame(n = c(2, 2),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(2)$modify_seqPos(position = 1, newState = 2)
  get_private(combi_obj$get_singleStr(2))$update_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "Second structure. One left singleStr of length 2, update left")
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[2], 5,
               info = "Second structure. One left singleStr of length 2, update right")

})

test_that("singleStructureGenerator $update_neighbSt() instance of length 3 change seq state position 1", {
  
  if (! "modify_seqPos"%in% names(singleStructureGenerator$public_methods)){
    singleStructureGenerator$set("public", "modify_seqPos", function(position, newState) {
      private$seq[position] <-newState
    })
  }
  mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3)
  
  ## Isolated: change seq state position 1, update neighbSt position 2
  infoStr <- data.frame(n = c(3),
                        globalState = c("M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 1, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(1)$get_seq()[3]]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "isolated singleStr assigns wrong right neighbSt")
  # Check that when there is no left neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(1))$neighbSt), 3,
               info = "isolated structure. No left neighbor. length of neighbSt not equal to $seq length")
  
  ## First structure. One right singleStr of length 1
  infoStr <- data.frame(n = c(3, 1),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 1, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(1)$get_seq()[3]]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "First structure. One right singleStr of length 1")
  # Check that when there is no left neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(1))$neighbSt), 3,
               info = "First structure. No left neighbor. length of neighbSt not equal to $seq length")
  
  ## Second structure. One left singleStr of length 1
  infoStr <- data.frame(n = c(1, 3),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(2)$modify_seqPos(position = 1, newState = 2)
  get_private(combi_obj$get_singleStr(2))$update_neighbSt(1)
  exp_neighbSt <- 5 
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "Second structure. One left singleStr of length 1, update left")
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(2)$get_seq()[3]]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[2], exp_neighbSt,
               info = "Second structure. One left singleStr of length 1, update right")
  
  ## Second structure. One left singleStr of length 2
  infoStr <- data.frame(n = c(2, 3),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(2)$modify_seqPos(position = 1, newState = 2)
  get_private(combi_obj$get_singleStr(2))$update_neighbSt(1)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "Second structure. One left singleStr of length 2, update left")
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(2)$get_seq()[3]]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[2], exp_neighbSt,
               info = "Second structure. One left singleStr of length 2, update right")
  
})

test_that("singleStructureGenerator $update_neighbSt() instance of length 2 change seq state position 2", {
  
  if (! "modify_seqPos"%in% names(singleStructureGenerator$public_methods)){
    singleStructureGenerator$set("public", "modify_seqPos", function(position, newState) {
      private$seq[position] <-newState
    })
  }
  mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3)
  
  ## Isolated: change seq state position 2, update neighbSt position 1
  infoStr <- data.frame(n = c(2),
                        globalState = c("M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 2, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(2)
  exp_neighbSt <- 5 
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "isolated singleStr assigns wrong left neighbSt")
  # Check that when there is no right neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(1))$neighbSt), 2,
               info = "isolated singleStr. length of neighbSt not equal to $seq length")
  
  ## First structure. One right singleStr of length 1
  infoStr <- data.frame(n = c(2, 1),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 2, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(2)
  exp_neighbSt <- 5
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "First structure. One right singleStr of length 1, update left")
  exp_neighbSt <- 5
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "First structure. One right singleStr of length 1, update right")
  
  ## First structure. One right singleStr of length 2
  infoStr <- data.frame(n = c(2, 2),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 2, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(2)
  exp_neighbSt <- 5
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "First structure. One right singleStr of length 2, update left")
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(2)$get_seq()[2]]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "First structure. One right singleStr of length 2, update right")
  
  ## Second structure. One left singleStr of length 1
  infoStr <- data.frame(n = c(1, 2),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(2)$modify_seqPos(position = 2, newState = 2)
  get_private(combi_obj$get_singleStr(2))$update_neighbSt(2)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "Second structure. One left singleStr of length 1, update left")
  # Check that when there is no right neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(2))$neighbSt), 2,
               info = "Second structure. No right neighbor. length of neighbSt not equal to $seq length")
  
  
})

test_that("singleStructureGenerator $update_neighbSt() instance of length 3 change seq state position 2", {
  
  if (! "modify_seqPos"%in% names(singleStructureGenerator$public_methods)){
    singleStructureGenerator$set("public", "modify_seqPos", function(position, newState) {
      private$seq[position] <-newState
    })
  }
  mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3)
  
  ## Isolated: change seq state position 2, update neighbSt position 1 and 3
  infoStr <- data.frame(n = c(3),
                        globalState = c("M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 2, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(2)
  exp_neighbSt <- 5
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "isolated singleStr assigns wrong left neighbSt")
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[3], exp_neighbSt,
               info = "isolated singleStr assigns wrong right neighbSt")
  
  
  ## First structure. One right singleStr of length 1
  infoStr <- data.frame(n = c(3, 1),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 2, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(2)
  exp_neighbSt <- 5
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "First structure. One right singleStr of length 1, update left")
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(2)$get_seq()[1]]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[3], exp_neighbSt,
               info = "First structure. One right singleStr of length 1, update right")
  
  
  ## Second structure. One left singleStr of length 1
  infoStr <- data.frame(n = c(1, 3),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(2)$modify_seqPos(position = 2, newState = 2)
  get_private(combi_obj$get_singleStr(2))$update_neighbSt(2)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "Second structure. One left singleStr of length 1, update left")
  exp_neighbSt <- 5
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[3], exp_neighbSt,
               info = "Second structure. One left singleStr of length 1, update right")
  
})

test_that("singleStructureGenerator $update_neighbSt() instance of length 4 change seq state position 2", {
  
  if (! "modify_seqPos"%in% names(singleStructureGenerator$public_methods)){
    singleStructureGenerator$set("public", "modify_seqPos", function(position, newState) {
      private$seq[position] <-newState
    })
  }
  mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3)
  
  ## Isolated: change seq state position 2, update neighbSt position 1 and 3
  infoStr <- data.frame(n = c(4),
                        globalState = c("M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 2, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(2)
  exp_neighbSt <- 5
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "isolated singleStr assigns wrong left neighbSt")
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(1)$get_seq()[4]]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[3], exp_neighbSt,
               info = "isolated singleStr assigns wrong right neighbSt")
  
  
  ## First structure. One right singleStr of length 1
  infoStr <- data.frame(n = c(4, 1),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 2, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(2)
  exp_neighbSt <- 5
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[1], exp_neighbSt,
               info = "First structure. One right singleStr of length 1, update left")
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(1)$get_seq()[4]]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[3], exp_neighbSt,
               info = "First structure. One right singleStr of length 1, update right")
  
  
  ## Second structure. One left singleStr of length 1
  infoStr <- data.frame(n = c(1, 4),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(2)$modify_seqPos(position = 2, newState = 2)
  get_private(combi_obj$get_singleStr(2))$update_neighbSt(2)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "Second structure. One left singleStr of length 1, update left")
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(2)$get_seq()[4]]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[3], exp_neighbSt,
               info = "Second structure. One left singleStr of length 1, update right")
  
})

test_that("singleStructureGenerator $update_neighbSt() instance of length 3 change seq state position 3", {
  
  if (! "modify_seqPos"%in% names(singleStructureGenerator$public_methods)){
    singleStructureGenerator$set("public", "modify_seqPos", function(position, newState) {
      private$seq[position] <-newState
    })
  }
  mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3)
  
  ## Isolated: change seq state position 3, update neighbSt position 2
  infoStr <- data.frame(n = c(3),
                        globalState = c("M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 3, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "isolated singleStr assigns wrong left neighbSt")
  # Check that when there is no right neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(1))$neighbSt), 3,
               info = "isolated structure. No right neighbor. length of neighbSt not equal to $seq length")
  
  ## Second structure. One left singleStr of length 1
  infoStr <- data.frame(n = c(1, 3),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(2)$modify_seqPos(position = 3, newState = 2)
  get_private(combi_obj$get_singleStr(2))$update_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(2)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[2], exp_neighbSt,
               info = "Second structure. One left singleStr of length 1, update left")
  # Check that when there is no right neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(2))$neighbSt), 3,
               info = "Second structure. No right neighbor. length of neighbSt not equal to $seq length")
  
  ## First structure. One right singleStr of length 1
  infoStr <- data.frame(n = c(3, 1),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 3, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "First structure. One right singleStr of length 1, update left")
  exp_neighbSt <- 5
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "First structure. One right singleStr of length 1, update right")
  
  ## First structure. One right singleStr of length 2
  infoStr <- data.frame(n = c(3, 2),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 3, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "First structure. One right singleStr of length 1, update left")
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(2)$get_seq()[2]]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "First structure. One right singleStr of length 2, update right")
  
  ## First structure. Two right singleStr of length 1 each
  infoStr <- data.frame(n = c(3, 1, 1),
                        globalState = c("M", "M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 3, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "First structure. One right singleStr of length 1, update left")
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(3)$get_seq()[1]]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "First structure. Two right singleStr of length 1, update right")
  
})

test_that("singleStructureGenerator $update_neighbSt() instance of length 4 change seq state position 3", {
  
  if (! "modify_seqPos"%in% names(singleStructureGenerator$public_methods)){
    singleStructureGenerator$set("public", "modify_seqPos", function(position, newState) {
      private$seq[position] <-newState
    })
  }
  mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3)
  
  ## Isolated: change seq state position 3, update neighbSt position 2 and 4
  infoStr <- data.frame(n = c(4),
                        globalState = c("M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 3, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "isolated singleStr assigns wrong left neighbSt")
  exp_neighbSt <- 5
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[4], exp_neighbSt,
               info = "isolated singleStr assigns wrong right neighbSt")
  
  ## Second structure. One left singleStr of length 1
  infoStr <- data.frame(n = c(1, 4),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(2)$modify_seqPos(position = 3, newState = 2)
  get_private(combi_obj$get_singleStr(2))$update_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(2)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[2], exp_neighbSt,
               info = "Second structure. One left singleStr of length 1, update left")
  exp_neighbSt <- 5
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[4], exp_neighbSt,
               info = "Second structure. One left singleStr of length 1, update right")
  
  ## First structure. One right singleStr of length 1
  infoStr <- data.frame(n = c(4, 1),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 3, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "First structure. One right singleStr of length 1, update left")
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(2)$get_seq()[1]]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[4], exp_neighbSt,
               info = "First structure. One right singleStr of length 1, update right")
  
})

test_that("singleStructureGenerator $update_neighbSt() instance of length 4 change seq state position 4", {
  
  if (! "modify_seqPos"%in% names(singleStructureGenerator$public_methods)){
    singleStructureGenerator$set("public", "modify_seqPos", function(position, newState) {
      private$seq[position] <-newState
    })
  }
  mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3)
  
  ## Isolated: change seq state position 4, update neighbSt position 3
  infoStr <- data.frame(n = c(4),
                        globalState = c("M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 4, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(4)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[2], 2]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[3], exp_neighbSt,
               info = "isolated singleStr assigns wrong left neighbSt")
  # Check that when there is no right neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(1))$neighbSt), 4,
               info = "isolated structure. No right neighbor. length of neighbSt not equal to $seq length")
  
  ## Second structure. One left singleStr of length 1
  infoStr <- data.frame(n = c(1, 4),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(2)$modify_seqPos(position = 4, newState = 2)
  get_private(combi_obj$get_singleStr(2))$update_neighbSt(4)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(2)$get_seq()[2], 2]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[3], exp_neighbSt,
               info = "Second structure. One left singleStr of length 1, update left")
  # Check that when there is no right neighbor, the length of the neighbSt encoding is equal to seq length
  expect_equal(length(get_private(combi_obj$get_singleStr(2))$neighbSt), 4,
               info = "Second structure. No right neighbor. length of neighbSt not equal to $seq length")
  
  ## First structure. One right singleStr of length 1
  infoStr <- data.frame(n = c(4, 1),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 4, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(4)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[2], 2]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[3], exp_neighbSt,
               info = "First structure. One right singleStr of length 1, update left")
  exp_neighbSt <- 5
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "First structure. One right singleStr of length 1, update right")
  
  ## First structure. One right singleStr of length 2
  infoStr <- data.frame(n = c(4, 2),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 4, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(4)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[2], 2]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[3], exp_neighbSt,
               info = "First structure. One right singleStr of length 2, update left")
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(2)$get_seq()[2]]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "First structure. One right singleStr of length 2, update right")
  
  ## First structure. Two right singleStr of length 1 each
  infoStr <- data.frame(n = c(4, 1, 1),
                        globalState = c("M", "M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 4, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(4)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[2], 2]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[3], exp_neighbSt,
               info = "First structure. Two right singleStr of length 1 each, update left")
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(3)$get_seq()[1]]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[1], exp_neighbSt,
               info = "First structure. Two right singleStr of length 1 each, update right")
})

test_that("singleStructureGenerator $update_neighbSt() seqlength > 4 intermediate positions", {
  
  if (! "modify_seqPos"%in% names(singleStructureGenerator$public_methods)){
    singleStructureGenerator$set("public", "modify_seqPos", function(position, newState) {
      private$seq[position] <-newState
    })
  }
  mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3)
  
  ## Isolated: change seq state position 3, update neighbSt position 2 and 4
  infoStr <- data.frame(n = c(5),
                        globalState = c("M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(1)$modify_seqPos(position = 3, newState = 2)
  get_private(combi_obj$get_singleStr(1))$update_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(1)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[2], exp_neighbSt,
               info = "isolated singleStr assigns wrong left neighbSt")
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(1)$get_seq()[5]]
  expect_equal(get_private(combi_obj$get_singleStr(1))$neighbSt[4], exp_neighbSt,
               info = "isolated singleStr assigns wrong right neighbSt")
  
  
  ## Second structure. One left singleStr of length 1
  infoStr <- data.frame(n = c(1, 5),
                        globalState = c("M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  combi_obj$get_singleStr(2)$modify_seqPos(position = 3, newState = 2)
  get_private(combi_obj$get_singleStr(2))$update_neighbSt(3)
  exp_neighbSt <- mapNeighbSt_matrix[combi_obj$get_singleStr(2)$get_seq()[1], 2]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[2], exp_neighbSt,
               info = "Second structure. One left singleStr of length 1, update left")
  exp_neighbSt <- mapNeighbSt_matrix[2, combi_obj$get_singleStr(2)$get_seq()[5]]
  expect_equal(get_private(combi_obj$get_singleStr(2))$neighbSt[4], exp_neighbSt,
               info = "isolated singleStr assigns wrong right neighbSt")
  
})



test_that("singleStructureGenerator init_Ri_values()",{
  # singleStructure instance
  single_obj <- singleStructureGenerator$new("U", 5)
  iota <- get_private(single_obj)$iota
  expect_equal(mean(get_private(single_obj)$Ri_values), iota,
               info ="mean Ri value does not correspond to iota in isolated singleStructure instance")
  expect_equal(length(get_private(single_obj)$Ri_values), 3,
               info = "incorrect number of Ri values in isolated singleStructure instance")
  # combiStructure instance
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  for (i in 1:nrow(infoStr)){
    expect_equal(mean(get_private(combi_obj$get_singleStr(i))$Ri_values), iota,
                 info ="mean Ri value does not correspond to iota in singleStructure instance within combiStructure")
    expect_equal(length(get_private(combi_obj$get_singleStr(i))$Ri_values), 3,
                 info = "incorrect number of Ri values in singleStructure instance within combiStructure")
  }
})

test_that("singleStructureGenerator init_Rc_values()", {
  # singleStructure instance
  single_obj <- singleStructureGenerator$new("U", 5)
  iota <- get_private(single_obj)$iota
  expect_equal(sum(get_private(single_obj)$Rc_values$Rcl, get_private(single_obj)$Rc_values$Rcr), 1-iota,
               info ="sum of Rc values does not correspond to 1 - iota in isolated singleStructure instance")
  expect_equal(length(get_private(single_obj)$Rc_values), 2,
               info ="incorrect number of Rc values in isolated singleStructure instance")
  # combiStructure instance
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  for (i in 1:nrow(infoStr)){
    expect_equal(sum(get_private(combi_obj$get_singleStr(i))$Rc_values$Rcl, get_private(combi_obj$get_singleStr(i))$Rc_values$Rcr), 1-iota,
                 info ="mean Ri value does not correspond to iota in singleStructure instance within combiStructure")
    expect_equal(length(get_private(combi_obj$get_singleStr(i))$Rc_values), 2,
                 info = "incorrect number of Ri values in singleStructure instance within combiStructure")
  }
})

test_that("singleStructureGenerator set_Qi()", {
  # singleStructure instance
  ## Check Qi structure
  obj <- singleStructureGenerator$new("U", 10)
  Qi_list <- get_private(obj)$Qi
  expect_true(is.list(Qi_list), info = "does not generate list in isolated singleStructure instance")
  expect_equal(length(Qi_list), 3, info = "does not generate correct number of rate matrices in isolated singleStructure instance")
  expect_true(all(is.matrix(Qi_list[[1]]), is.matrix(Qi_list[[2]]), is.matrix(Qi_list[[3]])), info = "elements are not matrices in isolated singleStructure instance")
  expect_equal(c(length(Qi_list[[1]]), length(Qi_list[[2]]), length(Qi_list[[3]])), c(9, 9, 9), info = "matrices are not of correct length in isolated singleStructure instance")
  ## Check Qi properties (rate matrices)
  validationStates <- listRateMatrix_validation(Qi_list, "set_Qi() test multiRegion_SIM.R in isolated singleStructure instance")
  output <- listMatrices_validationResults(validationStates)
  expect_null(output, info = "validation properties of Qi not met in isolated singleStructure instance")

  # combiStructure instance
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  ## Check Qi structure
  for (i in 1:nrow(infoStr)){
    Qi_list <- get_private(combi_obj$get_singleStr(i))$Qi
    expect_true(is.list(Qi_list), info = "does not generate listin singleStructure instance within combiStructure")
    expect_equal(length(Qi_list), 3, info = "does not generate correct number of rate matrices in singleStructure instance within combiStructure")
    expect_true(all(is.matrix(Qi_list[[1]]), is.matrix(Qi_list[[2]]), is.matrix(Qi_list[[3]])), info = "elements are not matrices in singleStructure instance within combiStructure")
    expect_equal(c(length(Qi_list[[1]]), length(Qi_list[[2]]), length(Qi_list[[3]])), c(9, 9, 9), info = "matrices are not of correct length in singleStructure instance within combiStructure")
  }
  ## Check Qi properties (rate matrices)
  for (i in 1:nrow(infoStr)){
    Qi_list <- get_private(combi_obj$get_singleStr(i))$Qi
    validationStates <- listRateMatrix_validation(Qi_list, "set_Qi() test multiRegion_SIM.R in singleStructure instance within combiStructure")
    output <- listMatrices_validationResults(validationStates)
    expect_null(output, info = "validation properties of Qi not met in singleStructure instance within combiStructure")
  }
})

test_that("singleStructureGenerator set_Qc()", {
  # singleStructure instance
  ## Check Qc structure
  obj <- singleStructureGenerator$new("U", 10)
  Qc_list <- get_private(obj)$Qc
  expect_true(is.list(Qc_list), info = "does not generate list in isolated singleStructure instance")
  expect_equal(length(Qc_list), 9, info = "does not generate correct number of rate matrices in isolated singleStructure instance")
  expect_true(all(is.matrix(Qc_list[[1]]), is.matrix(Qc_list[[2]]), is.matrix(Qc_list[[3]]),
                  is.matrix(Qc_list[[4]]), is.matrix(Qc_list[[5]]), is.matrix(Qc_list[[6]]),
                  is.matrix(Qc_list[[7]]), is.matrix(Qc_list[[8]]), is.matrix(Qc_list[[9]])),
              info = "elements are not matrices in isolated singleStructure instance")
  expect_equal(c(length(Qc_list[[1]]), length(Qc_list[[2]]), length(Qc_list[[3]]),
                 length(Qc_list[[4]]), length(Qc_list[[5]]), length(Qc_list[[6]]),
                 length(Qc_list[[7]]), length(Qc_list[[8]]), length(Qc_list[[9]])),
               rep(9, 9), info = "matrices are not of correct length in isolated singleStructure instance")
  ## Check Qc properties (rate matrices)
  validationStates <- listRateMatrix_validation(Qc_list, "set_Qc() test multiRegion_SIM.R in isolated singleStructure instance")
  output <- listMatrices_validationResults(validationStates)
  expect_null(output, info = "validation properties of Qc not met in isolated singleStructure instance")

  # combiStructure instance
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  ## Check Qc structure
  for (i in 1:nrow(infoStr)){
    Qc_list <- get_private(combi_obj$get_singleStr(i))$Qc
    expect_true(is.list(Qc_list), info = "does not generate list in singleStructure instance within combiStructure")
    expect_equal(length(Qc_list), 9, info = "does not generate correct number of rate matrices in singleStructure instance within combiStructure")
    expect_true(all(is.matrix(Qc_list[[1]]), is.matrix(Qc_list[[2]]), is.matrix(Qc_list[[3]]),
                    is.matrix(Qc_list[[4]]), is.matrix(Qc_list[[5]]), is.matrix(Qc_list[[6]]),
                    is.matrix(Qc_list[[7]]), is.matrix(Qc_list[[8]]), is.matrix(Qc_list[[9]])),
                info = "elements are not matrices in singleStructure instance within combiStructure")
    expect_equal(c(length(Qc_list[[1]]), length(Qc_list[[2]]), length(Qc_list[[3]]),
                   length(Qc_list[[4]]), length(Qc_list[[5]]), length(Qc_list[[6]]),
                   length(Qc_list[[7]]), length(Qc_list[[8]]), length(Qc_list[[9]])),
                 rep(9, 9), info = "matrices are not of correct length in singleStructure instance within combiStructure")
  }
  ## Check Qc properties (rate matrices)
  for (i in 1:nrow(infoStr)){
    Qc_list <- get_private(combi_obj$get_singleStr(i))$Qc
    validationStates <- listRateMatrix_validation(Qc_list, "set_Qc() test multiRegion_SIM.R in singleStructure instance within combiStructure")
    output <- listMatrices_validationResults(validationStates)
    expect_null(output, info = "validation properties of Qc not met in singleStructure instance within combiStructure")
  }
})

test_that("singleStructureGenerator set_Q()", {
  # singleStructure instance
  ## Check Q structure
  obj <- singleStructureGenerator$new("U", 10)
  Q_list <- get_private(obj)$Q
  expect_true(is.list(Q_list), info = "does not generate list in isolated singleStructure instance")
  expect_true(is.list(Q_list[[1]]), info = "does not generate nested list in isolated singleStructure instance")
  expect_true(is.list(Q_list[[2]]), info = "does not generate nested list in isolated singleStructure instance")
  expect_true(is.list(Q_list[[3]]), info = "does not generate nested list in isolated singleStructure instance")
  expect_equal(length(Q_list), 3, info = "does not generate correct number of nested lists in isolated singleStructure instance")
  expect_equal(length(Q_list[[1]]), 9, info = "does not generate correct number of rate matrices in nested list in isolated singleStructure instance")
  expect_equal(length(Q_list[[2]]), 9, info = "does not generate correct number of rate matrices in nested list in isolated singleStructure instance")
  expect_equal(length(Q_list[[3]]), 9, info = "does not generate correct number of rate matrices in nested list in isolated singleStructure instance")
  expect_true(all(is.matrix(Q_list[[1]][[9]]), is.matrix(Q_list[[2]][[5]]), is.matrix(Q_list[[3]][[1]])), info = "elements are not matrices in isolated singleStructure instance")
  ## Check Q properties (rate matrices)
  for (Ri in 1:3){
    validationStates <- listRateMatrix_validation(Q_list[[Ri]], "set_Q() test multiRegion_SIM.R in isolated singleStructure instance")
    output <- listMatrices_validationResults(validationStates)
    expect_null(output, info = "validation properties of Q not met in isolated singleStructure instance")
  }

  # combiStructure instance
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  for (i in 1:nrow(infoStr)){
    ## Check Q structure
    Q_list <- get_private(combi_obj$get_singleStr(i))$Q
    expect_true(is.list(Q_list), info = "does not generate list in singleStructure instance within combiStructure")
    expect_true(is.list(Q_list[[1]]), info = "does not generate nested list in singleStructure instance within combiStructure")
    expect_true(is.list(Q_list[[2]]), info = "does not generate nested list in singleStructure instance within combiStructure")
    expect_true(is.list(Q_list[[3]]), info = "does not generate nested list in singleStructure instance within combiStructure")
    expect_equal(length(Q_list), 3, info = "does not generate correct number of nested lists in singleStructure instance within combiStructure")
    expect_equal(length(Q_list[[1]]), 9, info = "does not generate correct number of rate matrices in nested list in singleStructure instance within combiStructure")
    expect_equal(length(Q_list[[2]]), 9, info = "does not generate correct number of rate matrices in nested list in singleStructure instance within combiStructure")
    expect_equal(length(Q_list[[3]]), 9, info = "does not generate correct number of rate matrices in nested list in singleStructure instance within combiStructure")
    expect_true(all(is.matrix(Q_list[[1]][[9]]), is.matrix(Q_list[[2]][[5]]), is.matrix(Q_list[[3]][[1]])), info = "elements are not matrices in singleStructure instance within combiStructure")
    ## Check Q properties (rate matrices)
    for (Ri in 1:3){
      validationStates <- listRateMatrix_validation(Q_list[[Ri]], "set_Q() test multiRegion_SIM.R in singleStructure instance within combiStructure")
      output <- listMatrices_validationResults(validationStates)
      expect_null(output, info = "validation properties of Q not met in singleStructure instance within combiStructure")
    }
  }
})

test_that("singleStructureGenerator initialize_ratetree()", {
  # singleStructure instance
  ### a) with sequence of long length
  # Check that all $ratetree levels sum the same total rate after initialize_ratetree
  obj <- singleStructureGenerator$new("U", 100)
  expect_equal(sd(sapply(get_private(obj)$ratetree, sum)), 0,
               info=paste("Different total rate sums in levels of ratetree after isolated singleStructure initialization a):\n", sapply(get_private(obj)$ratetree, sum)))
  ### b) with sequence of length 1
  obj <- singleStructureGenerator$new("U", 1)
  expect_equal(length(get_private(obj)$ratetree), 1,
               info=paste("Incorrect length of ratetree after isolated singleStructure initialization b):\n", sapply(get_private(obj)$ratetree, sum)))
  expect_true(is.numeric(get_private(obj)$ratetree[[1]]),
              info = "ratetree initialization does not output numeric rate after isolated singleStructure initialization b)")
  ### c) with sequence of length 2
  # Check that all $ratetree levels sum the same total rate after initialize_ratetree
  obj <- singleStructureGenerator$new("U", 2)
  expect_equal(sd(sapply(get_private(obj)$ratetree, sum)), 0,
               info=paste("Different total rate sums in levels of ratetree after isolated singleStructure initialization c):\n", sapply(get_private(obj)$ratetree, sum)))
  ### d) with equilibrium frequencies as all methylated or all unmethylated the total rate should be 0
  obj <- singleStructureGenerator$new(globalState = "U", 100, eqFreqs = c(1,0,0))
  expect_equal(get_private(obj)$ratetree[[1]][1], 0,
               info = paste("Completely unmethylated sequence has total rate different from 0. ratetree[[1]]:", get_private(obj)$ratetree[[1]]))
  obj <- singleStructureGenerator$new(globalState = "M", 100, eqFreqs = c(0,0,1))
  expect_equal(get_private(obj)$ratetree[[1]][1], 0,
               info = paste("Completely methylated sequence has total rate different from 0. ratetree[[1]]:", get_private(obj)$ratetree[[1]]))

  # combiStructure instance
  ### a) with sequences of long length
  # Check that all $ratetree levels sum the same total rate after initialize_ratetree
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  for (i in 1:nrow(infoStr)){
    expect_equal(sd(sapply(get_private(combi_obj$get_singleStr(i))$ratetree, sum)), 0,
                 info=paste("Different total rate sums in levels of ratetree after singleStructure instance within combiStructure initialization:\n", sapply(get_private(obj)$ratetree, sum)))
  }
  ### b) with sequence of length 1
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_equal(length(get_private(combi_obj$get_singleStr(2))$ratetree), 1,
               info=paste("Incorrect length of ratetree after singleStructure instance within combiStructure initialization b):\n", sapply(get_private(obj)$ratetree, sum)))
  expect_true(is.numeric(get_private(combi_obj$get_singleStr(2))$ratetree[[1]]),
              info = "ratetree initialization does not output numeric rate after singleStructure instance within combiStructure initialization b)")
  ### c) with sequence of length 2
  # Check that all $ratetree levels sum the same total rate after initialize_ratetree
  infoStr <- data.frame(n = c(13, 2, 4),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_equal(sd(sapply(get_private(combi_obj$get_singleStr(2))$ratetree, sum)), 0,
               info=paste("Different total rate sums in levels of ratetree after singleStructure instance within combiStructure initialization c):\n", sapply(get_private(obj)$ratetree, sum)))
  
  
})

test_that("singleStructureGenerator update_ratetree()", {
  # singleStructure instance
  ### a) with sequences of long length
  obj <- singleStructureGenerator$new("U", 100)
  newrate <- 0.3
  get_private(obj)$update_ratetree(1, newrate)
  expect_equal(get_private(obj)$ratetree[[length(get_private(obj)$ratetree)]][1], newrate,
               info = "fails to update rate value in isolated singleStructure instance a) first change")
  # Check that all $ratetree levels sum the same total rate after update_ratetree
  expect_equal(sd(sapply(get_private(obj)$ratetree, sum)), 0,
               info=paste("Different total rate sums in levels of ratetree after after one change in isolated singleStructure a):\n", sapply(get_private(obj)$ratetree, sum)))
  newrate <- 0.7
  get_private(obj)$update_ratetree(20, 0.7)
  expect_equal(get_private(obj)$ratetree[[length(get_private(obj)$ratetree)]][20], newrate,
               info = "fails to update rate value in isolated singleStructure instance a) second change")
  expect_equal(sd(sapply(get_private(obj)$ratetree, sum)), 0,
               info=paste("Different total rate sums in levels of ratetree after after 2 changes in isolated singleStructure a):\n", sapply(get_private(obj)$ratetree, sum)))
  ### b) with sequence of length 1
  obj <- singleStructureGenerator$new("U", 1)
  newrate <- 0.3
  get_private(obj)$update_ratetree(1, newrate)
  expect_equal(get_private(obj)$ratetree[[length(get_private(obj)$ratetree)]][1], newrate,
               info = "fails to update rate value in isolated singleStructure instance b) first change")
  newrate <- 0.7
  get_private(obj)$update_ratetree(20, 0.7)
  expect_equal(get_private(obj)$ratetree[[length(get_private(obj)$ratetree)]][20], newrate,
               info = "fails to update rate value in isolated singleStructure instance b) second change")


  # combiStructure instance
  ### a) with sequences of long length
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  get_private(combi_obj$get_singleStr(1))$update_ratetree(1, newrate)
  expect_equal(get_private(combi_obj$get_singleStr(1))$ratetree[[length(get_private(combi_obj$get_singleStr(1))$ratetree)]][1],
               newrate, info = "fails to update rate value in singleStructure instance within combiStructure")
  for (i in 1:nrow(infoStr)){
    expect_equal(sd(sapply(get_private(combi_obj$get_singleStr(i))$ratetree, sum)), 0,
                 info=paste("Different total rate sums in levels of ratetree after singleStructure instance within combiStructure update:\n", sapply(get_private(obj)$ratetree, sum)))
  }
  ### b) with sequence of length 1
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  newrate <- 0.3
  get_private(combi_obj$get_singleStr(2))$update_ratetree(1, newrate)
  expect_equal(get_private(combi_obj$get_singleStr(2))$ratetree[[1]], newrate,
               info = "fails to update rate value in singleStructure instance within combiStructure b) first change")
  newrate <- 0.7
  get_private(combi_obj$get_singleStr(2))$update_ratetree(1, 0.7)
  expect_equal(get_private(combi_obj$get_singleStr(2))$ratetree[[1]], newrate,
               info = "fails to update rate value in singleStructure instance within combiStructure b) second change")
})

test_that("singleStructureGenerator() get_Q",{
  s <- singleStructureGenerator$new("U", n = 100) 
  obj <- s$get_Q()
  expect_true(is.list(obj), info = "get_Q with null arguments does not return list")
  obj <- s$get_Q(siteR = 1, neighbSt = 1, oldSt = 1, newSt = 1)
  expect_true(is.numeric(obj), info = "get_Q with non-null arguments does not return numeric object")
  expect_equal(length(obj), 1, info = "get_Q with non-null arguments does not return one rate value")
})

test_that("singleStructureGenerator() get_siteR",{
  s <- singleStructureGenerator$new("U", n = 100) 
  obj <- s$get_siteR()
  expect_equal(length(obj), 100, info = "get_siteR with null arguments does not return vector")
  obj <- s$get_siteR(index = 1)
  expect_equal(length(obj), 1, info = "get_siteR with non-null argument does not return one siteR value")
})

test_that("singleStructureGenerator() get_neighbSt",{
  s <- singleStructureGenerator$new("U", n = 100) 
  obj <- s$get_neighbSt()
  expect_equal(length(obj), 100, info = "get_neighbSt with null arguments does not return vector")
  obj <- s$get_neighbSt(index = 1)
  expect_equal(length(obj), 1, info = "get_neighbSt with non-null argument does not return one get_neighbSt value")
})

test_that("singleStructureGenerator() update_ratetree_otherStr",{
  s <- singleStructureGenerator$new("U", n = 100) 
  old_rate <- get_private(s)$ratetree[[8]][1]
  old_total_rate <- get_private(s)$ratetree[[1]][1]
  new_rate <- old_rate + 5
  new_total_rate <- old_total_rate + 5
  s$update_ratetree_otherStr(position = 1, rate = new_rate)
  expect_equal(get_private(s)$ratetree[[8]][1], new_rate,
               info = "fails updating site rate")
  expect_equal(get_private(s)$ratetree[[1]][1], new_total_rate, 
               info = "fails updating total rate")
})

test_that("singleStructureGenerator() update_ratetree_betweenStr",{
  if (! "modify_neighbSt"%in% names(singleStructureGenerator$public_methods)){
    singleStructureGenerator$set("public", "modify_neighbSt", function(position, newState) {
      private$neighbSt[position] <-newState
    })
  }
  ## Case 1: update next Structure
  test_str <- data.frame(n = c(100,100), globalState = c("U", "M"), 
                         u_eqFreq = c(1, 0),
                         p_eqFreq = c(0, 0),
                         m_eqFreq = c(0, 1))
  c <- combiStructureGenerator$new(infoStr = test_str)
  s2 <- c$get_singleStr(2)
  siteR <- s2$get_siteR(1)
  seq <- s2$get_seq()[1]
  new_neighbSt <- 6
  s2$modify_neighbSt(position = 1, new_neighbSt)
  s1 <- c$get_singleStr(1)
  get_private(s1)$update_ratetree_betweenStr(nextStr = TRUE)
  exp_new_rate <- abs(s2$get_Q(siteR = siteR, neighbSt = new_neighbSt, oldSt = seq, newSt = seq))
  site_new_rate <- get_private(s2)$ratetree[[8]][1]
  expect_equal(site_new_rate, exp_new_rate,
               info = "fails to assign correct rate after change in neighbSt, case 1")
  
  ## Case 2: update previous Structure
  test_str <- data.frame(n = c(100,100), globalState = c("U", "M"), 
                         u_eqFreq = c(1, 0),
                         p_eqFreq = c(0, 0),
                         m_eqFreq = c(0, 1))
  c <- combiStructureGenerator$new(infoStr = test_str)
  s1 <- c$get_singleStr(1)
  siteR <- s1$get_siteR(100)
  seq <- s1$get_seq()[100]
  new_neighbSt <- 2
  s1$modify_neighbSt(position = 100, new_neighbSt)
  s2 <- c$get_singleStr(2)
  get_private(s2)$update_ratetree_betweenStr(prevStr = TRUE)
  exp_new_rate <- abs(s1$get_Q(siteR = siteR, neighbSt = new_neighbSt, oldSt = seq, newSt = seq))
  site_new_rate <- get_private(s1)$ratetree[[8]][100]
  expect_equal(site_new_rate, exp_new_rate,
               info = "fails to assign correct rate after change in neighbSt, case 2")
  
  # Check when no argument is given it throws error
  expect_error(get_private(s2)$update_ratetree_betweenStr(), info = "fails to throw an error when no argument is given")
  
  
})

test_that("singleStructureGenerator() update_ratetree_betweenStr",{
  if (! "modify_neighbSt"%in% names(singleStructureGenerator$public_methods)){
    singleStructureGenerator$set("public", "modify_neighbSt", function(position, newState) {
      private$neighbSt[position] <-newState
    })
  }
  ## Case 1: update next Structure
  test_str <- data.frame(n = c(100,100), globalState = c("U", "M"), 
                         u_eqFreq = c(1, 0),
                         p_eqFreq = c(0, 0),
                         m_eqFreq = c(0, 1))
  c <- combiStructureGenerator$new(infoStr = test_str)
  s2 <- c$get_singleStr(2)
  siteR <- s2$get_siteR(1)
  seq <- s2$get_seq()[1]
  new_neighbSt <- 6
  s2$modify_neighbSt(position = 1, new_neighbSt)
  s1 <- c$get_singleStr(1)
  get_private(s1)$update_ratetree_betweenStr(nextStr = TRUE)
  exp_new_rate <- abs(s2$get_Q(siteR = siteR, neighbSt = new_neighbSt, oldSt = seq, newSt = seq))
  site_new_rate <- get_private(s2)$ratetree[[8]][1]
  expect_equal(site_new_rate, exp_new_rate,
               info = "fails to assign correct rate after change in neighbSt, case 1")
  
  ## Case 2: update previous Structure
  test_str <- data.frame(n = c(100,100), globalState = c("U", "M"), 
                         u_eqFreq = c(1, 0),
                         p_eqFreq = c(0, 0),
                         m_eqFreq = c(0, 1))
  c <- combiStructureGenerator$new(infoStr = test_str)
  s1 <- c$get_singleStr(1)
  siteR <- s1$get_siteR(100)
  seq <- s1$get_seq()[100]
  new_neighbSt <- 2
  s1$modify_neighbSt(position = 100, new_neighbSt)
  s2 <- c$get_singleStr(2)
  get_private(s2)$update_ratetree_betweenStr(prevStr = TRUE)
  exp_new_rate <- abs(s1$get_Q(siteR = siteR, neighbSt = new_neighbSt, oldSt = seq, newSt = seq))
  site_new_rate <- get_private(s1)$ratetree[[8]][100]
  expect_equal(site_new_rate, exp_new_rate,
               info = "fails to assign correct rate after change in neighbSt, case 2")
  
  # Check when no argument is given it throws error
  expect_error(get_private(s2)$update_ratetree_betweenStr(), info = "fails to throw an error when no argument is given")
  
  
})

test_that("singleStructureGenerator() update_ratetree_allCases",{
  if (! "modify_neighbSt"%in% names(singleStructureGenerator$public_methods)){
    singleStructureGenerator$set("public", "modify_neighbSt", function(position, newState) {
      private$neighbSt[position] <-newState
    })
  }
  ## Case 1: update positions 5,100 (and 1 of second structure)
  test_str <- data.frame(n = c(100,100), globalState = c("U", "M"), 
                         u_eqFreq = c(1, 0),
                         p_eqFreq = c(0, 0),
                         m_eqFreq = c(0, 1))
  c <- combiStructureGenerator$new(infoStr = test_str)
  s1 <- c$get_singleStr(1)
  # change neighbSt position 5
  siteR_5 <- s1$get_siteR(5)
  seq_5 <- s1$get_seq()[5]
  new_neighbSt_5 <- 4
  s1$modify_neighbSt(position = 5, new_neighbSt_5)
  # change neighbSt position 100
  siteR_100 <- s1$get_siteR(100)
  seq_100 <- s1$get_seq()[100]
  new_neighbSt_100 <- 5
  s1$modify_neighbSt(position = 100, new_neighbSt_100)
  # change neighbState position 1 second structure
  s2 <- c$get_singleStr(2)
  siteR_1 <- s2$get_siteR(1)
  seq_1 <- s2$get_seq()[1]
  new_neighbSt_1 <- 9
  s2$modify_neighbSt(position = 1, new_neighbSt_1)
  get_private(s1)$update_ratetree_allCases(index = c(5,100))
  # check change position 5
  exp_new_rate <- abs(s1$get_Q(siteR = siteR_5, neighbSt = new_neighbSt_5, oldSt = seq_5, newSt = seq_5))
  site_new_rate <- get_private(s1)$ratetree[[8]][5]
  expect_equal(site_new_rate, exp_new_rate,
               info = "fails to update ratetree, position 5")
  # check change position 100
  exp_new_rate <- abs(s1$get_Q(siteR = siteR_100, neighbSt = new_neighbSt_100, oldSt = seq_100, newSt = seq_100))
  site_new_rate <- get_private(s1)$ratetree[[8]][100]
  expect_equal(site_new_rate, exp_new_rate,
               info = "fails to update ratetree, position 100")
  # check change position 1 next structure
  exp_new_rate <- abs(s2$get_Q(siteR = siteR_1, neighbSt = new_neighbSt_1, oldSt = seq_1, newSt = seq_1))
  site_new_rate <- get_private(s2)$ratetree[[8]][1]
  expect_equal(site_new_rate, exp_new_rate,
               info = "fails to update ratetree, position 1 next structure")
})

test_that("singleStructureGenerator() choose_random_seqpos",{
  obj <- singleStructureGenerator$new("U", 100)
  # Check chosen seqposition is correct
  # with long$seq length
  v <- get_private(obj)$choose_random_seqpos(testing=TRUE)
  index <- v[1]
  sampled_r <- v[2]
  rateTree <- get_private(obj)$ratetree
  rates <- rateTree[[length(rateTree)]]
  expect_true(index == 1 | sum(rates[1:(index-1)]) < sampled_r,
              info = paste("Chosen index too large in long isolated singleStr instance.\n Sum of previous rates:", sum(rates[1:(index-1)]), "Sampled index:", sampled_r))
  expect_true(sampled_r <= sum(rates[1:index]),
              info = paste("Chosen index too small in long isolated singleStr instance.\n Sum of rates from 1 to index:", sum(rates[index:length(rates)]), "Sampled index:", sampled_r))
  # with $seq length of 1
  obj <- singleStructureGenerator$new("U", 1)
  v <- get_private(obj)$choose_random_seqpos(testing=TRUE)
  index <- v[1]
  sampled_r <- v[2]
  rateTree <- get_private(obj)$ratetree
  rate <- rateTree[[length(rateTree)]]
  expect_equal(index, 1,
              info = paste("Chosen index is not only $seq index in isolated singleStr instance of length 1.\n Sum of rates from 1 to index:", sum(rates[index:length(rates)]), "Sampled index:", sampled_r))
})

test_that("singleStructureGenerator choose_number_of_changes()",{
  obj <- singleStructureGenerator$new("U", 100)
  # Check chosen number of changes is integer
  c <- get_private(obj)$choose_number_of_changes(dt = 0.01)
  expect_true(is.integer(c))
  ## If sequence is totally unmethylated or methylated the total rate of change is 0, no changes should be sampled
  obj <- singleStructureGenerator$new(globalState = "U", n = 100, eqFreqs = c(1,0,0))
  dt_possibilities <- seq(from=0.01, to  = 10, by =0.1)
  for(dt in 1:length(dt_possibilities)){
    expect_equal(get_private(obj)$choose_number_of_changes(dt = dt), 0,
                 info = paste("sampled number of changes non-0 for sequence with rate 0"))
  }
  
})

test_that("combiStructureGenerator $get_sharedCounter and $reset_sharedCounter", {
  # Initiate an instance
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  # Reset shared counter
  c$reset_sharedCounter()
  expect_equal(c$get_sharedCounter(), 0)
  c <- combiStructureGenerator$new(infoStr)
  expect_equal(c$get_sharedCounter(), 1)
  c <- combiStructureGenerator$new(infoStr)
  c <- combiStructureGenerator$new(infoStr)
  c <- combiStructureGenerator$new(infoStr)
  expect_equal(c$get_sharedCounter(), 4)
  c$reset_sharedCounter()
  expect_equal(c$get_sharedCounter(), 0)
})

test_that("combiStructureGenerator $get_id()", {
  
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  # Reset shared counter
  c <- combiStructureGenerator$new(infoStr)
  c$reset_sharedCounter()
  # Initiate an instance
  c <- combiStructureGenerator$new(infoStr)
  expect_equal(c$get_id(), 1,
               info = "method returns incorrect value after initialization")
  c <- combiStructureGenerator$new(infoStr)
  expect_equal(c$get_id(), 2,
               info = "method returns incorrect value after 2nd initialization")
  
  # Reset shared counter
  c$reset_sharedCounter()
})

test_that("combiStructureGenerator $set_id()", {
  
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  # Reset shared counter
  c <- combiStructureGenerator$new(infoStr)
  c$reset_sharedCounter()
  # Initiate an instance
  c <- combiStructureGenerator$new(infoStr)
  expect_equal(c$get_id(), 1,
               info = "incorrect id value after initialization")
  clone_c <- c$copy()
  # Test use within $copy method
  expect_equal(clone_c$get_id(), 2,
               info = "incorrect id value of cloned combi instance")
  expect_equal(c$get_id(), 1,
               info = "incorrect id value of original combi instance after cloning")
  c$set_id(5136)
  expect_equal(c$get_id(), 5136,
               info = "method returns incorrect value after setting to know value")
  
  # Reset shared counter
  c$reset_sharedCounter()
})

test_that("combiStructureGenerator $get_next_id()", {
  # Initiate an instance
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  expect_equal(c$get_id(), 1,
               info = "attribute $static_counter is not initated with value 1")
  value <- get_private(c)$get_next_id()
  expect_equal(value, 2,
               info = "calling get_next_id does not return a correct value")
  expect_equal(c$get_sharedCounter(), 2,
               info = "calling get_next_id does not assign a correct value to attribute $static_counter")
  c$reset_sharedCounter()
})

test_that("combiStructureGenerator set_singleStr() and copy() check $my_combiStructure$get_id() points to correct combi instance",{
  # Initiate an instance to reset counter
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  c$reset_sharedCounter()
  
  # Check singleStrctures get in my_combiStructure the correct combi instance
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  for (i in 1: 3){
    expect_equal(get_private(c$get_singleStr(i))$my_combiStructure$get_id(), 1,
                 info = paste("singleStr", i, "does not have correct my_combiStructure according to ID before cloning copy()"))
  }
  cloned_c <- c$copy()
  for (i in 1: 3){
    expect_equal(get_private(c$get_singleStr(i))$my_combiStructure$get_id(), 1,
                 info = paste("Original combi: singleStr", i, "does not have correct my_combiStructure according to ID after cloning with copy()"))
    expect_equal(get_private(cloned_c$get_singleStr(i))$my_combiStructure$get_id(), 2,
                 info = paste("Cloned combi: singleStr", i, "does not have correct my_combiStructure according to ID after cloning with copy()"))
  }
  cloned2_c <- c$copy()
  cloned_from_cloned_c <- cloned_c$copy()
  for (i in 1: 3){
    expect_equal(get_private(c$get_singleStr(i))$my_combiStructure$get_id(), 1,
                 info = paste("Original combi: singleStr", i, "does not have correct my_combiStructure according to ID after cloning with copy()"))
    expect_equal(get_private(cloned_c$get_singleStr(i))$my_combiStructure$get_id(), 2,
                 info = paste("Cloned combi: singleStr", i, "does not have correct my_combiStructure according to ID after cloning with copy()"))
    expect_equal(get_private(cloned2_c$get_singleStr(i))$my_combiStructure$get_id(), 3,
                 info = paste("Second clone from original combi: singleStr", i, "does not have correct my_combiStructure according to ID after cloning with copy()"))
    expect_equal(get_private(cloned_from_cloned_c$get_singleStr(i))$my_combiStructure$get_id(), 4,
                 info = paste("Clone from cloned combi: singleStr", i, "does not have correct my_combiStructure according to ID after cloning with copy()"))
  }
  # Reset combi instance counter
  c$reset_sharedCounter()
})

test_that("combiStructureGenerator set_singleStr() and copy() check modification without self reference only affects clone calling modification (c)", {
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  cloned_c <- c$copy()
  cloned2_c <- c$copy()
  cloned_from_cloned_c <- cloned_c$copy()
  
  # Check after cloning sequences are the same
  original_seq <- get_private(c$get_singleStr(2))$seq
  copied_seq <- get_private(cloned_c$get_singleStr(2))$seq
  copied_seq2 <- get_private(cloned2_c$get_singleStr(2))$seq
  copied_from_copied_seq <- get_private(cloned_from_cloned_c$get_singleStr(2))$seq
  expect_equal(original_seq, copied_seq, info="first clone fails to copy $seq data")
  expect_equal(original_seq, copied_seq2, info="second clone fails to copy $seq data")
  expect_equal(original_seq, copied_from_copied_seq, info="clone from clone fails to copy $seq data")
  
  # Check after cloning neighbSt are the same
  original_neighbSt <- get_private(c$get_singleStr(2))$neighbSt
  copied_neighbSt <- get_private(cloned_c$get_singleStr(2))$neighbSt
  copied_neighbSt2 <- get_private(cloned2_c$get_singleStr(2))$neighbSt
  copied_from_copied_neighbSt <- get_private(cloned_from_cloned_c$get_singleStr(2))$neighbSt
  expect_equal(original_neighbSt, copied_neighbSt, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_neighbSt2, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_from_copied_neighbSt, info="first clone fails to copy $neighbSt data")
  
  # Modify intermediate position
  if (original_seq[2]==1 || original_seq[2]==3) newSt <- 2 else newSt <- 3
  c$get_singleStr(2)$set_seqSt_update_neighbSt(index = 2, newSt = newSt)
  
  modified_seq <- get_private(c$get_singleStr(2))$seq
  expect_false(all(modified_seq == original_seq), info = "clone fails to get $seq modified")
  expect_equal(original_seq, cloned_c$get_singleStr(2)$get_seq(), info= "first clone gets incorrect modification of $seq data")
  expect_equal(original_seq, cloned2_c$get_singleStr(2)$get_seq(), info="second clone gets incorrect modification of $seq data")
  expect_equal(original_seq, cloned_from_cloned_c$get_singleStr(2)$get_seq(), info="clone from clone gets incorrect modification of $seq data")
  
  modified_neighbSt <- get_private(c$get_singleStr(2))$neighbSt
  expect_false(all(modified_neighbSt == original_neighbSt), info = "clone fails to get $neighbSt modified")
  expect_equal(original_neighbSt, cloned_c$get_singleStr(2)$get_neighbSt(), info= "first clone gets incorrect modification of $neighbSt data")
  expect_equal(original_neighbSt, cloned2_c$get_singleStr(2)$get_neighbSt(), info="second clone gets incorrect modification of $neighbSt data")
  expect_equal(original_neighbSt, cloned_from_cloned_c$get_singleStr(2)$get_neighbSt(), info="clone from clone gets incorrect modification of $neighbSt data")
  
  # Reset combi instance counter
  c$reset_sharedCounter()
})

test_that("combiStructureGenerator set_singleStr() and copy() check modification without self reference only affects clone calling modification (cloned_c)", {
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  cloned_c <- c$copy()
  cloned2_c <- c$copy()
  cloned_from_cloned_c <- cloned_c$copy()
  
  # Check after cloning sequences are the same
  original_seq <- get_private(c$get_singleStr(2))$seq
  copied_seq <- get_private(cloned_c$get_singleStr(2))$seq
  copied_seq2 <- get_private(cloned2_c$get_singleStr(2))$seq
  copied_from_copied_seq <- get_private(cloned_from_cloned_c$get_singleStr(2))$seq
  expect_equal(original_seq, copied_seq, info="first clone fails to copy $seq data")
  expect_equal(original_seq, copied_seq2, info="second clone fails to copy $seq data")
  expect_equal(original_seq, copied_from_copied_seq, info="clone from clone fails to copy $seq data")
  
  # Check after cloning neighbSt are the same
  original_neighbSt <- get_private(c$get_singleStr(2))$neighbSt
  copied_neighbSt <- get_private(cloned_c$get_singleStr(2))$neighbSt
  copied_neighbSt2 <- get_private(cloned2_c$get_singleStr(2))$neighbSt
  copied_from_copied_neighbSt <- get_private(cloned_from_cloned_c$get_singleStr(2))$neighbSt
  expect_equal(original_neighbSt, copied_neighbSt, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_neighbSt2, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_from_copied_neighbSt, info="first clone fails to copy $neighbSt data")
  
  # Modify intermediate position
  if (original_seq[2]==1 || original_seq[2]==3) newSt <- 2 else newSt <- 3
  cloned_c$get_singleStr(2)$set_seqSt_update_neighbSt(index = 2, newSt = newSt)
  
  modified_seq <- get_private(cloned_c$get_singleStr(2))$seq
  expect_false(all(modified_seq == original_seq), info = "clone fails to get $seq modified")
  expect_equal(original_seq, c$get_singleStr(2)$get_seq(), info= "original combi gets incorrect modification of $seq data")
  expect_equal(original_seq, cloned2_c$get_singleStr(2)$get_seq(), info="second clone gets incorrect modification of $seq data")
  expect_equal(original_seq, cloned_from_cloned_c$get_singleStr(2)$get_seq(), info="clone from clone gets incorrect modification of $seq data")
  
  modified_neighbSt <- get_private(cloned_c$get_singleStr(2))$neighbSt
  expect_false(all(modified_neighbSt == original_neighbSt), info = "clone fails to get $neighbSt modified")
  expect_equal(original_neighbSt, c$get_singleStr(2)$get_neighbSt(), info= "original combi gets incorrect modification of $neighbSt data")
  expect_equal(original_neighbSt, cloned2_c$get_singleStr(2)$get_neighbSt(), info="second clone gets incorrect modification of $neighbSt data")
  expect_equal(original_neighbSt, cloned_from_cloned_c$get_singleStr(2)$get_neighbSt(), info="clone from clone gets incorrect modification of $neighbSt data")
  
  # Reset combi instance counter
  c$reset_sharedCounter()
})

test_that("combiStructureGenerator set_singleStr() and copy() check modification without self reference only affects clone calling modification (cloned2_c)", {
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  cloned_c <- c$copy()
  cloned2_c <- c$copy()
  cloned_from_cloned_c <- cloned_c$copy()
  
  # Check after cloning sequences are the same
  original_seq <- get_private(c$get_singleStr(2))$seq
  copied_seq <- get_private(cloned_c$get_singleStr(2))$seq
  copied_seq2 <- get_private(cloned2_c$get_singleStr(2))$seq
  copied_from_copied_seq <- get_private(cloned_from_cloned_c$get_singleStr(2))$seq
  expect_equal(original_seq, copied_seq, info="first clone fails to copy $seq data")
  expect_equal(original_seq, copied_seq2, info="second clone fails to copy $seq data")
  expect_equal(original_seq, copied_from_copied_seq, info="clone from clone fails to copy $seq data")
  
  # Check after cloning neighbSt are the same
  original_neighbSt <- get_private(c$get_singleStr(2))$neighbSt
  copied_neighbSt <- get_private(cloned_c$get_singleStr(2))$neighbSt
  copied_neighbSt2 <- get_private(cloned2_c$get_singleStr(2))$neighbSt
  copied_from_copied_neighbSt <- get_private(cloned_from_cloned_c$get_singleStr(2))$neighbSt
  expect_equal(original_neighbSt, copied_neighbSt, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_neighbSt2, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_from_copied_neighbSt, info="first clone fails to copy $neighbSt data")
  
  # Modify intermediate position
  if (original_seq[2]==1 || original_seq[2]==3) newSt <- 2 else newSt <- 3
  cloned2_c$get_singleStr(2)$set_seqSt_update_neighbSt(index = 2, newSt = newSt)
  
  modified_seq <- get_private(cloned2_c$get_singleStr(2))$seq
  expect_false(all(modified_seq == original_seq), info = "clone fails to get $seq modified")
  expect_equal(original_seq, c$get_singleStr(2)$get_seq(), info= "original combi gets incorrect modification of $seq data")
  expect_equal(original_seq, cloned_c$get_singleStr(2)$get_seq(), info="first clone gets incorrect modification of $seq data")
  expect_equal(original_seq, cloned_from_cloned_c$get_singleStr(2)$get_seq(), info="clone from clone gets incorrect modification of $seq data")
  
  modified_neighbSt <- get_private(cloned2_c$get_singleStr(2))$neighbSt
  expect_false(all(modified_neighbSt == original_neighbSt), info = "clone fails to get $neighbSt modified")
  expect_equal(original_neighbSt, c$get_singleStr(2)$get_neighbSt(), info= "original combi gets incorrect modification of $neighbSt data")
  expect_equal(original_neighbSt, cloned_c$get_singleStr(2)$get_neighbSt(), info="first clone gets incorrect modification of $neighbSt data")
  expect_equal(original_neighbSt, cloned_from_cloned_c$get_singleStr(2)$get_neighbSt(), info="clone from clone gets incorrect modification of $neighbSt data")
  
  # Reset combi instance counter
  c$reset_sharedCounter()
})


test_that("combiStructureGenerator set_singleStr() and copy() check modification without self reference only affects clone calling modification (cloned_from_cloned_c)", {
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  cloned_c <- c$copy()
  cloned2_c <- c$copy()
  cloned_from_cloned_c <- cloned_c$copy()
  
  # Check after cloning sequences are the same
  original_seq <- get_private(c$get_singleStr(2))$seq
  copied_seq <- get_private(cloned_c$get_singleStr(2))$seq
  copied_seq2 <- get_private(cloned2_c$get_singleStr(2))$seq
  copied_from_copied_seq <- get_private(cloned_from_cloned_c$get_singleStr(2))$seq
  expect_equal(original_seq, copied_seq, info="first clone fails to copy $seq data")
  expect_equal(original_seq, copied_seq2, info="second clone fails to copy $seq data")
  expect_equal(original_seq, copied_from_copied_seq, info="clone from clone fails to copy $seq data")
  
  # Check after cloning neighbSt are the same
  original_neighbSt <- get_private(c$get_singleStr(2))$neighbSt
  copied_neighbSt <- get_private(cloned_c$get_singleStr(2))$neighbSt
  copied_neighbSt2 <- get_private(cloned2_c$get_singleStr(2))$neighbSt
  copied_from_copied_neighbSt <- get_private(cloned_from_cloned_c$get_singleStr(2))$neighbSt
  expect_equal(original_neighbSt, copied_neighbSt, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_neighbSt2, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_from_copied_neighbSt, info="first clone fails to copy $neighbSt data")
  
  # Modify intermediate position
  if (original_seq[2]==1 || original_seq[2]==3) newSt <- 2 else newSt <- 3
  cloned_from_cloned_c$get_singleStr(2)$set_seqSt_update_neighbSt(index = 2, newSt = newSt)
  
  modified_seq <- get_private(cloned_from_cloned_c$get_singleStr(2))$seq
  expect_false(all(modified_seq == original_seq), info = "clone fails to get $seq modified")
  expect_equal(original_seq, c$get_singleStr(2)$get_seq(), info= "original combi gets incorrect modification of $seq data")
  expect_equal(original_seq, cloned_c$get_singleStr(2)$get_seq(), info="first clone gets incorrect modification of $seq data")
  expect_equal(original_seq, cloned2_c$get_singleStr(2)$get_seq(), info="second clone gets incorrect modification of $seq data")
  
  modified_neighbSt <- get_private(cloned_from_cloned_c$get_singleStr(2))$neighbSt
  expect_false(all(modified_neighbSt == original_neighbSt), info = "clone fails to get $neighbSt modified")
  expect_equal(original_neighbSt, c$get_singleStr(2)$get_neighbSt(), info= "original combi gets incorrect modification of $neighbSt data")
  expect_equal(original_neighbSt, cloned_c$get_singleStr(2)$get_neighbSt(), info="first clone gets incorrect modification of $neighbSt data")
  expect_equal(original_neighbSt, cloned2_c$get_singleStr(2)$get_neighbSt(), info="second clone gets incorrect modification of $neighbSt data")
  
  # Reset combi instance counter
  c$reset_sharedCounter()
})

test_that("combiStructureGenerator set_singleStr() and copy() check modification with self reference only affects clone calling modification (c)", {
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  cloned_c <- c$copy()
  cloned2_c <- c$copy()
  cloned_from_cloned_c <- cloned_c$copy()
  
  # Check after cloning sequences are the same
  original_seq <- get_private(c$get_singleStr(2))$seq
  copied_seq <- get_private(cloned_c$get_singleStr(2))$seq
  copied_seq2 <- get_private(cloned2_c$get_singleStr(2))$seq
  copied_from_copied_seq <- get_private(cloned_from_cloned_c$get_singleStr(2))$seq
  expect_equal(original_seq, copied_seq, info="first clone fails to copy $seq data")
  expect_equal(original_seq, copied_seq2, info="second clone fails to copy $seq data")
  expect_equal(original_seq, copied_from_copied_seq, info="clone from clone fails to copy $seq data")
  
  # Check after cloning neighbSt are the same
  original_neighbSt <- get_private(c$get_singleStr(2))$neighbSt
  copied_neighbSt <- get_private(cloned_c$get_singleStr(2))$neighbSt
  copied_neighbSt2 <- get_private(cloned2_c$get_singleStr(2))$neighbSt
  copied_from_copied_neighbSt <- get_private(cloned_from_cloned_c$get_singleStr(2))$neighbSt
  expect_equal(original_neighbSt, copied_neighbSt, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_neighbSt2, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_from_copied_neighbSt, info="first clone fails to copy $neighbSt data")

  # Check modifications only applied to combi calling the change
  lastPosFirstStr_original_neighbSt <- get_private(c$get_singleStr(1))$neighbSt[13]
  firstPosLastStr_original_neighbSt <- get_private(c$get_singleStr(3))$neighbSt[1]
  old_completeSeq <- c(c$get_singleStr(1)$get_seq(), c$get_singleStr(2)$get_seq(), c$get_singleStr(3)$get_seq())
  old_2ndStr_NeighbSt <- c$get_singleStr(2)$get_neighbSt()
  
  if (lastPosFirstStr_original_neighbSt != 9 && firstPosLastStr_original_neighbSt != 9){
    if(!all(old_completeSeq == 3)){ # this also ensures that all old_2ndStr_NeighbSt are not equal to 9

      # Modify all $seq positions
      for(str in 1:3){
        # Set the sequences for each structure as all m 
        c$get_singleStr(str)$cftp_all_equal(state = "M")
      }
      
      # Update neighbSt for all positions
      for(str in 1:3){
        # Set the sequences for each structure as all m 
        c$get_singleStr(str)$init_neighbSt()
      }
      
      # Check modifications in $seq
      modified_completeSeq <- c(c$get_singleStr(1)$get_seq(), c$get_singleStr(2)$get_seq(), c$get_singleStr(3)$get_seq())
      expect_true(all(modified_completeSeq == 3), info = "clone fails to get $seq modified")
      expect_equal(c(cloned_c$get_singleStr(1)$get_seq(), cloned_c$get_singleStr(2)$get_seq(), cloned_c$get_singleStr(3)$get_seq()), 
                   old_completeSeq, 
                   info= "first clone gets incorrect modification of $seq data")
      expect_equal(c(cloned2_c$get_singleStr(1)$get_seq(),cloned2_c$get_singleStr(2)$get_seq(), cloned2_c$get_singleStr(3)$get_seq()), 
                   old_completeSeq, 
                   info="second clone gets incorrect modification of $seq data")
      expect_equal(c(cloned_from_cloned_c$get_singleStr(1)$get_seq(), cloned_from_cloned_c$get_singleStr(2)$get_seq(), cloned_from_cloned_c$get_singleStr(3)$get_seq()), 
                   old_completeSeq, 
                   info="clone from clone gets incorrect modification of $seq data")
      
      # Check modifications in $neighbSt second singleStr
      modified_2ndStr_neighbSt <- c$get_singleStr(2)$get_neighbSt()
      expect_true(all(modified_2ndStr_neighbSt == 9), info = "clone fails to get $neighbSt modified at second singleStr")
      
      expect_equal(cloned_c$get_singleStr(2)$get_neighbSt(), 
                   old_2ndStr_NeighbSt, 
                   info= "first clone gets incorrect modification of $neighbSt data at second singleStr")
      expect_equal(cloned2_c$get_singleStr(2)$get_neighbSt(), 
                   old_2ndStr_NeighbSt, 
                   info="second clone gets incorrect modification of $neighbSt data at second singleStr")
      expect_equal(cloned_from_cloned_c$get_singleStr(2)$get_neighbSt(),
                   old_2ndStr_NeighbSt, 
                   info="clone from clone gets incorrect modification of $neighbSt data at second singleStr")
      
      # Check modifications in $neighbSt last position first singleStr
      modified_lastPosFirstStr <- c$get_singleStr(1)$get_neighbSt()[13]
      expect_true(modified_lastPosFirstStr == 9, info = "clone fails to get $neighbSt modified at last position first singleStr")
      
      expect_equal(cloned_c$get_singleStr(1)$get_neighbSt()[13], 
                   lastPosFirstStr_original_neighbSt, 
                   info= "first clone gets incorrect modification of $neighbSt data at last position first singleStr")
      expect_equal(cloned2_c$get_singleStr(1)$get_neighbSt()[13], 
                   lastPosFirstStr_original_neighbSt, 
                   info="second clone gets incorrect modification of $neighbSt data at last position first singleStr")
      expect_equal(cloned_from_cloned_c$get_singleStr(1)$get_neighbSt()[13],
                   lastPosFirstStr_original_neighbSt, 
                   info="clone from clone gets incorrect modification of $neighbSt data at last position first singleStr")
      
      # Check modifications in $neighbSt first position last singleStr
      modified_firstPosLastStr <- c$get_singleStr(3)$get_neighbSt()[1]
      expect_true(modified_firstPosLastStr == 9, info = "clone fails to get $neighbSt modified at first position last singleStr")
      expect_equal(cloned_c$get_singleStr(3)$get_neighbSt()[1], 
                   firstPosLastStr_original_neighbSt, 
                   info= "first clone gets incorrect modification of $neighbSt data at first position last singleStr")
      expect_equal(cloned2_c$get_singleStr(3)$get_neighbSt()[1], 
                   firstPosLastStr_original_neighbSt, 
                   info="second clone gets incorrect modification of $neighbSt data at first position last singleStr")
      expect_equal(cloned_from_cloned_c$get_singleStr(3)$get_neighbSt()[1],
                   firstPosLastStr_original_neighbSt, 
                   info="clone from clone gets incorrect modification of $neighbSt data at first position last singleStr")
    }
  }
  # Reset combi instance counter
  c$reset_sharedCounter()
})

test_that("combiStructureGenerator set_singleStr() and copy() check modification with self reference only affects clone calling modification (cloned_c)", {
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  cloned_c <- c$copy()
  cloned2_c <- c$copy()
  cloned_from_cloned_c <- cloned_c$copy()
  
  # Check after cloning sequences are the same
  original_seq <- get_private(c$get_singleStr(2))$seq
  copied_seq <- get_private(cloned_c$get_singleStr(2))$seq
  copied_seq2 <- get_private(cloned2_c$get_singleStr(2))$seq
  copied_from_copied_seq <- get_private(cloned_from_cloned_c$get_singleStr(2))$seq
  expect_equal(original_seq, copied_seq, info="first clone fails to copy $seq data")
  expect_equal(original_seq, copied_seq2, info="second clone fails to copy $seq data")
  expect_equal(original_seq, copied_from_copied_seq, info="clone from clone fails to copy $seq data")
  
  # Check after cloning neighbSt are the same
  original_neighbSt <- get_private(c$get_singleStr(2))$neighbSt
  copied_neighbSt <- get_private(cloned_c$get_singleStr(2))$neighbSt
  copied_neighbSt2 <- get_private(cloned2_c$get_singleStr(2))$neighbSt
  copied_from_copied_neighbSt <- get_private(cloned_from_cloned_c$get_singleStr(2))$neighbSt
  expect_equal(original_neighbSt, copied_neighbSt, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_neighbSt2, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_from_copied_neighbSt, info="first clone fails to copy $neighbSt data")
  
  # Check modifications only applied to combi calling the change
  lastPosFirstStr_original_neighbSt <- get_private(cloned_c$get_singleStr(1))$neighbSt[13]
  firstPosLastStr_original_neighbSt <- get_private(cloned_c$get_singleStr(3))$neighbSt[1]
  old_completeSeq <- c(cloned_c$get_singleStr(1)$get_seq(), cloned_c$get_singleStr(2)$get_seq(), cloned_c$get_singleStr(3)$get_seq())
  old_2ndStr_NeighbSt <- cloned_c$get_singleStr(2)$get_neighbSt()
  
  if (lastPosFirstStr_original_neighbSt != 9 && firstPosLastStr_original_neighbSt != 9){
    if(!all(old_completeSeq == 3)){ # this also ensures that all old_2ndStr_NeighbSt are not equal to 9
      
      # Modify all $seq positions
      for(str in 1:3){
        # Set the sequences for each structure as all m 
        cloned_c$get_singleStr(str)$cftp_all_equal(state = "M")
      }
      
      # Update neighbSt for all positions
      for(str in 1:3){
        # Set the sequences for each structure as all m 
        cloned_c$get_singleStr(str)$init_neighbSt()
      }
      
      # Check modifications in $seq
      modified_completeSeq <- c(cloned_c$get_singleStr(1)$get_seq(), cloned_c$get_singleStr(2)$get_seq(), cloned_c$get_singleStr(3)$get_seq())
      expect_true(all(modified_completeSeq == 3), info = "clone fails to get $seq modified")
      expect_equal(c(c$get_singleStr(1)$get_seq(), c$get_singleStr(2)$get_seq(), c$get_singleStr(3)$get_seq()), 
                   old_completeSeq, 
                   info= "original combi gets incorrect modification of $seq data")
      expect_equal(c(cloned2_c$get_singleStr(1)$get_seq(),cloned2_c$get_singleStr(2)$get_seq(), cloned2_c$get_singleStr(3)$get_seq()), 
                   old_completeSeq, 
                   info="second clone gets incorrect modification of $seq data")
      expect_equal(c(cloned_from_cloned_c$get_singleStr(1)$get_seq(), cloned_from_cloned_c$get_singleStr(2)$get_seq(), cloned_from_cloned_c$get_singleStr(3)$get_seq()), 
                   old_completeSeq, 
                   info="clone from clone gets incorrect modification of $seq data")
      
      # Check modifications in $neighbSt second singleStr
      modified_2ndStr_neighbSt <- cloned_c$get_singleStr(2)$get_neighbSt()
      expect_true(all(modified_2ndStr_neighbSt == 9), info = "clone fails to get $neighbSt modified at second singleStr")
      
      expect_equal(c$get_singleStr(2)$get_neighbSt(), 
                   old_2ndStr_NeighbSt, 
                   info= "original combi gets incorrect modification of $neighbSt data at second singleStr")
      expect_equal(cloned2_c$get_singleStr(2)$get_neighbSt(), 
                   old_2ndStr_NeighbSt, 
                   info="second clone gets incorrect modification of $neighbSt data at second singleStr")
      expect_equal(cloned_from_cloned_c$get_singleStr(2)$get_neighbSt(),
                   old_2ndStr_NeighbSt, 
                   info="clone from clone gets incorrect modification of $neighbSt data at second singleStr")
      
      # Check modifications in $neighbSt last position first singleStr
      modified_lastPosFirstStr <- cloned_c$get_singleStr(1)$get_neighbSt()[13]
      expect_true(modified_lastPosFirstStr == 9, info = "clone fails to get $neighbSt modified at last position first singleStr")
      
      expect_equal(c$get_singleStr(1)$get_neighbSt()[13], 
                   lastPosFirstStr_original_neighbSt, 
                   info= "original combi gets incorrect modification of $neighbSt data at last position first singleStr")
      expect_equal(cloned2_c$get_singleStr(1)$get_neighbSt()[13], 
                   lastPosFirstStr_original_neighbSt, 
                   info="second clone gets incorrect modification of $neighbSt data at last position first singleStr")
      expect_equal(cloned_from_cloned_c$get_singleStr(1)$get_neighbSt()[13],
                   lastPosFirstStr_original_neighbSt, 
                   info="clone from clone gets incorrect modification of $neighbSt data at last position first singleStr")
      
      # Check modifications in $neighbSt first position last singleStr
      modified_firstPosLastStr <- cloned_c$get_singleStr(3)$get_neighbSt()[1]
      expect_true(modified_firstPosLastStr == 9, info = "clone fails to get $neighbSt modified at first position last singleStr")
      expect_equal(c$get_singleStr(3)$get_neighbSt()[1], 
                   firstPosLastStr_original_neighbSt, 
                   info= "original combi gets incorrect modification of $neighbSt data at first position last singleStr")
      expect_equal(cloned2_c$get_singleStr(3)$get_neighbSt()[1], 
                   firstPosLastStr_original_neighbSt, 
                   info="second clone gets incorrect modification of $neighbSt data at first position last singleStr")
      expect_equal(cloned_from_cloned_c$get_singleStr(3)$get_neighbSt()[1],
                   firstPosLastStr_original_neighbSt, 
                   info="clone from clone gets incorrect modification of $neighbSt data at first position last singleStr")
    }
  }
  # Reset combi instance counter
  c$reset_sharedCounter()
})

test_that("combiStructureGenerator set_singleStr() and copy() check modification with self reference only affects clone calling modification (cloned2_c)", {
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  cloned_c <- c$copy()
  cloned2_c <- c$copy()
  cloned_from_cloned_c <- cloned_c$copy()
  
  # Check after cloning sequences are the same
  original_seq <- get_private(c$get_singleStr(2))$seq
  copied_seq <- get_private(cloned_c$get_singleStr(2))$seq
  copied_seq2 <- get_private(cloned2_c$get_singleStr(2))$seq
  copied_from_copied_seq <- get_private(cloned_from_cloned_c$get_singleStr(2))$seq
  expect_equal(original_seq, copied_seq, info="first clone fails to copy $seq data")
  expect_equal(original_seq, copied_seq2, info="second clone fails to copy $seq data")
  expect_equal(original_seq, copied_from_copied_seq, info="clone from clone fails to copy $seq data")
  
  # Check after cloning neighbSt are the same
  original_neighbSt <- get_private(c$get_singleStr(2))$neighbSt
  copied_neighbSt <- get_private(cloned_c$get_singleStr(2))$neighbSt
  copied_neighbSt2 <- get_private(cloned2_c$get_singleStr(2))$neighbSt
  copied_from_copied_neighbSt <- get_private(cloned_from_cloned_c$get_singleStr(2))$neighbSt
  expect_equal(original_neighbSt, copied_neighbSt, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_neighbSt2, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_from_copied_neighbSt, info="first clone fails to copy $neighbSt data")
  
  # Check modifications only applied to combi calling the change
  lastPosFirstStr_original_neighbSt <- get_private(cloned2_c$get_singleStr(1))$neighbSt[13]
  firstPosLastStr_original_neighbSt <- get_private(cloned2_c$get_singleStr(3))$neighbSt[1]
  old_completeSeq <- c(cloned2_c$get_singleStr(1)$get_seq(), cloned2_c$get_singleStr(2)$get_seq(), cloned2_c$get_singleStr(3)$get_seq())
  old_2ndStr_NeighbSt <- cloned2_c$get_singleStr(2)$get_neighbSt()
  
  if (lastPosFirstStr_original_neighbSt != 9 && firstPosLastStr_original_neighbSt != 9){
    if(!all(old_completeSeq == 3)){ # this also ensures that all old_2ndStr_NeighbSt are not equal to 9
      
      # Modify all $seq positions
      for(str in 1:3){
        # Set the sequences for each structure as all m 
        cloned2_c$get_singleStr(str)$cftp_all_equal(state = "M")
      }
      
      # Update neighbSt for all positions
      for(str in 1:3){
        # Set the sequences for each structure as all m 
        cloned2_c$get_singleStr(str)$init_neighbSt()
      }
      
      # Check modifications in $seq
      modified_completeSeq <- c(cloned2_c$get_singleStr(1)$get_seq(), cloned2_c$get_singleStr(2)$get_seq(), cloned2_c$get_singleStr(3)$get_seq())
      expect_true(all(modified_completeSeq == 3), info = "clone fails to get $seq modified")
      expect_equal(c(c$get_singleStr(1)$get_seq(), c$get_singleStr(2)$get_seq(), c$get_singleStr(3)$get_seq()), 
                   old_completeSeq, 
                   info= "original combi gets incorrect modification of $seq data")
      expect_equal(c(cloned_c$get_singleStr(1)$get_seq(),cloned_c$get_singleStr(2)$get_seq(), cloned_c$get_singleStr(3)$get_seq()), 
                   old_completeSeq, 
                   info="first clone gets incorrect modification of $seq data")
      expect_equal(c(cloned_from_cloned_c$get_singleStr(1)$get_seq(), cloned_from_cloned_c$get_singleStr(2)$get_seq(), cloned_from_cloned_c$get_singleStr(3)$get_seq()), 
                   old_completeSeq, 
                   info="clone from clone gets incorrect modification of $seq data")
      
      # Check modifications in $neighbSt second singleStr
      modified_2ndStr_neighbSt <- cloned2_c$get_singleStr(2)$get_neighbSt()
      expect_true(all(modified_2ndStr_neighbSt == 9), info = "clone fails to get $neighbSt modified at second singleStr")
      
      expect_equal(c$get_singleStr(2)$get_neighbSt(), 
                   old_2ndStr_NeighbSt, 
                   info= "original combi gets incorrect modification of $neighbSt data at second singleStr")
      expect_equal(cloned_c$get_singleStr(2)$get_neighbSt(), 
                   old_2ndStr_NeighbSt, 
                   info="first clone gets incorrect modification of $neighbSt data at second singleStr")
      expect_equal(cloned_from_cloned_c$get_singleStr(2)$get_neighbSt(),
                   old_2ndStr_NeighbSt, 
                   info="clone from clone gets incorrect modification of $neighbSt data at second singleStr")
      
      # Check modifications in $neighbSt last position first singleStr
      modified_lastPosFirstStr <- cloned2_c$get_singleStr(1)$get_neighbSt()[13]
      expect_true(modified_lastPosFirstStr == 9, info = "clone fails to get $neighbSt modified at last position first singleStr")
      
      expect_equal(c$get_singleStr(1)$get_neighbSt()[13], 
                   lastPosFirstStr_original_neighbSt, 
                   info= "original combi gets incorrect modification of $neighbSt data at last position first singleStr")
      expect_equal(cloned_c$get_singleStr(1)$get_neighbSt()[13], 
                   lastPosFirstStr_original_neighbSt, 
                   info="first clone gets incorrect modification of $neighbSt data at last position first singleStr")
      expect_equal(cloned_from_cloned_c$get_singleStr(1)$get_neighbSt()[13],
                   lastPosFirstStr_original_neighbSt, 
                   info="clone from clone gets incorrect modification of $neighbSt data at last position first singleStr")
      
      # Check modifications in $neighbSt first position last singleStr
      modified_firstPosLastStr <- cloned2_c$get_singleStr(3)$get_neighbSt()[1]
      expect_true(modified_firstPosLastStr == 9, info = "clone fails to get $neighbSt modified at first position last singleStr")
      expect_equal(c$get_singleStr(3)$get_neighbSt()[1], 
                   firstPosLastStr_original_neighbSt, 
                   info= "original combi gets incorrect modification of $neighbSt data at first position last singleStr")
      expect_equal(cloned_c$get_singleStr(3)$get_neighbSt()[1], 
                   firstPosLastStr_original_neighbSt, 
                   info="first clone gets incorrect modification of $neighbSt data at first position last singleStr")
      expect_equal(cloned_from_cloned_c$get_singleStr(3)$get_neighbSt()[1],
                   firstPosLastStr_original_neighbSt, 
                   info="clone from clone gets incorrect modification of $neighbSt data at first position last singleStr")
    }
  }
  # Reset combi instance counter
  c$reset_sharedCounter()
})

test_that("combiStructureGenerator set_singleStr() and copy() check modification with self reference only affects clone calling modification (cloned_from_cloned_c)", {
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  cloned_c <- c$copy()
  cloned2_c <- c$copy()
  cloned_from_cloned_c <- cloned_c$copy()
  
  # Check after cloning sequences are the same
  original_seq <- get_private(c$get_singleStr(2))$seq
  copied_seq <- get_private(cloned_c$get_singleStr(2))$seq
  copied_seq2 <- get_private(cloned2_c$get_singleStr(2))$seq
  copied_from_copied_seq <- get_private(cloned_from_cloned_c$get_singleStr(2))$seq
  expect_equal(original_seq, copied_seq, info="first clone fails to copy $seq data")
  expect_equal(original_seq, copied_seq2, info="second clone fails to copy $seq data")
  expect_equal(original_seq, copied_from_copied_seq, info="clone from clone fails to copy $seq data")
  
  # Check after cloning neighbSt are the same
  original_neighbSt <- get_private(c$get_singleStr(2))$neighbSt
  copied_neighbSt <- get_private(cloned_c$get_singleStr(2))$neighbSt
  copied_neighbSt2 <- get_private(cloned2_c$get_singleStr(2))$neighbSt
  copied_from_copied_neighbSt <- get_private(cloned_from_cloned_c$get_singleStr(2))$neighbSt
  expect_equal(original_neighbSt, copied_neighbSt, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_neighbSt2, info="first clone fails to copy $neighbSt data")
  expect_equal(original_neighbSt, copied_from_copied_neighbSt, info="first clone fails to copy $neighbSt data")
  
  # Check modifications only applied to combi calling the change
  lastPosFirstStr_original_neighbSt <- get_private(cloned_from_cloned_c$get_singleStr(1))$neighbSt[13]
  firstPosLastStr_original_neighbSt <- get_private(cloned_from_cloned_c$get_singleStr(3))$neighbSt[1]
  old_completeSeq <- c(cloned_from_cloned_c$get_singleStr(1)$get_seq(), cloned_from_cloned_c$get_singleStr(2)$get_seq(), cloned_from_cloned_c$get_singleStr(3)$get_seq())
  old_2ndStr_NeighbSt <- cloned_from_cloned_c$get_singleStr(2)$get_neighbSt()
  
  if (lastPosFirstStr_original_neighbSt != 9 && firstPosLastStr_original_neighbSt != 9){
    if(!all(old_completeSeq == 3)){ # this also ensures that all old_2ndStr_NeighbSt are not equal to 9
      
      # Modify all $seq positions
      for(str in 1:3){
        # Set the sequences for each structure as all m 
        cloned_from_cloned_c$get_singleStr(str)$cftp_all_equal(state = "M")
      }
      
      # Update neighbSt for all positions
      for(str in 1:3){
        # Set the sequences for each structure as all m 
        cloned_from_cloned_c$get_singleStr(str)$init_neighbSt()
      }
      
      # Check modifications in $seq
      modified_completeSeq <- c(cloned_from_cloned_c$get_singleStr(1)$get_seq(), cloned_from_cloned_c$get_singleStr(2)$get_seq(), cloned_from_cloned_c$get_singleStr(3)$get_seq())
      expect_true(all(modified_completeSeq == 3), info = "clone fails to get $seq modified")
      expect_equal(c(c$get_singleStr(1)$get_seq(), c$get_singleStr(2)$get_seq(), c$get_singleStr(3)$get_seq()), 
                   old_completeSeq, 
                   info= "original combi gets incorrect modification of $seq data")
      expect_equal(c(cloned_c$get_singleStr(1)$get_seq(),cloned_c$get_singleStr(2)$get_seq(), cloned_c$get_singleStr(3)$get_seq()), 
                   old_completeSeq, 
                   info="first clone gets incorrect modification of $seq data")
      expect_equal(c(cloned2_c$get_singleStr(1)$get_seq(), cloned2_c$get_singleStr(2)$get_seq(), cloned2_c$get_singleStr(3)$get_seq()), 
                   old_completeSeq, 
                   info="second clone gets incorrect modification of $seq data")
      
      # Check modifications in $neighbSt second singleStr
      modified_2ndStr_neighbSt <- cloned_from_cloned_c$get_singleStr(2)$get_neighbSt()
      expect_true(all(modified_2ndStr_neighbSt == 9), info = "clone fails to get $neighbSt modified at second singleStr")
      
      expect_equal(c$get_singleStr(2)$get_neighbSt(), 
                   old_2ndStr_NeighbSt, 
                   info= "original combi gets incorrect modification of $neighbSt data at second singleStr")
      expect_equal(cloned_c$get_singleStr(2)$get_neighbSt(), 
                   old_2ndStr_NeighbSt, 
                   info="first clone gets incorrect modification of $neighbSt data at second singleStr")
      expect_equal(cloned2_c$get_singleStr(2)$get_neighbSt(),
                   old_2ndStr_NeighbSt, 
                   info="second clone gets incorrect modification of $neighbSt data at second singleStr")
      
      # Check modifications in $neighbSt last position first singleStr
      modified_lastPosFirstStr <- cloned_from_cloned_c$get_singleStr(1)$get_neighbSt()[13]
      expect_true(modified_lastPosFirstStr == 9, info = "clone fails to get $neighbSt modified at last position first singleStr")
      
      expect_equal(c$get_singleStr(1)$get_neighbSt()[13], 
                   lastPosFirstStr_original_neighbSt, 
                   info= "original combi gets incorrect modification of $neighbSt data at last position first singleStr")
      expect_equal(cloned_c$get_singleStr(1)$get_neighbSt()[13], 
                   lastPosFirstStr_original_neighbSt, 
                   info="first clone gets incorrect modification of $neighbSt data at last position first singleStr")
      expect_equal(cloned2_c$get_singleStr(1)$get_neighbSt()[13],
                   lastPosFirstStr_original_neighbSt, 
                   info="second clone gets incorrect modification of $neighbSt data at last position first singleStr")
      
      # Check modifications in $neighbSt first position last singleStr
      modified_firstPosLastStr <- cloned_from_cloned_c$get_singleStr(3)$get_neighbSt()[1]
      expect_true(modified_firstPosLastStr == 9, info = "clone fails to get $neighbSt modified at first position last singleStr")
      expect_equal(c$get_singleStr(3)$get_neighbSt()[1], 
                   firstPosLastStr_original_neighbSt, 
                   info= "original combi gets incorrect modification of $neighbSt data at first position last singleStr")
      expect_equal(cloned_c$get_singleStr(3)$get_neighbSt()[1], 
                   firstPosLastStr_original_neighbSt, 
                   info="first clone gets incorrect modification of $neighbSt data at first position last singleStr")
      expect_equal(cloned2_c$get_singleStr(3)$get_neighbSt()[1],
                   firstPosLastStr_original_neighbSt, 
                   info="second clone gets incorrect modification of $neighbSt data at first position last singleStr")
    }
  }
  # Reset combi instance counter
  c$reset_sharedCounter()
})


test_that("singleStructureGenerator SSE_evol()", {
  ################# singleStructure instance ##
  obj <- singleStructureGenerator$new("U", 100)
  original_obj <- obj$clone()
  # Testing mode: Returns SSE_evol info with dataframe
  number_SSEs <- 100
  dtime = 0.01
  test_info <- list()
  for (i in 1:number_SSEs){
    test_info[[i]] <- obj$SSE_evol(dt = dtime, testing = TRUE)
  }
  # Test 1: Dataframe contains as many rows as events sampled
  for (i in 1:number_SSEs){
    expect_equal(test_info[[i]]$event_number, nrow(test_info[[i]]$SSE_evolInfo),
                 info = paste("Test 1 isolated singleStr failed for SSE number:", i))
  }
  # Check that in positions with changes..
  if(any(sapply(test_info, function(x) x$event_number != 0))){
    indices <- which(sapply(test_info, function(x) x$event_number != 0))
    filtered_dataframes <- do.call(rbind, lapply(indices, function(i) {
      test_info[[i]]$SSE_evolInfo
    }))
    # Test 2: Old sequence equals old state info and new sequence equals new state info
    for (i in filtered_dataframes[,"position"]){
      expect_equal(filtered_dataframes[filtered_dataframes$position==i,"old_St"][1], get_private(original_obj)$seq[i],
                   info = "Test 2 isolated singleStr instance: oldSt of first event in $seq position does not correspond to original sequence")
      expect_equal(filtered_dataframes[filtered_dataframes$position==i,"new_St"][length(filtered_dataframes[filtered_dataframes$position==i,"new_St"])],get_private(obj)$seq[i],
                   info = "Test 2 isolated singleStr instance: newSt of last event in $seq position does not correspond to evolved sequence")
    }
  }
  # Test that by default (testing = FALSE) the function returns nothing
  result <- obj$SSE_evol(dt = 0.01)
  expect_null(result, info="output not null in isolated singleStr instance")

  #################### combiStructure instance ##
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  original_combi_obj <- combi_obj$copy()

  # Testing mode: Returns SSE_evol info with dataframe
  number_SSEs <- 100
  dtime = 0.01
  test_info <- list()
  for (i in 1:number_SSEs){
    test_info[[i]] <- combi_obj$get_singleStr(1)$SSE_evol(dt = dtime, testing = TRUE)
  }
  # Test 1: Dataframe contains as many rows as events sampled
  for (i in 1:number_SSEs){
    expect_equal(test_info[[i]]$event_number, nrow(test_info[[i]]$SSE_evolInfo),
                 info = paste("Test 1 singleStr within combiStr failed for SSE number:", i))
  }
  # Check that in positions with changes..
  if(any(sapply(test_info, function(x) x$event_number != 0))){
    indices <- which(sapply(test_info, function(x) x$event_number != 0))
    filtered_dataframes <- do.call(rbind, lapply(indices, function(i) {
      test_info[[i]]$SSE_evolInfo
    }))
    # Test 2: Old sequence equals old state info and new sequence equals new state info
    for (i in filtered_dataframes[,"position"]){
      expect_equal(filtered_dataframes[filtered_dataframes$position==i,"old_St"][1], get_private(original_combi_obj$get_singleStr(1))$seq[i],
                   info = "Test 2 isolated singleStr instance:oldSt of first event in $seq position does not correspond to original sequence")
      expect_equal(filtered_dataframes[filtered_dataframes$position==i,"new_St"][length(filtered_dataframes[filtered_dataframes$position==i,"new_St"])],get_private(combi_obj$get_singleStr(1))$seq[i],
                   info = "Test 2 isolated singleStr instance: newSt of last event in $seq position does not correspond to evolved sequence")
    }
  }
  # Test that by default (testing = FALSE) the function returns nothing
  result <- combi_obj$get_singleStr(1)$SSE_evol(dt = 0.01)
  expect_null(result, info = "output not null in singleStr instance within combiStr")


})

test_that("combiStructureGenerator SSE_evol()",{
  # Test 1: combiStructure instance with number of singleStr > 1
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  test_info <- get_private(combi_obj)$SSE_evol(dt = 0.01, testing=TRUE)
  expect_equal(length(test_info$evol_order), combi_obj$get_singleStr_number(), info = "Test1: length of singleStr evol_order different from singleStr number")
  expect_equal(length(test_info$testing_info), combi_obj$get_singleStr_number(), info = "Test1: length of SSE testing info different from singleStr number")

  # Test 1: combiStructure instance with number of singleStr > 1
  infoStr <- data.frame(n = c(13),
                        globalState = c("M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  test_info <- get_private(combi_obj)$SSE_evol(dt = 0.01, testing=TRUE)
  expect_equal(length(test_info$evol_order), combi_obj$get_singleStr_number(), info = "Test2: length of singleStr evol_order different from singleStr number")
  expect_equal(length(test_info$testing_info), combi_obj$get_singleStr_number(), info = "Test2: length of SSE testing info different from singleStr number")

  # Check it does not produce output with testing = FALSE
  test_info <- get_private(combi_obj)$SSE_evol(dt = 0.01, testing=FALSE)
  expect_null(test_info, info = "outputs info with testing = FALSE")
})

test_that("combiStructureGenerator interval_evol()",{
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  # Testing mode:
  #Interval_length is bigger than dt with remainder
  output <- get_private(combi_obj)$interval_evol(interval_length = 1.005, dt = 0.01, testing=T)
  expect_equal(output[1], paste("Number of SSE:", 100, ". Interval length:", 0.01))
  expect_equal(strsplit(output[2], " ")[[1]][4], "remainder")

  # Interval_length is smaller than dt
  output <- get_private(combi_obj)$interval_evol(interval_length = 0.005, dt = 0.01, testing=T)
  expect_equal(output, paste("SSE evolving in shorter interval_length than dt. Interval length:", 0.005))

  # Interval_length is bigger than dt and no remainder
  output <- get_private(combi_obj)$interval_evol(interval_length = 1, dt = 0.01, testing=T)
  expect_equal(output[1], paste("Number of SSE: 100 . Interval length:", 0.01))
  expect_equal(output[2], "No remainder")

  # Test that by default (testing = FALSE) the function returns nothing
  output <- get_private(combi_obj)$interval_evol(interval_length = 1.005, dt = 0.01)
  expect_null(output)
})


test_that("singleStructureGenerator $get_transMat() errors/warnings", {
  
  # Initialize singleStructureGenerator instance
  obj <- singleStructureGenerator$new("U", 100)
  
  # Expect error when one or both frequency vectors are not given
  expect_error(obj$get_transMat(info = "test"),
               info = "method fails to throw error when no frequency vector is given")
  expect_error(obj$get_transMat(old_eqFreqs = c(.1, .4, .5), info = "test"),
               info = "method fails to throw error when argument 'new_eqFreqs' is not given")
  expect_error(obj$get_transMat(new_eqFreqs = c(.1, .4, .5), info = "test"),
               info = "method fails to throw error when argument 'old_eqFreqs' is not given")
  
  # Expect error when no info is given or when it is not a character string
  expect_error(obj$get_transMat(old_eqFreqs = c(.1, .4, .5), new_eqFreqs = c(.1, .4, .5)),
               info = "method fails to throw error when argument 'info' is not given")
  expect_error(obj$get_transMat(old_eqFreqs = c(.1, .4, .5), new_eqFreqs = c(.1, .4, .5), info = 2),
               info = "method fails to throw error when argument 'info' is not a character string")
  
  # Expect error when any of the frequency vectors is not a numeric vector of length 3
  expect_error(obj$get_transMat(old_eqFreqs = "M", new_eqFreqs = c(.1, .4, .5), info = "test"),
               info = "method fails to throw error when 'old_eqFreqs' is not numeric vector")
  expect_error(obj$get_transMat(old_eqFreqs = c(.1, .4, .5), new_eqFreqs = "M", info = "test"),
               info = "method fails to throw error when 'new_eqFreqs' is not numeric vector")
  expect_error(obj$get_transMat(old_eqFreqs = c(.1, .4, .5), new_eqFreqs = 1, info = "test"),
               info = "method fails to throw error when 'new_eqFreqs' is not vector with 3 values")
  expect_error(obj$get_transMat(old_eqFreqs = 1, new_eqFreqs = c(.1, .4, .5), info = "test"),
               info = "method fails to throw error when 'old_eqFreqs' is not numeric vector")
  
  # Expect error when any of the frequency vectors has values <0 or >1
  expect_error(obj$get_transMat(old_eqFreqs = c(-.1, .4, .5), new_eqFreqs = c(.1, .4, .5), info = "test"),
               info = "method fails to throw error when 'old_eqFreqs' has values <0 or >1")
  expect_error(obj$get_transMat(old_eqFreqs = c(.1, .4, .5), new_eqFreqs = c(.1, .4, 1.1), info = "test"),
               info = "method fails to throw error when 'new_eqFreqs' has values <0 or >1")
  
  # Expect warning when the sum of the frequency vectors is not 1
  expect_warning(obj$get_transMat(old_eqFreqs = c(.005, .4, .5), new_eqFreqs = c(.1, .4, .5), info = "test"),
                 info = "method fails to throw warning when 'old_eqFreqs' dont sum 1")
  expect_warning(obj$get_transMat(old_eqFreqs = c(.1, .4, .5), new_eqFreqs = c(.005, .4, .5), info = "test"),
                 info = "method fails to throw warning when 'new_eqFreqs' dont sum 1")
  
  # Expect only matrix output when testing is (as default) FALSE
  output <- obj$get_transMat(old_eqFreqs = c(.1, .4, .5), new_eqFreqs = c(.1, .4, .5), info = "test")
  expect_true(is.matrix(output),
              info = "with testing FALSE method fails to output matrix")
})

test_that("singleStructureGenerator $get_transMat() test output Case 1. u bigger", {
  # Initialize singleStructureGenerator instance
  obj <- singleStructureGenerator$new("U", 100)
  
  # Set values for arguments testing and info
  t = TRUE
  i = "test"
  
  # Set expected case
  exp_case <- "Case 1. u bigger or equal"
  
  # Case 1. u bigger. p and m smaller
  info_subcase <- "p and m smaller."
  o_eqFreqs <- c(.6, .2, .2)
  n_eqFreqs <- c(.8, .1, .1)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. u bigger. p smaller and m equal
  info_subcase <- "p smaller and m equal."
  o_eqFreqs <- c(.6, .2, .2)
  n_eqFreqs <- c(.7, .1, .2)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. u bigger. p equal and m smaller
  info_subcase <- "p equal and m smaller."
  o_eqFreqs <- c(.6, .2, .2)
  n_eqFreqs <- c(.7, .2, .1)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. u bigger. old u was 0
  info_subcase <- "old u was 0."
  o_eqFreqs <- c(0, .5, .5)
  n_eqFreqs <- c(.2, .4, .4)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. u bigger. p is 0
  info_subcase <- "p is 0."
  o_eqFreqs <- c(.4, 0, .6)
  n_eqFreqs <- c(.6, 0, .4)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. u bigger. m is 0
  info_subcase <- "m is 0."
  o_eqFreqs <- c(.4, .6, 0)
  n_eqFreqs <- c(.6, .4, 0)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
})

test_that("singleStructureGenerator $get_transMat() test output Case 1. u equal", {
  # Initialize singleStructureGenerator instance
  obj <- singleStructureGenerator$new("U", 100)
  
  # Set values for arguments testing and info
  t = TRUE
  i = "test"
  
  # Set expected case
  exp_case <- "Case 1. u bigger or equal"
  
  # Case 1. u bigger. p and m smaller
  info_subcase <- "u stays 0. p and m smaller."
  o_eqFreqs <- c(0, .5, .5)
  n_eqFreqs <- c(0, .5, .5) - c(0, 1e-10, 1e-10)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  info_subcase <- "u stays non-0. p and m smaller."
  o_eqFreqs <- c(.2, .4, .4)
  n_eqFreqs <- c(.2, .4, .4) - c(0, 1e-10, 1e-10)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  # Case. u equal. p smaller and m equal
  info_subcase <- "u stays 0. p smaller and m equal."
  o_eqFreqs <- c(0, .5, .5)
  n_eqFreqs <- c(0, .5, .5) - c(0, 1e-10, 0)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  info_subcase <- "u stays non-0. p smaller and m equal."
  o_eqFreqs <- c(.1, .4, .5)
  n_eqFreqs <- c(.1, .4, .5) - c(0, 1e-10, 0)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  
  # Case. u equal. p equal and m smaller
  info_subcase <- "u stays 0. p equal and m smaller."
  o_eqFreqs <- c(0, .5, .5)
  n_eqFreqs <- c(0, .5, .5) - c(0, 0, 1e-10)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  info_subcase <- "u stays non-0. p equal and m smaller."
  o_eqFreqs <- c(.1, .4, .5)
  n_eqFreqs <- c(.1, .4, .5) - c(0, 0,1e-10)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  # Case. u equal. p is 0
  info_subcase <- "p is 0."
  o_eqFreqs <- c(.5, 0, .5)
  n_eqFreqs <- c(.5, 0, .5) - c(0, 0, 1e-10)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  # Case. u equal. m is 0
  info_subcase <- "m is 0."
  o_eqFreqs <- c(.5, .5, 0)
  n_eqFreqs <- c(.5, .5, 0) - c(0, 1e-10, 0)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
})

test_that("singleStructureGenerator $get_transMat() test output Case 1. p bigger", {
  # Initialize singleStructureGenerator instance
  obj <- singleStructureGenerator$new("U", 100)
  
  # Set values for arguments testing and info
  t = TRUE
  i = "test"
  
  # Set expected case
  exp_case <- "Case 1. p bigger or equal"
  
  # Case 1. p bigger. u and m smaller
  info_subcase <- "u and m smaller."
  o_eqFreqs <- c(.2, .6, .2)
  n_eqFreqs <- c(.1, .8, .1)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. p bigger. u smaller and m equal
  info_subcase <- "u smaller and m equal."
  o_eqFreqs <- c(.2, .6, .2)
  n_eqFreqs <- c(.1, .7, .2)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. p bigger. u equal and m smaller
  info_subcase <- "u equal and m smaller."
  o_eqFreqs <- c(.2, .6, .2)
  n_eqFreqs <- c(.2, .7, .1)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. p bigger. old p was 0
  info_subcase <- "old p was 0."
  o_eqFreqs <- c(.5, 0, .5)
  n_eqFreqs <- c(.4, .2, .4)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. p bigger. u is 0
  info_subcase <- "u is 0."
  o_eqFreqs <- c(0, .4, .6)
  n_eqFreqs <- c(0, .6, .4)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. p bigger. m is 0
  info_subcase <- "m is 0."
  o_eqFreqs <- c(.6, .4, 0)
  n_eqFreqs <- c(.4, .6, 0)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
})

test_that("singleStructureGenerator $get_transMat() test output Case 1. p equal", {
  # Initialize singleStructureGenerator instance
  obj <- singleStructureGenerator$new("U", 100)
  
  # Set values for arguments testing and info
  t = TRUE
  i = "test"
  
  # Set expected case
  exp_case <- "Case 1. p bigger or equal"
  
  # Case 1. p equal. u and m smaller
  info_subcase <- "p stays 0. u and m smaller."
  o_eqFreqs <- c(.5, 0, .5)
  n_eqFreqs <- c(.5, 0, .5) - c(1e-10, 0, 1e-10)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  info_subcase <- "p equal but non-0. u and m smaller."
  o_eqFreqs <- c(.4, .2, .4)
  n_eqFreqs <- c(.4, .2, .4) - c(1e-10, 0, 1e-10)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  # Case 1. p equal. u smaller and m equal
  info_subcase <- "p stays 0. u smaller and m equal."
  o_eqFreqs <- c(.5, 0, .5)
  n_eqFreqs <- c(.5, 0, .5) - c(1e-10, 0, 0)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  info_subcase <- "p equal but non-0. u smaller and m equal."
  o_eqFreqs <- c(.4, .2, .4)
  n_eqFreqs <- c(.4, .2, .4) - c(1e-10, 0, 0)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  # Case 1. p equal. u equal and m smaller. Enters first if (u equal)
  
  # Case 1. p equal. u is 0. Enters first if (u equal or bigger)
  
  # Case. p equal. m is 0. 
  info_subcase <- "m is 0."
  o_eqFreqs <- c(.5, .5, 0)
  n_eqFreqs <- c(.5, .5, 0) - c(1e-10, 0, 0)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
})

test_that("singleStructureGenerator $get_transMat() test output Case 1. m bigger", {
  # Initialize singleStructureGenerator instance
  obj <- singleStructureGenerator$new("U", 100)
  
  # Set values for arguments testing and info
  t = TRUE
  i = "test"
  
  # Set expected case
  exp_case <- "Case 1. m bigger or equal"
  
  # Case 1. m bigger. u and p smaller
  info_subcase <- "u and p smaller."
  o_eqFreqs <- c(.2, .2, .6)
  n_eqFreqs <- c(.1, .1, .8)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. m bigger. u smaller and p equal
  info_subcase <- "u smaller and p equal."
  o_eqFreqs <- c(.2, .2, .6)
  n_eqFreqs <- c(.1, .2, .7)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. m bigger. u equal and p smaller
  info_subcase <- "u equal and p smaller."
  o_eqFreqs <- c(.2, .2, .6)
  n_eqFreqs <- c(.2, .1, .7)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. m bigger. old m was 0
  info_subcase <- "old m was 0."
  o_eqFreqs <- c(.5, .5, 0)
  n_eqFreqs <- c(.4, .4, .2)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. m bigger. u is 0
  info_subcase <- "u is 0."
  o_eqFreqs <- c(0, .6, .4)
  n_eqFreqs <- c(0, .4, .6)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  # Case 1. m bigger. p is 0
  info_subcase <- "p is 0."
  o_eqFreqs <- c(.6, 0, .4)
  n_eqFreqs <- c(.4, 0, .6)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
})

test_that("singleStructureGenerator $get_transMat() test output Case 1. m equal", {
  # Initialize singleStructureGenerator instance
  obj <- singleStructureGenerator$new("U", 100)
  
  # Set values for arguments testing and info
  t = TRUE
  i = "test"
  
  # Set expected case
  exp_case <- "Case 1. m bigger or equal"
  
  # Case 1. m equal. u and p smaller
  info_subcase <- "m stays 0. u and p smaller."
  o_eqFreqs <- c(.5, .5, 0)
  n_eqFreqs <- c(.5, .5, 0) - c(1e-10, 1e-10, 0)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  info_subcase <- "m stays non-0. u and p smaller."
  o_eqFreqs <- c(.5, .4, .1)
  n_eqFreqs <- c(.5, .4, .1) - c(1e-10, 1e-10, 0)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  # Case 1. m equal. u smaller and p equal: Enters Case 1. p bigger or equal
  
  # Case 1. m equal. u equal and p smaller: Enters Case 1. u bigger or equal
 
  # Case 1. m equal. u is 0: Enters Case 1. u bigger or equal
  
  # Case 1. m equal. p is 0: Enters Case 1. p bigger or equal
})

test_that("singleStructureGenerator $get_transMat() test output Case 2. u smaller", {
  # Initialize singleStructureGenerator instance
  obj <- singleStructureGenerator$new("U", 100)
  
  # Set values for arguments testing and info
  t = TRUE
  i = "test"
  
  # Set expected case
  exp_case <- "Case 2. u smaller"
  
  # Case 2. u smaller. p and m bigger
  info_subcase <- "p and m bigger"
  o_eqFreqs <- c(.8, .1, .1)
  n_eqFreqs <- c(.6, .2, .2)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  # Case 2. u smaller. p bigger and m equal: Enters Case 1. p bigger or equal
  
  # Case 2. u smaller. p equal and m bigger: Enters Case 1. m bigger or equal
  
  # Case 2. u smaller. new u is 0
  info_subcase <- "new u is 0."
  o_eqFreqs <- c(.2, .4, .4)
  n_eqFreqs <- c(0, .5, .5)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  # Case 2. u smaller. p is 0
  info_subcase <- "p is 0."
  o_eqFreqs <- c(.6, 0, .4)
  n_eqFreqs <- c(.4, .1, .5)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  # Case 2. u smaller. m is 0
  info_subcase <- "m is 0."
  o_eqFreqs <- c(.6, .4, 0)
  n_eqFreqs <- c(.4, .5, .1)
  output <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i, t)
  expect_equal(output$case, exp_case,
               info = paste(info_subcase, "Method fails to output correct case"))
  expect_false(any(is.na(output$transMat) | is.nan(output$transMat) | is.infinite(output$transMat)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% output$transMat), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
})

test_that("singleStructureGenerator $get_transMat() test output for debugged errors", {
  
  # Initialize singleStructureGenerator instance
  obj <- singleStructureGenerator$new("U", 100)
  
  # Set values for arguments testing and info
  i = "test"
  
  # Set test cases that led to producing matrices with NaN values
  info_subcase <- "subcase id 1."
  o_eqFreqs <- c(0.0000000000, 0.0001296276, 0.9998703724)
  n_eqFreqs <- c(0.0000000, 0.3757835, 0.6242165)
  m <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i)
  expect_false(any(is.na(m) | is.nan(m) | is.infinite(m)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% m), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  info_subcase <- "subcase id 2."
  o_eqFreqs <- c(0.0000000000, 0.0009138619, 0.9990861381)
  n_eqFreqs <- c(0.0000000, 0.4442138, 0.5557862)
  m <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i)
  expect_false(any(is.na(m) | is.nan(m) | is.infinite(m)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% m), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  info_subcase <- "subcase id 3."
  o_eqFreqs <- c(0.0000000, 0.3582264, 0.6417736)
  n_eqFreqs <- c(0.0000000000, 0.0002113157, 0.9997886843)
  m <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i)
  expect_false(any(is.na(m) | is.nan(m) | is.infinite(m)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% m), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  info_subcase <- "subcase id 4."
  o_eqFreqs <- c(0.000000e+00, 1.742531e-09, 1.000000e+00)
  n_eqFreqs <- c(0.00000e+00, 1.79338e-13, 1.00000e+00)
  m <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i)
  expect_false(any(is.na(m) | is.nan(m) | is.infinite(m)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% m), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
  
  info_subcase <- "subcase id 5."
  o_eqFreqs <- c(0.00000e+00, 1.20016e-11, 1.00000e+00)
  n_eqFreqs <- c(0.00000e+00, 6.33789e-10, 1.00000e+00)
  m <- obj$get_transMat(o_eqFreqs, n_eqFreqs, i)
  expect_false(any(is.na(m) | is.nan(m) | is.infinite(m)),
               info = paste(info_subcase, "Method outputs matrix with non-numeric entries"))
  expect_equal(as.numeric(o_eqFreqs %*% m), n_eqFreqs,
               info = paste(info_subcase, "Output matrix does not fulfill MC transition property"))
})


test_that("singleStructureGenerator IWE_evol()", {
  ######## singleStructure instance ##
  ### a) with long $seq length
  obj <- singleStructureGenerator$new("U", 100)
  original_obj <- obj$clone()

  # Testing mode: Returns list with IWE info
  number_IWEs <- 100
  test_info <- list()
  for (i in 1:number_IWEs){
    test_info[[i]] <- obj$IWE_evol(testing = TRUE)
  }
  # Each IWE outputs a list
  expect_true(all(sapply(test_info, is.list)), info ="Not all IWEs with testing=TRUE output list in IWE_evol of isolated singleStructure instance a)")
  # Function to compare Q matrices returns TRUE if not identical and FALSE if identical
  compare_matrices <- function(mat1, mat2) {
    return(!identical(mat1, mat2))
  }
  for(i in 1:number_IWEs){
    expect_true(is.matrix(test_info[[i]]$Mk), info = "testing output $Mk does not return matrix in IWE_evol of isolated singleStructure instance a)")
    if (test_info[[i]]$eqFreqsChange){
      expect_equal(length(test_info[[i]]), 11,
                   info = "IWE output length does not correspond with $eqFreqsChange TRUE in IWE_evol of isolated singleStructure instance a)")
      # Test case matching
      if (test_info[[i]]$IWE_case == "Case 1. u bigger or equal"){
        expect_true(test_info[[i]]$new_eqFreqs[1]>=test_info[[i]]$old_eqFreqs[1], info = "fails test case matching u bigger in IWE_evol of isolated singleStructure instance a)")
      }
      if (test_info[[i]]$IWE_case == "Case 1. p bigger or equal"){
        expect_true(test_info[[i]]$new_eqFreqs[2]>=test_info[[i]]$old_eqFreqs[2], info = "fails test case matching p bigger in IWE_evol of isolated singleStructure instance a)")
      }
      if (test_info[[i]]$IWE_case == "Case 1. m bigger or equal"){
        expect_true(test_info[[i]]$new_eqFreqs[3]>=test_info[[i]]$old_eqFreqs[3], info = "fails test case matching m bigger in IWE_evol of isolated singleStructure instance a)")
      }
      if (test_info[[i]]$IWE_case == "Case 2. u smaller"){
        expect_true(test_info[[i]]$new_eqFreqs[1]<test_info[[i]]$old_eqFreqs[1], info = "fails test case matching u smaller in IWE_evol of isolated singleStructure instance a)")
      }
      if (test_info[[i]]$IWE_case == "Case 2. p smaller"){
        expect_true(test_info[[i]]$new_eqFreqs[2]<test_info[[i]]$old_eqFreqs[2], info = "fails test case matching p smaller in IWE_evol of isolated singleStructure instance a)")
      }
      if (test_info[[i]]$IWE_case == "Case 2. m smaller"){
        expect_true(test_info[[i]]$new_eqFreqs[3]<test_info[[i]]$old_eqFreqs[3], info = "fails test case matching m smaller in IWE_evol of isolated singleStructure instance a)")
      }
      if (i >=2){
        for (Ri in 1:3){
          for (neighbCase in 1:9){
            expect_true(compare_matrices(test_info[[i]]$new_Q[[Ri]][[neighbCase]], test_info[[i-1]]$new_Q[[Ri]][[neighbCase]]),
                        info = "$new_Q not updated with $eqFreqsChange TRUE in IWE_evol of isolated singleStructure instance a)")
          }
        }
      }

    } else {
      expect_false(test_info[[i]]$eqFreqsChange)
      expect_equal(length(test_info[[i]]), 3,
                   info = "IWE output length does not correspond with $eqFreqsChange FALSE in IWE_evol of isolated singleStructure instance a)")
    }
    if (i>=2){
      expect_equal(test_info[[i]]$old_eqFreqs, test_info[[i-1]]$new_eqFreqs,
                   info = "old_eqFreqs after IWE does not correspond with new_eqFreqs in previous IWE in IWE_evol of isolated singleStructure instance a)")
      expect_equal(test_info[[i]]$old_Q, test_info[[i-1]]$new_Q,
                   info = "old_Q after IWE does not correspond with new_Q in previous IWE in IWE_evol of isolated singleStructure instance a)")
    }

  }
  expect_equal(test_info[[1]]$old_eqFreqs, get_private(original_obj)$eqFreqs,
               info = "old_eqFreqs after first IWE does not correspond with eqFreqs original object in IWE_evol of isolated singleStructure instance a)")
  expect_equal(test_info[[number_IWEs]]$new_eqFreqs, get_private(obj)$eqFreqs,
               info = "old_eqFreqs after last IWE does not correspond with eqFreqs modified object in IWE_evol of isolated singleStructure instance a)")

  expect_equal(test_info[[1]]$old_Q, get_private(original_obj)$Q,
               info = "old_Q after first IWE does not correspond with $Q original object in IWE_evol of isolated singleStructure instance a)")
  expect_equal(test_info[[number_IWEs]]$new_Q, get_private(obj)$Q,
               info = "new_Q after last IWE does not correspond with $Q modified object in IWE_evol of isolated singleStructure instance a)")

  obj <- singleStructureGenerator$new("U", 100)
  original_obj <- obj$clone()

  test_info <- obj$IWE_evol(testing = TRUE)

  if (test_info$eqFreqsChange){
    if(!is.null(test_info$changedPos)){
      for (i in test_info$changedPos){
        expect_false(get_private(obj)$seq[i]==get_private(original_obj)$seq[i],
                     info = "$seq[changedPos] not changed with $eqFreqsChange T in IWE_evol of isolated singleStructure instance a)")
        if (i >= 2){
          expect_false(get_private(obj)$neighbSt[i-1]==get_private(original_obj)$neighbSt[i-1],
                       info = "neighbSt not updated in IWE_evol of isolated singleStructure instance a)")
        }
        if (i<= 99){
          expect_false(get_private(obj)$neighbSt[i+1]==get_private(original_obj)$neighbSt[i+1],
                       info = "neighbSt not updated in IWE_evol of isolated singleStructure instance a)")
        }
        for(j in max(i-1, 1):min(i+1, length(get_private(obj)$seq))){
          rate_obj <- abs(get_private(obj)$Q[[get_private(obj)$siteR[j]]][[get_private(obj)$neighbSt[j]]][get_private(obj)$seq[j], get_private(obj)$seq[j]])
          rate_oriObj <- abs(get_private(original_obj)$Q[[get_private(original_obj)$siteR[j]]][[get_private(original_obj)$neighbSt[j]]][get_private(original_obj)$seq[j], get_private(original_obj)$seq[j]])
          if (rate_obj !=rate_oriObj){
            expect_false(get_private(obj)$ratetree[[length(get_private(obj)$ratetree)]][j]==get_private(original_obj)$ratetree[[length(get_private(original_obj)$ratetree)]][j],
                         info = "$ratetree not updated for changedPos and/or neighbours with different change rate in IWE_evol of isolated singleStructure instance a)")
          }
        } #While distances are neglected, there can be a change that leads to a position with neighbSt 2 to neighbSt 4
        # and that does not translate in a different rate because those Qc are equal
      }
    }
  }

  # Test that by default (testing = FALSE) the function returns nothing
  result <- obj$IWE_evol()
  expect_null(result)

  ######## singleStructure instance ##
  ### b) with $seq of length = 1
  obj <- singleStructureGenerator$new("U", 1)
  original_obj <- obj$clone()

  # Testing mode: Returns list with IWE info
  number_IWEs <- 100
  test_info <- list()
  for (i in 1:number_IWEs){
    test_info[[i]] <- obj$IWE_evol(testing = TRUE)
  }

  # Each IWE outputs a list
  expect_true(all(sapply(test_info, is.list)), info ="Not all IWEs with testing=TRUE output list in IWE_evol of isolated singleStructure instance b)")
  for(i in 1:number_IWEs){
    expect_true(is.matrix(test_info[[i]]$Mk), info = "testing output $Mk does not return matrix in IWE_evol of isolated singleStructure instance b)")
    if (test_info[[i]]$eqFreqsChange){
      expect_equal(length(test_info[[i]]), 11,
                   info = "IWE output length does not correspond with $eqFreqsChange TRUE in IWE_evol of isolated singleStructure instance b)")
      # Test case matching
      if (test_info[[i]]$IWE_case == "Case 1. u bigger or equal"){
        expect_true(test_info[[i]]$new_eqFreqs[1]>test_info[[i]]$old_eqFreqs[1], info = "fails test case matching u bigger in IWE_evol of isolated singleStructure instance b)")
      }
      if (test_info[[i]]$IWE_case == "Case 1. p bigger"){
        expect_true(test_info[[i]]$new_eqFreqs[2]>test_info[[i]]$old_eqFreqs[2], info = "fails test case matching p bigger in IWE_evol of isolated singleStructure instance b)")
      }
      if (test_info[[i]]$IWE_case == "Case 1. m bigger"){
        expect_true(test_info[[i]]$new_eqFreqs[3]>test_info[[i]]$old_eqFreqs[3], info = "fails test case matching m bigger in IWE_evol of isolated singleStructure instance b)")
      }
      if (test_info[[i]]$IWE_case == "Case 2. u smaller"){
        expect_true(test_info[[i]]$new_eqFreqs[1]<test_info[[i]]$old_eqFreqs[1], info = "fails test case matching u smaller in IWE_evol of isolated singleStructure instance b)")
      }
      if (test_info[[i]]$IWE_case == "Case 2. p smaller"){
        expect_true(test_info[[i]]$new_eqFreqs[2]<test_info[[i]]$old_eqFreqs[2], info = "fails test case matching p smaller in IWE_evol of isolated singleStructure instance b)")
      }
      if (test_info[[i]]$IWE_case == "Case 2. m smaller"){
        expect_true(test_info[[i]]$new_eqFreqs[3]<test_info[[i]]$old_eqFreqs[3], info = "fails test case matching m smaller in IWE_evol of isolated singleStructure instance b)")
      }
      if (i >=2){
        for (Ri in 1:3){
          for (neighbCase in 1:9){
            expect_true(compare_matrices(test_info[[i]]$new_Q[[Ri]][[neighbCase]], test_info[[i-1]]$new_Q[[Ri]][[neighbCase]]),
                        info = "$new_Q not updated with $eqFreqsChange TRUE in IWE_evol of isolated singleStructure instance b)")
          }
        }
      }

    } else {
      expect_false(test_info[[i]]$eqFreqsChange)
      expect_equal(length(test_info[[i]]), 3,
                   info = "IWE output length does not correspond with $eqFreqsChange FALSE in IWE_evol of isolated singleStructure instance b)")
    }
    if (i>=2){
      expect_equal(test_info[[i]]$old_eqFreqs, test_info[[i-1]]$new_eqFreqs,
                   info = "old_eqFreqs after IWE does not correspond with new_eqFreqs in previous IWE in IWE_evol of isolated singleStructure instance b)")
      expect_equal(test_info[[i]]$old_Q, test_info[[i-1]]$new_Q,
                   info = "old_Q after IWE does not correspond with new_Q in previous IWE in IWE_evol of isolated singleStructure instance b)")
    }

  }
  expect_equal(test_info[[1]]$old_eqFreqs, get_private(original_obj)$eqFreqs,
               info = "old_eqFreqs after first IWE does not correspond with eqFreqs original object in IWE_evol of isolated singleStructure instance b)")
  expect_equal(test_info[[number_IWEs]]$new_eqFreqs, get_private(obj)$eqFreqs,
               info = "old_eqFreqs after last IWE does not correspond with eqFreqs modified object in IWE_evol of isolated singleStructure instance b)")

  expect_equal(test_info[[1]]$old_Q, get_private(original_obj)$Q,
               info = "old_Q after first IWE does not correspond with $Q original object in IWE_evol of isolated singleStructure instance b)")
  expect_equal(test_info[[number_IWEs]]$new_Q, get_private(obj)$Q,
               info = "new_Q after last IWE does not correspond with $Q modified object in IWE_evol of isolated singleStructure instance b)")


})


test_that("combiStructureGenerator get_island_number()",{
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_equal(combi_obj$get_island_number(), 1)
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("U", "U", "U"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_equal(combi_obj$get_island_number(), 3)
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_equal(combi_obj$get_island_number(), 0)
})

test_that("combiStructureGenerator get_island_index()",{
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_equal(combi_obj$get_island_index(), 2)
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("U", "U", "U"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_equal(combi_obj$get_island_index(), c(1,2,3))
})

test_that("combiStructureGenerator $set_IWE_rate()",{
  # Test 1
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  mu <- get_private(combi_obj)$mu
  island_number <- 1
  expected <- mu * island_number
  expect_equal(get_private(combi_obj)$IWE_rate, expected, info ="test 1 fails")
  # Test 2
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("U", "U", "U"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  island_number <- 3
  expected <- mu * island_number
  expect_equal(get_private(combi_obj)$IWE_rate, expected, info ="test 2 fails")
  # Test 3
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  island_number <- 0
  expected <- mu * island_number
  expect_equal(get_private(combi_obj)$IWE_rate, expected, info ="test 3 fails")

})

test_that("combiStructureGenerator $IWE_events",{
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_null(combi_obj$get_IWE_events(), info = "$IWE_events not initialized as NULL")
  combi_obj$set_IWE_events("booh")
  expect_equal(combi_obj$get_IWE_events(), "booh", info = "$get_IWE_events() does not return info assigned with $set_IWE_events()")
})

test_that("combiStructureGenerator $branch_evol()", {
  # Testing mode:
  # No islands: Simulation without IWEs
  infoStr <- data.frame(n = c(100, 100, 100, 100, 100, 100, 100),
                        globalState= c("M", "M", "M", "M", "M", "M", "M"))

  obj <- combiStructureGenerator$new(infoStr)
  branchLength <- 1
  output <- obj$branch_evol(branch_length = branchLength, dt = 0.01, testing=T)
  expect_false(output$IWE_event, info ="without islands it samples IWE_events")
  expect_equal(obj$get_IWE_events(), "Simulation without IWE events.")

  # Long branch length to favor IWE events
  infoStr <- data.frame(n = c(100, 100, 100, 100, 100, 100, 100),
                        globalState= c("M", "U", "M", "U", "U", "U", "U"))

  obj <- combiStructureGenerator$new(infoStr)
  branchLength <- 10
  output <- obj$branch_evol(branch_length = branchLength, dt = 0.01, testing=T)
  if(output$IWE_event){
    expect_true(all(output$IWE_times < branchLength),
                info = "not all IWE_times < branch_length when IWE_event TRUE")
    expect_true(length(output$SSE_intervals) == length(output$IWE_times)+1,
                info = "number of intervals not equal to number of IWE_t + 1 when IWE_event TRUE")
    expect_equal(sum(output$SSE_intervals), branchLength,
                 info = "sum of SSE_interval lengths not equal to branch length when IWE_event TRUE")
    expect_equal(length(output$infoIWE), length(output$IWE_times),
                 info = "infoIWE length not equal to IWE_times length when IWE_event TRUE")
    expect_equal(obj$get_IWE_events()$times, output$IWE_times,
                 info = "$get_IWE_events()$times not equal to testing output IWE_times when IWE_event TRUE")
    expect_equal(obj$get_IWE_events()$islands, output$islands,
                 info = "$get_IWE_events()$islands not equal to testing output IWE_times when IWE_event TRUE")
  } else {
    expect_equal(length(output$IWE_times), 1,
                 info = "length of IWE times not 1 when IWE_event FALSE")
    expect_true(output$IWE_times>branchLength,
                info = "IWE_t is not bigger than branch_length when IWE_event FALSE")
    expect_false(obj$get_IWE_events(),
                 info = "$IWE_events not FALSE when IWE_event FALSE")
  }

  # Short branch length to favor no IWE events
  infoStr <- data.frame(n = c(100, 100, 100, 100, 100, 100, 100),
                        globalState= c("M", "U", "M", "U", "U", "U", "U"))

  obj <- combiStructureGenerator$new(infoStr)
  branchLength <- 1
  output <- obj$branch_evol(branch_length = branchLength, dt = 0.01, testing=T)
  if(output$IWE_event){
    expect_true(all(output$IWE_times < branchLength),
                info = "not all IWE_times < branch_length when IWE_event TRUE")
    expect_true(length(output$SSE_intervals) == length(output$IWE_times)+1,
                info = "number of intervals not equal to number of IWE_t + 1 when IWE_event TRUE")
    expect_equal(sum(output$SSE_intervals), branchLength,
                 info = "sum of SSE_interval lengths not equal to branch length when IWE_event TRUE")
    expect_equal(length(output$infoIWE), length(output$IWE_times),
                 info = "infoIWE length not equal to IWE_times length when IWE_event TRUE")
    expect_equal(obj$get_IWE_events()$times, output$IWE_times,
                 info = "$get_IWE_events()$times not equal to testing output IWE_times when IWE_event TRUE")
    expect_equal(obj$get_IWE_events()$islands, output$islands,
                 info = "$get_IWE_events()$islands not equal to testing output IWE_times when IWE_event TRUE")
  } else {
    expect_equal(length(output$IWE_times), 1,
                 info = "length of IWE times not 1 when IWE_event FALSE")
    expect_true(output$IWE_times>branchLength,
                info = "IWE_t is not bigger than branch_length when IWE_event FALSE")
    expect_false(obj$get_IWE_events(),
                 info = "$IWE_events not FALSE when IWE_event FALSE")
  }

  # Test that by default (testing = FALSE) the function returns nothing
  output <- obj$branch_evol(branch_length = branchLength, dt = 0.01)
  expect_null(output)
})

test_that("combiStructureGenerator $set_name() and $get_name()", {
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_null(combi_obj$get_name(), info = "$name is not itialized as null")
  combi_obj$set_name("name")
  expect_equal(combi_obj$get_name(), "name", info = "$set_name() argument does not correspond with $get_name() output")
})

test_that("combiStructureGenerator $set_own_index() and $get_own_index()", {
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_null(combi_obj$get_own_index(), info = "$own_index is not itialized as null")
  combi_obj$set_own_index(1)
  expect_equal(combi_obj$get_own_index(), 1, info = "$set_own_index() argument does not correspond with $get_own_index() output")
})

test_that("combiStructureGenerator $set_offspring_index() and $get_offspring_index()", {
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_null(combi_obj$get_offspring_index(), info = "$offspring_index is not itialized as null")
  combi_obj$set_offspring_index(1)
  expect_equal(combi_obj$get_offspring_index(), 1, info = "$set_offspring_index() argument does not correspond with $get_offspring_index() output")
})

test_that("combiStructureGenerator $add_offspring_index()", {
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_null(combi_obj$get_offspring_index(), info = "$offspring_index is not itialized as null")
  combi_obj$add_offspring_index(1)
  expect_equal(combi_obj$get_offspring_index(), 1, info = "first offspring index")
  combi_obj$add_offspring_index(2)
  expect_equal(combi_obj$get_offspring_index(), c(1,2), info = "second offspring index")
})

test_that("combiStructureGenerator $set_parent_index() and $get_parent_index()", {
  infoStr <- data.frame(n = c(13, 1, 5),
                        globalState = c("M", "M", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_null(combi_obj$get_parent_index(), info = "$parent_index is not itialized as null")
  combi_obj$set_parent_index(1)
  expect_equal(combi_obj$get_parent_index(), 1, info = "$set_parent_index() argument does not correspond with $get_parent_index() output")
})

test_that("split_newick correctly splits Newick tree", {

  # Test case 1: Test with a simple tree
  tree <- "(a:1, c:2, (d:3.7, e:4):5);"
  expected_output <- data.frame(
    "unit" = c("a", "c", "(d:3.7, e:4)"),
    "brlen" = c(1, 2, 5),
    stringsAsFactors = FALSE
  )
  expect_output <- split_newick(tree)
  expect_true(is.data.frame(expect_output),
              info = "Test case 1: Output should be a data frame.")
  expect_equal(ncol(expect_output), 2,
               info = "Test case 1: Output should have 2 columns.")
  expect_true(is.character(expect_output$unit),
              info = "Test case 1: 'unit' column should be character type.")
  expect_true(is.numeric(expect_output$brlen),
              info = "Test case 1: 'brlen' column should be numeric type.")
  expect_equal(expect_output, expected_output,
               info = "Test case 1: Output should match the expected result.")

  # Test case 2: Test with a subtree
  tree <- "(d:3.7, e:4)"
  expected_output <- data.frame(
    "unit" = c("d", "e"),
    "brlen" = c(3.7, 4),
    stringsAsFactors = FALSE
  )
  expect_output <- split_newick(tree)
  expect_true(is.data.frame(expect_output),
              info = "Test case 2: Output should be a data frame.")
  expect_equal(ncol(expect_output), 2,
               info = "Test case 2: Output should have 2 columns.")
  expect_true(is.character(expect_output$unit),
              info = "Test case 2: 'unit' column should be character type.")
  expect_true(is.numeric(expect_output$brlen),
              info = "Test case 2: 'brlen' column should be numeric type.")
  expect_equal(expect_output, expected_output,
               info = "Test case 2: Output should match the expected result.")

  # Test case 3: Test with a more complex tree
  tree <- "((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);"
  expected_output <- data.frame(
    "unit" = c("(a:1, b:1)", "c", "(d:3.7, (e:4, f:1):3)"),
    "brlen" = c(2, 2, 5),
    stringsAsFactors = FALSE
  )
  expect_output <- split_newick(tree)
  expect_true(is.data.frame(expect_output),
              info = "Test case 3: Output should be a data frame.")
  expect_equal(ncol(expect_output), 2,
               info = "Test case 3: Output should have 2 columns.")
  expect_true(is.character(expect_output$unit),
              info = "Test case 3: 'unit' column should be character type.")
  expect_true(is.numeric(expect_output$brlen),
              info = "Test case 3: 'brlen' column should be numeric type.")
  expect_equal(expect_output, expected_output,
               info = "Test case 3: Output should match the expected result.")

  # Test case 4: Test with complex tree tip names
  tree <- "(weird_name:1, c:2, (d:3.7, 25name_with_numbers0:4):5);"
  expected_output <- data.frame(
    "unit" = c("weird_name", "c", "(d:3.7, 25name_with_numbers0:4)"),
    "brlen" = c(1, 2, 5),
    stringsAsFactors = FALSE
  )
  expect_output <- split_newick(tree)
  expect_true(is.data.frame(expect_output),
              info = "Test case 4: Output should be a data frame.")
  expect_equal(ncol(expect_output), 2,
               info = "Test case 4: Output should have 2 columns.")
  expect_true(is.character(expect_output$unit),
              info = "Test case 4: 'unit' column should be character type.")
  expect_true(is.numeric(expect_output$brlen),
              info = "Test case 4: 'brlen' column should be numeric type.")
  expect_equal(expect_output, expected_output,
               info = "Test case 4: Output should match the expected result.")

  # Test case 5: Test with non integer branch lengths
  tree <- "(a:0.169, c:2, (d:3.782, e:4):5.01);"
  expected_output <- data.frame(
    "unit" = c("a", "c", "(d:3.782, e:4)"),
    "brlen" = c(0.169, 2, 5.01),
    stringsAsFactors = FALSE
  )
  expect_output <- split_newick(tree)
  expect_true(is.data.frame(expect_output),
              info = "Test case 5: Output should be a data frame.")
  expect_equal(ncol(expect_output), 2,
               info = "Test case 5: Output should have 2 columns.")
  expect_true(is.character(expect_output$unit),
              info = "Test case 5: 'unit' column should be character type.")
  expect_true(is.numeric(expect_output$brlen),
              info = "Test case 5: 'brlen' column should be numeric type.")
  expect_equal(expect_output, expected_output,
               info = "Test case 5: Output should match the expected result.")

  # Test case 6: Test with a tree with different spacing pattern
  tree <- "( a:1, c: 2, (d:3.7, e :4):5);"
  expected_output <- data.frame(
    "unit" = c("a", "c", "(d:3.7, e :4)"),
    "brlen" = c(1, 2, 5),
    stringsAsFactors = FALSE
  )
  expect_output <- split_newick(tree)
  expect_true(is.data.frame(expect_output),
              info = "Test case 6: Output should be a data frame.")
  expect_equal(ncol(expect_output), 2,
               info = "Test case 6: Output should have 2 columns.")
  expect_true(is.character(expect_output$unit),
              info = "Test case 6: 'unit' column should be character type.")
  expect_true(is.numeric(expect_output$brlen),
              info = "Test case 6: 'brlen' column should be numeric type.")
  expect_equal(expect_output, expected_output,
               info = "Test case 6: Output should match the expected result.")
})

test_that("treeMultiRegionSimulator", {
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"))
  message <- capture.output(treeData <- treeMultiRegionSimulator$new(infoStr, tree = "(a:1, c:2, (d:3.7, e:4):5);"), type = "message")
  expect_equal(message, "Simulating data at root and letting it evolve along given tree:  (a:1, c:2, (d:3.7, e:4):5);")
  expect_equal(length(treeData$Branch), 6, info = "Generates incorrect number of branches")
  expect_equal(length(treeData$Branch), 6, info = "Generates incorrect number of branches")
  expect_null(treeData$Branch[[1]]$get_parent_index(), info = "Data at root has not null parent_index")
  expect_equal(treeData$Branch[[2]]$get_parent_index(), 1, info ="Assigns incorrect parent_index")
  expect_equal(treeData$Branch[[3]]$get_parent_index(), 1, info ="Assigns incorrect parent_index")
  expect_equal(treeData$Branch[[4]]$get_parent_index(), 1, info ="Assigns incorrect parent_index")
  expect_equal(treeData$Branch[[1]]$get_offspring_index(), c(2,3,4), info = "Assigns incorrect offspring_index")
  expect_null(treeData$Branch[[2]]$get_offspring_index(), info = "Assigns incorrect offspring_index")
  expect_null(treeData$Branch[[3]]$get_offspring_index(), info = "Assigns incorrect offspring_index")
  expect_equal(treeData$Branch[[4]]$get_offspring_index(), c(5,6), info = "Assigns incorrect offspring_index")
  expect_null(treeData$Branch[[5]]$get_offspring_index(), info = "Assigns incorrect offspring_index")
  expect_null(treeData$Branch[[6]]$get_offspring_index(), info = "Assigns incorrect offspring_index")
  expect_null(treeData$Branch[[1]]$get_name(), info = "Assigns incorrect name")
  expect_equal(treeData$Branch[[2]]$get_name(), "a", info = "Assigns incorrect name")
  expect_equal(treeData$Branch[[3]]$get_name(), "c", info = "Assigns incorrect name")
  expect_null(treeData$Branch[[4]]$get_name(), info = "Assigns incorrect name")
  expect_equal(treeData$Branch[[5]]$get_name(), "d", info = "Assigns incorrect name")
  expect_equal(treeData$Branch[[6]]$get_name(), "e", info = "Assigns incorrect name")
  expect_equal(length(treeData$branchLength), 6, info = "Generates incorrect number of branchLength")
  expect_equal(treeData$branchLength[1], NA_real_, info = "Assigns incorrect branchLength")
  expect_equal(treeData$branchLength[2], 1, info = "Assigns incorrect branchLength")
  expect_equal(treeData$branchLength[3], 2, info = "Assigns incorrect branchLength")
  expect_equal(treeData$branchLength[4], 5, info = "Assigns incorrect branchLength")
  expect_equal(treeData$branchLength[5], 3.7, info = "Assigns incorrect branchLength")
  expect_equal(treeData$branchLength[6], 4, info = "Assigns incorrect branchLength")
  expect_null(treeData$Branch[[1]]$get_IWE_events(), info = "Data at root has not null IWE_events")
  for (branch in 2:6){
    expect_false(is.null(treeData$Branch[[branch]]$get_IWE_events()), info = "Branches should have $IWE_events FALSE/vector of times")
  }
  for (branch in 1:6){
    expect_equal(treeData$Branch[[branch]]$get_singleStr_number(), 3, info = "Branches have incorrect number of singleStructures")
    for (str in 1:3){
      expect_equal(length(treeData$Branch[[branch]]$get_singleStr(str)$get_seq()), 100, info ="Incorrect sequence length")
    }
  }
  
  # Expect objects with correct combiStructure ID
  # Initiate an instance to reset shared counter
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  # Reset shared counter
  c$reset_sharedCounter()
  # Expected id when Class is initiated for a given infoStr
  message <- capture.output(treeData <- treeMultiRegionSimulator$new(infoStr, tree = "(a:1, c:2, (d:3.7, e:4):5);"), type = "message")
  expect_equal(treeData$Branch[[1]]$get_id(), 1, 
               info = "root ID not equal to one after resetting counter")
  expect_equal(treeData$Branch[[2]]$get_id(), 2,
               info = "branch 2 ID not equal to 2")
  expect_equal(treeData$Branch[[3]]$get_id(), 3,
               info = "branch 3 ID not equal to 3")
  expect_equal(treeData$Branch[[4]]$get_id(), 4,
               info = "branch 4 ID not equal to 4")
  expect_equal(treeData$Branch[[5]]$get_id(), 5,
               info = "branch 5 ID not equal to 5")
  expect_equal(treeData$Branch[[6]]$get_id(), 6,
               info = "branch 6 ID not equal to 6")
  for (i in 1:3){
    expect_equal(get_private(treeData$Branch[[1]]$get_singleStr(i))$my_combiStructure$get_id(), 1,
                 info = paste("root singleStr", i, "does not point to correct my_combiStructure"))
    expect_equal(get_private(treeData$Branch[[2]]$get_singleStr(i))$my_combiStructure$get_id(), 2,
                 info = paste("branch 2 singleStr", i, "does not point to correct my_combiStructure"))
    expect_equal(get_private(treeData$Branch[[3]]$get_singleStr(i))$my_combiStructure$get_id(), 3,
                 info = paste("branch 3 singleStr", i, "does not point to correct my_combiStructure"))
    expect_equal(get_private(treeData$Branch[[4]]$get_singleStr(i))$my_combiStructure$get_id(), 4,
                 info = paste("branch 4 singleStr", i, "does not point to correct my_combiStructure"))
    expect_equal(get_private(treeData$Branch[[5]]$get_singleStr(i))$my_combiStructure$get_id(), 5,
                 info = paste("branch 5 singleStr", i, "does not point to correct my_combiStructure"))
    expect_equal(get_private(treeData$Branch[[6]]$get_singleStr(i))$my_combiStructure$get_id(), 6,
                 info = paste("branch 6 singleStr", i, "does not point to correct my_combiStructure"))
  }
  # Initiate an instance to reset shared counter
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  # Reset shared counter
  c$reset_sharedCounter()
  c <- combiStructureGenerator$new(infoStr)
  # Expected id when Class is initiated for a given rootData
  message <- capture.output(treeData <- treeMultiRegionSimulator$new(rootData = c, tree = "(a:1, c:2, (d:3.7, e:4):5);"), type = "message")
  expect_equal(treeData$Branch[[1]]$get_id(), 2, 
               info = "root ID not equal to one after resetting counter and initiating with given rootData")
  expect_equal(treeData$Branch[[2]]$get_id(), 3,
               info = "branch 2 ID not equal to 3 after initiating with given rootData")
  expect_equal(treeData$Branch[[3]]$get_id(), 4,
               info = "branch 3 ID not equal to 4 after initiating with given rootData")
  expect_equal(treeData$Branch[[4]]$get_id(), 5,
               info = "branch 4 ID not equal to 5 after initiating with given rootData")
  expect_equal(treeData$Branch[[5]]$get_id(), 6,
               info = "branch 5 ID not equal to 6 after initiating with given rootData")
  expect_equal(treeData$Branch[[6]]$get_id(), 7,
               info = "branch 6 ID not equal to 7 after initiating with given rootData")
  for (i in 1:3){
    expect_equal(get_private(treeData$Branch[[1]]$get_singleStr(i))$my_combiStructure$get_id(), 2,
                 info = paste("root singleStr", i, "does not point to correct my_combiStructure after initiating with given rootData"))
    expect_equal(get_private(treeData$Branch[[2]]$get_singleStr(i))$my_combiStructure$get_id(), 3,
                 info = paste("branch 2 singleStr", i, "does not point to correct my_combiStructure after initiating with given rootData"))
    expect_equal(get_private(treeData$Branch[[3]]$get_singleStr(i))$my_combiStructure$get_id(), 4,
                 info = paste("branch 3 singleStr", i, "does not point to correct my_combiStructure after initiating with given rootData"))
    expect_equal(get_private(treeData$Branch[[4]]$get_singleStr(i))$my_combiStructure$get_id(), 5,
                 info = paste("branch 4 singleStr", i, "does not point to correct my_combiStructure after initiating with given rootData"))
    expect_equal(get_private(treeData$Branch[[5]]$get_singleStr(i))$my_combiStructure$get_id(), 6,
                 info = paste("branch 5 singleStr", i, "does not point to correct my_combiStructure after initiating with given rootData"))
    expect_equal(get_private(treeData$Branch[[6]]$get_singleStr(i))$my_combiStructure$get_id(), 7,
                 info = paste("branch 6 singleStr", i, "does not point to correct my_combiStructure after initiating with given rootData"))
  }
})

test_that("customized params", {
  # From singleStructure initialization
  params <- get_parameterValues()
  params$alpha_pI <- 0.25
  obj <- singleStructureGenerator$new("U", 10, params = params)
  expect_equal(obj$get_alpha_pI(), 0.25, info = "fails to initiate singleStructure with new parameter value")

  # From combiStructure initialization
  params <- get_parameterValues()
  params$alpha_Ri <- 0.3
  params$mu <- 0.05
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr, params)
  expect_equal(combi_obj$get_singleStr(1)$get_alpha_Ri(), 0.3,
               info = "fails to initiate first structure initialized within combiStructure with new parameter value")
  expect_equal(combi_obj$get_singleStr(2)$get_alpha_Ri(), 0.3,
               info = "fails to initiate second structure initialized within combiStructure with new parameter value")
  expect_equal(combi_obj$get_singleStr(3)$get_alpha_Ri(), 0.3,
               info = "fails to initiate third structure initialized within combiStructure with new parameter value")
  expect_equal(combi_obj$get_mu(), 0.05,
               info ="fails to initiate combiStructure with new parameter value")

  # From treeMultiRegionSimulator initialization
  params <- get_parameterValues()
  params$mu <- 0.002
  params$iota <- 0.1
  message <- capture.output(treeData <- treeMultiRegionSimulator$new(infoStr = infoStr, tree = "(a:1, c:2, (d:3.7, e:4):5);", params = params), type = "message")
  expect_equal(message, "Simulating data at root and letting it evolve along given tree:  (a:1, c:2, (d:3.7, e:4):5);")
  for (br in 1:6){
    expect_equal(treeData$Branch[[br]]$get_mu(), 0.002,
                 info = paste("fails to initiate new parameter value in combiStructure initialized from treeMultiRegionSimulator, branch number: ", i))
    for(str in 1:3){
      expect_equal(treeData$Branch[[br]]$get_singleStr(str)$get_iota(), 0.1,
                   info = paste("fails to initiate new parameter value in singleStr", str, "of branch", br, "initialized from treeMultiRegionSimulator"))
    }
  }

})

test_that("fixed eqFreqs",{
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"),
                        u_eqFreq = c(0.1, 0.8, 0.1),
                        p_eqFreq = c(0.1, 0.1, 0.1),
                        m_eqFreq = c(0.8, 0.1, 0.8))
  combi_obj <- combiStructureGenerator$new(infoStr = infoStr)
  for (i in 1:nrow(infoStr)){
    expect_equal(combi_obj$get_singleStr(i)$get_eqFreqs(), c(infoStr$u_eqFreq[i], infoStr$p_eqFreq[i], infoStr$m_eqFreq[i]),
                 info = "does not assign correct eqFreqs when given. Generated from combiStructureGenerator")
  }
  silence <- capture.output(tree_obj <- treeMultiRegionSimulator$new(infoStr = infoStr, tree = "(a:1, c:2, (d:3.7, e:4):5);"), type = "message")
  for (i in 1:nrow(infoStr)){
    expect_equal(tree_obj$Branch[[1]]$get_singleStr(i)$get_eqFreqs(), c(infoStr$u_eqFreq[i], infoStr$p_eqFreq[i], infoStr$m_eqFreq[i]),
                 info = "does not assign correct eqFreqs when given. Generated from combiStructureGenerator")
  }

  # Incorrect frequencies or missing values
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"),
                        u_eqFreq = c(0.1, 0.8, 0.1),
                        p_eqFreq = c(NA, 0.1, 0.1),
                        m_eqFreq = c(0.8, 0.1, 0.8))
  expect_error(capture.output(combiStructureGenerator$new(infoStr = infoStr), type = "message"),
               info = "fails throwing an error when eqFreqs have missing values. Generated from combiStructureGenerator")
  expect_error(capture.output(treeMultiRegionSimulator$new(infoStr = infoStr, tree = "(a:1, c:2, (d:3.7, e:4):5);"), type = "message"),
               info = "fails throwing an error when eqFreqs have missing values. Generated from treeMultiRegionSimulator")
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"),
                        u_eqFreq = c(0.1, 0.8, 0.1),
                        p_eqFreq = c(0, 0.1, 0.1),
                        m_eqFreq = c(0.8, 0.1, 0.8))
  expect_error(combiStructureGenerator$new(infoStr = infoStr),
               info = "fails throwing an error when eqFreqs wrong frequencies. Generated from combiStructureGenerator")
  expect_error(capture.output(treeMultiRegionSimulator$new(infoStr = infoStr, tree = "(a:1, c:2, (d:3.7, e:4):5);"), type = "message"),
               info = "fails throwing an error when eqFreqs wrong frequencies. Generated from treeMultiRegionSimulator")
})

test_that("CFTP: singleStructureGenerator $get_Qi()",{
  
  # Initialize singleStructureGenerator instance
  single_obj <- singleStructureGenerator$new("U",10)
  
  # Get list of 3 SSEi rate matrices
  SSEi_3mat <- single_obj$get_Qi()
  
  expect_equal(length(SSEi_3mat), 3,
               info = "method with null arguments does not return 3 objects")
  expect_true(is.matrix(SSEi_3mat[[1]]),
              info = "method with null arguments does not return a list of matrices")
  expect_true(is.matrix(SSEi_3mat[[2]]),
              info = "method with null arguments does not return a list of matrices")
  expect_true(is.matrix(SSEi_3mat[[3]]),
              info = "method with null arguments does not return a list of matrices")
  
  # Get rate for serveral cases
  SSEi_111 <- single_obj$get_Qi(siteR = 1, oldSt = 1, newSt = 1)
  SSEi_213 <- single_obj$get_Qi(siteR = 2, oldSt = 1, newSt = 3)
  SSEi_323 <- single_obj$get_Qi(siteR = 3, oldSt = 2, newSt = 3)
  
  expect_equal(SSEi_111, SSEi_3mat[[1]][1,1],
               info = "method with arguments does not return rate as in matrices")
  expect_equal(SSEi_213, SSEi_3mat[[2]][1,3],
               info = "method with arguments does not return rate as in matrices")
  expect_equal(SSEi_323, SSEi_3mat[[3]][2,3],
               info = "method with arguments does not return rate as in matrices")
})

test_that("CFTP: singleStructureGenerator $get_seqSt_leftneighb() and $get_seqSt_rightneighb()",{
  
  # Initialize singleStructureGenerator instance
  single_obj <- singleStructureGenerator$new("U",100)
  
  # Expect error when function is called without giving a value for the index 
  expect_error(single_obj$get_seqSt_leftneighb(),
               info = "method fails to throw error when no value for 'index' argument is given")
  expect_error(single_obj$get_seqSt_rightneighb(),
               info = "method fails to throw error when no value for 'index' argument is given")
  
  # Expect error when function is called with a non-valid index value
  expect_error(single_obj$get_seqSt_leftneighb(index = "U"),
               info = "method fails to throw error when 'index' argument is not one integer value")
  expect_error(single_obj$get_seqSt_rightneighb(index = "U"),
               info = "method fails to throw error when 'index' argument is not one integer value")
  expect_error(single_obj$get_seqSt_leftneighb(index = c(1,2)),
               info = "method fails to throw error when 'index' argument is not one integer value")
  expect_error(single_obj$get_seqSt_rightneighb(index = c(1,2)),
               info = "method fails to throw error when 'index' argument is not one integer value")
  expect_error(single_obj$get_seqSt_leftneighb(index = 1.5),
               info = "method fails to throw error when 'index' argument is not one integer value")
  expect_error(single_obj$get_seqSt_rightneighb(index = 1.5),
               info = "method fails to throw error when 'index' argument is not one integer value")
  
  # Expect error when function is called with an index value not within sequence length
  expect_error(single_obj$get_seqSt_leftneighb(index = -1),
               info = "method fails to throw error when 'index' argument is not within sequence length")
  expect_error(single_obj$get_seqSt_rightneighb(index = 200),
               info = "method fails to throw error when 'index' argument is not within sequence length")
  
  # Test that the sequence state for each site's left and right neighbor is correct
  for (s in 2:10){
    expect_equal(single_obj$get_seqSt_leftneighb(s), single_obj$get_seq()[s-1],
                 info = "sequence methylation state obtained with get_leftneighbSt is not correct")
  }
  for (s in 1:9){
    expect_equal(single_obj$get_seqSt_rightneighb(s), single_obj$get_seq()[s+1],
                 info = "neighbSt obtained with get_rightneighbSt is not correct")
  }
})

test_that("singleStructureGenerator $cftp_all_equal()",{
  
  # Initialize singleStructureGenerator instance
  single_obj <- singleStructureGenerator$new("U",10)
  
  # Expect error when function is called without giving a value for the 'state' argument
  expect_error(single_obj$cftp_all_equal(),
               info = "method fails to throw error when no value for 'state' argument is given")
  
  # Expect error when value of argument 'state' is not correct
  expect_error(single_obj$cftp_all_equal(state=1),
               info = "method fails to throw error when 'state' value is not correct")
  
  # Expect NULL output when state is correct but testing is (as default) FALSE
  expect_null(single_obj$cftp_all_equal(state="U"),
              info = "whith testing = FALSE method generates output")
  
  # Check generated sequence for both possible states
  expect_equal(single_obj$cftp_all_equal(state = "U", testing = TRUE), rep(1, 10),
               info = "method fails to assign sequence of 'u' states")
  expect_equal(single_obj$cftp_all_equal(state = "M", testing = TRUE), rep(3, 10),
               info = "method fails to assign sequence of 'm' states")
  
})

test_that("singleStructureGenerator $set_seqSt_update_neighbSt",{
  
  # Initialize singleStructureGenerator instance
  single_obj <- singleStructureGenerator$new("U",10)
  
  # Expect error when either index or newSt arguments are not given
  expect_error(single_obj$set_seqSt_update_neighbSt(newSt = 2),
               info = "method fails to throw error when 'index' argument is not given")
  expect_error(single_obj$set_seqSt_update_neighbSt(index = 2),
               info = "method fails to throw error when 'newSt' argument is not given")
  
  # Expect error when index is not one numerical value or not a value without decimals
  expect_error(single_obj$set_seqSt_update_neighbSt(index = c(1,10), newSt = 2),
               info = "method fails to throw error when 'index' argument is not one numerical value")
  expect_error(single_obj$set_seqSt_update_neighbSt(index = 1.5, newSt = 2),
               info = "method fails to throw error when 'index' argument has decimals")
  
  # Expect error when newSt is not 1, 2 or 3 (for unmethylated, partially-methylated or methylated)
  expect_error(single_obj$set_seqSt_update_neighbSt(index = 1, newSt = 4),
               info = "method fails to throw error when 'newSt' is not 1, 2 or 3")
  
  # Expect error when index given does not exist within the singleStructure instance length
  expect_error(single_obj$set_seqSt_update_neighbSt(index = 11, newSt = 2),
               info = "method fails to throw error when 'index' is not within sequence length")
  
  # Expect NULL output when arguments are correct but testing is (as default) FALSE
  expect_null(single_obj$set_seqSt_update_neighbSt(index = 2, newSt = 2),
              info = "whith testing = FALSE method generates output")
  
  # Test sequence state is correctly changed
  output <- single_obj$set_seqSt_update_neighbSt(index = 4, newSt = 3, testing = TRUE)
  expect_equal(output$seq[4], 3,
               info = "method fails to assign new methylaton state to the given index")
  
  # Test neighbSt is correctly updated
  mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3)
  exp3rdsite_neighbSt <- mapNeighbSt_matrix[output$seq[2], output$seq[4]]
  exp5thsite_neighbSt <- mapNeighbSt_matrix[output$seq[4], output$seq[6]]
  expect_equal(output$neighbSt[3], exp3rdsite_neighbSt,
               info = "method fails to update neighbSt to left neighbor")
  expect_equal(output$neighbSt[5], exp5thsite_neighbSt,
               info = "method fails to update neighbSt to right neighbor")
})

test_that("combiStructureGenerator $get_highest_rate", {
  
  # Initialize combiStructureGenerator instance
  infoStr <- data.frame(n = c(10, 10, 10),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  
  output <- c$get_highest_rate()
  
  expect_true(all(is.numeric(output), length(output) == 1, output > 0))
})

test_that("combiStructureGenerator $get_singleStr_siteNumber and $get_singleStr_number", {
  
  # test 1
  test <- 1
  infoStr <- data.frame(n = c(10, 8, 15),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  
  expect_equal(c$get_singleStr_number(), length(infoStr$n),
               info = paste("Incorrect number of singleStr instances in test:", test))
  expect_equal(c$get_singleStr_siteNumber(), infoStr$n,
               info = paste("Incorrect number of sites in singleStr instances in test:", test))
  
  # test 2
  test <- 2
  infoStr <- data.frame(n = c(25, 78, 1, 16, 88, 40),
                        globalState = c("M", "U", "M", "M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  
  expect_equal(c$get_singleStr_number(), length(infoStr$n),
               info = paste("Incorrect number of singleStr instances in test:", test))
  expect_equal(c$get_singleStr_siteNumber(), infoStr$n,
               info = paste("Incorrect number of sites in singleStr instances in test:", test))
  
})

test_that("cftpStepGenerator $new", {
  
  cftp <- cftpStepGenerator$new(singleStr_number = 3, singleStr_siteNumber = c(10, 8, 15), CFTP_highest_rate = 0.8)
  
  expect_equal(cftp$singleStr_number, 3,
               info = "Incorrect number of singleStr instances")
  expect_equal(cftp$singleStr_siteNumber, c(10, 8, 15),
               info = "Incorrect number of sites in singleStr instances")
  expect_equal(cftp$CFTP_highest_rate,  0.8,
               info = "Incorrect value of CFTP highest rate")
  expect_equal(cftp$number_steps, 0,
               info = "Incorrect number of steps")
  expect_true(is.null(cftp$steps_perVector),
              info = "Not null value of steps per vector at initialization")
  
  expect_true(is.list(cftp$CFTP_chosen_singleStr),
              info = "Attribute $CFTP_chosen_singleStr not initialized as list")
  expect_equal(length(cftp$CFTP_chosen_singleStr), 0,
               info = "Attribute $CFTP_chosen_singleStr not initialized with length 0")
  
  expect_true(is.list(cftp$CFTP_chosen_site),
              info = "Attribute $CFTP_chosen_site not initialized as list")
  expect_equal(length(cftp$CFTP_chosen_site), 0,
               info = "Attribute $CFTP_chosen_site not initialized with length 0")
  
  expect_true(is.list(cftp$CFTP_event),
              info = "Attribute $CFTP_event not initialized as list")
  expect_equal(length(cftp$CFTP_event), 0,
               info = "Attribute $CFTP_event not initialized with length 0")
  
  expect_true(is.list(cftp$CFTP_random),
              info = "Attribute $CFTP_random not initialized as list")
  expect_equal(length(cftp$CFTP_random), 0,
               info = "Attribute $CFTP_random not initialized with length 0")
  
})

test_that("cftpStepGenerator $generate_events input control", {
  cftp <- cftpStepGenerator$new(singleStr_number = 3, singleStr_siteNumber = c(10, 8, 15), CFTP_highest_rate = 0.8)
  
  # Expect error when steps argument is not an non-decimal numerical value of min value 1
  expect_error(cftp$generate_events(steps = "5"),
               info = "method fails to throw error when 'steps' argument is non-numeric")
  expect_error(cftp$generate_events(steps = c(1,5)),
               info = "method fails to throw error when 'steps' argument is not of length 1")
  expect_error(cftp$generate_events(steps = 0),
               info = "method fails to throw error when 'steps' argument is < 1")
  expect_error(cftp$generate_events(steps = 100.5),
               info = "method fails to throw error when 'steps' argument has decimal numbers")
})

test_that("cftpStepGenerator $generate_events correctly initializes steps", {
  obj <- cftpStepGenerator$new(singleStr_number = 3, singleStr_siteNumber = c(10, 8, 15), CFTP_highest_rate = 0.8)
  steps <- 10000
  
  # Expect NULL output when arguments are correct but testing is (as default) FALSE
  expect_null(obj$generate_events(steps = steps),
              info = "whith testing = FALSE method generates output")
  
  obj <- cftpStepGenerator$new(singleStr_number = 3, singleStr_siteNumber = c(10, 8, 15), CFTP_highest_rate = 0.8)
  obj$generate_events(steps = steps)
  
  expect_equal(obj$steps_perVector, steps,
               info = "non-correct $steps_perVector after first generate_events call")
  expect_equal(obj$number_steps, steps,
               info = "non-correct number of steps after first generate_events call")
  expect_equal(length(obj$CFTP_chosen_singleStr), 1,
               info = "non-correct number of list elements in $CFTP_chosen_singleStr after first generate_events call")
  expect_equal(length(obj$CFTP_chosen_singleStr[[1]]), steps,
               info = "non-correct number of steps in $CFTP_chosen_singleStr after first generate_events call")
  expect_true(all(obj$CFTP_chosen_singleStr[[1]] %in% c(1,2,3)),
              info = "non-correct structure indices in $CFTP_chosen_singleStr after first generate_events call")
  expect_equal(length(obj$CFTP_chosen_site), 1,
               info = "non-correct number of list elements in $CFTP_chosen_site after first generate_events call")
  expect_equal(length(obj$CFTP_chosen_site[[1]]), steps,
               info = "non-correct number of steps in $CFTP_chosen_site after first generate_events call")
  expect_true(all(obj$CFTP_chosen_site[[1]] %in% 1:15),
              info = "non-correct site indices in $CFTP_chosen_site after first generate_events call")
  expect_equal(length(obj$CFTP_event), 1,
               info = "non-correct number of list elements in $CFTP_event after first generate_events call")
  expect_equal(length(obj$CFTP_event[[1]]), steps,
               info = "non-correct number of steps in $CFTP_event after first generate_events call")
  expect_true(all(obj$CFTP_event[[1]] %in% 1:5),
              info = "non-correct event encoding in $CFTP_event after first generate_events call")
  expect_equal(length(obj$CFTP_random), 1,
               info = "non-correct number of list elements in $CFTP_random after first generate_events call")
  expect_equal(length(obj$CFTP_random[[1]]), steps,
               info = "non-correct number of steps in $CFTP_random after first generate_events call")
  expect_true(all(obj$CFTP_random[[1]] >= 0 & obj$CFTP_random[[1]] <= 1),
              info = "non-correct values in $CFTP_random after first generate_events call")
})

test_that("cftpStepGenerator $generate_events prevents duplicate steps", {
  obj <- cftpStepGenerator$new(singleStr_number = 3, singleStr_siteNumber = c(10, 8, 15), CFTP_highest_rate = 0.8)
  obj$generate_events(steps = 10000)
  
  expect_error(obj$generate_events(steps = 5000), "The given number of steps has already been generated")
  expect_error(obj$generate_events(steps = 10000), "The given number of steps has already been generated")
})

test_that("cftpStepGenerator $generate_events handles step increments", {
  obj <- cftpStepGenerator$new(singleStr_number = 3, singleStr_siteNumber = c(10, 8, 15), CFTP_highest_rate = 0.8)
  steps <- 10000
  obj$generate_events(steps = steps)
  
  # Increment steps as in second cftp iteration
  steps <- steps*2
  obj$generate_events(steps = steps)
  
  expect_equal(obj$steps_perVector, 10000,
               info = "non-correct $steps_perVector after second generate_events call")
  expect_equal(obj$number_steps, steps,
               info = "non-correct number of steps after second generate_events call")
  expect_equal(length(obj$CFTP_chosen_singleStr), 2,
               info = "non-correct number of list elements in $CFTP_chosen_singleStr after second generate_events call")
  expect_equal(length(obj$CFTP_chosen_singleStr[[2]]), 10000,
               info = "non-correct number of steps in $CFTP_chosen_singleStr after second generate_events call")
  expect_true(all(obj$CFTP_chosen_singleStr[[2]] %in% c(1,2,3)),
              info = "non-correct structure indices in $CFTP_chosen_singleStr after second generate_events call")
  expect_equal(length(obj$CFTP_chosen_site), 2,
               info = "non-correct number of list elements in $CFTP_chosen_site after second generate_events call")
  expect_equal(length(obj$CFTP_chosen_site[[2]]), 10000,
               info = "non-correct number of steps in $CFTP_chosen_site after second generate_events call")
  expect_true(all(obj$CFTP_chosen_site[[2]] %in% 1:15),
              info = "non-correct site indices in $CFTP_chosen_site after second generate_events call")
  expect_equal(length(obj$CFTP_event), 2,
               info = "non-correct number of list elements in $CFTP_event after second generate_events call")
  expect_equal(length(obj$CFTP_event[[2]]), 10000,
               info = "non-correct number of steps in $CFTP_event after second generate_events call")
  expect_true(all(obj$CFTP_event[[1]] %in% 1:5),
              info = "non-correct event encoding in $CFTP_event after second generate_events call")
  expect_equal(length(obj$CFTP_random), 2,
               info = "non-correct number of list elements in $CFTP_random after second generate_events call")
  expect_equal(length(obj$CFTP_random[[2]]), 10000,
               info = "non-correct number of steps in $CFTP_random after second generate_events call")
  expect_true(all(obj$CFTP_random[[2]] >= 0 & obj$CFTP_random[[2]] <= 1),
              info = "non-correct values in $CFTP_random after second generate_events call")
  
  # Increment steps as in third cftp iteration
  steps <- steps*2
  obj$generate_events(steps = steps)
  
  expect_equal(obj$steps_perVector, 10000,
               info = "non-correct $steps_perVector after third generate_events call")
  expect_equal(obj$number_steps, steps,
               info = "non-correct number of steps after third generate_events call")
  expect_equal(length(obj$CFTP_chosen_singleStr), 4,
               info = "non-correct number of list elements in $CFTP_chosen_singleStr after third generate_events call")
  expect_equal(length(obj$CFTP_chosen_singleStr[[3]]), 10000,
               info = "non-correct number of steps in $CFTP_chosen_singleStr after third generate_events call")
  expect_equal(length(obj$CFTP_chosen_singleStr[[4]]), 10000,
               info = "non-correct number of steps in $CFTP_chosen_singleStr after third generate_events call")
  expect_true(all(obj$CFTP_chosen_singleStr[[3]] %in% c(1,2,3)),
              info = "non-correct structure indices in $CFTP_chosen_singleStr after third generate_events call")
  expect_true(all(obj$CFTP_chosen_singleStr[[4]] %in% c(1,2,3)),
              info = "non-correct structure indices in $CFTP_chosen_singleStr after third generate_events call")
  expect_equal(length(obj$CFTP_chosen_site), 4,
               info = "non-correct number of list elements in $CFTP_chosen_site after third generate_events call")
  expect_equal(length(obj$CFTP_chosen_site[[3]]), 10000,
               info = "non-correct number of steps in $CFTP_chosen_site after third generate_events call")
  expect_equal(length(obj$CFTP_chosen_site[[4]]), 10000,
               info = "non-correct number of steps in $CFTP_chosen_site after third generate_events call")
  expect_true(all(obj$CFTP_chosen_site[[3]] %in% 1:15),
              info = "non-correct site indices in $CFTP_chosen_site after third generate_events call")
  expect_true(all(obj$CFTP_chosen_site[[4]] %in% 1:15),
              info = "non-correct site indices in $CFTP_chosen_site after third generate_events call")
  expect_equal(length(obj$CFTP_event), 4,
               info = "non-correct number of list elements in $CFTP_event after third generate_events call")
  expect_equal(length(obj$CFTP_event[[3]]), 10000,
               info = "non-correct number of steps in $CFTP_event after third generate_events call")
  expect_equal(length(obj$CFTP_event[[4]]), 10000,
               info = "non-correct number of steps in $CFTP_event after third generate_events call")
  expect_true(all(obj$CFTP_event[[3]] %in% 1:5),
              info = "non-correct event encoding in $CFTP_event after third generate_events call")
  expect_true(all(obj$CFTP_event[[4]] %in% 1:5),
              info = "non-correct event encoding in $CFTP_event after third generate_events call")
  expect_equal(length(obj$CFTP_random), 4,
               info = "non-correct number of list elements in $CFTP_random after third generate_events call")
  expect_equal(length(obj$CFTP_random[[3]]), 10000,
               info = "non-correct number of steps in $CFTP_random after third generate_events call")
  expect_equal(length(obj$CFTP_random[[4]]), 10000,
               info = "non-correct number of steps in $CFTP_random after third generate_events call")
  expect_true(all(obj$CFTP_random[[3]] >= 0 & obj$CFTP_random[[3]] <= 1),
              info = "non-correct values in $CFTP_random after third generate_events call")
  expect_true(all(obj$CFTP_random[[4]] >= 0 & obj$CFTP_random[[4]] <= 1),
              info = "non-correct values in $CFTP_random after third generate_events call")
})


test_that("combiStructureGenerator $cftp initialization of cftpStepGenerator instance", {
  
  # test 1
  test <- 1
  infoStr <- data.frame(n = c(10, 8, 15),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  
  expect_true(is.null(c$get_CFTP_info()),
              info = paste("Not null info in combi instance before calling $cftp() in test:", test))
  
  c$cftp()
  
  expect_equal(class(c$get_CFTP_info())[1], "cftpStepGenerator")
  expect_equal(c$get_CFTP_info()$singleStr_number, length(infoStr$n),
               info = paste("Incorrect number of singleStr instances in test:", test))
  expect_equal(c$get_CFTP_info()$singleStr_siteNumber, infoStr$n,
               info = paste("Incorrect number of sites in singleStr instances in test:", test))
  
  clone <- c$copy()
  expect_equal(clone$get_CFTP_info()$singleStr_number, length(infoStr$n),
               info = paste("Incorrect number of singleStr instances in test:", test))
  expect_equal(clone$get_CFTP_info()$singleStr_siteNumber, infoStr$n,
               info = paste("Incorrect number of sites in singleStr instances in test:", test))
  
  
  # test 2
  infoStr <- data.frame(n = c(25, 78, 1, 16, 88, 40),
                        globalState = c("M", "U", "M", "M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  
  expect_true(is.null(c$get_CFTP_info()),
              info = paste("Not null info in combi instance before calling $cftp() in test:", test))
  
  c$cftp()
  
  expect_equal(class(c$get_CFTP_info())[1], "cftpStepGenerator")
  expect_equal(c$get_CFTP_info()$singleStr_number, length(infoStr$n),
               info = paste("Incorrect number of singleStr instances in test:", test))
  expect_equal(c$get_CFTP_info()$singleStr_siteNumber, infoStr$n,
               info = paste("Incorrect number of sites in singleStr instances in test:", test))
  
  clone <- c$copy()
  expect_equal(clone$get_CFTP_info()$singleStr_number, length(infoStr$n),
               info = paste("Incorrect number of singleStr instances in test:", test))
  expect_equal(clone$get_CFTP_info()$singleStr_siteNumber, infoStr$n,
               info = paste("Incorrect number of sites in singleStr instances in test:", test))
  
  
})



test_that("cftpStepGenerator $generate_events according to combi instance", {
  
  # test 1
  test <- 1
  infoStr <- data.frame(n = c(25, 78, 1, 16, 88, 40),
                        globalState = c("M", "U", "M", "M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  cftp <- cftpStepGenerator$new(singleStr_number = c$get_singleStr_number(),
                                singleStr_siteNumber = c$get_singleStr_siteNumber(), 
                                CFTP_highest_rate = c$get_highest_rate())
  cftp$generate_events()

  expect_true(all(unlist(cftp$CFTP_chosen_singleStr) %in% 1:6),
              info = paste("Samples singleStr indices not in combistrucutre instance in test:", test))
  
  for (str in 1:6){
    singleStr_indices <- which(unlist(cftp$CFTP_chosen_singleStr) == str)
    expect_true(all(unlist(cftp$CFTP_chosen_site)[singleStr_indices] %in% 1:infoStr$n[str]),
                info = paste("Samples site indice not within singleStr length in test:", test, "singleStr index:", str))
  }
  
    
  # test 2: one singleStr
  test <- 2
  infoStr <- data.frame(n = c(10),
                        globalState = c("M"))
  c <- combiStructureGenerator$new(infoStr)
  cftp <- cftpStepGenerator$new(singleStr_number = c$get_singleStr_number(),
                                singleStr_siteNumber = c$get_singleStr_siteNumber(), 
                                CFTP_highest_rate = c$get_highest_rate())
  cftp$generate_events()
  
  expect_true(all(unlist(cftp$CFTP_chosen_singleStr) == 1),
              info = paste("Samples singleStr indices not in combistrucutre instance in test:", test))
  expect_true(all(unlist(cftp$CFTP_chosen_site) %in% 1:10),
              info = paste("Samples site indices not in combistrucutre instance in test:", test))
  
  # test 3: one singleStr with one position
  test <- 3
  infoStr <- data.frame(n = c(1),
                        globalState = c("M"))
  c <- combiStructureGenerator$new(infoStr)
  cftp <- cftpStepGenerator$new(singleStr_number = c$get_singleStr_number(),
                                singleStr_siteNumber = c$get_singleStr_siteNumber(), 
                                CFTP_highest_rate = c$get_highest_rate())
  cftp$generate_events()
  
  expect_true(all(unlist(cftp$CFTP_chosen_singleStr) == 1),
              info = paste("Samples singleStr indices not in combistrucutre instance in test:", test))
  expect_true(all(unlist(cftp$CFTP_chosen_site) == 1),
              info = paste("Samples site indices not in combistrucutre instance in test:", test))
  
  
})

test_that("combiStructureGenerator $set_CFTP_info", {
  
  # Initialize combiStructureGenerator instance
  infoStr <- data.frame(n = c(10, 10, 10),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  
  # Expect error if given object is not of class cftpStepGenerator
  expect_error(c$set_CFTP_info(1),
               info = "method fails to throw an error when given object is not instance of class cftpStepGenerator")
  
  # Initialize cftpStepGenerator instance
  cftp <- cftpStepGenerator$new(singleStr_number = c$get_singleStr_number(),
                                singleStr_siteNumber = c$get_singleStr_siteNumber(), 
                                CFTP_highest_rate = c$get_highest_rate())
  
  c$set_CFTP_info(cftp)
  expect_true(class(c$get_CFTP_info())[1] == "cftpStepGenerator")
  expect_equal(length(c$get_CFTP_info()$CFTP_event), 0,
               info = "Assigned cftp instance without steps has length of CFTP_event non 0")
  
  cftp$generate_events(100)
  expect_equal(length(unlist(c$get_CFTP_info()$CFTP_event)), 100,
               info = "Assigned cftp instance with 100 steps has length of CFTP_event non 100")
})


test_that("combiStructureGenerator $cftp_apply_events input control", {
  
  # Initialize combiStructureGenerator instance
  infoStr <- data.frame(n = c(10, 10, 10),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  
  # Expect error when calling method before generating events
  expect_error(c$cftp_apply_events(),
               info = "method fails to throw error when called before generating CFTP events")
  
  # Initialize cftpStepGenerator instance and assign it to combiStructureGenerator instance
  cftp <- cftpStepGenerator$new(singleStr_number = c$get_singleStr_number(),
                                singleStr_siteNumber = c$get_singleStr_siteNumber(), 
                                CFTP_highest_rate = c$get_highest_rate())
  c$set_CFTP_info(cftp)
  # Fake two iterations
  cftp$generate_events(steps = 100)
  cftp$generate_events(steps = 200)
  # Delete the last list element in one of the lists
  cftp$CFTP_random <- cftp$CFTP_random[1]
  expect_error(c$cftp_apply_events(),
               info = "method fails to throw error when not all lists with cftp info are of equal length")
  
})

test_that("combiStructureGenerator $cftp_apply_events event acceptance/rejection", {
  # Initialize combiStructureGenerator instance
  infoStr <- data.frame(n = c(10, 10, 10),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  # Initialize cftpStepGenerator instance and assign it to combiStructureGenerator instance
  cftp <- cftpStepGenerator$new(singleStr_number = c$get_singleStr_number(),
                                singleStr_siteNumber = c$get_singleStr_siteNumber(), 
                                CFTP_highest_rate = c$get_highest_rate())
  c$set_CFTP_info(cftp)
  # Fake two iterations
  cftp$generate_events(steps = 100)
  cftp$generate_events(steps = 200)
  output <- c$cftp_apply_events(testing = TRUE)
  
  # Set correspondence of event number and applied case
  encoding_case <- data.frame(n = 1:5,
                              case = c("SSEi_1", "SSEi_2", "SSEi_3", "SSEc_left", "SSEc_right"))
  
  # Unlist testing output
  event_acceptance <- unlist(output$event_acceptance)
  r_jk <- unlist(output$r_jk)
  r_m <- unlist(output$r_m)
  CFTP_random <- unlist(output$CFTP_random)
  CFTP_event <- unlist(output$CFTP_event)
  applied_event <- unlist(output$applied_event)
  
  accepted_indeces <- which(event_acceptance == TRUE)
  if (length(accepted_indeces) > 0){
    expect_true(all(r_jk[accepted_indeces]/r_m > CFTP_random[accepted_indeces]),
                info = "Not all accepted events fulfill relative rate higher than sampled threshold")
    expect_false(any(is.na(CFTP_event[accepted_indeces])),
                 info = "Not all cases of accepted events return non-NA value in testing output applied_event")
    for(e in 1:length(accepted_indeces)){
      expect_equal(encoding_case[CFTP_event[accepted_indeces][e], "case"], applied_event[accepted_indeces][e],
                   info = "Error in correspondence between CFTP_event encoding and applied event")
    }
    
  }
  
  non_accepted_indeces <- which(event_acceptance == FALSE)
  if (length(non_accepted_indeces) > 0){
    expect_true(all(CFTP_event[non_accepted_indeces][is.na(r_jk[non_accepted_indeces])] %in% c(1, 2, 3)),
                info = "Not all cases in which a rate was not sampled because of SSEi newSt and oldSt being equal correspond to SSEi events")
    expect_true(all(r_jk[non_accepted_indeces][!is.na(r_jk[non_accepted_indeces])]/r_m <= CFTP_random[non_accepted_indeces][!is.na(r_jk[non_accepted_indeces])]),
                info = "Not all non_accepted_events with rate fulfill relative rate smaller or equal than sampled threshold")
    expect_true(all(is.na(applied_event[non_accepted_indeces])),
                info = "Not all cases of non-accepted events return NA in testing output applied_event")
    
  }
})


test_that("combiStructureGenerator $cftp_apply_events null default output", {
  # Initialize combiStructureGenerator instance
  infoStr <- data.frame(n = c(10, 10, 10),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  # Initialize cftpStepGenerator instance and assign it to combiStructureGenerator instance
  cftp <- cftpStepGenerator$new(singleStr_number = c$get_singleStr_number(),
                                singleStr_siteNumber = c$get_singleStr_siteNumber(), 
                                CFTP_highest_rate = c$get_highest_rate())
  c$set_CFTP_info(cftp)
  # Fake two iterations
  cftp$generate_events(steps = 100)
  cftp$generate_events(steps = 200)
  
  expect_null(c$cftp_apply_events(),
              info = "whith testing = FALSE method generates output")
})


test_that("combiStructureGenerator $cftp", {
  
  # Initialize combiStructureGenerator instance
  infoStr <- data.frame(n = c(10, 10, 10),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  
  # Test total CFTP step number is minimum the given number
  test_steps <- 100
  output<- combi_obj$cftp(steps = test_steps, testing = TRUE)
  expect_true(output$total_steps >= test_steps,
              info = "method testing output total_steps is smaller than given one")
  
  # Extract sequences of combi instances to test they are equal
  m_seq <- c(output$combi_m$get_singleStr(1)$get_seq(), output$combi_m$get_singleStr(2)$get_seq(), output$combi_m$get_singleStr(3)$get_seq())
  self_seq <- c(output$self$get_singleStr(1)$get_seq(), output$self$get_singleStr(2)$get_seq(), output$self$get_singleStr(3)$get_seq())
  expect_true(all(m_seq == self_seq),
              info = "method testing output with different $seq in combi_u and self")
  
  # Expect method to return nothing when testing is (as default) FALSE
  combi_obj <- combiStructureGenerator$new(infoStr)
  expect_null(combi_obj$cftp(steps = test_steps),
               info = "method returns something when testing is FALSE")
  
  # Expect objects with correct combiStructure ID
  # Initiate an instance with a reset shared counter
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  # Reset shared counter
  c$reset_sharedCounter()
  c <- combiStructureGenerator$new(infoStr)
  output <- c$cftp(testing = TRUE)
  expect_equal(output$self$get_id(), 1, 
               info = "self ID not equal to one after resetting counter")
  expect_equal(output$combi_m$get_id(), output$counter+1,
               info = "combi_m ID not equal to 2* the counter of cftp cycles before convergence")
  
  for (i in 1:3){
    expect_equal(get_private(output$self$get_singleStr(i))$my_combiStructure$get_id(), 1,
                 info = paste("self singleStr", i, "does not point to correct my_combiStructure according to self ID"))
    expect_equal(get_private(output$combi_m$get_singleStr(i))$my_combiStructure$get_id(), output$counter+1,
                 info = paste("combi_m singleStr", i, "does not pont to correct my_combiStructure according to combi_m ID"))
  }
  
  # Expect number of cftp steps corresponding to default_steps*2^(counter-1)
  default_steps <- 10000
  calculate_total_steps <- function(counter) {
    10000 * 2^(counter - 1)
  }
  expect_equal(output$total_steps, calculate_total_steps(output$counter),
               info = "incorrect number of steps generated for given number of cftp cycles before convergence")
  
  
  # Test case with several singleStr with different lengths
  infoStr <- data.frame(n = c(10, 35, 1),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  output <- c$cftp(testing = TRUE)
  
  expect_true(all(sort(unique(unlist(output$CFTP_chosen_site)))==1:35),
              info = "not all possible sites are chosen when singleStructures have different lengths")
  
  # Expect clones to have the same CFTP_info
  expect_true(identical(output$self$get_CFTP_info(), output$combi_m$get_CFTP_info()),
              info = "Clones have non-identical CFTP_info")
})

test_that("treeMultiRegionSimulator initialize with cftp = TRUE", {
  # Initiate a combiStructure instance to reset shared counter
  infoStr <- data.frame(n = c(13, 13, 13),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  # Reset shared counter
  c$reset_sharedCounter()
  
  # Initialize treeMultiRegionSimulator instance
  infoStr <- data.frame(n = c(20, 10, 20),
                        globalState = c("M", "U", "M"))
  message <- capture.output(t <- treeMultiRegionSimulator$new(infoStr = infoStr, tree = "(a:1);", CFTP = TRUE, testing = TRUE), type = "message")
  
  # Extract sequence info
  root_before_cftp_seq <- t$testing_output$seq_before_cftp
  root_after_cftp_seq <- c()
  self_seq <- c()
  for (i in 1:3){
    root_after_cftp_seq <- c(root_after_cftp_seq, t$Branch[[1]]$get_singleStr(i)$get_seq())
    self_seq <- c(self_seq, t$testing_output$cftp_output$self$get_singleStr(i)$get_seq())
  }
  
  expect_true(all(unique(sort(unlist(t$testing_output$cftp_output$CFTP_chosen_site))) == 1:20),
              info = "Not all sites chosen by $cftp")
  # Expect sequence changing after cftp and root sequence to be as in cftp output
  # Not always
  #expect_false(all(root_after_cftp_seq == root_before_cftp_seq),
  #             info = "$seq doesnt change after cftp")
  expect_true(all(self_seq == root_after_cftp_seq),
              info = "Root $seq doesnt correspond to $seq after cftp")
  
  # Expect ID in root data to be 1
  expect_equal(t$Branch[[1]]$get_id(), 1,
               info = "root ID does not stay as initial ID")
  
  # Expect new ID in root data to correspond to cftp's combi_u id
  expect_equal(t$testing_output$cftp_output$combi_m$get_id(), t$testing_output$cftp_output$counter +1,
               info = "$cftp combi_m ID does not correspond to the counter")
  for (i in 1:3){
    expect_equal(get_private(t$Branch[[1]]$get_singleStr(i))$my_combiStructure$get_id(), 1,
                 info = paste("root singleStr", i, "does not point to correct my_combiStructure according to root ID"))
    expect_equal(get_private(t$testing_output$cftp_output$combi_m$get_singleStr(i))$my_combiStructure$get_id(), t$testing_output$cftp_output$counter +1,
                 info = paste("combi_m singleStr", i, "does not point to correct my_combiStructure according to counter"))
  }
  
  # Test ID change consistent with testing = FALSE
  c$reset_sharedCounter()
  message <- capture.output(t <- treeMultiRegionSimulator$new(infoStr = infoStr, tree = "(a:1);", CFTP = TRUE), type = "message")
  
  expect_true(t$Branch[[1]]$get_id() == 1,
              info = "id with testing = FALSE remains consistent to testing = TRUE")
  
  
  # Expect informative message when CFTP = TRUE and no message about CFTP when is, as default, FALSE
  message <- capture.output(t <- treeMultiRegionSimulator$new(infoStr = infoStr, tree = "(a:1);", CFTP = TRUE), type = "message")
  
  expect_true(message[2] == "Calling CFTP algorithm for data at root before letting it evolve along given tree.",
              info = "Class fails to inform when CFTP is set to TRUE")
  
  message <- capture.output(t <- treeMultiRegionSimulator$new(infoStr = infoStr, tree = "(a:1);"), type = "message")
  
  expect_true(is.na(message[2]),
              info = "Class contains informative message when CFTP is set to FALSE")
})





test_that("treeMultiRegionSimulator errors in input", {
  infoStr <- data.frame(n = c(20, 10, 20),
                        globalState = c("M", "U", "M"))
  c <- combiStructureGenerator$new(infoStr)
  expect_error(treeMultiRegionSimulator$new(rootData = c, infoStr = infoStr, tree = "(a:1);"),
               info = "Class fails to throw error when both rootData and infoStr are given")
  params <- get_parameterValues()
  expect_error(treeMultiRegionSimulator$new(rootData = c, params = params, tree = "(a:1);"),
               info = "Class fails to throw error when both rootData and params are given")
  expect_error(treeMultiRegionSimulator$new(params = as.matrix(params), infoStr = infoStr, tree = "(a:1);"),
               info = "Class fails to throw error when params is not dataframe")
  expect_error(treeMultiRegionSimulator$new(params = params[,1:3], infoStr = infoStr, tree = "(a:1);"),
               info = "Class fails to throw error when params is incomplete.")
  expect_error(treeMultiRegionSimulator$new(tree = "(a:1);"),
               info = "Class fails to throw error when none of both 'infoStr' and 'rootData' are NULL")
  expect_error(treeMultiRegionSimulator$new(rootData = c),
               info = "Class does not throw error when 'tree' is missing")
  expect_error(treeMultiRegionSimulator$new(infoStr = infoStr),
               info = "Class does not throw error when 'tree' is missing")

})


test_that("combiStructureGenerator $cftp step_limit", {
  
  # Initialize combiStructureGenerator instance
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"))
  combi_obj <- combiStructureGenerator$new(infoStr)
  
  # Given a number of steps smaller than the number of sites, 
  # there is no possiblity of convergence in a single iteration
  steps <- 100
  # Set a step limit that is applied before the second iteration
  step_limit <- 90
  message <- capture.output(
    output<- combi_obj$cftp(steps, step_limit, testing = TRUE), type= "message")
  expect_equal(message,
               "Steps limit reached. Applying approximation for CFTP.")
  expect_equal(output$counter, 1)
  expect_equal(output$total_steps, steps)
  
  
})

test_that("treeMultiRegionSimulator CFTP_step_limit", {
  
  # Initialize combiStructureGenerator instance
  tree <- "(a:1,b:1);"
  infoStr <- data.frame(n = c(5000, 5000, 100),
                        globalState = c("M", "U", "M"))
  # Given a number of steps smaller than the number of sites, 
  # there is no possiblity of convergence in a single iteration
  # Set a step limit that is applied before the second iteration
  message <- capture.output(
    tree_obj <- treeMultiRegionSimulator$new(infoStr, 
                                             tree = tree, 
                                             CFTP = TRUE, 
                                             CFTP_step_limit = 9000),
    type="message")
  expect_equal(message[3],
               "Steps limit reached. Applying approximation for CFTP.")
})




