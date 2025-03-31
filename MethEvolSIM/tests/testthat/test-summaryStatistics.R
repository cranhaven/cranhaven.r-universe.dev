test_that("get_MeanFreqP non-valid structureIndices", {
  
  data <- list(
    list(c(0, 0, 0), c(0, 0, 0)), # tip 1
    list(c(0, 0, 0), c(1, 1, 1)) # tip 2
  )
  
  index_islands <- c(1,3)
  index_nonislands <- c(1,3)
  
  expect_error(get_islandMeanFreqP(data = data, index_islands = index_islands, sample_n =2),
               "Invalid island indices detected: 3. Number of structures in given data: 2",
               info = "fails to throw error with incorrect island index")
  expect_error(get_nonislandMeanFreqP(data = data, index_nonislands = index_nonislands, sample_n =2),
               "Invalid non-island indices detected: 3. Number of structures in given data: 2",
               info = "fails to throw error with incorrect nonisland index")
  
  index_islands <- c()
  index_nonislands <- c()
  
  expect_error(get_islandMeanFreqP(data = data, index_islands = index_islands, sample_n =2),
               "'index_islands' has no indices")
  expect_error(get_nonislandMeanFreqP(data = data, index_nonislands = index_nonislands, sample_n =2),
               "'index_nonislands' has no indices")
  
})

test_that("get_MeanFreqP non-categorized input of methylation states", {
  
  data <- list(
    list(c(0.1, 0.2, 0.02), c(0.05, 0.25, 0.15)), # tip 1
    list(c(0.01, 0.7, 0.85), c(0.3, 0.1, 0.98)) # tip 2
  )
  
  index_islands <- c(1,2)
  index_nonislands <- c(1,2)
  
  mean_freqP <- mean(c(0, 0, 0, 0, 0.5, 0, 0, 0.5, 1, 0.5, 0, 1)==0.5)
  
  expect_equal(get_islandMeanFreqP(data = data, index_islands = index_islands, sample_n =2),
               mean_freqP,
               info = "incorrect value with uncategorized input data")
  expect_equal(get_nonislandMeanFreqP(data = data, index_nonislands = index_nonislands, sample_n =2),
               mean_freqP,
               info = "incorrect value with uncategorized input data")
  
})

test_that("get_MeanFreqP structures with equal length islands and non-islands", {
  # Two tips and two islands
  index_islands <- c(1, 2)
  index_nonislands <- c(1, 2)
  data <- list(
    list(c(0, 0, 0), c(0, 0, 0)), # tip 1
    list(c(0, 0, 0), c(1, 1, 1)) # tip 2
  )
  sample_n <- 2
  expect_equal(get_islandMeanFreqP(index_islands, data, categorized_data = TRUE, sample_n), 0,
               info = "incorrect mean in two tips and two islands, freq 0")
  expect_equal(get_nonislandMeanFreqP(index_nonislands, data, categorized_data = TRUE, sample_n), 0,
               info = "incorrect mean in two tips and two non-islands, freq 0")
  
  # Two tips and two islands
  index_islands <- c(1, 2)
  index_nonislands <- c(1, 2)
  data <- list(
    list(c(.5, .5, 0), c(0, 0, .5)), # tip 1
    list(c(0, .5, .5), c(1, .5, 1)) # tip 2
  )
  sample_n <- 2
  expect_equal(get_islandMeanFreqP(index_islands, data, categorized_data = TRUE, sample_n), .5,
               info = "incorrect mean in two tips and two islands, freq .5")
  expect_equal(get_nonislandMeanFreqP(index_nonislands, categorized_data = TRUE, data, sample_n), .5,
               info = "incorrect mean in two tips and two non-islands, freq .5")
  
  # Two tips and 1 island
  index_islands <- c(2) # island index 2
  index_nonislands <- c(2) # non-island index 2
  data <- list(
    list(c(.5, .5, 0), c(0, 0, .5)), # tip 1
    list(c(0, .5, .5), c(1, .5, 1)) # tip 2
  )
  sample_n <- 2
  expect_equal(get_islandMeanFreqP(index_islands, data, categorized_data = TRUE, sample_n), 1/3,
               info = "incorrect mean in two tips and one islands, freq 1/3")
  expect_equal(get_nonislandMeanFreqP(index_nonislands, data, sample_n), 1/3,
               info = "incorrect mean in two tips and one non-island, freq 1/3")
  index_islands <- c(1) # island index 1
  index_nonislands <- c(1) # island index 1
  expect_equal(get_islandMeanFreqP(index_islands, data, categorized_data = TRUE, sample_n), 2/3,
               info = "incorrect mean in two tips and one islands, freq 2/3")
  expect_equal(get_nonislandMeanFreqP(index_nonislands, data, sample_n), 2/3,
               info = "incorrect mean in two tips and one non-island, freq 2/3")
  
  # One tip, one island
  index_islands <- c(1)
  index_nonislands <- c(1)
  sample_n <- 1
  
  data <- list(c(0.5, 1, 0.5)) # single tip
  expect_equal(get_islandMeanFreqP(index_islands, data, categorized_data = TRUE, sample_n), 2/3,
               info = "incorrect mean in one tip one island")
  expect_equal(get_nonislandMeanFreqP(index_nonislands, data, sample_n), 2/3,
               info = "incorrect mean in one tip one non-island")

  data <- list(c(1, 1, 0)) # single tip, no 0.5 state
  expect_equal(get_islandMeanFreqP(index_islands, data, categorized_data = TRUE, sample_n), 0,
               info = "incorrect mean in one tip one island no .5 state")
  expect_equal(get_nonislandMeanFreqP(index_nonislands, data, sample_n), 0,
               info = "incorrect mean in one tip one non-island no .5 state")
})

test_that("get_MeanFreqP structures with different length (islands and non-islands)", {
  # Two tips and two islands, freq 0
  index_islands <- c(1, 2)
  index_nonislands <- c(1, 2)
  data <- list(
    list(c(0, 0, 0, 0, 0, 0, 0), c(0, 0, 0)), # tip 1
    list(c(0, 0, 0, 0, 0, 0, 0), c(1, 1, 1)) # tip 2
  )
  sample_n <- 2
  expect_equal(get_islandMeanFreqP(index_islands, data, categorized_data = TRUE, sample_n), 0,
               info = "incorrect mean in two tips and two islands, freq 0")
  expect_equal(get_nonislandMeanFreqP(index_nonislands, data, categorized_data = TRUE, sample_n), 0,
               info = "incorrect mean in two tips and two non-islands, freq 0")
  
  # Two tips and two islands, freq .5 evenly distributed
  index_islands <- c(1, 2)
  index_nonislands <- c(1, 2)
  data <- list(
    list(c(.5, .5, 0, 0, 0, .5), c(.5, 0, 0, .5)), # tip 1
    list(c(0, .5, .5, 1, 1, .5), c(1, .5, 1, .5)) # tip 2
  )
  sample_n <- 2
  expect_equal(get_islandMeanFreqP(index_islands, data, categorized_data = TRUE, sample_n), .5,
               info = "incorrect mean in two tips and two islands, freq .5")
  expect_equal(get_nonislandMeanFreqP(index_nonislands, data, categorized_data = TRUE, sample_n), .5,
               info = "incorrect mean in two tips and two non-islands, freq .5")
  
  # Two tips and two islands, freq .5 unevenly distributed (fist island mean 3/4, second island mean 1/4)
  index_islands <- c(1, 2)
  index_nonislands <- c(1, 2)
  data <- list(
    list(c(.5, .5, 0, .5, .5, .5), c(.5, 0, 0, 0)), # tip 1
    list(c(0, .5, .5, 1, .5, .5), c(1, 0, 1, .5)) # tip 2
  )
  sample_n <- 2
  expect_equal(get_islandMeanFreqP(index_islands, data, categorized_data = TRUE, sample_n), .5,
               info = "incorrect mean in two tips and two islands, freq .5")
  expect_equal(get_nonislandMeanFreqP(index_nonislands, data, categorized_data = TRUE, sample_n), .5,
               info = "incorrect mean in two tips and two non-islands, freq .5")
 
})

test_that("get_MeanFreqM non-valid structureIndices", {
  
  data <- list(
    list(c(0, 0, 0), c(0, 0, 0)), # tip 1
    list(c(0, 0, 0), c(1, 1, 1)) # tip 2
  )
  
  index_islands <- c(1,3)
  index_nonislands <- c(1,3)
  
  expect_error(get_islandMeanFreqM(data = data, index_islands = index_islands, sample_n =2),
               "Invalid island indices detected: 3. Number of structures in given data: 2",
               info = "fails to throw error with incorrect island index")
  expect_error(get_nonislandMeanFreqM(data = data, index_nonislands = index_nonislands, sample_n =2),
               "Invalid non-island indices detected: 3. Number of structures in given data: 2",
               info = "fails to throw error with incorrect nonisland index")
  
  index_islands <- c()
  index_nonislands <- c()
  
  expect_error(get_islandMeanFreqM(data = data, index_islands = index_islands, sample_n =2),
               "'index_islands' has no indices")
  expect_error(get_nonislandMeanFreqM(data = data, index_nonislands = index_nonislands, sample_n =2),
               "'index_nonislands' has no indices")
})

test_that("get_MeanFreqM non-categorized input of methylation states", {
  
  data <- list(
    list(c(0.1, 0.2, 0.02), c(0.05, 0.25, 0.15)), # tip 1
    list(c(0.01, 0.7, 0.85), c(0.3, 0.1, 0.98)) # tip 2
  )
  
  index_islands <- c(1,2)
  index_nonislands <- c(1,2)
  
  mean_freqM <- mean(c(0, 0, 0, 0, 0.5, 0, 0, 0.5, 1, 0.5, 0, 1)==1)
  
  expect_equal(get_islandMeanFreqM(data = data, index_islands = index_islands, sample_n =2),
               mean_freqM,
               info = "incorrect value with uncategorized input data")
  expect_equal(get_nonislandMeanFreqM(data = data, index_nonislands = index_nonislands, sample_n =2),
               mean_freqM,
               info = "incorrect value with uncategorized input data")
  
})


test_that("get_MeanFreqM structures with equal length islands and non-islands", {
  # Two tips and two islands
  index_islands <- c(1, 2)
  index_nonislands <- c(1, 2)
  data <- list(
    list(c(0, 0, 0), c(0, 0, 0)), # tip 1
    list(c(0, 0, 0), c(.5, .5, .5)) # tip 2
  )
  sample_n <- 2
  expect_equal(get_islandMeanFreqM(index_islands, data, categorized_data = TRUE, sample_n), 0,
               info = "incorrect mean in two tips and two islands, freq 0")
  expect_equal(get_nonislandMeanFreqM(index_nonislands, data, categorized_data = TRUE, sample_n), 0,
               info = "incorrect mean in two tips and two non-islands, freq 0")
  
  # Two tips and two islands
  index_islands <- c(1, 2)
  index_nonislands <- c(1, 2)
  data <- list(
    list(c(1, 1, 0), c(0, 0, .5)), # tip 1
    list(c(1, 1, .5), c(1, .5, 1)) # tip 2
  )
  sample_n <- 2
  expect_equal(get_islandMeanFreqM(index_islands, data, categorized_data = TRUE, sample_n), 0.5,
               info = "incorrect mean in two tips and two islands, freq 0.5")
  expect_equal(get_nonislandMeanFreqM(index_nonislands, data, categorized_data = TRUE, sample_n), 0.5,
               info = "incorrect mean in two tips and two non-islands, freq 0.5")
  
  # Two tips and 1 island
  index_islands <- c(2) # island index 2
  index_nonislands <- c(2) # non-island index 2
  data <- list(
    list(c(.5, .5, 0), c(0, 0, .5)), # tip 1
    list(c(0, .5, 1), c(1, 1, 1)) # tip 2
  )
  sample_n <- 2
  expect_equal(get_islandMeanFreqM(index_islands, data, categorized_data = TRUE, sample_n), .5,
               info = "incorrect mean in two tips and one island, freq 0.5")
  expect_equal(get_nonislandMeanFreqM(index_nonislands, data, categorized_data = TRUE, sample_n), .5,
               info = "incorrect mean in two tips and one non-island, freq 0.5")
  
  # One tip, one island
  index_islands <- c(1)
  index_nonislands <- c(1)
  sample_n <- 1
  
  data <- list(c(1, 1, 0.5)) # single tip
  expect_equal(get_islandMeanFreqM(index_islands, data, categorized_data = TRUE, sample_n), 2/3,
               info = "incorrect mean in one tip one island")
  expect_equal(get_nonislandMeanFreqM(index_nonislands, data, sample_n), 2/3,
               info = "incorrect mean in one tip one non-island")
  
  data <- list(c(0, 0, 0)) # single tip, no methylated state
  expect_equal(get_islandMeanFreqM(index_islands, data, categorized_data = TRUE, sample_n), 0,
               info = "incorrect mean in one tip one island no methylated state")
  expect_equal(get_nonislandMeanFreqM(index_nonislands, data, sample_n), 0,
               info = "incorrect mean in one tip one non-island no methylated state")
})

test_that("get_MeanFreqM structures with different length (islands and non-islands)", {
  # Two tips and two islands, freq 1/4
  index_islands <- c(1, 2)
  index_nonislands <- c(1, 2)
  data <- list(
    list(c(0, 0, 0, 0, 0, 0, 0), c(0, 0, 0)), # tip 1
    list(c(0, 0, 0, 0, 0, 0, 0), c(1, 1, 1)) # tip 2
  )
  sample_n <- 2
  expect_equal(get_islandMeanFreqM(index_islands, data, categorized_data = TRUE, sample_n), 1/4,
               info = "incorrect mean in two tips and two islands, freq 1/4")
  expect_equal(get_nonislandMeanFreqM(index_nonislands, data, categorized_data = TRUE, sample_n), 1/4,
               info = "incorrect mean in two tips and two non-islands, freq 1/4")
  
  # Two tips and two islands, freq .5 evenly distributed
  index_islands <- c(1, 2)
  index_nonislands <- c(1, 2)
  data <- list(
    list(c(.5, .5, 1, 1, 1, .5), c(1, 1, .5, 0)), # tip 1
    list(c(0, 1, .5, 1, 1, .5), c(1, 1, 0, 0)) # tip 2
  )
  sample_n <- 2
  expect_equal(get_islandMeanFreqM(index_islands, data, categorized_data = TRUE, sample_n), 0.5,
               info = "incorrect mean in two tips and two islands, freq .5")
  expect_equal(get_nonislandMeanFreqM(index_nonislands, data, categorized_data = TRUE, sample_n), 0.5,
               info = "incorrect mean in two tips and two non-islands, freq .5")
  
  # Two tips and two islands, freq .5 unevenly distributed (first island mean 3/4, second island mean 1/4)
  index_islands <- c(1, 2)
  index_nonislands <- c(1, 2)
  data <- list(
    list(c(1, .5, 0, 1, 1, 1), c(.5, 1, 0, .5)), # tip 1
    list(c(1, 1, .5, 1, 1, 1), c(1, 0, .5, 0)) # tip 2
  )
  sample_n <- 2
  expect_equal(get_islandMeanFreqM(index_islands, data, categorized_data = TRUE, sample_n), .5,
               info = "incorrect mean in two tips and two islands, freq .5")
  expect_equal(get_nonislandMeanFreqM(index_nonislands, data, categorized_data = TRUE, sample_n), .5,
               info = "incorrect mean in two tips and two non-islands, freq .5")
})

test_that("get_islandSDFreqP non-valid structureIndices", {
  
  data <- list(
    list(c(0, 0, 0), c(0, 0, 0)), # tip 1
    list(c(0, 0, 0), c(1, 1, 1)) # tip 2
  )
  
  index_islands <- c(1,3)
  index_nonislands <- c(1,3)
  
  expect_error(get_islandSDFreqP(data = data, index_islands = index_islands, sample_n =2),
               "Invalid island indices detected: 3. Number of structures in given data: 2",
               info = "fails to throw error with incorrect island index")
  expect_error(get_nonislandSDFreqP(data = data, index_nonislands = index_nonislands, sample_n =2),
               "Invalid non-island indices detected: 3. Number of structures in given data: 2",
               info = "fails to throw error with incorrect nonisland index")
})

test_that("get_SDFreqP non-categorized input of methylation states", {
  
  data <- list(
    list(c(0.1, 0.2, 0.02), c(0.05, 0.25, 0.15)), # tip 1
    list(c(0.01, 0.7, 0.85), c(0.3, 0.1, 0.98)) # tip 2
  )
  
  index_islands <- c(1,2)
  index_nonislands <- c(1,2)
  
  sdTip1 <- sd(c(mean(c(0, 0, 0) == 0.5), mean(c(0, 0.5, 0) == 0.5)))
  sdTip2 <- sd(c(mean(c(0, 0.5, 1) == 0.5), mean(c(0.5, 0, 1) == 0.5)))
  
  expect_equal(get_islandSDFreqP(data = data, index_islands = index_islands, sample_n =2),
               mean(c(sdTip1, sdTip2)),
               info = "incorrect value with uncategorized input data")
  expect_equal(get_nonislandSDFreqP(data = data, index_nonislands = index_nonislands, sample_n =2),
               mean(c(sdTip1, sdTip2)),
               info = "incorrect value with uncategorized input data")
  
})

test_that("get_SDFreqP structures with equal length islands and non-islands", {
  # One tip and four islands / non-islands
  index_islands <- c(1, 2, 3, 4)
  index_nonislands <- c(1, 2, 3, 4)
  # 1 tip mean freq p 0.5
  data <- list(c(0, 0, 0), # freq P 0
               c(.5, .5, .5), # freq P 1
               c(0, 0, 0), # freq P 0
               c(.5, .5, .5)) # freq P 1
  exp_SD <- sqrt(4*(0.5)^2/3) 
  sample_n <- 1
  expect_equal(get_islandSDFreqP(index_islands, data, categorized_data = TRUE, sample_n), exp_SD,
               info = "incorrect SD in one tip and 4 islands")
  expect_equal(get_nonislandSDFreqP(index_nonislands, data, categorized_data = TRUE, sample_n), exp_SD,
               info = "incorrect SD in one tip and 4 non-islands")
  
  # Two tips and four islands / non-islands
  index_islands <- c(1, 2, 3, 4)
  index_nonislands <- c(1, 2, 3, 4)
  # Tip 1 mean freq p 0.5, tip 2 mean freq p 0.5
  data <- list(
    list(c(0, 0, 0), # Tip 1: freq P 0
         c(.5, .5, .5), # Tip 1: freq P 1
         c(0, 0, 0), # Tip 1: freq P 0
         c(.5, .5, .5)), # Tip 1: freq P 1
    list(c(0, 0, 0), # Tip 2: freq P 0
         c(1, .5, 1), # Tip 2: freq P 1/3
         c(.5, 0, .5), # Tip 2: freq P 2/3
         c(.5, .5, .5)) # Tip 2: freq P 1
  )
  exp_SD1 <- sqrt(4*(0.5)^2/3)
  exp_SD2 <- sqrt((2*(0.5)^2+2*((1/3)-0.5)^2)/3)
  mean_SD <- mean(c(exp_SD1, exp_SD2))
  sample_n <- 2
  expect_equal(get_islandSDFreqP(index_islands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect SD in two tip and 4 islands")
  expect_equal(get_nonislandSDFreqP(index_nonislands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect SD in two tip and 4 non-islands")
  
  # Two tips and four islands / non-islands (length 1)
  index_islands <- c(1, 2, 3, 4)
  index_nonislands <- c(1, 2, 3, 4)
  # Tip 1 mean freq p 0.5, tip 2 mean freq p 0.5
  data <- list(
    list(c(0), # Tip 1: freq P 0
         c(.5), # Tip 1: freq P 1
         c(0), # Tip 1: freq P 0
         c(.5)), # Tip 1: freq P 1
    list(c(0), # Tip 2: freq P 0
         c(1), # Tip 2: freq P 0
         c(.5), # Tip 2: freq P 1
         c(.5)) # Tip 2: freq P 1
  )
  exp_SD1 <- sqrt(4*(0.5)^2/3)
  exp_SD2 <- sqrt(4*(0.5)^2/3)
  mean_SD <- mean(c(exp_SD1, exp_SD2))
  sample_n <- 2
  expect_equal(get_islandSDFreqP(index_islands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect SD in two tips and 4 islands (length 1)")
  expect_equal(get_nonislandSDFreqP(index_nonislands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect SD in two tips and 4 non-islands (length 1)")
  
  
  
  # Two tips with 4 islands and 4 non-islands (8 structures)
  index_islands <- c(1, 3, 5, 7)
  index_nonislands <- c(2, 4, 6, 8)
  # Tip 1 mean freq p 0.5, tip 2 mean freq p 0.5
  data <- list(
    list(c(0, 0, 0), # Tip 1 island: freq P 0
         c(0, 0, 0), # Tip 1 non-island: freq P 0
         c(.5, .5, .5), # Tip 1 island: freq P 1
         c(.5, .5, .5), # Tip 1 non-island: freq P 1
         c(0, 0, 0), # Tip 1 island: freq P 0
         c(0, 0, 0), # Tip 1 non-island: freq P 0
         c(.5, .5, .5),# Tip 1 island: freq P 1
         c(.5, .5, .5)), # Tip 1 non-island: freq P 1
    list(c(0, 0, 0), # Tip 2 island: freq P 0
         c(0, 0, 0), # Tip 2 non-island: freq P 0
         c(1, .5, 1), # Tip 2 island: freq P 1/3
         c(1, .5, 1), # Tip 2 non-island: freq P 1/3
         c(.5, 0, .5), # Tip 2 island: freq P 2/3
         c(.5, 0, .5), # Tip 2 non-island: freq P 2/3
         c(.5, .5, .5), # Tip 2 island: freq P 1
         c(.5, .5, .5)) # Tip 2 non-island: freq P 1
  )
  exp_SD1 <- sqrt(4*(0.5)^2/3)
  exp_SD2 <- sqrt((2*(0.5)^2+2*((1/3)-0.5)^2)/3)
  mean_SD <- mean(c(exp_SD1, exp_SD2))
  sample_n <- 2
  expect_equal(get_islandSDFreqP(index_islands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect island SD in two tips 4 islands and 4 non-islands (8 structures)")
  expect_equal(get_nonislandSDFreqP(index_nonislands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect non-island SD in two tips 4 islands and 4 non-islands (8 structures)")
  
  # Two tips with only 1 island and 7 non-islands 
  index_islands <- c(1)
  index_nonislands <- c(2, 3, 4, 5, 6, 7, 8)
  # Tip 1 mean freq p 0.5, tip 2 mean freq p 0.5
  data <- list(
    list(c(0, 0, 0), # Tip 1 island: freq P 0
         c(0, 0, 0), # Tip 1 non-island: freq P 0
         c(.5, .5, .5), # Tip 1 non-island: freq P 1
         c(.5, .5, .5), # Tip 1 non-island: freq P 1
         c(0, 0, 0), # Tip 1 non-island: freq P 0
         c(0, 0, 0), # Tip 1 non-island: freq P 0
         c(.5, .5, .5),# Tip 1 non-island: freq P 1
         c(.5, .5, .5)), # Tip 1 non-island: freq P 1
    list(c(0, 0, 0), # Tip 2 island: freq P 0
         c(0, 0, 0), # Tip 2 non-island: freq P 0
         c(1, .5, 1), # Tip 2 non-island: freq P 1/3
         c(1, .5, 1), # Tip 2 non-island: freq P 1/3
         c(.5, 0, .5), # Tip 2 non-island: freq P 2/3
         c(.5, 0, .5), # Tip 2 non-island: freq P 2/3
         c(.5, .5, .5), # Tip 2 non-island: freq P 1
         c(.5, .5, .5)) # Tip 2 non-island: freq P 1
  )
  sample_n <- 2
  expect_true(is.na(get_islandSDFreqP(index_islands, data, categorized_data = TRUE, sample_n)),
              info = "does not return NA when there is only one island structure per tip")
  
})


test_that("get_SDFreqP structures with different length islands and non-islands", {
  # One tip and four islands / non-islands
  index_islands <- c(1, 2, 3, 4)
  index_nonislands <- c(1, 2, 3, 4)
  # 1 tip mean freq p 0.5
  data <- list(c(0, 0, 0, 0, 0), # freq P 0
               c(.5, .5, .5), # freq P 1
               c(0, 0, 0, 0, 0), # freq P 0
               c(.5, .5, .5)) # freq P 1
  exp_SD <- sqrt(4*(0.5)^2/3) 
  sample_n <- 1
  expect_equal(get_islandSDFreqP(index_islands, data, categorized_data = TRUE, sample_n), exp_SD,
               info = "incorrect SD in one tip and 4 islands")
  expect_equal(get_nonislandSDFreqP(index_nonislands, data, categorized_data = TRUE, sample_n), exp_SD,
               info = "incorrect SD in one tip and 4 non-islands")
  
  # Two tips and four islands / non-islands
  index_islands <- c(1, 2, 3, 4)
  index_nonislands <- c(1, 2, 3, 4)
  # Tip 1 mean freq p 0.5, tip 2 mean freq p 0.5
  data <- list(
    list(c(0, 0, 0, 0, 0, 0), # Tip 1: freq P 0
         c(.5, .5, .5), # Tip 1: freq P 1
         c(0, 0, 0, 0, 0, 0), # Tip 1: freq P 0
         c(.5, .5, .5)), # Tip 1: freq P 1
    list(c(0, 0, 0, 0, 0, 0), # Tip 2: freq P 0
         c(1, .5, 1), # Tip 2: freq P 1/3
         c(.5, 0, .5, 1, .5, .5), # Tip 2: freq P 2/3
         c(.5, .5, .5)) # Tip 2: freq P 1
  )
  exp_SD1 <- sqrt(4*(0.5)^2/3)
  exp_SD2 <- sqrt((2*(0.5)^2+2*((1/3)-0.5)^2)/3)
  mean_SD <- mean(c(exp_SD1, exp_SD2))
  sample_n <- 2
  expect_equal(get_islandSDFreqP(index_islands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect SD in two tips and 4 islands")
  expect_equal(get_nonislandSDFreqP(index_nonislands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect SD in two tips and 4 non-islands")
  
  # Two tips with 4 islands and 4 non-islands (8 structures)
  index_islands <- c(1, 3, 5, 7)
  index_nonislands <- c(2, 4, 6, 8)
  # Tip 1 mean freq p 0.5, tip 2 mean freq p 0.5
  data <- list(
    list(c(0, 0, 0, 0, 0, 0), # Tip 1 island: freq P 0
         c(0, 0, 0, 0, 0, 0), # Tip 1 non-island: freq P 0
         c(.5, .5, .5), # Tip 1 island: freq P 1
         c(.5, .5, .5), # Tip 1 non-island: freq P 1
         c(0, 0, 0), # Tip 1 island: freq P 0
         c(0, 0, 0), # Tip 1 non-island: freq P 0
         c(.5, .5, .5),# Tip 1 island: freq P 1
         c(.5, .5, .5)), # Tip 1 non-island: freq P 1
    list(c(0, 0, 0, 0, 0, 0), # Tip 2 island: freq P 0
         c(0, 0, 0, 0, 0, 0), # Tip 2 non-island: freq P 0
         c(1, .5, 1), # Tip 2 island: freq P 1/3
         c(1, .5, 1), # Tip 2 non-island: freq P 1/3
         c(.5, 0, .5), # Tip 2 island: freq P 2/3
         c(.5, 0, .5), # Tip 2 non-island: freq P 2/3
         c(.5, .5, .5), # Tip 2 island: freq P 1
         c(.5, .5, .5)) # Tip 2 non-island: freq P 1
  )
  exp_SD1 <- sqrt(4*(0.5)^2/3)
  exp_SD2 <- sqrt((2*(0.5)^2+2*((1/3)-0.5)^2)/3)
  mean_SD <- mean(c(exp_SD1, exp_SD2))
  sample_n <- 2
  expect_equal(get_islandSDFreqP(index_islands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect island SD in two tips 4 islands and 4 non-islands (8 structures)")
  expect_equal(get_nonislandSDFreqP(index_nonislands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect non-island SD in two tips 4 islands and 4 non-islands (8 structures)")
  
  # Two tips with 4 islands and 4 non-islands (8 structures, last one only one site)
  index_islands <- c(1, 3, 5, 7)
  index_nonislands <- c(2, 4, 6, 8)
  # Tip 1 mean freq p 0.5, tip 2 mean freq p 0.5
  data <- list(
    list(c(0, 0, 0, 0, 0, 0), # Tip 1 island: freq P 0
         c(0, 0, 0, 0, 0, 0), # Tip 1 non-island: freq P 0
         c(.5, .5, .5), # Tip 1 island: freq P 1
         c(.5, .5, .5), # Tip 1 non-island: freq P 1
         c(0, 0, 0), # Tip 1 island: freq P 0
         c(0, 0, 0), # Tip 1 non-island: freq P 0
         c(.5, .5, .5),# Tip 1 island: freq P 1
         c(.5)), # Tip 1 non-island: freq P 1
    list(c(0, 0, 0, 0, 0, 0), # Tip 2 island: freq P 0
         c(0, 0, 0, 0, 0, 0), # Tip 2 non-island: freq P 0
         c(1, .5, 1), # Tip 2 island: freq P 1/3
         c(1, .5, 1), # Tip 2 non-island: freq P 1/3
         c(.5, 0, .5), # Tip 2 island: freq P 2/3
         c(.5, 0, .5), # Tip 2 non-island: freq P 2/3
         c(.5, .5, .5), # Tip 2 island: freq P 1
         c(.5)) # Tip 2 non-island: freq P 1
  )
  exp_SD1 <- sqrt(4*(0.5)^2/3)
  exp_SD2 <- sqrt((2*(0.5)^2+2*((1/3)-0.5)^2)/3)
  mean_SD <- mean(c(exp_SD1, exp_SD2))
  sample_n <- 2
  expect_equal(get_islandSDFreqP(index_islands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect island SD in two tips 4 islands and 4 non-islands (8 structures, last one only one site)")
  expect_equal(get_nonislandSDFreqP(index_nonislands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect non-island SD in two tips 4 islands and 4 non-islands (8 structures, last one only one site)")
  
})

test_that("get_islandSDFreqM non-valid structureIndices", {
  
  data <- list(
    list(c(0, 0, 0), c(0, 0, 0)), # tip 1
    list(c(0, 0, 0), c(1, 1, 1)) # tip 2
  )
  
  index_islands <- c(1,3)
  index_nonislands <- c(1,3)
  
  expect_error(get_islandSDFreqM(data = data, index_islands = index_islands, sample_n =2),
               "Invalid island indices detected: 3. Number of structures in given data: 2",
               info = "fails to throw error with incorrect island index")
  expect_error(get_nonislandSDFreqM(data = data, index_nonislands = index_nonislands, sample_n =2),
               "Invalid non-island indices detected: 3. Number of structures in given data: 2",
               info = "fails to throw error with incorrect nonisland index")
})

test_that("get_SDFreqM non-categorized input of methylation states", {
  
  data <- list(
    list(c(0.1, 0.2, 0.02), c(0.05, 0.25, 0.15)), # tip 1
    list(c(0.01, 0.7, 0.85), c(0.3, 0.1, 0.98)) # tip 2
  )
  
  index_islands <- c(1,2)
  index_nonislands <- c(1,2)
  
  sdTip1 <- sd(c(mean(c(0, 0, 0) == 1), mean(c(0, 0.5, 0) == 1)))
  sdTip2 <- sd(c(mean(c(0, 0.5, 1) == 1), mean(c(0.5, 0, 1) == 1)))
  
  expect_equal(get_islandSDFreqM(data = data, index_islands = index_islands, sample_n =2),
               mean(c(sdTip1, sdTip2)),
               info = "incorrect value with uncategorized input data")
  expect_equal(get_nonislandSDFreqM(data = data, index_nonislands = index_nonislands, sample_n =2),
               mean(c(sdTip1, sdTip2)),
               info = "incorrect value with uncategorized input data")
  
})


test_that("get_SDFreqM structures with equal length islands and non-islands", {
  # One tip and four islands / non-islands
  index_islands <- c(1, 2, 3, 4)
  index_nonislands <- c(1, 2, 3, 4)
  # 1 tip mean freq m 0.5
  data <- list(c(0, 0, 0), # freq M 0
               c(1, 1, 1), # freq M 1
               c(.5, 0, 0), # freq M 0
               c(1, 1, 1)) # freq M 1
  exp_SD <- sqrt(4*(0.5)^2/3) 
  sample_n <- 1
  expect_equal(get_islandSDFreqM(index_islands, data, categorized_data = TRUE, sample_n), exp_SD,
               info = "incorrect SD in one tip and 4 islands")
  expect_equal(get_nonislandSDFreqM(index_nonislands, data, categorized_data = TRUE, sample_n), exp_SD,
               info = "incorrect SD in one tip and 4 non-islands")
  
  # Two tips and four islands / non-islands
  index_islands <- c(1, 2, 3, 4)
  index_nonislands <- c(1, 2, 3, 4)
  # Tip 1 mean freq m 0.5, tip 2 mean freq m 0.5
  data <- list(
    list(c(0, .5, 0), # Tip 1: freq M 0
         c(1, 1, 1), # Tip 1: freq M 1
         c(.5, 0, 0), # Tip 1: freq M 0
         c(1, 1, 1)), # Tip 1: freq M 1
    list(c(0, 0, 0), # Tip 2: freq M 0
         c(1, .5, 0), # Tip 2: freq M 1/3
         c(1, 1, .5), # Tip 2: freq M 2/3
         c(1, 1, 1)) # Tip 2: freq M 1
  )
  exp_SD1 <- sqrt(4*(0.5)^2/3)
  exp_SD2 <- sqrt((2*(0.5)^2+2*((1/3)-0.5)^2)/3)
  mean_SD <- mean(c(exp_SD1, exp_SD2))
  sample_n <- 2
  expect_equal(get_islandSDFreqM(index_islands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect SD in two tips and 4 islands")
  expect_equal(get_nonislandSDFreqM(index_nonislands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect SD in two tips and 4 non-islands")
  
  # Two tips and four islands / non-islands (length 1)
  index_islands <- c(1, 2, 3, 4)
  index_nonislands <- c(1, 2, 3, 4)
  # Tip 1 mean freq m 0.5, tip 2 mean freq m 0.5
  data <- list(
    list(c(0), # Tip 1: freq M 0
         c(1), # Tip 1: freq M 1
         c(0), # Tip 1: freq M 0
         c(1)), # Tip 1: freq M 1
    list(c(0), # Tip 2: freq M 0
         c(.5), # Tip 2: freq M 0
         c(1), # Tip 2: freq M 1
         c(1)) # Tip 2: freq M 1
  )
  exp_SD1 <- sqrt(4*(0.5)^2/3)
  exp_SD2 <- sqrt(4*(0.5)^2/3)
  mean_SD <- mean(c(exp_SD1, exp_SD2))
  sample_n <- 2
  expect_equal(get_islandSDFreqM(index_islands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect SD in two tips and 4 islands (length 1)")
  expect_equal(get_nonislandSDFreqM(index_nonislands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect SD in two tips and 4 non-islands (length 1)")
  
  
  
  # Two tips with 4 islands and 4 non-islands (8 structures)
  index_islands <- c(1, 3, 5, 7)
  index_nonislands <- c(2, 4, 6, 8)
  # Tip 1 mean freq m 0.5, tip 2 mean freq m 0.5
  data <- list(
    list(c(0, 0, 0), # Tip 1 island: freq M 0
         c(0, .5, 0), # Tip 1 non-island: freq M 0
         c(1, 1, 1), # Tip 1 island: freq M 1
         c(1, 1, 1), # Tip 1 non-island: freq M 1
         c(0, 0, 0), # Tip 1 island: freq M 0
         c(0, 0, 0), # Tip 1 non-island: freq M 0
         c(1, 1, 1),# Tip 1 island: freq M 1
         c(1, 1, 1)), # Tip 1 non-island: freq M 1
    list(c(0, 0, 0), # Tip 2 island: freq M 0
         c(0, 0, 0), # Tip 2 non-island: freq M 0
         c(1, .5, 0), # Tip 2 island: freq M 1/3
         c(0, .5, 1), # Tip 2 non-island: freq M 1/3
         c(1, 1, .5), # Tip 2 island: freq M 2/3
         c(.5, 1, 1), # Tip 2 non-island: freq M 2/3
         c(1, 1, 1), # Tip 2 island: freq M 1
         c(1, 1, 1)) # Tip 2 non-island: freq M 1
  )
  exp_SD1 <- sqrt(4*(0.5)^2/3)
  exp_SD2 <- sqrt((2*(0.5)^2+2*((1/3)-0.5)^2)/3)
  mean_SD <- mean(c(exp_SD1, exp_SD2))
  sample_n <- 2
  expect_equal(get_islandSDFreqM(index_islands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect island SD in two tips 4 islands and 4 non-islands (8 structures)")
  expect_equal(get_nonislandSDFreqM(index_nonislands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect non-island SD in two tips 4 islands and 4 non-islands (8 structures)")
  
  # Two tips with only 1 island and 7 non-islands 
  index_islands <- c(1)
  index_nonislands <- c(2, 3, 4, 5, 6, 7, 8)
  # Tip 1 mean freq m 0.5, tip 2 mean freq m 0.5
  data <- list(
    list(c(0, 0, 0), # Tip 1 island: freq M 0
         c(0, 0, 0), # Tip 1 non-island: freq M 0
         c(1, 1, 1), # Tip 1 non-island: freq M 1
         c(1, 1, 1), # Tip 1 non-island: freq M 1
         c(0, 0, 0), # Tip 1 non-island: freq M 0
         c(0, 0, 0), # Tip 1 non-island: freq M 0
         c(1, 1, 1),# Tip 1 non-island: freq M 1
         c(1, 1, 1)), # Tip 1 non-island: freq M 1
    list(c(0, 0, 0), # Tip 2 island: freq M 0
         c(0, 0, 0), # Tip 2 non-island: freq M 0
         c(1, .5, 0), # Tip 2 non-island: freq M 1/3
         c(0, .5, 1), # Tip 2 non-island: freq M 1/3
         c(.5, 1, 1), # Tip 2 non-island: freq M 2/3
         c(1, 1, .5), # Tip 2 non-island: freq M 2/3
         c(1, 1, 1), # Tip 2 non-island: freq M 1
         c(1, 1, 1)) # Tip 2 non-island: freq M 1
  )
  sample_n <- 2
  expect_true(is.na(get_islandSDFreqP(index_islands, data, categorized_data = TRUE, sample_n)),
              info = "does not return NA when there is only one island structure per tip")
  
})


test_that("get_SDFreqM structures with different length islands and non-islands", {
  # One tip and four islands / non-islands
  index_islands <- c(1, 2, 3, 4)
  index_nonislands <- c(1, 2, 3, 4)
  # 1 tip mean freq m 0.5
  data <- list(c(0, 0, 0, 0, 0), # freq M 0
               c(1, 1, 1), # freq M 1
               c(0, 0, 0, 0, 0), # freq M 0
               c(1, 1, 1)) # freq M 1
  exp_SD <- sqrt(4*(0.5)^2/3) 
  sample_n <- 1
  expect_equal(get_islandSDFreqM(index_islands, data, categorized_data = TRUE, sample_n), exp_SD,
               info = "incorrect SD in one tip and 4 islands")
  expect_equal(get_nonislandSDFreqM(index_nonislands, data, categorized_data = TRUE, sample_n), exp_SD,
               info = "incorrect SD in one tip and 4 non-islands")
  
  # Two tips and four islands / non-islands
  index_islands <- c(1, 2, 3, 4)
  index_nonislands <- c(1, 2, 3, 4)
  # Tip 1 mean freq m 0.5, tip 2 mean freq m 0.5
  data <- list(
    list(c(0, 0, 0, 0, 0, 0), # Tip 1: freq M 0
         c(1, 1, 1), # Tip 1: freq M 1
         c(0, 0, 0, 0, 0, 0), # Tip 1: freq M 0
         c(1, 1, 1)), # Tip 1: freq M 1
    list(c(0, 0, 0, 0, 0, 0), # Tip 2: freq M 0
         c(1, .5, 0), # Tip 2: freq M 1/3
         c(.5, 0, 1, 1, 1, 1), # Tip 2: freq M 2/3
         c(1, 1, 1)) # Tip 2: freq M 1
  )
  exp_SD1 <- sqrt(4*(0.5)^2/3)
  exp_SD2 <- sqrt((2*(0.5)^2+2*((1/3)-0.5)^2)/3)
  mean_SD <- mean(c(exp_SD1, exp_SD2))
  sample_n <- 2
  expect_equal(get_islandSDFreqM(index_islands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect SD in two tips and 4 islands")
  expect_equal(get_nonislandSDFreqM(index_nonislands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect SD in two tips and 4 non-islands")
  
  # Two tips with 4 islands and 4 non-islands (8 structures)
  index_islands <- c(1, 3, 5, 7)
  index_nonislands <- c(2, 4, 6, 8)
  # Tip 1 mean freq m 0.5, tip 2 mean freq m 0.5
  data <- list(
    list(c(0, 0, 0, 0, 0, 0), # Tip 1 island: freq M 0
         c(0, 0, 0, 0, 0, 0), # Tip 1 non-island: freq M 0
         c(1, 1, 1), # Tip 1 island: freq M 1
         c(1, 1, 1), # Tip 1 non-island: freq M 1
         c(0, 0, 0), # Tip 1 island: freq M 0
         c(0, 0, 0), # Tip 1 non-island: freq M 0
         c(1, 1, 1),# Tip 1 island: freq M 1
         c(1, 1, 1)), # Tip 1 non-island: freq M 1
    list(c(0, 0, 0, 0, 0, 0), # Tip 2 island: freq M 0
         c(0, 0, 0, 0, 0, 0), # Tip 2 non-island: freq M 0
         c(1, .5, 0), # Tip 2 island: freq M 1/3
         c(0, .5, 1), # Tip 2 non-island: freq M 1/3
         c(1, 1, .5), # Tip 2 island: freq M 2/3
         c(.5, 1, 1), # Tip 2 non-island: freq M 2/3
         c(1, 1, 1), # Tip 2 island: freq M 1
         c(1, 1, 1)) # Tip 2 non-island: freq M 1
  )
  exp_SD1 <- sqrt(4*(0.5)^2/3)
  exp_SD2 <- sqrt((2*(0.5)^2+2*((1/3)-0.5)^2)/3)
  mean_SD <- mean(c(exp_SD1, exp_SD2))
  sample_n <- 2
  expect_equal(get_islandSDFreqM(index_islands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect island SD in two tips 4 islands and 4 non-islands (8 structures)")
  expect_equal(get_nonislandSDFreqM(index_nonislands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect non-island SD in two tips 4 islands and 4 non-islands (8 structures)")
  
  # Two tips with 4 islands and 4 non-islands (8 structures, last one only one site)
  index_islands <- c(1, 3, 5, 7)
  index_nonislands <- c(2, 4, 6, 8)
  # Tip 1 mean freq m 0.5, tip 2 mean freq m 0.5
  data <- list(
    list(c(0, 0, 0, 0, 0, 0), # Tip 1 island: freq M 0
         c(0, 0, 0, 0, 0, 0), # Tip 1 non-island: freq M 0
         c(1, 1, 1), # Tip 1 island: freq M 1
         c(1, 1, 1), # Tip 1 non-island: freq M 1
         c(0, 0, 0), # Tip 1 island: freq M 0
         c(0, 0, 0), # Tip 1 non-island: freq M 0
         c(1, 1, 1),# Tip 1 island: freq M 1
         c(1)), # Tip 1 non-island: freq M 1
    list(c(0, 0, 0, 0, 0, 0), # Tip 2 island: freq M 0
         c(0, 0, 0, 0, 0, 0), # Tip 2 non-island: freq M 0
         c(1, .5, 0), # Tip 2 island: freq M 1/3
         c(0, .5, 1), # Tip 2 non-island: freq M 1/3
         c(1, 1, .5), # Tip 2 island: freq M 2/3
         c(.5, 1, 1), # Tip 2 non-island: freq M 2/3
         c(1, 1, 1), # Tip 2 island: freq M 1
         c(1)) # Tip 2 non-island: freq M 1
  )
  exp_SD1 <- sqrt(4*(0.5)^2/3)
  exp_SD2 <- sqrt((2*(0.5)^2+2*((1/3)-0.5)^2)/3)
  mean_SD <- mean(c(exp_SD1, exp_SD2))
  sample_n <- 2
  expect_equal(get_islandSDFreqM(index_islands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect island SD in two tips 4 islands and 4 non-islands (8 structures, last one only one site)")
  expect_equal(get_nonislandSDFreqM(index_nonislands, data, categorized_data = TRUE, sample_n), mean_SD,
               info = "incorrect non-island SD in two tips 4 islands and 4 non-islands (8 structures, last one only one site)")
  
})

test_that("menCor non-valid structureIndices", {
  
  data <- list(
    list(c(0, 0, 0), c(0, 0, 0)), # tip 1
    list(c(0, 0, 0), c(1, 1, 1)) # tip 2
  )
  
  index_islands <- c(1,3)
  index_nonislands <- c(1,3)
  
  expect_error(compute_meanCor_i(data = data, index_islands = index_islands, minN_CpG = 1, shore_length = 0, sample_n =2),
               "Invalid island indices detected: 3. Number of structures in given data: 2",
               info = "fails to throw error with incorrect island index")
  expect_error(compute_meanCor_ni(data = data, index_nonislands = index_nonislands, minN_CpG = 1, shore_length = 0, sample_n =2),
               "Invalid non-island indices detected: 3. Number of structures in given data: 2",
               info = "fails to throw error with incorrect nonisland index")
})

test_that("meanCor non-categorized input of methylation states", {
  
  data <- list(
    list(c(0.1, 0.2, 0.02), c(0.05, 0.25, 0.15)), # tip 1
    list(c(0.01, 0.7, 0.85), c(0.3, 0.1, 0.98)) # tip 2
  )
  
  index_islands <- c(1,2)
  index_nonislands <- c(1,2)
  
  cor2 <- cor(c(0,0.5), c(0.5,0))
  cor3 <- cor(c(0,0.5), c(0.5,1))
  cor4 <- cor(c(0.5,0), c(0,1))
  meanCor <- mean(c(cor2, cor3, cor4))
  
  expect_equal(compute_meanCor_i(data = data, index_islands = index_islands, minN_CpG = 1, shore_length = 0, sample_n =2),
               meanCor,
               info = "incorrect value with uncategorized input data")
  expect_equal(compute_meanCor_ni(data = data, index_nonislands = index_nonislands, minN_CpG = 1, shore_length = 0, sample_n =2),
               meanCor,
               info = "incorrect value with uncategorized input data")
  
})

test_that("meanCor", {
  # Expect NA with structures under min_CpG
  
  index_islands <- c(1, 2, 3, 4)
  index_nonislands <- c(1, 2, 3, 4)
  # 1 tip
  data <- list(c(0, 0, 0, 0, .5), 
               c(.5, 1, 1), 
               c(.5, 0, 0, 0, .5), 
               c(1, 1, 1)) 
  expect_true(is.na(compute_meanCor_i(index_islands, minN_CpG = 10, shore_length = 0, data, sample_n = 1, categorized_data = TRUE)),
              info = "meanCor_i returns non-NA value when all structures are smaller than 'min_CpG' (single tip)")
  expect_true(is.na(compute_meanCor_ni(index_nonislands, minN_CpG = 10, shore_length = 0, data, sample_n = 1, categorized_data = TRUE)),
              info = "meanCor_ni returns non-NA value when all structures are smaller than 'min_CpG' (single tip)")
  # 2 tips
  data <- list(
    list(c(0, 0, 0, 0, 0, .5), 
         c(.5, 1, 1), 
         c(.5, .5, 0, 0, 0, .5), 
         c(.5, 1, 1)), 
    list(c(0, 0, 0, 0, 0, .5), 
         c(.5, 1, 1), 
         c(.5, .5, 0, 0, 0, .5), 
         c(.5, 1, 1))
  )
  expect_true(is.na(compute_meanCor_i(index_islands, minN_CpG = 10, shore_length = 0, data, sample_n = 2, categorized_data = TRUE)),
              info = "meanCor_i returns non-NA value when all structures are smaller than 'min_CpG' (2 tips)")
  expect_true(is.na(compute_meanCor_ni(index_nonislands, minN_CpG = 10, shore_length = 0, data, sample_n = 2, categorized_data = TRUE)),
              info = "meanCor_ni returns non-NA value when all structures are smaller than 'min_CpG' (2 tips)")
  
  # Expect NA when there is no variation within structures
  # 1 tip
  data <- list(c(0, 0, 0, 0, 0), 
               c(1, 1, 1, 1, 1), 
               c(0, 0, 0, 0, 0), 
               c(1, 1, 1)) 
  expect_true(is.na(compute_meanCor_i(index_islands, minN_CpG = 5, shore_length = 0, data, sample_n = 1, categorized_data = TRUE)),
              info = "meanCor_i returns non-NA value when there is no variation within structures (single tip)")
  expect_true(is.na(compute_meanCor_ni(index_nonislands, minN_CpG = 5, shore_length = 0, data, sample_n = 1, categorized_data = TRUE)),
              info = "meanCor_ni returns non-NA value when there is no variation within structures (single tip)")
  
  # 2 tips
  data <- list(
    list(c(0, 0, 0, 0, 0), 
         c(1, 1, 1, 1, 1), 
         c(0, 0, 0, 0, 0), 
         c(1, 1, 1)), 
    list(c(0, 0, 0, 0, 0), 
         c(1, 1, 1, 1, 1), 
         c(0, 0, 0, 0, 0), 
         c(1, 1, 1))
  )
  expect_true(is.na(compute_meanCor_i(index_islands, minN_CpG = 5, shore_length = 0, data, sample_n = 2, categorized_data = TRUE)),
              info = "meanCor_i returns non-NA value when there is no variation within structures (2 tips)")
  expect_true(is.na(compute_meanCor_ni(index_nonislands, minN_CpG = 5, shore_length = 0, data, sample_n = 2, categorized_data = TRUE)),
              info = "meanCor_ni returns non-NA value when there is no variation within structures (2 tips)")
  
  
  get_cor <- function(s1, s2){
    cov(s1,s2)/(sd(s1)*sd(s2))
  }
  # Test case shore length 0 equal sizes
  # 1 tip
  data <- list(c(.5, 0, 0, 0, .5), 
               c(.5, 1, 1), 
               c(.5, 0, 0, .5, 1), 
               c(1, 1, 1)) 
  cor1 <- get_cor(data[[1]][1:4], data[[1]][2:5]) ## -0.33
  cor2 <- get_cor(data[[3]][1:4], data[[3]][2:5]) ## 0.301
  expMeanCor <- mean(c(cor1, cor2)) ## -0.0159
  expect_equal(compute_meanCor_i(index_islands, minN_CpG = 5, shore_length = 0, data, sample_n = 1, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_i returns non-correct value (shore 0 1 tip)")
  expect_equal(compute_meanCor_ni(index_nonislands, minN_CpG = 5, shore_length = 0, data, sample_n = 1, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_ni returns non-correct value (shore 0 1 tip)")
  
  # 2 tips
  data <- list(
    list(c(.5, .5, 0, 0, 0), 
         c(.5, 1, 1), 
         c(.5, .5, 0, .5, .5), 
         c(.5, 1, 1)), 
    list(c(.5, .5, .5, 0, 0), 
         c(.5, 1, 1), 
         c(.5, 0, 0, .5, .5), 
         c(.5, 1, 1))
  )
  cor1_t1 <- get_cor(data[[1]][[1]][1:4], data[[1]][[1]][2:5]) ## 0.5773503
  cor2_t1 <- get_cor(data[[1]][[3]][1:4], data[[1]][[3]][2:5]) ## -0.3333333
  cor1_t2 <- get_cor(data[[2]][[1]][1:4], data[[2]][[1]][2:5]) ## 0.5773503
  cor2_t2 <- get_cor(data[[2]][[3]][1:4], data[[2]][[3]][2:5]) ## 0
  expMeanCor <- mean(c(cor1_t1, cor2_t1, cor1_t2, cor2_t2))
  expect_equal(compute_meanCor_i(index_islands, minN_CpG = 5, shore_length = 0, data, sample_n = 2, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_i returns non-correct value (shore 0 2 tip)")
  expect_equal(compute_meanCor_ni(index_nonislands, minN_CpG = 5, shore_length = 0, data, sample_n = 2, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_ni returns non-correct value (shore 0 2 tip)")
  
  # Test case shore length 0 non-equal sizes
  # 1 tip
  data <- list(c(.5, 0, 0, 0, .5, .5, .5, .5, .5, 1), # 10 sites
               c(.5, 1, 1, 1, .5), # 5 sites
               c(.5, 0, 0, .5, 1, 1), # 6 sites
               c(1, 1, 1, .5, .5, .5, 0, 0, 0, .5, .5, .5)) #12 sites
  cor1 <- get_cor(data[[1]][1:9], data[[1]][2:10]) ## 0.5
  cor2 <- get_cor(data[[2]][1:4], data[[2]][2:5]) ## -0.3333333
  cor3 <- get_cor(data[[3]][1:5], data[[3]][2:6]) ## 0.5976143
  cor4 <- get_cor(data[[4]][1:11], data[[4]][2:12]) ## 0.7370277
  expMeanCor <- mean(c(cor1, cor2, cor3, cor4)) ## 0.3753272
  expect_equal(compute_meanCor_i(index_islands, minN_CpG = 5, shore_length = 0, data, sample_n = 1, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_i returns non-correct value (shore 0 1 tip non-equal sizes)")
  expect_equal(compute_meanCor_ni(index_nonislands, minN_CpG = 5, shore_length = 0, data, sample_n = 1, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_ni returns non-correct value (shore 0 1 tip non-equal sizes)")
  
  # 2 tips
  data <- list(
    list(c(.5, 0, 0, 0, .5, .5, .5, .5, .5, 1), # 10 sites
         c(.5, 1, 1, 1, .5), # 5 sites
         c(.5, 0, 0, .5, 1, 1), # 6 sites
         c(1, 1, 1, .5, .5, .5, 0, 0, 0, .5, .5, .5)), #12 sites 
    list(c(.5, 0, 0, 0, 0, 0, .5, .5, 1, 1), # 10 sites
         c(.5, .5, 1, .5, .5), # 5 sites
         c(.5, .5, 0, .5, 1, 1), # 6 sites
         c(1, .5, .5, .5, .5, .5, 0, 0, 0, .5, 1, .5)) #12 sites
  )
  cor1_t1 <- get_cor(data[[1]][[1]][1:9], data[[1]][[1]][2:10]) ## 0.5
  cor2_t1 <- get_cor(data[[1]][[2]][1:4], data[[1]][[2]][2:5]) ## -0.3333333
  cor3_t1 <- get_cor(data[[1]][[3]][1:5], data[[1]][[3]][2:6]) ## 0.5976143
  cor4_t1 <- get_cor(data[[1]][[4]][1:11], data[[1]][[4]][2:12]) ## 0.7370277
  cor1_t2 <- get_cor(data[[2]][[1]][1:9], data[[2]][[1]][2:10]) ## 0.7284928
  cor2_t2 <- get_cor(data[[2]][[2]][1:4], data[[2]][[2]][2:5]) ## -0.3333333
  cor3_t2 <- get_cor(data[[2]][[3]][1:5], data[[2]][[3]][2:6]) ## 0.4225771
  cor4_t2 <- get_cor(data[[2]][[4]][1:11], data[[2]][[4]][2:12]) ## 0.4303315
  expMeanCor <- mean(c(cor1_t1, cor2_t1, cor3_t1, cor4_t1, cor1_t2, cor2_t2, cor3_t2, cor4_t2))
  expect_equal(compute_meanCor_i(index_islands, minN_CpG = 5, shore_length = 0, data, sample_n = 2, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_i returns non-correct value (shore 0 2 tips non-equal sizes)")
  expect_equal(compute_meanCor_ni(index_nonislands, minN_CpG = 5, shore_length = 0, data, sample_n = 2, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_ni returns non-correct value (shore 0 2 tips non-equal sizes)")
  
  
  # Test case shore length 10 equal sizes
  # 1 tip
  data <- list(c(.5, 0, 0, 0, .5, .5, .5, .5, .5, 1, .5, 0, 0, 0, .5, .5, .5, .5, .5, 1, .5, 0, 0, 0, .5, .5, .5, .5, .5, 1), # 30 sites
               c(.5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5), # 30 sites
               c(.5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1), # 30 sites
               c(1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 1, 1, 1, .5, .5, .5, 0, 0, 0, .5)) # 30 sites
  cor1 <- get_cor(data[[1]][11:19], data[[1]][12:20]) ## 0.5
  cor2 <- get_cor(data[[2]][11:19], data[[2]][12:20]) ## -1.355253e-20
  cor3 <- get_cor(data[[3]][11:19], data[[3]][12:20]) ## 0.2828427
  cor4 <- get_cor(data[[4]][11:19], data[[4]][12:20]) ## 0.7385489
  expMeanCor <- mean(c(cor1, cor2, cor3, cor4)) ## 0.3803479
  expect_equal(compute_meanCor_i(index_islands, minN_CpG = 10, shore_length = 10, data, sample_n = 1, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_i returns non-correct value (shore 10 1 tip equal sizes)")
  expect_equal(compute_meanCor_ni(index_nonislands, minN_CpG = 10, shore_length = 10, data, sample_n = 1, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_ni returns non-correct value (shore 10 1 tip equal sizes)")
  
  # 2 tips
  data <- list(
    list(c(.5, 0, 0, 0, .5, .5, .5, .5, .5, 1, .5, 0, 0, 0, .5, .5, .5, .5, .5, 1, .5, 0, 0, 0, .5, .5, .5, .5, .5, 1), # 30 sites
         c(.5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5), # 30 sites
         c(.5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1), # 30 sites
         c(1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 1, 1, 1, .5, .5, .5, 0, 0, 0, .5)), # 30 sites
    list(c(.5, 0, 0, .5, .5, .5, 0, 0, .5, 1, .5, 0, 0, 0, 0, .5, .5, 1, 1, 1, .5, 0, 0, 0, .5, .5, 1, 1, 1, 1), # 30 sites
         c(.5, .5, 1, 1, .5, .5, 1, 1, 1, .5, .5, 0, 0, 0, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5), # 30 sites
         c(.5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, .5, .5, 0, 0, .5, 1, 1, 1, 1, .5, 1, .5, 0, 0, .5, 1), # 30 sites
         c(1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 1, 1, 1, 1, .5, .5, 0, 0, 0, .5, 1, 1, 1, .5, .5, .5, .5, .5, 0, .5)) # 30 sites
  )
  cor1_t1 <- get_cor(data[[1]][[1]][11:19], data[[1]][[1]][12:20]) ## 0.5
  cor2_t1 <- get_cor(data[[1]][[2]][11:19], data[[1]][[2]][12:20]) ## -1.355253e-20
  cor3_t1 <- get_cor(data[[1]][[3]][11:19], data[[1]][[3]][12:20]) ## 0.2828427
  cor4_t1 <- get_cor(data[[1]][[4]][11:19], data[[1]][[4]][12:20]) ## 0.7385489
  cor1_t2 <- get_cor(data[[2]][[1]][11:19], data[[2]][[1]][12:20]) ## 0.7723028
  cor2_t2 <- get_cor(data[[2]][[2]][11:19], data[[2]][[2]][12:20]) ## 0.6666667
  cor3_t2 <- get_cor(data[[2]][[3]][11:19], data[[2]][[3]][12:20]) ## 0.2236068
  cor4_t2 <- get_cor(data[[2]][[4]][11:19], data[[2]][[4]][12:20]) ## 0.7777138
  expMeanCor <- mean(c(cor1_t1, cor2_t1, cor3_t1, cor4_t1, cor1_t2, cor2_t2, cor3_t2, cor4_t2))
  expect_equal(compute_meanCor_i(index_islands, minN_CpG = 10, shore_length = 10, data, sample_n = 2, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_i returns non-correct value (shore 10 2 tips equal sizes)")
  expect_equal(compute_meanCor_ni(index_nonislands, minN_CpG = 10, shore_length = 10, data, sample_n = 2, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_ni returns non-correct value (shore 10 2 tips equal sizes)")
  
  
  # Test case shore length 10 non-equal sizes
  # 1 tip
  data <- list(c(.5, 0, 0, 0, .5, .5, .5, .5, .5, 1, .5, 0, 0, 0, .5, .5, .5, .5, .5, 1, .5, 0, 0, 0, .5, .5, .5, .5, .5, 1), # 30 sites
               c(.5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5), # 25 sites
               c(.5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1), # 40 sites
               c(1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 1, 1, 1, .5, .5, .5, 0, 0, 0, .5, .5, 0, 0, 0, .5)) # 35 sites
  cor1 <- get_cor(data[[1]][11:19], data[[1]][12:20]) ## 0.5
  cor3 <- get_cor(data[[3]][11:29], data[[3]][12:30]) ## 0.2835379
  cor4 <- get_cor(data[[4]][11:24], data[[4]][12:25]) ## 0.6938887
  expMeanCor <- mean(c(cor1, cor3, cor4)) ## 0.4924755
  expect_equal(compute_meanCor_i(index_islands, minN_CpG = 10, shore_length = 10, data, sample_n = 1, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_i returns non-correct value (shore 10 1 tip non-equal sizes)")
  expect_equal(compute_meanCor_ni(index_nonislands, minN_CpG = 10, shore_length = 10, data, sample_n = 1, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_ni returns non-correct value (shore 10 1 tip non-equal sizes)")
  
  # 2 tips
  data <- list(
    list(c(.5, 0, 0, 0, .5, .5, .5, .5, .5, 1, .5, 0, 0, 0, .5, .5, .5, .5, .5, 1, .5, 0, 0, 0, .5, .5, .5, .5, .5, 1), # 30 sites
         c(.5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5), # 25 sites
         c(.5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1), # 40 sites
         c(1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 1, 1, 1, .5, .5, .5, 0, 0, 0, .5, .5, 0, 0, 0, .5)), # 35 sites
    list(c(.5, 0, 0, .5, .5, .5, 0, 0, .5, 1, .5, 0, 0, 0, 0, .5, .5, 1, 1, 1, .5, 0, 0, 0, .5, .5, 1, 1, 1, 1), # 30 sites
         c(.5, .5, 1, 1, .5, .5, 1, 1, 1, .5, .5, 0, 0, 0, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5), # 25 sites
         c(.5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, .5, .5, 0, 0, .5, 1, 1, 1, 1, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1), # 40 sites
         c(1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 1, 1, 1, 1, .5, .5, 0, 0, 0, .5, 1, 1, 1, .5, .5, .5, .5, .5, 0, .5, .5, .5, .5, 0, .5)) # 35 sites
  )
  cor1_t1 <- get_cor(data[[1]][[1]][11:19], data[[1]][[1]][12:20]) ## 0.5
  cor3_t1 <- get_cor(data[[1]][[3]][11:29], data[[1]][[3]][12:30]) ## 0.2835379
  cor4_t1 <- get_cor(data[[1]][[4]][11:24], data[[1]][[4]][12:25]) ## 0.6938887
  cor1_t2 <- get_cor(data[[2]][[1]][11:19], data[[2]][[1]][12:20]) ## 0.7723028
  cor3_t2 <- get_cor(data[[2]][[3]][11:29], data[[2]][[3]][12:30]) ## 0.5234868
  cor4_t2 <- get_cor(data[[2]][[4]][11:24], data[[2]][[4]][12:25]) ## 0.7139942
  expMeanCor <- mean(c(cor1_t1, cor3_t1, cor4_t1, cor1_t2, cor3_t2, cor4_t2))
  expect_equal(compute_meanCor_i(index_islands, minN_CpG = 10, shore_length = 10, data, sample_n = 2, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_i returns non-correct value (shore 10 2 tips non-equal sizes)")
  expect_equal(compute_meanCor_ni(index_nonislands, minN_CpG = 10, shore_length = 10, data, sample_n = 2, categorized_data = TRUE), expMeanCor,
               info = "MeanCor_ni returns non-correct value (shore 10 2 tips non-equal sizes)")
})

test_that("validate_tree input errors", {
  # No input
  expect_error(validate_tree(),
               info = "function fails to throw an error when no tree is given")
  
  # Tree not character string or phylo class
  tree <- 5
  expect_error(validate_tree(tree),
               info = "function fails to throw error when tree is not character string or phylo object")
  
  # Incorrect newick format
  tree <- "a:1, b:2"
  expect_error(validate_tree(tree),
               info = "function fails to throw error when given tree has incorrect format")
  tree <- "a:1, b:2;"
  expect_error(validate_tree(tree),
               info = "function fails to throw error when given tree has incorrect format")
  # Test error when tree has only one tip
  tree <- "(1:1);"
  expect_error(validate_tree(tree),
               info = "function fails to throw error when given tree has only one tip")
  
  # Test error when tree has one duplicated tip label
  tree <- "((a:1,b:1):2,(d:1.25,d:1.25):0.5);"
  expect_error(validate_tree(tree),
               "The input tree has duplicated tree labels: d",
               info = "function fails to throw error when given tree has one duplicated tip label")
  # Test error when tree has one triplicated tip label
  tree <- "((a:1,d:1):2,(d:1.25,d:1.25):0.5);"
  expect_error(validate_tree(tree),
               "The input tree has duplicated tree labels: d",
               info = "function fails to throw error when given tree has one triplicated tip label")
  # Test error when tree has two duplicated tip labels
  tree <- "((5:1,5:1):2,(d:1.25,d:1.25):0.5);"
  expect_error(validate_tree(tree),
               "The input tree has duplicated tree labels: 5, d",
               info = "function fails to throw error when given tree has two duplicated tip labels")
})


test_that("validate_tree returns phylo tree",{
  tree <- "(a:1,b:2);"
  o <- validate_tree(tree)
  expect_equal(class(o), "phylo")
})


test_that("get_cherryDist input control errors", {
  
  # No input
  expect_error(get_cherryDist(),
               info = "function fails to throw an error when no tree is given")
  
})

test_that("get_cherryDist processing of different input types", {
  
  # One cherry, numeric tip labels ordered
  type <- "One cherry, numeric tip labels ordered"
  tree <- "((1:0.1,2:0.1):2,3:3);"
  output <- get_cherryDist(tree)
  expect_equal(output$first_tip_name, "1",
               info = paste("Fails to output correct $first_tip_name with input type:", type))
  expect_equal(output$second_tip_name, "2",
               info = paste("Fails to output correct $second_tip_name with input type:", type))
  expect_equal(output$first_tip_index, 1,
               info = paste("Fails to output correct $first_tip_index with input type:", type))
  expect_equal(output$second_tip_index, 2,
               info = paste("Fails to output correct $second_tip_index with input type:", type))
  expect_equal(output$dist, 0.2,
               info = paste("Fails to output correct $dist tip with input type:", type))
  
  # One cherry, numeric tip labels unordered
  type <- "One cherry, numeric tip labels unordered"
  tree <- "((3:0.15,1:0.2):2,2:3);"
  output <- get_cherryDist(tree)
  expect_equal(output$first_tip_name, "3",
               info = paste("Fails to output correct $first_tip_name with input type:", type))
  expect_equal(output$second_tip_name, "1",
               info = paste("Fails to output correct $second_tip_name with input type:", type))
  expect_equal(output$first_tip_index, 1,
               info = paste("Fails to output correct $first_tip_index with input type:", type))
  expect_equal(output$second_tip_index, 2,
               info = paste("Fails to output correct $second_tip_index with input type:", type))
  expect_equal(output$dist, 0.35,
               info = paste("Fails to output correct $dist tip with input type:", type))

  
  # One cherry, named tips
  type <- "One cherry, named tips"
  tree <- "((a:0.8,b:0.2):2,c:3);"
  output <- get_cherryDist(tree)
  expect_equal(output$first_tip_name, "a",
               info = paste("Fails to output correct $first_tip_name with input type:", type))
  expect_equal(output$second_tip_name, "b",
               info = paste("Fails to output correct $second_tip_name with input type:", type))
  expect_equal(output$first_tip_index, 1,
               info = paste("Fails to output correct $first_tip_index with input type:", type))
  expect_equal(output$second_tip_index, 2,
               info = paste("Fails to output correct $second_tip_index with input type:", type))
  expect_equal(output$dist, 1,
               info = paste("Fails to output correct $dist tip with input type:", type))
  
  
  # One cherry, named tips, cherry to the right of tree
  type <- "One cherry, named tips, cherry to the right of tree"
  tree <- "(a:3,(c:2.1:,b:2.1):1);"
  output <- get_cherryDist(tree)
  expect_equal(output$first_tip_name, "c",
               info = paste("Fails to output correct $first_tip_name with input type:", type))
  expect_equal(output$second_tip_name, "b",
               info = paste("Fails to output correct $second_tip_name with input type:", type))
  expect_equal(output$first_tip_index, 2,
               info = paste("Fails to output correct $first_tip_index with input type:", type))
  expect_equal(output$second_tip_index, 3,
               info = paste("Fails to output correct $second_tip_index with input type:", type))
  expect_equal(output$dist, 4.2,
               info = paste("Fails to output correct $dist tip with input type:", type))
  
  
  # Two cherries, numeric tip labels ordered
  type <- "Two cherries, numeric tip labels ordered"
  tree <- "((1:0.1,2:0.1):3,(3:3,4:3):0.2);"
  output <- get_cherryDist(tree)
  expect_equal(output$first_tip_name, c("1", "3"),
               info = paste("Fails to output correct $first_tip_name with input type:", type))
  expect_equal(output$second_tip_name, c("2", "4"),
               info = paste("Fails to output correct $second_tip_name with input type:", type))
  expect_equal(output$first_tip_index, c(1,3),
               info = paste("Fails to output correct $first_tip_index with input type:", type))
  expect_equal(output$second_tip_index, c(2,4),
               info = paste("Fails to output correct $second_tip_index with input type:", type))
  expect_equal(output$dist, c(0.2, 6),
               info = paste("Fails to output correct $dist tip with input type:", type))

  
  # Two cherries, numeric tip labels unordered
  type <- "Two cherries, numeric tip labels unordered"
  tree <- "((1:0.15,5:0.2):10,(2:10,10:10):0.35);"
  output <- get_cherryDist(tree)
  expect_equal(output$first_tip_name, c("1", "2"),
               info = paste("Fails to output correct $first_tip_name with input type:", type))
  expect_equal(output$second_tip_name, c("5", "10"),
               info = paste("Fails to output correct $second_tip_name with input type:", type))
  expect_equal(output$first_tip_index, c(1,3),
               info = paste("Fails to output correct $first_tip_index with input type:", type))
  expect_equal(output$second_tip_index, c(2,4),
               info = paste("Fails to output correct $second_tip_index with input type:", type))
  expect_equal(output$dist, c(0.35, 20),
               info = paste("Fails to output correct $dist tip with input type:", type))
  
  
  # Two cherries, named tips
  type <- "Two cherries, named tips"
  tree <- "((a:0.15,b:0.2):10,(c:0.1,1d:10):0.35);"
  output <- get_cherryDist(tree)
  expect_equal(output$first_tip_name, c("a", "c"),
               info = paste("Fails to output correct $first_tip_name with input type:", type))
  expect_equal(output$second_tip_name, c("b", "1d"),
               info = paste("Fails to output correct $second_tip_name with input type:", type))
  expect_equal(output$first_tip_index, c(1,3),
               info = paste("Fails to output correct $first_tip_index with input type:", type))
  expect_equal(output$second_tip_index, c(2,4),
               info = paste("Fails to output correct $second_tip_index with input type:", type))
  expect_equal(output$dist, c(0.35, 10.1),
               info = paste("Fails to output correct $dist tip with input type:", type))

})


test_that("validate_data_cherryDist", {
  
  # Different number of structures across tips
  type <- "Different number of structures across tips"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10))) 
  cherryDist <- get_cherryDist(tree)
  expect_error(validate_data_cherryDist(cherryDist, data),
               info = paste(type, "fails to throw error"))
  
  # Tip without structures
  type <- "Tip without structures"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list()) 
  cherryDist <- get_cherryDist(tree)
  expect_error(validate_data_cherryDist(cherryDist, data),
               info = paste(type, "fails to throw error"))
  
  # Data with a structure of length 0
  type <- "Data with a structure of length 0"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), c()), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), c()), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), c())) 
  cherryDist <- get_cherryDist(tree)
  expect_error(validate_data_cherryDist(cherryDist, data),
               info = paste(type, "fails to throw error"))
  
  # Different number of sites in data
  type <- "Different number of sites in data"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,11)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10))) 
  cherryDist <- get_cherryDist(tree)
  expect_error(validate_data_cherryDist(cherryDist, data),
               info = paste(type, "fails to throw error"))
  
  # Number of tips in data smaller than given cherry tip indices
  type <- "Number of tips in data smaller than given cherry tip indices"
  tree <- "((1:1,2:1):2,(3:2,5:2):1);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,11)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10))) 
  cherryDist <- get_cherryDist(tree)
  expect_error(validate_data_cherryDist(cherryDist, data),
               info = paste(type, "fails to throw error"))
})

test_that("countSites_cherryMethDiff input control errors", {
  
  # Incomplete argument cherryDist
  type <- "Incomplete argument cherryDist"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10))) 
  cherryDist <- get_cherryDist(tree)
  cherryDist <- cherryDist[,-1]
  expect_error(countSites_cherryMethDiff(cherryDist, data),
               info = paste(type, "fails to throw error"))
  
  # No cherry info in cherryDist
  type <- "No cherry info in cherryDist"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10))) 
  cherryDist <- get_cherryDist(tree)
  cherryDist <- cherryDist[-1,]
  expect_error(countSites_cherryMethDiff(cherryDist, data),
               info = paste(type, "fails to throw error"))
  
  # Different number of structures across tips
  type <- "Different number of structures across tips"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10))) 
  cherryDist <- get_cherryDist(tree)
  expect_error(countSites_cherryMethDiff(cherryDist, data),
               info = paste(type, "fails to throw error"))
  
  # Tip without structures
  type <- "Tip without structures"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list()) 
  cherryDist <- get_cherryDist(tree)
  expect_error(countSites_cherryMethDiff(cherryDist, data),
               info = paste(type, "fails to throw error"))
  
  # Data with a structure of length 0
  type <- "Data with a structure of length 0"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), c()), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), c()), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), c())) 
  cherryDist <- get_cherryDist(tree)
  expect_error(countSites_cherryMethDiff(cherryDist, data),
               info = paste(type, "fails to throw error"))
  
  # Different number of sites in data
  type <- "Different number of sites in data"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,11)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10))) 
  cherryDist <- get_cherryDist(tree)
  expect_error(countSites_cherryMethDiff(cherryDist, data),
               info = paste(type, "fails to throw error"))
  
  # Number of tips in data smaller than given cherry tip indices
  type <- "Number of tips in data smaller than given cherry tip indices"
  tree <- "((1:1,2:1):2,(3:2,5:2):1);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,11)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10))) 
  cherryDist <- get_cherryDist(tree)
  expect_error(countSites_cherryMethDiff(cherryDist, data),
               info = paste(type, "fails to throw error"))
})

test_that("countSites_cherryMethDiff non-categorized input of methylation states", {
  
  tree <- "(a:1,b:1);"
  data <- list(
    list(c(0.1, 0.2, 0.02), c(0.05, 0.25, 0.15)), # tip 1
    list(c(0.01, 0.7, 0.85), c(0.3, 0.1, 0.98)) # tip 2
  )
  
  cherryDist <- get_cherryDist(tree)
  o <- countSites_cherryMethDiff(cherryDist, data)
  
  expect_true(all(as.numeric(o[1,-c(1,2,3)]) == c(1,1,1,2)),
              info = "incorrect value with uncategorized input data")
  
})

test_that("countSites_cherryMethDiff correct output with different input types", {
  
  # One cherry numeric labels (left newick)
  type <- "One cherry numeric labels (left newick)"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10))) 
  cherryDist <- get_cherryDist(tree)
  o <- countSites_cherryMethDiff(cherryDist, data, categorized_data = TRUE)
  expect_equal(nrow(o), 1, 
               info = paste("Number of output rows does not correspond with input type:", type))
  expect_equal(o$tip_names, "1-2", 
               info = paste("Tip names do not correspond with input type:", type))
  expect_equal(o$tip_indices, "1-2", 
               info = paste("Tip indices do not correspond with input type:", type))
  expect_equal(o$dist, 2, 
               info = paste("Tips distance does not correspond with input type:", type))
  expect_true(all(as.numeric(o[1,-c(1,2,3)]) == c(0, 0, 0, 10, 10, 0)),
              info = paste("Incorrect count number with input type:", type))
  
  # One cherry named labels (right newick)
  type <- "One cherry named labels (right newick)"
  tree <- "(c:5,(b:1.3,a:1.3):2.4);"
  data <- list(
    list(rep(1,10), rep(0,15), rep(1,15)), 
    list(rep(1,10), rep(0.5,15), rep(0,15)), # tip 1 cherry
    list(c(rep(0.5,8), 0, 1), c(rep(0.5,10), 1, 1, 0, 0, 0), rep(0,15))) # tip 2 cherry
  cherryDist <- get_cherryDist(tree)
  o <- countSites_cherryMethDiff(cherryDist, data, categorized_data = TRUE)
  expect_equal(nrow(o), 1, 
               info = paste("Number of output rows does not correspond with input type:", type))
  expect_equal(o$tip_names, "b-a", 
               info = paste("Tip names do not correspond with input type:", type))
  expect_equal(o$tip_indices, "2-3", 
               info = paste("Tip indices do not correspond with input type:", type))
  expect_equal(o$dist, 2.6, 
               info = paste("Tips distance does not correspond with input type:", type))
  expect_true(all(as.numeric(o[1,-c(1,2,3)]) == c(1, 8, 0, 5, 0, 0)),
              info = paste("Incorrect count number with input type:", type))
  
  # Test case two cherries numeric labels unordered
  tree <- "((25:1.51,24:1.51):2,(38:2.1,45:2.1):1.5);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)),
    list(rep(1,10), rep(0.5,10), rep(0,10)),
    list(rep(1,10), rep(0.5,10), rep(0,10)),
    list(c(rep(0,5), rep(0.5, 5)), c(0, 0, 1, 1, 1, rep(0.5, 5)), c(0.5, 1, rep(0, 8))))
  cherryDist <- get_cherryDist(tree)
  o <- countSites_cherryMethDiff(cherryDist, data, categorized_data = TRUE)
  expect_equal(nrow(o), 2, 
               info = paste("Number of output rows does not correspond with input type:", type))
  expect_equal(o$tip_names, c("25-24", "38-45"), 
               info = paste("Tip names do not correspond with input type:", type))
  expect_equal(o$tip_indices, c("1-2", "3-4"), 
               info = paste("Tip indices do not correspond with input type:", type))
  expect_equal(o$dist, c(3.02, 4.2), 
               info = paste("Tips distances do not correspond with input type:", type))
  expect_true(all(as.numeric(o[1,-c(1,2,3)]) == c(0, 0, 0, 10, 10, 0)),
              info = "case two cherries incorrect count output for first cherry")
  expect_true(all(as.numeric(o[2,-c(1,2,3)]) == c(5, 5, 0, 5, 1, 1)),
              info = "case two cherries incorrect count output for second cherry")
  
})


test_that("freqSites_cherryMethDiff input control error", {

  # No input
  expect_error(freqSites_cherryMethDiff(),
               info = "function fails to throw an error when no tree is given")
  
  ## Check errors for incorrect tree argument ##
  
  # Tree not character string or phylo class
  tree <- 5
  expect_error(freqSites_cherryMethDiff(tree),
               info = "function fails to throw error when tree is not character string or phylo object")
  
  # Incorrect newick format
  tree <- "a:1, b:2"
  expect_error(freqSites_cherryMethDiff(tree),
               info = "function fails to throw error when given tree has incorrect format")
  tree <- "a:1, b:2;"
  expect_error(freqSites_cherryMethDiff(tree),
               info = "function fails to throw error when given tree has incorrect format")
  # Test error when tree has only one tip
  tree <- "(1:1);"
  expect_error(freqSites_cherryMethDiff(tree),
               info = "function fails to throw error when given tree has only one tip")
  
  
  ## Check errors for incorrect data argument ##
  
  # Different number of structures across tips
  type <- "Different number of structures across tips"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10))) 
  expect_error(freqSites_cherryMethDiff(tree, data),
               info = paste(type, "fails to throw error"))
  
  # Tip without structures
  type <- "Tip without structures"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list()) 
  expect_error(freqSites_cherryMethDiff(tree, data),
               info = paste(type, "fails to throw error"))
  
  # Data with a structure of length 0
  type <- "Data with a structure of length 0"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), c()), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), c()), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), c())) 
  expect_error(freqSites_cherryMethDiff(tree, data),
               info = paste(type, "fails to throw error"))
  
  # Different number of sites in data
  type <- "Different number of sites in data"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,11)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10))) 
  expect_error(freqSites_cherryMethDiff(tree, data),
               info = paste(type, "fails to throw error"))
  
  # Number of tips in data smaller than given cherry tip indices
  type <- "Number of tips in data smaller than given cherry tip indices"
  tree <- "((1:1,2:1):2,(3:2,5:2):1);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,11)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10))) 
  expect_error(freqSites_cherryMethDiff(tree, data),
               info = paste(type, "fails to throw error"))
})

test_that("freqSites_cherryMethDiff non-categorized input of methylation states", {
  
  tree <- "(a:1,b:1);"
  data <- list(
    list(c(0.1, 0.2, 0.02), c(0.05, 0.25, 0.15)), # tip 1
    list(c(0.01, 0.7, 0.85), c(0.3, 0.1, 0.98)) # tip 2
  )
  
  o <- freqSites_cherryMethDiff(tree = tree, data = data)
  
  expect_equal(as.numeric(o[1,4:ncol(o)]), c(1/3, 1/3, 1/3, 2/3),
               info = "incorrect value with uncategorized input data")

})


test_that("freqSites_cherryMethDiff output", {
  
  # Test counts are transformed into frequencies correctly
  tree <- "((a:1.5,b:1.5):2,(c:2,d:2):1.5);"
  data <- list(
    list(rep(1,10), rep(0,5), rep(1,8)),
    list(rep(1,10), rep(0.5,5), rep(0,8)),
    list(rep(1,10), rep(0.5,5), rep(0,8)),
    list(c(rep(0,5), rep(0.5, 5)), c(0, 0, 1, 1, 1), c(0.5, 1, rep(0, 6))))
  o <- freqSites_cherryMethDiff(tree = tree, data = data, categorized_data = TRUE)
  expect_equal(o$tip_names[1], "a-b",
               info = "incorrect tip names in cherry 1")
  expect_equal(o$tip_names[2], "c-d",
               info = "incorrect tip names in cherry 2")
  expect_equal(o$tip_indices[1], "1-2",
               info = "incorrect tip names in cherry 1")
  expect_equal(o$tip_indices[2], "3-4",
               info = "incorrect tip names in cherry 2")
  expect_equal(o$dist[1], 3,
               info = "incorrect tips distance in cherry 1")
  expect_equal(o$dist[2], 4,
               info = "incorrect tips distance in cherry 2")
  column_CpGn <- rep(c(10, 5, 8), each=2)
  f_h_count <- c(0, 0, 0, 5, 8, 0)
  f_h_freq <- f_h_count/column_CpGn
  expect_equal(as.numeric(o[1,4:ncol(o)]), f_h_freq,
               info = "incorrect f and h freqs in cherry 1")
  f_h_count <- c(5, 5, 0, 5, 1, 1)
  f_h_freq <- f_h_count/column_CpGn
  expect_equal(as.numeric(o[2,4:ncol(o)]), f_h_freq,
               info = "incorrect f and h freqs in cherry 2")
})

test_that("get_siteFChange_cherry wrong input", {
  
  # No input
  expect_error(get_siteFChange_cherry(),
               info = "function fails to throw an error when no tree is given")
  
  ## Check errors for incorrect tree argument ##
  
  # Tree not character string or phylo class
  tree <- 5
  expect_error(get_siteFChange_cherry(tree),
               info = "function fails to throw error when tree is not character string or phylo object")
  
  # Incorrect newick format
  tree <- "a:1, b:2"
  expect_error(get_siteFChange_cherry(tree),
               info = "function fails to throw error when given tree has incorrect format")
  tree <- "a:1, b:2;"
  expect_error(get_siteFChange_cherry(tree),
               info = "function fails to throw error when given tree has incorrect format")
  # Test error when tree has only one tip
  tree <- "(1:1);"
  expect_error(get_siteFChange_cherry(tree),
               info = "function fails to throw error when given tree has only one tip")
  
  
  ## Check errors for incorrect data argument ##
  
  # Different number of structures across tips
  type <- "Different number of structures across tips"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10))) 
  expect_error(get_siteFChange_cherry(tree, data),
               info = paste(type, "fails to throw error"))
  
  # Tip without structures
  type <- "Tip without structures"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list()) 
  expect_error(get_siteFChange_cherry(tree, data),
               info = paste(type, "fails to throw error"))
  
  # Data with a structure of length 0
  type <- "Data with a structure of length 0"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), c()), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), c()), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), c())) 
  expect_error(get_siteFChange_cherry(tree, data),
               info = paste(type, "fails to throw error"))
  
  # Different number of sites in data
  type <- "Different number of sites in data"
  tree <- "((1:1,2:1):1,3:2);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,11)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10))) 
  expect_error(get_siteFChange_cherry(tree, data),
               info = paste(type, "fails to throw error"))
  
  # Number of tips in data smaller than given cherry tip indices
  type <- "Number of tips in data smaller than given cherry tip indices"
  tree <- "((1:1,2:1):2,(3:2,5:2):1);"
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,11)), # tip 1 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10)), # tip 2 cherry
    list(rep(1,10), rep(0.5,10), rep(0,10))) 
  expect_error(get_siteFChange_cherry(tree, data),
               info = paste(type, "fails to throw error"))
})

test_that("get_MeanFreqP non-categorized input of methylation states", {
  
  tree <- "(a:1,b:1);"
  data <- list(
    list(c(0.1, 0.2, 0.02), c(0.05, 0.25, 0.15)), # tip 1
    list(c(0.01, 0.7, 0.85), c(0.3, 0.1, 0.98)) # tip 2
  )
  
  o <- get_siteFChange_cherry(tree = tree, data = data)
  
  expect_equal(as.numeric(o[1,4:ncol(o)]), c(2/3, 1),
               info = "incorrect value with uncategorized input data")

})

test_that("get_siteFChange_cherry output", {
  
  # Test frequency of sites changing per structure and cherry is computed correctly
  tree <- "((a:1.5,b:1.5):2,(c:2,d:2):1.5);"
  data <- list(
    list(rep(1,10), rep(0,5), rep(1,8)),
    list(rep(1,10), rep(0.5,5), rep(0,8)),
    list(rep(1,10), rep(0.5,5), rep(0,8)),
    list(c(rep(0,5), rep(0.5, 5)), c(0, 0, 1, 1, 1), c(0.5, 1, rep(0, 6))))
  
  o <- get_siteFChange_cherry(tree = tree, data = data, categorized_data = TRUE)
  expect_equal(o$tip_names[1], "a-b",
               info = "incorrect tip names in cherry 1")
  expect_equal(o$tip_names[2], "c-d",
               info = "incorrect tip names in cherry 2")
  expect_equal(o$tip_indices[1], "1-2",
               info = "incorrect tip names in cherry 1")
  expect_equal(o$tip_indices[2], "3-4",
               info = "incorrect tip names in cherry 2")
  expect_equal(o$dist[1], 3,
               info = "incorrect tips distance in cherry 1")
  expect_equal(o$dist[2], 4,
               info = "incorrect tips distance in cherry 2")
  Fchange_firstCherry <- c(0,1,1)
  expect_equal(as.numeric(o[1,4:ncol(o)]), Fchange_firstCherry,
               info = "incorrect f and h freqs in cherry 1")
  Fchange_secondCherry <- c(1,1,0.25)
  expect_equal(as.numeric(o[2,4:ncol(o)]), Fchange_secondCherry,
               info = "incorrect f and h freqs in cherry 2")
})

test_that("validate_structureIndices", {
  
  # Example data
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)),
    list(rep(1,10), rep(0.5,10), rep(0,10)), 
    list(rep(1,10), rep(0.5,10), rep(0,10))
  )
  
  expect_silent(validate_structureIndices(data, c(1, 2), c(3)))
  expect_error(validate_structureIndices(data, c(4), c(1)), 
               info = "fails to report incorrect island index") 
  expect_error(validate_structureIndices(data, c(1), c(5)), 
               info = "fails to report incorrect non-island index")
  expect_warning(validate_structureIndices(data, c(1, 2), c(2, 3)), 
                 info = "fails to report duplicated index in both islands and non_islands")
  
  
})



test_that("MeanSiteFChange_cherry input control", {
  # Example data
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)),
    list(rep(1,10), rep(0.5,10), rep(0,10)), 
    list(rep(1,10), rep(0.5,10), rep(0,10))
  )
  # Example tree
  tree <- "((a:1,b:1):1,c:2);"
  
  # Test error when no argument is given
  expect_error(MeanSiteFChange_cherry(),
               info = "fails to throw an error when no argument is given")
  
  # Test errors incorrect structure indices
  expect_error(MeanSiteFChange_cherry(data, tree, index_islands = c(4), index_nonislands = c(1)), 
               info = "fails to report incorrect island index") 
  expect_error(MeanSiteFChange_cherry(data, tree, index_islands = c(1), index_nonislands = c(5)), 
               info = "fails to report incorrect non-island index")
  expect_error(MeanSiteFChange_cherry(data, tree, index_islands = c(1, 2), index_nonislands = c(2, 3)), 
                 info = "fails to report duplicated index in both islands and non_islands")
  
  # Test errors with non-consistent number of structures
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)),
    list(rep(1,10), rep(0.5,10), rep(0,10)), 
    list(rep(1,10), rep(0.5,10))
  )
  expect_error(MeanSiteFChange_cherry(data, tree, index_islands = c(2,3), index_nonislands = c(1)),
               info = "fails to throw an error when number of structures is non-equal at different tips")
  
  # Test errors with non-consistent number of sites for a given structure in different tips
  data <- list(
    list(rep(1,10), rep(0,10), rep(1,10)),
    list(rep(1,10), rep(0.5,10), rep(0,10)), 
    list(rep(1,10), rep(0.5,10), rep(0,8))
  )
  expect_error(MeanSiteFChange_cherry(data, tree, index_islands = c(2,3), index_nonislands = c(1)),
               info = "fails to throw an error when number of sites is non-equal for a given structure at different tips")
})

test_that("MeanSiteFChange_cherry non-categorized input of methylation states", {
  
  tree <- "(a:1,b:1);"
  data <- list(
    list(c(0.1, 0.2, 0.02), c(0.05, 0.25, 0.15)), # tip 1 (u,u,u)(u,p,u)
    list(c(0.01, 0.7, 0.85), c(0.3, 0.1, 0.98)) # tip 2 (u,p,m)(p,u,m)
  )
  
  expect_equal(MeanSiteFChange_cherry(data = data, tree = tree, index_islands = c(1,2), index_nonislands = c())$island_meanFChange, 5/6,
               info = "incorrect value with uncategorized input data")
  expect_equal(MeanSiteFChange_cherry(data = data, tree = tree, index_islands = c(), index_nonislands = c(1))$nonisland_meanFChange, 2/3,
               info = "incorrect value with uncategorized input data")
})

test_that("MeanSiteFChange_cherry output", {
  
  tree <- "((1:1.5,2:1.5):2,(3:2,4:2):1.5);"
  data <- list(
    list(rep(1,10), rep(0,5), rep(1,8)),
    list(rep(1,10), rep(0.5,5), rep(0,8)),
    list(rep(1,10), rep(0.5,5), rep(0,8)),
    list(c(rep(0,5), rep(0.5, 5)), c(0, 0, 1, 1, 1), c(0.5, 1, rep(0, 6))))
  str1_sites_n <- 10
  str2_sites_n <- 5
  str3_sites_n <- 8
  total_sites_n <- str1_sites_n + str2_sites_n + str3_sites_n
  
  # Test case all structures are islands
  test <- "All structures are islands"
  index_islands <- c(1,2,3)
  index_nonislands <- c()
  o <- MeanSiteFChange_cherry(data = data, categorized_data = TRUE, tree = tree, 
                              index_islands = index_islands, index_nonislands = index_nonislands)
  exp_mean_ch1 <- (0+5+8)/total_sites_n
  exp_mean_ch2 <- (10+5+2)/total_sites_n
  expect_equal(o$island_meanFChange, c(exp_mean_ch1, exp_mean_ch2),
               info = paste("returns incorrect island_meanFChange in test case", test))
  expect_true(all(is.na(o$nonisland_meanFChange)),
              info = paste("returns incorrect nonisland_meanFChange in test case", test))
  
  # Test case all structures are non-islands
  test <- "All structures are non-islands"
  index_nonislands <- c(1,2,3)
  index_islands <- c()
  o <- MeanSiteFChange_cherry(data = data, categorized_data = TRUE, tree = tree, 
                              index_islands = index_islands, index_nonislands = index_nonislands)
  exp_mean_ch1 <- (0+5+8)/total_sites_n
  exp_mean_ch2 <- (10+5+2)/total_sites_n
  expect_equal(o$nonisland_meanFChange, c(exp_mean_ch1, exp_mean_ch2),
               info = paste("returns incorrect nonisland_meanFChange in test case", test))
  expect_true(all(is.na(o$island_meanFChange)),
              info = paste("returns incorrect island_meanFChange in test case", test))
  
  # Test case 2 non-islands, 1 island
  test <- "2 non-islands, 1 island"
  index_nonislands <- c(1,3)
  index_islands <- c(2)
  o <- MeanSiteFChange_cherry(data = data, categorized_data = TRUE, tree = tree, 
                              index_islands = index_islands, index_nonislands = index_nonislands)
  exp_meanI_ch1 <- (5)/str2_sites_n
  exp_meanNI_ch1 <- (0+8)/(str1_sites_n+str3_sites_n)
  exp_meanI_ch2 <- (5)/str2_sites_n
  exp_meanNI_ch2 <- (10+2)/(str1_sites_n+str3_sites_n)
  expect_equal(o$island_meanFChange, c(exp_meanI_ch1, exp_meanI_ch2),
               info = paste("returns incorrect island_meanFChange in test case", test))
  expect_equal(o$nonisland_meanFChange, c(exp_meanNI_ch1, exp_meanNI_ch2),
               info = paste("returns incorrect nonisland_meanFChange in test case", test))
  
  # Test case 2 islands, 1 non-island
  test <- "2 islands, 1 non-island"
  index_islands <- c(1,3)
  index_nonislands <- c(2)
  o <- MeanSiteFChange_cherry(data = data, categorized_data = TRUE, tree = tree, 
                              index_islands = index_islands, index_nonislands = index_nonislands)
  exp_meanNI_ch1 <- (5)/str2_sites_n
  exp_meanI_ch1 <- (0+8)/(str1_sites_n+str3_sites_n)
  exp_meanNI_ch2 <- (5)/str2_sites_n
  exp_meanI_ch2 <- (10+2)/(str1_sites_n+str3_sites_n)
  expect_equal(o$island_meanFChange, c(exp_meanI_ch1, exp_meanI_ch2),
               info = paste("returns incorrect island_meanFChange in test case", test))
  expect_equal(o$nonisland_meanFChange, c(exp_meanNI_ch1, exp_meanNI_ch2),
               info = paste("returns incorrect nonisland_meanFChange in test case", test))
  
  # Test case data with one singlestr
  test <- "Data with one singleStr: island"
  data <- list(
    list(rep(1,10)),
    list(rep(1,10)),
    list(rep(1,10)),
    list(c(rep(0,5), rep(0.5, 5))))
  str1_sites_n <- 10
  index_islands <- c(1)
  index_nonislands <- c()
  o <- MeanSiteFChange_cherry(data = data, categorized_data = TRUE, tree = tree, 
                              index_islands = index_islands, index_nonislands = index_nonislands)
  expect_equal(o$island_meanFChange, c(0, 1),
               info = paste("returns incorrect island_meanFChange in test case", test))
  expect_true(all(is.na(o$nonisland_meanFChange)),
              info = paste("returns incorrect nonisland_meanFChange in test case", test))
  
  # Test case data with one singlestr
  test <- "Data with one singleStr: nonisland"
  data <- list(
    list(rep(1,10)),
    list(rep(1,10)),
    list(rep(1,10)),
    list(c(rep(0,5), rep(0.5, 5))))
  str1_sites_n <- 10
  index_nonislands <- c(1)
  index_islands <- c()
  o <- MeanSiteFChange_cherry(data = data, categorized_data = TRUE, tree = tree, 
                              index_islands = index_islands, index_nonislands = index_nonislands)
  expect_equal(o$nonisland_meanFChange, c(0, 1),
               info = paste("returns incorrect nonisland_meanFChange in test case", test))
  expect_true(all(is.na(o$island_meanFChange)),
              info = paste("returns incorrect island_meanFChange in test case", test))
  
  # Test case data with one singleStr and one site
  test <- "Data with one singleStr with a single site: island"
  data <- list(
    list(c(1)),
    list(c(1)),
    list(c(1)),
    list(c(0.5)))
  str1_sites_n <- 10
  index_islands <- c(1)
  index_nonislands <- c()
  o <- MeanSiteFChange_cherry(data = data, categorized_data = TRUE, tree = tree, 
                              index_islands = index_islands, index_nonislands = index_nonislands)
  expect_equal(o$island_meanFChange, c(0, 1),
               info = paste("returns incorrect island_meanFChange in test case", test))
  expect_true(all(is.na(o$nonisland_meanFChange)),
              info = paste("returns incorrect nonisland_meanFChange in test case", test))
  
  # Test case data with one singleStr and one site
  test <- "Data with one singleStr with a single site: nonisland"
  data <- list(
    list(c(1)),
    list(c(1)),
    list(c(1)),
    list(c(0.5)))
  str1_sites_n <- 10
  index_islands <- c()
  index_nonislands <- c(1)
  o <- MeanSiteFChange_cherry(data = data, categorized_data = TRUE, tree = tree, 
                              index_islands = index_islands, index_nonislands = index_nonislands)
  expect_equal(o$nonisland_meanFChange, c(0, 1),
               info = paste("returns incorrect nonisland_meanFChange in test case", test))
  expect_true(all(is.na(o$island_meanFChange)),
              info = paste("returns incorrect island_meanFChange in test case", test))
  
  
  # Test case with tree with a single cherry
  test <- "Data with tree with a single cherry"
  tree <- "((a:1,b:1):1,c:2);"
  data <- list(
    list(c(1)),
    list(c(1)),
    list(c(0.5)))
  str1_sites_n <- 10
  index_islands <- c()
  index_nonislands <- c(1)
  o <- MeanSiteFChange_cherry(data = data, categorized_data = TRUE, tree = tree, 
                              index_islands = index_islands, index_nonislands = index_nonislands)
  expect_equal(o$nonisland_meanFChange, 0,
               info = paste("returns incorrect nonisland_meanFChange in test case", test))
  expect_true(is.na(o$island_meanFChange),
              info = paste("returns incorrect island_meanFChange in test case", test))
  
})


test_that("get_meanMeth_islands input control",{
  data <- list(
    #Tip 1
    list(c(rep(1,5), rep(0,5)), # mean 0.5
         c(rep(0,9), 1), # mean 0.1
         c(rep(1,8), rep(0.5,2))), # mean 0.9
    #Tip 2
    list(c(rep(1,9), rep(0.5,1)), # mean 0.95
         c(rep(0.5,9), 1), # mean 0.55
         c(rep(1,8), rep(0.5,2))), # mean 0.9
    #Tip 3
    list(c(rep(1,9), rep(0.5,1)), # mean 0.95
         c(rep(0.5,9), 1), # mean 0.55
         c(rep(1,8), rep(0.5,2))), # mean 0.9
    #Tip 4
    list(c(rep(1,9), rep(0.5,1)), # mean 0.95
         c(rep(1,9), 0), # mean 0.9
         c(rep(1,8), rep(0.5,2)))) # mean 0.9
  
  index_islands <- c(1,4)
  
  expect_error(get_meanMeth_islands(index_islands = index_islands, data = data),
               "Invalid island indices detected: 4. Number of structures in given data: 3",
               info = "fails to throw error with incorrect island index")
  
  index_islands <- c()
  expect_error(get_meanMeth_islands(index_islands = index_islands, data = data),
               "'index_islands' has no indices")
  
})

test_that("get_meanMeth_islands several sites per island", {
  data <- list(
    #Tip 1
    list(c(rep(1,5), rep(0,5)), # mean 0.5
         c(rep(0,9), 1), # mean 0.1
         c(rep(1,8), rep(0.5,2))), # mean 0.9
    #Tip 2
    list(c(rep(1,9), rep(0.5,1)), # mean 0.95
         c(rep(0.5,9), 1), # mean 0.55
         c(rep(1,8), rep(0.5,2))), # mean 0.9
    #Tip 3
    list(c(rep(1,9), rep(0.5,1)), # mean 0.95
         c(rep(0.5,9), 1), # mean 0.55
         c(rep(1,8), rep(0.5,2))), # mean 0.9
    #Tip 4
    list(c(rep(1,9), rep(0.5,1)), # mean 0.95
         c(rep(1,9), 0), # mean 0.9
         c(rep(1,8), rep(0.5,2)))) # mean 0.9
  
  index_islands <- c(1,3)
  o <- get_meanMeth_islands(index_islands = index_islands, data = data)
  expect_equal(o[[1]], c(0.5, 0.9),
               info = "incorrect output tip 1")
  expect_equal(o[[2]], c(0.95, 0.9),
               info = "incorrect output tip 2")
  expect_equal(o[[3]], c(0.95, 0.9),
               info = "incorrect output tip 3")
  expect_equal(o[[4]], c(0.95, 0.9),
               info = "incorrect output tip 4")
  
  index_islands <- c(2)
  o <- get_meanMeth_islands(index_islands = index_islands, data = data)
  expect_equal(o[[1]], 0.1,
               info = "incorrect output tip 1")
  expect_equal(o[[2]], 0.55,
               info = "incorrect output tip 2")
  expect_equal(o[[3]], 0.55,
               info = "incorrect output tip 3")
  expect_equal(o[[4]], 0.9,
               info = "incorrect output tip 4")
})

test_that("categorize_islandGlbSt input control", {
  data <- list(
    list(c(rep(1,5), rep(0,5)), # mean 0.5
         c(rep(0,9), 1), # mean 0.1
         c(rep(1,8), rep(0.5,2)))) # mean 0.9
  index_islands <- c(1,3)
  meanMeth_islands <- get_meanMeth_islands(index_islands = index_islands, data = data)

  u_threshold <- -0.1
  m_threshold <- 0.9
  expect_error(categorize_islandGlbSt(meanMeth_islands[[1]], u_threshold = u_threshold, m_threshold = m_threshold),
               "Both 'u_threshold' and 'm_threshold' must be between 0 and 1, and 'u_threshold' must be smaller than 'm_threshold'.",
               info = "fails to throw an error with 'u_threshold' smaller than 0")
  
  u_threshold <- 0.1
  m_threshold <- 1.1
  expect_error(categorize_islandGlbSt(meanMeth_islands[[1]], u_threshold = u_threshold, m_threshold = m_threshold),
               "Both 'u_threshold' and 'm_threshold' must be between 0 and 1, and 'u_threshold' must be smaller than 'm_threshold'.",
               info = "fails to throw an error with 'm_threshold' larger than 1")
  
  u_threshold <- 0.5
  m_threshold <- 0.4
  expect_error(categorize_islandGlbSt(meanMeth_islands[[1]], u_threshold = u_threshold, m_threshold = m_threshold),
               "Both 'u_threshold' and 'm_threshold' must be between 0 and 1, and 'u_threshold' must be smaller than 'm_threshold'.",
               info = "fails to throw an error with 'm_threshold' larger than 'm_threshold'")
  
})

test_that("categorize_islandGlbSt categorizes correctly for different thresholds", {
  data <- list(
    #Tip 1
    list(c(rep(1,5), rep(0,5)), # mean 0.5
         c(rep(0,9), 1), # mean 0.1
         c(rep(1,8), rep(0.5,2)))) # mean 0.9
  index_islands <- c(1,3)
  meanMeth_islands <- get_meanMeth_islands(index_islands = index_islands, data = data)
  expect_equal(categorize_islandGlbSt(meanMeth_islands[[1]], u_threshold = 0.2, m_threshold = 0.9),
               c("p", "m"))
  expect_equal(categorize_islandGlbSt(meanMeth_islands[[1]], u_threshold = 0.2, m_threshold = 0.91),
               c("p", "p"))
  index_islands <- c(2)
  meanMeth_islands <- get_meanMeth_islands(index_islands = index_islands, data = data)
  expect_equal(categorize_islandGlbSt(meanMeth_islands[[1]], u_threshold = 0.2, m_threshold = 0.9),
               "u")
  expect_equal(categorize_islandGlbSt(meanMeth_islands[[1]], u_threshold = 0.09, m_threshold = 0.91),
               "p")
})

test_that("compute_fitch input control", {
  
  # Non-valid tree, less than 2 tips
  tree <- "(1:1);"
  meth <- matrix(c("m"))
  rownames(meth) <- "1"
  expect_error(compute_fitch(tree, meth),
               "The input 'tree' must have a minimum of 2 tips.")
  
  # Non-valid Newick format
  tree <- "1:1"
  expect_error(compute_fitch(tree, meth),
               info = "fails to throw error with non-valid Newick format")
  
  # Non-valid tree, duplicated tree labels
  tree <- "((a:1,a:1):2,(b:1.25,c:1.25):0.5);"
  meth <- matrix(rep("m",4), nrow = 4)
  rownames(meth) <- c("a", "a", "b", "c")
  expect_error(compute_fitch(tree, meth),
               "The input tree has duplicated tree labels: a")
  
  # meth matrix rownames not identical to tree tip labels
  tree <- "((a:1,d:1):2,(b:1.25,c:1.25):0.5);"
  meth <- matrix(rep("m",4), nrow = 4)
  rownames(meth) <- c("a", "e", "b", "c")
  expect_error(compute_fitch(tree, meth),
               "Input 'meth' matrix rownames must match tree tip labels exactly, in the same order as tips from left to right in the Newick tree.")
  
  # Check that with input control FALSE no initial error is given
  # (only uninformative error)
  expect_error(compute_fitch(tree, meth, input_control = FALSE),
               "subscript out of bounds")
})

test_that("computeFitch_islandGlbSt input control",{
  
  # Non-valid tree, less than 2 tips
  tree <- "(1:1);"
  data <- list(list(rep(1,5)))
  index_islands <- c(1)
  expect_error(computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 0.1, m_threshold = 0.9),
               "The input 'tree' must have a minimum of 2 tips.")
  
  # Non-valid tree, duplicated tree labels
  tree <- "(a:1,a:1);"
  data <- list(
    list(rep(1,5)),
    list(rep(1,5))
  )
  expect_error(computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 0.1, m_threshold = 0.9),
               "The input tree has duplicated tree labels: a")
  
  # Non-valid island index
  tree <- "(a:1,b:1);"
  index_islands <- c(2)
  expect_error(computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 0.1, m_threshold = 0.9),
               "Invalid island indices detected: 2. Number of structures in given data: 1")
  
  # Incorrect u_threshold
  index_islands <- c(1)
  expect_error(computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 5, m_threshold = 0.9),
               "Both 'u_threshold' and 'm_threshold' must be between 0 and 1, and 'u_threshold' must be smaller than 'm_threshold'.")
  
  # Incorrect m_threshold
  expect_error(computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 0.1, m_threshold = 0),
               "Both 'u_threshold' and 'm_threshold' must be between 0 and 1, and 'u_threshold' must be smaller than 'm_threshold'.")
})

test_that("computeFitch_islandGlbSt testing output",{
  
  # Set tree and data
  tree <- "((1:1,2:1):2,(3:1.25,4:1.25):0.5);"
  data <- list(
    #Tip 1
    list(c(rep(1,5), rep(0,5)), # mean 0.5
         c(rep(0,9), 1), # mean 0.1
         c(rep(1,8), rep(0.5,2))), # mean 0.9
    #Tip 2
    list(c(rep(1,9), rep(0.5,1)), # mean 0.95
         c(rep(0.5,9), 1), # mean 0.55
         c(rep(1,8), rep(0.5,2))), # mean 0.9
    #Tip 3
    list(c(rep(1,9), rep(0.5,1)), # mean 0.95
         c(rep(0.5,9), 1), # mean 0.55
         c(rep(1,8), rep(0.5,2))), # mean 0.9
    #Tip 4
    list(c(rep(1,9), rep(0.5,1)), # mean 0.95
         c(rep(1,9), 0), # mean 0.9
         c(rep(1,8), rep(0.5,2)))) # mean 0.9
  
  index_islands <- c(1,3)
  o <- computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 0.1, m_threshold = 0.9, testing = TRUE)
  expect_equal(rownames(o$upmdata), c("1", "2", "3", "4"))
  expect_equal(o$upmdata_list[[1]], c("p", "m"))
  expect_equal(o$upmdata[1,], c("p", "m"))
  expect_equal(o$upmdata_list[[2]], c("m", "m"))
  expect_equal(o$upmdata[2,], c("m", "m"))
  expect_equal(o$upmdata_list[[3]], c("m", "m"))
  expect_equal(o$upmdata[3,], c("m", "m"))
  expect_equal(o$upmdata_list[[4]], c("m", "m"))
  expect_equal(o$upmdata[4,], c("m", "m"))
  
  # Test with non-numeric labels
  tree <- "((a:1,b:1):2,(c:1.25,d:1.25):0.5);"
  o <- computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 0.1, m_threshold = 0.9, testing = TRUE)
  expect_equal(rownames(o$upmdata), c("a", "b", "c", "d"))
  expect_equal(o$upmdata_list[[1]], c("p", "m"))
  expect_equal(o$upmdata[1,], c("p", "m"))
  expect_equal(o$upmdata_list[[2]], c("m", "m"))
  expect_equal(o$upmdata[2,], c("m", "m"))
  expect_equal(o$upmdata_list[[3]], c("m", "m"))
  expect_equal(o$upmdata[3,], c("m", "m"))
  expect_equal(o$upmdata_list[[4]], c("m", "m"))
  expect_equal(o$upmdata[4,], c("m", "m"))
})

test_that("compute_fitch 8 tips numeric labels 2 structures", {
  
  # Set tree and data
  tree <- "(((1:1,2:1):1,(3:1,4:1):1):1,((5:1,6:1):1,(7:1,8:1):1):1);"
  data <- list(
    #Tip 1
    list(c(rep(1,5), rep(0,5)), # p
         c(rep(0,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 2
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(0.5,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 3
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(0.5,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 4
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(1,9), 0), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 5
    list(c(rep(1,5), rep(0,5)), # p
         c(rep(0,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 6
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(0.5,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 7
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(0.5,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 8
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(1,9), 0), 
         c(rep(0,9), rep(0.5,1)))) # u
  
  index_islands <- c(1,3)
  u_threshold <- 0.1
  m_threshold <- 0.9
  
  tree <- validate_tree(tree)
  meanMeth_islands <- get_meanMeth_islands(index_islands, data)
  upmdata_list <- lapply(meanMeth_islands, categorize_islandGlbSt, u_threshold, m_threshold)
  upmdata  <- matrix(unlist(upmdata_list), nrow=length(tree$tip.label), byrow=TRUE)
  rownames(upmdata)<-tree$tip.label
  o <- compute_fitch(ape::write.tree(tree), upmdata)
  expect_equal(o$optStateSet[[1]], "m")
  expect_equal(o$optStateSet[[2]], "m")
  expect_equal(o$minChange_number, c(3,1))
})

test_that("computeFitch_islandGlbSt 8 tips numeric labels 2 structures", {
  
  # Set tree and data
  tree <- "(((1:1,2:1):1,(3:1,4:1):1):1,((5:1,6:1):1,(7:1,8:1):1):1);"
  data <- list(
    #Tip 1
    list(c(rep(1,5), rep(0,5)), # p
         c(rep(0,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 2
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(0.5,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 3
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(0.5,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 4
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(1,9), 0), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 5
    list(c(rep(1,5), rep(0,5)), # p
         c(rep(0,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 6
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(0.5,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 7
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(0.5,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 8
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(1,9), 0), 
         c(rep(0,9), rep(0.5,1)))) # u
  
  index_islands <- c(1,3)
  
  o <- computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 0.1, m_threshold = 0.9)
  expect_equal(o, c(3,1))
})


test_that("compute_fitch 8 tips non-numeric labels 2 structures", {
  
  # Set tree and data
  tree <- "(((a:1,b:1):1,(c:1,d:1):1):1,((e:1,f:1):1,(g:1,h:1):1):1);"
  data <- list(
    #Tip 1
    list(c(rep(1,5), rep(0,5)), # p
         c(rep(0,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 2
    list(c(rep(0.5,9), rep(0.5,1)), # p
         c(rep(0.5,9), 1), 
         c(rep(0,8), rep(0.5,2))), # u
    #Tip 3
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(0.5,9), 1), 
         c(rep(0.5,8), rep(0.5,2))), # p
    #Tip 4
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(1,9), 0), 
         c(rep(0.5,8), rep(0.5,2))), # p
    #Tip 5
    list(c(rep(0,5), rep(0,5)), # u
         c(rep(0,9), 1), 
         c(rep(0.5,8), rep(0.5,2))), # p
    #Tip 6
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(0.5,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 7
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(0.5,9), 1), 
         c(rep(0,8), rep(0.5,2))), # u
    #Tip 8
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(1,9), 0), 
         c(rep(0,9), rep(0.5,1)))) # u
  
  index_islands <- c(1,3)
  u_threshold <- 0.1
  m_threshold <- 0.9
  
  tree <- validate_tree(tree)
  meanMeth_islands <- get_meanMeth_islands(index_islands, data)
  upmdata_list <- lapply(meanMeth_islands, categorize_islandGlbSt, u_threshold, m_threshold)
  upmdata  <- matrix(unlist(upmdata_list), nrow=length(tree$tip.label), byrow=TRUE)
  rownames(upmdata)<-tree$tip.label
  o <- compute_fitch(ape::write.tree(tree), upmdata)
  expect_true(all(c("u", "p", "m") %in% o$optStateSet[[1]]))
  expect_true(all(c("u", "p", "m") %in% o$optStateSet[[2]]))
  expect_equal(o$minChange_number, c(2,4))
})

test_that("computeFitch_islandGlbSt 8 tips non-numeric labels 2 structures", {

  # Set tree and data
  tree <- "(((a:1,b:1):1,(c:1,d:1):1):1,((e:1,f:1):1,(g:1,h:1):1):1);"
  data <- list(
    #Tip 1
    list(c(rep(1,5), rep(0,5)), # p
         c(rep(0,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 2
    list(c(rep(0.5,9), rep(0.5,1)), # p
         c(rep(0.5,9), 1), 
         c(rep(0,8), rep(0.5,2))), # u
    #Tip 3
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(0.5,9), 1), 
         c(rep(0.5,8), rep(0.5,2))), # p
    #Tip 4
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(1,9), 0), 
         c(rep(0.5,8), rep(0.5,2))), # p
    #Tip 5
    list(c(rep(0,5), rep(0,5)), # u
         c(rep(0,9), 1), 
         c(rep(0.5,8), rep(0.5,2))), # p
    #Tip 6
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(0.5,9), 1), 
         c(rep(1,8), rep(0.5,2))), # m
    #Tip 7
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(0.5,9), 1), 
         c(rep(0,8), rep(0.5,2))), # u
    #Tip 8
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(1,9), 0), 
         c(rep(0,9), rep(0.5,1)))) # u
  
  index_islands <- c(1,3)
  
  o <- computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 0.1, m_threshold = 0.9)
  expect_equal(o, c(2,4))
})


test_that("compute_fitch 4 tips numeric labels 2 structures", {
  
  # Set tree and data
  tree <- "((1:1,2:1):2,(3:1.25,4:1.25):0.5);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1)), # m
         c(rep(0,9), 1), 
         c(rep(0,9), rep(0.5,1))), # u
    #Tip 2
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1))), # m
    #Tip 3
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(0.5,9), 1), 
         c(rep(0,9), rep(0.5,1))), # u
    #Tip 4
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1)))) # m
  
  index_islands <- c(1,3)
  u_threshold <- 0.1
  m_threshold <- 0.9
  
  tree <- validate_tree(tree)
  meanMeth_islands <- get_meanMeth_islands(index_islands, data)
  upmdata_list <- lapply(meanMeth_islands, categorize_islandGlbSt, u_threshold, m_threshold)
  upmdata  <- matrix(unlist(upmdata_list), nrow=length(tree$tip.label), byrow=TRUE)
  rownames(upmdata)<-tree$tip.label
  o <- compute_fitch(ape::write.tree(tree), upmdata)
  expect_true(all(c("u", "m") %in% o$optStateSet[[1]]))
  expect_true(all(c("u", "m") %in% o$optStateSet[[2]]))
  expect_equal(o$minChange_number, c(2,2))
})

test_that("computeFitch_islandGlbSt 4 tips numeric labels 2 structures", {
  
  # Set tree and data
  tree <- "((1:1,2:1):2,(3:1.25,4:1.25):0.5);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1)), # m
         c(rep(0,9), 1), 
         c(rep(0,9), rep(0.5,1))), # u
    #Tip 2
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1))), # m
    #Tip 3
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(0.5,9), 1), 
         c(rep(0,9), rep(0.5,1))), # u
    #Tip 4
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1)))) # m
  
  index_islands <- c(1,3)
  
  o <- computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 0.1, m_threshold = 0.9)
  expect_equal(o, c(2,2))
})


test_that("compute_fitch 4 tips non-numeric labels 2 structures", {
  
  # Set tree and data
  tree <- "((d:1,e:1):2,(a:1.25,b:1.25):0.5);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1)), # m
         c(rep(0,9), 1), 
         c(rep(0,9), rep(0.5,1))), # u
    #Tip 2
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1))), # m
    #Tip 3
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(0.5,9), 1), 
         c(rep(0,9), rep(0.5,1))), # u
    #Tip 4
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1)))) # m
  
  index_islands <- c(1,3)
  u_threshold <- 0.1
  m_threshold <- 0.9
  
  tree <- validate_tree(tree)
  meanMeth_islands <- get_meanMeth_islands(index_islands, data)
  upmdata_list <- lapply(meanMeth_islands, categorize_islandGlbSt, u_threshold, m_threshold)
  upmdata  <- matrix(unlist(upmdata_list), nrow=length(tree$tip.label), byrow=TRUE)
  rownames(upmdata)<-tree$tip.label
  o <- compute_fitch(ape::write.tree(tree), upmdata)
  expect_true(all(c("u", "m") %in% o$optStateSet[[1]]))
  expect_true(all(c("u", "m") %in% o$optStateSet[[2]]))
  expect_equal(o$minChange_number, c(2,2))
})

test_that("computeFitch_islandGlbSt 4 tips non-numeric labels 2 structures", {
  
  # Set tree and data
  tree <- "((d:1,e:1):2,(a:1.25,b:1.25):0.5);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1)), # m
         c(rep(0,9), 1), 
         c(rep(0,9), rep(0.5,1))), # u
    #Tip 2
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1))), # m
    #Tip 3
    list(c(rep(1,9), rep(0.5,1)), # m
         c(rep(0.5,9), 1), 
         c(rep(0,9), rep(0.5,1))), # u
    #Tip 4
    list(c(rep(0,9), rep(0.5,1)), # u
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1)))) # m
  
  index_islands <- c(1,3)
  
  o <- computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 0.1, m_threshold = 0.9)
  expect_equal(o, c(2,2))
})


test_that("compute_fitch 3 tips unordered numeric labels 1 structure", {
  
  # Set tree and data
  tree <- "((3:1,2:1):2,5:2);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1))), # m
    #Tip 2
    list(c(rep(0,9), rep(0.5,1))), # u
    #Tip 3
    list(c(rep(0.5,9), rep(0.5,1)))) # p
  
  index_islands <- c(1)
  u_threshold <- 0.1
  m_threshold <- 0.9
  
  tree <- validate_tree(tree)
  meanMeth_islands <- get_meanMeth_islands(index_islands, data)
  upmdata_list <- lapply(meanMeth_islands, categorize_islandGlbSt, u_threshold, m_threshold)
  upmdata  <- matrix(unlist(upmdata_list), nrow=length(tree$tip.label), byrow=TRUE)
  rownames(upmdata)<-tree$tip.label
  o <- compute_fitch(ape::write.tree(tree), upmdata)
  expect_true(all(c("u", "p", "m") %in% o$optStateSet[[1]]))
  expect_equal(o$minChange_number, 2)
})

test_that("computeFitch_islandGlbSt 3 tips unordered numeric labels 1 structure", {
  
  # Set tree and data
  tree <- "((3:1,2:1):2,5:2);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1))), # m
    #Tip 2
    list(c(rep(0,9), rep(0.5,1))), # u
    #Tip 3
    list(c(rep(0.5,9), rep(0.5,1)))) # p
  
  index_islands <- c(1)
  
  o <- computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 0.1, m_threshold = 0.9)
  expect_equal(o, 2)
})


test_that("compute_fitch 3 tips non-numeric labels 1 structure", {
  
  # Set tree and data
  tree <- "((bla:1,bah:1):2,booh:2);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1))), # m
    #Tip 2
    list(c(rep(0.5,9), rep(0.5,1))), # p
    #Tip 3
    list(c(rep(0.5,9), rep(0.5,1)))) # p
  
  index_islands <- c(1)
  u_threshold <- 0.1
  m_threshold <- 0.9
  
  tree <- validate_tree(tree)
  meanMeth_islands <- get_meanMeth_islands(index_islands, data)
  upmdata_list <- lapply(meanMeth_islands, categorize_islandGlbSt, u_threshold, m_threshold)
  upmdata  <- matrix(unlist(upmdata_list), nrow=length(tree$tip.label), byrow=TRUE)
  rownames(upmdata)<-tree$tip.label
  o <- compute_fitch(ape::write.tree(tree), upmdata)
  expect_equal(o$optStateSet[[1]], "p")
  expect_equal(o$minChange_number, 1)
})

test_that("computeFitch_islandGlbSt 3 tips non-numeric labels 1 structure", {
  
  # Set tree and data
  tree <- "((bla:1,bah:1):2,booh:2);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1))), # m
    #Tip 2
    list(c(rep(0.5,9), rep(0.5,1))), # p
    #Tip 3
    list(c(rep(0.5,9), rep(0.5,1)))) # p
  
  index_islands <- c(1)
  
  o <- computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 0.1, m_threshold = 0.9)
  expect_equal(o, 1)
})



test_that("compute_fitch 4 tips asymmetric tree numeric labels 1 structure", {
  
  # Set tree and data
  tree <- "(((1:1,2:1):2,3:2):1,4:4);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1))), # m
    #Tip 2
    list(c(rep(0,9), rep(0.5,1))), # u
    #Tip 3
    list(c(rep(0.5,9), rep(0.5,1))), # p
    #Tip 4
    list(c(rep(0.5,9), rep(0.5,1)))) # p
  
  index_islands <- c(1)
  u_threshold <- 0.1
  m_threshold <- 0.9
  
  tree <- validate_tree(tree)
  meanMeth_islands <- get_meanMeth_islands(index_islands, data)
  upmdata_list <- lapply(meanMeth_islands, categorize_islandGlbSt, u_threshold, m_threshold)
  upmdata  <- matrix(unlist(upmdata_list), nrow=length(tree$tip.label), byrow=TRUE)
  rownames(upmdata)<-tree$tip.label
  o <- compute_fitch(ape::write.tree(tree), upmdata)
  expect_equal(o$optStateSet[[1]], "p")
  expect_equal(o$minChange_number, 2)
})

test_that("computeFitch_islandGlbSt 4 tips asymmetric tree labels 1 structure", {
  
  # Set tree and data
  tree <- "(((1:1,2:1):2,3:2):1,4:4);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1))), # m
    #Tip 2
    list(c(rep(0,9), rep(0.5,1))), # u
    #Tip 3
    list(c(rep(0.5,9), rep(0.5,1))), # p
    #Tip 4
    list(c(rep(0.5,9), rep(0.5,1)))) # p
  
  index_islands <- c(1)
  
  o <- computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 0.1, m_threshold = 0.9)
  expect_equal(o, 2)
})


test_that("compute_fitch 4 tips asymmetric tree non-numeric labels 1 structure", {
  
  # Set tree and data
  tree <- "(((d5:1,epsilon:1):2,f501267:2):1,iota:4);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1))), # m
    #Tip 2
    list(c(rep(0,9), rep(0.5,1))), # u
    #Tip 3
    list(c(rep(0.5,9), rep(0.5,1))), # p
    #Tip 4
    list(c(rep(0.5,9), rep(0.5,1)))) # p
  
  index_islands <- c(1)
  u_threshold <- 0.1
  m_threshold <- 0.9
  
  tree <- validate_tree(tree)
  meanMeth_islands <- get_meanMeth_islands(index_islands, data)
  upmdata_list <- lapply(meanMeth_islands, categorize_islandGlbSt, u_threshold, m_threshold)
  upmdata  <- matrix(unlist(upmdata_list), nrow=length(tree$tip.label), byrow=TRUE)
  rownames(upmdata)<-tree$tip.label
  o <- compute_fitch(ape::write.tree(tree), upmdata)
  expect_equal(o$optStateSet[[1]], "p")
  expect_equal(o$minChange_number, 2)
})

test_that("computeFitch_islandGlbSt 4 tips asymmetric tree non-numeric labels 1 structure", {
  
  # Set tree and data
  tree <- "(((d5:1,epsilon:1):2,f501267:2):1,iota:4);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1))), # m
    #Tip 2
    list(c(rep(0,9), rep(0.5,1))), # u
    #Tip 3
    list(c(rep(0.5,9), rep(0.5,1))), # p
    #Tip 4
    list(c(rep(0.5,9), rep(0.5,1)))) # p
  
  index_islands <- c(1)
  
  o <- computeFitch_islandGlbSt(index_islands, data, tree, u_threshold = 0.1, m_threshold = 0.9)
  expect_equal(o, 2)
})

test_that("count_upm", {
  data <- c(0,0,0,1,1,0.5,0.5,0.5,0.5,0.5)
  o <- count_upm(data)
  expect_equal(o, c(3,5,2))
})

test_that("compare_CherryFreqs returns value of 1 when upm counts are equal", {
  tip1 <- rep(0,15)
  tip2 <- rep(0,15)
  expect_equal(compare_CherryFreqs(tip1, tip2), 1)
})

test_that("compare_CherryFreqs handling of 0 marginals", {
  tip1 <- c(rep(0,6), rep(0.5,2))
  tip2 <- rep(0,8)
  o <- compare_CherryFreqs(tip1, tip2, testing = TRUE)
  expect_equal(ncol(o$contingency_table), 2)
  expect_false(is.na(o$chi_sq_result$p.value))
})

test_that("pValue_CherryFreqsChange_i input control",{
  wrong_tree <- "(a:1);"
  data <- list(c(rep(0,5), rep(0.5,5)),
               rep(1,10),
               c(rep(0,10)))
  index_islands <- c(1,3)
  expect_error(pValue_CherryFreqsChange_i(data, categorized_data = T, index_islands, tree = wrong_tree),
               "The input 'tree' must have a minimum of 2 tips.")
  
  tree <- "(a:1,b:1);"
  data <- list(
    list(c(rep(0,5), rep(0.5,5)),
         rep(1,10),
         c(rep(0,10))),
    list(c(rep(0,5), rep(0.5,5)),
         rep(1,10),
         c(rep(0,5), rep(0.5,5))))
  
  wrong_index_islands <- c(1,4)
  expect_error(pValue_CherryFreqsChange_i(data, categorized_data = T, wrong_index_islands, tree),
               "Invalid island indices detected: 4. Number of structures in given data: 3")
})

test_that("pValue_CherryFreqsChange_i non-categorized input of methylation states", {
  
  tree <- "(a:1,b:1);"
  data <- list(
    list(c(0.1, 0.2, 0.02), c(0.05, 0.15, 0.15)), # tip 1 (u,u,u)(u,u,u)
    list(c(0.01, 0.05, 0.1), c(0.8, 0.9, 0.98)) # tip 2   (u,u,u)(m,m,m)
  )
  index_islands <- c(1,2)
  
  o <- pValue_CherryFreqsChange_i(data = data, index_islands = index_islands, tree = tree)
  
  expect_equal(o[1, "island_1"], 1,
              info = "p value for identical categorical states not correct with non-categorized input data")
  expect_true(o[1, "island_2"] >= 0 && o[1, "island_2"] < 1,
              info = "p value not different categorical states not correct with non-categorized input data")
  
})

test_that("pValue_CherryFreqsChange_i", {
  # Set tree and data
  tree <- "((d:1,e:1):2,a:2);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1)), 
         c(rep(0,9), 1), 
         c(rep(0,9), rep(0.5,1))), 
    #Tip 2
    list(c(rep(0,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1))), 
    #Tip 3
    list(c(rep(1,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(0,9), rep(0.5,1)))) 

  
  index_islands <- c(1,3)
  
  o <- pValue_CherryFreqsChange_i(data, categorized_data = T, index_islands, tree)
  expect_true(o[1, "island_1"] >= 0 && o[1, "island_1"] <= 1,
              info = "p value not between 1 and 0 for island_1")
  expect_true(o[1, "island_3"] >= 0 && o[1, "island_3"] <= 1,
              info = "p value not between 1 and 0 for island_3")
  
  # Set tree and data
  tree <- "((d:1,e:1):2,(a:2,b:2):1);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1)), 
         c(rep(0,9), 1), 
         c(rep(0,9), rep(0.5,1))), 
    #Tip 2
    list(c(rep(0,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1))),
    #Tip 3
    list(c(rep(0,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1))),
    #Tip 4
    list(c(rep(1,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(0,8), rep(0.5,2)))) 
  o <- pValue_CherryFreqsChange_i(data, categorized_data = T, index_islands, tree)
  expect_equal(nrow(o), 2,
               info = "fails to return a dataframe with 2 rows when tree has 2 cherries")
})

test_that("mean_CherryFreqsChange_i input control",{
  wrong_tree <- "(a:1);"
  data <- list(c(rep(0,5), rep(0.5,5)),
               rep(1,10),
               c(rep(0,10)))
  index_islands <- c(1,3)
  expect_error(mean_CherryFreqsChange_i(data, categorized_data = T, index_islands, tree = wrong_tree, pValue_threshold = 0.05),
               "The input 'tree' must have a minimum of 2 tips.")
  
  tree <- "(a:1,b:1);"
  data <- list(
    list(c(rep(0,5), rep(0.5,5)),
         rep(1,10),
         c(rep(0,10))),
    list(c(rep(0,5), rep(0.5,5)),
         rep(1,10),
         c(rep(0,5), rep(0.5,5))))
  
  wrong_index_islands <- c(1,4)
  expect_error(mean_CherryFreqsChange_i(data, categorized_data = T, wrong_index_islands, tree, pValue_threshold = 0.05),
               "Invalid island indices detected: 4. Number of structures in given data: 3")
})

test_that("mean_CherryFreqsChange_i non-categorized input of methylation states", {
  
  tree <- "(a:1,b:1);"
  data <- list(
    list(c(0.1, 0.2, 0.02, 0.2), c(0.05, 0.15, 0.15, 0.2)), # tip 1 (u,u,u,u)(u,u,u,u)
    list(c(0.01, 0.05, 0.1, 0.2), c(0.8, 0.9, 0.98, 0.8)) # tip 2   (u,u,u,u)(m,m,m,m)
  )
  index_islands <- c(1,2)
  
  o <- mean_CherryFreqsChange_i(data, categorized_data = F, index_islands, tree, pValue_threshold = 0.05)
  
  expect_equal(o$FreqsChange, 0.5,
               info = "not correct mean number of changes per island with non-categorized input data")
})

test_that("mean_CherryFreqsChange_i correct output", {
  # Set tree and data
  tree <- "((d:1,e:1):2,a:2);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1)), 
         c(rep(0,9), 1), 
         c(rep(0,9), rep(0.5,1))), 
    #Tip 2
    list(c(rep(0,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1))), 
    #Tip 3
    list(c(rep(1,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(0,9), rep(0.5,1)))) 
  
  
  index_islands <- c(1,3)
  
  o <- mean_CherryFreqsChange_i(data, categorized_data = T, index_islands, tree, pValue_threshold = 0.05)
  expect_true(o[1, "FreqsChange"] == 1,
              info = "does not return the correct mean of observed changes per island")

  ## Test for 2 cherries
  # Set tree and data
  tree <- "((d:1,e:1):2,(a:2,b:2):1);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1)), 
         c(rep(0,9), 1), 
         c(rep(0,9), rep(0,1))), 
    #Tip 2
    list(c(rep(0,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(0,9), rep(0,1))),
    #Tip 3
    list(c(rep(0,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1))),
    #Tip 4
    list(c(rep(0,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(1,8), rep(0.5,2)))) 
  o <- mean_CherryFreqsChange_i(data, categorized_data = T, index_islands, tree, pValue_threshold = 0.05)
  expect_equal(nrow(o), 2,
               info = "fails to return a dataframe with 2 rows when tree has 2 cherries")
  expect_equal(o$FreqsChange, c(0.5, 0),
               info = "does not return the correct mean of observed changes per island")
})


test_that("validate_dataAcrossTips detects incorrect input structures", {
  
  # Not a list
  expect_error(validate_dataAcrossTips(42), 
               "Error: 'data' must be a list structured as data\\[\\[tip\\]\\]\\[\\[structure\\]\\].")
  
  # Less than 2 tips
  expect_error(validate_dataAcrossTips(list(list(c(0, 1, 0)))), 
               "Error: 'data' must contain at least two tips.*Found 1.*")
  
  # A tip is not a list
  expect_error(validate_dataAcrossTips(list(list(c(0, 1, 0)), c(0, 1, 0))),
               "Error: Each element of 'data' \\(data\\[\\[tip\\]\\]\\) must be a list of structures.*")
  
  # Different number of structures across tips
  expect_error(validate_dataAcrossTips(list(list(c(0, 1, 0)), list(c(0, 1, 0), c(1, 1, 1)))),
               "Error: All tips in 'data' must have the same number of structures.*")
  
  expect_error(validate_dataAcrossTips(list(list(numeric(0)), list(c(0, 1, 0)))),
               "Error: Each structure in 'data' must contain non-empty numeric vectors.*zero-length data.*")
  
  # Different structure lengths
  expect_error(validate_dataAcrossTips(list(list(c(0, 1, 0)), list(c(0, 1)))),
               "Error: All structures in 'data' must have the same number of sites across tips.*")
  
  # Valid case
  expect_silent(validate_dataAcrossTips(list(
    list(c(0, 1, 0), c(1, 1, 0)),
    list(c(0, 0, 1), c(1, 0, 1)),
    list(c(1, 0, 0), c(0, 1, 1))
  )))
})

test_that("categorize_siteMethSt works with already categorized data", {
  data <- list(
    list(c(0, 0.5, 1), c(0.5, 1, 0)),
    list(c(0, 0, 1), c(1, 0.5, 0))
  )
  
  # No changes expected as all values are already in the categories
  transformed_data <- categorize_siteMethSt(data)
  
  expect_equal(transformed_data, data)
})


test_that("categorize_siteMethSt transforms data based on default thresholds", {
  data <- list(
    list(c(0.1, 0.7, 0.9), c(0.3, 0.5, 0.9)),
    list(c(0.2, 0.8, 0.6), c(0.9, 0.4, 0.7))
  )
  
  # Transform the data with default thresholds
  transformed_data <- categorize_siteMethSt(data)
  
  expected_data <- list(
    list(c(0, 0.5, 1), c(0.5, 0.5, 1)),
    list(c(0, 1, 0.5), c(1, 0.5, 0.5))
  )
  
  expect_equal(transformed_data, expected_data)
})

test_that("categorize_siteMethSt transforms data based on custom thresholds", {
  data <- list(
    list(c(0.1, 0.7, 0.9), c(0.3, 0.5, 0.9)),
    list(c(0.2, 0.8, 0.6), c(0.9, 0.4, 0.7))
  )
  
  # Transform the data with custom thresholds
  transformed_data <- categorize_siteMethSt(data, u_threshold = 0.15, m_threshold = 0.85)
  
  expected_data <- list(
    list(c(0, 0.5, 1), c(0.5, 0.5, 1)),
    list(c(0.5, 0.5, 0.5), c(1, 0.5, 0.5))
  )
  
  expect_equal(transformed_data, expected_data)
})

test_that("categorize_siteMethSt stops with invalid values", {
  # Test with data that contains a value smaller than 0
  data_invalid_low <- list(
    list(c(0.1, -0.1, 0.5), c(0.4, 0.3, 0.6)),
    list(c(0.7, 0.8, 0.2), c(0.9, 0.6, 0.7))
  )
  
  expect_error(categorize_siteMethSt(data_invalid_low),
               "All values in data must be between 0 and 1 as they represent methylation frequencies.")
  
  # Test with data that contains a value larger than 1
  data_invalid_high <- list(
    list(c(0.1, 1.1, 0.5), c(0.4, 0.3, 0.6)),
    list(c(0.7, 0.8, 0.2), c(0.9, 0.6, 0.7))
  )
  
  expect_error(categorize_siteMethSt(data_invalid_high),
               "All values in data must be between 0 and 1.")
})


test_that("mean_TreeFreqsChange_i input control",{
  wrong_tree <- "(a:1);"
  data <- list(c(rep(0,5), rep(0.5,5)),
               rep(1,10),
               c(rep(0,10)))
  index_islands <- c(1,3)
  expect_error(mean_TreeFreqsChange_i(tree = wrong_tree, data, categorized_data = T, index_islands, pValue_threshold = 0.05),
               "The input 'tree' must have a minimum of 2 tips.")
  
  tree <- "(a:1,b:1);"
  data <- list(
    list(c(rep(0,5), rep(0.5,5)),
         rep(1,10),
         c(rep(0,10))),
    list(c(rep(0,5), rep(0.5,5)),
         rep(1,10),
         c(rep(0,5), rep(0.5,5))))
  wrong_index_islands <- c(1,4)
  expect_error(mean_TreeFreqsChange_i(tree, data, categorized_data = T, wrong_index_islands, pValue_threshold = 0.05),
               "Invalid island indices detected: 4. Number of structures in given data: 3")
  
  tree <- "(a:1,b:1);"
  data <- list(
    list(c(rep(0,5), rep(0.5,5)),
         rep(1,10),
         c(rep(0,10))),
    list(c(rep(0,5), rep(0.5,5)),
         rep(1,10),
         c(rep(0,5), rep(0.5,5))))
  index_islands <- c(1,3)
  expect_error(mean_TreeFreqsChange_i(tree, data, categorized_data = T, index_islands, pValue_threshold = 0),
               "pValue_threshold needs to be between 0 and 1.")
})

test_that("mean_TreeFreqsChange_i non-categorized input of methylation states", {
  
  tree <- "(a:1,b:1);"
  data <- list(
    list(c(0.1, 0.2, 0.02, 0.2), c(0.05, 0.15, 0.15, 0.2)), # tip 1 (u,u,u,u)(u,u,u,u)
    list(c(0.01, 0.05, 0.1, 0.2), c(0.8, 0.9, 0.98, 0.8)) # tip 2   (u,u,u,u)(m,m,m,m)
  )
  index_islands <- c(1,2)
  
  o <- mean_TreeFreqsChange_i(tree, data, categorized_data = F, index_islands, pValue_threshold = 0.05)
  
  expect_equal(o, 0.5,
               info = "not correct mean number of changes per island with non-categorized input data")
})

test_that("mean_TreeFreqsChange_i testing output returns pvalue of 1 when upm counts are equal", {
  tree <-"((a:1,b:1):1,c:2);"
  data <- list(
    list(rep(0,11)),
    list(rep(0,11)),
    list(rep(0,11))
  )
  index_islands <- c(1)
  pValue_threshold <- 0.05
  o <- mean_TreeFreqsChange_i(tree, data, categorized_data = TRUE,
                               index_islands,
                               pValue_threshold, testing = TRUE)
  expect_equal(o$pValues[1], 1)
  expect_true(is.na(o$island_upmCounts_list[[1]]))
})

test_that("mean_TreeFreqsChange_i handling of 0 marginals", {
  tree <-"((a:1,b:1):1,c:2);"
  data <- list(
    list(rep(0,11)),
    list(rep(0,11)),
    list(c(rep(0,2), rep(0.5, 9)))
  )
  index_islands <- c(1)
  pValue_threshold <- 0.05
  
  o <- mean_TreeFreqsChange_i(tree, data, categorized_data = TRUE,
                               index_islands, 
                               pValue_threshold, testing = TRUE)
  expect_equal(ncol(o$island_upmCounts_list[[1]]), 2)
  expect_false(is.na(o$pValues[1]))
})

test_that("mean_CherryFreqsChange_i correct output tree 2 tips",{
  tree <- "(a:1,b:1);"
  data <- list(
    list(c(rep(0,5), rep(0.5,5)),
         rep(1,10),
         c(rep(0,10))),
    list(c(rep(0,5), rep(0.5,5)),
         rep(1,10),
         c(rep(0,5), rep(0.5,5))))
  index_islands <- c(1,3)
  pValue_threshold <- 0.05
  o <- mean_TreeFreqsChange_i(tree, data, categorized_data = TRUE,
                               index_islands, 
                               pValue_threshold)
  expect_equal(o, 0.5)
})

test_that("mean_CherryFreqsChange_i correct output tree 3 tips",{
  tree <- "((d:1,e:1):2,a:2);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1)), 
         c(rep(0,9), 1), 
         c(rep(0,9), rep(0.5,1))), 
    #Tip 2
    list(c(rep(1,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1))), 
    #Tip 3
    list(c(rep(1,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(0,9), rep(0.5,1)))) 
  index_islands <- c(1,3)
  pValue_threshold <- 0.05
  o <- mean_TreeFreqsChange_i(tree, data, categorized_data = TRUE,
                               index_islands, 
                               pValue_threshold)
  
  expect_equal(o, 0.5)
})

test_that("mean_CherryFreqsChange_i correct output tree 4 tips",{
  tree <- "((d:1,e:1):2,(a:2,b:2):1);"
  data <- list(
    #Tip 1
    list(c(rep(1,9), rep(0,1)), 
         c(rep(0,9), 1), 
         c(rep(0,9), rep(0,1))), 
    #Tip 2
    list(c(rep(0,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(0,9), rep(0,1))),
    #Tip 3
    list(c(rep(0,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(1,9), rep(0,1))),
    #Tip 4
    list(c(rep(0,9), rep(0.5,1)), 
         c(rep(0.5,9), 1), 
         c(rep(1,8), rep(0.5,2)))) 
  index_islands <- c(1,3)
  pValue_threshold <- 0.05
  o <- mean_TreeFreqsChange_i(tree, data, categorized_data = TRUE,
                               index_islands, 
                               pValue_threshold)
  
  expect_equal(o, 1)
})







