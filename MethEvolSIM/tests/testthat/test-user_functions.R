# Function to access private variables and functions
get_private <- function(x) {
  x[['.__enclos_env__']]$private
}

test_that("get_parameterValues()",{
  # Test error when rootData is not instance of combiStructureGenerator
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState= c("M", "U", "M"))
  output <- get_parameterValues()
  output$alpha_pI <- 0.5
  # Set root data to be the list output of simulate_initialData
  m <- capture.output(wrongRootData <- simulate_initialData(infoStr = infoStr, params = output), type = "message")
  expect_error(get_parameterValues(rootData = wrongRootData), info = "fails to throw error when argument is not combiStructureInstance")
  # Test return default parameter values
  output <- get_parameterValues()
  expect_true(is.data.frame(output), info = "fails to output dataframe")
  # Test return rootData parameter values
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState= c("M", "U", "M"))
  output$alpha_pI <- 0.5
  m <- capture.output(rootData <- simulate_initialData(infoStr = infoStr, params = output)$data, type = "message")
  output <- get_parameterValues(rootData = rootData)
  expect_equal(output$alpha_pI, 0.5, info ="fails to return rootData parameters when given")
})

test_that("simulate_initialData",{
  # Test errors for incorrect input

  ## a) Incomplete infoStr argument
  infoStr <- data.frame(n = c(100, 100, 100))
  expect_error(simulate_initialData(infoStr = infoStr), info ="fails to throw error 'infoStr should be a dataframe with columns: 'n', 'globalState''")

   ## b) infoStr with eqFreqs: incorrect values
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"),
                        u_eqFreq = c(0.1, 0.8, 0.1),
                        p_eqFreq = c(NA, 0.1, 0.1),
                        m_eqFreq = c(0.8, 0.1, 0.8))
  expect_error(simulate_initialData(infoStr = infoStr), info = "fails to throw error when given eqFreqs in infoStr have missing values")
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"),
                        u_eqFreq = c(0.1, 0.8, 0.1),
                        p_eqFreq = c(0, 0.1, 0.1),
                        m_eqFreq = c(0.8, 0.1, 0.8))
  expect_error(simulate_initialData(infoStr = infoStr), info = "fails to throw error when given eqFreqs in infoStr are incorrect")

  ## c) incorrect customized params
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"))
  custom_params <- matrix(1:11, ncol = 11)
  colnames(custom_params) <- c("alpha_pI", "beta_pI", "alpha_mI", "beta_mI", "alpha_pNI", "beta_pNI", "alpha_mNI", "beta_mNI", "mu", "alpha_Ri", "iota")
  expect_error(simulate_initialData(infoStr = infoStr, params = custom_params), info ="fails to throw error when given params is not data frame")
  custom_params <- get_parameterValues()
  custom_params <- custom_params[,-1]
  expect_error(simulate_initialData(infoStr = infoStr, params = custom_params), info ="fails to throw error when given params dataframe is not complete")

  # Test output is correctly generated for correct input
  ## a) For input without customized eqFreqs or params
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState= c("M", "U", "M"))
  m <- capture.output(output <- simulate_initialData(infoStr = infoStr), type = "message")
  expect_equal(m, "Simulating initial data with default parameter values")
  expect_equal(class(output$data)[1], "combiStructureGenerator", info ="does not generate correct output$data class for correct input")
  expect_true(all(output$params == get_parameterValues()), info ="does not return default parameter values when not given a)")
  ## b) For input with customized eqFreqs
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"),
                        u_eqFreq = c(0.1, 0.8, 0.1),
                        p_eqFreq = c(0.1, 0.1, 0.1),
                        m_eqFreq = c(0.8, 0.1, 0.8))
  m <- capture.output(output <- simulate_initialData(infoStr = infoStr), type = "message")
  expect_equal(m, "Simulating initial data with default parameter values")
  expect_equal(class(output$data)[1], "combiStructureGenerator", info ="does not generate correct output$data class for correct input")
  expect_true(all(output$params == get_parameterValues()), info ="does not return default parameter values when not given b)")
  ## c) For input with customized params
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"),
                        u_eqFreq = c(0.1, 0.8, 0.1),
                        p_eqFreq = c(0.1, 0.1, 0.1),
                        m_eqFreq = c(0.8, 0.1, 0.8))
  custom_params <- get_parameterValues()
  custom_params$iota <- 0.5
  m <- capture.output(output <- simulate_initialData(infoStr = infoStr, params = custom_params), type = "message")
  expect_equal(m, "Simulating initial data with customized parameter values")
  expect_equal(class(output$data)[1], "combiStructureGenerator", info ="does not generate correct output$data class for correct input")
  expect_true(all(output$params == custom_params), info ="does not return customized params when given")
  ## d) For CFTP = TRUE
  infoStr <- data.frame(n = c(30, 10, 5),
                        globalState= c("M", "U", "M"))
  message <- capture.output(o <- simulate_initialData(infoStr = infoStr, CFTP = TRUE), type = "message")
  expect_equal(message[2], "Calling CFTP algorithm.")
  # Reset combi counter to check ID of output 
  # First without CFTP
  c <- combiStructureGenerator$new(infoStr = infoStr); c$reset_sharedCounter()
  m <- capture.output(o <- simulate_initialData(infoStr = infoStr), type = "message")
  expect_equal(o$data$get_id(), 1,
               info = "wrong ID of generated data without CFTP")
  # Now with CFTP
  c <- combiStructureGenerator$new(infoStr = infoStr); c$reset_sharedCounter()
  m <- capture.output(o <- simulate_initialData(infoStr = infoStr, CFTP = TRUE), type = "message")
  expect_true(o$data$get_id() == 1,
               info = "wrong ID of generated data with CFTP")
})

test_that("simulate_evolData input control",{
  # Test errors for incorrect input
  ## a) Missing tree or infoStr
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState= c("M", "U", "M"))
  tree <- "(a:1, c:2, (d:3.7, e:4):5);"
  expect_error(capture.output(simulate_evolData(infoStr = infoStr), type = "message"), 
               info='fails to throw error when tree argument is missing')
  expect_error(capture.output(simulate_evolData(tree = tree), type = "message"), 
               info='fails to throw error when neither infoStr nor rootData are given')

  ## b) infoStr with eqFreqs: incorrect values
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"),
                        u_eqFreq = c(0.1, 0.8, 0.1),
                        p_eqFreq = c(NA, 0.1, 0.1),
                        m_eqFreq = c(0.8, 0.1, 0.8))
  expect_error(capture.output(simulate_evolData(infoStr = infoStr, tree = tree), type = "message"), 
               info = "fails to throw error when given eqFreqs in infoStr have missing values")
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"),
                        u_eqFreq = c(0.1, 0.8, 0.1),
                        p_eqFreq = c(0, 0.1, 0.1),
                        m_eqFreq = c(0.8, 0.1, 0.8))
  expect_error(capture.output(simulate_evolData(infoStr = infoStr, tree = tree), type ="message"), 
               info = "fails to throw error when given eqFreqs in infoStr are incorrect")

  ## c) incorrect customized params
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"))
  custom_params <- matrix(1:11, ncol = 11)
  colnames(custom_params) <- c("alpha_pI", "beta_pI", "alpha_mI", "beta_mI", "alpha_pNI", "beta_pNI", "alpha_mNI", "beta_mNI", "mu", "alpha_Ri", "iota")
  expect_error(simulate_evolData(infoStr = infoStr, tree = tree, params = custom_params), info ="fails to throw error when given params is not data frame")
  custom_params <- get_parameterValues()
  custom_params <- custom_params[,-1]
  expect_error(simulate_evolData(infoStr = infoStr, tree = tree, params = custom_params), info ="fails to throw error when given params dataframe is not complete")

  ## d) params given when rootData is given (if rootData is given, its parameter values are used)
  params <- get_parameterValues()
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState= c("M", "U", "M"))
  m <- capture.output(rootData <- simulate_initialData(infoStr = infoStr)$data, type = "message")
  expect_error(simulate_evolData(rootData = rootData, tree = tree, params = params), info = "fails to throw error when rootData is given and params not NULL")

})

test_that("simulate_evolData output",{
  # Test output is correctly generated for correct input
  ## a.1) For input without customized eqFreqs or params: input infoStr
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState= c("M", "U", "M"))
  tree <- "(a:1, c:2, (d:3.7, e:4):5);"
  silence <- capture.output(output <- simulate_evolData(infoStr = infoStr, tree = tree), type = "message")
  expect_equal(class(output$data), "list", info ="does not generate correct output$data list (a1)")
  print <- sub("\\[\\d+\\] \"(.+):.*\"$", "\\1", silence[2])
  expect_print <- "Simulating data at root and letting it evolve along given tree:  (a:1, c:2, (d:3.7, e:4):5);"
  expect_equal(print, expect_print, info = "Not simulating initial data when infoStr is given")
  expect_true(all(output$params == get_parameterValues()), info = "fails to return default params when not given")
  expect_equal(output$tree, tree, info ="returns different tree from given one (a1)")

  ## a.2) For input without customized eqFreqs or params: input rootData
  m <- capture.output(rootData <- simulate_initialData(infoStr = infoStr)$data, type = "message")
  silence <- capture.output(output <- simulate_evolData(rootData = rootData, tree = tree), type = "message")
  expect_equal(class(output$data), "list", info ="does not generate correct output$data list (a2)")
  print <- sub("\\[\\d+\\] \"(.+):.*\"$", "\\1", silence[1])
  expect_print <- "Simulating evolution of given data at root along given tree:  (a:1, c:2, (d:3.7, e:4):5);"
  expect_equal(print, expect_print, info = "Not initiating evolution of given data when rootData is given")
  expect_true(all(output$params == get_parameterValues()), info = "fails to return default params when not given")
  expect_equal(output$tree, tree, info ="returns different tree from given one (a2)")

  ## b) For input with customized eqFreqs
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"),
                        u_eqFreq = c(0.1, 0.8, 0.1),
                        p_eqFreq = c(0.1, 0.1, 0.1),
                        m_eqFreq = c(0.8, 0.1, 0.8))
  silence <- capture.output(output <- simulate_evolData(infoStr = infoStr, tree = tree), type = "message")
  expect_equal(class(output$data), "list", info ="does not generate correct output$data list (b)")
  print <- sub("\\[\\d+\\] \"(.+):.*\"$", "\\1", silence[2])
  expect_print <- "Simulating data at root and letting it evolve along given tree:  (a:1, c:2, (d:3.7, e:4):5);"
  expect_equal(print, expect_print, info = "Not simulating initial data when infoStr is given (b)")
  expect_true(all(output$params == get_parameterValues()), info ="does not return default parameter values when not given b)")
  expect_equal(output$tree, tree, info ="returns different tree from given one (b)")

  ## c) For input with customized params: input infoStr and params
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"),
                        u_eqFreq = c(0.1, 0.8, 0.1),
                        p_eqFreq = c(0.1, 0.1, 0.1),
                        m_eqFreq = c(0.8, 0.1, 0.8))
  custom_params <- get_parameterValues()
  custom_params$iota <- 0.5
  silence <- capture.output(output <- simulate_evolData(infoStr = infoStr, tree = tree, params = custom_params), type = "message")
  expect_equal(class(output$data), "list", info ="does not generate correct output$data list (c)")
  print <- sub("\\[\\d+\\] \"(.+):.*\"$", "\\1", silence[1])
  expect_print <- "Simulating data at root and letting it evolve along given tree:  (a:1, c:2, (d:3.7, e:4):5);"
  expect_equal(print, expect_print, info = "Not simulating initial data when infoStr is given c)")
  expect_true(all(output$params == custom_params), info ="does not return customized params when given")
  expect_equal(output$tree, tree, info ="returns different tree from given one (c)")

  ## d) For input with customized params: input rootData
  custom_params <- get_parameterValues()
  custom_params$alpha_mNI <- 0.5
  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState= c("M", "U", "M"))
  m <- capture.output(rootData <- simulate_initialData(infoStr = infoStr, params = custom_params)$data, type = "message")
  silence <- capture.output(output <- simulate_evolData(rootData = rootData, tree = tree), type = "message")
  expect_equal(class(output$data), "list", info ="does not generate correct output$data list (d)")
  print <- sub("\\[\\d+\\] \"(.*)\"", "\\1", silence[1])
  expect_print <- "Simulating evolution of given data at root along given tree:  (a:1, c:2, (d:3.7, e:4):5);"
  expect_equal(print, expect_print, info = "Fails to inform that parameter values are set as in given rootData d)")
  expect_equal(output$params$alpha_mNI, 0.5, info = "does not return params as in given rootData")
  
  ## e) For CFTP
  infoStr <- data.frame(n = c(10, 10, 20),
                        globalState= c("M", "U", "M"))
  silence <- capture.output(output <- simulate_evolData(infoStr = infoStr, tree = tree, CFTP = TRUE), type = "message")
  expect_equal(silence[3], "Calling CFTP algorithm for data at root before letting it evolve along given tree.")
})

test_that("simulate_evolData output$data",{

  infoStr <- data.frame(n = c(100, 100, 100),
                        globalState= c("M", "U", "M"))
  tree <- "(a:1, c:2, (d:3.7, e:4):5);"
  replicate_number <- 3

  # Case only_tip = TRUE
  silence <- capture.output(output <- simulate_evolData(infoStr = infoStr, tree = tree, n_rep = replicate_number, only_tip = TRUE), type = "message")
  expect_equal(length(output$data), replicate_number, info = "$data output length not equal to number of replicates")
  expect_equal(length(output$data[[1]]), 4, info = "replicate output lenght different from number of tree tips when only_tip = TRUE")
  expect_equal(names(output$data[[1]][[1]]), c("name", "seq"), info = "tip information does not include 'name' and 'seq'")
  expect_equal(output$data[[1]][[1]]$name, "a", info = "returns incorrect tip name")
  expect_equal(output$data[[1]][[2]]$name, "c", info = "returns incorrect tip name")
  expect_equal(output$data[[1]][[3]]$name, "d", info = "returns incorrect tip name")
  expect_equal(output$data[[1]][[4]]$name, "e", info = "returns incorrect tip name")
  expect_equal(length(output$data[[1]][[1]]$seq), dim(infoStr)[1], info = "returns incorrect number of $seq vectors")
  expect_equal(class(output$data[[1]][[1]]$seq[[1]]), "numeric", info = "fails to return numeric vector with methylation encoding per structure")

  # Case only_tip = FALSE
  silence <- capture.output(output <- simulate_evolData(infoStr = infoStr, tree = tree, n_rep = 3, only_tip = FALSE), type = "message")
  expect_true("sim_data" %in% names(output$data), info = "$data output does not contain simulated data")
  expect_true("branchInTree" %in% names(output$data), info = "$data output does not contain info on relationship between tree branches")
  expect_equal(length(output$data$sim_data), replicate_number, info = "$data output length not equal to number of replicates")
  expect_equal(length(output$data$sim_data[[1]]), 6, info = "replicate output lenght different from number of tree branches when only_tip = FALSE")
  expect_equal(names(output$data$sim_data[[1]][[1]]), c("name", "IWE", "seq", "eqFreqs"), info = " incomplete branch information")
  expect_null(output$data$sim_data[[1]][[1]]$name, info = "returns incorrect branch name")
  expect_equal(output$data$sim_data[[1]][[2]]$name, "a", info = "returns incorrect branch name")
  expect_equal(output$data$sim_data[[1]][[3]]$name, "c", info = "returns incorrect branch name")
  expect_null(output$data$sim_data[[1]][[4]]$name, info = "returns incorrect branch name")
  expect_equal(output$data$sim_data[[1]][[5]]$name, "d", info = "returns incorrect branch name")
  expect_equal(output$data$sim_data[[1]][[6]]$name, "e", info = "returns incorrect branch name")
  expect_equal(length(output$data$sim_data[[1]][[1]]$seq), dim(infoStr)[1], info = "returns incorrect number of $seq vectors")
  expect_equal(class(output$data$sim_data[[1]][[1]]$seq[[1]]), "numeric", info = "fails to return numeric vector with methylation encoding per structure")
  expect_null(output$data$sim_data[[1]][[1]]$IWE, info = "returns not NULL $IWE information for tree root")
  for (i in 2:length(output$data$sim_data[[1]])){
    IWE <- output$data$sim_data[[1]][[i]]$IWE
    if (is.logical(IWE)) {
      # If IWE is a logical value (FALSE), assert that it is FALSE
      expect_false(IWE, info = paste("IWE for i =", i, "is not FALSE"))
    } else {
      # If IWE is a list, assert that it contains names "island" and "times"
      expect_true("islands" %in% names(IWE), info = paste("island missing for i =", i))
      expect_true("times" %in% names(IWE), info = paste("times missing for i =", i))
    }
  }
  expect_equal(length(output$data$sim_data[[1]][[1]]$eqFreqs), dim(infoStr)[1], info = "returns incorrect number of $eqFreqs vectors")
  expect_equal(class(output$data$sim_data[[1]][[1]]$eqFreqs[[1]]), "numeric", info = "fails to return numeric vector with methylation equilibrium frequencies")
  expect_null(output$data$branchInTree[[1]]$parent_index, info = "tree root does not have NULL parent_index")
  expect_equal(output$data$branchInTree[[1]]$offspring_index, c(2,3,4))
  expect_equal(output$data$branchInTree[[2]]$parent_index, 1)
  expect_equal(output$data$branchInTree[[3]]$parent_index, 1)
  expect_equal(output$data$branchInTree[[4]]$parent_index, 1)
  expect_null(output$data$branchInTree[[2]]$offspring_index)
  expect_null(output$data$branchInTree[[3]]$offspring_index)
  expect_equal(output$data$branchInTree[[4]]$offspring_index, c(5,6))
  expect_equal(output$data$branchInTree[[5]]$parent_index, 4)
  expect_equal(output$data$branchInTree[[6]]$parent_index, 4)
  expect_null(output$data$branchInTree[[5]]$offspring_index)
  expect_null(output$data$branchInTree[[6]]$offspring_index)
})

test_that("simulate_evolData CFTP_step_limit", {
  
  tree <- "(a:1,b:1);"
  infoStr <- data.frame(n = c(5000, 5000, 100),
                        globalState = c("M", "U", "M"))
  # Given a number of steps smaller than the number of sites, 
  # there is no possiblity of convergence in a single iteration
  # Set a step limit that is applied before the second iteration
  message <- capture.output(
    evolData <- simulate_evolData(infoStr,
                                  tree = tree,
                                  CFTP = TRUE,
                                  CFTP_step_limit = 9000),
    type="message")
  expect_equal(message[4],
               "Steps limit reached. Applying approximation for CFTP.")
})

test_that("simulate_initialData CFTP_step_limit", {
  
  tree <- "(a:1,b:1);"
  infoStr <- data.frame(n = c(5000, 5000, 100),
                        globalState = c("M", "U", "M"))
  # Given a number of steps smaller than the number of sites, 
  # there is no possiblity of convergence in a single iteration
  # Set a step limit that is applied before the second iteration
  message <- capture.output(
    initialData <- simulate_initialData(infoStr,
                                     CFTP = TRUE,
                                     CFTP_step_limit = 9000),
    type="message")
  expect_equal(message[3],
               "Steps limit reached. Applying approximation for CFTP.")
})





