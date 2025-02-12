# =====================================================================================
# Example I: play random numbers.
# =====================================================================================
rm(list = ls()); gc()
subsetSize = 200L
supersetSize = 1000L
superset = 10000 * sort(rnorm(supersetSize) ^ 3 + 2 * runif(supersetSize) ^ 2 +
                          3 * rgamma(supersetSize, 5, 1) + 4)
subsetSum = runif(1, sum(superset[1L : subsetSize]), sum(superset[(supersetSize -
                                                                     subsetSize + 1L) : supersetSize]))
subsetSumError = 1e-3


# Mine 3 subsets
rst1 = FLSSS::FLSSS(len = subsetSize, v = superset, target = subsetSum,
                    ME = subsetSumError, solutionNeed = 3, tlimit = 4)


# Mine 3 subsets via solving the conjugate problem
rst2 = FLSSS::FLSSS(len = subsetSize, v = superset, target = subsetSum,
                    ME = subsetSumError, solutionNeed = 3, tlimit = 4,
                    viaConjugate = TRUE)


# Verify uniqueness
cat("rst1 number of solutions =",
    length(unique(lapply(rst1, function(x) sort(x)))), "\n")
cat("rst2 number of solutions =",
    length(unique(lapply(rst2, function(x) sort(x)))), "\n")


# Verify solutions
if(length(rst1) > 0)
  all(unlist(lapply(rst1, function(x)
    abs(sum(superset[x]) - subsetSum) <= subsetSumError)))
if(length(rst2) > 0)
  all(unlist(lapply(rst2, function(x)
    abs(sum(superset[x]) - subsetSum) <= subsetSumError)))


# Mine 3 subsets in bounded solution space.
# Make up the lower and upper bounds for the solution space:
tmp = sort(sample(1L : supersetSize, subsetSize))
tmp2 = sort(sample(1L : supersetSize, subsetSize))
lowerBounds = pmin(tmp, tmp2)
upperBounds = pmax(tmp, tmp2)
rm(tmp, tmp2)


# 'FLSSS()' does not work if there are elements not under the hood of
# lowerBounds + upperBounds. Exclude those elements:
remainIndex = unique(unlist(apply(cbind(lowerBounds, upperBounds), 1,
                                  function(x) x[1] : x[2])))
lowerBounds = match(lowerBounds, remainIndex)
upperBounds = match(upperBounds, remainIndex)
superset = superset[remainIndex]


# Plant a subset sum:
solution = integer(subsetSize)
solution[1] = sample(lowerBounds[1] : upperBounds[1], 1)
for(i in 2L : subsetSize)
{
  l = max(lowerBounds[i], solution[i - 1] + 1L)
  u = upperBounds[i]
  if(l == u) solution[i] = u
  else solution[i] = sample(l : u, 1)
}
subsetSum = sum(superset[solution])
subsetSumError = abs(subsetSum) * 0.01 # relative error within 1%
rm(solution)


rst3 = FLSSS::FLSSS(len = subsetSize, v = superset, target = subsetSum,
                    ME = subsetSumError, solutionNeed = 2, tlimit = 4,
                    LB = lowerBounds, UB = upperBounds, viaConjugate = TRUE)


print(length(rst3))


# Verify solutions
if(length(rst3) > 0)
  cat(all(unlist(lapply(rst3, function(x)
    abs(sum(superset[x]) - subsetSum) <= subsetSumError))), "\n")




# =====================================================================================
# Example II: mine a real-world dataset.
# =====================================================================================
rm(list = ls()); gc()
superset = c(
  -1119924501, -793412295, -496234747,  -213654767,   16818148,   26267601,   26557292,
  27340260,   28343800,   32036573,    32847411,   34570996,   34574989,   43633028,
  44003100,   47724096,   51905122,    52691025,   53600924,   56874435,   58207678,
  60225777,   60639161,   60888288,    60890325,   61742932,   63780621,   63786876,
  65167464,   66224357,   67198760,    69366452,   71163068,   72338751,   72960793,
  73197629,   76148392,   77779087,    78308432,   81196763,   82741805,   85315243,
  86446883,   87820032,   89819002,    90604146,   93761290,   97920291,   98315039,
  310120088, -441403864, -548143111,  -645883459, -149110919,  305170449, -248934805,
  -1108320430, -527806318, -192539936, -1005074405, -101557770, -156782742, -285384687,
  -418917176,   80346546, -273215446,  -552291568,   86824498,  -95392618, -707778486)
superset = sort(superset)
subsetSum = 139254953
subsetSumError = 0.1


# Find a subset of size 10.
subsetSize = 10L
rst = FLSSS::FLSSS(len = subsetSize, v = superset, target = subsetSum,
                   ME = subsetSumError, solutionNeed = 1, tlimit = 4)
# Verify:
all(unlist(lapply(rst, function(x)
  abs(sum(superset[x]) - subsetSum) <= subsetSumError)))


# Find a subset without size specification.
rst = FLSSS::FLSSS(len = subsetSize, v = superset, target = subsetSum,
                   ME = subsetSumError, solutionNeed = 1, tlimit = 4)
# Verify:
all(unlist(lapply(rst, function(x)
  abs(sum(superset[x]) - subsetSum) <= subsetSumError)))


# Find a subset via looping subset size over 2L : (length(v)).
for(len in 2L : length(superset))
{
  rst = FLSSS::FLSSS(len = subsetSize, v = superset, target = subsetSum,
                     ME = subsetSumError, solutionNeed = 1, tlimit = 4)
  if(length(rst) > 0) break
}
# Verify:
all(unlist(lapply(rst, function(x)
  abs(sum(superset[x]) - subsetSum) <= subsetSumError)))


# Find as many qualified susbets as possible in 2 seconds
rst = FLSSS::FLSSS(len = subsetSize, v = superset, target = subsetSum,
                   ME = subsetSumError, solutionNeed = 999999L, tlimit = 2)
cat("Number of solutions =", length(rst), "\n")


# Verify:
all(unlist(lapply(rst, function(x)
  abs(sum(superset[x]) - subsetSum) <= subsetSumError)))




# =====================================================================================
# Example III: solve a special knapsack problem.
# Given the knapsack's capacity, the number of catagories, the number of items in each
# catagory, select the least number of items to fulfill at least 95% of the knapsack's
# capacity.
# =====================================================================================
rm(list = ls()); gc()
capacity = 361
catagories = LETTERS[1L : 10L] # A, B, ..., J, 10 catagories
catagoryMasses = round(runif(length(catagories)) * 20 + 1)
catagoryItems = sample(1L : 20L, length(catagories))


itemLabel = unlist(mapply(function(x, i) rep(i, x), catagoryItems, catagories))
itemMasses = unlist(mapply(function(x, i) rep(x, i), catagoryMasses, catagoryItems))
vorder = order(itemMasses)
itemLabel = itemLabel[vorder]


superset = itemMasses[vorder]
rate = 0.95
subsetSum = (capacity * rate + capacity) / 2
subsetSumError = capacity - subsetSum
for(subsetSize in 1L : length(itemMasses))
{
  rst = FLSSS::FLSSS(len = subsetSize, v = superset, target = subsetSum,
                     ME = subsetSumError, solutionNeed = 1, tlimit = 4)
  if(length(rst) > 0) break
}


# There may exist no qualified subsets. One can lower 'rate' until a solution
# shows up.
if(length(rst) == 0L)
{
  cat("No solutions. Please lower rate and rerun.\n")
} else
{
  cat("A solution:\n")
  print(table(itemLabel[rst[[1]]]))
}


rm(list = ls()); gc()








rm(list = ls()); gc()
Nsupersets = 30L
supersetSizes = sample(5L : 20L, Nsupersets, replace = TRUE)
subsetSizes = sapply(supersetSizes, function(x) sample(1L : x, 1))


# Create supersets at random:
supersets = lapply(supersetSizes, function(n)
{
  1000 * (rnorm(n) ^ 3 + 2 * runif(n) ^ 2 + 3 * rgamma(n, 5, 1) + 4)
})
str(supersets) # see the structure


# Give a subset sum
solution = mapply(function(n, l) sample(1L : n, l), supersetSizes, subsetSizes)
str(solution) # See structure
subsetsSum = sum(mapply(function(x, s) sum(x[s]), supersets, solution, SIMPLIFY = TRUE))
subsetsSumError = abs(subsetsSum) * 1e-7 # relative error within 0.00001%
rm(solution)


# Mine subsets:
rst = FLSSS::FLSSSmultiset(len = subsetSizes, buckets = supersets, target = subsetsSum,
                           ME = subsetsSumError, solutionNeed = 3, tlimit = 4)
cat("Number of solutions =", length(rst), "\n")


# Verify:
ver = all(unlist(lapply(rst, function(sol)
{
  S = sum(unlist(mapply(function(x, y) sum(x[y]), supersets, sol)))
  abs(S - subsetsSum) <= subsetsSumError
})))
cat("All subsets are qualified:", ver)









# =====================================================================================
# Play random numbers
# =====================================================================================
rm(list = ls()); gc()
agents = 5L
tasks = 12L
costs = t(as.data.frame(lapply(1L : agents, function(x) runif(tasks) * 1000)))
budgets = apply(costs, 1, function(x) runif(1, min(x), sum(x)))
profits = t(as.data.frame(lapply(1L : agents, function(x)
  abs(rnorm(tasks) + runif(1, 0, 4)) * 10000)))


# A dirty function for examining the result's integrity. The function takes in
# the task-agent assignment, the profit or cost matrix M, and calculates the cost
# or profit generated by each agent. 'assignment' is a 2-column data
# frame, first column task, second column agent.
agentCostsOrProfits <- function(assignment, M)
{
  n = ncol(M) * nrow(M)
  M2 = matrix(numeric(n), ncol = tasks)
  for(i in 1L : nrow(assignment))
  {
    x = as.integer(assignment[i, ])
    M2[x[2], x[1]] = M[x[2], x[1]]
  }
  apply(M2, 1, function(x) sum(x))
}


dimnames(costs) = NULL
dimnames(profits) = NULL
names(budgets) = NULL


rst = FLSSS::GAP(maxCore = 2L, agentsCosts = costs, agentsProfits = profits,
                 agentsBudgets = budgets, heuristic = FALSE, tlimit = 2,
                 useBiSrchInFB = FALSE, threadLoad = 8L, verbose = TRUE)
# Function also saves the assignment costs and profits
rst$assignedAgents
rst$assignmentProfit
rst$assignmentCosts


# Examine rst$assignmentCosts
if(sum(rst$assignedAgents) > 0) # all zeros mean the function has not found a solution.
    agentCostsOrProfits(rst$assignedAgents, costs)
# Should equal rst$assignmentCosts and not surpass budgets


  # Examine rst$assignmentProfits
if(sum(rst$assignedAgents) > 0)
    sum(agentCostsOrProfits(rst$assignedAgents, profits))
# Should equal rst$assignmentProfit




# =====================================================================================
# Test case P03 from
# https://people.sc.fsu.edu/~jburkardt/datasets/generalized_assignment/
# =====================================================================================
agents = 3L
tasks = 8L
profits = matrix(c(
  27, 12, 12, 16, 24, 31, 41, 13,
  14,  5, 37,  9, 36, 25,  1, 34,
  34, 34, 20,  9, 19, 19,  3, 34), ncol = tasks)
costs = matrix(c(
  21, 13,  9,  5,  7, 15,  5, 24,
  20,  8, 18, 25,  6,  6,  9,  6,
  16, 16, 18, 24, 11, 11, 16, 18), ncol = tasks)
budgets = c(26, 25, 34)


rst = FLSSS::GAP(maxCore = 2L, agentsCosts = costs, agentsProfits = profits,
                 agentsBudgets = budgets, heuristic = FALSE, tlimit = 2,
                 useBiSrchInFB = FALSE, threadLoad = 8L, verbose = TRUE)
agentCostsOrProfits(rst$assignedAgents, costs)
# Should equal rst$assignmentCosts and not surpass budgets


knownOptSolution = as.integer(c(3, 3, 1, 1, 2, 2, 1, 2))
knownOptSolution = data.frame(task = 1L : tasks, agent = knownOptSolution)


# Total profit from knownOptSolution:
sum(agentCostsOrProfits(knownOptSolution, profits))
# Total profit frim FLSSS::GAP():
rst$assignmentProfit
# FLSSS::GAP() generated a better solution.








# =====================================================================================
# Play random numbers
# =====================================================================================
rm(list = ls()); gc()
agents = 5L
tasks = 12L
costs = t(as.data.frame(lapply(1L : agents, function(x) runif(tasks) * 1000)))
budgets = apply(costs, 1, function(x) runif(1, min(x), sum(x)))
profits = t(as.data.frame(lapply(1L : agents, function(x)
  abs(rnorm(tasks) + runif(1, 0, 4)) * 10000)))


# A dirty function for examining the result's integrity. The function takes in
# the task-agent assignment, the profit or cost matrix M, and calculates the cost
# or profit generated by each agent. 'assignment' is a 2-column data
# frame, first column task, second column agent.
agentCostsOrProfits <- function(assignment, M)
{
  n = ncol(M) * nrow(M)
  M2 = matrix(numeric(n), ncol = tasks)
  for(i in 1L : nrow(assignment))
  {
    x = as.integer(assignment[i, ])
    M2[x[2], x[1]] = M[x[2], x[1]]
  }
  apply(M2, 1, function(x) sum(x))
}


dimnames(costs) = NULL
dimnames(profits) = NULL
names(budgets) = NULL


  rst = FLSSS::GAPintegerized(maxCore = 2L, agentsCosts = costs, agentsProfits = profits,
                              agentsBudgets = budgets, heuristic = FALSE,
                              precisionLevel = rep(tasks * 4L, agents), tlimit = 2,
                              useBiSrchInFB = FALSE, threadLoad = 8L, verbose = TRUE)


  # Function also saves the assignment costs and profits
  rst$assignedAgents
  rst$assignmentProfit
  rst$assignmentCosts


  # Examine rst$assignmentCosts
  if(sum(rst$assignedAgents) > 0) # all zeros mean the function has not found a solution.
    agentCostsOrProfits(rst$assignedAgents, costs)
  # Should equal rst$assignmentCosts and not surpass budgets


  # Examine rst$assignmentProfits
  if(sum(rst$assignedAgents) > 0)
    sum(agentCostsOrProfits(rst$assignedAgents, profits))
  # Should equal rst$assignmentProfit




# =====================================================================================
# Test case P03 from
# https://people.sc.fsu.edu/~jburkardt/datasets/generalized_assignment/
# =====================================================================================
agents = 3L
tasks = 8L
profits = matrix(c(
  27, 12, 12, 16, 24, 31, 41, 13,
  14,  5, 37,  9, 36, 25,  1, 34,
  34, 34, 20,  9, 19, 19,  3, 34), ncol = tasks)
costs = matrix(c(
  21, 13,  9,  5,  7, 15,  5, 24,
  20,  8, 18, 25,  6,  6,  9,  6,
  16, 16, 18, 24, 11, 11, 16, 18), ncol = tasks)
budgets = c(26, 25, 34)


rst = FLSSS::GAPintegerized(maxCore = 2L, agentsCosts = costs, agentsProfits = profits,
                            agentsBudgets = budgets, heuristic = FALSE, tlimit = 2,
                            useBiSrchInFB = FALSE, threadLoad = 8L, verbose = TRUE)
agentCostsOrProfits(rst$assignedAgents, costs)
# Should equal rst$assignmentCosts and not surpass budgets


knownOptSolution = as.integer(c(3, 3, 1, 1, 2, 2, 1, 2))
knownOptSolution = data.frame(task = 1L : tasks, agent = knownOptSolution)


# Total profit from knownOptSolution:
sum(agentCostsOrProfits(knownOptSolution, profits))
# Total profit frim FLSSS::GAP():
rst$assignmentProfit
# FLSSS::GAP() generated a better solution.









rm(list = ls()); gc()
subsetSize = 7L
supersetSize = 60L
dimension = 5L # dimensionality


# Create a supertset at random:
N = supersetSize * dimension
superset = matrix(1000 * (rnorm(N) ^ 3 + 2 * runif(N) ^ 2 +
                            3 * rgamma(N, 5, 1) + 4), ncol = dimension)
rm(N)


# Plant a subset sum:
solution = sample(1L : supersetSize, subsetSize)
subsetSum = colSums(superset[solution, ])
subsetSumError = abs(subsetSum) * 0.01 # relative error within 1%
rm(solution)


# Mine subsets, dimensions fully bounded
rst = FLSSS::mFLSSSpar(maxCore = 2, len = subsetSize, mV = superset,
                       mTarget = subsetSum, mME = subsetSumError,
                       solutionNeed = 2, dl = ncol(superset), du = ncol(superset),
                       tlimit = 2, useBiSrchInFB = FALSE, avgThreadLoad = 8L)


# Verify:
cat("Number of solutions = ", length(rst), "\n")
if(length(rst) > 0)
{
  cat("Solutions unique: ")
  cat(length(unique(lapply(rst, function(x) sort(x)))) == length(rst), "\n")
  cat("Solutions correct: ")
  cat(all(unlist(lapply(rst, function(x)
    abs(colSums(superset[x, ]) - subsetSum) <= subsetSumError))), "\n")
} else
{
  cat("No solutions exist or timer ended too soon.\n")
}




# Mine subsets, the first 3 dimensions lower bounded,
# the last 4 dimension upper bounded
rst = FLSSS::mFLSSSpar(maxCore = 2, len = subsetSize, mV = superset,
                       mTarget = subsetSum, mME = subsetSumError,
                       solutionNeed = 2, dl = 3L, du = 4L,
                       tlimit = 2, useBiSrchInFB = FALSE, avgThreadLoad = 8L)


# Verify:
cat("Number of solutions = ", length(rst), "\n")
if(length(rst) > 0)
{
  cat("Solutions unique: ")
  cat(length(unique(lapply(rst, function(x) sort(x)))) == length(rst), "\n")
  cat("Solutions correct: ")
  cat(all(unlist(lapply(rst, function(x)
  {
    lowerBoundedDim = 1L : 3L
    lowerBounded = all(colSums(superset[x, lowerBoundedDim]) >=
                         subsetSum[lowerBoundedDim] - subsetSumError[lowerBoundedDim])


    upperBoundedDim = (ncol(superset) - 3L) : ncol(superset)
    upperBounded = all(colSums(superset[x, upperBoundedDim]) <=
                         subsetSum[upperBoundedDim] + subsetSumError[upperBoundedDim])


    lowerBounded & upperBounded
  }))), "\n")
} else
{
  cat("No solutions exist or timer ended too soon.\n")
}









rm(list = ls()); gc()
subsetSize = 7L
supersetSize = 60L
dimension = 5L # dimensionality


# Create a supertset at random:
N = supersetSize * dimension
superset = matrix(1000 * (rnorm(N) ^ 3 + 2 * runif(N) ^ 2 +
                              3 * rgamma(N, 5, 1) + 4), ncol = dimension)
rm(N)


# Make up the lower and upper bounds for the solution space:
tmp = sort(sample(1L : supersetSize, subsetSize))
tmp2 = sort(sample(1L : supersetSize, subsetSize))
lowerBounds = pmin(tmp, tmp2)
upperBounds = pmax(tmp, tmp2)
rm(tmp, tmp2)


# Exclude elements not covered by 'lowerBounds' and 'upperBounds':
remainIndex = unique(unlist(apply(cbind(lowerBounds, upperBounds), 1,
                                    function(x) x[1] : x[2])))
lowerBounds = match(lowerBounds, remainIndex)
upperBounds = match(upperBounds, remainIndex)
superset = superset[remainIndex, ]


# Plant a subset sum:
solution = apply(rbind(lowerBounds, upperBounds), 2, function(x)
    sample(x[1] : x[2], 1))
subsetSum = colSums(superset[solution, ])
subsetSumError = abs(subsetSum) * 0.01 # relative error within 1%
rm(solution)


rst = FLSSS::mFLSSSparImposeBounds(
    maxCore = 2L, len = subsetSize, mV = superset, mTarget = subsetSum,
    mME = subsetSumError, LB = lowerBounds, UB = upperBounds,
    solutionNeed = 1, tlimit = 2, dl = ncol(superset), du = ncol(superset),
    targetsOrder = NULL, useBiSrchInFB = FALSE, avgThreadLoad = 8L)


# Verify:
cat("Number of solutions = ", length(rst), "\n")
if(length(rst) > 0)
{
    cat("Solutions unique: ")
    cat(length(unique(lapply(rst, function(x) sort(x)))) == length(rst), "\n")
    cat("Solution in bounded space: ")
    cat(all(unlist(lapply(rst, function(x)
      sort(x) <= upperBounds & sort(x) >= lowerBounds))), "\n")
    cat("Solutions correct: ")
    cat(all(unlist(lapply(rst, function(x)
      abs(colSums(superset[x, ]) - subsetSum) <= subsetSumError))), "\n")
} else
{
    cat("No solutions exist or timer ended too soon.\n")
}








rm(list = ls()); gc()
subsetSize = 7L
supersetSize = 60L
dimension = 5L # dimensionality


# Create a superset at random:
N = supersetSize * dimension
superset = matrix(1000 * (rnorm(N) ^ 3 + 2 * runif(N) ^ 2 +
                              3 * rgamma(N, 5, 1) + 4), ncol = dimension)
rm(N)


# Make up the lower and upper bounds for the solution space:
tmp = sort(sample(1L : supersetSize, subsetSize))
tmp2 = sort(sample(1L : supersetSize, subsetSize))
lowerBounds = pmin(tmp, tmp2)
upperBounds = pmax(tmp, tmp2)
rm(tmp, tmp2)


# 'mFLSSSparImposeBoundsIntegerized()' does not work if there are elements not
# under the hood of 'lowerBounds' + 'upperBounds'. Exclude these elements first:
remainIndex = unique(unlist(
  apply(cbind(lowerBounds, upperBounds), 1, function(x) x[1] : x[2])))
  lowerBounds = match(lowerBounds, remainIndex)
  upperBounds = match(upperBounds, remainIndex)
  superset = superset[remainIndex, ]


# Plant a subset sum:
solution = integer(subsetSize)
solution[1] = sample(lowerBounds[1] : upperBounds[1], 1)
for(i in 2L : subsetSize)
{
    l = max(lowerBounds[i], solution[i - 1] + 1L)
    u = upperBounds[i]
    if(l == u) solution[i] = u
    else solution[i] = sample(l : u, 1)
}
subsetSum = colSums(superset[solution, ])
subsetSumError = abs(subsetSum) * 0.01 # relative error within 1%
rm(solution)


system.time({rst = FLSSS::mFLSSSparImposeBoundsIntegerized(
    maxCore = 2L, len = subsetSize, mV = superset, mTarget = subsetSum,
    mME = subsetSumError, LB = lowerBounds, UB = upperBounds,
    solutionNeed = 1, tlimit = 3, dl = ncol(superset), du = ncol(superset),
    targetsOrder = NULL, useBiSrchInFB = FALSE, avgThreadLoad = 8L)})


# Compare the time cost of 'mFLSSSparImposeBoundsIntegerized()' and
# 'mFLSSSparImposeBounds()'. The speed advantage of 'mFLSSSparIntegerized()'
# may not be pronounced for toy examples.
system.time(FLSSS::mFLSSSparImposeBounds(
    maxCore = 2L, len = subsetSize, mV = superset, mTarget = subsetSum,
    mME = subsetSumError, LB = lowerBounds, UB = upperBounds,
    solutionNeed = 1, tlimit = 2, dl = ncol(superset), du = ncol(superset),
    targetsOrder = NULL, useBiSrchInFB = FALSE, avgThreadLoad = 8L))


# Verify:
cat("Number of solutions = ", length(rst$solution), "\n")
if(length(rst$solution) > 0)
{
  cat("Solutions unique: ")
  cat(length(unique(lapply(rst$solution, function(x)
      sort(x)))) == length(rst$solution), "\n")
  cat("Solution in bounded space: ")
  cat(all(unlist(lapply(rst$solution, function(x)
      sort(x) <= upperBounds & sort(x) >= lowerBounds))), "\n")


  cat("Solutions correct regarding integerized data: ")
  cat(all(unlist(lapply(rst$solution, function(x)
    abs(colSums(rst$INT$mV[x, ]) - rst$INT$mTarget) <= rst$INT$mME))), "\n")


  cat("Solutions correct regarding original data: ")
  boolean = all(unlist(lapply(rst$solution, function(x)
  abs(colSums(superset[x, ]) - subsetSum) <= subsetSumError)))
  cat(boolean, "\n")
  if(!boolean)
  {
      cat("The given error threshold relative to subset sum:\n")
      givenRelaErr = round(abs(subsetSumError / subsetSum), 5)
      cat(givenRelaErr, "\n")


      cat("Solution subset sum relative error:\n")
      tmp = lapply(rst$solution, function(x)
      {
        err = round(abs(colSums(superset[x, ]) / subsetSum -1), 5)
        for(i in 1L : length(err))
        {
          if(givenRelaErr[i] < err[i]) message(paste0(err[i], " "), appendLF = FALSE)
          else cat(err[i], "")
        }
        cat("\n")
      })
      cat("Integerization caused the errors. Future versions of")
      cat("'mFLSSSparIntegerized()' would have a parameter of precision level.\n")
  }
} else
{
    cat("No solutions exist or timer ended too soon.\n")
}








rm(list = ls()); gc()
subsetSize = 7L
supersetSize = 60L
dimension = 5L # dimensionality


# Create a supertset at random:
N = supersetSize * dimension
superset = matrix(1000 * (rnorm(N) ^ 3 + 2 * runif(N) ^ 2 + 3 * rgamma(N, 5, 1) + 4),
                  ncol = dimension)
rm(N)


# Plant a subset sum:
solution = sample(1L : supersetSize, subsetSize)
subsetSum = colSums(superset[solution, ])
subsetSumError = abs(subsetSum) * 0.01 # relative error within 1%
rm(solution)


# Mine subsets, dimensions fully bounded
system.time({rst = FLSSS::mFLSSSparIntegerized(
  maxCore = 2, len = subsetSize, mV = superset, mTarget = subsetSum,
  mME = subsetSumError, solutionNeed = 2, dl = ncol(superset),
  du = ncol(superset), tlimit = 2, useBiSrchInFB = FALSE, avgThreadLoad = 8L)})


# Compare the time cost of 'mFLSSSparIntegerized()' and 'mFLSSSpar()'. The
# speed advantage of 'mFLSSSparIntegerized()' may not be pronounced for toy
# examples.
system.time(FLSSS::mFLSSSpar(
  maxCore = 2, len = subsetSize, mV = superset, mTarget = subsetSum,
  mME = subsetSumError, solutionNeed = 2, dl = ncol(superset),
  du = ncol(superset), tlimit = 2, useBiSrchInFB = FALSE, avgThreadLoad = 8L))


# Verify:
cat("Number of solutions = ", length(rst$solution), "\n")
if(length(rst$solution) > 0)
{
  cat("Solutions unique: ")
  cat(length(unique(lapply(rst$solution, function(x)
    sort(x)))) == length(rst$solution), "\n")


  cat("Solutions correct regarding integerized data: ")
  cat(all(unlist(lapply(rst$solution, function(x)
    abs(colSums(rst$INT$mV[x, ]) - rst$INT$mTarget) <= rst$INT$mME))), "\n")


  cat("Solutions correct regarding original data: ")
  boolean = all(unlist(lapply(rst$solution, function(x)
    abs(colSums(superset[x, ]) - subsetSum) <= subsetSumError)))
  cat(boolean, "\n")
  if(!boolean)
  {
    cat("The given error threshold relative to subset sum:\n")
    givenRelaErr = round(abs(subsetSumError / subsetSum), 5)
    cat(givenRelaErr, "\n")


    cat("Solution subset sum relative error:\n")
    tmp = lapply(rst$solution, function(x)
    {
      err = round(abs(colSums(superset[x, ]) / subsetSum -1), 5)
      for(i in 1L : length(err))
      {
        if(givenRelaErr[i] < err[i]) message(paste0(err[i], " "), appendLF = FALSE)
        else cat(err[i], "")
      }
      cat("\n")
    })
    cat("Integerization caused the errors. Future versions of")
    cat("'mFLSSSparIntegerized()' would have a parameter of precision level.\n")
  }
} else
{
  cat("No solutions exist or time ended too soon.\n")
}


# Mine subsets, the first 3 dimensions lower bounded,
# the last 4 dimension upper bounded
rst = FLSSS::mFLSSSparIntegerized(
  maxCore = 2, len = subsetSize, mV = superset, mTarget = subsetSum,
  mME = subsetSumError, solutionNeed = 2, dl = 3L, du = 4L, tlimit = 2,
  useBiSrchInFB = FALSE, avgThreadLoad = 8L)


# Verify:
cat("Number of solutions = ", length(rst$solution), "\n")
if(length(rst$solution) > 0)
{
  cat("Solutions unique: ")
  cat(length(unique(lapply(rst$solution, function(x)
    sort(x)))) == length(rst$solution), "\n")


  cat("Solutions correct regarding integerized data: ")
  cat(all(unlist(lapply(rst$solution, function(x)
  {
    lowerBoundedDim = 1L : 3L
    lowerBounded = all(colSums(rst$INT$mV[x, lowerBoundedDim]) >=
                         rst$INT$mTarget[lowerBoundedDim] - rst$INT$mME[lowerBoundedDim])


    upperBoundedDim = (ncol(rst$INT$mV) - 3L) : ncol(rst$INT$mV)
    upperBounded = all(colSums(rst$INT$mV[x, upperBoundedDim]) <=
                       rst$INT$mTarget[upperBoundedDim] + rst$INT$mME[upperBoundedDim])


    lowerBounded & upperBounded
  }))), "\n")
} else
{
  cat("No solutions exist or timer ended too soon.\n")
}








# =====================================================================================
# Play random numbers
# =====================================================================================
rm(list = ls()); gc()
subsetSize = 6
supersetSize = 60
NcostsAttr = 4

# \donttest{
  # Make up costs for each item.
  costs = abs(6 * (rnorm(supersetSize * NcostsAttr) ^ 3 +
                     2 * runif(supersetSize * NcostsAttr) ^ 2 +
                     3 * rgamma(supersetSize * NcostsAttr, 5, 1) + 4))
  costs = matrix(costs, ncol = NcostsAttr)


  # Make up cost limits.
  budgets = apply(costs, 2, function(x)
  {
    x = sort(x)
    Min = sum(x[1L : subsetSize])
    Max = sum(x[(supersetSize - subsetSize + 1L) : supersetSize])
    runif(1, Min, Max)
  })


  # Make up item profits.
  gains = rnorm(supersetSize) ^ 2 * 10000 + 100


  rst1 = FLSSS::mmKnapsack(
    maxCore = 2L, len = subsetSize, itemsProfits = gains, itemsCosts = costs,
    capacities = budgets, heuristic = FALSE, tlimit = 2, useBiSrchInFB = FALSE,
    threadLoad = 4L, verbose = TRUE)


  # Let 'x' be the solution given 'heuristic = TRUE'. The sum of ranks of the
  # profits subsetted by 'x' would be no less than that of the optimal solution.
  rst2 = FLSSS::mmKnapsack(
    maxCore = 2L, len = subsetSize, itemsProfits = gains, itemsCosts = costs,
    capacities = budgets, heuristic = TRUE, tlimit = 2, useBiSrchInFB = FALSE,
    threadLoad = 4L, verbose = TRUE)


  # Exam difference in total profits given by the heuristic and the optimal:
  cat(length(rst1)); cat(length(rst2)) # See if solution exists.
  if(length(rst1) > 0 & length(rst2) > 0) sum(gains[rst2]) / sum(gains[rst1])
# }


# =====================================================================================
# Test case P08 from
# https://people.sc.fsu.edu/~jburkardt/datasets/knapsack_01/knapsack_01.html
# =====================================================================================
rm(list = ls()); gc()
costs = matrix(c(382745, 799601, 909247, 729069, 467902,  44328,  34610, 698150,
                 823460, 903959, 853665, 551830, 610856, 670702, 488960, 951111,
                 323046, 446298, 931161,  31385, 496951, 264724, 224916, 169684),
               ncol = 1)


gains = c( 825594, 1677009, 1676628, 1523970,  943972,   97426,  69666, 1296457,
           1679693, 1902996, 1844992, 1049289, 1252836, 1319836, 953277, 2067538,
           675367,  853655, 1826027,   65731,  901489,  577243, 466257,  369261)


budgets = 6404180


# 'mmKnapsack()' is designed for the multidimensional Knapsack and may not
# be ideal for one-dimensional 0-1 Knapsack regarding computing speed.
# 'len = 0' causes substantial deceleration. Looping 'len' over possible
# values is recommended if 'len' is ungiven.
rst1 = FLSSS::mmKnapsack(
  maxCore = 2L, len = 12L, itemsProfits = gains, itemsCosts = costs,
  capacities = budgets, heuristic = FALSE, tlimit = 2, threadLoad = 4L,
  verbose = TRUE)
rst1 = sort(rst1)


cat("Correct solution:\n1 2 4 5 6 10 11 13 16 22 23 24\nFLSSS solution =\n")
cat(rst1, "\n")




# =====================================================================================
# Test case P07 from
# https://people.sc.fsu.edu/~jburkardt/datasets/knapsack_01/knapsack_01.html
# =====================================================================================
costs = matrix(c(70, 73, 77, 80, 82, 87, 90, 94, 98, 106, 110, 113, 115, 118, 120),
               ncol = 1)


gains = c(135, 139, 149, 150, 156, 163, 173, 184, 192, 201, 210, 214, 221, 229, 240)


budgets = 750


rst2 = FLSSS::mmKnapsack(
  maxCore = 2L, len = 8L, itemsProfits = gains, itemsCosts = costs,
  capacities = budgets, heuristic = FALSE, tlimit = 2,
  threadLoad = 4L, verbose = TRUE)
rst2 = sort(rst2)


cat("Correct solution:\n1 3 5 7 8 9 14 15\nFLSSS solution =\n")
cat(rst2, "\n")








# Play random numbers
# =====================================================================================
rm(list = ls()); gc()
subsetSize = 6
supersetSize = 60
NcostsAttr = 4

# \donttest{
  # Make up costs for each item.
  costs = abs(6 * (rnorm(supersetSize * NcostsAttr) ^ 3 +
                     2 * runif(supersetSize * NcostsAttr) ^ 2 +
                     3 * rgamma(supersetSize * NcostsAttr, 5, 1) + 4))
  costs = matrix(costs, ncol = NcostsAttr)


  # Make up cost limits.
  budgets = apply(costs, 2, function(x)
  {
    x = sort(x)
    Min = sum(x[1L : subsetSize])
    Max = sum(x[(supersetSize - subsetSize + 1L) : supersetSize])
    runif(1, Min, Max)
  })


  # Make up item profits.
  gains = rnorm(supersetSize) ^ 2 * 10000 + 100


  rst1 = FLSSS::mmKnapsackIntegerized(
    maxCore = 2L, len = subsetSize, itemsProfits = gains, itemsCosts = costs,
    capacities = budgets, heuristic = FALSE, tlimit = 2, useBiSrchInFB = FALSE,
    threadLoad = 4L, verbose = TRUE)


  # Examine if 'mmKnapsackIntegerized()' gives the same solution as 'mmKnapsack()'.
  rst2 = FLSSS::mmKnapsack(
    maxCore = 2L, len = subsetSize, itemsProfits = gains, itemsCosts = costs,
    capacities = budgets, heuristic = FALSE, tlimit = 2, useBiSrchInFB = FALSE,
    threadLoad = 4L, verbose = TRUE)
  # Differences in solutions are due to real-integer conversion




  # Let 'x' be the solution given 'heuristic = T'. The sum of ranks of the
  # profits subsetted by 'x' would be no less than that of the optimal solution.
  rst3 = FLSSS::mmKnapsackIntegerized(
    maxCore = 2L, len = subsetSize, itemsProfits = gains, itemsCosts = costs,
    capacities = budgets, heuristic = TRUE, tlimit = 2, useBiSrchInFB = FALSE,
    threadLoad = 4L, verbose = TRUE)


  # Exam difference in total profits given by the heuristic and the optimal:
  if(length(rst3$solution) > 0 & length(rst1$solution) > 0)
    sum(gains[rst3$solution]) / sum(gains[rst1$solution])
# }




# =====================================================================================
# Test case P08 from
# https://people.sc.fsu.edu/~jburkardt/datasets/knapsack_01/knapsack_01.html
# =====================================================================================
costs = matrix(c(382745, 799601, 909247, 729069, 467902,  44328,  34610, 698150,
                 823460, 903959, 853665, 551830, 610856, 670702, 488960, 951111,
                 323046, 446298, 931161,  31385, 496951, 264724, 224916, 169684),
               ncol = 1)


gains = c( 825594, 1677009, 1676628, 1523970,  943972,   97426,  69666, 1296457,
           1679693, 1902996, 1844992, 1049289, 1252836, 1319836, 953277, 2067538,
           675367,  853655, 1826027,   65731,  901489,  577243, 466257,  369261)


budgets = 6404180


# 'mmKnapsackIntegerized()' is designed for the multidimensional Knapsack
# and may not be ideal for one-dimensional 0-1 Knapsack regarding computing speed.
# 'len = 0' would cause severe deceleration. Looping 'len' over possible
# values is recommended if 'len' is ungiven.
rst = FLSSS::mmKnapsackIntegerized(
  maxCore = 2L, len = 12L, itemsProfits = gains, itemsCosts = costs,
  capacities = budgets, heuristic = FALSE, tlimit = 2, threadLoad = 4L, verbose = TRUE)
rst = sort(rst$solution)


cat("Correct solution:\n1 2 4 5 6 10 11 13 16 22 23 24\nFLSSS solution =\n")
cat(rst, "\n")
# The difference is due to rounding errors in real-integer conversion. The default
# 'precisionLevel' shifts, scales and rounds 'itemCosts' such that its
# maximal element is no less than 8 times the number of items.


# Increase the precision level
rst = FLSSS::mmKnapsackIntegerized(
  maxCore = 2L, len = 12L, itemsProfits = gains, itemsCosts = costs,
  capacities = budgets, heuristic = FALSE, precisionLevel = rep(500L, 1),
  tlimit = 2, threadLoad = 4L, verbose = TRUE)
# 'precisionLevel = 500' shifts, scales and rounds 'itemCosts' such that its
# maximal element is no less than 500.
rst = sort(rst$solution)
cat("Correct solution:\n1 2 4 5 6 10 11 13 16 22 23 24\nFLSSS solution =\n")
cat(rst, "\n")# Play random numbers
# =====================================================================================
rm(list = ls()); gc()
subsetSize = 6
supersetSize = 60
NcostsAttr = 4

# \donttest{
  # Make up costs for each item.
  costs = abs(6 * (rnorm(supersetSize * NcostsAttr) ^ 3 +
                     2 * runif(supersetSize * NcostsAttr) ^ 2 +
                     3 * rgamma(supersetSize * NcostsAttr, 5, 1) + 4))
  costs = matrix(costs, ncol = NcostsAttr)


  # Make up cost limits.
  budgets = apply(costs, 2, function(x)
  {
    x = sort(x)
    Min = sum(x[1L : subsetSize])
    Max = sum(x[(supersetSize - subsetSize + 1L) : supersetSize])
    runif(1, Min, Max)
  })


  # Make up item profits.
  gains = rnorm(supersetSize) ^ 2 * 10000 + 100


  rst1 = FLSSS::mmKnapsackIntegerized(
    maxCore = 2L, len = subsetSize, itemsProfits = gains, itemsCosts = costs,
    capacities = budgets, heuristic = FALSE, tlimit = 2, useBiSrchInFB = FALSE,
    threadLoad = 4L, verbose = TRUE)


  # Examine if 'mmKnapsackIntegerized()' gives the same solution as 'mmKnapsack()'.
  rst2 = FLSSS::mmKnapsack(
    maxCore = 2L, len = subsetSize, itemsProfits = gains, itemsCosts = costs,
    capacities = budgets, heuristic = FALSE, tlimit = 2, useBiSrchInFB = FALSE,
    threadLoad = 4L, verbose = TRUE)
  # Differences in solutions are due to real-integer conversion




  # Let 'x' be the solution given 'heuristic = T'. The sum of ranks of the
  # profits subsetted by 'x' would be no less than that of the optimal solution.
  rst3 = FLSSS::mmKnapsackIntegerized(
    maxCore = 2L, len = subsetSize, itemsProfits = gains, itemsCosts = costs,
    capacities = budgets, heuristic = TRUE, tlimit = 2, useBiSrchInFB = FALSE,
    threadLoad = 4L, verbose = TRUE)


  # Exam difference in total profits given by the heuristic and the optimal:
  if(length(rst3$solution) > 0 & length(rst1$solution) > 0)
    sum(gains[rst3$solution]) / sum(gains[rst1$solution])
# }




# =====================================================================================
# Test case P08 from
# https://people.sc.fsu.edu/~jburkardt/datasets/knapsack_01/knapsack_01.html
# =====================================================================================
costs = matrix(c(382745, 799601, 909247, 729069, 467902,  44328,  34610, 698150,
                 823460, 903959, 853665, 551830, 610856, 670702, 488960, 951111,
                 323046, 446298, 931161,  31385, 496951, 264724, 224916, 169684),
               ncol = 1)


gains = c( 825594, 1677009, 1676628, 1523970,  943972,   97426,  69666, 1296457,
           1679693, 1902996, 1844992, 1049289, 1252836, 1319836, 953277, 2067538,
           675367,  853655, 1826027,   65731,  901489,  577243, 466257,  369261)


budgets = 6404180


# 'mmKnapsackIntegerized()' is designed for the multidimensional Knapsack
# and may not be ideal for one-dimensional 0-1 Knapsack regarding computing speed.
# 'len = 0' would cause severe deceleration. Looping 'len' over possible
# values is recommended if 'len' is ungiven.
rst = FLSSS::mmKnapsackIntegerized(
  maxCore = 2L, len = 12L, itemsProfits = gains, itemsCosts = costs,
  capacities = budgets, heuristic = FALSE, tlimit = 2, threadLoad = 4L, verbose = TRUE)
rst = sort(rst$solution)


cat("Correct solution:\n1 2 4 5 6 10 11 13 16 22 23 24\nFLSSS solution =\n")
cat(rst, "\n")
# The difference is due to rounding errors in real-integer conversion. The default
# 'precisionLevel' shifts, scales and rounds 'itemCosts' such that its
# maximal element is no less than 8 times the number of items.


# Increase the precision level
rst = FLSSS::mmKnapsackIntegerized(
  maxCore = 2L, len = 12L, itemsProfits = gains, itemsCosts = costs,
  capacities = budgets, heuristic = FALSE, precisionLevel = rep(500L, 1),
  tlimit = 2, threadLoad = 4L, verbose = TRUE)
# 'precisionLevel = 500' shifts, scales and rounds 'itemCosts' such that its
# maximal element is no less than 500.
rst = sort(rst$solution)
cat("Correct solution:\n1 2 4 5 6 10 11 13 16 22 23 24\nFLSSS solution =\n")
cat(rst, "\n")# Play random numbers
# =====================================================================================
rm(list = ls()); gc()
subsetSize = 6
supersetSize = 60
NcostsAttr = 4

# \donttest{
  # Make up costs for each item.
  costs = abs(6 * (rnorm(supersetSize * NcostsAttr) ^ 3 +
                     2 * runif(supersetSize * NcostsAttr) ^ 2 +
                     3 * rgamma(supersetSize * NcostsAttr, 5, 1) + 4))
  costs = matrix(costs, ncol = NcostsAttr)


  # Make up cost limits.
  budgets = apply(costs, 2, function(x)
  {
    x = sort(x)
    Min = sum(x[1L : subsetSize])
    Max = sum(x[(supersetSize - subsetSize + 1L) : supersetSize])
    runif(1, Min, Max)
  })


  # Make up item profits.
  gains = rnorm(supersetSize) ^ 2 * 10000 + 100


  rst1 = FLSSS::mmKnapsackIntegerized(
    maxCore = 2L, len = subsetSize, itemsProfits = gains, itemsCosts = costs,
    capacities = budgets, heuristic = FALSE, tlimit = 2, useBiSrchInFB = FALSE,
    threadLoad = 4L, verbose = TRUE)


  # Examine if 'mmKnapsackIntegerized()' gives the same solution as 'mmKnapsack()'.
  rst2 = FLSSS::mmKnapsack(
    maxCore = 2L, len = subsetSize, itemsProfits = gains, itemsCosts = costs,
    capacities = budgets, heuristic = FALSE, tlimit = 2, useBiSrchInFB = FALSE,
    threadLoad = 4L, verbose = TRUE)
  # Differences in solutions are due to real-integer conversion




  # Let 'x' be the solution given 'heuristic = T'. The sum of ranks of the
  # profits subsetted by 'x' would be no less than that of the optimal solution.
  rst3 = FLSSS::mmKnapsackIntegerized(
    maxCore = 2L, len = subsetSize, itemsProfits = gains, itemsCosts = costs,
    capacities = budgets, heuristic = TRUE, tlimit = 2, useBiSrchInFB = FALSE,
    threadLoad = 4L, verbose = TRUE)


  # Exam difference in total profits given by the heuristic and the optimal:
  if(length(rst3$solution) > 0 & length(rst1$solution) > 0)
    sum(gains[rst3$solution]) / sum(gains[rst1$solution])
# }




# =====================================================================================
# Test case P08 from
# https://people.sc.fsu.edu/~jburkardt/datasets/knapsack_01/knapsack_01.html
# =====================================================================================
costs = matrix(c(382745, 799601, 909247, 729069, 467902,  44328,  34610, 698150,
                 823460, 903959, 853665, 551830, 610856, 670702, 488960, 951111,
                 323046, 446298, 931161,  31385, 496951, 264724, 224916, 169684),
               ncol = 1)


gains = c( 825594, 1677009, 1676628, 1523970,  943972,   97426,  69666, 1296457,
           1679693, 1902996, 1844992, 1049289, 1252836, 1319836, 953277, 2067538,
           675367,  853655, 1826027,   65731,  901489,  577243, 466257,  369261)


budgets = 6404180


# 'mmKnapsackIntegerized()' is designed for the multidimensional Knapsack
# and may not be ideal for one-dimensional 0-1 Knapsack regarding computing speed.
# 'len = 0' would cause severe deceleration. Looping 'len' over possible
# values is recommended if 'len' is ungiven.
rst = FLSSS::mmKnapsackIntegerized(
  maxCore = 2L, len = 12L, itemsProfits = gains, itemsCosts = costs,
  capacities = budgets, heuristic = FALSE, tlimit = 2, threadLoad = 4L, verbose = TRUE)
rst = sort(rst$solution)


cat("Correct solution:\n1 2 4 5 6 10 11 13 16 22 23 24\nFLSSS solution =\n")
cat(rst, "\n")
# The difference is due to rounding errors in real-integer conversion. The default
# 'precisionLevel' shifts, scales and rounds 'itemCosts' such that its
# maximal element is no less than 8 times the number of items.


# Increase the precision level
rst = FLSSS::mmKnapsackIntegerized(
  maxCore = 2L, len = 12L, itemsProfits = gains, itemsCosts = costs,
  capacities = budgets, heuristic = FALSE, precisionLevel = rep(500L, 1),
  tlimit = 2, threadLoad = 4L, verbose = TRUE)
# 'precisionLevel = 500' shifts, scales and rounds 'itemCosts' such that its
# maximal element is no less than 500.
rst = sort(rst$solution)
cat("Correct solution:\n1 2 4 5 6 10 11 13 16 22 23 24\nFLSSS solution =\n")
cat(rst, "\n")
