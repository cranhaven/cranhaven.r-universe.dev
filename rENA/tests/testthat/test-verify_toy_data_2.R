d = data.frame(
  GroupName = "Group_1",
  UserName = c("User1","User2","User3","User1","User2","User3"),
  Condition = "FirstGame",
  Context = "Public",
  Timestamp = c("9/17/13 09:43","9/17/13 09:46","9/17/13 09:49","9/17/13 09:52","9/17/13 09:55","9/17/13 09:58"),
  A = c(3,0,1,0,0,3),
  B = c(0,1,0,0,2,0),
  C = c(0,2,0,4,0,1),
  stringsAsFactors = FALSE
)

my.hoo.TMA = list(
  list(Condition = "<UNIT>", GroupName = "<UNIT>", Context = "Public")
  #list(Condition = "<UNIT>"), #, UserName = "<UNIT>")
)
my.units = unique(d[,c("Condition", "UserName"), drop = FALSE])
codes = c('A','B','C')
weighted_step_func <- function(window_size) {
  return(Vectorize(function(x) {
    scalar = 1;
    if (x == 0) {
      return(1*scalar);
    } else if( x < window_size && x > 0) {
      return(1*scalar);
    } else {
      return(0);
    }
  }, "x"))
}
step_func <- function(window_size) {
  return(Vectorize(function(x) {
    if (x == 0) {
      # return(1);
      return(0.5);
    } else if( x < window_size && x > 0) {
      return(1);
    } else {
      return(0);
    }
  }, "x"))
}

test_window_size = 3
# weighted_decays = list(
#   Private = weighted_step_func(test_window_size),
#   Public = weighted_step_func(test_window_size)
# )
# reg_decays = list(
#   Private = step_func(test_window_size),
#   Public = step_func(test_window_size)
# )
# tma_reg = tma::tma(
#   data = d,
#   units = my.units,
#   codes = codes,
#   hoo.rules = my.hoo.TMA,
#   weight.by = function(x) {return((x))},
#   return.ena.set = TRUE,
#   time.column = NA,
#   decay.function = reg_decays,
#   mode.column = 'Context',
#   time.limit = NA,
#   # mask = NA,
#   meta.data = c('Condition')
# )
# tma_sqrt = tma::tma(
#   data = d,
#   units = my.units,
#   codes = codes,
#   hoo.rules = my.hoo.TMA,
#   weight.by = function(x) {return(sqrt(x))},
#   return.ena.set = TRUE,
#   time.column = NA,
#   decay.function = weighted_decays,
#   mode.column = 'Context',
#   time.limit = NA,
#   # mask = NA,
#   meta.data = c('Condition')
# )

rena_binary = rENA:::ena.accumulate.data.file(d,
                               units.by = c("UserName"),
                               conversations.by = c("Condition"),
                               codes = LETTERS[1:3],
                               window.size.back = test_window_size,
                               weight.by = "binary")
rena_prod = rENA:::ena.accumulate.data.file(d,
                               units.by = c("Condition", "UserName"),
                               conversations.by = c("Condition"),
                               codes = LETTERS[1:3],
                               window.size.back = test_window_size,
                               weight.by = "product")
rena_sqrt = rENA:::ena.accumulate.data.file(d,
                               units.by = c("Condition", "UserName"),
                               conversations.by = c("Condition"),
                               codes = LETTERS[1:3],
                               window.size.back = test_window_size,
                               weight.by = sqrt)

# rena_prod$model$row.connection.counts
# rena_binary$model$row.connection.counts
# rena_binary$connection.counts
# rena_prod$connection.counts
# # tma_reg$connection.counts
# # tma_sqrt$connection.counts
# rena_sqrt$connection.counts


test_that("verify binary", {
  testthat::expect_equal(which(as.matrix(rena_prod$model$row.connection.counts) > 0), which(as.matrix(rena_binary$model$row.connection.counts) == 1))
})

test_that("verify product accumulation", {
  testthat::expect_equal(as.matrix(rena_prod$model$row.connection.counts)[1, ], c(0, 0, 0), check.attributes = FALSE)
  testthat::expect_equal(as.matrix(rena_prod$model$row.connection.counts)[3, ], c(1, 2, 0), check.attributes = FALSE)
  testthat::expect_equal(as.matrix(rena_prod$model$row.connection.counts)[6, ], c(6, 15, 2), check.attributes = FALSE)
})
test_that("verify weighted accumulation", {
  testthat::expect_equal(as.matrix(rena_sqrt$model$row.connection.counts)[1, ], c(0, 0, 0), check.attributes = FALSE)
  testthat::expect_equal(as.matrix(rena_sqrt$model$row.connection.counts)[3, ], c(sqrt(1), sqrt(2), sqrt(0)), check.attributes = FALSE)
  testthat::expect_equal(as.matrix(rena_sqrt$model$row.connection.counts)[6, ], c(sqrt(6), sqrt(15), sqrt(2)), check.attributes = FALSE)
})

# test_that("verify directed", {
#   User1_directed = t(matrix(c(
#     0,0,1,
#     0,0,1,
#     0,0,1
#   ),3,3))
#   colnames(User1_directed) = rownames(User1_directed) = c('A','B','C')
#   testthat::expect_equal(
#     User1_directed, accumulation_window_3$contexts$`FirstGame::User1`$directed.adjacency
#   )
#
#   User2_directed = t(matrix(c(
#     0,1,0,
#     0,0,0.5,
#     0,1.5,0
#   ),3,3))
#   colnames(User2_directed) = rownames(User2_directed) = c('A','B','C')
#   testthat::expect_equal(
#     User2_directed, accumulation_window_3$contexts$`FirstGame::User2`$directed.adjacency
#   )
#
#   User3_directed = t(matrix(c(
#     0,0,0.5,
#     2,0,1,
#     2.5,0,1
#   ),3,3))
#   colnames(User3_directed) = rownames(User3_directed) = c('A','B','C')
#   testthat::expect_equal(
#     User3_directed, accumulation_window_3$contexts$`FirstGame::User3`$directed.adjacency
#   )
# })

# testthat::test_that("verify undirected", {
#   User1_undirected = t(matrix(c(
#     NA,0,1,
#     NA,NA,1,
#     NA,NA,NA
#   ),3,3))
#   colnames(User1_undirected) = rownames(User1_undirected) = c('A','B','C')
#   testthat::expect_equal(
#     User1_undirected[upper.tri(User1_undirected)],
#     accumulation_window_3$model$contexts$`FirstGame::User1`$undirected.adjacancy[upper.tri(User1_undirected)]
#   )
#
#   User2_undirected = t(matrix(c(
#     NA,1,0,
#     NA,NA,2,
#     NA,NA,NA
#   ),3,3))
#   colnames(User2_undirected) = rownames(User2_undirected) = c('A','B','C')
#   testthat::expect_equal(
#     User2_undirected[upper.tri(User2_undirected)],
#     accumulation_window_3$model$contexts$`FirstGame::User2`$undirected.adjacancy[upper.tri(User2_undirected)]
#   )
#
#   User3_undirected = t(matrix(c(
#     NA,2,3,
#     NA,NA,1,
#     NA,NA,NA
#   ),3,3))
#   colnames(User3_undirected) = rownames(User3_undirected) = c('A','B','C')
#   testthat::expect_equal(
#     User3_undirected[upper.tri(User3_undirected)],
#     accumulation_window_3$model$contexts$`FirstGame::User3`$undirected.adjacancy[upper.tri(User3_undirected)]
#   )
# })
#
#
# test_that("verify undirected", {
#   User1_undirected = t(matrix(c(
#     0,0,1,
#     NA,0,.5,
#     NA,NA,.5
#   ),3,3))
#   colnames(User1_undirected) = rownames(User1_undirected) = c('A','B','C')
#   testthat::expect_equal(
#     User1_undirected, accumulation_window_3$contexts$`FirstGame::User1`$undirected.adjacancy
#   )
#
#   User2_undirected = t(matrix(c(
#     0,.5,0,
#     NA,0,2,
#     NA,NA,0
#   ),3,3))
#   colnames(User2_undirected) = rownames(User2_undirected) = c('A','B','C')
#   testthat::expect_equal(
#     User2_undirected, accumulation_window_3$contexts$`FirstGame::User2`$undirected.adjacancy
#   )
#
#   User3_undirected = t(matrix(c(
#     0,2,2.5,
#     NA,0,1,
#     NA,NA,.5
#   ),3,3))
#   colnames(User3_undirected) = rownames(User3_undirected) = c('A','B','C')
#   testthat::expect_equal(
#     User3_undirected, accumulation_window_3$contexts$`FirstGame::User3`$undirected.adjacancy
#   )
# })
