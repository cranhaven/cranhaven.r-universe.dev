library(tinytest)
# test solving
ode <- function(t, y, ydot, parameter) {
  a_db = at(parameter, 1)
  b_db = at(parameter, 2)
  c_db = at(parameter, 3)
  d_db = at(parameter, 4)
  predator_db = at(y,1)
  prey_db = at(y, 2)
  ydot[1] = predator_db*prey_db*c_db - predator_db*d_db
  ydot[2] = prey_db*a_db - prey_db*predator_db*b_db
  return(ydot)
}
path <- system.file("examples", package = "paropt")
states <- read.table(paste(path,"/states_LV.txt", sep = ""), header = TRUE)
parameter <- data.frame(time = 0, a = 1.1, b = 0.4, c = 0.1, d = 0.4)
res <- paropt::solve(ode,
                     parameter,
                     reltol = 1e-06, abstol = c(1e-08, 1e-08),
                     states = states, verbose = TRUE)

error <- res[[1]]
expect_equal( (error < 0.05), TRUE)






# test optimization constant parameter
#  ode <- function(t, y, ydot, parameter) {
#    a_db = at(parameter, 1)
#    b_db = at(parameter, 2)
#    c_db = at(parameter, 3)
#    d_db = at(parameter, 4)
#    predator_db = at(y,1)
#    prey_db = at(y, 2)
#    ydot[1] = predator_db*prey_db*c_db - predator_db*d_db
#    ydot[2] = prey_db*a_db - prey_db*predator_db*b_db
#    return(ydot)
#  }
#  path <- system.file("examples", package = "paropt")
#  states <- read.table(paste(path,"/states_LV.txt", sep = ""), header = TRUE)
#  lb <- data.frame(time = 0, a = 0.8, b = 0.3, c = 0.09, d = 0.09)
#  ub <- data.frame(time = 0, a = 1.3, b = 0.7, c = 0.4, d = 0.7)
#  set.seed(1)
#  res <- paropt::optimize(ode,
#                          lb = lb, ub = ub,
#                          reltol = 1e-06, abstol = c(1e-08, 1e-08),
#                          error = 0.0001,
#                          npop = 40, ngen = 1000,
#                          states = states, number_threads = 1)
# error <- res[[1]]
# expect_equal( (error < 0.05), TRUE)
# par <- res$best_parameter_set
#  diff <- function(par) {
#    s <- sum(par[1, 2:4] - c(1.1, 0.4, 0.1, 0.4))
#    s < 0.5
#  }
#  expect_equal(diff(res[[2]]), TRUE)
#
#
#
#
#
# # test optimization with variable parameter
# ode <- function(t, y, parameter) {
#   a_db = at(parameter, 1)
#   b_db = at(parameter, 2)
#   c_db = at(parameter, 3)
#   d1 <- vector(0, 4)
#   d1[1] = at(parameter, 4)
#   d1[2] = at(parameter, 5)
#   d1[3] = at(parameter, 6)
#   d1[4] = at(parameter, 7)
#   time <- c(0, 20, 60, 80)
#   t_db <- t[1]
#   d_db <- cmr(t_db, time, d1)
#   predator_db = at(y,1)
#   prey_db = at(y, 2)
#   ydot <- vector(0, 2)
#   ydot[1] = predator_db*prey_db*c_db - predator_db*d_db
#   ydot[2] = prey_db*a_db - prey_db*predator_db*b_db
#   return(ydot)
# }
#
# odecpp <- ast2ast::translate(ode)
# odecpp_list_ret <- function(t, y, parameter) {
#   res <- odecpp(t, y, parameter)
#   list(res)
# }
#
# out <- deSolve::ode(y = c(10, 10), states$time,
#                     func = odecpp_list_ret, parms = c(1.1, 0.4, 0.1, 0.4, 0.4, 0.4, 0.4))
# compare_later <- out
#
# ode <- function(t, y, ydot, parameter) {
#   a_db = at(parameter, 1)
#   b_db = at(parameter, 2)
#   c_db = at(parameter, 3)
#   d_db <- at(parameter, 4)
#   predator_db = at(y,1)
#   prey_db = at(y, 2)
#   ydot[1] = predator_db*prey_db*c_db - predator_db*d_db
#   ydot[2] = prey_db*a_db - prey_db*predator_db*b_db
#   return(ydot)
# }
#
# r <- function(a) {
#   c(a, rep(NA, 3))
# }
#
# lb <- data.frame(time = c(0, 20, 60, 80),
#                  a = r(0.8), b = r(0.3), c = r(0.09), d = 0.1)
# ub <- data.frame(time = c(0, 20, 60, 80),
#                  a = r(1.3), b = r(0.7), c = r(0.4), d = 0.6)
# set.seed(1)
# cl <- as.data.frame(compare_later)
# res <- paropt::optimize(ode,
#                         lb = lb, ub = ub,
#                         reltol = 1e-06, abstol = c(1e-08, 1e-08),
#                         error = 0.0001,
#                         npop = 40, ngen = 3000,
#                         states = cl, number_threads = 1)
# par <- res[[2]]
# par <- par[, 2:5]
# par <- do.call(c, par)
# par <- par[!is.na(par)]
# diff <- function(par) {
#   s <- sum(par - c(1.1, 0.4, 0.1, 0.4, 0.4, 0.4, 0.4))
#   s < 1
# }
# expect_equal(diff(par), TRUE)
