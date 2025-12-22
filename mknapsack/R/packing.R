
#' @importFrom assertthat assert_that
suppressPackageStartupMessages({
  library(assertthat)
  library(data.table)
})

#' Collapse function for the MOQ items
#'
#' Combines items with MOQ greater than one to a single line that represents min amount that can be ordered
#' @import data.table
#'
#' @export
#' @param units data.table with following fields: sku, utility, volume, moq
#' @return data.table with sku, utility, volume and units fields. first lines for each sku are grouped according to moq
group_moq <- function(units) {
  sku <- utility <- cnt <- group <- moq <- volume <- NULL # removes NOTEs in check

  dt <- copy(units)
  dt$units <- 1L
  setorder(dt, sku, -utility)

  # Sort by sku
  setkeyv(dt, c("sku"))
  dt[, cnt := 1:.N, by = sku]

  # Up to moq assign the same group.
  dt[, group := as.integer(ifelse(cnt <= moq, 1, cnt)), by = sku]

  # Aggregate to moq group
  res <- dt[, list(
    utility = sum(utility),
    volume = sum(volume),
    units = sum(units)
  ), by = list(sku, group)]
  res[, moq := ifelse(group == 1, 1L, 0L)]
  res$group <- NULL

  return(res)
}


#' Optimal packing into multiple containers
#'
#' Gets containers based on the utility of individual items, their volume and container size
#' @export
#'
#' @param profit vector with profit for item
#' @param volume vector of item sizes in cubic meters
#' @param moq vector of flags where 1 means that row contans mininum order quantity (MOQ).
#'     Defaults to zero vector matching profit in length.
#' @param cap size of the container in cubic meters
#' @param sold vector with a number of items that were sold on demand
#' @return vector with container numbers keeping the permutation of the original data
#'
#' @examples
#'
#' # Calculate the optimal containers summary for a sample dataset
#' data(unitsbro)
#' library(data.table)
#' units.combined <- data.table(unitsbro)
#' moq <- units.combined$moq
#' profit <- units.combined$utility
#' volume <- units.combined$volume
#' res <- mknapsack(profit, volume, moq, 65)
#' units.combined$container <- as.factor(res)
#' #Aggregate solution to container
#' containers <- units.combined[order(container), .(volume = sum(volume),
#' profit = sum(profit)), by = container]
#'
mknapsack <- function(profit, volume,
                      moq = rep(0, length(profit)),
                      cap = 65,
                      sold = rep(0, length(profit))) {

  assert_that(is.numeric(profit), is.numeric(volume))
  res <- rep(NA_integer_, length(profit))
  container <- 0
  ids <- 1:length(profit)

  # force sold items to be in the top container(s)
  profit[sold > 0] <- max(profit / volume) * volume[sold > 0] * 10

  repeat {
    solution <- knapsack(profit, volume, moq, cap)

    if (sum(solution, na.rm = T) == 0) break

    container <- container + 1
    pack <- which(solution > 0) # permutations for current container

    res[ids[pack]] <- container # assign container number to result

    # remove items from current container
    profit <- profit[-pack]
    volume <- volume[-pack]
    moq <- moq[-pack]
    ids <- ids[-pack]

    if (length(profit) == 0) break
  }
  return(res)
}

#' Solves knapsack problem with the library defined
#' in knapsack.solver option:
#'  - cbc (default) - uses rcbc package
#'  - lpsolve - uses lpSolve package
#'
#' @export
#' @inherit mknapsack
knapsack <- function(profit, volume,
                     moq = rep(0, length(profit)),
                     cap = 65) {
  do.call(solver(), as.list(environment()))
}

#' gets solver name from the environment variable
#' @noRd
solver <- function() {
  name <- getOption("mknapsack.solver")
  assert_that(name != "")
  get(paste0("knapsack.", name))
}

#' Solve knapsack problem with lpSolve package
#' @noRd
#' @inherit knapsack
knapsack.lpsolve <- function(profit, volume, moq, cap) {
  moq.constraints <- moq_constraint(moq)
  moq.lines <- nrow(moq.constraints)

  mod <- lpSolve::lp(
    direction = "max",
    objective.in = profit,
    const.mat = rbind(volume, moq.constraints),
    const.dir = c("<=", rep(">=", moq.lines)),
    const.rhs = c(cap, rep(0, moq.lines)),
    all.bin = TRUE
  )
  res <- mod$solution
  return(res)
}

#' Solve knapsack problem with rcbc package
#' @noRd
#' @inherit knapsack
#' @seealso https://github.com/dirkschumacher/rcbc
#' @seealso https://github.com/dirkschumacher/ROI.plugin.cbc
knapsack.cbc <- function(profit, volume, moq, cap) {
  # CBC solver produces out-of-bound solution if coefs are zero.
  volume[volume == 0] <- 1e-10
  arguments <- as.list(environment())
  arguments <- append(
    arguments,
    list(
      solver = "cbc",
      control = list(logLevel = 0, sec = 60)
    )
  )
  do.call(knapsack.roi, arguments)
}

#' Solve knapsack problem with glpk
#' @noRd
#' @inherit knapsack
#' @seealso https://www.gnu.org/software/glpk/
knapsack.glpk <- function(profit, volume, moq, cap) {
  arguments <- as.list(environment())
  arguments <- append(
    arguments,
    list(solver = "glpk")
  )
  do.call(knapsack.roi, arguments)
}
#' Solve knapsack problem via ROI package interface
#' @noRd
#' @inherit knapsack
#' @param solver code for the library that will be used to solve a problem
#' @inheritParams ROI::ROI_solve
#' @seealso http://r-forge.r-project.org/projects/roi
knapsack.roi <- function(profit, volume, moq, cap, solver, control = list()) {
  n <- length(profit)

  if (sum(volume) <= cap) {
    return(rep(1, n))
  }

  moq.constraints <- moq_constraint(moq)
  moq.lines <- nrow(moq.constraints)

  lp <- ROI::OP(
    objective = profit,
    constraints = ROI::L_constraint(
      L = rbind(volume, moq.constraints),
      dir = c("<=", rep(">=", moq.lines)),
      rhs = c(cap, rep(0, moq.lines))
    ),
    maximum = TRUE,
    types = rep("B", length(volume))
  )

  mod <- ROI::ROI_solve(lp, solver, control = control)
  res <- mod$solution
  res[is.na(res)] <- 0
  res <- as.integer(round(res, 0))
  res[res >= 2] <- 0 # Values should be between 0 and 1
  res
}

#' Mininum Order Quantity (MOQ) contstraint generator
#'
#' Creates matrix of moq constraints for the LP optimisation.
#' It is assumed that there is only one moq position per SKU and
#' data is sorted by sku, therefore SKU index can be calculated
#'
#' @param moq flag that indicates that this position contains MOQ
#' @return matrix that expesses the MOQ constraint:
#'   non-MOQ item cannot be put into container that does not contain MOQ item
moq_constraint <- function(moq) {
  sku <- cumsum(moq)
  res <- matrix(nrow = length(sku), ncol = length(sku))
  for (p in unique(sku)) {
    non.moq <- which(sku == p & moq == 0L)
    non.moq.count <- length(non.moq)
    if (non.moq.count > 0 & sum(moq) > 0) {
      # skips cases where we only have one line for a product that contains MOQ
      res[non.moq, non.moq] <- diag(rep(-1, non.moq.count), nrow = non.moq.count)
    }
    res[which(sku == p), which(sku == p & moq == 1L)] <- 1
  }
  res[is.na(res)] <- 0
  res <- subset(res, subset = moq != 1L)
  if (nrow(res) == 0) return(res)
  res <- res[rowSums(res == 0) != ncol(res), ]
  return(res)
}
