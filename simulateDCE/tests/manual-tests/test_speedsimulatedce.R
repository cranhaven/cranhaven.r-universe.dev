# ─────────────────────────────────────────────────────────────────────────────
# BENCHMARK in pure data.table, no back-and-forth to data.frame
# ─────────────────────────────────────────────────────────────────────────────

library(formula.tools)  # lhs() / rhs()
library(compiler)        # cmpfun()
library(purrr)           # map(), map2()
library(data.table)      # as.data.table(), .GRP, rbindlist(), setcolorder()
library(tictoc)          # tic()/toc()

# 1) Prepare data ------------------------------------------------------------
set.seed(2025)
n    <- 1e8
ngrp <- 10
df <- data.frame(
  price   = runif(n, 5, 100),
  quality = runif(n, 1,   5),
  group   = rep(1:ngrp, length.out = n)
)
bprice   <- -0.2
bquality <-  0.8

single_group <- list(
  V1 = V1 ~ bprice * price + bquality * quality,
  V2 = V2 ~ 0
)
utility_list <- rep(list(single_group), ngrp)

# 2) Helpers ---------------------------------------------------------------

compile_one <- function(fm) {
  nm  <- as.character(lhs(fm))
  rhs <- rhs(fm)
  fn  <- eval(bquote(function(d) with(d, .(rhs))))
  list(name = nm, fun = cmpfun(fn))
}

compile_utility_list <- function(ul) {
  lapply(ul, function(fl) {
    tmp <- lapply(fl, compile_one)
    setNames(lapply(tmp, `[[`, "fun"),
             vapply(tmp, `[[`, "", "name"))
  })
}

# 3) Four methods, all returning data.tables -------------------------------

old_group <- function(data, utility) {
  tic("OLD-GROUP")
  subs <- split(data, data$group)
  out  <- map2(utility, subs, ~ {
    dt <- as.data.table(.y)
    for (fm in .x) {
      dt[, as.character(lhs(fm)) := eval(rhs(fm), dt)]
    }
    dt
  })
  res <- rbindlist(out)
  toc(log = FALSE)
  res
}

new1_group <- function(data, utility) {
  ufuns <- compile_utility_list(utility)
  tic("NEW1-GROUP")
  subs  <- split(data, data$group)
  out   <- map2(ufuns, subs, ~ {
    dt <- as.data.table(.y)
    for (nm in names(.x)) dt[, (nm) := .x[[nm]](dt)]
    dt
  })
  res <- rbindlist(out)
  toc(log = FALSE)
  res
}

new2_group <- function(data, utility) {
  ufuns  <- compile_utility_list(utility)
  dt     <- as.data.table(data)
  groups <- sort(unique(dt$group))
  gm     <- setNames(seq_along(groups), groups)

  tic("NEW2-GROUP (loop)")
  for (g in groups) {
    idx <- gm[as.character(g)]
    fns <- ufuns[[idx]]
    dt[group == g, (names(fns)) := lapply(fns, function(f) f(.SD))]
  }
  toc(log = FALSE)
  dt
}

new3_group <- function(data, utility) {
  ufuns <- compile_utility_list(utility)
  dt    <- as.data.table(data)

  # figure which columns your utilities will actually use
  sdcols <- intersect(
    names(dt),
    unique(unlist(lapply(utility, function(fl) {
      unlist(lapply(fl, function(fm) all.vars(rhs(fm))))
    })))
  )
  varn <- names(ufuns[[1]])

  tic("NEW3-GROUP (single dt[ , by=group ])")
  out <- dt[
    ,
    c(.SD,
      setNames(lapply(ufuns[[.GRP]], function(f) f(.SD)), varn)
    ),
    by      = group,
    .SDcols = sdcols
  ]
  toc(log = FALSE)

  # reorder columns to exactly: (sdcols, group, new vars)
  setcolorder(out, c(sdcols, "group", varn))
  out
}

# 4) Run & compare ---------------------------------------------------------

res_old  <- old_group(df, utility_list)
res_new1 <- new1_group(df, utility_list)
res_new2 <- new2_group(df, utility_list)
res_new3 <- new3_group(df, utility_list)

# numeric‐accuracy check (ignores type/class)
stopifnot(
  isTRUE(all.equal(res_old,  res_new1)),
  isTRUE(all.equal(res_new1, res_new2)),
  isTRUE(all.equal(res_new2, res_new3))
)
message("✅ All four methods agree (within numeric tolerance).")
