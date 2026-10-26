# ==============================================================================
# Balance as a flow
# ==============================================================================
# The largest matched sample meeting a categorical balance requirement is the
# optimum of a minimum-cost flow problem, and this file is that problem.
# Nothing here searches: the network is built so that its cheapest feasible flow
# is that sample, with balance and then total distance breaking the ties.
#
# Every unit on both sides carries a category at each of H nested levels, level
# 1 the coarsest and level H the finest. Write T_c and K_c for the units of
# finest category c on the left and on the right, a_c and b_c for how many of
# them a matching uses, and
#
#     U_c = max(|T_c|, |K_c|),      K = sum_c U_c.
#
# U_c is a budget of flow forced through category c on each side, and it is the
# maximum rather than the minimum because a category holding only left units
# beside a sibling holding only right ones would otherwise be capped below the
# pairs it can actually reach. The unit arcs cap the real flow at |T_c| and
# |K_c| anyway, so a budget above them buys nothing but slack.
#
# The budget is what turns balance into conservation. Category c on the treated
# side receives exactly U_c and spends it on matched units, on a slack arc, and
# on transfers out; category c on the control side must deliver exactly U_c and
# receives it from matched units, from that same slack arc, and from transfers
# in:
#
#     a_c + s_c + out_c = U_c,      b_c + s_c + in_c = U_c,
#
# so a_c - b_c = in_c - out_c. The slack arc runs from the treated node to the
# control node of the same category, which is what makes s_c the same number in
# both equations, and with no transfer arcs at all it forces a_c = b_c for every
# c: exact fine balance, structurally rather than as a side condition.
#
# Refined balance relaxes that by letting one unit of imbalance travel between
# categories through a pair of port trees. Surplus leaves the treated side at
# the finest level, climbs the OUT tree, crosses to the IN tree at some level,
# and descends to the control side of another category. A crossing at level h
# joins two cells that agree down to level h and differ from level h+1 on, so it
# is a unit of imbalance at exactly the levels finer than h, and the lowest
# level at which the two cells meet is their lowest common ancestor: nothing
# crosses below it, and crossing above it costs strictly more. Pricing a
# crossing at level h at
#
#     P + gamma_h,      gamma_h = sum_{l > h} eps_l,
#
# therefore makes it pay eps_l once for each level l at which it stands as an
# imbalance. The leading P is not a second charge for the same thing: every unit
# leaving the source arrives at the sink as a pair, as slack, or as a transfer,
# so
#
#     pairs = K - S - T,
#
# and the P on a transfer stands in for the slack unit the transfer displaced.
# The arc costs then total
#
#     P * (K - pairs) + sum_h eps_h * imbalance_h + sum_ij d_ij x_ij,
#
#     imbalance_h = (1/2) sum_{c at level h} |a_c - b_c|,
#
# which is the lexicographic objective the design asks for: cardinality first,
# then balance from the coarsest level down, then distance. eps_1 > ... > eps_H
# and P above all of them come from .lex_tier_weights(), which owns the
# precision argument.
#
# `exact` names how many of the coarsest levels admit no crossing above them.
# With crossings only at levels exact .. H-1 no unit of imbalance can leave a
# level-`exact` cell, so balance at levels 1 .. exact holds exactly and the
# finer levels are priced. exact = H is fine balance on the finest partition;
# exact = 0 prices every level, including the coarsest, through a crossing at
# the root.
#
# The tree arcs above the finest level carry forced bounds rather than free
# ones. A free arc would let the treated and the control side of one cell draw
# different amounts from their parent, and that difference would buy imbalance
# no crossing had paid for; forcing every level's budget is what keeps
# a_c - b_c equal to in_c - out_c at every cell instead of only in total.
#
# Node ids are 1-based and follow the convention in src/flow/flow_problem.h:
# source, sink, left units, right units, then the design's own nodes.
# .balance_node_layout() is the only place a node id is computed.
# ==============================================================================

# Sum `x` within groups, over a fixed group range, keeping empty groups at zero.
.balance_group_sum <- function(x, group, n_groups) {
  out <- numeric(n_groups)
  if (!length(x) || !n_groups) {
    return(out)
  }
  agg <- rowsum(as.numeric(x), group = as.integer(group), reorder = FALSE)
  out[as.integer(rownames(agg))] <- as.numeric(agg)
  out
}

# Columns naming a partition. A character vector lists them; a one-sided formula
# names them through its terms, where `+` and `:` both stand for the same
# cross-classification because a partition has no main effects to separate.
.balance_columns <- function(spec, what = "spec") {
  if (is.null(spec)) {
    return(character(0))
  }
  if (inherits(spec, "formula")) {
    if (length(spec) != 2L) {
      stop("`", what, "` must be a one-sided formula, as in ~ region + sex.",
           call. = FALSE)
    }
    return(unique(all.vars(spec[[2L]])))
  }
  if (is.character(spec)) {
    return(unique(spec))
  }
  stop("`", what, "` must be a character vector of column names or a ",
       "one-sided formula.", call. = FALSE)
}

# One key per row, joining the partition columns. The separator is a carriage
# return so that a key never collides with one built from different values.
.balance_keys <- function(df, cols, side) {
  if (!length(cols)) {
    return(rep("", nrow(df)))
  }
  absent <- setdiff(cols, names(df))
  if (length(absent)) {
    stop("`", side, "` has no column(s) ", paste(absent, collapse = ", "),
         " to partition on.", call. = FALSE)
  }
  parts <- lapply(cols, function(k) {
    v <- df[[k]]
    if (anyNA(v)) {
      stop("Column `", k, "` has missing values in `", side, "`; a unit with ",
           "no category has no place in the partition.", call. = FALSE)
    }
    as.character(v)
  })
  do.call(paste, c(parts, list(sep = "\r")))
}

.balance_label <- function(x) gsub("\r", " : ", x, fixed = TRUE)

# Category codes for both sides against one shared set of labels, taken from the
# pooled rows so that a category present on one side only still gets a code. It
# has to: U_c = max(|T_c|, |K_c|) is what such a category contributes, and a
# code it does not hold cannot contribute anything.
.balance_partition <- function(left, right, spec) {
  cols <- .balance_columns(spec, "spec")
  key_left <- .balance_keys(left, cols, "left")
  key_right <- .balance_keys(right, cols, "right")
  labels <- sort(unique(c(key_left, key_right)))
  list(left = match(key_left, labels),
       right = match(key_right, labels),
       labels = labels,
       n_cats = length(labels),
       columns = cols)
}

# The groupings a refined specification names, coarsest first. A list states
# them one by one; a character vector is the shorthand for its own sequence of
# prefixes, so c("region", "site") means region, then region crossed with site.
.refined_levels <- function(spec) {
  if (is.null(spec)) {
    return(list(character(0)))
  }
  if (inherits(spec, "formula")) {
    return(list(.balance_columns(spec, "refined")))
  }
  if (is.character(spec)) {
    if (!length(spec)) {
      return(list(character(0)))
    }
    return(lapply(seq_along(spec), function(k) spec[seq_len(k)]))
  }
  if (is.list(spec)) {
    if (!length(spec)) {
      return(list(character(0)))
    }
    return(lapply(spec, .balance_columns, what = "refined"))
  }
  stop("`refined` must be a character vector, a one-sided formula, or a list ",
       "of groupings ordered coarsest first.", call. = FALSE)
}

# Nestedness is checked, not assumed. Every level-h cell has to sit inside one
# level-(h-1) cell across the pooled rows; a cell straddling two parents has no
# lowest common ancestor for the transfers to cross at, so the network would
# price an imbalance it cannot route.
.refined_check_nested <- function(fine, coarse) {
  code_fine <- c(fine$left, fine$right)
  code_coarse <- c(coarse$left, coarse$right)
  if (!length(code_fine)) {
    return(integer(0))
  }
  first <- code_coarse[match(seq_len(fine$n_cats), code_fine)]
  bad <- which(code_coarse != first[code_fine])
  if (length(bad)) {
    k <- bad[[1L]]
    parent_seen <- coarse$labels[[first[[code_fine[[k]]]]]]
    parent_here <- coarse$labels[[code_coarse[[k]]]]
    stop("`refined` is not nested: cell ",
         .balance_label(fine$labels[[code_fine[[k]]]]),
         " straddles ", .balance_label(parent_seen),
         " and ", .balance_label(parent_here),
         " at the coarser level.", call. = FALSE)
  }
  first
}

.refined_exact_arg <- function(exact, n_levels) {
  if (is.null(exact)) {
    return(1L)
  }
  exact <- .flow_count(exact, "exact")
  if (exact > n_levels) {
    stop("`exact` must be between 0 and ", n_levels,
         ", the depth of `refined`.", call. = FALSE)
  }
  as.integer(exact)
}

# A validated hierarchy: the codes at every level, the parent of every cell, and
# how many of the coarsest levels are enforced exactly. Level 0 is the root cell
# holding everything, which is where a crossing lands when no level is exact.
.refined_hierarchy <- function(left, right, spec, exact = 1L) {
  groupings <- .refined_levels(spec)
  n_levels <- length(groupings)
  exact <- .refined_exact_arg(exact, n_levels)

  parts <- lapply(groupings,
                  function(cols) .balance_partition(left, right, cols))

  parent <- vector("list", n_levels)
  parent[[1L]] <- rep.int(1L, parts[[1L]]$n_cats)
  if (n_levels >= 2L) {
    for (h in 2:n_levels) {
      parent[[h]] <- .refined_check_nested(parts[[h]], parts[[h - 1L]])
    }
  }

  list(n_levels = n_levels,
       exact = exact,
       n_cats = c(1L, vapply(parts, function(p) as.integer(p$n_cats),
                             integer(1))),
       codes_left = lapply(parts, `[[`, "left"),
       codes_right = lapply(parts, `[[`, "right"),
       labels = lapply(parts, `[[`, "labels"),
       columns = lapply(parts, `[[`, "columns"),
       parent = parent,
       n_left = nrow(left),
       n_right = nrow(right))
}

# The codes a unit carries at level `h`, walked up from the finest level through
# the parent maps rather than recomputed from the data, so an overridden set of
# finest codes stays consistent with the tree it is placed in.
.balance_codes_at <- function(hier, finest, h) {
  out <- as.integer(finest)
  n_levels <- hier$n_levels
  if (h < n_levels) {
    for (l in seq.int(n_levels, h + 1L)) {
      out <- hier$parent[[l]][out]
    }
  }
  out
}

# The forced budget at every level: max(|T_c|, |K_c|) at the finest one, summed
# up the tree above it.
.balance_budget <- function(hier, code_left, code_right) {
  n_levels <- hier$n_levels
  n_fine <- hier$n_cats[[n_levels + 1L]]
  count_left <- tabulate(code_left, nbins = n_fine)
  count_right <- tabulate(code_right, nbins = n_fine)

  level <- vector("list", n_levels)
  level[[n_levels]] <- as.numeric(pmax(count_left, count_right))
  if (n_levels >= 2L) {
    for (h in seq.int(n_levels - 1L, 1L)) {
      level[[h]] <- .balance_group_sum(level[[h + 1L]], hier$parent[[h + 1L]],
                                       hier$n_cats[[h + 1L]])
    }
  }

  list(level = level, finest = level[[n_levels]],
       count_left = count_left, count_right = count_right,
       total = sum(level[[n_levels]]))
}

# The levels a crossing may happen at. They run from the coarsest level that is
# not enforced exactly down to the level above the finest, and level 0 is the
# root: a crossing there joins two different level-1 cells.
.balance_cross_levels <- function(hier) {
  if (hier$exact > hier$n_levels - 1L) {
    return(integer(0))
  }
  seq.int(hier$exact, hier$n_levels - 1L)
}

#' Node layout of a balance flow network
#'
#' Every base offset the network uses, and the accessors that turn a level and a
#' category into a node id. No other function computes a node id.
#'
#' @keywords internal
.balance_node_layout <- function(n_left, n_right, hierarchy) {
  n_left <- as.integer(.flow_count(n_left, "n_left"))
  n_right <- as.integer(.flow_count(n_right, "n_right"))
  n_levels <- hierarchy$n_levels
  n_cats <- hierarchy$n_cats
  cross <- .balance_cross_levels(hierarchy)

  left_base <- 2L
  right_base <- left_base + n_left
  cursor <- right_base + n_right

  tc_base <- rep(NA_integer_, n_levels + 1L)
  cc_base <- rep(NA_integer_, n_levels + 1L)
  for (h in seq_len(n_levels)) {
    tc_base[[h + 1L]] <- cursor
    cursor <- cursor + n_cats[[h + 1L]]
    cc_base[[h + 1L]] <- cursor
    cursor <- cursor + n_cats[[h + 1L]]
  }

  out_base <- rep(NA_integer_, n_levels + 1L)
  in_base <- rep(NA_integer_, n_levels + 1L)
  for (h in cross) {
    out_base[[h + 1L]] <- cursor
    cursor <- cursor + n_cats[[h + 1L]]
    in_base[[h + 1L]] <- cursor
    cursor <- cursor + n_cats[[h + 1L]]
  }

  list(source = 1L,
       sink = 2L,
       left_base = left_base,
       right_base = right_base,
       tc_base = tc_base,
       cc_base = cc_base,
       out_base = out_base,
       in_base = in_base,
       cross_levels = cross,
       n_left = n_left,
       n_right = n_right,
       n_nodes = cursor,
       node_source = function() 1L,
       node_sink = function() 2L,
       node_left = function(i) left_base + as.integer(i),
       node_right = function(j) right_base + as.integer(j),
       node_tc = function(h, c) tc_base[as.integer(h) + 1L] + as.integer(c),
       node_cc = function(h, c) cc_base[as.integer(h) + 1L] + as.integer(c),
       node_out = function(h, c) out_base[as.integer(h) + 1L] + as.integer(c),
       node_in = function(h, c) in_base[as.integer(h) + 1L] + as.integer(c))
}

# Tier weights for cardinality over balance over distance. `counts` runs from
# the tier that yields first to the tier that yields last, so the finest level's
# imbalance sits lowest and the pair count highest, and the distance the weights
# have to clear is bounded by K * d_max.
.balance_tiers <- function(n_levels, total_budget, max_imbalance, d_max) {
  counts <- c(rep.int(max_imbalance, n_levels), total_budget)
  weights <- .lex_tier_weights(counts, base_magnitude = total_budget * d_max)
  if (is.null(weights)) {
    stop("The cost range is too wide to order matchings by cardinality ",
         "exactly in double precision. Rescale the costs, or reduce the depth ",
         "of `refined`.", call. = FALSE)
  }
  eps <- rev(weights[seq_len(n_levels)])
  penalty <- weights[[n_levels + 1L]]
  # gamma[h + 1] is the sum of eps over the levels finer than h, which is what a
  # crossing at level h owes: one eps for every level at which it is imbalanced.
  gamma <- c(rev(cumsum(rev(eps))), 0)
  list(penalty = penalty, eps = eps, gamma = gamma, weights = weights,
       counts = counts, base_magnitude = total_budget * d_max)
}

.balance_arc_bounds <- function(arc_bounds, total_budget) {
  defaults <- list(left_unit = 1, right_unit = 1, pair = 1,
                   slack = total_budget, port = total_budget)
  if (is.null(arc_bounds)) {
    return(defaults)
  }
  if (!is.list(arc_bounds)) {
    stop("`arc_bounds` must be a named list of upper bounds.", call. = FALSE)
  }
  unknown <- setdiff(names(arc_bounds), names(defaults))
  if (length(unknown)) {
    stop("`arc_bounds` has no arc class(es) ", paste(unknown, collapse = ", "),
         ".", call. = FALSE)
  }
  for (nm in names(arc_bounds)) {
    defaults[[nm]] <- .flow_count(arc_bounds[[nm]], paste0("arc_bounds$", nm),
                                  allow_inf = TRUE)
  }
  defaults
}

# The pairs a balance network carries arcs for, and the distance range the tier
# weights are built against. A matrix gives every admissible cell, in the
# column-major order `which()` reads them in. A pool built elsewhere gives the
# pairs it holds together with the smallest and largest admissible distance over
# every pair, which is what keeps the weights valid for the complete problem
# while the network carries only some of its pairs.
.balance_pair_pool <- function(cost) {
  if (inherits(cost, "balance_pair_pool")) {
    return(cost)
  }
  cost <- as.matrix(cost)
  if (!is.numeric(cost)) {
    stop("`cost` must be a numeric matrix of distances.", call. = FALSE)
  }
  valid <- .is_valid_cost(cost)
  cell <- which(valid, arr.ind = TRUE)
  structure(list(n_left = nrow(cost), n_right = ncol(cost),
                 left = as.integer(cell[, 1L]), right = as.integer(cell[, 2L]),
                 distance = as.numeric(cost[valid]),
                 min = if (any(valid)) min(cost[valid]) else NA_real_,
                 max = if (any(valid)) max(cost[valid]) else NA_real_),
            class = "balance_pair_pool")
}

#' Compile a balance design into a flow problem
#'
#' @param cost A matrix of distances, or a `balance_pair_pool` naming the pairs
#'   to carry and the distance range over every admissible pair.
#' @return A list with `problem`, a `couplr_flow_problem`, and `index`, holding
#'   the arc ranges and the units and cells behind each arc.
#' @keywords internal
.balance_flow_problem <- function(cost, hier, codes = NULL, tiers = NULL,
                                  arc_bounds = NULL) {
  pool <- .balance_pair_pool(cost)
  n_left <- pool$n_left
  n_right <- pool$n_right
  n_levels <- hier$n_levels
  n_fine <- hier$n_cats[[n_levels + 1L]]

  if (is.null(codes)) {
    codes <- list(left = hier$codes_left[[n_levels]],
                  right = hier$codes_right[[n_levels]])
  }
  code_left <- as.integer(codes$left)
  code_right <- as.integer(codes$right)
  if (length(code_left) != n_left || length(code_right) != n_right) {
    stop("`codes` must give one finest-level category per row of `cost` on ",
         "each side; got ", length(code_left), " and ", length(code_right),
         " for a ", n_left, " by ", n_right, " cost matrix.", call. = FALSE)
  }
  if (anyNA(code_left) || anyNA(code_right) ||
      any(code_left < 1L) || any(code_left > n_fine) ||
      any(code_right < 1L) || any(code_right > n_fine)) {
    stop("`codes` must be category indices in 1:", n_fine, ".", call. = FALSE)
  }

  budget <- .balance_budget(hier, code_left, code_right)
  total_budget <- budget$total

  # A forbidden or infinite cell gets no arc at all. Distances are shifted so
  # that the cheapest admissible pair costs nothing, which keeps every arc cost
  # non-negative for a shortest-path search and leaves the ordering of matchings
  # at equal cardinality untouched.
  any_admissible <- is.finite(pool$min)
  shift <- if (any_admissible) pool$min else 0
  d_max <- if (any_admissible) pool$max - shift else 0

  if (is.null(tiers)) {
    tiers <- .balance_tiers(n_levels, total_budget, min(n_left, n_right), d_max)
  }
  penalty <- tiers$penalty
  gamma <- tiers$gamma

  layout <- .balance_node_layout(n_left, n_right, hier)
  cross <- layout$cross_levels
  bounds <- .balance_arc_bounds(arc_bounds, total_budget)

  parts <- list()
  ranges <- list()
  placed <- 0L
  add_arcs <- function(name, tail, head, lower, upper, cost) {
    k <- if (!length(tail) || !length(head)) 0L
         else max(length(tail), length(head))
    tail <- rep_len(as.integer(tail), k)
    head <- rep_len(as.integer(head), k)
    parts[[name]] <<- list(tail = tail, head = head,
                           lower = rep_len(as.numeric(lower), k),
                           upper = rep_len(as.numeric(upper), k),
                           cost = rep_len(as.numeric(cost), k))
    ranges[[name]] <<- if (k) seq.int(placed + 1L, placed + k) else integer(0)
    placed <<- placed + k
    invisible(NULL)
  }

  # The source forces the coarsest budget in and the sink forces it back out.
  n_coarse <- hier$n_cats[[2L]]
  coarse_cells <- seq_len(n_coarse)
  add_arcs("budget_in", layout$node_source(),
           layout$node_tc(1L, coarse_cells),
           budget$level[[1L]], budget$level[[1L]], 0)

  # Every level's budget is forced, so the treated and the control side of one
  # cell always draw the same amount from their parent.
  tree_level <- integer(0)
  tree_cat <- integer(0)
  tree_parent <- integer(0)
  tree_budget <- numeric(0)
  if (n_levels >= 2L) {
    for (h in 2:n_levels) {
      n_h <- hier$n_cats[[h + 1L]]
      tree_level <- c(tree_level, rep.int(h, n_h))
      tree_cat <- c(tree_cat, seq_len(n_h))
      tree_parent <- c(tree_parent, hier$parent[[h]])
      tree_budget <- c(tree_budget, budget$level[[h]])
    }
  }
  add_arcs("tree_tc", layout$node_tc(tree_level - 1L, tree_parent),
           layout$node_tc(tree_level, tree_cat),
           tree_budget, tree_budget, 0)

  add_arcs("unit_left", layout$node_tc(n_levels, code_left),
           layout$node_left(seq_len(n_left)), 0, bounds$left_unit, 0)

  pair_left <- as.integer(pool$left)
  pair_right <- as.integer(pool$right)
  pair_cost <- as.numeric(pool$distance) - shift
  add_arcs("pair", layout$node_left(pair_left), layout$node_right(pair_right),
           0, bounds$pair, pair_cost)

  add_arcs("unit_right", layout$node_right(seq_len(n_right)),
           layout$node_cc(n_levels, code_right), 0, bounds$right_unit, 0)

  add_arcs("tree_cc", layout$node_cc(tree_level, tree_cat),
           layout$node_cc(tree_level - 1L, tree_parent),
           tree_budget, tree_budget, 0)

  add_arcs("budget_out", layout$node_cc(1L, coarse_cells), layout$node_sink(),
           budget$level[[1L]], budget$level[[1L]], 0)

  # The slack arc is the same s_c in both of the budget equations, and it is
  # what a category that cannot fill its budget with pairs pays instead.
  fine_cells <- seq_len(n_fine)
  add_arcs("slack", layout$node_tc(n_levels, fine_cells),
           layout$node_cc(n_levels, fine_cells), 0, bounds$slack, penalty)

  # Surplus climbs the OUT tree from the finest level, crosses at the level its
  # two cells share, and descends the IN tree to the other category.
  port_level <- integer(0)
  port_cat <- integer(0)
  port_parent <- integer(0)
  if (length(cross)) {
    for (l in seq.int(hier$exact + 1L, n_levels)) {
      n_l <- hier$n_cats[[l + 1L]]
      port_level <- c(port_level, rep.int(l, n_l))
      port_cat <- c(port_cat, seq_len(n_l))
      port_parent <- c(port_parent, hier$parent[[l]])
    }
  }
  out_tail <- ifelse(port_level == n_levels,
                     layout$node_tc(n_levels, port_cat),
                     layout$node_out(pmin(port_level, n_levels - 1L), port_cat))
  add_arcs("out_up", out_tail, layout$node_out(port_level - 1L, port_parent),
           0, bounds$port, 0)

  cross_level <- integer(0)
  cross_cat <- integer(0)
  for (h in cross) {
    cross_level <- c(cross_level, rep.int(h, hier$n_cats[[h + 1L]]))
    cross_cat <- c(cross_cat, seq_len(hier$n_cats[[h + 1L]]))
  }
  add_arcs("cross", layout$node_out(cross_level, cross_cat),
           layout$node_in(cross_level, cross_cat),
           0, bounds$port, penalty + gamma[cross_level + 1L])

  in_head <- ifelse(port_level == n_levels,
                    layout$node_cc(n_levels, port_cat),
                    layout$node_in(pmin(port_level, n_levels - 1L), port_cat))
  add_arcs("in_down", layout$node_in(port_level - 1L, port_parent), in_head,
           0, bounds$port, 0)

  arcs <- tibble::tibble(
    tail = as.integer(unlist(lapply(parts, `[[`, "tail"), use.names = FALSE)),
    head = as.integer(unlist(lapply(parts, `[[`, "head"), use.names = FALSE)),
    lower = as.numeric(unlist(lapply(parts, `[[`, "lower"), use.names = FALSE)),
    upper = as.numeric(unlist(lapply(parts, `[[`, "upper"), use.names = FALSE)),
    cost = as.numeric(unlist(lapply(parts, `[[`, "cost"), use.names = FALSE))
  )

  supply <- numeric(layout$n_nodes)
  supply[[layout$node_source()]] <- total_budget
  supply[[layout$node_sink()]] <- -total_budget

  problem <- .flow_problem(layout$n_nodes, supply, arcs)

  index <- list(
    layout = layout,
    hier = hier,
    tiers = tiers,
    ranges = ranges,
    n_arcs = nrow(arcs),
    n_left = n_left,
    n_right = n_right,
    code_left = code_left,
    code_right = code_right,
    budget = budget,
    total_budget = total_budget,
    cost_shift = shift,
    d_max = d_max,
    pair_left = pair_left,
    pair_right = pair_right,
    pair_cost = pair_cost,
    pair_key = pair_left + (pair_right - 1L) * n_left,
    tree_level = tree_level,
    tree_cat = tree_cat,
    port_level = port_level,
    port_cat = port_cat,
    cross_level = cross_level,
    cross_cat = cross_cat,
    bounds = bounds
  )

  list(problem = problem, index = index)
}

# The network with more pairs. Their arcs go on the end of the arc list, so
# every arc already there keeps its index, and with it every bound a search has
# already placed on one; the pair class is the range those new indices extend.
.balance_add_pairs <- function(built, left, right, distance) {
  k <- length(left)
  if (!k) {
    return(built)
  }
  index <- built$index
  problem <- built$problem
  layout <- index$layout
  first <- index$n_arcs + 1L
  added <- tibble::tibble(tail = layout$node_left(left),
                          head = layout$node_right(right),
                          lower = 0,
                          upper = as.numeric(index$bounds$pair),
                          cost = as.numeric(distance) - index$cost_shift)
  arcs <- rbind(problem$arcs, added)

  index$ranges$pair <- c(index$ranges$pair, seq.int(first, first + k - 1L))
  index$pair_left <- c(index$pair_left, as.integer(left))
  index$pair_right <- c(index$pair_right, as.integer(right))
  index$pair_cost <- c(index$pair_cost, added$cost)
  index$pair_key <- index$pair_left + (index$pair_right - 1L) * index$n_left
  index$n_arcs <- nrow(arcs)

  list(problem = .flow_problem(problem$n_nodes, problem$supply, arcs),
       index = index)
}

# A matching stated either as one right index per left unit, with 0 for
# unmatched, or as a two-column table of pairs.
.balance_matching_vector <- function(matching, n_left, n_right) {
  if (is.data.frame(matching)) {
    matching <- as.matrix(matching[, 1:2])
  }
  if (is.matrix(matching)) {
    out <- integer(n_left)
    if (nrow(matching)) {
      out[as.integer(matching[, 1L])] <- as.integer(matching[, 2L])
    }
    matching <- out
  }
  matching <- as.integer(matching)
  if (length(matching) != n_left) {
    stop("`matching` must give one right index per left unit; got ",
         length(matching), " for ", n_left, " left units.", call. = FALSE)
  }
  if (anyNA(matching) || any(matching < 0L) || any(matching > n_right)) {
    stop("`matching` must hold right indices in 0:", n_right,
         ", with 0 for an unmatched left unit.", call. = FALSE)
  }
  taken <- matching[matching > 0L]
  if (anyDuplicated(taken)) {
    stop("`matching` uses a right unit more than once.", call. = FALSE)
  }
  matching
}

#' The flow a matched set corresponds to
#'
#' Builds the flow vector a candidate matched set maps to: unit and pair arcs at
#' one, the slack each category needs to fill its budget, and the transfers that
#' carry its imbalance, each crossing at the lowest level its two cells share.
#'
#' Returns `NULL` when the matched set is not balanced at the levels the design
#' enforces exactly, since no flow in this network represents it.
#'
#' @keywords internal
.balance_flow_encode <- function(matching, index, hier = index$hier) {
  matching <- .balance_matching_vector(matching, index$n_left, index$n_right)
  n_levels <- hier$n_levels
  n_fine <- hier$n_cats[[n_levels + 1L]]
  ranges <- index$ranges
  budget <- index$budget

  left_idx <- which(matching > 0L)
  right_idx <- matching[left_idx]

  arc <- match(left_idx + (right_idx - 1L) * index$n_left, index$pair_key)
  if (anyNA(arc)) {
    stop("`matching` pairs a left and a right unit the cost matrix forbids.",
         call. = FALSE)
  }

  a <- tabulate(index$code_left[left_idx], nbins = n_fine)
  b <- tabulate(index$code_right[right_idx], nbins = n_fine)

  # Minimal per-category encoding: the budget goes to pairs first, the signed
  # imbalance leaves as a transfer, and the remainder is slack.
  out_flow <- pmax(b - a, 0)
  in_flow <- pmax(a - b, 0)
  slack <- budget$finest - pmax(a, b)
  if (any(slack < 0)) {
    stop("A category holds more matched units than its budget, which cannot ",
         "happen with U_c = max(|T_c|, |K_c|).", call. = FALSE)
  }

  # Route the transfers up the port trees, crossing as low as possible. A
  # crossing at level h costs P + gamma_h and gamma is decreasing in h, so
  # crossing everything that can meet at the current level is what min-cost
  # does, and what is left over has to travel further up.
  cross <- .balance_cross_levels(hier)
  res_out <- vector("list", n_levels + 1L)
  res_in <- vector("list", n_levels + 1L)
  res_out[[n_levels + 1L]] <- as.numeric(out_flow)
  res_in[[n_levels + 1L]] <- as.numeric(in_flow)
  cross_flow <- vector("list", n_levels + 1L)

  if (!length(cross)) {
    if (any(out_flow > 0) || any(in_flow > 0)) {
      return(NULL)
    }
  } else {
    for (h in rev(cross)) {
      up_out <- .balance_group_sum(res_out[[h + 2L]], hier$parent[[h + 1L]],
                                   hier$n_cats[[h + 1L]])
      up_in <- .balance_group_sum(res_in[[h + 2L]], hier$parent[[h + 1L]],
                                  hier$n_cats[[h + 1L]])
      cross_flow[[h + 1L]] <- pmin(up_out, up_in)
      res_out[[h + 1L]] <- up_out - cross_flow[[h + 1L]]
      res_in[[h + 1L]] <- up_in - cross_flow[[h + 1L]]
    }
    top <- min(cross)
    if (any(res_out[[top + 1L]] > 0) || any(res_in[[top + 1L]] > 0)) {
      return(NULL)
    }
  }

  flow <- numeric(index$n_arcs)
  flow[ranges$budget_in] <- budget$level[[1L]]
  if (length(ranges$tree_tc)) {
    tree_budget <- unlist(lapply(2:n_levels, function(h) budget$level[[h]]),
                          use.names = FALSE)
    flow[ranges$tree_tc] <- tree_budget
    flow[ranges$tree_cc] <- tree_budget
  }
  flow[ranges$budget_out] <- budget$level[[1L]]
  flow[ranges$unit_left[left_idx]] <- 1
  flow[ranges$pair[arc]] <- 1
  flow[ranges$unit_right[right_idx]] <- 1
  flow[ranges$slack] <- slack

  if (length(cross)) {
    port <- unlist(lapply(seq.int(hier$exact + 1L, n_levels),
                          function(l) res_out[[l + 1L]]), use.names = FALSE)
    flow[ranges$out_up] <- port
    flow[ranges$in_down] <- unlist(
      lapply(seq.int(hier$exact + 1L, n_levels),
             function(l) res_in[[l + 1L]]), use.names = FALSE)
    flow[ranges$cross] <- unlist(
      lapply(cross, function(h) cross_flow[[h + 1L]]), use.names = FALSE)
  }

  flow
}

#' Read a solved balance flow
#'
#' @return A list with the matched pairs as left and right indices, the slack
#'   each finest category carried, and the transfers with the level they crossed
#'   at.
#' @keywords internal
.balance_flow_read <- function(index, flow) {
  flow <- .flow_extract_flow(flow)
  if (length(flow) != index$n_arcs) {
    stop("`flow` has ", length(flow), " values but the problem has ",
         index$n_arcs, " arcs.", call. = FALSE)
  }
  ranges <- index$ranges
  taken <- flow[ranges$pair] > 0.5

  left <- index$pair_left[taken]
  right <- index$pair_right[taken]
  matching <- integer(index$n_left)
  matching[left] <- right

  cross_flow <- if (length(ranges$cross)) flow[ranges$cross] else numeric(0)
  keep <- cross_flow > 0.5

  list(left = left,
       right = right,
       matching = matching,
       n_pairs = length(left),
       total_distance = sum(index$pair_cost[taken]),
       slack = flow[ranges$slack],
       total_slack = sum(flow[ranges$slack]),
       transfers = tibble::tibble(
         level = index$cross_level[keep],
         category = index$cross_cat[keep],
         flow = cross_flow[keep]),
       total_transfers = sum(cross_flow))
}

#' Check a balance flow against the objective it is meant to encode
#'
#' Recomputes the matched sample's cardinality, its imbalance at every level and
#' its total distance from the flow's own pair arcs, and compares the arc costs
#' against
#' \code{P * (K - pairs) + sum_h eps_h * imbalance_h + sum_ij d_ij x_ij}.
#'
#' @return A list holding the feasibility counts, the objective read both ways,
#'   and the gap between them.
#' @keywords internal
.balance_flow_audit <- function(problem, index, flow, cost = NULL,
                                tiers = index$tiers) {
  problem <- .as_flow_problem(problem)
  flow <- .flow_extract_flow(flow)
  arcs <- problem$arcs
  if (length(flow) != nrow(arcs)) {
    stop("`flow` has ", length(flow), " values but the problem has ",
         nrow(arcs), " arcs.", call. = FALSE)
  }

  below <- sum(flow < arcs$lower - 1e-9)
  above <- sum(flow > arcs$upper + 1e-9)
  net <- .balance_group_sum(flow, arcs$tail, problem$n_nodes) -
    .balance_group_sum(flow, arcs$head, problem$n_nodes)
  conservation_error <- max(abs(net - problem$supply), 0)

  read <- .balance_flow_read(index, flow)
  hier <- index$hier
  n_levels <- hier$n_levels

  imbalance <- vapply(seq_len(n_levels), function(h) {
    n_h <- hier$n_cats[[h + 1L]]
    a <- tabulate(.balance_codes_at(hier, index$code_left, h)[read$left],
                  nbins = n_h)
    b <- tabulate(.balance_codes_at(hier, index$code_right, h)[read$right],
                  nbins = n_h)
    sum(pmax(a - b, 0))
  }, numeric(1))

  distance <- if (is.null(cost)) {
    read$total_distance
  } else {
    cost <- as.matrix(cost)
    if (length(read$left)) {
      sum(cost[cbind(read$left, read$right)] - index$cost_shift)
    } else {
      0
    }
  }

  arc_cost <- sum(arcs$cost * flow)
  identity <- tiers$penalty * (index$total_budget - read$n_pairs) +
    sum(tiers$eps * imbalance) + distance

  # A cell feeding both directions of a crossing would be sending surplus out
  # and asking for it back at the same time, which costs gamma per unit and buys
  # nothing.
  self_crossing <- 0
  if (length(index$ranges$out_up)) {
    out_up <- flow[index$ranges$out_up]
    in_down <- flow[index$ranges$in_down]
    self_crossing <- sum(out_up > 0.5 & in_down > 0.5)
  }

  list(n_below_lower = below,
       n_above_upper = above,
       max_conservation_error = conservation_error,
       feasible = below == 0L && above == 0L && conservation_error < 1e-9,
       n_pairs = read$n_pairs,
       imbalance = imbalance,
       distance = distance,
       total_slack = read$total_slack,
       total_transfers = read$total_transfers,
       arc_cost = arc_cost,
       objective = identity,
       identity_gap = arc_cost - identity,
       pairs_identity_gap = read$n_pairs -
         (index$total_budget - read$total_slack - read$total_transfers),
       n_self_crossing = self_crossing)
}
