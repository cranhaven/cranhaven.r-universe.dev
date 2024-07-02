context("testing graph partitioning methods")

dat <- data.frame(
  id = 1:48,
  mom = c(NA, NA, 2L, 2L, 2L, NA, NA, 7L, 7L, 7L, 3L, 3L, 3L, 3L, NA, 15L, 15L, 43L, 18L, NA, NA, 21L, 21L, 9L, 9L, 9L, 9L, NA, NA, 29L, 29L, 29L, 30L, 30L, NA, NA, 36L, 36L, 36L, 38L, 38L, NA, NA, 43L, 43L, 43L, 32L, 32L),
  dad = c(NA, NA, 1L, 1L, 1L, NA, NA, 6L, 6L, 6L, 8L, 8L, 8L, 8L, NA, 4L, 4L, 42L, 5L, NA, NA, 20L, 20L, 22L, 22L, 22L, 22L, NA, NA, 28L, 28L, 28L, 23L, 23L, NA, NA, 35L, 35L, 35L, 31L, 31L, NA, NA, 42L, 42L, 42L, 45L, 45L),
  sex = c(1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L))

is_final <- c(16:17, 11:14, 24:27, 33:34, 40:41, 47:48, 19L)
dat$id_weight <- ifelse(dat$id %in% is_final, 1., 1e-5)
dat$father_weight <- dat$mother_weight <- ifelse(dat$id %in% is_final, 10., 1.)

test_that("the _pedigree methods give the same", {
  cuts <- with(
    dat, biconnected_components_pedigree(id = id, father.id = dad,
                                         mother.id = mom))
  expect_known_value(cuts, "biconnected_components_pedigree.RDS")

  tree <- with(
    dat, block_cut_tree_pedigree(id = id, father.id = dad, mother.id = mom))
  expect_known_value(cuts, "block_cut_tree_pedigree.RDS")

  partition <- with(
    dat, max_balanced_partition_pedigree(
      id = id, father.id = dad, mother.id = mom, do_reorder = FALSE))
  expect_known_value(partition, "max_balanced_partition_pedigree.RDS")
  expect_true(length(intersect(partition$set_1, partition$set_2)) < 1)
  expect_true(all(dat$id %in% c(partition$set_1, partition$set_2)))

  partition_reordered <- with(
    dat, max_balanced_partition_pedigree(
      id = id, father.id = dad, mother.id = mom, do_reorder = TRUE))
  expect_equal(partition, partition_reordered)

  # w/ cut
  partition <- with(
    dat, max_balanced_partition_pedigree(
      id = id, father.id = dad, mother.id = mom, slack = .1, max_kl_it = 50L,
      max_kl_it_inner = 1000L, do_reorder = FALSE))
  expect_known_value(partition, "max_balanced_partition_pedigree-w_cut.RDS")
  expect_true(length(intersect(partition$set_1, partition$set_2)) < 1)
  expect_true(all(dat$id %in% c(partition$set_1, partition$set_2)))

  partition_reordered <- with(
    dat, max_balanced_partition_pedigree(
      id = id, father.id = dad, mother.id = mom, slack = .1, max_kl_it = 50L,
      max_kl_it_inner = 1000L, do_reorder = TRUE))
  expect_equal(partition, partition_reordered)

  # w/ cut and weights
  partition <- with(
    dat, max_balanced_partition_pedigree(
      id = id, father.id = dad, mother.id = mom, slack = .1, max_kl_it = 50L,
      max_kl_it_inner = 1000L, id_weight = id_weight, do_reorder = FALSE))
  expect_known_value(partition, "max_balanced_partition_pedigree-w_cut-n-weights.RDS")
  expect_true(length(intersect(partition$set_1, partition$set_2)) < 1)
  expect_true(all(dat$id %in% c(partition$set_1, partition$set_2)))

  partition_reordered <- with(
    dat, max_balanced_partition_pedigree(
      id = id, father.id = dad, mother.id = mom, slack = .1, max_kl_it = 50L,
      max_kl_it_inner = 1000L, id_weight = id_weight, do_reorder = TRUE))
  expect_equal(partition, partition_reordered)

  # w/ cut and weights for both vertices and edges
  partition <- with(
    dat, max_balanced_partition_pedigree(
      id = id, father.id = dad, mother.id = mom, slack = .1, max_kl_it = 50L,
      max_kl_it_inner = 1000L, id_weight = id_weight, do_reorder = FALSE,
      father_weight = father_weight, mother_weight = mother_weight))
  expect_known_value(partition, "max_balanced_partition_pedigree-w_cut-n-2xweights.RDS")
  expect_true(length(intersect(partition$set_1, partition$set_2)) < 1)
  expect_true(all(dat$id %in% c(partition$set_1, partition$set_2)))

  partition_reordered <- with(
    dat, max_balanced_partition_pedigree(
      id = id, father.id = dad, mother.id = mom, slack = .1, max_kl_it = 50L,
      max_kl_it_inner = 1000L, id_weight = id_weight, do_reorder = TRUE,
      father_weight = father_weight, mother_weight = mother_weight))
  expect_equal(partition, partition_reordered)

  # unconnected
  skip_on_os("solaris")
  connected_partition <- partition
  partition <- with(
    dat, unconnected_partition_pedigree(
      id = id, father.id = dad, mother.id = mom, slack = .1, max_kl_it = 50L,
      max_kl_it_inner = 1000L, id_weight = id_weight,
      father_weight = father_weight, mother_weight = mother_weight))
  expect_known_value(partition, "unconnected_partition_pedigree-w_cut-n-2xweights.RDS")
  expect_true(length(intersect(partition$set_1, partition$set_2)) < 1)
  expect_true(all(dat$id %in% c(partition$set_1, partition$set_2)))

  # unconnected and start from connected
  partition <- with(
    dat, unconnected_partition_pedigree(
      id = id, father.id = dad, mother.id = mom, slack = .1, max_kl_it = 50L,
      max_kl_it_inner = 1000L, id_weight = id_weight,
      init = connected_partition$set_1))
  expect_known_value(partition, "unconnected_partition_pedigree-start.RDS")
  expect_true(length(intersect(partition$set_1, partition$set_2)) < 1)
  expect_true(all(dat$id %in% c(partition$set_1, partition$set_2)))
})

# # simulates a connected graph of a given size
# sim_graph <- function(size){
#   stopifnot(size > 2)
#   w_all <- (500:1)^(-2)
#   out <- lapply(3:size, function(n){
#     n_links <- sample.int(4L, 1L, prob = c(.5, .4, .05, .05))
#     n_links <- min(n - 1L, n_links)
#     idx_start <- max(1L, n - 500L)
#     idx_end <- n - 1L
#     prob <- if(n > 502L) w_all else (idx_end:idx_start)^(-2)
#     cbind(from = n, to = sample(idx_start:idx_end, n_links, prob = prob))
#   })
#   out <- c(list(cbind(1L, 2L)), out)
#   out <- do.call(rbind, out)
#   ids <- sample.int(2L * size, size)
#   out[] <- ids[out]
#   out <- t(apply(out, 1L, sort))
#   setNames(as.data.frame(out), c("from", "to"))
# }
# set.seed(99L)
# dat <- sim_graph(50L)
# saveRDS(dat, "graph.RDS")
dat <- readRDS("graph.RDS")

test_that("the partitioning functions for graphs give the same", {
  cuts <- with(
    dat, biconnected_components(from = from, to = to))
  expect_known_value(cuts, "biconnected_components.RDS")

  tree <- with(
    dat, block_cut_tree(from = from, to = to))
  expect_known_value(cuts, "block_cut_tree.RDS")

  partition <- with(
    dat, max_balanced_partition(from = from, to = to, do_reorder = FALSE))
  expect_known_value(partition, "max_balanced_partition.RDS")
  expect_true(length(intersect(partition$set_1, partition$set_2)) < 1)
  expect_true(all(c(dat$from, dat$to) %in% c(partition$set_1, partition$set_2)))

  partition_reordered <- with(
    dat, max_balanced_partition(from = from, to = to, do_reorder = TRUE))
  expect_equal(partition, partition_reordered)

  # w/ cut
  connected_partition <- partition
  partition <- with(
    dat, max_balanced_partition(
      from = from, to = to, slack = .1, max_kl_it = 50L,
      max_kl_it_inner = 1000L, do_reorder = FALSE))
  expect_known_value(partition, "max_balanced_partition-w_cut.RDS")
  expect_true(length(intersect(partition$set_1, partition$set_2)) < 1)
  expect_true(all(c(dat$from, dat$to) %in% c(partition$set_1, partition$set_2)))

  partition_reordered <- with(
    dat, max_balanced_partition(
      from = from, to = to, slack = .1, max_kl_it = 50L,
      max_kl_it_inner = 1000L, do_reorder = FALSE))
  expect_equal(partition, partition_reordered)

  # unconnected
  skip_on_os("solaris")
  partition <- with(
    dat, unconnected_partition(
      from = from, to = to, slack = .1, max_kl_it = 50L,
      max_kl_it_inner = 1000L))
  expect_known_value(partition, "unconnected_partition-w_cut.RDS")
  expect_true(length(intersect(partition$set_1, partition$set_2)) < 1)
  expect_true(all(c(dat$from, dat$to) %in% c(partition$set_1, partition$set_2)))

  # unconnected and start from connected
  partition <- with(
    dat, unconnected_partition(
      from = from, to = to, slack = .1, max_kl_it = 50L,
      max_kl_it_inner = 1000L, init = connected_partition$set_1))
  expect_known_value(partition, "unconnected_partition-start-w_cut.RDS")
  expect_true(length(intersect(partition$set_1, partition$set_2)) < 1)
  expect_true(all(c(dat$from, dat$to) %in% c(partition$set_1, partition$set_2)))

  # w/ cut and weights
  partition <- with(
    dat, max_balanced_partition(
      from = from, to = to, slack = .1, max_kl_it = 50L, do_reorder = FALSE,
      max_kl_it_inner = 1000L, weight_data = list(id = dat$from[3],
                                                  weight = 100)))
  expect_known_value(partition, "max_balanced_partition-w_cut-n-weights.RDS")
  expect_true(length(intersect(partition$set_1, partition$set_2)) < 1)
  expect_true(all(c(dat$from, dat$to) %in% c(partition$set_1, partition$set_2)))

  partition_reordered <- with(
    dat, max_balanced_partition(
      from = from, to = to, slack = .1, max_kl_it = 50L, do_reorder = TRUE,
      max_kl_it_inner = 1000L, weight_data = list(id = dat$from[3],
                                                  weight = 100)))
  expect_equal(partition, partition_reordered)
})
