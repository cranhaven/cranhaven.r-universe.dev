test_that("dnapath method works on list input", {
  n <- c(10, 15)
  p <- 100
  expr_pair <- list(x1 = matrix(rnorm(p * n[1], 10), nrow = n[1], ncol = p),
                    x2 = matrix(rnorm(p * n[2], 10), nrow = n[2], ncol = p))
  colnames(expr_pair[[1]]) <- 1:p
  colnames(expr_pair[[2]]) <- 1:p
  groups <- NULL
  
  # Delete some of the genes in each group
  expr_pair[[1]] <- expr_pair[[1]][, -c(1, 5, 9)]
  expr_pair[[1]] <- expr_pair[[1]][, -c(9, 10, 23, 90:p)]
  
  network_inference <- function(x, ...) suppressWarnings(run_corr(x, ...))
  n_perm = 10
  lp = 2
  
  pathway_list <- as.character(1:20)
  # Check that seed can be set without error and returns equivalent results.
  results1 <- dnapath(expr_pair, pathway_list, groups,
                 network_inference, n_perm, lp, seed = 1)
  results2 <- dnapath(expr_pair, pathway_list, groups,
                 network_inference, n_perm, lp, seed = 1)
  expect_equal(unclass(results1), unclass(results2))
  
  # Check that methods work on 'dnapath' object
  results <- dnapath(expr_pair, pathway_list, groups,
                 network_inference, n_perm, lp)
  expect_is(results, "dnapath")
  expect_is(summary(results), "tbl")
  expect_is(summarize_genes(results), "tbl")
  expect_is(summarize_pathways(results), "tbl")
  # expect_invisible(plot(results))
  
  # Check that methods work on 'dnapath_list' object
  # Create pathway list.
  pathway_list <- list(as.character(5:9), 
                       as.character(20),
                       as.character(c(1, 5, 8, 13, 19)), 
                       as.character(c(2, 3, 4, 6:20, 101)))
  results <- dnapath(expr_pair, pathway_list, groups,
                 network_inference, n_perm, lp)
  expect_true(unname(dnapath:::get_mean_expr_mat(results[[1]])[1, 4]) == 0)
  expect_is(results, "dnapath_list")
  expect_is(summary(results), "tbl")
  expect_is(summarize_genes(results), "tbl")
  expect_is(summarize_pathways(results), "tbl")
  # expect_invisible(plot(results[[1]]))
  
  # Note the ordering of results[[1]]$genes is affect by the missing
  # columns in each dataset.
  expect_equal(dplyr::arrange(summarize_genes(results), as.numeric(gene))$n_pathways, 
               c(1, 1, 1, 1, 2, 2, 2, 3, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1))
  
  # Check that gene names are unchanged after subsetting.
  genes_before_subset <- results$param$gene_names[results$pathway[[1]]$genes]
  results <- results[c(1, 3)]
  genes_after_subset <- results$param$gene_names[results$pathway[[1]]$genes]
  expect_equal(genes_before_subset, genes_after_subset)
  expect_equal(genes_before_subset, c("6", "7", "8", "5", "9"))
  # Pathway 2 is omitted from the results because it contains only 1 gene.
  expect_equal(sort(results$param$gene_names[results$pathway[[2]]$genes]), 
               sort(unique(as.character(pathway_list[[4]][-19]))))
  
  # Check that methods work on 'dnapath_list' object
  # Now use a named pathway_list.
  pathway_list <- list("a" = 16:49, 
                       "b" = c(1, 5, 5, 5, 5, 5, 8, 13, 19), 
                       "c" = c(2, 3, 4, 6:20))
  results <- dnapath(expr_pair, pathway_list, groups,
                 network_inference, n_perm, lp)
  expect_is(results, "dnapath_list")
  expect_equal(names(results), names(pathway_list))
  
  expect_is(summary(results), "tbl")
  expect_is(summarize_genes(results), "tbl")
  expect_is(summarize_pathways(results), "tbl")
  # expect_invisible(plot(results[[1]]))
  
  # Check that gene names are unchanged after subsetting.
  genes_before_subset <- results$param$gene_names[results$pathway[[3]]$genes]
  results <- results[c(1, 3)]
  genes_after_subset <- results$param$gene_names[results$pathway[[2]]$genes]
  expect_equal(genes_before_subset, genes_after_subset)
  # Pathway 2 is omitted from the results because it contains only 1 gene.
  expect_equal(sort(results$param$gene_names[results$pathway[[2]]$genes]), 
               sort(unique(as.character(pathway_list[[3]]))))
})


test_that("dnapath method works on data.frame input", {
  n <- c(10, 15)
  p <- 100
  expr_pair <- matrix(rnorm(p * (n[1] + n[2]), 10), ncol = p)
  colnames(expr_pair) <- 1:p
  groups <- c('A', 'A', 'B', 'B', 'A', 'B', 'B', 'B', 'B', 'A', 'A', 'A', 
              'B', 'B', 'B', 'B', 'B', 'A', 'B', 'A', 'A', 'B', 'A', 'B', 'B')
  
  # Delete some of the genes in each group
  expr_pair[groups == "A", c(1, 5, 9)] <- 0
  expr_pair[groups == "B", c(9, 10, 23, 90:p)] <- 0
  
  network_inference <- function(x, weights, ...) suppressWarnings(run_corr(x, weights, ...))
  n_perm = 10
  lp = 2
  
  pathway_list <- as.character(1:20)
  # Check that seed can be set without error and returns equivalent results.
  results1 <- dnapath(expr_pair, pathway_list, groups,
                  network_inference, n_perm, lp, seed = 1)
  results2 <- dnapath(expr_pair, pathway_list, groups,
                  network_inference, n_perm, lp, seed = 1)
  expect_equal(unclass(results1), unclass(results2))
  
  # Check that methods work on 'dnapath' object
  results <- dnapath(expr_pair, pathway_list, groups,
                 network_inference, n_perm, lp)
  expect_is(results, "dnapath")
  expect_is(summary(results), "tbl")
  expect_is(summarize_genes(results), "tbl")
  expect_is(summarize_pathways(results), "tbl")
  # expect_invisible(plot(results))
  
  # Check that methods work on 'dnapath_list' object.
  # Create pathway list.
  pathway_list <- list(as.character(5:9), 
                       as.character(20),
                       as.character(c(1, 5, 8, 13, 19)), 
                       as.character(c(2, 3, 4, 6:20, 10001)))
  results <- dnapath(expr_pair, pathway_list, groups,
                     network_inference, n_perm, lp)
  expect_true(all(unname(dnapath:::get_mean_expr_mat(results[[1]])[1, c(1, 5)]) == c(0, 0)))
  expect_true(unname(dnapath:::get_mean_expr_mat(results[[1]])[2, c(5)]) == 0)
  expect_is(results, "dnapath_list")
  expect_is(summary(results), "tbl")
  expect_is(summarize_genes(results), "tbl")
  expect_is(summarize_pathways(results), "tbl")
  
  # Check that gene names are unchanged after subsetting.
  genes_before_subset <- results$param$gene_names[results$pathway[[1]]$genes]
  results <- results[c(1, 3)]
  genes_after_subset <- results$param$gene_names[results$pathway[[1]]$genes]
  expect_equal(genes_before_subset, genes_after_subset)
  expect_equal(genes_before_subset, c("5", "6", "7", "8", "9"))
  # Pathway 2 is omitted from the results because it contains only 1 gene.
  expect_equal(sort(get_genes(results[[2]])), 
               sort(unique(as.character(pathway_list[[4]][-19]))))
  
  # Check that methods work on 'dnapath_list' object
  # Now use a named pathway_list.
  pathway_list <- list("a" = 16:49, 
                       "b" = c(1, 5, 5, 5, 5, 5, 8, 13, 19), 
                       "c" = c(2, 3, 4, 6:20))
  results <- dnapath(expr_pair, pathway_list, groups,
                 network_inference, n_perm, lp)
  expect_is(results, "dnapath_list")
  expect_equal(names(results), names(pathway_list))
  
  expect_is(summary(results), "tbl")
  expect_is(summarize_genes(results), "tbl")
  expect_is(summarize_pathways(results), "tbl")
  # expect_invisible(plot(results[[1]]))
  
  # Check that gene names are unchanged after subsetting.
  genes_before_subset <- results$param$gene_names[results$pathway[[3]]$genes]
  results <- results[c(1, 3)]
  genes_after_subset <- results$param$gene_names[results$pathway[[2]]$genes]
  expect_equal(genes_before_subset, genes_after_subset)
  # Pathway 2 is omitted from the results because it contains only 1 gene.
  expect_equal(sort(results$param$gene_names[results$pathway[[2]]$genes]), 
               sort(unique(as.character(pathway_list[[3]]))))
})


test_that("dnapath method works on data.frame input and probabilistic group labels", {
  n <- 30
  group_prob <- round(rbeta(n, 1, 1), 3)
  group_prob <- cbind(group_prob, 1 - group_prob)
  colnames(group_prob) <- c("A", "B")
  p <- 100
  expr_pair <- matrix(rnorm(p * n, 10, 1), ncol = p)
  colnames(expr_pair) <- 1:p
  
  # Delete some of the genes in each group
  expr_pair[group_prob[, 1] > 0.5, c(1, 5, 9)] <- 0
  expr_pair[group_prob[, 1] <= 0.5, c(9, 10, 23, 90:p)] <- 0
  
  network_inference <- function(x, weights, ...) suppressWarnings(run_corr(x, weights, ...))
  n_perm = 10
  lp = 2
  
  pathway_list <- as.character(1:20)
  # Check that seed can be set without error and returns equivalent results.
  results1 <- dnapath(expr_pair, pathway_list, group_prob,
                      network_inference, n_perm, lp, seed = 1)
  results2 <- dnapath(expr_pair, pathway_list, group_prob,
                      network_inference, n_perm, lp, seed = 1)
  expect_equal(unclass(results1), unclass(results2))
  
  # Check that methods work on 'dnapath' object
  results <- dnapath(expr_pair, pathway_list, group_prob,
                     network_inference, n_perm, lp)
  expect_is(results, "dnapath")
  expect_is(summary(results), "tbl")
  expect_is(summarize_genes(results), "tbl")
  expect_is(summarize_pathways(results), "tbl")
  # expect_invisible(plot(results))
  
  # Check that methods work on 'dnapath_list' object.
  # Create pathway list.
  pathway_list <- list(as.character(5:9), 
                       as.character(20),
                       as.character(c(1, 5, 8, 13, 19)), 
                       as.character(c(2, 3, 4, 6:20, 10001)))
  results <- dnapath(expr_pair, pathway_list, group_prob,
                     network_inference, n_perm, lp)
  expect_true(unname(dnapath:::get_mean_expr_mat(results[[1]])[1, 1]) < 5)
  expect_true(unname(dnapath:::get_mean_expr_mat(results[[1]])[1, 5]) == 0)
  expect_true(unname(dnapath:::get_mean_expr_mat(results[[1]])[2, 5]) == 0)
  expect_is(results, "dnapath_list")
  expect_is(summary(results), "tbl")
  expect_is(summarize_genes(results), "tbl")
  expect_is(summarize_pathways(results), "tbl")
  
  # Check that gene names are unchanged after subsetting.
  genes_before_subset <- results$param$gene_names[results$pathway[[1]]$genes]
  results <- results[c(1, 3)]
  genes_after_subset <- results$param$gene_names[results$pathway[[1]]$genes]
  expect_equal(genes_before_subset, genes_after_subset)
  expect_equal(genes_before_subset, c("5", "6", "7", "8", "9"))
  # Pathway 2 is omitted from the results because it contains only 1 gene.
  expect_equal(sort(get_genes(results[[2]])), 
               sort(unique(as.character(pathway_list[[4]][-19]))))
  
  # Check that methods work on 'dnapath_list' object
  # Now use a named pathway_list.
  pathway_list <- list("a" = 16:49, 
                       "b" = c(1, 5, 5, 5, 5, 5, 8, 13, 19), 
                       "c" = c(2, 3, 4, 6:20))
  results <- dnapath(expr_pair, pathway_list, group_prob,
                     network_inference, n_perm, lp)
  expect_is(results, "dnapath_list")
  expect_equal(names(results), names(pathway_list))
  
  expect_is(summary(results), "tbl")
  expect_is(summarize_genes(results), "tbl")
  expect_is(summarize_pathways(results), "tbl")
  # expect_invisible(plot(results[[1]]))
  
  # Check that gene names are unchanged after subsetting.
  genes_before_subset <- results$param$gene_names[results$pathway[[3]]$genes]
  results <- results[c(1, 3)]
  genes_after_subset <- results$param$gene_names[results$pathway[[2]]$genes]
  expect_equal(genes_before_subset, genes_after_subset)
  # Pathway 2 is omitted from the results because it contains only 1 gene.
  expect_equal(sort(results$param$gene_names[results$pathway[[2]]$genes]), 
               sort(unique(as.character(pathway_list[[3]]))))
})


test_that("dnapath method works when lp is a vector", {
  n <- c(10, 15)
  p <- 100
  expr_pair <- matrix(rnorm(p * (n[1] + n[2]), 10), ncol = p)
  colnames(expr_pair) <- 1:p
  groups <- c('A', 'A', 'B', 'B', 'A', 'B', 'B', 'B', 'B', 'A', 'A', 'A', 
              'B', 'B', 'B', 'B', 'B', 'A', 'B', 'A', 'A', 'B', 'A', 'B', 'B')
  
  # Delete some of the genes in each group
  expr_pair[groups == "A", c(1, 5, 9)] <- 0
  expr_pair[groups == "B", c(9, 10, 23, 90:p)] <- 0
  
  network_inference <- function(x, ...) suppressWarnings(run_corr(x, ...))
  n_perm = 10
  lp = c(0.5, 1, 2, Inf)
  
  pathway_list <- as.character(1:20)
  # Check that seed can be set without error and returns equivalent results.
  results1 <- dnapath(expr_pair, pathway_list, groups,
                  network_inference, n_perm, lp, seed = 1)
  results2 <- dnapath(expr_pair, pathway_list, groups,
                  network_inference, n_perm, lp, seed = 1)
  expect_equal(unclass(results1[[1]]), unclass(results2[[1]]))
  
  # Check that methods work on 'dnapath' object
  results <- dnapath(expr_pair, pathway_list, groups,
                 network_inference, n_perm, lp)
  expect_is(results[[1]], "dnapath")
  expect_is(summary(results[[2]]), "tbl")
  expect_is(summarize_genes(results[[3]]), "tbl")
  expect_is(summarize_pathways(results[[4]]), "tbl")
  
  # Check that methods work on 'dnapath_list' object
  # Now use a named pathway_list.
  pathway_list <- list("a" = 16:49, 
                       "b" = c(1, 5, 5, 5, 5, 5, 8, 13, 19), 
                       "c" = c(2, 3, 4, 6:20, 1001))
  results <- dnapath(expr_pair, pathway_list, groups,
                 network_inference, n_perm, lp)
  expect_is(results[[2]], "dnapath_list")
  expect_equal(names(results[[1]]), names(pathway_list))
  
  # Note the ordering of results[[1]][[1]]$genes is affect by the missing
  # columns in each dataset.
  expect_equal(max(summarize_genes(results[[1]])$n_pathways), 3)
  
  expect_is(summary(results[[4]]), "tbl")
  expect_is(summarize_genes(results[[3]]), "tbl")
  expect_is(summarize_pathways(results[[2]]), "tbl")
  # expect_invisible(plot(results[[1]][[1]]))
})



test_that("dnapath method gives warnings and errors.", {
  # Check for various warnings and errors.
  n <- c(3, 3)
  n_perm <- 9
  p <- 20
  lp <- 1
  network_inference <- run_corr
  expr_pair <- rbind(matrix(rnorm(p * n[1], 10), nrow = n[1], ncol = p),
                     matrix(rnorm(p * n[2], 10), nrow = n[2], ncol = p))
  colnames(expr_pair) <- 1:p
  pathway_list <- as.character(5:15)
  groups <- rep(0:1, n)
  expect_warning(dnapath(expr_pair, pathway_list, groups,
                     network_inference, n_perm = 100, lp))
  expect_error(dnapath(expr_pair, pathway_list, groups = NULL,
                   network_inference, n_perm, lp))
  expect_error(dnapath(expr_pair, pathway_list, groups = 1,
                   network_inference, n_perm, lp))
  expect_error(dnapath(expr_pair, pathway_list, groups = rep(1, sum(n)),
                   network_inference, n_perm, lp))
})



test_that("rename_genes function works", {
  n <- c(10, 15)
  p <- 20
  x <- matrix(rnorm(p * (n[1] + n[2]), 10), ncol = p)
  colnames(x) <- 1:p
  groups <- c('A', 'A', 'B', 'B', 'A', 'B', 'B', 'B', 'B', 'A', 'A', 'A', 
              'B', 'B', 'B', 'B', 'B', 'A', 'B', 'A', 'A', 'B', 'A', 'B', 'B')

  pathway_list <- list(as.character(5:9), 
                       as.character(c(1, 5, 8, 13, 19)), 
                       as.character(c(2, 3, 4, 6:20)))
  
  results <- dnapath(x, pathway_list, groups,
                     network_inference = run_corr)
  
  gene_mat <- data.frame(genes = c(0, 1, 2, 3, 4, 5), 
                         new_name = c("NA", "A", "B", "C", "D", "E"))
  
  # Check renaming on pathway list and single pathway
  pathway_list2 <- rename_genes(pathway_list, gene_mat)
  expect_equal(pathway_list2[[1]], c("E", "6", "7", "8", "9"))
  pathway2 <- rename_genes(pathway_list[[2]], gene_mat)
  expect_equal(pathway2, c("A", "E", "8", "13", "19"))
  
  results1 <- rename_genes(results, gene_mat)
  expect_equal(results1$param$gene_names[results1$pathway[[1]]$genes], 
               c("E", "6", "7", "8", "9"))
  results2 <- rename_genes(results[[2]], gene_mat)
  expect_equal(get_genes(results2),
               c("A", "E", "8", "13", "19"))
  
  
  n <- c(10, 15)
  p <- 100
  x <- matrix(rnorm(p * (n[1] + n[2]), 10), ncol = p)
  colnames(x) <- p:1
  groups <- c('A', 'A', 'B', 'B', 'A', 'B', 'B', 'B', 'B', 'A', 'A', 'A', 
              'B', 'B', 'B', 'B', 'B', 'A', 'B', 'A', 'A', 'B', 'A', 'B', 'B')
  pathway_list <- list(as.character(5:9), 
                       as.character(c(1, 5, 8, 130, 190)), 
                       as.character(c(2, 3, 4, 6:20)))
  results <- dnapath(x, pathway_list, groups,
                     network_inference = run_corr)
  expect_equal(get_genes(results[[2]]), c("8", "5", "1"))
  gene_mat <- data.frame(genes = c(0, 1, 2, 3, 4, 5), 
                         new_name = c("NA", "A", "B", "C", "D", "E"))
  results2 <- rename_genes(results[[2]], gene_mat)
  expect_is(results2, "dnapath")
  expect_equal(get_genes(results2), rev(c("A", "E", "8")))
})
