make_plan_with_fasta <- function() {
  dir <- tempfile("fasta")
  dir.create(dir)
  file_a <- file.path(dir, "a.fasta")
  file_b <- file.path(dir, "b.fasta")
  file_c <- file.path(dir, "c.fasta")

  writeLines(c(">gene1 sample_a", "ACGTACGT", ">gene2 sample_a", "TTTT"), file_a)
  writeLines(c(">gene1 sample_b", "ACGTTCGT", ">gene2 sample_b", "TTTA"), file_b)
  writeLines(c(">gene1 sample_c", "ACGGACGT", ">gene3 sample_c", "CCCC"), file_c)

  data.frame(
    sequence_id = c(1L, 2L, 3L),
    scientific_name = c("Species a", "Species b", "Species c"),
    order = "Order",
    family = "Family",
    genus = "Species",
    specific_epithet = c("a", "b", "c"),
    fasta_file_name = c("a.fasta", "b.fasta", "c.fasta"),
    local_path = normalizePath(c(file_a, file_b, file_c), winslash = "/", mustWork = TRUE),
    status = "ok",
    stringsAsFactors = FALSE
  )
}

test_that("FASTA long table and summary are tidy", {
  plan <- make_plan_with_fasta()

  long <- tol_fasta_long(plan)
  summary <- tol_fasta_summary(plan)

  expect_s3_class(long, "tbl_df")
  expect_equal(nrow(long), 6)
  expect_true(all(c("sequence_id", "gene_id", "sequence", "width") %in% names(long)))
  expect_equal(nrow(summary), 3)
  expect_true(all(summary$n_sequences == 2))
})

test_that("common genes are ranked by shared records", {
  plan <- make_plan_with_fasta()

  genes <- tol_common_genes(plan, min_records = 2)

  expect_equal(genes$gene_id[1], "gene1")
  expect_equal(genes$n_records[1], 3)
})

test_that("gene tree builds from a shared gene", {
  plan <- make_plan_with_fasta()

  tree <- tol_build_gene_tree(plan, gene_id = "gene1", min_records = 3)

  expect_s3_class(tree$tree, "hclust")
  expect_equal(tree$gene_id, "gene1")
  expect_equal(nrow(tree$sequences), 3)
})

test_that("ggplot helpers return plot objects", {
  plan <- make_plan_with_fasta()
  summary <- tol_fasta_summary(plan)
  records <- data.frame(
    scientific_name = c("Species a", "Species b"),
    no_of_genes_recovered = c(2L, 3L)
  )

  expect_s3_class(tol_plot_gene_recovery(records), "ggplot")
  expect_s3_class(tol_plot_fasta_summary(summary), "ggplot")
  expect_s3_class(tol_plot_tree(tol_build_gene_tree(plan, gene_id = "gene1")), "ggplot")
  expect_s3_class(tol_plot_tree(tol_build_gene_tree(plan, gene_id = "gene1"), label_offset = 0.05), "ggplot")
})
