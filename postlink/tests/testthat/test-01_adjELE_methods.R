local_edition(3)

# Helper
setup_ele_print_data <- function() {
 n <- 100
 data.frame(
  id = 1:n,
  # 2 blocks
  block = rep(c("A", "B"), each = 50)
 )
}

test_that("print.adjELE: Simple global case output", {
 df <- setup_ele_print_data()
 adj <- adjELE(linked.data = df, m.rate = 0.05, weight.matrix = "ratio")

 out <- capture_output(print(adj))

 expect_match(out, "Adjustment Object: Exchangeable Linkage Errors")
 expect_match(out, "Weight Matrix:\\s+ratio")
 expect_match(out, "Blocks:\\s+Single Block")
 expect_match(out, "Mismatch Rate:\\s+0.05 \\(Constant\\)")
 expect_match(out, "Audit Sample:\\s+None")
})

test_that("print.adjELE: Complex variable rate output", {
 df <- setup_ele_print_data()
 # Different rates for the 2 blocks
 rates <- rep(c(0.1, 0.2), each = 50)

 adj <- adjELE(linked.data = df,
               m.rate = rates,
               blocks = block,
               weight.matrix = "LL")

 out <- capture_output(print(adj))

 expect_match(out, "Weight Matrix:\\s+LL")
 expect_match(out, "Blocks:\\s+2 distinct blocks")
 # Check for summary stats string
 expect_match(out, "Variable \\(Mean: 0.150, Range: 0.100 - 0.200\\)")
})

test_that("print.adjELE: Audit size reporting", {
 df <- setup_ele_print_data()
 # Variable audit sizes
 audits <- rep(c(10, 20), each = 50)

 adj <- adjELE(linked.data = df,
               m.rate = 0.1,
               blocks = block,
               audit.size = audits)

 out <- capture_output(print(adj))
 expect_match(out, "Audit Sample:\\s+Varied sizes \\(Range: 10 - 20 \\)")
})

test_that("print.adjELE: Robustness to empty/broken objects", {
 # Manually construct a broken object (simulating internal error or weird state)
 adj_broken <- structure(
  list(
   data_ref = NULL, # Missing data env
   m.rate = NULL,   # Missing rates
   blocks = NULL,
   weight.matrix = NULL
  ),
  class = c("adjELE", "adjustment")
 )

 out <- capture_output(print(adj_broken))

 # Should not error
 expect_match(out, "Weight Matrix:\\s+Unknown")
 expect_match(out, "Mismatch Rate:\\s+None specified")
})
