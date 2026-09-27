local_edition(3)

# Helper: Create Dummy Data
setup_ele_data <- function() {
 n <- 100
 data.frame(
  id = 1:n,
  # Create 4 blocks of size 25
  block_id = rep(1:4, each = 25),
  # Create inconsistent block ID for testing
  val = runif(n)
 )
}

test_that("adjELE: Basic valid construction works", {
 df <- setup_ele_data()

 # Case 1: Global Rate, No Blocks (defaults to single block)
 adj1 <- adjELE(linked.data = df, m.rate = 0.1)
 expect_s3_class(adj1, "adjELE")
 expect_equal(adj1$weight.matrix, "ratio") # Default check
 expect_equal(length(adj1$blocks), 100)
 expect_true(all(adj1$blocks == 1)) # Default blocks are 1s

 # Case 2: Block-specific rates
 # 4 unique blocks, so we provide 4 rates
 rates <- c(0.05, 0.1, 0.15, 0.2)
 adj2 <- adjELE(linked.data = df,
                m.rate = rates,
                blocks = df$block_id,
                weight.matrix = "LL")

 expect_equal(adj2$m.rate, rates)
 expect_equal(adj2$blocks, df$block_id)
 expect_equal(adj2$weight.matrix, "LL")
})

test_that("adjELE: NSE for blocks works", {
 df <- setup_ele_data()

 # Pass 'block_id' as a bare name (column in df)
 adj <- adjELE(linked.data = df,
               m.rate = 0.1,
               blocks = block_id)

 expect_equal(adj$blocks, df$block_id)

 # Pass external vector
 ext_blocks <- rep(1:2, each = 50)
 adj_ext <- adjELE(linked.data = df,
                   m.rate = c(0.1, 0.2),
                   blocks = ext_blocks)
 expect_equal(adj_ext$blocks, ext_blocks)
})

test_that("adjELE: Reference semantics are preserved", {
 df <- setup_ele_data()
 adj <- adjELE(linked.data = df, m.rate = 0.1)

 expect_type(adj$data_ref, "environment")
 expect_identical(adj$data_ref$data, df)
})

test_that("adjELE: m.rate validation", {
 df <- setup_ele_data()

 # Missing
 expect_error(adjELE(df), "'m.rate' must be specified")

 # Non-numeric
 expect_error(adjELE(df, m.rate = "0.1"), "must be a numeric vector")

 # Out of bounds
 expect_error(adjELE(df, m.rate = 1.5), "between 0 and 1")
 expect_error(adjELE(df, m.rate = -0.01), "between 0 and 1")

 # Dimension mismatch (Global vs Block vs Record)
 # df has 4 blocks. Providing 3 rates should fail.
 expect_error(adjELE(df, m.rate = c(0.1, 0.2, 0.3), blocks = block_id),
              "Length of 'm.rate' \\(3\\) must match")
})

test_that("adjELE: m.rate consistency check (Rate per Record)", {
 df <- setup_ele_data()

 # Valid: Every record gets the rate corresponding to its block
 # Block 1 -> 0.1, Block 2 -> 0.2, etc.
 valid_rates <- rep(c(0.1, 0.2, 0.3, 0.4), each = 25)
 adj <- adjELE(df, m.rate = valid_rates, blocks = block_id)
 expect_s3_class(adj, "adjELE")

 # Invalid: One record in Block 1 has a different rate
 bad_rates <- valid_rates
 bad_rates[1] <- 0.99 # Record 1 (Block 1) is 0.99, Record 2 (Block 1) is 0.1

 expect_error(adjELE(df, m.rate = bad_rates, blocks = block_id),
              "values must be constant within each block")
})

test_that("adjELE: audit.size validation and consistency", {
 df <- setup_ele_data()

 # Non-numeric
 expect_error(adjELE(df, m.rate = 0.1, audit.size = "50"),
              "must be a numeric vector")

 # Dimension mismatch
 expect_error(adjELE(df, m.rate = 0.1, blocks = block_id, audit.size = c(10, 20)),
              "Length of 'audit.size' \\(2\\) must match")

 # Consistency Check (Per Record)
 # Valid:
 valid_audit <- rep(c(10, 20, 30, 40), each = 25)
 adj <- adjELE(df, m.rate = 0.1, blocks = block_id, audit.size = valid_audit)
 expect_equal(adj$audit.size, valid_audit)

 # Invalid:
 bad_audit <- valid_audit
 bad_audit[1] <- 999
 expect_error(adjELE(df, m.rate = 0.1, blocks = block_id, audit.size = bad_audit),
              "values must be constant within each block")
})

test_that("adjELE: weight.matrix argument matching", {
 df <- setup_ele_data()

 # Default
 adj <- adjELE(df, m.rate = 0.1)
 expect_equal(adj$weight.matrix, "ratio")

 # Explicit valid
 adj_blue <- adjELE(df, m.rate = 0.1, weight.matrix = "BLUE")
 expect_equal(adj_blue$weight.matrix, "BLUE")

 # Invalid
 expect_error(adjELE(df, m.rate = 0.1, weight.matrix = "invalid_method"),
              "should be one of")
})

test_that("adjELE: linked.data validation", {
 # Missing
 expect_error(adjELE(m.rate = 0.1), "'linked.data' must be provided")

 # Wrong type
 expect_error(adjELE(linked.data = "string", m.rate = 0.1),
              "must be a data.frame")
})
