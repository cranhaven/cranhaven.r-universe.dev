context("Testing utr5 mrl prediction module")
skip_if(Sys.info()[["sysname"]] == "Darwin" && str_detect(Sys.info()[["version"]], "RELEASE_ARM"))
skip_on_cran()
skip_on_ci()

# Test predictMRL ---------------------------------------------------------

test_that("Test predictMRL will get 100nt of 5' UTR sequences preceding the start codon, will pad the sequence with N if it is less than 100nt, then encode the sequence, and predict the MRL", {
  variants <- data.frame(Chr = c(1,2), Pos = c(10,10), Ref = c("C", "G"), Alt = c("A", "T"), utr5_transcript_id = c("tran1", "tran2"))
  seq_table <- data.table(ensembl_transcript_id = c("tran1", "tran2"), seq = c(paste0(rep("A", 50), collapse = ""), paste0(rep("G", 100), collapse = "")))
  utr5_100_mrl <- predictMRL(variants, seq_table, "utr5_transcript_id")
  expect_equal(length(utr5_100_mrl), 2)
  k_clear_session()
  # compare the predictMRL output with the output from a step by step process
  model <- load_model_hdf5(system.file("extdata", "Varying_length_25to100_model.hdf5", package = "MRL.dl.model", mustWork = TRUE))
  utr5_seq_1 <- one_hot_encode(str_c(paste0(rep("N", 50), collapse = ""), seq_table[ensembl_transcript_id == "tran1", seq]))
  utr5_seq_1_mrl <- inverse_transform(predict(model, utr5_seq_1))
  utr5_seq_2 <- one_hot_encode(seq_table[ensembl_transcript_id == "tran2", seq])
  utr5_seq_2_mrl <- inverse_transform(predict(model, utr5_seq_2))
  expect_true(utr5_100_mrl[1] == utr5_seq_1_mrl)
  expect_true(utr5_100_mrl[2] == utr5_seq_2_mrl)
})

test_that("Test predictMRL will work on a variant with multiple trans", {
  variants <- data.frame(Chr = c(1), Pos = c(10), Ref = c("C"), Alt = c("A"), utr5_transcript_id = c("tran1;tran2"))
  seq_table <- data.table(ensembl_transcript_id = c("tran1", "tran2"), seq = c(paste0(rep("A", 50), collapse = ""), paste0(rep("A", 100), collapse = "")))
  utr5_100_mrl <- predictMRL(variants, seq_table, "utr5_transcript_id")
  expect_equal(length(utr5_100_mrl), 1)
  k_clear_session()
  # compare the predictMRL output with the output from a step by step process
  model <- load_model_hdf5(system.file("extdata", "Varying_length_25to100_model.hdf5", package = "MRL.dl.model", mustWork = TRUE))
  utr5_seq_1 <- one_hot_encode(str_c(paste0(rep("N", 50), collapse = ""), seq_table[ensembl_transcript_id == "tran1", seq]))
  utr5_seq_1_mrl <- inverse_transform(predict(model, utr5_seq_1))
  utr5_seq_2 <- one_hot_encode(seq_table[ensembl_transcript_id == "tran2", seq])
  utr5_seq_2_mrl <- inverse_transform(predict(model, utr5_seq_2))
  expect_true(utr5_100_mrl[1] == paste0(c(utr5_seq_1_mrl, utr5_seq_2_mrl), collapse = ";"))
})

test_that("Test predictMRL will return NA if transcript id is NA, or no sequence in the seq table for that transcript", {
  variants <- data.frame(Chr = c(1,2,3,4), Pos = c(10,10,10,10), Ref = c("C", "G", "A","A"), Alt = c("A", "T", "C", "C"), utr5_transcript_id = c("tran1", "NA", "tran3;tran4;tran5", NA))
  seq_table <- data.table(ensembl_transcript_id = c("tran2"), seq = c(paste0(rep("G", 100), collapse = "")))
  utr5_100 <- predictMRL(variants, seq_table, "utr5_transcript_id")
  expect_equal(length(utr5_100), 4)
  expect_true(is.na(utr5_100[1]))
  expect_true(is.na(utr5_100[2]))
  expect_true(is.na(utr5_100[3]))
  expect_true(is.na(utr5_100[4]))
})

test_that("Test predictMRL will return NA if the sequence is unavailable in the seq table for that transcript", {
  variants <- data.frame(Chr = c(1), Pos = c(10), Ref = c("C"), Alt = c("A"), utr5_transcript_id = c("tran1"))
  seq_table <- data.table(ensembl_transcript_id = c("tran1"), seq = c("Sequence unavailable"))
  utr5_100 <- predictMRL(variants, seq_table, "utr5_transcript_id")
  expect_equal(length(utr5_100), 1)
  expect_true(is.na(utr5_100[1]))
})


test_that("Test predictMRL will be able to concatenate NAs and numbers when there're multiple transcripts for a variant and some of them have no sequences", {
  variants <- data.frame(Chr = c(1,2), Pos = c(10,10), Ref = c("C", "G"), Alt = c("A", "T"), utr5_transcript_id = c("tran1;tran2", "NA"))
  seq_table <- data.table(ensembl_transcript_id = c("tran2"), seq = c(paste0(rep("G", 100), collapse = "")))
  utr5_100 <- predictMRL(variants, seq_table, "utr5_transcript_id")
  expect_equal(length(utr5_100), 2)
  k_clear_session()
  model <- load_model_hdf5(system.file("extdata", "Varying_length_25to100_model.hdf5", package = "MRL.dl.model", mustWork = TRUE))
  utr5_seq_2 <- one_hot_encode(seq_table[ensembl_transcript_id == "tran2", seq])
  utr5_seq_2_mrl <- inverse_transform(predict(model, utr5_seq_2))
  expect_equal(utr5_100[1], paste0(c("NA", utr5_seq_2_mrl), collapse = ";"))
  expect_true(is.na(utr5_100[2]))
})


# Test predictMRLInAlt ----------------------------------------------------


test_that("Test predictMRLInAlt will get mutated 100nt of 5' UTR sequences preceding the start codon, will pad the sequence with N if it is less than 100nt, then encode the sequence, and predict the MRL", {
  variants <- data.frame(Chr = c(1,2), Pos = c(10,10), Ref = c("A", "G"), Alt = c("C", "T"), utr5_transcript_id = c("tran1", "tran2"))
  seq_table <- data.table(ensembl_transcript_id = c("tran1", "tran2"), seq = c(paste0(rep("A", 50), collapse = ""), paste0(rep("G", 100), collapse = "")))
  transcript_table <- data.table(ensembl_transcript_id = c("tran1", "tran2"), start = c(1, 5), end = c(50, 104), strand = c(1, 1))
  utr5_100_mrl <- predictMRLInAlt(variants, seq_table, transcript_table, "utr5_transcript_id")
  expect_equal(length(utr5_100_mrl), 2)
  k_clear_session()
  model <- load_model_hdf5(system.file("extdata", "Varying_length_25to100_model.hdf5", package = "MRL.dl.model", mustWork = TRUE))
  utr5_seq_1 <- one_hot_encode(str_c(paste0(rep("N", 50), collapse = ""), str_sub(seq_table[ensembl_transcript_id == "tran1", seq], 1, 9), "C", str_sub(seq_table[ensembl_transcript_id == "tran1", seq], 11)))
  utr5_seq_1_mrl <- inverse_transform(predict(model, utr5_seq_1))
  utr5_seq_2 <- one_hot_encode(str_c(str_c(str_sub(seq_table[ensembl_transcript_id == "tran2", seq], 1, 5), "T", str_sub(seq_table[ensembl_transcript_id == "tran2", seq], 7))))
  utr5_seq_2_mrl <- inverse_transform(predict(model, utr5_seq_2))
  expect_true(utr5_100_mrl[1] == utr5_seq_1_mrl)
  expect_true(utr5_100_mrl[2] == utr5_seq_2_mrl)
})

test_that("Test predictMRLInAlt will get MRL of mutated 100nt of 5' UTR sequences preceding the start codon for multiple trans", {
  variants <- data.frame(Chr = c(1), Pos = c(10), Ref = c("A"), Alt = c("C"), utr5_transcript_id = c("tran1;tran2"))
  seq_table <- data.table(ensembl_transcript_id = c("tran1", "tran2"), seq = c(paste0(rep("A", 50), collapse = ""), paste0(rep("A", 100), collapse = "")))
  transcript_table <- data.table(ensembl_transcript_id = c("tran1", "tran2"), start = c(1, 5), end = c(50, 104), strand = c(1, 1))
  utr5_100_mrl <- predictMRLInAlt(variants, seq_table, transcript_table, "utr5_transcript_id")
  expect_equal(length(utr5_100_mrl), 1)
  k_clear_session()
  model <- load_model_hdf5(system.file("extdata", "Varying_length_25to100_model.hdf5", package = "MRL.dl.model", mustWork = TRUE))
  utr5_seq_1 <- one_hot_encode(str_c(paste0(rep("N", 50), collapse = ""), str_sub(seq_table[ensembl_transcript_id == "tran1", seq], 1, 9), "C", str_sub(seq_table[ensembl_transcript_id == "tran1", seq], 11)))
  utr5_seq_1_mrl <- inverse_transform(predict(model, utr5_seq_1))
  utr5_seq_2 <- one_hot_encode(str_c(str_c(str_sub(seq_table[ensembl_transcript_id == "tran2", seq], 1, 5), "C", str_sub(seq_table[ensembl_transcript_id == "tran2", seq], 7))))
  utr5_seq_2_mrl <- inverse_transform(predict(model, utr5_seq_2))
  expect_true(utr5_100_mrl[1] == paste0(c(utr5_seq_1_mrl, utr5_seq_2_mrl), collapse = ";"))
})

test_that("Test predictMRLInAlt works on minus strand", {
  variants <- data.frame(Chr = c(1,2), Pos = c(10,10), Ref = c("T", "C"), Alt = c("G", "A"), utr5_transcript_id = c("tran1", "tran2"))
  seq_table <- data.table(ensembl_transcript_id = c("tran1", "tran2"), seq = c(paste0(rep("A", 50), collapse = ""), paste0(rep("G", 100), collapse = "")))
  transcript_table <- data.table(ensembl_transcript_id = c("tran1", "tran2"), start = c(1, 5), end = c(50, 104), strand = c(-1, -1))
  utr5_100_mrl <- predictMRLInAlt(variants, seq_table, transcript_table, "utr5_transcript_id")
  expect_equal(length(utr5_100_mrl), 2)
  model <- load_model_hdf5(system.file("extdata", "Varying_length_25to100_model.hdf5", package = "MRL.dl.model", mustWork = TRUE))
  utr5_seq_1 <- one_hot_encode(str_c(paste0(rep("N", 50), collapse = ""), str_sub(seq_table[ensembl_transcript_id == "tran1", seq], 1, 40), "C", str_sub(seq_table[ensembl_transcript_id == "tran1", seq], 42)))
  utr5_seq_1_mrl <- inverse_transform(predict(model, utr5_seq_1))
  utr5_seq_2 <- one_hot_encode(str_c(str_sub(seq_table[ensembl_transcript_id == "tran2", seq], 1, 94), "T", str_sub(seq_table[ensembl_transcript_id == "tran2", seq], 96)))
  utr5_seq_2_mrl <- inverse_transform(predict(model, utr5_seq_2))
  expect_true(utr5_100_mrl[1] == utr5_seq_1_mrl)
  expect_true(utr5_100_mrl[2] == utr5_seq_2_mrl)
})


test_that("Test predictMRLInAlt will return NA if transcript id is NA, or no sequence in the seq table for that transcript", {
  variants <- data.frame(Chr = c(1,2,3), Pos = c(10,10,10), Ref = c("C", "G", "A"), Alt = c("A", "T", "C"), utr5_transcript_id = c("tran1", NA, "tran3;tran4;tran5"))
  seq_table <- data.table(ensembl_transcript_id = c("tran2"), seq = c(paste0(rep("G", 100), collapse = "")))
  transcript_table <- data.table(ensembl_transcript_id = c("tran1", "tran2"), start = c(1, 5), end = c(50, 104), strand = c(1, 1))
  utr5_100 <- predictMRLInAlt(variants, seq_table, transcript_table, "utr5_transcript_id")
  expect_equal(length(utr5_100), 3)
  expect_true(is.na(utr5_100[1]))
  expect_true(is.na(utr5_100[2]))
  expect_true(is.na(utr5_100[3]))
})

test_that("Test predictMRLInAlt will return NA if the sequence is unavailable in the seq table for that transcript", {
  variants <- data.frame(Chr = c(1), Pos = c(10), Ref = c("C"), Alt = c("A"), utr5_transcript_id = c("tran1"))
  seq_table <- data.table(ensembl_transcript_id = c("tran1"), seq = c("Sequence unavailable"))
  transcript_table <- data.table(ensembl_transcript_id = c("tran1", "tran2"), start = c(1, 5), end = c(50, 104), strand = c(1, 1))
  utr5_100 <- predictMRLInAlt(variants, seq_table, transcript_table, "utr5_transcript_id")
  expect_equal(length(utr5_100), 1)
  expect_true(is.na(utr5_100[1]))
})

test_that("Test predictMRLInAlt will be able to concatenate NAs and numbers when there're multiple transcripts for a variant and some of them have no sequences", {
  variants <- data.frame(Chr = c(1,2), Pos = c(10,10), Ref = c("G", "G"), Alt = c("C", "T"), utr5_transcript_id = c("tran1;tran2", NA))
  seq_table <- data.table(ensembl_transcript_id = c("tran2"), seq = c(paste0(rep("G", 100), collapse = "")))
  transcript_table <- data.table(ensembl_transcript_id = c("tran1", "tran2"), start = c(1, 5), end = c(50, 104), strand = c(1, 1))
  utr5_100 <- predictMRLInAlt(variants, seq_table, transcript_table, "utr5_transcript_id")
  expect_equal(length(utr5_100), 2)
  k_clear_session()
  model <- load_model_hdf5(system.file("extdata", "Varying_length_25to100_model.hdf5", package = "MRL.dl.model", mustWork = TRUE))
  utr5_seq_2 <- one_hot_encode(str_c(str_sub(seq_table[ensembl_transcript_id == "tran2", seq], 1, 5), "C", str_sub(seq_table[ensembl_transcript_id == "tran2", seq], 7)))
  utr5_seq_2_mrl <- inverse_transform(predict(model, utr5_seq_2))
  expect_equal(utr5_100[1], paste0(c("NA", utr5_seq_2_mrl), collapse = ";"))
  expect_true(is.na(utr5_100[2]))
})


# Test one_hot_encode -----------------------------------------------------


test_that("Test one_hot_encode can create a three dims array (num_seqs, 100, 4)",  {
  # All A
  dna <- paste0(rep("A", 100), collapse = "")
  encoded <- one_hot_encode(dna)
  expect_equal(dim(encoded), c(1, 100, 4))
  for (i in 1:100) {
    expect_true(all(encoded[1, i, ] == c(1,0,0,0)))
  }
  # All C
  dna <- paste0(rep("C", 100), collapse = "")
  encoded <- one_hot_encode(dna)
  expect_equal(dim(encoded), c(1, 100, 4))
  for (i in 1:100) {
    expect_true(all(encoded[1, i, ] == c(0,1,0,0)))
  }
  # All G
  dna <- paste0(rep("G", 100), collapse = "")
  encoded <- one_hot_encode(dna)
  expect_equal(dim(encoded), c(1, 100, 4))
  for (i in 1:100) {
    expect_true(all(encoded[1, i, ] == c(0,0,1,0)))
  }
  # All T
  dna <- paste0(rep("T", 100), collapse = "")
  encoded <- one_hot_encode(dna)
  expect_equal(dim(encoded), c(1, 100, 4))
  for (i in 1:100) {
    expect_true(all(encoded[1, i, ] == c(0,0,0,1)))
  }
  # All N
  dna <- paste0(rep("N", 100), collapse = "")
  encoded <- one_hot_encode(dna)
  expect_equal(dim(encoded), c(1, 100, 4))
  for (i in 1:100) {
    expect_true(all(encoded[1, i, ] == c(0,0,0,0)))
  }
  # Try a mix
  dna <- str_c(paste0(rep("N", 60), collapse = ""), paste0(rep("C", 10), collapse = ""), paste0(rep("A", 10), collapse = ""), paste0(rep("T", 10), collapse = ""), paste0(rep("G", 10), collapse = ""))
  encoded <- one_hot_encode(dna)
  expect_equal(dim(encoded), c(1, 100, 4))
  for (i in 1:60) {
    expect_true(all(encoded[1, i, ] == c(0,0,0,0)))
  }
  for (i in 61:70) {
    expect_true(all(encoded[1, i, ] == c(0,1,0,0)))
  }
  for (i in 71:80) {
    expect_true(all(encoded[1, i, ] == c(1,0,0,0)))
  }
  for (i in 81:90) {
    expect_true(all(encoded[1, i, ] == c(0,0,0,1)))
  }
  for (i in 91:100) {
    expect_true(all(encoded[1, i, ] == c(0,0,1,0)))
  }
})



# Test inverse_transform --------------------------------------------------

test_that("Test inverse_transform will use default mean and std", {
  expect_equal(meanVal, 5.269942, tolerance = 0.0001)
  expect_equal(stdVal, 1.355174, tolerance = 0.0001)
})

test_that("Test inverse_transform will use default mean and std if not pass user defined ones", {
  vec <- c(1,2,4)
  nvec <- inverse_transform(vec)
  expect_equal(nvec, vec * stdVal + meanVal)
})

