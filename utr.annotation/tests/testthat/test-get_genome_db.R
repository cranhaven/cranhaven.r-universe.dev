context("Testing get_genome_db interface")
skip_on_cran()
skip_on_ci()


# getTranscriptIds --------------------------------------------------------
test_that("Test createEnsDbFromAH will create an EnsDb file with specified species and version", {
    edbFile <- createEnsDbFromAH("human", "93", dataDir = "sqlite_test")
    edb <- EnsDb(edbFile)
    expect_true(file.exists(file.path("sqlite_test", "human", "93", "Homo_sapiens.GRCh38.93.sqlite")))
    expect_equal(organism(edb), "Homo sapiens")
    expect_equal(ensemblVersion(edb), "93")
})

test_that("Test getTranscriptIdsOneVariant will find transcripts that overlap with the variant", {
  variant <- data.frame(Chr = "chr12", Pos = 4685084, Ref = "A", Alt = "G")
  expect_equal(getTranscriptIdsOneVariant(variant, "human", 93, "sqlite_test"), "ENST00000266544;ENST00000540688")
})

test_that("Test getTranscriptIdsOneVariant will work on Chr without chr prefix", {
  variant <- data.frame(Chr = "12", Pos = 4685084, Ref = "A", Alt = "G")
  expect_equal(getTranscriptIdsOneVariant(variant, "human", 93, "sqlite_test"), "ENST00000266544;ENST00000540688")
})

test_that("Test getTranscriptIdsOneVariant will return NA if not find transcripts that overlap with the variant", {
  variant <- data.frame(Chr = "chr1", Pos = 3747728, Ref = "T", Alt = "C")
  expect_equal(getTranscriptIdsOneVariant(variant, "human", 93, "sqlite_test"), NA)
})

test_that("Test getTranscriptIds will find transcripts that overlap with each variant in variant table", {
  variants <- data.frame(Chr = c("chr1", "chr12", "chr15", "chr15", "chr1"), Pos = c(1308643, 4685084, 45691267, 38253188, 38338861), Ref = rep("T", 5), Alt = rep("C", 5))
  expect_equal(getTranscriptIds(variants, "human", 93, "sqlite_test"), c("ENST00000379031", "ENST00000266544;ENST00000540688", "ENST00000260324;ENST00000568606", "ENST00000299084;ENST00000561317", NA))
})


# Test validateTranscripts -------------------------------

test_that("Test validateTranscripts will flag incomplete transcripts whose coding sequences start with N as TRUE in valid_startcodon", {
  ## a simplified version of transcript_regions table
  ### example transcripts: no stopcodon, has both start stop codon, no start codon, no start or stop codon, no peptide, start codon as X
  transIds <- c("ENST00000542967", "ENST00000360273", "ENST00000505657", "ENST00000431440", "ENST00000479164", "ENST00000643789")
  transOut <- validateTranscripts(transIds, "human", "102", "test_db")
  expectFlags <- data.table(ensembl_transcript_id = c("ENST00000542967", "ENST00000360273", "ENST00000505657", "ENST00000431440", "ENST00000479164", "ENST00000643789"),
                            protein_coding = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
                            valid_startcodon = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
                            valid_stopcodon = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE),
                            startcodon_M = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(transOut[order(ensembl_transcript_id)], expectFlags[order(ensembl_transcript_id)])
  unlink("test_db", recursive = T)
})

# Test queryEnsemblInfo ---------------------------------------------------

test_that("Test queryEnsemblInfo will save transcripts regions, utr sequences, and coding sequences to the specified directory", {
  ## Add Transcript column
  variants <- readVariantData("testdata/variants_sample.csv")
  variants$Transcript <- getTranscriptIds(variants, "human", "101", dataDir = "sqlite_test")
  queryEnsemblInfo(variants$Transcript, "human", "101", dataDir = "db_test")
  expect_true(file.exists(file.path("db_test", "human", "101", "transcript_regions.rds")))
  expect_true(file.exists(file.path("db_test", "human", "101", "utr3_seq.rds")))
  expect_true(file.exists(file.path("db_test", "human", "101", "utr5_seq.rds")))
  expect_true(file.exists(file.path("db_test", "human", "101", "coding_seq.rds")))
  utr5_seq <- readRDS(file.path("db_test", "human", "101", "utr5_seq.rds"))
  transcript_regions <- readRDS(file.path("db_test", "human", "101", "transcript_regions.rds"))
  expect_true(is.data.table(utr5_seq))
  expect_true(is.data.table(transcript_regions))
  unlink("db_test", recursive = T)
})

test_that("Test queryEnsemblInfo will not write any file to the specified directory if no valid transcripts in variants", {
  ## Add Transcript column
  variants <- readVariantData("testdata/variants_no_feature.csv")
  variants$Transcript <- getTranscriptIds(variants, "human", "101", dataDir = "sqlite_test")
  expect_equal(queryEnsemblInfo(variants$Transcript, "human", "101", dataDir = "db_test"), NA)
  expect_false(file.exists(file.path("db_test", "human", "101", "transcript_regions.rds")))
  expect_false(file.exists(file.path("db_test", "human", "101", "utr3_seq.rds")))
  expect_false(file.exists(file.path("db_test", "human", "101", "utr5_seq.rds")))
  expect_false(file.exists(file.path("db_test", "human", "101", "coding_seq.rds")))
  unlink("db_test", recursive = T)
})


# Test getTrasncriptsRegions ----------------------------------------------

test_that("Test getTrasncriptsRegions will get transcripts id, transcript info, 5'UTR and 3'UTR coordinates for each gene in variants table", {
  variant <- data.frame(Chr = "chr12", Pos = 4685084, Ref = "A", Alt = "G")
  variant$Transcript <- getTranscriptIds(variant, "human", 93, "sqlite_test")
  trans_regions <- getTrasncriptsRegions(variant$Transcript, "human", 93, "sqlite_test")
  expect_equal(nrow(trans_regions), 13)
  expect_equal(unique(trans_regions$ensembl_transcript_id), c("ENST00000266544","ENST00000540688"))
})

test_that("Test getTrasncriptsRegions will get transcripts id, transcript info, 5'UTR and 3'UTR coordinates for each gene in variants table, multiple rows", {
  variant <- data.frame(Chr = c("chr12", "chr17", "chr1"), Pos = c(4685084, 39065232, 1308643), Ref = c("A", "G", "CAT"), Alt = c("G", "A", "C"))
  variant$Transcript <- getTranscriptIds(variant, "human", 93, "sqlite_test")
  trans_regions <- getTrasncriptsRegions(variant$Transcript, "human", 93, "sqlite_test")
  expect_equal(unique(trans_regions$ensembl_transcript_id), c("ENST00000266544", "ENST00000315392", "ENST00000379031", "ENST00000540688"))
})


# Test getSeqTable --------------------------------------------------------

test_that("Test getSeqTable will print empty data table if cannot find given transcript ids", {
  transcriptIds <- c("a","b")
  utr5_seq <- getSeqTable(transcriptIds, '5utr', "human", 101, "test_db")
  expect_true(nrow(utr5_seq) == 0)
  unlink("db_test", recursive = T)
})

test_that("Test getSeqTable will print data table with Sequence unavailable if there's no sequence for given transcript ids", {
  transcriptIds <- c("ENST00000565956","ENST00000565353")
  utr5_seq <- getSeqTable(transcriptIds, '5utr', "human", 101, "test_db")
  expect_true(nrow(utr5_seq) == 2)
  expect_identical(utr5_seq, data.table(seq = c("Sequence unavailable", "Sequence unavailable"), ensembl_transcript_id = c("ENST00000565353", "ENST00000565956")))
  unlink("db_test", recursive = T)
})

test_that("Test getSeqTable will print data table with Sequence unavailable if there's no sequence for given transcript ids", {
  transcriptIds <- c("ENST00000565956")
  utr5_seq <- getSeqTable(transcriptIds, '5utr', "human", 101, "test_db")
  expect_true(nrow(utr5_seq) == 1)
  expect_identical(utr5_seq, data.table(seq = c("Sequence unavailable"), ensembl_transcript_id = c("ENST00000565956")))
  unlink("db_test", recursive = T)
})



