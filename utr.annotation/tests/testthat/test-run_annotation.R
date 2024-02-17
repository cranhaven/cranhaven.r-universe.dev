context("Testing run_annotation interface")
skip_on_cran()
skip_on_ci()

# Test partitionVariantFile ----------------------------------------------------------

test_that("Test partitionVariantFile create chunkPath if not exist and create chunk files in the chunkPath", {
  suppressWarnings(partitionVariantFile("testdata/partition_example.csv", chunkSize = 2, species = "mouse"))
  expect_true(dir.exists("chunks"))
  variants <- fread("testdata/partition_example.csv")
  expect_equal(nrow(variants), 15)
  expect_true(length(list.files("chunks", pattern = "*.csv")) == 8)
  chunk_1 <- read_csv("chunks/variants_chunks_1.csv")
  expect_true(nrow(chunk_1) == 2 && chunk_1$Ref[1] == "ATCTCTC" && chunk_1$Alt[1] == "ATC" && chunk_1$Pos[1] == 3559180)
  chunk_2 <- read_csv("chunks/variants_chunks_2.csv")
  expect_true(nrow(chunk_2) == 2 && chunk_2$Ref[1] == "GCTCTCTCTCT" && chunk_2$Alt[1] == "GCT" && chunk_2$Pos[1] == 3539056)
  chunk_8 <- read_csv("chunks/variants_chunks_8.csv")
  expect_true(nrow(chunk_8) == 1 && chunk_8$Ref[1] == "A" && chunk_8$Alt[1] == "C" && chunk_8$Pos[1] == 81178154)
  ## remove chunkPath after testing
  unlink("chunks", recursive = T)
})

test_that("Test partitionVariantFile will create user specified number of partitions in the chunkPath", {
  partitionVariantFile("testdata/partition_example.csv", chunkNum = 4, species = "mouse")
  expect_true(dir.exists("chunks"))
  variants <- fread("testdata/partition_example.csv")
  expect_equal(nrow(variants), 15)
  expect_true(length(list.files("chunks", pattern = "*.csv")) == 4)
  chunk_1 <- read_csv("chunks/variants_chunks_1.csv")
  expect_true(nrow(chunk_1) == 4 && chunk_1$Ref[1] == "ATCTCTC" && chunk_1$Alt[1] == "ATC" && chunk_1$Pos[1] == 3559180)
  chunk_2 <- read_csv("chunks/variants_chunks_2.csv")
  expect_true(nrow(chunk_2) == 4 && chunk_2$Ref[1] == "T" && chunk_2$Alt[1] == "TA" && chunk_2$Pos[1] == 3510738)
  chunk_3 <- read_csv("chunks/variants_chunks_3.csv")
  expect_true(nrow(chunk_3) == 4 && chunk_3$Ref[1] == "C" && chunk_3$Alt[1] == "T" && chunk_3$Pos[1] == 7932400)
  chunk_4 <- read_csv("chunks/variants_chunks_4.csv")
  expect_true(nrow(chunk_4) == 3 && chunk_4$Ref[1] == "A" && chunk_4$Alt[1] == "C" && chunk_4$Pos[1] == 81178152)
  ## remove chunkPath after testing
  unlink("chunks", recursive = T)
})

test_that("Test partitionVariantFile fail when chunkPath not empty and overwrite is FALSE", {
  dir.create("chunks_test")
  file.create("chunks_test/myfile.txt")
  expect_error(partitionVariantFile("testdata/partition_example.csv", chunkSize = 2, chunkPath = "chunks_test", species = "mouse"), "chunkPath already exists and not empty")
  ## remove chunkPath after testing
  unlink("chunks_test", recursive = T)
})

test_that("Test partitionVariantFile success when chunkPath not empty and overwrite is TRUE", {
  dir.create("chunks_test")
  file.create("chunks_test/myfile.txt")
  suppressWarnings(partitionVariantFile("testdata/partition_example.csv", chunkSize = 2, chunkPath = "chunks_test", overwrite = T, species = "mouse"))
  expect_equal(length(list.files("chunks_test", pattern = "*.csv")), 8)
  ## old files are removed
  expect_equal(length(list.files("chunks_test", pattern = "*.txt")), 0)
  ## remove chunkPath after testing
  unlink("chunks_test", recursive = T)
})

test_that("Test partitionVariantFile generate lookup table when specify chunkSize", {
  partitionVariantFile("testdata/partition_example.csv", chunkSize = 3, species = "mouse")
  expect_true(file.exists("chunks/lookup.tab"))
  lookup <- read_tsv("chunks/lookup.tab", col_names = c("idx", "filename"))
  expect_equal(nrow(lookup), 5)
  expect_equal(lookup$idx, c(1:5))
  expect_equal(lookup$filename, paste0("variants_chunks_", lookup$idx, ".csv"))
  ## remove chunkPath after testing
  unlink("chunks", recursive = T)
})

test_that("Test partitionVariantFile generate lookup table when specify chunkNum", {
  partitionVariantFile("testdata/partition_example.csv", chunkNum = 3, species = "mouse")
  expect_true(file.exists("chunks/lookup.tab"))
  lookup <- read_tsv("chunks/lookup.tab", col_names = c("idx", "filename"))
  expect_equal(nrow(lookup), 3)
  expect_equal(lookup$idx, c(1:3))
  expect_equal(lookup$filename, paste0("variants_chunks_", lookup$idx, ".csv"))
  ## remove chunkPath after testing
  unlink("chunks", recursive = T)
})

test_that("Test partitionVariantFile will return error message if a user specify both chunkSize and chunkNum", {
  expect_error(partitionVariantFile("testdata/partition_example.csv", chunkNum = 3, chunkSize = 5, species = "mouse"), "You can partition the file by specifying either chunkNum or chunkSize, but not both!")
})

test_that("Test partitionVariantFile will return error message if a user specify both chunkSize and chunkNum", {
  expect_error(partitionVariantFile("testdata/partition_example.csv", species = "mouse"), "Please specify the chunkSize or chunkNum!")
})

test_that("Test partitionVariantFile will return error message if a user specify chunkNum <= 0", {
  expect_error(partitionVariantFile("testdata/partition_example.csv", chunkNum = 0, species = "mouse"), "chunkNum > 0 is not TRUE")
})

test_that("Test partitionVariantFile will return error message if a user specify chunkSize <= 0", {
  expect_error(partitionVariantFile("testdata/partition_example.csv", chunkSize = 0, species = "mouse"), "chunkSize > 0 is not TRUE")
})

test_that("Test partitionVariantFile can accept string input for chunkSize and convert it to int", {
  chunkSizeStr <- "2"
  partitionVariantFile("testdata/partition_example.csv", chunkSize = chunkSizeStr, species = "mouse")
  expect_true(file.exists("chunks/lookup.tab"))
  expect_equal(length(list.files("chunks", pattern = "*.csv")), 8)
  ## remove chunkPath after testing
  unlink("chunks", recursive = T)
})

# test_that("Test partitionVariantFile will query the Ensembl db information for transcript in Transcript column if it exists", {
#   partitionVariantFile("testdata/partition_example.csv", chunkNum = 3, species = "mouse", ensemblVersion = 100)
#   expect_equal(length(list.files("chunks", pattern = "*.csv")), 3)
#   transcriptIds <- read_csv("testdata/partition_example.csv")$Transcript
#   chunk_1 <- read_csv("chunks/variants_chunks_1.csv")
#   expect_true(nrow(chunk_1) == 5 && chunk_1$Transcript == transcriptIds[1:5])
#   chunk_2 <- read_csv("chunks/variants_chunks_2.csv")
#   expect_true(nrow(chunk_2) == 5 && chunk_2$Transcript == transcriptIds[6:10])
#   chunk_3 <- read_csv("chunks/variants_chunks_3.csv")
#   expect_true(nrow(chunk_3) == 5 && chunk_3$Transcript == transcriptIds[11:15])
#   transcript_regions <- readRDS("db_partition_example.csv/mouse/100/transcript_regions.rds")
#   expect_true(all(sort(unique(transcript_regions$ensembl_transcript_id)) %in% sort(unique(transcriptIds))))
#   ## remove chunkPath after testing
#   unlink("chunks", recursive = T)
# })
#
#
# test_that("Test if Transcript column doesn't exist, partitionVariantFile will get ids of transcripts that overlap with variants by default", {
#   partitionVariantFile("testdata/variants_sample.csv", chunkNum = 3, species = "human")
#   expect_equal(length(list.files("chunks", pattern = "*.csv")), 3)
#   transcriptIds <- read_csv("testdata/variants_sample_results.csv")$Transcript
#   chunk_1 <- read_csv("chunks/variants_chunks_1.csv")
#   expect_true(nrow(chunk_1) == 5 && chunk_1$Transcript == transcriptIds[1:5])
#   chunk_2 <- read_csv("chunks/variants_chunks_2.csv")
#   expect_true(nrow(chunk_2) == 5 && chunk_2$Transcript == transcriptIds[6:10])
#   chunk_3 <- read_csv("chunks/variants_chunks_3.csv")
#   expect_true(nrow(chunk_3) == 3 && chunk_3$Transcript == transcriptIds[11:13])
#   ## remove chunkPath after testing
#   unlink("chunks", recursive = T)
# })

# test_that("Test if Transcript column doesn't exist, partitionVariantFile will not get ids of transcripts and query the Ensembl db information for all transcripts for the species", {
#   partitionVariantFile("testdata/variants_sample.csv", chunkNum = 3, species = "human", getTranscript = F, ensemblVersion = 103)
#   expect_equal(length(list.files("chunks", pattern = "*.csv")), 3)
#   transcriptIds <- read_csv("testdata/variants_sample_results.csv")$Transcript
#   chunk_1 <- read_csv("chunks/variants_chunks_1.csv")
#   expect_true(nrow(chunk_1) == 5 && ! "Transcript" %in% colnames(chunk_1))
#   chunk_2 <- read_csv("chunks/variants_chunks_2.csv")
#   expect_true(nrow(chunk_2) == 5 && ! "Transcript" %in% colnames(chunk_2))
#   chunk_3 <- read_csv("chunks/variants_chunks_3.csv")
#   expect_true(nrow(chunk_3) == 3 && ! "Transcript" %in% colnames(chunk_3))
#   expect_equal(sort(list.files("db_variants_sample.csv/human/103")), c("coding_seq.rds", "ensembl_human_103.rds", "Homo_sapiens.GRCh38.103.sqlite", "transcript_regions.rds", "utr3_seq.rds", "utr5_seq.rds"))
#   transcript_regions <- readRDS("db_variants_sample.csv/human/103/transcript_regions.rds")
#   expect_equal(length(unique(transcript_regions$ensembl_transcript_id)), 59961)
#   ## remove chunkPath after testing
#   unlink("chunks", recursive = T)
# })


# Test runUTRAnnotation --------------------------------------------------

test_that("Test runUTRAnnotation with default kozak strength file", {
  runUTRAnnotation("testdata/variants_sample.csv", "testdata/variants_sample_testresults.csv", "human", "93")
  expect_true(file.exists("testdata/variants_sample_testresults.csv"))
  # expected_results <- read_csv("testdata/variants_sample_results.csv")
  # expected_results <- expected_results[, order(colnames(expected_results))]
  annotation_results <- read_csv("testdata/variants_sample_testresults.csv")
  annotation_results <- annotation_results[,order(colnames(annotation_results))]
  if (Sys.info()[["sysname"]] == "Darwin" && str_detect(Sys.info()[["version"]], "RELEASE_ARM")) {
    expected_results <- read_csv("testdata/variants_sample_results_no_mrl.csv")
    expected_results <- expected_results[, order(colnames(expected_results))]
    expect_equal(annotation_results, expected_results)
  } else {
    expected_results <- read_csv("testdata/variants_sample_results.csv")
    expected_results <- expected_results[, order(colnames(expected_results))]
    expect_equal(annotation_results, expected_results)
  }

  # expect_equal(annotation_results, expected_results[!str_detect(colnames(expected_results), "mrl")])
  # skip_if(Sys.info()[["sysname"]] == "Darwin" && str_detect(Sys.info()[["version"]], "RELEASE_ARM"))
  # expect_equal(annotation_results[str_detect(colnames(expected_results), "mrl")], expected_results[str_detect(colnames(expected_results), "mrl")])
  # # expect_equal(select(annotation_results, -kozak_score, -kozak_altered_score, -mrl, -mrl_altered), select(expected_results, -kozak_score, -kozak_altered_score, -mrl, -mrl_altered))
  # expect_equal(as.double(annotation_results$kozak_score), as.double(expected_results$kozak_score), tolerance = 0.0001)
  # expect_equal(as.double(annotation_results$kozak_altered_score), as.double(expected_results$kozak_altered_score), tolerance = 0.0001)
  # expect_equal(as.double(annotation_results$mrl), as.double(expected_results$mrl), tolerance = 0.0001)
  # expect_equal(as.double(annotation_results$mrl_altered), as.double(expected_results$mrl_altered), tolerance = 0.0001)
  # ## tear up
  file.remove("testdata/variants_sample_testresults.csv")
})

test_that("Test runUTRAnnotation without specifiy ensembl version will use the latest version", {
  runUTRAnnotation("testdata/variants_sample.csv", "testdata/variants_sample_testresults.csv", "human", dataDir = "latest_db")
  current <- getLatestEnsemblVersion("human")
  expect_true(file.exists(file.path("latest_db", "human", current, paste0("ensembl_human_", current, ".rds"))))
  ## tear up
  file.remove("testdata/variants_sample_testresults.csv")
})


test_that("Test runUTRAnnotation with user provided phastCons and phyloP bigWig files directory", {
  runUTRAnnotation("testdata/variants_sample.csv", "testdata/variants_sample_testresults.csv", "human", "93", conservationBwFiles ="testdata/Conservation_scores")
  annotation_results <- read_csv("testdata/variants_sample_testresults.csv")
  expect_equal(annotation_results[["hg38.phastCons100way.subset.bw"]], c("0.002;0.58;0.894", "1", "0.008", "1", "1", "1;1;1;0.999;0.999", "0.042", "0.003", "0", "0.002", "1", "1", "1"))
  expect_equal(annotation_results[["hg38.phyloP100way.subset.bw"]], c("-0.894;0.787;1.239", "6.806", "0.641", "2.649", "1.849", "2.48;2.607;4.156;0.496;0.724", "0.421", "0.016", "-0.412", "1.055", "9.999", "3.369", "6.916"))
  ## tear up
  file.remove("testdata/variants_sample_testresults.csv")
})

test_that("Test runUTRAnnotation will not run conservation analysis if user provide bigWig files directory is invalid", {
  runUTRAnnotation("testdata/variants_sample.csv", "testdata/variants_sample_testresults.csv", "human", "93", conservationBwFiles ="testdata/wrong_folder")
  annotation_results <- read_csv("testdata/variants_sample_testresults.csv")
  if (Sys.info()[["sysname"]] == "Darwin" && str_detect(Sys.info()[["version"]], "RELEASE_ARM")) {
    expected_results <- read_csv("testdata/variants_sample_results_no_mrl.csv")
    expect_equal(sort(colnames(annotation_results)), sort(colnames(expected_results)))
  } else {
    expected_results <- read_csv("testdata/variants_sample_results.csv")
    expect_equal(sort(colnames(annotation_results)), sort(colnames(expected_results)))
  }
  ## tear up
  file.remove("testdata/variants_sample_testresults.csv")
})

test_that("Test runUTRAnnotation with Transcript column in the variants table", {
  runUTRAnnotation("testdata/variants_sample_with_trans.csv", "testdata/variants_sample_with_trans_testresults.csv", "human", "93")
  annotation_results <- read_csv("testdata/variants_sample_with_trans_testresults.csv")
  expect_equal(sum(!is.na(annotation_results[["kozak_transcript_id"]])), 2)
  expect_equal(sum(!is.na(annotation_results[["utr3_transcript_id"]])), 3)
  expect_equal(sum(!is.na(annotation_results[["utr5_transcript_id"]])), 3)
  expect_equal(sum(!is.na(annotation_results[["startCodon_transcript_id"]])), 1)
   ## tear up
  file.remove("testdata/variants_sample_with_trans_testresults.csv")
})


test_that("Test runUTRAnnotation with the variants table which do not have any mutations fall in any feature element regions", {
  runUTRAnnotation("testdata/variants_no_feature.csv", "testdata/variants_no_feature_testresults.csv", "human", "93")
  variants <- read_csv("testdata/variants_no_feature.csv", col_types = cols(.default = "c"))
  annotation_results <- read_csv("testdata/variants_no_feature_testresults.csv", col_types = cols(.default = "c"))
  expect_true(setdiff(colnames(annotation_results), colnames(variants)) == "Transcript")
  # convert tibble to data.frame than compare the two to ignore the attr section
  expect_identical(data.frame(dplyr::select(annotation_results, -Transcript)), data.frame(variants))
  ## tear up
  file.remove("testdata/variants_no_feature_testresults.csv")
})

test_that("Test runUTRAnnotation with VCF input", {
  annotation_results <- runUTRAnnotation("testdata/vcf_variants.vcf", "testdata/vcf_variants_testresults.csv", "mouse", "93", format = "vcf")
  annotation_results <- read_csv("testdata/vcf_variants_testresults.csv")
  if (Sys.info()[["sysname"]] == "Darwin" && str_detect(Sys.info()[["version"]], "RELEASE_ARM")) {
    expected_results <- read_csv("testdata/vcf_variants_results_no_mrl.csv")
    expect_equal(annotation_results, expected_results)
  } else {
    expected_results <- read_csv("testdata/vcf_variants_results.csv")
    expect_equal(annotation_results, expected_results)
  }
  ## tear up
  file.remove("testdata/vcf_variants_testresults.csv")
})

# Test concatenateAnnotationResult ----------------------------------------

test_that("Test concatenateAnnotationResult can concatenate annotation result of the chunks into one file and rows are in the same order as the input variant file", {
  partitionVariantFile("testdata/variants_sample.csv", chunkSize = 3, chunkPath = "chunks_test", species = "human", ensemblVersion = 93)
  lookupFile <- file.path("chunks_test/lookup.tab")
  expect_true(file.exists(lookupFile))
  lookup <- read_tsv(lookupFile, col_names = c("idx", "filename"))
  expect_equal(nrow(lookup), 5)
  for (row in 1:nrow(lookup)) {
    resultFile <- file.path("chunks_test", "results", paste0("variants_matrix_chunks_", lookup[row,]$idx, ".csv"))
    runUTRAnnotation(file.path("chunks_test", lookup[row,]$filename), resultFile, "human", "93", dataDir = "db_variants_sample.csv")
  }
  expect_true(dir.exists("chunks_test/results"))
  resultFiles <- list.files("chunks_test/results", pattern = ".csv")
  expect_equal(length(resultFiles), 5)
  concatenateAnnotationResult("chunks_test/results", "testdata/final_matrix.csv")
  expect_true(file.exists("testdata/final_matrix.csv"))
  annotation_results <- read_csv("testdata/final_matrix.csv")
  annotation_results <- annotation_results[,order(colnames(annotation_results))]
  ## check if final annotation matrix has correct content
  if (Sys.info()[["sysname"]] == "Darwin" && str_detect(Sys.info()[["version"]], "RELEASE_ARM")) {
    expected_results <- read_csv("testdata/variants_sample_results_no_mrl.csv")
    expected_results <- expected_results[, order(colnames(expected_results))]
    expect_equal(annotation_results, expected_results)
  } else {
    expected_results <- read_csv("testdata/variants_sample_results.csv")
    expected_results <- expected_results[, order(colnames(expected_results))]
    expect_equal(annotation_results, expected_results)
  }
  # expected_results <- read_csv("testdata/variants_sample_results.csv")
  # expected_results <- expected_results[, order(colnames(expected_results))]
  # annotation_results <- read_csv("testdata/final_matrix.csv")
  # annotation_results <- annotation_results[,order(colnames(annotation_results))]
  # expect_identical(select(annotation_results, -kozak_score, -kozak_altered_score, -mrl, -mrl_altered), select(expected_results, -kozak_score, -kozak_altered_score, -mrl, -mrl_altered))
  # expect_equal(as.double(annotation_results$kozak_score), as.double(expected_results$kozak_score), tolerance = 0.0001)
  # expect_equal(as.double(annotation_results$kozak_altered_score), as.double(expected_results$kozak_altered_score), tolerance = 0.0001)
  # expect_equal(as.double(annotation_results$mrl), as.double(expected_results$mrl), tolerance = 0.0001)
  # expect_equal(as.double(annotation_results$mrl_altered), as.double(expected_results$mrl_altered), tolerance = 0.0001)
  ## tear up
  unlink("chunks_test", recursive = T)
  file.remove("testdata/final_matrix.csv")
})

test_that("Test concatenateAnnotationResult will create output folder for the output file if it is not exist", {
  partitionVariantFile("testdata/variants_sample.csv", chunkSize = 3, chunkPath = "chunks_test", species = "human")
  lookupFile <- file.path("chunks_test/lookup.tab")
  lookup <- read_tsv(lookupFile, col_names = c("idx", "filename"))
  for (row in 1:nrow(lookup)) {
    resultFile <- file.path("chunks_test", "results", paste0("variants_matrix_chunks_", lookup[row,]$idx, ".csv"))
    suppressWarnings(runUTRAnnotation(file.path("chunks_test", lookup[row,]$filename), resultFile, "human", "93", dataDir = "db_variants_sample.csv"))
  }
  resultFiles <- list.files("chunks_test/results", pattern = ".csv")
  concatenateAnnotationResult("chunks_test/results", "testdata_new/final_matrix.csv")
  expect_true(file.exists("testdata_new/final_matrix.csv"))
  ## tear up
  unlink("chunks_test", recursive = T)
  unlink("testdata_new", recursive = T)
})
