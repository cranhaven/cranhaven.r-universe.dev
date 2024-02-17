context("Testing utr_analysis module")
skip_on_cran()

# Test getTranscriptIdsForUTRVariants -------------------------------------

test_that("Test getTranscriptIdsForUTRVariants to get transcript ids of variants at 3' UTR region", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"), Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"),
                                  utr3_start = c(31, 10, 80), strand = c(-1,1,-1), utr3_end = c(110, 148, 150))
  utr3_transcript_ids <- getTranscriptIdsForUTRVariants(variantsTable, transcriptRegions, "utr3")
  expect_equal(utr3_transcript_ids, c("tran1", "tran2", "tran3"))
})

test_that("Test getTranscriptIdsForUTRVariants to get transcript ids of variants at CDS region", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"), Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"),
                                  genomic_coding_start = c(31, 10, 80), strand = c(-1,1,-1), genomic_coding_end = c(110, 148, 150))
  cds_transcript_ids <- getTranscriptIdsForUTRVariants(variantsTable, transcriptRegions, "cds")
  expect_equal(cds_transcript_ids, c("tran1", "tran2", "tran3"))
})

test_that("Test getTranscriptIdsForUTRVariants to get transcript ids of variants at 5' UTR region", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"), Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"),
                                  utr5_start = c(31, 10, 80), strand = c(-1,1,-1), utr5_end = c(110, 148, 150))
  utr5_transcript_ids <- getTranscriptIdsForUTRVariants(variantsTable, transcriptRegions, "utr5")
  expect_equal(utr5_transcript_ids, c("tran1", "tran2", "tran3"))
})

test_that("Test getTranscriptIdsForUTRVariants to get transcript ids of variants at transcript region", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"), Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"),
                                  transcript_start = c(31, 10, 80), strand = c(-1,1,-1), transcript_end = c(110, 148, 150))
  transcript_ids <- getTranscriptIdsForUTRVariants(variantsTable, transcriptRegions, "transcript")
  expect_equal(transcript_ids, c("tran1", "tran2", "tran3"))
})

test_that("Test getTranscriptIdsForUTRVariants to get transcript ids of variants at transcript region, the transcripts for each gene will be sorted by transcript id", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"), Transcript = c("tran1", "tran2", "tran3;tran4"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"),
                                  transcript_start = c(31, 10, 80, 90), strand = c(-1,1,-1,-1), transcript_end = c(110, 148, 150, 150))
  transcript_ids <- getTranscriptIdsForUTRVariants(variantsTable, transcriptRegions, "transcript")
  expect_equal(transcript_ids, c("tran1", "tran2", "tran3;tran4"))
})

test_that("Test getTranscriptIdsForUTRVariants to get transcript ids of variants at 3' UTR region, multiple isoforms, at gene level", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"), Transcript = c("tran1", "tran2", "tran3;tran4"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"),
                                  utr3_start = c(31, 10, 80, 90), strand = c(1,1,-1,-1), utr3_end = c(110, 148, 150, 150))
  utr3_transcript_ids <- getTranscriptIdsForUTRVariants(variantsTable, transcriptRegions, "utr3")
  expect_equal(utr3_transcript_ids, c("tran1", "tran2", "tran3;tran4"))
})

test_that("Test getTranscriptIdsForUTRVariants to get transcript ids of variants at 3' UTR region, at transcript level", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"), Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"),
                                  utr3_start = c(31, 10, 80, 90), strand = c(1,1,-1,-1), utr3_end = c(110, 148, 150, 150))
  utr3_transcript_ids <- getTranscriptIdsForUTRVariants(variantsTable, transcriptRegions, "utr3")
  expect_equal(utr3_transcript_ids, c("tran1", "tran2", "tran3"))
})

test_that("Test getTranscriptIdsForUTRVariants to get transcript ids of variants at a specified region with corner case deletion", {
  variantsTable <- data.table(Chr = c(3, 2, 3, 1), Pos = c(30, 149, 78, 98), Ref = c("CT", "TTG", "GTT", "GC"), Alt = c("C", "C", "A", "C"), Transcript = c("tran1", "tran2", "tran3", "tran4"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3", "g4"), ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"),
                                  utr3_start = c(31, 10, 80, 100), strand = c(-1,1,-1,1), utr3_end = c(110, 148, 150, 200))
  utr3_transcript_ids <- getTranscriptIdsForUTRVariants(variantsTable, transcriptRegions, "utr3")
  expect_equal(utr3_transcript_ids, c("tran1", NA, "tran3", NA))
})

test_that("Test getTranscriptIdsForUTRVariants to get transcript ids of variants at a specified region with corner case insertion", {
  variantsTable <- data.table(Chr = c(3), Pos = c(98), Ref = c("G"), Alt = c("GTTTG"),
                              Transcript = c("tran1;tran2"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g1"), ensembl_transcript_id = c("tran1", "tran2"),
                                  utr3_start = c(95, 100), strand = c(-1,1), utr3_end = c(150, 200))
  utr3_transcript_ids <- getTranscriptIdsForUTRVariants(variantsTable, transcriptRegions, "utr3")
  expect_equal(utr3_transcript_ids, c("tran1"))
})

test_that("Test getTranscriptIdsForUTRVariants to get transcript ids of variants at a specified region with multiple trans or no trans", {
  variantsTable <- data.table(Chr = c(3, 2, 2), Pos = c(30, 90, 25), Ref = c("CT", "T", "T"), Alt = c("C", "C", "G"),
                              Transcript = c("tran1", "tran2;tran3;tran4", NA))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g2", "g2"), ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"),
                                  utr3_start = c(31, 10, 80, 100), strand = c(-1,1,-1,1), utr3_end = c(110, 148, 150, 200))
  utr3_transcript_ids <- getTranscriptIdsForUTRVariants(variantsTable, transcriptRegions, "utr3")
  expect_equal(utr3_transcript_ids, c("tran1", "tran2;tran3", NA))
})


# Test getTranscriptIdsForCodonVariants -----------------------------------

test_that("Test getTranscriptIdsForCodonVariants return transcript ids of stop codon mutations", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"),
                              Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"),
                                  genomic_coding_start = c(31, 10, 80), strand = c(-1,1,-1), genomic_coding_end = c(110, 148, 150))
  stopCodonIds <- getTranscriptIdsForCodonVariants(variantsTable, transcriptRegions, "stopCodon")
  expect_equal(stopCodonIds$ensembl_transcript_id, c("tran1", "tran2", NA))
  expect_equal(stopCodonIds$codon_positions, c("31|32|33", "146|147|148", NA))
})

test_that("Test getTranscriptIdsForCodonVariants return transcript ids of stop codon mutations, multiple isoforms", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"),
                              Transcript = c("tran1", "tran2;tran4", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3", "g2"), ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"),
                                  genomic_coding_start = c(31, 10, 80, 140), strand = c(-1,1,-1,1), genomic_coding_end = c(110, 148, 150, 149))
  stopCodonIds <- getTranscriptIdsForCodonVariants(variantsTable, transcriptRegions, "stopCodon")
  expect_equal(stopCodonIds$ensembl_transcript_id, c("tran1", "tran2;tran4", NA))
  expect_equal(stopCodonIds$codon_positions, c("31|32|33", "146|147|148;147|148|149", NA))
})

test_that("Test getTranscriptIdsForCodonVariants return transcript ids of stop codon mutations, multiple isoforms, transcript level", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"),
                              Transcript = c("tran1", "tran4", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3", "g2"), ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"),
                                  genomic_coding_start = c(31, 10, 80, 140), strand = c(-1,1,-1,1), genomic_coding_end = c(110, 148, 150, 149))
  stopCodonIds <- getTranscriptIdsForCodonVariants(variantsTable, transcriptRegions, "stopCodon")
  expect_equal(stopCodonIds$ensembl_transcript_id, c("tran1", "tran4", NA))
  expect_equal(stopCodonIds$codon_positions, c("31|32|33", "147|148|149", NA))
})

test_that("Test getTranscriptIdsForCodonVariants return transcript ids of stop codon mutations, stop codon positions are not continuous", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(33, 147, 110), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"),
                              Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g1", "g2", "g2", "g3", "g3"), ensembl_transcript_id = c("tran1", "tran1", "tran2", "tran2", "tran3", "tran3"),
                                  genomic_coding_start = c(31, 45, 10, 147, 80, 149), strand = c(-1,-1,1,1,1,1), genomic_coding_end = c(31, 110, 100, 148, 110, 150))
  stopCodonIds <- getTranscriptIdsForCodonVariants(variantsTable, transcriptRegions, "stopCodon")
  expect_equal(stopCodonIds$ensembl_transcript_id, c(NA, "tran2", "tran3"))
  expect_equal(stopCodonIds$codon_positions, c(NA, "100|147|148", "110|149|150"))
})

test_that("Test getTranscriptIdsForCodonVariants return transcript ids of start codon mutations", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(108, 11, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"),
                              Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"),
                                  genomic_coding_start = c(31, 10, 80), strand = c(-1,1,-1), genomic_coding_end = c(110, 148, 150))
  startCodonIds <- getTranscriptIdsForCodonVariants(variantsTable, transcriptRegions, "startCodon")
  expect_equal(startCodonIds$ensembl_transcript_id, c("tran1", "tran2", NA))
  expect_equal(startCodonIds$codon_positions, c("108|109|110", "10|11|12", NA))
})

test_that("Test getTranscriptIdsForCodonVariants return transcript ids of start codon mutations, start codon positions are not continuous", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(109, 12, 159), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"),
                              Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g1", "g2", "g2", "g3", "g3"), ensembl_transcript_id = c("tran1", "tran1", "tran2", "tran2", "tran3", "tran3"),
                                  genomic_coding_start = c(31, 109, 10, 50, 140, 159), strand = c(-1,-1,1,1,1,1), genomic_coding_end = c(100, 110, 11, 148, 141, 180))
  startCodonIds <- getTranscriptIdsForCodonVariants(variantsTable, transcriptRegions, "startCodon")
  expect_equal(startCodonIds$ensembl_transcript_id, c("tran1", NA, "tran3"))
  expect_equal(startCodonIds$codon_positions, c("100|109|110", NA, "140|141|159"))
})

test_that("Test getTranscriptIdsForCodonVariants return transcript ids of stop codon mutations with corner cases", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(30, 145, 120), Ref = c("CT", "T", "G"), Alt = c("C", "CG", "A"),
                              Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"),
                                  genomic_coding_start = c(31, 10, 80), strand = c(-1,1,-1), genomic_coding_end = c(110, 148, 150))
  stopCodonIds <- getTranscriptIdsForCodonVariants(variantsTable, transcriptRegions, "stopCodon")
  expect_equal(stopCodonIds$ensembl_transcript_id, c("tran1", NA, NA))
  expect_equal(stopCodonIds$codon_positions, c("31|32|33", NA, NA))
})

test_that("Test getTranscriptIdsForCodonVariants return transcript ids of start codon mutations with corner cases", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(106, 8, 120), Ref = c("CAT", "T", "G"), Alt = c("C", "CTGT", "A"),
                              Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"),
                                  genomic_coding_start = c(31, 10, 80), strand = c(-1,1,-1), genomic_coding_end = c(110, 148, 150))
  startCodonIds <- getTranscriptIdsForCodonVariants(variantsTable, transcriptRegions, "startCodon")
  expect_equal(startCodonIds$ensembl_transcript_id, c("tran1", NA, NA))
  expect_equal(startCodonIds$codon_positions, c("108|109|110", NA, NA))
})

test_that("Test getTranscriptIdsForCodonVariants behavive on no mutation fall in start codon region", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(106, 8, 120), Ref = c("CAT", "T", "G"), Alt = c("C", "CTGT", "A"),
                              Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"),
                                  genomic_coding_start = c(1, 1, 1), strand = c(-1,1,-1), genomic_coding_end = c(5, 5, 5))
  startCodonIds <- getTranscriptIdsForCodonVariants(variantsTable, transcriptRegions, "startCodon")
  expect_true(all(is.na(startCodonIds$ensembl_transcript_id)))
  expect_true(all(is.na(startCodonIds$codon_positions)))
})


test_that("Test getTranscriptIdsForCodonVariants return data frame with all NAs if all transcript ids are NAs", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(106, 8, 120), Ref = c("CAT", "T", "G"), Alt = c("C", "CTGT", "A"),
                              Transcript = c(NA, NA, NA))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"),
                                  genomic_coding_start = c(1, 1, 1), strand = c(-1,1,-1), genomic_coding_end = c(5, 5, 5))
  startCodonIds <- getTranscriptIdsForCodonVariants(variantsTable, transcriptRegions, "startCodon")
  expect_true(all(is.na(startCodonIds$ensembl_transcript_id)))
  expect_true(all(is.na(startCodonIds$codon_positions)))
})


# Test getTranscriptIdsForTSSKozakVariants --------------------------------

test_that("Test getTranscriptIdsForTSSKozakVariants return transcript ids of TSS Kozak region mutations", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(106, 8, 120), Ref = c("C", "T", "G"), Alt = c("G", "C", "A"),
                              Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"),
                                  genomic_coding_start = c(31, 10, 80), strand = c(-1,1,-1), genomic_coding_end = c(110, 148, 150),
                                  utr5_start = c(120, 3, 160), utr5_end = c(130, 9, 171))
  kozakIds <- getTranscriptIdsForTSSKozakVariants(variantsTable, transcriptRegions)
  expect_equal(kozakIds$ensembl_transcript_id, c("tran1", "tran2", NA))
  expect_equal(kozakIds$kozak_positions, c("106|107|108|109|110|120|121|122", "7|8|9|10|11|12|13|14", NA))
})

test_that("Test getTranscriptIdsForTSSKozakVariants return transcript ids of TSS Kozak region mutations, multiple isoforms", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(106, 8, 120), Ref = c("C", "T", "G"), Alt = c("G", "C", "A"),
                              Transcript = c("tran1;tran4", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3", "g1"), ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"),
                                  genomic_coding_start = c(31, 10, 80, 120), strand = c(-1,1,-1,-1), genomic_coding_end = c(110, 148, 150, 300),
                                  utr5_start = c(120, 3, 160, 104), utr5_end = c(130, 9, 171, 110))
  kozakIds <- getTranscriptIdsForTSSKozakVariants(variantsTable, transcriptRegions)
  expect_equal(kozakIds$ensembl_transcript_id, c("tran1;tran4", "tran2", NA))
  expect_equal(kozakIds$kozak_positions, c("106|107|108|109|110|120|121|122;104|105|106|296|297|298|299|300", "7|8|9|10|11|12|13|14", NA))
})

test_that("Test getTranscriptIdsForTSSKozakVariants return transcript ids of TSS Kozak region mutations, regions continuous", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(109, 132, 119), Ref = c("C", "T", "G"), Alt = c("G", "C", "A"),
                              Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g1", "g2", "g2", "g3", "g3"), ensembl_transcript_id = c("tran1", "tran1", "tran2", "tran2", "tran3", "tran3"),
                                  genomic_coding_start = c(31, 109, 31, 109, 130, 140), strand = c(-1,-1,-1,-1,1,1), genomic_coding_end = c(100, 110, 100, 110, 133, 150),
                                  utr5_start = c(120, 125, 120, 135, 105, 119), utr5_end = c(121,136, 130,136, 106, 120))
  kozakIds <- getTranscriptIdsForTSSKozakVariants(variantsTable, transcriptRegions)
  expect_equal(kozakIds$ensembl_transcript_id, c("tran1", NA, "tran3"))
  expect_equal(kozakIds$kozak_positions, c("98|99|100|109|110|120|121|125", NA, "106|119|120|130|131|132|133|140"))
})

test_that("Test getTranscriptIdsForTSSKozakVariants return transcript ids of TSS Kozak region mutations, multiple isoforms, transcript level", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(106, 8, 120), Ref = c("C", "T", "G"), Alt = c("G", "C", "A"),
                              Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3", "g1"), ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"),
                                  genomic_coding_start = c(31, 10, 80, 120), strand = c(-1,1,-1,-1), genomic_coding_end = c(110, 148, 150, 300),
                                  utr5_start = c(120, 3, 160, 104), utr5_end = c(130, 9, 171, 110))
  kozakIds <- getTranscriptIdsForTSSKozakVariants(variantsTable, transcriptRegions)
  expect_equal(kozakIds$ensembl_transcript_id, c("tran1", "tran2", NA))
  expect_equal(kozakIds$kozak_positions, c("106|107|108|109|110|120|121|122", "7|8|9|10|11|12|13|14", NA))
})

test_that("Test getTranscriptIdsForTSSKozakVariants return transcript ids of TSS Kozak region mutations with corner cases", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(104, 6, 158), Ref = c("C", "TA", "GC"), Alt = c("GGTC", "C", "A"),
                              Transcript = c("tran1", "tran2", "tran3"))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"),
                                  genomic_coding_start = c(31, 10, 80), strand = c(-1,1,-1), genomic_coding_end = c(110, 148, 150),
                                  utr5_start = c(120, 3, 160), utr5_end = c(130, 9, 171))
  kozakIds <- getTranscriptIdsForTSSKozakVariants(variantsTable, transcriptRegions)
  expect_equal(kozakIds$ensembl_transcript_id, c(NA, "tran2", NA))
  expect_equal(kozakIds$kozak_positions, c(NA, "7|8|9|10|11|12|13|14", NA))
})


test_that("Test getTranscriptIdsForTSSKozakVariants return data frame with all NAs if all transcript ids are NAs", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(104, 6, 158), Ref = c("C", "TA", "GC"), Alt = c("GGTC", "C", "A"),
                              Transcript = c(NA, NA, NA))
  transcriptRegions <- data.table(ensembl_gene_id = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"),
                                  genomic_coding_start = c(31, 10, 80), strand = c(-1,1,-1), genomic_coding_end = c(110, 148, 150),
                                  utr5_start = c(120, 3, 160), utr5_end = c(130, 9, 171))
  kozakIds <- getTranscriptIdsForTSSKozakVariants(variantsTable, transcriptRegions)
  expect_true(all(is.na(kozakIds$ensembl_transcript_id)))
  expect_true(all(is.na(kozakIds$kozak_positions)))
})


# Test getAltSequence -----------------------------------------------------

test_that("Test getAltSequence write error message and return NA if the Ref doesn't match the actual sequence, positive strand", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran2"
  pos <- "121"
  ref <- "C"
  alt <- "G"
  expect_warning(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), paste0("Ref ", ref, " at postion ", pos, " doesn't match the transcript ", transcriptId))
  expect_identical(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), NA_character_)
})

test_that("Test getAltSequence write error message and return NA if the Ref doesn't match the actual sequence, negative strand", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran3"
  pos <- "31"
  ref <- "A"
  alt <- "G"
  expect_warning(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), paste0("Ref ", ref, " at postion ", pos, " doesn't match the transcript ", transcriptId))
  expect_identical(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), NA_character_)
})

test_that("Test getAltSequence returns altered sequence on positive strand", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran2"
  pos <- "121"
  ref <- "T"
  alt <- "G"
  expect_equal(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), "AGTACCCG")
})

test_that("Test getAltSequence returns altered sequence on positive strand for deletion", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran2"
  pos <- "121"
  ref <- "TTA"
  alt <- "G"
  expect_equal(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), "AGCCCG")
})

test_that("Test getAltSequence returns altered sequence on positive strand for insertion", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran2"
  pos <- "123"
  ref <- "A"
  alt <- "GGG"
  expect_equal(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), "ATTGGGCCCG")
})

test_that("Test getAltSequence returns altered sequence on positive strand for deletion, long ref goes across the region", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran1"
  pos <- "105"
  ref <- "ACCT"
  alt <- "G"
  expect_equal(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), "ATGCCG")
})

test_that("Test getAltSequence returns altered sequence on negative strand for deletion, long ref goes across the region", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran3"
  pos <- "39"
  ref <- "AAACC"
  alt <- "A"
  expect_equal(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), "TCCAATCCCG")
})

test_that("Test getAltSequence returns altered sequence on positive strand for deletion, the pos not inside the region, but long ref overlaps the region", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran1"
  pos <- "98"
  ref <- "ACAT"
  alt <- "G"
  expect_equal(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), "GCCA")
})

test_that("Test getAltSequence returns altered sequence on positive strand for insertion, the pos not inside the region, but long alt overlaps the region", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran2"
  pos <- "119"
  ref <- "A"
  alt <- "GCCT"
  expect_equal(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), "ATTACCCG")
})

test_that("Test getAltSequence returns altered sequence on negative strand", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran3"
  pos <- "34"
  # ref and alt sequences are always on + strand
  ref <- "A"
  alt <- "C"
  expect_equal(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), "TTCCAAGCCCG")
})

test_that("Test getAltSequence returns altered sequence on negative strand for deletion", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran3"
  pos <- "34"
  # ref and alt sequences are always on + strand
  ref <- "ATT"
  alt <- "C"
  expect_equal(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), "TTCCGCCCG")
})

test_that("Test getAltSequence returns altered sequence on negative strand for insertion", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran3"
  pos <- "34"
  # ref and alt sequences are always on + strand
  ref <- "A"
  alt <- "CAC"
  expect_equal(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), "TTCCAAGTGCCCG")
})

test_that("Test getAltSequence returns altered sequence on negative strand for deletion, the pos not inside the region, but long ref overlaps the region", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran3"
  pos <- "28"
  ref <- "ACCG"
  alt <- "A"
  expect_equal(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), "TTCCAATCC")
})

test_that("Test getAltSequence returns altered sequence on negative strand for insertion, the pos not inside the region, but long alt overlaps the region", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran3"
  pos <- "29"
  ref <- "A"
  alt <- "GCCT"
  expect_equal(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), "TTCCAATCCCG")
})

test_that("Test getAltSequence returns altered sequence on negative strand for deletion, end position of Ref exceeds end coordinate of the region", {
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  transcriptId <- "tran1"
  pos <- "105"
  ref <- "ATTCC"
  alt <- "G"
  expect_equal(getAltSequence(seqTable, transcriptsTable, transcriptId, pos, ref, alt), "ATGCCG")
})


# Test countDNAPattern ----------------------------------------------------

test_that("Test countDNAPattern", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"),
                              Gene = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"))
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  dnaPattern <- "ATG"
  transcriptIdCol <- "ensembl_transcript_id"
  expect_equal(countDNAPattern(variantsTable, seqTable, dnaPattern, transcriptIdCol), c("1","0","0"))
})

test_that("Test countDNAPattern with no transcript id column", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"),
                              Gene = c("g1", "g2", "g3"))
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG"))
  dnaPattern <- "ATG"
  transcriptIdCol <- "ensembl_transcript_id"
  expect_equal(countDNAPattern(variantsTable, seqTable, dnaPattern, transcriptIdCol), NA)
})

test_that("Test countDNAPattern with empty seqTable", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"),
                              Gene = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"))
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("Sequence unavailable", "Sequence unavailable", "Sequence unavailable"))
  dnaPattern <- "ATG"
  transcriptIdCol <- "ensembl_transcript_id"
  expect_equal(countDNAPattern(variantsTable, seqTable, dnaPattern, transcriptIdCol), c("0", "0", "0"))
})

test_that("Test countDNAPattern with a seqTable missing some transcripts", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"),
                              Gene = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1", "tran2", "tran3"))
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran3"), seq = c("ATGCCA", "Sequence unavailable"))
  dnaPattern <- "ATG"
  transcriptIdCol <- "ensembl_transcript_id"
  expect_equal(countDNAPattern(variantsTable, seqTable, dnaPattern, transcriptIdCol), c("1",NA,"0"))
})

test_that("Test countDNAPattern with multiple transcript ids", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"),
                              Gene = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1;tran4", "tran2", "tran3"))
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG", "ATGCATG"))
  dnaPattern <- "ATG"
  transcriptIdCol <- "ensembl_transcript_id"
  expect_equal(countDNAPattern(variantsTable, seqTable, dnaPattern, transcriptIdCol), c("1;2","0","0"))
})

test_that("Test countDNAPattern with no transcript id", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 147, 120), Ref = c("CT", "T", "G"), Alt = c("C", "C", "A"),
                              Gene = c("g1", "g2", "g3"), ensembl_transcript_id = c(NA, "tran2", NA))
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG", "ATGCATG"))
  dnaPattern <- "ATG"
  transcriptIdCol <- "ensembl_transcript_id"
  expect_equal(countDNAPattern(variantsTable, seqTable, dnaPattern, transcriptIdCol), c(NA,"0",NA))
})


# Test countDNAPatternInAlt -----------------------------------------------

test_that("Test countDNAPatternInAlt return number of patterns in alt sequences", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 122, 120), Ref = c("CT", "T", "G"), Alt = c("C", "G", "A"),
                              Gene = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1;tran4", "tran2", "tran3"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), start = c(100, 120, 30, 112), end = c(105, 127, 40, 118), strand = c(1,1,-1,-1))
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG", "ATGCATG"))
  dnaPattern <- "ATG"
  transcriptIdCol <- "ensembl_transcript_id"
  expect_equal(countDNAPatternInAlt(variantsTable, seqTable, transcriptsTable, dnaPattern, transcriptIdCol), c("1;2","1","0"))

})

test_that("Test countDNAPatternInAlt return number of patterns in alt sequences, get more pattern after deletion", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 122, 33), Ref = c("CT", "T", "CG"), Alt = c("C", "G", "C"),
                              Gene = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1;tran4", "tran2", "tran3"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), start = c(100, 120, 30, 112), end = c(105, 127, 40, 118), strand = c(1,1,-1,-1))
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), seq = c("ATGCCA", "ATTACCCG", "TCCAATCGCCG", "ATGCATG"))
  dnaPattern <- "ATG"
  transcriptIdCol <- "ensembl_transcript_id"
  expect_equal(countDNAPatternInAlt(variantsTable, seqTable, transcriptsTable, dnaPattern, transcriptIdCol), c("1;2","1","1"))

})

test_that("Test countDNAPatternInAlt return NA in alt sequences if the ref doesn't match in getAltSequence", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 122, 33), Ref = c("C", "A", "C"), Alt = c("A", "G", "A"),
                              Gene = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1;tran4", "tran2", "tran3;tran4"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), start = c(31, 120, 30, 27), end = c(36, 127, 40, 33), strand = c(1,1,-1,-1))
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), seq = c("ATGCCA", "ATTACCCG", "TTCCAATCCCG", "ATGCATG"))
  dnaPattern <- "ATG"
  transcriptIdCol <- "ensembl_transcript_id"
  expect_equal(countDNAPatternInAlt(variantsTable, seqTable, transcriptsTable, dnaPattern, transcriptIdCol), c("NA;1", NA, NA))

})

test_that("Test countDNAPatternInAlt return NA in alt sequences if transcript seq is unavailable", {
  variantsTable <- data.table(Chr = c(3, 2), Pos = c(32, 122), Ref = c("A", "A"), Alt = c("C", "G"),
                              Gene = c("g1", "g2"), ensembl_transcript_id = c("tran1;tran4", "tran2"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), start = c(100, 120, 30, 28), end = c(105, 127, 40, 34), strand = c(1,1,-1,-1))
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), seq = c("Sequence unavailable", "ATTACCCG", "Sequence unavailable", "ATTACCCG"))
  dnaPattern <- "ATG"
  transcriptIdCol <- "ensembl_transcript_id"
  expect_equal(countDNAPatternInAlt(variantsTable, seqTable, transcriptsTable, dnaPattern, transcriptIdCol), c("NA;1",NA))

})


# Test checkIfGainOrLoseAfterAlt ------------------------------------------

test_that("Test checkIfGainOrLoseAfterAlt", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 122, 33), Ref = c("CT", "T", "CG"), Alt = c("C", "G", "C"),
                              Gene = c("g1", "g2", "g3"), ensembl_transcript_id = c("tran1;tran4", "tran2", "tran3"),
                              numBefore = c("1", "0", "2;4;10.1;2;3.1"),
                              numAfter = c("0", "0", "1;6;2.5;11;3.1"))

  expect_equal(checkIfGainOrLoseAfterAlt(variantsTable, "numBefore", "numAfter"), c("lost", "equal", "lost;gained;lost;gained;equal"))
})


# Test checkIfLostCodonAfterAlt -------------------------------------------

test_that("Test checkIfLostCodonAfterAlt, check if lost start codon ATG", {
  variantsTable <- data.table(Chr = c(3, 2, 3), Pos = c(31, 122, 33), Ref = c("CT", "T", "CG"), Alt = c("C", "G", "C"),
                              start_codon_alt = c("ATG", "TG;ATG", NA))
  expect_equal(checkIfLostCodonAfterAlt(variantsTable, "start_codon_alt", "ATG"), c("FALSE", "TRUE;FALSE", NA))
})


# Test getCodon -----------------------------------------------------------

test_that("Test getCodon to get stop codon with providing coding sequences", {
  variantsTable <-  data.table(Chr = c(3, 2, 3), Pos = c(31, 122, 33), Ref = c("CT", "T", "CG"), Alt = c("C", "G", "C"),
                               transcript_ids = c("tran1", "tran2", "tran3"))
  # coding sequence
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), seq = c("ATGCCA", "ATTACCCG", "TCCAATCGCCG", "ATGCATG"))
  codonRegion <- "stopCodon"
  transcriptIdColumn <- "transcript_ids"
  expect_equal(getCodon(variantsTable, seqTable, transcriptIdColumn, codonRegion), c("CCA", "CCG", "CCG"))

})

test_that("Test getCodon to get start codon with providing coding sequences", {
  variantsTable <-  data.table(Chr = c(3, 2, 3), Pos = c(31, 122, 33), Ref = c("CT", "T", "CG"), Alt = c("C", "G", "C"),
                               transcript_ids = c("tran1", "tran2", "tran3"))
  # coding sequence
  seqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), seq = c("ATGCCA", "ATTACCCG", "TCCAATCGCCG", "ATGCATG"))
  codonRegion <- "startCodon"
  transcriptIdColumn <- "transcript_ids"
  expect_equal(getCodon(variantsTable, seqTable, transcriptIdColumn, codonRegion), c("ATG", "ATT", "TCC"))

})


# Test getRegionsForDiscretePos -------------------------------------------

test_that("Test getRegionsForDiscretePos will output a dataframe of regions for a vector of continous positions", {
  positions <- c(97,98,99,100,101)
  regions <- getRegionsForDiscretePos(positions)
  expect_equal(regions, data.table(start = 97, end = 101))
})

test_that("Test getRegionsForDiscretePos will output a dataframe of regions for a vector of discrete positions", {
  positions <- c(97,98,99,120,121)
  regions <- getRegionsForDiscretePos(positions)
  expect_equal(regions, data.table(start = c(97,120), end = c(99,121)))
})




# Test getCodonInAlt ------------------------------------------------------

test_that("Test getCodonInAlt to get altered stop codon sequence", {
  variantsTable <-  data.table(Chr = c(3, 2, 3), Pos = c(105, 122, 31), Ref = c("T", "T", "T"), Alt = c("C", "G", "G"),
                               transcript_ids = c("tran1", "tran2", "tran3"), stopCodonPos = c("105|104|103", "127|126|125", "30|31|32"), stopCodonSeq = c("ATT", "TTG", "CAC"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), start = c(100, 120, 30, 112), end = c(105, 127, 40, 400), strand = c(1,1,-1,-1))
  codonPositionsColumn <- "stopCodonPos"
  transcriptIdColumn <- "transcript_ids"
  codonSeqColumn <- "stopCodonSeq"
  expect_equal(getCodonInAlt(variantsTable, transcriptsTable, transcriptIdColumn, codonPositionsColumn, codonSeqColumn), c("ATC", "TTG", "CCC"))

})

test_that("Test getCodonInAlt to get altered stop codon sequence, multiple isoforms", {
  variantsTable <-  data.table(Chr = c(3), Pos = c(105), Ref = c("T"), Alt = c("C"),
                               transcript_ids = c("tran1;tran2;tran3"), stopCodonPos = c("105|104|103;105|109|112;90|105|106"), stopCodonSeq = c("ATT;TTG;CAC"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), start = c(100, 120, 30), end = c(105, 127, 40), strand = c(1,1,-1))
  codonPositionsColumn <- "stopCodonPos"
  transcriptIdColumn <- "transcript_ids"
  codonSeqColumn <- "stopCodonSeq"
  expect_equal(getCodonInAlt(variantsTable, transcriptsTable, transcriptIdColumn, codonPositionsColumn, codonSeqColumn), c("ATC;CTG;CGC"))

})

test_that("Test getCodonInAlt to get altered stop codon sequence, codon positions are not continous", {
  variantsTable <-  data.table(Chr = c(3, 2, 3), Pos = c(98, 110, 45), Ref = c("A", "T", "G"), Alt = c("C", "G", "T"),
                               transcript_ids = c("tran1", "tran2", "tran3"), stopCodonPos = c("105|104|98", "127|110|109", "30|31|45"), stopCodonSeq = c("ATT", "TTG", "CAC"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), start = c(100, 120, 30, 112), end = c(105, 127, 40, 400), strand = c(1,1,-1,-1))
  codonPositionsColumn <- "stopCodonPos"
  transcriptIdColumn <- "transcript_ids"
  codonSeqColumn <- "stopCodonSeq"
  expect_equal(getCodonInAlt(variantsTable, transcriptsTable, transcriptIdColumn, codonPositionsColumn, codonSeqColumn), c("CTT", "TGG", "AAC"))

})

test_that("Test getCodonInAlt to get altered stop codon sequence, corner case deletion", {
  variantsTable <-  data.table(Chr = c(3, 2, 3), Pos = c(102, 127, 31), Ref = c("TAT", "GCC", "T"), Alt = c("T", "G", "G"),
                               transcript_ids = c("tran1", "tran2", "tran3"), stopCodonPos = c("105|104|103", "127|126|125", "30|31|32"), stopCodonSeq = c("ATT", "TTG", "CAC"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), start = c(100, 120, 30, 112), end = c(105, 127, 40, 400), strand = c(1,1,-1,-1))
  codonPositionsColumn <- "stopCodonPos"
  transcriptIdColumn <- "transcript_ids"
  codonSeqColumn <- "stopCodonSeq"
  expect_equal(getCodonInAlt(variantsTable, transcriptsTable, transcriptIdColumn, codonPositionsColumn, codonSeqColumn), c("T", "TTG", "CCC"))

})

test_that("Test getCodonInAlt to get altered stop codon sequence, corner case insertion", {
  variantsTable <-  data.table(Chr = c(3, 2, 3), Pos = c(105, 122, 31), Ref = c("T", "T", "T"), Alt = c("CCC", "G", "TA"),
                               transcript_ids = c("tran1", "tran2", "tran3"), stopCodonPos = c("105|104|103", "127|126|125", "30|31|32"), stopCodonSeq = c("ATT", "TTG", "CAC"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), start = c(100, 120, 30, 112), end = c(105, 127, 40, 400), strand = c(1,1,-1,-1))
  codonPositionsColumn <- "stopCodonPos"
  transcriptIdColumn <- "transcript_ids"
  codonSeqColumn <- "stopCodonSeq"
  expect_equal(getCodonInAlt(variantsTable, transcriptsTable, transcriptIdColumn, codonPositionsColumn, codonSeqColumn), c("ATCCC", "TTG", "CTAC"))

})

test_that("Test getCodonInAlt to get altered start codon sequence", {
  variantsTable <-  data.table(Chr = c(3, 2, 3), Pos = c(105, 122, 31), Ref = c("T", "T", "T"), Alt = c("C", "G", "G"),
                               transcript_ids = c("tran1", "tran2", "tran3"), startCodonPos = c("105|104|103", "127|126|125", "30|31|32"), startCodonSeq = c("ATT", "TTG", "CAC"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), start = c(100, 120, 30, 112), end = c(105, 127, 40, 400), strand = c(1,1,-1,-1))
  codonPositionsColumn <- "startCodonPos"
  transcriptIdColumn <- "transcript_ids"
  codonSeqColumn <- "startCodonSeq"
  expect_equal(getCodonInAlt(variantsTable, transcriptsTable, transcriptIdColumn, codonPositionsColumn, codonSeqColumn), c("ATC", "TTG", "CCC"))

})

test_that("Test getCodonInAlt to get altered start codon sequence, codon positions are not continous", {
  variantsTable <-  data.table(Chr = c(3, 2, 3), Pos = c(104, 146, 31), Ref = c("T", "G", "C"), Alt = c("C", "C", "A"),
                               transcript_ids = c("tran1", "tran2", "tran3"), startCodonPos = c("103|104|110", "121|122|146", "35|32|31"), startCodonSeq = c("ATG", "ATG", "ATG"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), start = c(100, 120, 30, 112), end = c(105, 127, 40, 400), strand = c(1,1,-1,-1))
  codonPositionsColumn <- "startCodonPos"
  transcriptIdColumn <- "transcript_ids"
  codonSeqColumn <- "startCodonSeq"
  expect_equal(getCodonInAlt(variantsTable, transcriptsTable, transcriptIdColumn, codonPositionsColumn, codonSeqColumn), c("ACG", "ATC", "ATT"))

})

test_that("Test getCodonInAlt to get altered start codon sequence, insertion and deletion", {
  variantsTable <-  data.table(Chr = c(3, 2, 3), Pos = c(102, 126, 31), Ref = c("TA", "T", "TGC"), Alt = c("C", "TC", "G"),
                               transcript_ids = c("tran1", "tran2", "tran3"), startCodonPos = c("105|104|103", "127|126|125", "30|31|32"), startCodonSeq = c("ATT", "TTG", "CAC"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), start = c(100, 120, 30, 112), end = c(105, 127, 40, 400), strand = c(1,1,-1,-1))
  codonPositionsColumn <- "startCodonPos"
  transcriptIdColumn <- "transcript_ids"
  codonSeqColumn <- "startCodonSeq"
  expect_equal(getCodonInAlt(variantsTable, transcriptsTable, transcriptIdColumn, codonPositionsColumn, codonSeqColumn), c("TT", "TTCG", "CC"))

})

test_that("Test getCodonInAlt to get altered Kozak sequences", {
  variantsTable <-  data.table(Chr = c(3, 2, 3), Pos = c(104, 146, 31), Ref = c("T", "G", "T"), Alt = c("C", "C", "A"),
                               transcript_ids = c("tran1", "tran2", "tran3"), kozakPos = c("103|104|110|120|121|122|123|127", "104|110|120|121|122|146|147|148", "35|32|31|120|121|122|123|125"), kozakSeq = c("TTCATGCC", "CATATGTG", "TCAATGGA"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), start = c(100, 120, 30, 112), end = c(105, 127, 40, 400), strand = c(1,1,-1,-1))
  codonPositionsColumn <- "kozakPos"
  transcriptIdColumn <- "transcript_ids"
  codonSeqColumn <- "kozakSeq"
  expect_equal(getCodonInAlt(variantsTable, transcriptsTable, transcriptIdColumn, codonPositionsColumn, codonSeqColumn), c("TCCATGCC", "CATATCTG", "TCAATGGT"))

})

test_that("Test getCodonInAlt to get altered Kozak sequences, return NA if ref doesn't match", {
  variantsTable <-  data.table(Chr = c(3, 2, 3), Pos = c(104, 146, 31), Ref = c("G", "G", "C"), Alt = c("C", "C", "A"),
                               transcript_ids = c("tran1", "tran2", "tran3"), kozakPos = c("103|104|110|120|121|122|123|127", "104|110|120|121|122|146|147|148", "35|32|31|120|121|122|123|125"), kozakSeq = c("TTCATGCC", "CATATGTG", "TCAATGGA"))
  transcriptsTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3", "tran4"), start = c(100, 120, 30, 112), end = c(105, 127, 40, 400), strand = c(1,1,-1,-1))
  codonPositionsColumn <- "kozakPos"
  transcriptIdColumn <- "transcript_ids"
  codonSeqColumn <- "kozakSeq"
  expect_equal(getCodonInAlt(variantsTable, transcriptsTable, transcriptIdColumn, codonPositionsColumn, codonSeqColumn), c("NA", "CATATCTG", "NA"))

})


# Test getTSSKozak --------------------------------------------------------

test_that("Test getTSSKozak", {
  variantsTable <-  data.table(Chr = c(3, 2, 3), Pos = c(105, 122, 31), Ref = c("T", "T", "T"), Alt = c("C", "G", "G"),
                               transcript_ids = c("tran1", "tran2", "tran3"))
  utr5SeqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("CTAGCC", "TACCCC", "CATGGG"))
  codingSeqTable <- data.table(ensembl_transcript_id = c("tran1", "tran2", "tran3"), seq = c("ATGCCAT", "ATGTCCAC", "ATGCCAACC"))
  transcriptIdColumn <- "transcript_ids"
  expect_equal(getTSSKozak(variantsTable, utr5SeqTable, codingSeqTable, transcriptIdColumn), c("GCCATGCC", "CCCATGTC", "GGGATGCC"))
})

test_that("Test getTSSKozak will return NA if 5' UTR sequence is unavailable", {
  variantsTable <-  data.table(Chr = c(3), Pos = c(105), Ref = c("T"), Alt = c("C"),
                               transcript_ids = c("tran1"))
  utr5SeqTable <- data.table(ensembl_transcript_id = c("tran1"), seq = c("Sequence unavailable"))
  codingSeqTable <- data.table(ensembl_transcript_id = c("tran1"), seq = c("ATGCCAT"))
  transcriptIdColumn <- "transcript_ids"
  expect_equal(getTSSKozak(variantsTable, utr5SeqTable, codingSeqTable, transcriptIdColumn), c(NA_character_))
})


# Test getKozakPWM -----------------------------------------------------------
test_that("Test getKozakPWM use internal/default uAUG_repressive_strength table and return kozakPWM", {
  kozakPWM <- getKozakPWM()
  expect_equal(nrow(kozakPWM), 4)
  expect_equal(ncol(kozakPWM), 8)
  expect_matrix <- readRDS("testdata/kozakPWM.rds")
  expect_equal(kozakPWM, expect_matrix)
})

test_that("Test getKozakPWM use external specified uAUG_repressive_strength file and return kozakPWM", {
  kozakStrenFile <- "testdata/uAUG_repressive_strength.csv"
  kozakPWM <- getKozakPWM(kozakStrenFile)
  expect_equal(nrow(kozakPWM), 4)
  expect_equal(ncol(kozakPWM), 8)
  expect_matrix <- readRDS("testdata/kozakPWM.rds")
  expect_equal(kozakPWM, expect_matrix)
})


# Test getKozakScore ------------------------------------------------------

test_that("Test getKozakScore", {
  variantsTable <- data.table(Chr = c(3, 2, 3, 2, 5), Pos = c(105, 122, 31, 110, 40), Ref = c("T", "T", "T", "G", "A"), Alt = c("C", "G", "G", "A", "C"),
                              kozak = c("ACCATGCC", NA, "ATGCCGG", "NA;NA", ""))
  kozakPWM <- getKozakPWM()
  kozakSeqs <- "kozak"
  scores <- getKozakScore(variantsTable, kozakSeqs, kozakPWM)
  expect_true(scores[1] > scores[3])
  expect_true(is.na(scores[2]))
})

test_that("Test getKozakScore corner case  all kozak sequences are empty", {
  variantsTable <- data.table(kozak = c("GACATGGG;GACATGGG;GACATGGG;GACATGGG", ";;;"))
  kozakPWM <- getKozakPWM()
  kozakSeqs <- "kozak"
  scores <- getKozakScore(variantsTable, kozakSeqs, kozakPWM)
  expect_equal(scores[1], "0.968574339078244;0.968574339078244;0.968574339078244;0.968574339078244")
  expect_equal(scores[2], "0;0;0;0")
})

