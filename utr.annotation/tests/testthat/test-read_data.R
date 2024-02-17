
context("Testing read data module")
skip_on_cran()

# Test readVariantData ----------------------------------------------------

test_that("Test readVariantData will stop when missing required columns", {
  input <- tempfile()
  write_csv(data.frame(Pos = c(100,200,300), Ref = c("A","T","T"),
                       Alt = c("C", "A", "G"), Consequence = c("upstream","downstream","intro")),input)
  expect_error(readVariantData(input), "Missing required column: Chr doesn't exist in the input!")
})

test_that("Test readVariantData will output a data table if input have Chr, Pos, Ref, Alt columns", {
  valid_input <- tempfile()
  write_csv(data.frame(Chr = c(1,2,3), Pos = c(100,200,300), Ref = c("A","T","T"),
                       Alt = c("C", "A", "G")), valid_input)
  varsDt <- readVariantData(valid_input)
  expect_true(is.data.table(varsDt))
  expect_true(all(c("Chr", "Pos", "Ref", "Alt") %in% colnames(varsDt)))
})


test_that("Test readVariantData will ok when have Transcript column but not Gene column", {
  input <- tempfile()
  write_csv(data.frame(Chr = c(1,2,3), Pos = c(100,200,300), Ref = c("A","T","T"),
                       Alt = c("C", "A", "G"), Transcript = c("ens1", "ens2", "ens3")),input)
  varsDt <- readVariantData(input)
  expect_true(is.data.table(varsDt))
  expect_equal(nrow(varsDt), 3)
})

test_that("Test readVariantData will split rows when there're multiple items in any of the Chr, Pos, Ref, Alt columns", {
  input <- tempfile()
  write_csv(data.frame(Chr = c(1,2,3), Pos = c(100,200,300), Ref = c("A","T","T"),
                       Alt = c("C,G", "A", "G"), Gene = c("ens1", "ens2", "ens3"), Consequence = c("upstream","downstream","intro")), input)
  expect_warning(readVariantData(input), "Multiple items in one or more columns: Alt")
  variants <- readVariantData(input)
  expect_true(is.data.table(variants))
  expect_equal(nrow(variants), 4)
  expect_equal(variants, data.table(Chr = c(1,1,2,3), Pos = c(100,100,200,300), Ref = c("A", "A", "T", "T"),
                                    Alt = c("C", "G", "A", "G"), Gene = c("ens1", "ens1", "ens2", "ens3"), Consequence = c("upstream","upstream", "downstream","intro")))
})

test_that("Test readVariantData will not split rows when there're multiple items in Transcript column", {
  input <- tempfile()
  write_csv(data.frame(Chr = c(1,2,3), Pos = c(100,200,300), Ref = c("A","T","T"),
                       Alt = c("C", "A", "G"), Transcript = c("tran1;tran4", "tran2", "tran3;tran6;tran7")), input)
  varsDt <- readVariantData(input)
  expect_true(is.data.table(varsDt))
  expect_equal(nrow(varsDt), 3)
})

test_that("Test readVariantData will not stop when there're multiple items in a column which is not Chr, Pos, Ref, Alt", {
  valid_input <- tempfile()
  write_csv(data.frame(Chr = c(1,2,3), Pos = c(100,200,300), Ref = c("A","T","T"),
                       Alt = c("C", "A", "G"), Gene = c("ens1,ens4", "ens2", "ens3"), Consequence = c("upstream,intro","downstream","intro")), valid_input)
  varsDt <- readVariantData(valid_input)
  expect_true(is.data.table(varsDt))
  expect_equal(nrow(varsDt), 3)
  expect_true(all(c("Chr", "Pos", "Ref", "Alt", "Gene", "Consequence") %in% colnames(varsDt)))
})

test_that("Test readVariantData can read from VCF format and split rows if there're multiple items in Alt", {
  variants <- readVariantData("testdata/vcf_multialts.vcf", format = "vcf")
  expect_true(is.data.table(variants))
  expect_equal(variants$Chr, c("1", "1", "1", "1", "1"))
  expect_equal(variants$Pos, c("3559180", "3559180", "3539056", "3539056", "3510736"))
  expect_equal(variants$Ref, c("ATCTCTC", "ATCTCTC", "GCTCTCTCTCT", "GCTCTCTCTCT", "T"))
  expect_equal(variants$Alt, c("ATC", "A", "GCT", "G", "TA"))
})


# Test readVCFData --------------------------------------------------------

test_that("Test readVCFData read VCF and return a data table", {
  variants <- readVCFData("testdata/vcf_multialts.vcf")
  expect_true(is.data.table(variants))
  expect_equal(variants$Chr, c("1", "1", "1"))
  expect_equal(variants$Pos, c("3559180", "3539056", "3510736"))
  expect_equal(variants$Ref, c("ATCTCTC", "GCTCTCTCTCT", "T"))
  expect_equal(variants$Alt, c("ATC,A", "GCT,G", "TA"))
})

# Test splitRowsIfMultiFeature -------------------------------------------
test_that("Test splitRowsIfMultiFeatures works if there're multiple sequences in Alt columns", {
  variants <- readVCFData("testdata/vcf_multialts.vcf")
  final_table <- splitRowsIfMultiFeature(variants, "Alt")
  expect_equal(nrow(final_table), 5)
  expect_true(final_table[1]$Alt == "ATC")
  expect_true(final_table[2]$Alt == "A")
  expect_true(final_table[3]$Alt == "GCT")
  expect_true(final_table[4]$Alt == "G")
  expect_true(final_table[5]$Alt == "TA")
})

test_that("Test splitRowsIfMultiFeatures works if there're multiple sequences in Ref and Alt columns", {
  variants <- data.table(Chr = c("1", "2"), Pos = c("100", "200"), Ref = c("A,A", "G,C,T"), Alt = c("ATC,T", "A,T,C"))
  final_table <- splitRowsIfMultiFeature(variants, c("Ref", "Alt"))
  expect_equal(nrow(final_table), 5)
  expect_true(final_table[1]$Ref == "A" && final_table[1]$Alt == "ATC")
  expect_true(final_table[2]$Ref == "A" && final_table[2]$Alt == "T")
  expect_true(final_table[3]$Ref == "G" && final_table[3]$Alt == "A")
  expect_true(final_table[4]$Ref == "C" && final_table[4]$Alt == "T")
  expect_true(final_table[5]$Ref == "T" && final_table[5]$Alt == "C")
})


test_that("Test splitRowsIfMultiFeatures works if there're multiple sequences in Ref and Alt columns with ; separator", {
  variants <- data.table(Chr = c("1", "2"), Pos = c("100", "200"), Ref = c("A;A", "G;C;T"), Alt = c("ATC;T", "A;T;C"))
  final_table <- splitRowsIfMultiFeature(variants, c("Ref", "Alt"), ";")
  expect_equal(nrow(final_table), 5)
  expect_true(final_table[1]$Ref == "A" && final_table[1]$Alt == "ATC")
  expect_true(final_table[2]$Ref == "A" && final_table[2]$Alt == "T")
  expect_true(final_table[3]$Ref == "G" && final_table[3]$Alt == "A")
  expect_true(final_table[4]$Ref == "C" && final_table[4]$Alt == "T")
  expect_true(final_table[5]$Ref == "T" && final_table[5]$Alt == "C")
})

test_that("Test splitRowsIfMultiFeatures works if there're multiple sequences in Alt columns and multiple gene/transcript/consequence in CSQ will stay the same", {
  variants <- data.table(Chr = c("1", "2"), Pos = c("100", "200"), Ref = c("A", "G"), Alt = c("C,T", "A,T,C"),
                         Gene = c("gene1,gene4", "gene2,gene3"), Transcript = c("tran1,tran2", "tran3,tran4"), Consequence = c("intron,intron", "utr5,cds"))
  final_table <- splitRowsIfMultiFeature(variants, "Alt")
  expect_equal(nrow(final_table), 5)
  expect_splitted_table <- data.table(Chr = c(rep("1",2), rep("2",3)), Pos = c(rep("100",2), rep("200", 3)), Ref = c(rep("A", 2), rep("G", 3)),
                                      Alt = c("C", "T", "A", "T", "C"), Gene = c(rep("gene1,gene4", 2), rep("gene2,gene3", 3)),
                                      Transcript = c(rep("tran1,tran2", 2), rep("tran3,tran4", 3)),
                                      Consequence = c(rep("intron,intron", 2), rep("utr5,cds", 3)))
 expect_true(all.equal(final_table, expect_splitted_table, check.attributes = F))
})




