test_that("merged allele frequencies can be loaded",{

  correct_answer <-  data.frame(seqnames = "chr2L", start = 10000208, end=10000208, width =1, strand = "*", name = 0, score = 0, ref = "G", alt = "A" , selected_parent_count  = 36, selected_parent_alt_af= 0.277778, backcrossed_parent_count = 16, backcrossed_parent_alt_af=0, offspring_count=10, offspring_alt_af =0) %>% GenomicRanges::GRanges()

  merged_frequencies.filename <- system.file("extdata", "merged_frequencies.example_data.tbl", package = "PopPsiSeqR")
  expect_no_error(frequencies.bg <- import.freqtbl(merged_frequencies.filename))

  expect_equal(correct_answer, head(frequencies.bg, n=1), ignore_attr=TRUE)

})


test_that("shifted frequencies can be saved",{

  shift2save <- data.frame(seqnames="chr2L",start=10000208,end=10000208,width=1,strand="*",score=0,name=0,ref="G",alt="A",selected_parent_count=36,selected_parent_alt_af=0.277778,backcrossed_parent_count=16,backcrossed_parent_alt_af=0,offspring_count=10,offspring_alt_af=0,backcrossed_parent_introg_deltaF=0,selected_parent_depletion_deltaF=0.277778,central=0.138889,mean_oriented_shift=-0.138889,max_oriented_shift=0.861111,min_oriented_shift=-0.138889,AF_difference=0.277778) %>% GenomicRanges::GRanges()
  shift_frequencies.filename <-  withr::local_tempfile(pattern = "shiftWriteTest-")
  expect_no_error(PopPsiSeqR::export.freqshft(shift2save,shift_frequencies.filename))

  #correct_hash = "e37492c5b6b3396449ec70e2ae27862a"
  #expect_equal(correct_hash, tools::md5sum(shift_frequencies.filename)[[1]])
  correct_hashes = c("e37492c5b6b3396449ec70e2ae27862a","8f3feb89facba1da64e34533b8a0b618") # windows compatibility
  expect_contains(correct_hashes, tools::md5sum(shift_frequencies.filename)[[1]])

})




test_that("windowed shifts can be loaded",{

  windowed_shifts.filename <- system.file("extdata", "windowed_shifts.example_data.bed", package = "PopPsiSeqR")
  expect_no_error( windowed_shifts.bg <-import.smvshift(windowed_shifts.filename) )
  expect_no_error( windowed_shifts.bg <-windowed_shifts.bg %>% as.data.frame() )

  check_line <- windowed_shifts.bg[220, ]
  row.names(check_line) <- "yes"

  correct_answer <-  data.frame( seqnames=factor("chr2L"),start=21900001,end=22000000,width=100000,strand=factor("*", levels = c("+", "-", "*")),name=220,sum_sim_deltaF=-0.575,sum_sec_deltaF=58.4666700,sum_simward_AFshift=-28.9458300,max_simward_AFshift=0.634890600,min_simward_AFshift=-0.365109400,sim_sec_difference=0.596380500,num_snp=99,avg_sim_deltaF=-0.005808081,avg_sec_deltaF=0.5905724,avg_simward_AFshift=-0.2923822,win=220 )
  row.names(correct_answer) <- "yes"


  expect_equal(correct_answer,  check_line, tolerance =  0.000001)

})
