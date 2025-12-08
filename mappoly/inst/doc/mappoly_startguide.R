## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 4, 
  fig.height= 4,
  eval = FALSE
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----load_data----------------------------------------------------------------
#  library(mappoly)
#  file.name <- system.file("extdata/potato_example.csv", package = "mappoly")
#  dat <- read_geno_csv(file.in = file.name, ploidy = 4)
#  print(dat, detailed = T)
#  plot(dat)

## ----eval=FALSE, filter_ind_donot_eval----------------------------------------
#  dat <- filter_individuals(dat)

## ----filter-------------------------------------------------------------------
#  dat <- filter_missing(dat, type = "marker", filter.thres = .05)
#  dat <- filter_missing(dat, type = "individual", filter.thres = .05)

## ----filter_seg---------------------------------------------------------------
#  seq.filt <- filter_segregation(dat, chisq.pval.thres = 0.05/dat$n.mrk)
#  seq.filt <- make_seq_mappoly(seq.filt)
#  seq.red  <- elim_redundant(seq.filt)

## ----make_seq-----------------------------------------------------------------
#  seq.init <- make_seq_mappoly(seq.red)
#  plot(seq.init)

## ----geno_ord-----------------------------------------------------------------
#  go <- get_genomic_order(input.seq = seq.init) ## get genomic order of the sequence
#  plot(go)

## ----twopt_show, eval=FALSE---------------------------------------------------
#  ncores <- parallel::detectCores() - 1
#  tpt <- est_pairwise_rf2(seq.init, ncpus = ncores)
#  m <- rf_list_to_matrix(tpt) ## converts rec. frac. list into a matrix
#  sgo <- make_seq_mappoly(go) ## creates a sequence of markers in the genome order
#  plot(m, ord = sgo, fact = 5) ## plots a rec. frac. matrix using the genome order, averaging neighbor cells in a 5 x 5 grid

## ----group--------------------------------------------------------------------
#  g <- group_mappoly(m, expected.groups = 12, comp.mat = TRUE)
#  plot(g)
#  g

## ----select_group-------------------------------------------------------------
#  s1 <- make_seq_mappoly(g, 1, ## Select LG1
#                         genomic.info = 1)
#  m1 <- make_mat_mappoly(m, s1)

## ----order_mds----------------------------------------------------------------
#  mds.o1 <- mds_mappoly(input.mat = m1)
#  s1.mds <- make_seq_mappoly(mds.o1)
#  plot(m1, ord = s1.mds)

## ----order_genome-------------------------------------------------------------
#  gen.o1 <- get_genomic_order(s1)
#  s1.gen <- make_seq_mappoly(gen.o1)
#  plot(m1, ord = s1.gen)

## ----map_lg1, results = FALSE, eval = FALSE-----------------------------------
#  tpt1 <- est_pairwise_rf(s1.mds, ncpus = ncores)
#  lg1.map <- est_rf_hmm_sequential(input.seq = s1.mds,
#                                  start.set = 3,
#                                  thres.twopt = 10,
#                                  thres.hmm = 20,
#                                  extend.tail = 50,
#                                  info.tail = TRUE,
#                                  twopt = tpt1,
#                                  sub.map.size.diff.limit = 5,
#                                  phase.number.limit = 20,
#                                  reestimate.single.ph.configuration = TRUE,
#                                  tol = 10e-3,
#                                  tol.final = 10e-4)

## ----map_lg1_plot-------------------------------------------------------------
#  print(lg1.map)
#  plot(lg1.map, mrk.names = TRUE, cex = 0.7)

## ----map_err_lg1, results='hide'----------------------------------------------
#  lg1.map.up <- est_full_hmm_with_global_error(input.map = lg1.map, error = 0.05,
#                                               verbose = TRUE)
#  plot(lg1.map.up, mrk.names = TRUE, cex = 0.7)

## ----map_reest, results='hide'------------------------------------------------
#  lg1.map.ols <- reest_rf(lg1.map, m1, method = "ols")
#  lg1.map.mds <- reest_rf(lg1.map, m1, method = "wMDS_to_1D_pc", input.mds = mds.o1)

## ----map_list,  results='hide'------------------------------------------------
#  map.list.lg1  <- list(orig = lg1.map,
#                        updt = lg1.map.up,
#                        ols = lg1.map.ols,
#                        mds = lg1.map.mds)
#  plot_map_list(map.list.lg1, col = "ggstyle", title = "Estimation method")

## ----homolog_p----------------------------------------------------------------
#  g1 <- calc_genoprob_error(lg1.map.up, step = 1, error = 0.05)
#  to.qtlpoly <- export_qtlpoly(g1) #export to QTLpoly
#  h1 <- calc_homologprob(g1)
#  plot(h1, lg = 1, ind = 10)

## ----meiosis_evaluation-------------------------------------------------------
#  p1 = calc_prefpair_profiles(g1)
#  plot(p1, min.y.prof = 0.25, max.y.prof = 0.4, P1 = "Atlantic", P2 = "B1829.5")

## ----eval=FALSE---------------------------------------------------------------
#  export_map_list(lg1.map.up, file = "output_file.csv")

## -----------------------------------------------------------------------------
#  in.file <- "https://github.com/mmollina/SCRI/raw/main/docs/tetra/maps_updated.rda"
#  map_file <- tempfile()
#  download.file(in.file, map_file)
#  load(map_file)

## -----------------------------------------------------------------------------
#  plot_genome_vs_map(MAPs.up, same.ch.lg = TRUE)

## -----------------------------------------------------------------------------
#  summary_maps(MAPs.up)

