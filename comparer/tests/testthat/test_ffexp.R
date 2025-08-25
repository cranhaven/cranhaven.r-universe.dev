context("ffexp")

test_that("test tempdir on Travis", {
  # print("getting a tempdir now")
  td <- paste0(tempdir(), "//ffexptmpdir")
  if (!dir.exists(td)) {
    dir.create(td)
  }
  # print(paste("tempdir is", td))
  # print("going to save a temp file")
  tf <- paste0(td, "//", "test.rds")
  # print(paste('temp file path is', tf))
  saveRDS(1:10, tf)
  # print("saved a file, does it exist")
  # print(file.exists(tf))
  expect_true(file.exists(tf))
  # print("done checking Travis and tempdir")
})

test_that("ffexp", {
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               nulleff=c(0,1),
                               eval_func=function(n, nulleff) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))}
  ), NA)
  expect_is(f1, "R6")
  expect_is(f1, "ffexp")
  expect_error(f1$run_all(verbose=0), NA)

  prt <- f1$plot_run_times()
  expect_is(prt, "gg")
  pp <- f1$plot_pairs()
  if (requireNamespace("GGally", quietly = TRUE)) {
    expect_is(pp, "gg")
  } else {
    expect_true(is.null(pp))
  }
  calc.eff <- f1$calculate_effects()
  expect_is(calc.eff, "list")
  expect_true(length(calc.eff) == 2)
  calc.eff2 <- f1$calculate_effects2()
  expect_is(calc.eff2, "numeric")
  expect_true(length(calc.eff) == 2)

  # Print
  t1 <- capture.output(f1)
  expect_is(t1, "character")
  # 5 lines beforre finished, 4 after finished
  expect_length(t1, 4)

  # rungrid2
  t2 <- f1$rungrid2()
  expect_is(t2, "data.frame")
  expect_equal(dim(t2), c(6, 2))
  t3 <- f1$rungrid2(rows=c(2,5,6)) # Select out rows
  expect_is(t3, "data.frame")
  expect_equal(dim(t3), c(3, 2))
  expect_true(all(t3 == t2[c(2,5,6),]))

  # Delete at end
  expect_error({rm(f1); gc()}, NA)
})

test_that("ffexp parallel", {
  # Check that parallel cores can be set, but don't run
  # because this will be run on Travis
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               nulleff=c(0,1),
                               eval_func=function(n, nulleff) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))},
                               parallel=T, parallel_cores=137,
                               save_output = F), NA)
  expect_equal(f1$parallel_cores, 137)
  # Skip tests with parallel=T on Travis, gives error
  testthat::skip_on_travis()

  folder <- paste0(tempdir(),"//ffexp3//")
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               nulleff=c(0,1),
                               eval_func=function(n, nulleff) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))},
                               parallel=T, parallel_cores=1,
                               save_output = T, folder_path=folder,
                               verbose=0
  ), NA)
  expect_error(f1$run_all(parallel_temp_save = T, write_start_files = T, write_error_files = T,
                          delete_parallel_temp_save_after = F, verbose=0), NA)
  expect_error(f1$save_self(), NA)
  expect_error(f1$recover_parallel_temp_save(), NA)

  # Delete at end
  for (tmpfile in list.files(f1$folder_path)) {
    unlink(paste0(f1$folder_path, tmpfile))
  }
  unlink(f1$folder_path, recursive=T)
  expect_error({rm(f1); gc()}, NA)
})


test_that("ffexp parallel 2", {
  # Skip tests with parallel=T on Travis, gives error
  testthat::skip_on_travis()
  # Was getting error on Linux/Mac b/c of folder path \\ instead of //
  # testthat::skip_on_os(c("linux", "mac", "solaris"))

  folder <- paste0(tempdir(),"//")
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               nulleff=c(0,1),
                               eval_func=function(n, nulleff) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))},
                               parallel=T, parallel_cores=1,
                               save_output = T, folder_path=folder
  ), NA)
  expect_error(f1$run_all(parallel_temp_save = T, write_start_files = T,
                          write_error_files = T,
                          delete_parallel_temp_save_after = T, verbose=0), NA)
  # expect_error(f1$save_self(), NA)
  expect_error(f1$delete_save_folder_if_empty(), NA)
  # Delete at end
  expect_error({rm(f1); gc()}, NA)
})

test_that("ffexp with runone", {
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               tdf=data.frame(a=1:2,b=3:4),
                               nulleff=c(0,1),
                               eval_func=function(n, nulleff, a, b) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))}
  ), NA)
  expect_is(f1, "R6")
  expect_is(f1, "ffexp")
  expect_error(f1$run_one(1, warn_repeat=F, verbose=0), NA)
  expect_warning(f1$run_one(1, warn_repeat=T, verbose=0))
  expect_error(f1$run_one(1:3, warn_repeat=F, verbose=0), NA)

  # Give bad run_order
  expect_error(f1$run_all(run_order = "reversebackwards", redo = T, verbose=0))
  expect_error(f1$run_all(run_order = "reverse", redo = T, warn_repeat=F, verbose=0), NA)

  prt <- f1$plot_run_times()
  expect_is(prt, "gg")
  calc.eff <- f1$calculate_effects()
  expect_is(calc.eff, "list")
  expect_true(length(calc.eff) == 3)

  # Delete at end
  expect_error({rm(f1); gc()}, NA)
})

test_that("ffexp parallel detect cores, don't run", {
  folder <- paste0(tempdir(),"\\")
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               nulleff=c(0,1),
                               eval_func=function(n, nulleff) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))},
                               parallel=T, parallel_cores="detect",
                               save_output = T, folder_path=folder
  ), NA)
  # Delete at end
  expect_error({rm(f1); gc()}, NA)
  # rm(f1)
})
test_that("ffexp parallel detect cores - 1, don't run", {
  folder <- paste0(tempdir(),"\\")
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               nulleff=c(0,1),
                               eval_func=function(n, nulleff) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))},
                               parallel=T, parallel_cores="detect-1",
                               save_output = T, folder_path=folder
  ), NA)
  # Delete at end
  expect_error({rm(f1); gc()}, NA)
  # rm(f1)
})




test_that("ffexp with list", {
  expect_error(f1 <- ffexp$new(x=list(a=list(r=1,s=2,t=3),b=list(r=4,s=6,t=8)),
                               y=1:4,
                               eval_func=function(a, b, y) {
                                 samp <- rnorm(b)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(b))}
  ), NA)
  expect_error(f1$run_all(run_order = "reverse", redo = T, verbose=0), NA)

  prt <- f1$plot_run_times()
  expect_is(prt, "gg")
  calc.eff <- f1$calculate_effects()
  expect_is(calc.eff, "list")
  expect_true(length(calc.eff) == 2)

  # Delete at end
  expect_error({rm(f1); gc()}, NA)
})

test_that("ffexp with error, only when parallel is available", {
  # Skip tests with parallel=T on Travis, gives error
  testthat::skip_on_travis()
  if (requireNamespace("parallel", quietly = TRUE) &&
      requireNamespace("snow", quietly = TRUE)) {
    expect_error(f1 <- ffexp$new(x=list(a=list(r=1,s=2,t=3),b=list(r=4,s=6,t=8)),
                                 y=1:4,
                                 eval_func=function(a, b, y) {
                                   if (y>2) stop()
                                   samp <- rnorm(b)
                                   data.frame(mean=mean(samp), se=sd(samp)/sqrt(b))},
                                 save_output=T,parallel = T, parallel_cores = 1,
                                 folder_path = paste0(tempdir(), "//ffexp5//")
    ), NA)
    expect_error(f1$run_all(run_order = "inorder", redo = T, parallel_temp_save = T,
                            write_error_files=T, verbose=0))
    expect_true(sum(f1$completed_runs)==0)
    # print(paste('folder path is', f1$folder_path))
    # print(list.files(f1$folder_path))
    expect_error(f1$recover_parallel_temp_save(delete_after = T), NA)
    # print(list.files(f1$folder_path))
    # Sys.sleep(2)
    # print(f1)
    # print(paste("Completed runs is", sum(f1$completed_runs)))
    expect_true(sum(f1$completed_runs)==6)

    prt <- f1$plot_run_times()
    expect_is(prt, "gg")
    calc.eff <- f1$calculate_effects()
    expect_is(calc.eff, "list")
    expect_true(length(calc.eff) == 2)

    # Delete at end
    for (tmpfile in list.files(f1$folder_path)) {
      unlink(paste0(f1$folder_path, tmpfile))
    }
    unlink(f1$folder_path, recursive=T)
    expect_error({rm(f1); gc()}, NA)
  }
})



test_that("ffexp with function input", {
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               nulleff=c(0,1),
                               functi=sin,
                               eval_func=function(n, nulleff, functi) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n), functi_0=functi(0))}
  ), NA)
  expect_is(f1, "R6")
  expect_is(f1, "ffexp")
  expect_error(f1$run_one(verbose=0), NA)
  expect_error(f1$run_all(verbose=0), NA)
  expect_error(f1$run_all(verbose=0), NA)
  expect_error(f1$run_one(verbose=0))

  prt <- f1$plot_run_times()
  expect_is(prt, "gg")
  calc.eff <- f1$calculate_effects()
  expect_is(calc.eff, "list")
  expect_true(length(calc.eff) == 3)

  # Delete at end
  expect_error({rm(f1); gc()}, NA)
})

test_that("ffexp with single row, rungrid2 was giving error", {
  f1 <- ffexp$new(
    df=data.frame(a=1:4,b=5:8,c=letters[1:4], stringsAsFactors = F),
    eval_func=function(a,b,c) {
      a^2
    }
  )
  expect_equal(class(f1$rungrid2()[,3]), "character")
  expect_error(f1$rungrid2(), NA)
  expect_equal(colnames(f1$rungrid2()), c('a', 'b', 'c'))

  expect_equal(f1$rungrid2(rows=c(1,3)), f1$rungrid2()[c(1,3),],
               check.attributes=FALSE) # Weird row names issue
  expect_equal(f1$rungrid2(rows=c(1,3)), f1$rungrid2(rows=c(T,F,T)),
               check.attributes=FALSE) # Weird row names issue
  # f1$run_all()
  # Delete at end
  expect_error({rm(f1); gc()}, NA)
})

test_that("Test add_variable", {
  f1 <- ffexp$new(n=c(100, 1000, 10000),
                  nulleff=c(0,1),
                  eval_func=function(n, nulleff) {
                    samp <- rnorm(n)
                    data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))}
  )
  expect_error(f1$run_all(run_order="random", verbose=0), NA)
  expect_error(f1$run_all(run_order="random", to_run=3, warn_repeat = F, verbose=0), NA)
  # Get error if try to use same name as existing
  expect_error(f1$add_variable("nulleff"))
  # Add numeric vector
  g1 <- f1$add_variable("newvar", 0, 1:2)
  expect_equal(sum(f1$completed_runs), sum(g1$completed_runs))
  expect_equal(length(g1$completed_runs), 3*length(f1$completed_runs))
  # Add data frame
  g1 <- f1$add_variable("newdf", data.frame(a="x",b=44), data.frame(a=c('y','z'), b=45:46))
  expect_equal(sum(f1$completed_runs), sum(g1$completed_runs))
  expect_equal(length(g1$completed_runs), 3*length(f1$completed_runs))

  # Delete at end
  expect_error({rm(f1, g1); gc()}, NA)
})

# Add level ----
test_that("Test add_level", {
  f1 <- ffexp$new(n=c(100, 1000, 10000),
                  nulleff=c(0,1),
                  eval_func=function(n, nulleff) {
                    samp <- rnorm(n)
                    data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))}
  )
  f1$run_all(to_run = c(1,4), verbose=0)
  # f1$run_all(verbose=0)
  expect_error(f1$add_level("notavar", 2))
  g1 <- f1$add_level("nulleff", 2)
  expect_equal(9, length(g1$completed_runs))
  expect_equal(sum(f1$completed_runs), sum(g1$completed_runs))
  expect_equal(length(g1$completed_runs), 3 + length(f1$completed_runs))
  expect_equal(nrow(g1$outcleandf), 9)

  # Add two new levels
  g1$run_all(verbose=0)
  expect_equal(9, sum(g1$completed_runs))
  h1 <- g1$add_level("nulleff", c(3,4))
  expect_equal(15, length(h1$completed_runs))
  expect_equal(sum(g1$completed_runs), sum(h1$completed_runs))
  expect_equal(length(h1$completed_runs), 9 + length(f1$completed_runs))

  # Delete at end
  expect_error({rm(f1, g1, h1); gc()}, NA)
})

test_that("Test add_level with data frame", {
  f1 <- ffexp$new(df=data.frame(n=c(100, 1000, 10000),
                                nulleff=c(0,1,2)),
                  eval_func=function(n, nulleff) {
                    samp <- rnorm(n)
                    data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))}
  )
  f1$run_all(verbose=0)
  g1 <- f1$add_level("df", data.frame(n=20, nulleff=3))
  expect_equal(4, length(g1$completed_runs))
  expect_equal(sum(f1$completed_runs), sum(g1$completed_runs))
  expect_equal(length(g1$completed_runs), 1 + length(f1$completed_runs))

  # Add two new levels
  g1$run_all(verbose=0)
  expect_equal(4, sum(g1$completed_runs))
  h1 <- g1$add_level("df", data.frame(n=c(30,40),
                                      nulleff=c(4,5)))
  expect_equal(6, length(h1$completed_runs))
  expect_equal(sum(g1$completed_runs), sum(h1$completed_runs))
  expect_equal(length(h1$completed_runs), 3 + length(f1$completed_runs))

  # Add another level without running the previous
  expect_equal(4, sum(h1$completed_runs))
  i1 <- h1$add_level("df", data.frame(n=c(50),
                                      nulleff=c(6)))
  expect_equal(7, length(i1$completed_runs))
  expect_equal(sum(h1$completed_runs), sum(i1$completed_runs))
  expect_equal(length(i1$completed_runs), 4 + length(f1$completed_runs))


  # Delete at end
  expect_error({rm(f1, g1, h1, i1); gc()}, NA)
})

test_that("ffexp with factor input", {
  f1 <- ffexp$new(n=factor(c(100, 1000, 10000)),
                  nulleff=factor(c(0,1)),
                  eval_func=function(n, nulleff) {
                    samp <- rnorm(n)
                    data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))}
  )
  expect_is(f1$arglist[[1]], "factor")
  expect_is(f1$arglist[[2]], "factor")
  f1$run_all(verbose=0)
  expect_true(all(f1$completed_runs))
  expect_error(f1$rungrid2(), NA)
  expect_equal(colnames(f1$rungrid2()), c('n', 'nulleff'))
  # Delete at end
  expect_error({rm(f1); gc()}, NA)
})

test_that("ffexp with factor input and df multirow output", {
  f1 <- ffexp$new(a=1:2,b=c("b",'c'), c=factor(c(5,6)),
                  eval_func=function(a,b,c) {tibble::tibble(a=1:3,b=letters[1:3])})
  expect_equal(length(f1$arglist), 3)
  expect_is(f1$arglist[[3]], "factor")
  expect_error(f1$run_all(verbose=0), NA)
  expect_true(all(f1$completed_runs))
  expect_error(f1$rungrid2(), NA)
  # Delete at end
  expect_error({rm(f1); gc()}, NA)
})

test_that("ffexp with single df input and df multirow output", {
  f1 <- ffexp$new(data.frame(a=1:2,b=c("b",'c'), c=factor(c(5,6))),
                  eval_func=function(a,b,c) {tibble::tibble(a=1:3,b=letters[1:3])})
  expect_equal(length(f1$arglist), 1)
  expect_is(f1$arglist[[1]][,3], "factor")
  expect_error(f1$run_all(verbose=0), NA)
  expect_true(all(f1$completed_runs))
  expect_error(f1$rungrid2(), NA)
  expect_equal(colnames(f1$rungrid2()), c('a','b','c'))
  # Delete at end
  expect_error({rm(f1); gc()}, NA)
})

test_that("length one vector output unnamed, df single col input", {
  f1 <- ffexp$new(data.frame(a=1:2),
                  eval_func=function(a) {a^2})
  expect_error(f1$run_all(verbose=0), NA)
  f1$outcleandf
  expect_equal(names(f1$outcleandf)[1:2], c("a", "V1"))
})

test_that("vector output", {
  f1 <- ffexp$new(data.frame(a=1:2,b=c("b",'c'), c=factor(c(5,6))),
                  eval_func=function(a,b,cc) {c(a=a, b=b, c=cc)})
  expect_error(f1$run_all(verbose=0), NA)
  f1$outcleandf
  expect_equal(names(f1$outcleandf)[1:6], c("a", "b", "c", "a", "b", "c"))
})

test_that("list output", {
  c1 <- ffexp$new(
    # save_output=T,
    eval_func=function(i) {list(i, i:10, list(i, i^2, i^3), list(data.frame(1:5, 2:6)),
                                tibble::tibble(11:5))},
    i=1:3
  )
  expect_warning(c1$run_all(verbose=0), NA)
  # Shouldn't have columns from list
  expect_equal(ncol(c1$outcleandf), 5)
})

test_that("no output", {
  f1 <- ffexp$new(data.frame(a=1:2,b=c("b",'c'), c=factor(c(5,6))),
                  eval_func=function(a,b,cc) {})
  expect_error(f1$run_all(verbose=0), NA)
  f1$outcleandf
})
test_that("input is df with single column, use name of column not df", {
  f1 <- ffexp$new(x=data.frame(a=1:5,b=c('c'), c=factor(c(5))),
                  eval_func=function(a,b,cc) {a})
  expect_equal(colnames(f1$rungrid2()), c('a','b','c'))
  # FIX1COLDF
  f1 <- ffexp$new(x=data.frame(a=1:5),
                  eval_func=function(a) {a})
  expect_equal(colnames(f1$rungrid2()), c('a'))
  f1$outcleandf
})
test_that("verbose", {
  f1 <- ffexp$new(data.frame(a=1:2,b=c("b",'c'), c=factor(c(5,6))),
                  eval_func=function(a,b,cc) {}, verbose=0)
  expect_equal(f1$verbose, 0)
  expect_error(f1$run_all(), NA)
  expect_error(c1 <- capture.output(f1$run_all()), NA)
  expect_equal(length(c1), 0)
  expect_error(c1 <- capture.output(f1$run_all(verbose=1, redo=T, warn_repeat = F)), NA)
  # expect_equal(length(c1), 2)
  f1$verbose <- 1
  expect_error(c1 <- capture.output(f1$run_all(redo=T, warn_repeat = F)), NA)
  # expect_equal(length(c1), 2)
  f1$verbose <- 2
  expect_error(c1 <- capture.output(f1$run_all(redo=T, warn_repeat = F)), NA)
  # Output is 22 or 24 lines, depends on system
  # expect_true(length(c1) >= 20)
})
# test_that("print isn't too long", {
#   # Need to run >=201 to get print to be shortened, and must be in parallel,
#   # too slow and can't run on Travis.
#   testthat::skip_on_travis()
#   testthat::skip() # Too slow, not worth it.
#   f1 <- ffexp$new(a=1:201, eval_func=function(a) {a})
#   c1 <- capture.output({f1$run_all(parallel=T, parallel_cores = 1)})
#   expect_true(nchar(paste(c1, collapse=' ')) < 300)
# })
test_that("run_all not parallel keeps going after error", {
  c1 <- ffexp$new(
    eval_func=function(i) {if (i<5) {1/'a'} else {5}},
    i=1:5
    , parallel=F
  )
  # Not actually testing that it gets to all, but it should.
  expect_error(c1$run_all(verbose=0))
  expect_true(all(c1$completed_runs == c(F,F,F,F,T)))
})
test_that('test that run_one saving output is saving proper output', {
  # Skip tests with parallel=T on Travis, gives error
  testthat::skip_on_travis()
  f1 <- ffexp$new(a=1:4,
                  eval_func=function(a,b,cc) {}, verbose=2)
  f1$run_all(to_run = 1, parallel_temp_save = T, parallel=F, delete_parallel_temp_save_after = F)
  expect_true(file.exists(paste0(f1$folder_path, "//parallel_temp_output_1.rds")))
  f1$recover_parallel_temp_save(delete_after = T)
  expect_true(!file.exists(paste0(f1$folder_path, "//parallel_temp_output_1.rds")))
})
test_that('parallel can be run even if initially set to false', {
  # Skip tests with parallel=T on Travis, gives error
  testthat::skip_on_travis()
  f1 <- ffexp$new(a=1:4,
                  eval_func=function(a,b,cc) {},
                  verbose=0, parallel=F,
                  folder_path = paste0(getwd(), "/test8523-",
                                       gsub(" ","_",gsub(":","-",Sys.time()))),
                  parallel_cores=1)
  expect_equal(f1$parallel_cores, 1)
  expect_error(f1$run_all(parallel=T, parallel_temp_save = F), NA)
  # Check you can change number of pcores
  expect_error(f1$run_all(parallel=T, parallel_cores=2, redo=T, warn_repeat = F,
                          parallel_temp_save = F), NA)
  if (requireNamespace("parallel", quietly = TRUE) &&
      requireNamespace("snow", quietly = TRUE)) {
    expect_equal(f1$parallel_cores, 2)
  }

  # Delete at end
  # Sleep so that there is time for the file to show up
  Sys.sleep(.3)
  for (tmpfile in list.files(f1$folder_path)) {
    unlink(paste0(f1$folder_path, "//", tmpfile))
  }
  # Delete folder
  unlink(f1$folder_path, recursive=T)
})

test_that('remake cluster if connection doesn\'t work anymore', {
  # Skip tests with parallel=T on Travis, gives error
  testthat::skip_on_travis()
  f1 <- ffexp$new(a=1:4,
                  eval_func=function(a,b,cc) {},
                  verbose=0, parallel=T,
                  folder_path = paste0(getwd(), "/test7733-",
                                       gsub(" ","_",gsub(":","-",Sys.time()))),
                  parallel_cores=1)
  if (requireNamespace("parallel", quietly = TRUE) &&
      requireNamespace("snow", quietly = TRUE)) {
    # Give it a cluster
    f1$parallel_cluster <- parallel::makeCluster(
      spec=1, type = "SOCK")
    # Then kill the cluster
    parallel::stopCluster(f1$parallel_cluster)
  }
  # It should realize cluster is dead and start a new one.
  # print(f1$parallel_cluster)
  expect_error(f1$run_all(to_run = 1:2), NA)

  # Delete at end
  # Sleep so that there is time for the file to show up
  Sys.sleep(.3)
  for (tmpfile in list.files(f1$folder_path)) {
    unlink(paste0(f1$folder_path, "//", tmpfile))
  }
  unlink(f1$folder_path, recursive=T)
})

if (F) {
  # Test recover_parallel_temp_save only_reload_new
  f1 <- ffexp$new(a=1:4,
                  eval_func=function(a) {if (a==4) {stop()} else {a^2}},
                  verbose=0, parallel=T,
                  parallel_cores=1)
  f1$run_all(to_run = 1, parallel_temp_save = T)
  f1$run_all(parallel_temp_save = T)
  f1
  # Should only reload 2,3, not 1
  f1$recover_parallel_temp_save(only_reload_new = T)
  f1
  # Should reload 1,2,3
  f1$recover_parallel_temp_save(only_reload_new = F)
  f1
}
test_that("run for time, not parallel", {
  f1 <- ffexp$new(a=1:100,
                  eval_func=function(a) {{a^2}},
                  parallel=F,
                  parallel_cores=1)
  expect_error(f1$run_for_time(1, 2), NA)
  expect_true(sum(f1$completed_runs) > 0)
})
if (F) { # test run_for_time
  # Use parallel, actually slower
  f1 <- ffexp$new(a=1:1000,
                  eval_func=function(a) {{a^2}},
                  verbose=2, parallel=T,
                  parallel_cores=3)
  f1$run_for_time(30, 12)
}

test_that("Run superbatch", {
  f1 <- ffexp$new(a=1:10,
                  eval_func=function(a) {{a^2}},
                  parallel=F,
                  parallel_cores=1)
  expect_error(f1$run_superbatch(3), NA)
  expect_true(sum(f1$completed_runs) > 0)
})
