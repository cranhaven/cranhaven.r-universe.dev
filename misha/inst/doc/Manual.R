## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(misha)
gdb.init_examples()

## ----eval=FALSE---------------------------------------------------------------
#  # 'annotations' is an intervals set saved in Genomic Database
#  gintervals.intersect("annotations", gintervals(2))

## ----eval=FALSE---------------------------------------------------------------
#  gvtrack.create("myvtrack", "dense_track")

## ----eval=FALSE---------------------------------------------------------------
#  gvtrack.create("myvtrack", "dense_track", "global.percentile")

## ----eval=FALSE---------------------------------------------------------------
#  gvtrack.create("myvtrack", "array_track", "sum")
#  gvtrack.array.slice("myvtrack", c("col2", "col5"), "max")

## ----eval=FALSE---------------------------------------------------------------
#  gvtrack.iterator("myvtrack", sshift = -100, eshift = 200)

## ----eval=FALSE---------------------------------------------------------------
#  gvtrack.create("myvtrack", "dense_track")
#  gvtrack.iterator("myvtrack", dim = 2)

## ----eval=FALSE---------------------------------------------------------------
#  gvtrack.create("myvtrack", "annotations", "distance")
#  intervs <- gscreen("dense_track > 0.45")
#  gextract("myvtrack", .misha$ALLGENOME, iterator = intervs)

## ----eval = FALSE-------------------------------------------------------------
#  options(gbuf.size = 1)

## ----eval=FALSE---------------------------------------------------------------
#  gextract("dense_track", gintervals(2, 340, 520))

## ----eval=FALSE---------------------------------------------------------------
#  intervs <- gintervals.2d(1, 200, 800, 1, 100, 1000)
#  intervs <- rbind(intervs, gintervals.2d(1, 900, 950, 1, 0, 200))
#  intervs <- rbind(intervs, gintervals.2d(1, 0, 100, 1, 0, 400))
#  intervs <- rbind(intervs, gintervals.2d(1, 900, 950, 2, 0, 200))
#  intervs

## ----eval=FALSE---------------------------------------------------------------
#  intervs <- gintervals.2d(1, c(100, 400), c(300, 490), 1, c(120, 180), c(200, 500))
#  gtrack.2d.create("test2d", "test 2D track", intervs, c(10, 20))
#  gextract("test2d", .misha$ALLGENOME)
#  gextract("test2d", .misha$ALLGENOME, iterator = gintervals.2d(1, 0, 1000, 1, 0, 1000))
#  gintervals.2d.band_intersect(intervs, band = c(150, 1000))
#  gextract("test2d", .misha$ALLGENOME, iterator = gintervals.2d(1, 0, 1000, 1, 0, 1000), band = c(150, 1000))
#  gtrack.rm("test2d", force = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  set.seed(60427)
#  r1 <- gsample("dense_track", 10)
#  r2 <- gsample("dense_track", 10) # r2 differs from r1
#  set.seed(60427)
#  r3 <- gsample("dense_track", 10) # r3 == r1

