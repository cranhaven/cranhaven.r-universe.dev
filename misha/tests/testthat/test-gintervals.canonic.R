test_that("gintervals.canonic works (1)", {
    i1 <- gscreen("test.fixedbin>0.16 & test.fixedbin<0.19", gintervals(c(1, 2)))
    i2 <- gscreen("test.fixedbin>0.13 & test.fixedbin<0.17", gintervals(c(1, 2)))
    i3 <- rbind(i1, i2)
    size <- dim(i3)[1]
    set.seed(60427)
    i4 <- i3[sample(1:size, size), ]
    expect_regression(gintervals.canonic(i4), "gintervals.canoic.1")
})

test_that("gintervals.canonic works (2)", {
    expect_equal(
        gintervals.canonic(rbind(gintervals.2d(5), gintervals.2d(1))) %>% mutate(chrom1 = as.character(chrom1), chrom2 = as.character(chrom2)),
        data.frame(
            chrom1 = c("chr1", "chr5"),
            start1 = c(0, 0),
            end1 = c(247249719, 180857866),
            chrom2 = c("chr1", "chr5"),
            start2 = c(0, 0),
            end2 = c(247249719, 180857866)
        ),
        ignore_attr = TRUE
    )
    expect_error(gintervals.canonic(rbind(gintervals.2d(5), gintervals.2d(1), gintervals.2d(1))))
})
