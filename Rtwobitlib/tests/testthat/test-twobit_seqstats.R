
test_that("twobit_seqstats()",
{
    ## on eboVir3.2bit (1 sequence)

    filepath <- system.file(package="Rtwobitlib", "extdata", "eboVir3.2bit")

    result <- twobit_seqstats(filepath)
    expected <- rbind(KM034562v1=c(seqlengths=18957L,
                                   A=6051L, C=4050L, G=3756L, T=5100L, N=0L))
    expect_identical(result, expected)

    result <- twobit_seqlengths(filepath)
    expect_identical(result, c(KM034562v1=18957L))

    ## on sacCer2.2bit (18 sequences)

    filepath <- system.file(package="Rtwobitlib", "extdata", "sacCer2.2bit")

    result <- twobit_seqstats(filepath)
    expect_true(is.matrix(result))
    expect_identical(dim(result), c(18L, 6L))
    expected_colnames <- c("seqlengths", "A", "C", "G", "T", "N")
    expect_identical(colnames(result), expected_colnames)
    some_expected_rownames <- c("chrI", "chrXVI", "chrM", "2micron")
    expect_true(all(some_expected_rownames %in% rownames(result)))
})

