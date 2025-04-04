### I don't know how to retrieve the warnings suppressed by suppressWarnings().
### Is it even possible? So here is a slightly modified version of
### suppressWarnings() that stores the warnings in '.last_suppressed_warnings'
### if any.
.last_suppressed_warnings <- NULL

.suppress_warnings <- function(expr, classes="warning")
{
    .last_suppressed_warnings <<- NULL
    withCallingHandlers(expr,
        warning=function(w) {
            if (inherits(w, classes)) {
                .last_suppressed_warnings <<- c(.last_suppressed_warnings,
                                                w$message)
                tryInvokeRestart("muffleWarning")
            }
        }
    )
}

### An easier way would be to just do:
###   tools::md5sum(file1) == tools::md5sum(file2)
.files_are_identical <- function(file1, file2, n=50000L)
{
    con1 <- file(file1, "rb")
    on.exit(close(con1))
    con2 <- file(file2, "rb")
    on.exit(close(con2), add=TRUE)
    while (TRUE) {
        bytes1 <- readBin(con1, what=raw(), n=n)
        bytes2 <- readBin(con2, what=raw(), n=n)
        if (!identical(bytes1, bytes2))
            return(FALSE)
        if (length(bytes1) == 0L)
            break
    }
    TRUE
}

test_that("twobit_read/twobit_write roundtrips are lossless",
{
    ## on eboVir3.2bit (1 sequence)
    inpath <- system.file(package="Rtwobitlib", "extdata", "eboVir3.2bit")
    dna <- twobit_read(inpath)
    outpath <- twobit_write(dna, tempfile())
    expect_true(.files_are_identical(inpath, outpath))

    ## on sacCer2.2bit (18 sequences)
    inpath <- system.file(package="Rtwobitlib", "extdata", "sacCer2.2bit")
    dna <- twobit_read(inpath)
    outpath <- twobit_write(dna, tempfile())
    expect_true(.files_are_identical(inpath, outpath))
})

test_that("twobit_write/twobit_read roundtrips are lossless",
{
    dna <- c(seq1="A", seq2="TnT")
    filepath <- twobit_write(dna, tempfile())
    expect_identical(twobit_read(filepath), dna)
    filepath <- twobit_write(dna, tempfile(), use.long=TRUE)
    expect_identical(twobit_read(filepath), dna)

    dna <- c(chr1="AAAAAATTcccgcgccgccgTTTTAATCGaataataataatGGNNNNN",
             chr2="TTTNNNNNNATTATTTTACCACCAAACCCCACACT",
             chrM="GGGCAAATGGCG")
    filepath <- twobit_write(dna, tempfile())
    expect_identical(twobit_read(filepath), dna)
    filepath <- twobit_write(dna, tempfile(), use.long=TRUE)
    expect_identical(twobit_read(filepath), dna)
})

test_that("twobit_write error handling",
{
    ## --- with empty sequences ---

    dna <- c(chr1="cccATT", chr2="", chr3="NNN", chr4="")
    filepath <- .suppress_warnings(twobit_write(dna, tempfile()))
    expected_warnings <-
        sprintf("sequence chr%d has length 0 ==> skipping it", c(2L, 4L))
    expect_identical(.last_suppressed_warnings, expected_warnings)
    expect_identical(twobit_read(filepath), dna[c(1L, 3L)])

    ## --- with duplicated sequence names ---

    dna <- c(seq1="A", seq2="CC", seq3="TnT", seq2="TTnnnG",
             seq2="NNNN", seq4="CGCGcgacgTA", seq1="ACGT")
    expect_error(twobit_write(dna, tempfile()),
                 regexp="duplicate sequence name seq2")
    filepath <-
        .suppress_warnings(twobit_write(dna, tempfile(), skip.dups=TRUE))
    expected_warnings <-
        sprintf("duplicate sequence name seq%d ==> skipping it", c(2L, 2L, 1L))
    expect_identical(.last_suppressed_warnings, expected_warnings)
    expect_identical(twobit_read(filepath), dna[c(1:3, 6L)])

    ## --- with a mix of empty sequences and duplicated sequence names ---

    dna <- c(chrX="cccATT", chrX="")
    filepath <- .suppress_warnings(twobit_write(dna, tempfile()))
    expected_warning <- "sequence chrX has length 0 ==> skipping it"
    expect_identical(.last_suppressed_warnings, expected_warning)
    expect_identical(twobit_read(filepath), dna[1L])

    dna <- c(chrX="", chrX="cccATT")
    filepath <- .suppress_warnings(twobit_write(dna, tempfile()))
    expected_warning <- "sequence chrX has length 0 ==> skipping it"
    expect_identical(.last_suppressed_warnings, expected_warning)
    expect_identical(twobit_read(filepath), dna[2L])

    dna <- c(chr1="AAAAAATTcccgcgccgccgTTTTAATCGaataataataatGGNNNNN",
             chr2="TTTNNNNNNATTATTTTACCACCAAACCCCACACT",
             chr3="",
             chr2="TT",
             chr4="NNNNGGACAGGACattcattcattcattcTTCGNNNnnnnnnNNNNTAGGAGTCNN",
             chr1="",
             chrX="TnT",
             chrX="GGGCAAATGGCG",
             chr1="a")
    expect_error(suppressWarnings(twobit_write(dna, tempfile())),
                 regexp="duplicate sequence name chr2")
    filepath <-
        .suppress_warnings(twobit_write(dna, tempfile(), skip.dups=TRUE))
    expected_warnings <- c("sequence chr3 has length 0 ==> skipping it",
                           "duplicate sequence name chr2 ==> skipping it",
                           "sequence chr1 has length 0 ==> skipping it",
                           "duplicate sequence name chrX ==> skipping it",
                           "duplicate sequence name chr1 ==> skipping it")
    expect_identical(.last_suppressed_warnings, expected_warnings)
    expect_identical(twobit_read(filepath), dna[c(1:2, 5L, 7L)])

    ## --- produce empty 2bit file ---

    dna0 <- setNames(character(0), character(0))
    filepath <- twobit_write(dna0, tempfile())
    expect_identical(twobit_read(filepath), dna0)

    dna <- c(chr1="", chr2="", chr3="")
    expect_warning(filepath <- twobit_write(dna, tempfile()))
    expect_identical(twobit_read(filepath), dna0)

    ## --- with unknown DNA letters ---

    ## Unknown DNA letters are silently converted to T or t (lowercase
    ## letters -> t, anything else -> T).

    dna <- c(chr1="AAx78g*p-CC", chr2="Gn-nNyN", chr3="?", chr4="z")
    filepath <- twobit_write(dna, tempfile())
    expected <- c(chr1="AAtTTgTtTCC", chr2="GnTnNtN", chr3="T", chr4="t")
    expect_identical(twobit_read(filepath), expected)

    ## --- with sequence names longer than 255 chars ---

    dna <- c("TnT", "A", "ACGTNacgtnNTGCA")
    names(dna) <- c("chr1", sprintf("A%0254d", 1), sprintf("B%0254d", 1))
    filepath <- twobit_write(dna, tempfile())
    expect_identical(twobit_read(filepath), dna)

    names(dna) <- c("chr1", sprintf("A%0254d", 1), sprintf("B%0255d", 1))
    expect_error(twobit_write(dna, tempfile()),
                 regexp="sequence name too long: B0+1")

    names(dna) <- c("chr1", sprintf("A%0255d", 1), sprintf("B%0255d", 1))
    expect_error(twobit_write(dna, tempfile()),
                 regexp="sequence name too long: A0+1")

    ## --- invalid 'x' ---

    expect_error(twobit_write(factor(), tempfile()),
                 regexp="must be a character vector")
    expect_error(twobit_write(letters, tempfile()),
                 regexp="must have names")
    expect_error(twobit_write(c("AA", NA), tempfile()),
                 regexp="must have names")
    expect_error(twobit_write(c(seq1="AA", seq2=NA), tempfile()),
                 regexp="cannot contain NAs")
    dna <- c("A", "TnT")
    expect_error(twobit_write(setNames(dna, c("seq1", NA)), tempfile()),
                 regexp="names on .* cannot contain NAs or empty strings")
    expect_error(twobit_write(setNames(dna, c("seq1", "")), tempfile()),
                 regexp="names on .* cannot contain NAs or empty strings")

    ## --- invalid 'filepath' ---

    expect_error(twobit_write(c(seq1="A", seq2="TnT"), factor()),
                 regexp="'filepath' must be a single string")
    expect_error(twobit_write(c(seq1="A", seq2="TnT"), letters),
                 regexp="'filepath' must be a single string")
    expect_error(twobit_write(c(seq1="A", seq2="TnT"), NA_character_),
                 regexp="'filepath' must be a single string")
    expect_error(twobit_write(c(seq1="A", seq2="TnT"), ""),
                 regexp="'filepath' must be a non-empty string")
    expect_error(twobit_write(c(seq1="A", seq2="TnT"), "https://foo/bar.2bit"),
                 regexp="directory .* does not exist")

    ## --- invalid 'skip.dups' ---

    expect_error(twobit_write(c(seq1="A", seq2="TnT"), tempfile(),
                              skip.dups=NA),
                 regexp="'skip.dups' must be TRUE or FALSE")
    expect_error(twobit_write(c(seq1="A", seq2="TnT"), tempfile(),
                              skip.dups=logical(0)),
                 regexp="'skip.dups' must be TRUE or FALSE")
})

