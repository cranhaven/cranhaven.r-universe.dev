test_that("findBeta works", {

    skip_on_cran()

    pathFileInput = system.file( "extdata", "exampleInputFile.csv", package = "ypssc" )

    temp = tempdir()
    if ( !dir.exists(temp) ) {
        dir.create(temp)
    }
    pathDirOutput = temp

    expect_output( findBeta( pathFileInput = pathFileInput,
                             pathDirOutput = pathDirOutput, TRUE ),
                   "Analysis completed successfully!" )

    unlink( pathDirOutput, recursive = TRUE )

})
