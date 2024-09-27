library(EnrichDO)

test_that("drawGraphViz", {
    demo.data = c(1636, 351, 102, 2932, 3077, 348, 4137, 54209, 5663, 5328, 23621, 3416, 3553)
    res <- sapply(c(0.01, 0.02, 0.03, 0.04), function(delta) {

        aa <- doEnrich(interestGenes = demo.data, delta = delta)
        record <- 1
        record <- tryCatch({
            drawGraphViz(EnrichResult = aa, n = 5)
            1
        }, error = function(e) {
            2
        }, finally = {
            1
        })
        return(record)
    })
    expect_true(all(res) == 1)
})
