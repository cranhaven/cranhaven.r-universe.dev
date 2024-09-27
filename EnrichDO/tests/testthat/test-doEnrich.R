library(EnrichDO)

test_that("doEnrich", {
    demo.data = c(1636, 351, 102, 2932, 3077, 348, 4137, 54209, 5663, 5328, 23621, 3416, 3553)
    res <- sapply(1:5, function(m) {
        aa <- doEnrich(interestGenes = demo.data, m = m)
        result <- aa@enrich
        bb <- filter(result, level < m)$p
        return(all(as.numeric(bb) == 1))
    })
    expect_true(all(res) == TRUE)
})
