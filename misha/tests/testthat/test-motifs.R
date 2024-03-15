test_that("gtrack.create_pwm_energy works", {
    allgenome <- .misha$ALLGENOME
    .misha$ALLGENOME[[1]]$end <- 1000000
    .misha$ALLGENOME[[2]]$end1 <- 1000000
    .misha$ALLGENOME[[2]]$end2 <- 1000000
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.create_pwm_energy("test.tmptrack", "", "misha_motifs", 0, 0.02, iterator = 50)
    r <- gextract("test.tmptrack", .misha$ALLGENOME)
    .misha$ALLGENOME <- allgenome
    expect_regression(r, "gtrack.create_pwm_energy")
})
