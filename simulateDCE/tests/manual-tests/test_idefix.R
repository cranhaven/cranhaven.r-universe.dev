devtools::load_all()

design_ngene <- system.file("extdata", "agora", "altscf_eff.ngd", package = "simulateDCE")

t0 <- readdesign(design_ngene)


design_sp <- system.file("extdata", "ValuGaps", "des1.RDS", package = "simulateDCE")





t <- readdesign(design_sp)
t2 <- readdesign(design_sp, designtype = "spdesign")
identical(t, t2)


design_idefix <- system.file("extdata", "Idefix_designs", "test_design2.RDS", package = "simulateDCE")

t3 <- readdesign(design_idefix)

c <- (readRDS(design_idefix))
