library( elec )
# library( testthat )

test_that("CAST works, nominally", {
    
    Z = make.cartoon()
    Z
    
    samp.info = CAST.calc.sample( Z )
    samp.info

    expect_true( is.audit.plan( samp.info ) )
    expect_true( !is.audit.plan.tri( samp.info ) )
    
    samp = CAST.sample( Z, samp.info )
    samp

    expect_equal( length( samp ), samp.info$ns )    
})






test_that( "sim.race works", {
  
    set.seed( 40404 )
    rps = replicate( 20, sim.race() )
    expect_equal( dim( rps ), c(3,20 ) )
    rps
    
})



test_that( "make.truth stuff", {
    Z = make.sample( M = 0.2, N = 800 )
    Z
    Zth <- make.truth.ex.bad(Z)
    expect_true( !is.null( Zth ) )
})


