context("Testing examples from the Article")


########################################################################
test_that("EXEMPLES of the articles", {
    # Exemple 1
    res <- Cohensdp( statistics = list( m1=72, m2=76, 
                                 s1=16, s2=16, 
                                 n=10,  rho=0.5),
              design = "within"
            )
    
    expect_output( str(res), "list")
    expect_equal( res$estimate, -0.25)
    expect_equal( res$interval, c(-0.8721566,+0.3803735), tolerance=1e-6 )


    # Exemple 2
    res <- Cohensdp( statistics = list( m1=72, m2=76, 
                                        s1=14.2, s2=18.8, 
                                        n=25, rho=0.2),
                       design = "within"
            )

    expect_output( str(res), "list")
    expect_equal( res$estimate, -0.2401028, tolerance=1e-6)
    expect_equal( res$interval, c(-0.7370541,+0.2594116), tolerance=1e-6 )


    # Exemple 3
    expect_message( res <- Hedgesgp( statistics = list( m1=72, m2=76, 
                                 s1=16, s2=16, 
                                 n=10,  rho=0.2),
              design = "within"
             )
    )
    expect_output( str(res), "list")
    expect_equal( res$estimate, -0.2390504, tolerance=1e-6)

    # Exemple 4 
    res <- J( statistics = list( n=10, rho=0.5),
        design = "within"
    )
    expect_output( str(res), "list")
    expect_equal( res$estimate, 0.9483177, tolerance=1e-6)


})




