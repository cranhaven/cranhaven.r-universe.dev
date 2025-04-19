# expect_error(), expect_warning(), expect_message(), expect_output()

context("Testing CohensdpLibrary")

########################################################################
test_that("TESTS of the hypergeometric functions (1/8)", {
    res <- hypergeometric0F1(12, 0.4)
    expect_equal( res, 1.033851, tolerance=1e-6 )
    res <- hypergeometric1F1(12, 14, 0.4)
    expect_equal( res, 1.409877, tolerance=1e-6 )
    res <- hypergeometric2F1(12, 14, 16, 0.4)
    expect_equal( res, 205.5699, tolerance=1e-6 )
})


########################################################################
test_that("TESTS of the Lecoutre's distributions (2/8)", {
    res <- dkprime(11.1, 9, 8, 10.0)
    expect_equal( res, 0.09410193, tolerance=1e-6 )
    res <- pkprime(11.1, 9, 8, 10.0)
    expect_equal( res, 0.6066524, tolerance=1e-6 )
    res <- qkprime(0.01, 9, 8, 10.0)
    expect_equal( res, 3.875234, tolerance=1e-6 )

    res <- dlprime(11.1, 9, 10)
    expect_equal( res, 0.129447, tolerance=1e-6 )
    res <- plprime(11.1, 9, 10)
    expect_equal( res, 0.7134134, tolerance=1e-6 )
    res <- qlprime(0.01, 9, 10)
    expect_equal( res, 4.2453, tolerance=1e-6 )
})


########################################################################
test_that("TESTS of the novel distributions lambda'' and piCI distributions (3/8)", {
    # test Lambda''
    res <- dlsecond(0.25, 9, 0.26, 0.333)   
    expect_equal( res, 1.037531, tolerance=1e-6 )
    res <- plsecond(0.25, 9, 0.26, 0.333)   
    expect_equal( res, 0.4942994, tolerance=1e-6 )
    res <- qlsecond(0.01, 9, 0.26, 0.333) 
    expect_equal( res, -0.6468003, tolerance=1e-6 )

    res1 <- qlsecond(0.025, 9, 0.26, 0.333)
    res2 <- plsecond(res1,  9, 0.26, 0.333)
    expect_equal( res2, 0.025, tolerance=1e-6)

    # test prior-informed Lambda''
    # the novel prior-informed distribution
    ### res <- dpilsecond(0.25, 9, 0.26, 0.333)   
    ### expect_equal( res, 1.186735, tolerance=1e-6 ) #Ok! takes over five seconds...
    res <- ppilsecond(0.25, 9, 0.26, 0.333)   
    expect_equal( res, 0.5150561, tolerance=1e-6 ) #mma returns 0.515409...
    res <- qpilsecond(0.025, 9, 0.26, 0.333)       
    expect_equal( res, -0.5523409, tolerance=1e-6 ) #Ok! 
    expect_equal( 0.025, ppilsecond(res, 9, 0.26, 0.333), tolerance=1e-3 )

})



########################################################################
test_that("TESTS of J (4/8)", {

    # testing some hidden functions
    expect_equal( TRUE, CohensdpLibrary:::is.inIt( list(n=21), c("n") ) )



    res <- J( statistics = list(n=21), design="single")
    expect_equal( res$estimate, 0.961945, tolerance=1e-6 )   
    res <- J( statistics = list(n1=26,n2=26), design="between")
    expect_equal( res$estimate, 0.9849119, tolerance=1e-6 )   
    res <- J( statistics = list(n=26,rho=0.200), design="within")
    expect_equal( res$estimate, 0.984342, tolerance=1e-6 )   
    res <- J( statistics = list(n=26,r=0.200), design="within")
    expect_equal( res$estimate, 0.984342, tolerance=1e-6 )   


    expect_error( J( design="between") )
    expect_error( J( statistics = list(g=1)) )
    expect_error( J( statistics = list(g=1), design="between") )
    expect_error( J( statistics = 34 ) )
    expect_error( J( statistics = list(n=10) ) )

    expect_error( J( statistics = list(n=1), design="single") )
    expect_error( J( statistics = list(n1=10), design="single") )

    expect_error( J( statistics = list(n1=10), design="within") )
    expect_error( J( statistics = list(n=10, r=1.1), design="within") )

    expect_error( J( statistics = list(n=10), design="between") )
    expect_error( J( statistics = list(n1=1), design="between") )
    expect_error( J( statistics = list(n1=11, n2=1), design="between") )
    expect_error( J( statistics = list(n1=1, n2=11), design="between") )

})



########################################################################
test_that("TESTS of Cohensdp (5/8)", {

    # testing single-group design
    # CohensdpLibrary:::Cohensdp.single(statistics=list(m=72, m0=76,s=16,n=20))
    resS <- Cohensdp( statistics = list(m=72,m0 =76,s=16,n=20), design="single")
    expect_equal( resS$estimate, -0.250, tolerance=1e-6  )
    expect_equal( resS$interval, c(-0.6921470, 0.1984842), tolerance=1e-6  )

    # testing between-group design
    # CohensdpLibrary:::Cohensdp.between(statistics=list(m1=72, m2=76,s1=16,s2=16,n1=10,n2=10))
    resB <- Cohensdp( statistics=list(m1=72, m2=76,s1=16,s2=16,n1=10,n2=10), design="between")
    expect_equal( resB$estimate, -0.250, tolerance=1e-6  )
    expect_equal( resB$interval, c(-1.1268491, 0.6336855), tolerance=1e-6  )

    # testing within-group design, rho known
    # CohensdpLibrary:::Cohensdp.within(statistics=list(rho= 0.2, m1=72, m2=76,s1=16,s2=16,n=10))
    resW <- Cohensdp( statistics=list(m1=72, m2=76,s1=16,s2=16,n=10, rho=0.2), design="within")
    expect_equal( resW$estimate, -0.250, tolerance=1e-6  )
    expect_equal( resW$interval, c(-1.0347886, 0.5418429), tolerance=1e-6  )


    # testing within-group design, rho unknown : 
    # CohensdpLibrary:::Cohensdp.within(statistics=list(r= 0.2, m1=72, m2=76,s1=16,s2=16,n=10))
    resW <- Cohensdp( statistics=list(m1=72, m2=76,s1=16,s2=16,n=10,r=0.2), 
        design="within",
        method = "adjustedlambdaprime") 
    expect_equal( resW$estimate, -0.250, tolerance=1e-6  )
    expect_equal( resW$interval, c(-1.0595437, 0.5883274), tolerance=1e-6  )


    resW <- Cohensdp( statistics=list(m1=72, m2=76,s1=16,s2=16,n=10,r=0.2), 
        design="within", method = "piCI")
    expect_equal( resW$estimate, -0.250, tolerance=1e-6  )
    expect_equal( resW$interval, c(-1.0582183, 0.5764807), tolerance=1e-6  )


    resW <- Cohensdp( statistics=list(m1=72, m2=76,s1=16,s2=16,n=10,r=0.2), 
        design="within", method = "adjustedlambdaprime")
    expect_equal( resW$estimate, -0.250, tolerance=1e-6  )
    expect_equal( resW$interval, c(-1.0595437, 0.5883274), tolerance=1e-6  )


    resW <- Cohensdp( statistics=list(m1=72, m2=76,s1=16,s2=16,n=10,r=0.2), 
        design="within", method = "alginakeselman2003") 
    expect_equal( resW$estimate, -0.250, tolerance=1e-6  )
    expect_equal( resW$interval, c(-1.0355115, 0.5488977), tolerance=1e-6  )


    expect_equal( 0.95893, CohensdpLibrary:::fittsAdjustedGamma(0.95, 10), tolerance = 1e-6)
    resW <- Cohensdp( statistics=list(m1=72, m2=76,s1=16,s2=16,n=10,r=0.2), 
        design="within", method = "morris2000") 
    expect_equal( resW$estimate, -0.250, tolerance=1e-6  )
    expect_equal( resW$interval, c(-1.0732257, 0.5732257), tolerance=1e-6  )


    resW <- Cohensdp( statistics=list(m1=72, m2=76,s1=16,s2=16,n=10,r=0.2), 
        design="within", method = "regressionapproximation") 
    expect_equal( resW$estimate, -0.250, tolerance=1e-6  )
    expect_equal( resW$interval, c(-1.0681263, 0.5752457), tolerance=1e-6  )


    # testing erroneous design methods
    expect_error( resW <- Cohensdp( statistics=list(m=72, m0=76,s=16,n=10), design="single", method = "piii") )
    expect_error( resW <- Cohensdp( statistics=list(m1=72, m2=76,s1=16,s2=16,n1=10,n2=10), design="between", method = "piii") )
    expect_error( resW <- Cohensdp( statistics=list(m1=72, m2=76,s1=16,s2=16,n=10,r=0.2), design="within", method = "piii") )


})



########################################################################
test_that("TESTS of Hedgesgp (6/8)", {

    # testing single-group design
    # CohensdpLibrary:::Hedgesgp.single(statistics=list(m=72, m0=76,s=16,n=20))
    expect_message( resS <- Hedgesgp( statistics = list(m=72,m0 =76,s=16,n=20), design="single") )
    expect_equal( resS$estimate, -0.2399776, tolerance=1e-6 )

    # testing between-group design
    # CohensdpLibrary:::Hedgesgp.between(statistics=list(m1=72, m2=76,s1=16,s2=16,n1=20,n2=20))
    expect_message( resB <- Hedgesgp( statistics = list(m1=72,m2=76,s1=16,s2=16,n1=20,n2=20), design="between") )
    expect_equal( resB$estimate, -0.2450276, tolerance=1e-6 )

    # testing within-group design, rho known
    # CohensdpLibrary:::Hedgesgp.within(statistics=list(m1=72, m2=76,s1=16,s2=16,n=20, rho=0.2))
    expect_message( resW <- Hedgesgp( statistics = list(m1=72,m2=76,s1=16,s2=16,n=20,rho=0.2), design="within") )
    expect_equal( resW$estimate, -0.2448432, tolerance=1e-6 )

    # testing within-group design, rho unknown
    # CohensdpLibrary:::Hedgesgp.within(statistics=list(m1=72, m2=76,s1=16,s2=16,n=20, r=0.2))
    expect_message( resW <- Hedgesgp( statistics = list(m1=72,m2=76,s1=16,s2=16,n=20,r=0.2), design="within") )
    expect_equal( resW$estimate, -0.2448432, tolerance=1e-6 )

})



########################################################################
test_that("TESTS of the options (7/8)", {

    expect_message( Hedgesgp( statistics = list(m1=72,m2=76,s1=16,s2=16,n=20,rho=0.2), design="within") )
    options("CohensdpLibrary.SHOWWARNINGS" = FALSE)
    expect_equal( -0.2448432, Hedgesgp( statistics = list(m1=72,m2=76,s1=16,s2=16,n=20,rho=0.2), design="within")$estimate, tolerance=1e-6 )
    options("CohensdpLibrary.SHOWWARNINGS" = TRUE)


})


context("Testing new checks (August 2024)")

########################################################################
test_that("TESTS of missing r/rho (8/8)", {

    expect_error( resW <- Cohensdp( statistics=list(m1=72, m2=76,s1=16,s2=16,n=10), design="within", method = "piii") )

})



########################################################################
test_that("TESTS of my moral (8/8)", {

    expect_error( Je * suis * le * plus * fort )

})



