test_that("trinomial bound works", {
    
    data(santa.cruz)
    Z = elec.data(santa.cruz, C.names=c("leopold","danner"))
    Z
    
    
    # Make a plan
    plan = tri.calc.sample( Z, beta=0.75, guess.N = 10, p_d = 0.05,
                            swing=10, power=0.9, bound="e.plus" )
    plan

    
    # Conduct the audit
    data(santa.cruz.audit)
    res = trinomial.audit( Z, santa.cruz.audit )
    res
    
    ## Compute the bound.  Everything is scaled by 100 (i.e. to percents) for easier numbers. 
    trinomial.bound(n=res$n, k = res$k, d=100*plan$d, e.max=100, p.value.bound=100/plan$T,
                    xlim=c(0.75,1), ylim=c(0.0,0.25),
                    alpha.lvls=c(25), asp=1,
                    main="Auditing Santa Cruz with Trinomial Bound" )
})
