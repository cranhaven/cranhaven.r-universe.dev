context("Bid function, log likelihood")

test_that("Bid function",{
    n_bids = sample(2:10, 1, replace = TRUE)
    mu = 2
    alpha = 3
    gamma_1p1oa = gamma(1 + 1/alpha)
                                        # Generate cost
    cost = (mu/gamma(1+1/alpha))*(-log(1-stats::runif(n_bids)))^(1/alpha)

                                        # test generic results
    bids <- auctionr:::vf__bid_function_fast(cost = cost,
                                                 num_bids = n_bids,
                                                 mu = mu,
                                                 alpha = alpha,
                                                 gamma_1p1oa = gamma_1p1oa)

    expect_equal(bids,
                 cost + 1/alpha*(mu/gamma_1p1oa)*(n_bids-1)^(-1/alpha)*
                 stats::pgamma((n_bids-1)*(1/(mu/gamma_1p1oa)*cost)^alpha, 1/alpha, lower=FALSE)*
                 gamma(1/alpha)*
                 1/exp(-(n_bids-1)*(1/(mu/gamma_1p1oa)*cost)^alpha))

    expect_gte(min(bids),0)
    expect_equal(is.numeric(bids),
                 TRUE)

    # test integrand at zero
    alpha = 0
    gamma_1p1oa = gamma(1 + 1/alpha)
    cost = (mu/gamma(1+1/alpha))*(-log(1-stats::runif(n_bids)))^(1/alpha)

    bids = auctionr:::vf__bid_function_fast(cost = cost,
                                                num_bids = n_bids,
                                                mu = mu,
                                                alpha = alpha,
                                                gamma_1p1oa = gamma_1p1oa)
    expect_equal(bids,
                 cost + mu/alpha*(n_bids-1)^(-1/alpha)*1/gamma_1p1oa*
                 ((n_bids-1)*(gamma_1p1oa/mu*cost)^alpha)^(1/alpha-1))
})
