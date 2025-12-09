### * potential_steady_state()

test_that("potential_steady_state() works", {
    # No split, no steady state, no lambdas
    m <- aquarium_mod
    m <- set_prior(m, constant_p(0), "lambda", quiet = TRUE)
    m <- set_params(m, sample_params(m))
    expect_true(potential_steady_state(m))
    # One non-zero lambda
    m <- set_prior(m, hcauchy_p(1), "lambda_NH4", quiet = TRUE)
    m <- set_params(m, sample_params(m))
    expect_false(potential_steady_state(m))
    # Split, steady state, non-zero lambdas
    m <- trini_mod[1, ]
    m <- set_params(m, sample_params(m))
    expect_true(potential_steady_state(m))
})

### * calculate_steady_state_one_row()

test_that("calculate_steady_state_one_row() works for no steady state source, no split", {
    m <- aquarium_mod
    m <- set_prior(m, constant_p(0), "lambda", quiet = TRUE)
    params <- c(eta = 0.142493300983702, lambda_algae = 0, lambda_daphnia = 0, 
                lambda_NH4 = 0, upsilon_algae_to_daphnia = 7.5051588290849e-05, 
                upsilon_daphnia_to_NH4 = 0.076850872955939, upsilon_NH4_to_algae = 0.172877125083978, 
                zeta = 0.0812341425176216)
    m <- set_params(m, params)
    proj <- project(m, end = 120)
    proj <- proj$trajectory[[1]]$sizes[[1]]
    last_proj <- proj[nrow(proj), ]
    z <- calculate_steady_state_one_row(m)
    expect_true(max(abs(z - last_proj)) < 5e-4)
})
