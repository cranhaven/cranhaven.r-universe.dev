library(dplyr)

test_that("Check that binary probability is correct", {
    suppressMessages({
        ss <- set_n(
            ssC = 140,
            ssE = 275,
            ssExt = 100
        )

        covset1 <- set_cov(
            n_cat = 1,
            n_cont = 0,
            mu_int = 0,
            mu_ext = 0,
            var = 1,
            cov = 0.5,
            prob_int = 0.7,
            prob_ext = 0.9
        )

        sample_cov <- simu_cov(
            ssObj = ss,
            covObj = covset1,
            HR = 1,
            driftHR = 1,
            nsim = 100,
            seed = 123
        )
    })

    cov1_prob <- bind_rows(lapply(sample_cov, as_tibble)) %>%
        group_by(ext) %>%
        summarise(m = mean(cov1))

    int_prob <- cov1_prob %>% filter(ext == 0) %>% pull(m)
    ext_prob <- cov1_prob %>% filter(ext == 1) %>% pull(m)

    expect_true(int_prob > 0.68 & int_prob < 0.72)
    expect_true(ext_prob > 0.88 & ext_prob < 0.92)
})


test_that("Check that Covariate distributions are created correctly", {
    suppressMessages({
        ss <- set_n(
            ssC = 20000,
            ssE = 20000,
            ssExt = 40000
        )

        vcov <- as_vcov(
            c(1, 3, 6, 8),
            c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
        )

        covset1 <- set_cov(
            n_cat = 2,
            n_cont = 2,
            mu_int = c(10,50,1,2),
            mu_ext = c(100,50,6,7),
            var = diag(vcov),
            cov = vcov[upper.tri(vcov)],
            prob_int = c(0.3, 0.7),
            prob_ext = c(0.1, 0.5)
        )

        sample_cov <- simu_cov(
            ssObj = ss,
            covObj = covset1,
            HR = 1,
            driftHR = 1,
            nsim=1,
            seed=5121
        )
    })

    dat_int <- sample_cov[[1]] %>% as_tibble() %>% filter(ext == 0)
    dat_ext <- sample_cov[[1]] %>% as_tibble() %>% filter(ext == 1)

    expect_around <- function(x, y, margin = 0.02) {
        expect_true(
            all(
                (x > y * (1 - margin)) &
                (x < y * (1 + margin))
            )
        )
    }

    expect_around(mean(dat_int$cov1), 0.3)
    expect_around(mean(dat_int$cov2), 0.7)
    expect_around(mean(dat_int$cov3), 1)
    expect_around(mean(dat_int$cov4), 2)
    expect_around(mean(dat_ext$cov1), 0.1)
    expect_around(mean(dat_ext$cov2), 0.5)
    expect_around(mean(dat_ext$cov3), 6)
    expect_around(mean(dat_ext$cov4), 7)


    vcov_cont <- vcov[c(3, 4), c(3, 4)]

    vcov_int_obs <- dat_int %>% select(cov3, cov4) %>% var
    vcov_ext_obs <- dat_ext %>% select(cov3, cov4) %>% var

    expect_around(vcov_int_obs, vcov_cont)
    expect_around(vcov_ext_obs, vcov_cont)
})
