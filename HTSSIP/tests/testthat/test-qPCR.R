check_qPCR = function(qPCR, physeq){
  expect_is(qPCR, 'list')
  expect_equal(qPCR$raw %>% nrow,
               phyloseq::sample_data(physeq) %>% nrow)
  expect_equal(qPCR$summary %>% nrow,
               phyloseq::sample_data(physeq) %>% nrow)
}

make_expr = function(d='norm'){
  d = tolower(d)
  if(d=='norm'){
    control_mean_fun = function(x) dnorm(x, mean=1.70, sd=0.01) * 1e8
    control_sd_fun = function(x) control_mean_fun(x) / 3
    treat_mean_fun = function(x) dnorm(x, mean=1.74, sd=0.01) * 1e8
    treat_sd_fun = function(x) treat_mean_fun(x) / 3
  } else
  if(d=='cauchy'){
    control_mean_fun = function(x) dcauchy(x, location=1.70, scale=0.01) * 1e8
    control_sd_fun = function(x) control_mean_fun(x) / 3
    treat_mean_fun = function(x) dcauchy(x, location=1.74, scale=0.01) * 1e8
    treat_sd_fun = function(x) treat_mean_fun(x) / 3
  } else {
    stop('distribution not recognized')
  }

  L = list(control_mean_fun = control_mean_fun,
           control_sd_fun = control_sd_fun,
           treat_mean_fun = treat_mean_fun,
           treat_sd_fun = treat_sd_fun)
  return(L)
}

test_that('qPCR simulation (control/treatment)', {
  skip_on_cran()

  L = make_expr()
  qPCR = qPCR_sim(physeq_S2D2,
         control_expr='Substrate=="12C-Con"',
         control_mean_fun=L$control_mean_fun,
         control_sd_fun=L$control_sd_fun,
         treat_mean_fun=L$treat_mean_fun,
         treat_sd_fun=L$treat_sd_fun)
  check_qPCR(qPCR, physeq_S2D2)

  p = ggplot2::ggplot(qPCR$summary, aes(Buoyant_density, qPCR_tech_rep_mean,
                      ymin=qPCR_tech_rep_mean-qPCR_tech_rep_sd,
                      ymax=qPCR_tech_rep_mean+qPCR_tech_rep_sd,
                      color=IS_CONTROL)) +
    geom_pointrange() +
    theme_bw()
  expect_is(p, 'ggplot')
})

test_that('qPCR simulation: replicate gradients', {
  skip_on_cran()

  L = make_expr()
  qPCR = qPCR_sim(physeq_rep3,
                  control_expr='Treatment=="12C-Con"',
                  control_mean_fun=L$control_mean_fun,
                  control_sd_fun=L$control_sd_fun,
                  treat_mean_fun=L$treat_mean_fun,
                  treat_sd_fun=L$treat_sd_fun)
  check_qPCR(qPCR, physeq_rep3)

  p = ggplot2::ggplot(qPCR$summary, aes(Buoyant_density, qPCR_tech_rep_mean,
                               ymin=qPCR_tech_rep_mean-qPCR_tech_rep_sd,
                               ymax=qPCR_tech_rep_mean+qPCR_tech_rep_sd,
                               color=IS_CONTROL)) +
    geom_pointrange() +
    theme_bw()
  expect_is(p, 'ggplot')
})


test_that('qPCR simulation (control/treatment); different distributions', {
  skip_on_cran()

  L = make_expr(d='cauchy')
  qPCR = qPCR_sim(physeq_S2D2,
                  control_expr='Substrate=="12C-Con"',
                  control_mean_fun=L$control_mean_fun,
                  control_sd_fun=L$control_sd_fun,
                  treat_mean_fun=L$treat_mean_fun,
                  treat_sd_fun=L$treat_sd_fun)
  check_qPCR(qPCR, physeq_S2D2)

  p = ggplot2::ggplot(qPCR$summary, aes(Buoyant_density, qPCR_tech_rep_mean,
                               ymin=qPCR_tech_rep_mean-qPCR_tech_rep_sd,
                               ymax=qPCR_tech_rep_mean+qPCR_tech_rep_sd,
                               color=IS_CONTROL)) +
    geom_pointrange() +
    theme_bw()
  expect_is(p, 'ggplot')
})


test_that('qPCR simulation warnings', {
  L = make_expr(d='cauchy')

expect_warning(
  qPCR_sim(physeq_S2D2,
                  control_expr='Substrate=="XXX"',
                  control_mean_fun=L$control_mean_fun,
                  control_sd_fun=L$control_sd_fun,
                  treat_mean_fun=L$treat_mean_fun,
                  treat_sd_fun=L$treat_sd_fun)
)
})
