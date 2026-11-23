test_that("Ensure errors are correct", {

  dt1 <- tibble(
    HR = c(0.8, 0.8),
    driftHR = c(1,1),
    nsim=c(10,10),
    prior = c('full_ext','gamma'),
    pred = c('all','all'),
    reject = c(0.2, 0.3)
  )

  dt2 <- tibble(
    HR = c(0.8, 0.8, 1.0, 1.0),
    driftHR = c(1,1,1,1),
    prior = c('full_ext','gamma','full_ext','gamma'),
    pred = c('all','all','all','all'),
    nsim = c(10,10,10,10),
    reject = c(0.2, 0.3,0.4,0.5),
    mean_HR_trt_cc = c(.5,.4,.6,.5),
    sd_HR_trt_cc = c(.1, .05, .1, .2),
    bias = c(-.2, -.1, .04, .003),
    var = c(.001, .003, .009, .0001),
    sd = c(.012, .020, .030, .018),
    mse = c(.11, .22, .33, .10),
    mean_HR_cc_hc = c(0, .8, 0, .8),
    sd_HR_cc_hc = c(0, .1, 0, .1)
  )

  dt3 <- dt2 %>%
    filter(HR != 1.0)

  dt4 <- dt2 %>%
    mutate(nsim=1) %>%
    mutate_at(c('bias','var','sd'), function(z) NaN)

  # plot_type1error
  expect_error(plot_type1error(dt1,
               'dt is not a summary data.frame from get_summary'))
  expect_error(plot_type1error(dt3, driftHR=1,pred='all'),
               "dt does not include HR = 1\\.0")
  expect_error(plot_type1error(dt2, driftHR=1.3,pred='all'),
               "dt does not include a driftHR of 1\\.3")
  expect_error(plot_type1error(dt2, driftHR=1,pred='cov1'),
               "dt does not include a pred of 'cov1'")

  # plot_power
  expect_error(plot_power(dt1,
                          'dt is not a summary data\\.frame from get_summary'))
  expect_error(plot_power(dt2, HR=1.2, pred='all'),
               "dt does not include an HR of 1\\.2")
  expect_error(plot_power(dt2, HR=0.8, driftHR=1.3,pred='all'),
               "dt does not include a driftHR of 1\\.3")
  expect_error(plot_power(dt2, HR=0.8, driftHR=1,pred='cov1'),
               "dt does not include a pred of 'cov1'")
  expect_error(plot_power(dt2, HR=1, driftHR=1.2, pred='all'),
               'HR = 1 is not a valid input into plot_power')

  # plot_hr
  expect_error(plot_hr(dt1,
                          'dt is not a summary data\\.frame from get_summary'))
  expect_error(plot_hr(dt2, HR=1.2, pred='all'),
               "dt does not include an HR of 1\\.2")
  expect_error(plot_hr(dt2, HR=0.8, driftHR=1.3,pred='all'),
               "dt does not include a driftHR of 1\\.3")
  expect_error(plot_hr(dt2, HR=0.8, driftHR=1,pred='cov1'),
               "dt does not include a pred of 'cov1'")

  # plot_bias
  expect_error(plot_bias(dt1,
                       'dt is not a summary data\\.frame from get_summary'))
  expect_error(plot_bias(dt2, HR=1.2, pred='all'),
               "dt does not include an HR of 1\\.2")
  expect_error(plot_bias(dt2, HR=0.8, driftHR=1.3,pred='all'),
               "dt does not include a driftHR of 1\\.3")
  expect_error(plot_bias(dt2, HR=0.8, driftHR=1,pred='cov1'),
               "dt does not include a pred of 'cov1'")

  # plot_mse
  expect_error(plot_mse(dt1,
                         'dt is not a summary data.frame from get_summary'))
  expect_error(plot_mse(dt2, HR=1.2, pred = 'all'),
               'dt does not include an HR of 1\\.2')
  expect_error(plot_mse(dt2, HR=0.8, driftHR=1.3,pred='all'),
               "dt does not include a driftHR of 1\\.3")
  expect_error(plot_mse(dt2, HR=0.8, driftHR=1,pred='cov1'),
               "dt does not include a pred of 'cov1'")
  expect_error(plot_mse(dt4, HR=0.8, driftHR=1,pred='all'),
               "plot_mse\\(\\) requires >1 simulation to estimate variance")

})

test_that("Ensure output is producing a ggplot2 object with appropriate parameters", {

  dt5 <- tibble(
    HR = c(rep(0.8,4), rep(1.0,4)),
    driftHR = rep(1.0, 8),
    prior = rep(c('no_ext','gamma','cauchy','full_ext'),2),
    pred = rep('all',8),
    nsim = rep(30, 8),
    reject = seq(0.1,0.3,length.out=8),
    mean_HR_trt_cc = c(rep(0.8,4), rep(0.95,4)),
    sd_HR_trt_cc = rep(c(.1, .05, .1, .2),2),
    bias = rep(c(-.2, -.1, .04, .003),2),
    var = rep(c(.001, .003, .009, .0001),2),
    sd = rep(c(.012, .020, .030, .018),2),
    mse = rep(c(.11, .22, .33, .10),2),
    mean_HR_cc_hc = rep(c(0, .8, .8, 0),times=2),
    sd_HR_cc_hc = rep(c(0, .1,.1,0), times=2)
  )

  dt6 <- dt5[dt5$prior!='no_ext',]

  p1 <- plot_type1error(dt5, driftHR=1, pred='all')
  p2 <- plot_type1error(dt6, driftHR=1, pred='all')
  p3 <- plot_power(dt5, HR = 0.8, driftHR = 1, pred = 'all')
  p4 <- plot_power(dt6, HR = 0.8, driftHR = 1, pred = 'all')
  p5 <- plot_hr(dt5, HR = 0.8, driftHR = 1, pred = 'all')
  p6 <- plot_hr(dt6, HR = 0.8, driftHR = 1, pred = 'all')
  p7 <- plot_bias(dt5, HR = 0.8, driftHR = 1, pred = 'all')
  p8 <- plot_bias(dt6, HR = 0.8, driftHR = 1, pred = 'all')
  p9 <- plot_mse(dt5, HR = 0.8, driftHR = 1, pred = 'all')
  p10 <- plot_mse(dt6, HR = 0.8, driftHR = 1, pred = 'all')

  for(p in list(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)){
    expect_s3_class(p, 'gg')
  }

  skip_if_not_installed("ggplot2", minimum_version = "3.5.2")

  for(p in list(p1, p3, p7, p9)) {
    expect_equal(NROW(p$layers), 3L)
    expect_equal(ggplot2::get_labs(p)$yintercept, 'ref')
  }
  for(p in list(p2, p4, p8, p10)) {
    expect_equal(NROW(p$layers), 2L)
  }
  expect_equal(NROW(p5$layers), 4L)
  expect_equal(NROW(p6$layers), 3L)

  expect_equal(ggplot2::get_labs(p1)$caption, 'Horizontal purple line refers to the type 1 error without any external arm (0.2143)')
  expect_equal(ggplot2::get_labs(p1)$title, 'Summarizing posterior distributions: Type 1 Error')
  expect_equal(ggplot2::get_labs(p2)$title, 'Summarizing posterior distributions: Type 1 Error')

  expect_equal(ggplot2::get_labs(p3)$caption, 'Horizontal purple line refers to the power without any external arm (0.1)')
  expect_equal(ggplot2::get_labs(p3)$title, 'Summarizing posterior distributions: power')
  expect_equal(ggplot2::get_labs(p4)$title, 'Summarizing posterior distributions: power')

  expect_equal(ggplot2::get_labs(p5)$caption, 'Horizontal purple line refers to the posterior hazard ratio without any external arm (0.8)')
  expect_equal(ggplot2::get_labs(p5)$title, 'Summarizing posterior distributions: mean posterior hazard ratio')
  expect_equal(ggplot2::get_labs(p6)$title, 'Summarizing posterior distributions: mean posterior hazard ratio')

  expect_equal(ggplot2::get_labs(p7)$caption, 'Horizontal purple line refers to the bias without any external arm (-0.2)')
  expect_equal(ggplot2::get_labs(p7)$title, 'Summarizing posterior distributions: bias')
  expect_equal(ggplot2::get_labs(p8)$title, 'Summarizing posterior distributions: bias')

  expect_equal(ggplot2::get_labs(p9)$caption, 'Horizontal purple line refers to the MSE without any external arm (0.11)')
  expect_equal(ggplot2::get_labs(p9)$title, 'Summarizing posterior distributions: MSE')
  expect_equal(ggplot2::get_labs(p10)$title, 'Summarizing posterior distributions: MSE')
})
