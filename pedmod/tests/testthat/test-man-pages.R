context("testing examples in manual pages")

test_that("examples in manual pages gives the correct answer for eval_pedigree_[ll]/[grad]", {
  S <- structure(c(0.766725077267174, 0.249137157109195, -0.301516767233159, 0.112735950556606, -0.121636706660597, 0.0853052478227257, -0.0111813042831139, -0.0201254731753443, 0.149427110129624, -0.021996990666367, 0.249137157109195, 1.42410274977015, -0.338609740421468, -0.0425085949907385, -0.613458260081048, -0.292986225195268, 0.126875549501989, 0.0939246378491726, 0.00591847504584558, 0.221189267455151, -0.301516767233159, -0.338609740421468, 1.13687004435125, 0.303669017834393, 0.399060537444824, -0.0256113853532709, 0.220769628380184, -0.0219655194790159, -0.107070143975143, 0.0556497760193132, 0.112735950556606, -0.0425085949907385, 0.303669017834393, 0.854325399612162, 0.0924461969299186, 0.0860338995123319, -0.058252637830639, -0.274008787179135, 0.137462284747638, -0.102623896963213, -0.121636706660597, -0.613458260081048, 0.399060537444824, 0.0924461969299186, 1.18847861341054, 0.137701677167591, -0.257837324154815, -0.125751791453519, 0.0941660348081916, 0.000446561075573518, 0.0853052478227257, -0.292986225195268, -0.0256113853532709, 0.0860338995123319, 0.137701677167591, 0.792314183442372, -0.0574683130409049, -0.127914440182539, -0.0819510156005461, -0.281344369049677, -0.0111813042831139, 0.126875549501989, 0.220769628380184, -0.058252637830639, -0.257837324154815, -0.0574683130409049, 1.0494755855744, 0.0680630636010323, -0.22917987710939, 0.339801755016208, -0.0201254731753443, 0.0939246378491726, -0.0219655194790159, -0.274008787179135, -0.125751791453519, -0.127914440182539, 0.0680630636010323, 0.66321514703524, 0.00981756084965376, 0.396173748919282, 0.149427110129624, 0.00591847504584558, -0.107070143975143, 0.137462284747638, 0.0941660348081916, -0.0819510156005461, -0.22917987710939, 0.00981756084965376, 0.967089365130523, -0.0792898297089128, -0.021996990666367, 0.221189267455151, 0.0556497760193132, -0.102623896963213, 0.000446561075573518, -0.281344369049677, 0.339801755016208, 0.396173748919282, -0.0792898297089128, 1.12782790972313), .Dim = c(10L, 10L))
  u <- c(-1.04413462631653, 0.569719627442413, -0.135054603880824, 2.40161776050478, -0.0392400027331692, 0.689739362450777, 0.0280021587806661, -0.743273208882405, 0.188792299514343, -1.80495862889104)

  n <- NCOL(S)
  # truth <- mvndst(
  #   lower = rep(-Inf, n), upper = u, sigma = S, mu = numeric(n),
  #   maxvls = 1e8, abs_eps = 0, rel_eps = 1e-6, use_aprx = FALSE)
  truth <- structure(
    7.34188854727007e-05, n_it = 8514544L, inform = 0L,
    abserr = 6.991581314015e-11)

  set.seed(1)
  pedmod_res <- mvndst(
    lower = rep(-Inf, n), upper = u, sigma = S, mu = numeric(n),
    maxvls = 1e6, abs_eps = 0, rel_eps = 1e-4, use_aprx = TRUE)
  expect_equal(truth, pedmod_res, check.attributes = FALSE,
               tolerance = 1e-4)

  pedmod_res <- mvndst(
    lower = rep(-Inf, n), upper = u, sigma = S, mu = numeric(n),
    maxvls = 1e6, abs_eps = 0, rel_eps = 1e-4, use_aprx = TRUE,
    method = 1L)
  expect_equal(truth, pedmod_res, check.attributes = FALSE,
               tolerance = 1e-4)

  pedmod_res <- mvndst(
    lower = rep(-Inf, n), upper = u, sigma = S, mu = numeric(n),
    maxvls = 1e6, abs_eps = 0, rel_eps = 1e-4, use_aprx = FALSE,
    method = 1L, use_tilting = TRUE)
  expect_equal(truth, pedmod_res, check.attributes = FALSE,
               tolerance = 1e-4)

  # truth <- numDeriv::grad(
  #   function(par){
  #     set.seed(1)
  #     mu <- head(par, n)
  #     S[upper.tri(S, TRUE)] <- tail(par, -n)
  #     S[lower.tri(S)] <- t(S)[lower.tri(S)]
  #     mvndst(
  #       lower = rep(-Inf, n), upper = u, sigma = S, mu = mu,
  #       maxvls = 1e5, minvls = 1e5, abs_eps = 0, rel_eps = 1e-4,
  #       use_aprx = TRUE)
  #   }, c(numeric(n), S[upper.tri(S, TRUE)]),
  #   method.args = list(d = .01, r = 5))
  #
  # truth_d_mu <- head(truth, n)
  # truth_d_sigma <- matrix(0, n, n)
  # truth_d_sigma[upper.tri(truth_d_sigma, TRUE)] <- tail(truth, -n)
  # truth_d_sigma[upper.tri(truth_d_sigma)] <-
  #   truth_d_sigma[upper.tri(truth_d_sigma)] / 2
  # truth_d_sigma[lower.tri(truth_d_sigma)] <-
  #   t(truth_d_sigma)[lower.tri(truth_d_sigma)]
  # dput(truth_d_mu)
  # dput(truth_d_sigma)

  truth_d_mu <- c(-0.000157461632211839, -3.01036928126701e-05, -7.60976129272462e-05, -1.90210707410233e-07, -6.20466237614201e-05, -5.58297797374393e-05, -3.49433111089417e-05, -9.26690522169505e-05, -4.97868834578411e-05, -0.000117557032107872)
  truth_d_sigma <- structure(c(0.000123503880246806, 3.5827801208006e-05, 7.38643701253946e-05, 2.76420169512411e-07, 6.58632920923649e-05, 6.37443105474721e-05, 3.83664972373636e-05, 9.81072293316601e-05, 5.74214318863026e-05, 0.000126245117348469, 3.5827801208006e-05, -9.76241295200255e-06, 1.38616555000579e-05, 4.29561850060824e-08, 8.05540630568904e-06, 8.62849003874717e-06, 7.3481099539518e-06, 1.86630117103637e-05, 1.00938038240444e-05, 2.63193606077769e-05, 7.38643701253946e-05, 1.38616555000579e-05, 1.13528017642824e-05, 2.05016655831098e-07, 3.83461157266685e-05, 2.79627502973949e-05, 2.19864122396345e-05, 4.77483706142954e-05, 2.48013280030012e-05, 6.10765721660881e-05, 2.76420169512411e-07, 4.29561850060824e-08, 2.05016655831098e-07, -3.76767456570712e-07, 7.00684990236153e-08, 9.09453841839405e-08, 2.94375234340492e-08, 3.06653117756397e-08, 9.99330949549511e-08, 1.56298728247351e-07, 6.58632920923649e-05, 8.05540630568904e-06, 3.83461157266685e-05, 7.00684990236153e-08, 3.6442400732165e-07, 2.5730521159742e-05, 1.0960781106995e-05, 3.54541632891777e-05, 2.27916724461984e-05, 5.2805014726334e-05, 6.37443105474721e-05, 8.62849003874717e-06, 2.79627502973949e-05, 9.09453841839405e-08, 2.5730521159742e-05, -9.48305465470344e-06, 1.37516762042244e-05, 3.40814284868063e-05, 1.66662274946442e-05, 3.89889585603314e-05, 3.83664972373636e-05, 7.3481099539518e-06, 2.19864122396345e-05, 2.94375234340492e-08, 1.0960781106995e-05, 1.37516762042244e-05, -1.2151921516351e-05, 2.09090336068982e-05, 9.69811109279882e-06, 3.33260160679038e-05, 9.81072293316601e-05, 1.86630117103637e-05, 4.77483706142954e-05, 3.06653117756397e-08, 3.54541632891777e-05, 3.40814284868063e-05, 2.09090336068982e-05, 1.1866613555155e-05, 3.24954475936034e-05, 8.81522915176662e-05, 5.74214318863026e-05, 1.00938038240444e-05, 2.48013280030012e-05, 9.99330949549511e-08, 2.27916724461984e-05, 1.66662274946442e-05, 9.69811109279882e-06, 3.24954475936034e-05, -6.75562634890286e-06, 3.83594066683842e-05, 0.000126245117348469, 2.63193606077769e-05, 6.10765721660881e-05, 1.56298728247351e-07, 5.2805014726334e-05, 3.89889585603314e-05, 3.33260160679038e-05, 8.81522915176662e-05, 3.83594066683842e-05, 5.97651249134544e-05), dim = c(10L, 10L))

  pedmod_res <- mvndst_grad(
    lower = rep(-Inf, n), upper = u, sigma = S, mu = numeric(n),
    maxvls = 1e5, minvls = 1e5, abs_eps = 0, rel_eps = 1e-4, use_aprx = TRUE)

  expect_equal(pedmod_res$likelihood, truth, check.attributes = FALSE,
               tolerance = 1e-4)
  expect_equal(pedmod_res$d_mu, truth_d_mu, tolerance = 1e-4)
  expect_equal(pedmod_res$d_sigma, truth_d_sigma, tolerance = 1e-4)
})

test_that("examples in manual pages gives the correct answer for eval_pedigree_[ll]/[grad]/[hess]", {
  # three families as an example
  fam_dat <- list(
    list(
      y = c(FALSE, TRUE, FALSE, FALSE),
      X = structure(c(1, 1, 1, 1, 1.2922654151273, 0.358134905909256, -0.734963997107464, 0.855235473516044, -1.16189500386223, -0.387298334620742, 0.387298334620742, 1.16189500386223), .Dim = 4:3, .Dimnames = list( NULL, c("(Intercept)", "X1", ""))),
      rel_mat = structure(c(1, 0.5, 0.5, 0.125, 0.5, 1, 0.5, 0.125, 0.5, 0.5, 1, 0.125, 0.125, 0.125, 0.125, 1), .Dim = c(4L, 4L)),
      met_mat = structure(c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1), .Dim = c(4L, 4L))),
    list(
      y = c(FALSE, FALSE, FALSE),
      X = structure(c(1, 1, 1, -0.0388728997202442, -0.0913782435233639, -0.0801619722392612, -1, 0, 1), .Dim = c(3L, 3L)),
      rel_mat = structure(c(1, 0.5, 0.125, 0.5, 1, 0.125, 0.125, 0.125, 1), .Dim = c(3L, 3L)),
      met_mat = structure(c(1, 1, 0, 1, 1, 0, 0, 0, 1), .Dim = c(3L, 3L))),
    list(
      y = c(TRUE, FALSE),
      X = structure(c(1, 1, 0.305275750370738, -1.49482995913648,  -0.707106781186547, 0.707106781186547), .Dim = 2:3, .Dimnames = list( NULL, c("(Intercept)", "X1", ""))),
      rel_mat = structure(c(1, 0.5,  0.5, 1), .Dim = c(2L, 2L)),
      met_mat = structure(c(1, 1, 1, 1), .Dim = c(2L,  2L))))

  # get the data into the format needed for the package
  dat_arg <- lapply(fam_dat, function(x){
    # we need the following for each family:
    #   y: the zero-one outcomes.
    #   X: the design matrix for the fixed effects.
    #   scale_mats: list with the scale matrices for each type of effect.
    list(y = as.numeric(x$y), X = x$X,
         scale_mats = list(x$rel_mat, x$met_mat))
  })

  # get a pointer to the C++ object
  ptr <- pedigree_ll_terms(dat_arg, max_threads = 1L)

  # approximate the log marginal likelihood
  beta <- c(-1, 0.3, 0.2) # fixed effect coefficients
  scs <- c(0.5, 0.33)

  # truth <- eval_pedigree_ll(
  #   ptr = ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e9,
  #   rel_eps = 1e-8, minvls = 2000, use_aprx = FALSE)
  truth <- structure(-5.30140009701486, n_fails = 0L,
                     std = 3.3296139122967e-09)

  set.seed(44492929)
  ll1 <- eval_pedigree_ll(
    ptr = ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e6,
    rel_eps = 1e-5, minvls = 2000, use_aprx = FALSE)
  expect_equal(ll1, truth, tolerance = 1e-5)

  # with the approximation of pnorm and qnorm
  ll2 <- eval_pedigree_ll(
    ptr = ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e6,
    rel_eps = 1e-5, minvls = 2000, use_aprx = TRUE)
  expect_equal(ll2, truth, tolerance = 1e-5)

  # with Sobol sequences
  ll3 <- eval_pedigree_ll(
    ptr = ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e6,
    rel_eps = 1e-5, minvls = 2000, use_aprx = FALSE, method = 1L)
  expect_equal(ll3, truth, tolerance = 1e-5)

  # w/ weights
  # truth_dat <- dat_arg[c(1, 2, 2, 2)]
  # truth_ptr <- pedigree_ll_terms(truth_dat, 1L)
  # deriv_truth <- eval_pedigree_grad(
  #   ptr = truth_ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e8,
  #   rel_eps = 1e-6, minvls = 2000, use_aprx = FALSE)
  deriv_truth <- structure(
    c(-2.39509630042317, -0.10824194375542, -0.940601039742817,
      -0.314925453459061, -0.278867316602556),
    logLik = -4.88592585105156, n_fails = 0L,
    std = c(1.17013082330889e-09, 4.84625759211153e-08, 2.05599876859111e-08,
            3.71573106370985e-08, 9.95606406437179e-08, 7.01965244943291e-08))

  deriv_w_weight <- eval_pedigree_grad(
    ptr = ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e6,
    rel_eps = 1e-3, minvls = 2000, use_aprx = TRUE,
    cluster_weights = c(1, 3, 0))
  expect_equal(deriv_w_weight, deriv_truth, tolerance = 1e-3)

  ll_w_weight <- eval_pedigree_ll(
    ptr = ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e6,
    rel_eps = 1e-3, minvls = 2000, use_aprx = TRUE,
    cluster_weights = c(1, 3, 0))
  expect_equal(c(ll_w_weight), attr(deriv_truth, "logLik"), tolerance = 1e-3)

  # the hessian
  # fn <- function(par){
  #   set.seed(1)
  #   eval_pedigree_ll(
  #     ptr = ptr, par = par, abs_eps = -1, maxvls = 1e6,
  #     rel_eps = 1e-12, minvls = 1e6, use_aprx = FALSE,
  #     cluster_weights = c(1, 3, 0))
  # }
  # hess_true <- numDeriv::hessian(
  #   fn, c(beta, log(scs)), method.args = list(eps = 1e-3, r = 2))
  # dput(hess_true)
  # fn_org <- function(par){
  #   par[4:5] <- log(par[4:5])
  #   set.seed(1)
  #   eval_pedigree_ll(
  #       ptr = ptr, par = par, abs_eps = -1, maxvls = 1e6,
  #       rel_eps = 1e-12, minvls = 1e6, use_aprx = FALSE,
  #       cluster_weights = c(1, 3, 0))
  # }
  # hess_true_org <- numDeriv::hessian(
  #   fn_org, c(beta, scs), method.args = list(eps = 1e-3, r = 2))
  # dput(hess_true_org)

  hess_true <- structure(c(
    -2.88891553189542, -0.372877416220607, -0.276971459436541, 0.0281657330877043, 0.0343517626691977, -0.372877416220607, -1.04898316015941, 0.41871400203138, -0.0128126086495478, -0.0190693013188479, -0.276971459436541, 0.41871400203138, -2.39137670342856, 0.124092440925949, 0.0660723990531604, 0.0281657330877043, -0.0128126086495478, 0.124092440925949, -0.21514573450055, 0.0842551312188162, 0.0343517626691977, -0.0190693013188479, 0.0660723990531604, 0.0842551312188162, -0.215708721111546),
    .Dim = c(5L, 5L))
  hess_true_org <- structure(c(
    -2.88891553189542, -0.372877416220607, -0.276971459436541, 0.0563308676050589, 0.104095006476072, -0.372877416220607, -1.04898316015941, 0.41871400203138, -0.0256252124640631, -0.057785767571208, -0.276971459436541, 0.41871400203138, -2.39137670342856, 0.248184857359668, 0.200219327614127, 0.0563308676050589, -0.0256252124640631, 0.248184857359668, 0.399117752341619, 0.51063693960504, 0.104095006476072, -0.057785767571208, 0.200219327614127, 0.51063693960504, 0.579967091261364),
    .Dim = c(5L, 5L))

  hess_w_weight <- eval_pedigree_hess(
    ptr = ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e6,
    rel_eps = 1e-3, minvls = 2000, use_aprx = TRUE,
    cluster_weights = c(1, 3, 0))

  expect_equal(
    hess_w_weight, hess_true, check.attributes = FALSE, tolerance = 1e-3)
  expect_equal(
    attr(hess_w_weight, "hess_org"), hess_true_org, tolerance = 1e-3)

  hess_grad <- attr(hess_w_weight, "grad")
  expect_equal(hess_grad, c(deriv_w_weight), tolerance = 1e-3)
  expect_equal(
    attr(hess_w_weight, "logLik"), c(ll_w_weight), tolerance = 1e-4)

  dum_dat <- dat_arg[c(1, 2, 2, 2)]
  dum_ptr <- pedigree_ll_terms(dum_dat, 1L)
  hess_dum <- eval_pedigree_hess(
    ptr = dum_ptr, par = c(beta, log(scs)), abs_eps = -1, maxvls = 1e6,
    rel_eps = 1e-3, minvls = 2000, use_aprx = TRUE)
  attr(hess_w_weight, "n_fails") <- attr(hess_dum, "n_fails") <- NULL
  attr(hess_w_weight, "std") <- attr(hess_dum, "std") <- NULL
  expect_equal(hess_dum, hess_w_weight, tolerance = 1e-2)

  # with loadings
  dat_arg_loadings <- lapply(fam_dat, function(x){
    list(y = as.numeric(x$y), X = x$X, Z = x$X[, 1:2],
         scale_mats = list(x$rel_mat, x$met_mat))
  })

  ptr_loadings <-
    pedigree_ll_terms_loadings(dat_arg_loadings, max_threads = 1L)

  scs <- c(log(0.5) / 2, 0, log(0.33) / 2, 0)
  ll_loadings <- eval_pedigree_ll(
    ptr = ptr_loadings, par = c(beta, scs), abs_eps = -1, maxvls = 1e4,
    rel_eps = 1e-3, minvls = 2000, use_aprx = TRUE)

  expect_equal(ll_loadings, truth, tolerance = 1e-5, check.attributes = FALSE)

  scs <- c(log(0.5) / 2, 0.1, log(0.33) / 2, 0.2)
  # deriv_truth_loadings <-
  #   eval_pedigree_grad(
  #     ptr = ptr_loadings, par = c(beta, scs), abs_eps = -1, maxvls = 1e7,
  #     rel_eps = 1e-8, minvls = 1e7, use_aprx = TRUE)
  deriv_truth_loadings <- c(
    -0.139556620266106, 0.580820507649435, -1.61359030470197, -0.110463712185143, 0.242133123173374, -0.331049584276145, 0.0120970212929095)

  deriv_loadings <- eval_pedigree_grad(
    ptr = ptr_loadings, par = c(beta, scs), abs_eps = -1, maxvls = 1e4,
    rel_eps = 1e-3, minvls = 2000, use_aprx = TRUE)
  expect_equal(deriv_loadings, deriv_truth_loadings, tolerance = 1e-3,
               check.attributes = FALSE)
})
