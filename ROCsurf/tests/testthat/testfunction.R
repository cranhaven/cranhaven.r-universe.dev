library(testthat)        # load testthat package
library(ROCsurf)            # load our package


test_that("Type, length and error checking of dG, dW and dL functions", {
  # dG
  resultG <- dG(1:10, 2, 2)
  expect_type(resultG, "double")
  expect_length(resultG, 10)
  expect_vector(resultG, ptype = double(), size = 10)
  expect_error(dG(1:10, alpha=-1, beta=2), "alpha value must be greater than 0")
  expect_error(dG(1:10, 2, -1), "beta value must be greater than 0")

  # dW
  resultW <- dW(1:10, 2, 2)
  expect_type(resultW, "double")
  expect_length(resultW, 10)
  expect_vector(resultW, ptype = double(), size = 10)
  expect_error(dW(1:10, -1, 2), "alpha value must be greater than 0")
  expect_error(dW(1:10, 2, -1), "beta value must be greater than 0")




  # dL
  resultL <- dL(1:10, 2, 2)
  expect_type(resultL, "double")
  expect_length(resultL, 10)
  expect_vector(resultL, ptype = double(), size = 10)
  expect_error(dL(1:10, -1, 2), "alpha value must be greater than 0")
  expect_error(dL(1:10, 2, -1), "beta value must be greater than 0")
})


test_that("Type, length and error checking of pG, pW and pL functions", {
  # pG
  resultG <- pG(1:10, 2, 2)
  expect_type(resultG, "double")
  expect_length(resultG, 10)
  expect_vector(resultG, ptype = double(), size = 10)
  expect_error(pG(1:10, -1, 2), "alpha value must be greater than 0")
  expect_error(pG(1:10, 2, -1), "beta value must be greater than 0")

  # pW
  resultW <- pW(1:10, 2, 2)
  expect_type(resultW, "double")
  expect_length(resultW, 10)
  expect_vector(resultW, ptype = double(), size = 10)
  expect_error(pW(1:10, -1, 2), "alpha value must be greater than 0")
  expect_error(pW(1:10, 2, -1), "beta value must be greater than 0")

  # pL
  resultL <- pL(1:10, 2, 2)
  expect_type(resultL, "double")
  expect_length(resultL, 10)
  expect_vector(resultL, ptype = double(), size = 10)
  expect_error(pL(1:10, -1, 2), "alpha value must be greater than 0")
  expect_error(pL(1:10, 2, -1), "beta value must be greater than 0")
})


test_that("Type, length and error checking of qG, qW and qL functions", {
  # qG
  resultG <- qG(seq(0, 1, length.out = 10), 2, 2)
  expect_type(resultG, "double")
  expect_length(resultG, 10)
  expect_vector(resultG, ptype = double(), size = 10)
  expect_error(qG(seq(0, 1, length.out = 10), -1, 2),
               "alpha value must be greater than 0")
  expect_error(qG(seq(0, 1, length.out = 10), 2, -1),
               "beta value must be greater than 0")
  expect_error(qG(c(-0.1, 0.5, 1.1), 2, 2), "p must be between 0 and 1")

  # qW
  resultW <- qW(seq(0, 1, length.out = 10), 2, 2)
  expect_type(resultW, "double")
  expect_length(resultW, 10)
  expect_vector(resultW, ptype = double(), size = 10)
  expect_error(qW(seq(0, 1, length.out = 10), -1, 2),
               "alpha value must be greater than 0")
  expect_error(qW(seq(0, 1, length.out = 10), 2, -1),
               "beta value must be greater than 0")
  expect_error(qW(c(-0.1, 0.5, 1.1), 2, 2), "p must be between 0 and 1")

  # qL
  resultL <- qL(seq(0, 1, length.out = 10), 2, 2)
  expect_type(resultL, "double")
  expect_length(resultL, 10)
  expect_vector(resultL, ptype = double(), size = 10)
  expect_error(qL(seq(0, 1, length.out = 10), -1, 2),
               "alpha value must be greater than 0")
  expect_error(qL(seq(0, 1, length.out = 10), 2, -1),
               "beta value must be greater than 0")
  expect_error(qL(c(-0.1, 0.5, 1.1), 2, 2), "p must be between 0 and 1")
})

test_that("Type, length and error checking of rG, rW and rL functions", {
  # rG
  resultG <- rG(10, 2, 2)
  expect_type(resultG, "double")
  expect_length(resultG, 10)
  expect_vector(resultG, ptype = double(), size = 10)
  expect_error(rG(0, 2, 2), "n value must be >=1")
  expect_error(rG(10, -1, 2), "alpha value must be greater than 0")
  expect_error(rG(10, 2, -1), "beta value must be greater than 0")

  # rW
  resultW <- rW(10, 2, 2)
  expect_type(resultW, "double")
  expect_length(resultW, 10)
  expect_vector(resultW, ptype = double(), size = 10)
  expect_error(rW(0, 2, 2), "n value must be >=1")
  expect_error(rW(10, -1, 2), "alpha value must be greater than 0")
  expect_error(rW(10, 2, -1), "beta value must be greater than 0")

  # rL
  resultL <- rL(10, 2, 2)
  expect_type(resultL, "double")
  expect_length(resultL, 10)
  expect_vector(resultL, ptype = double(), size = 10)
  expect_error(rL(0, 2, 2), "n value must be >=1")
  expect_error(rL(10, -1, 2), "alpha value must be greater than 0")
  expect_error(rL(10, 2, -1), "beta value must be greater than 0")
})


test_that("function return a double or list",
          {
        expect_type(r.tc_vus(true_param=c(alpha1=2,beta1=.22,alpha2=8,beta2=.8,
                                              alpha3=1,beta3=.1),
                                 model=c("GWL"), method=c("TRUE")), "double")
          expect_type(r.tc_vus(true_param=c(alpha1=2,beta1=.2,alpha2=5,beta2=.2,
                                              alpha3=4,beta3=1.9),
                                 model=c("GGW"), method=c("TRUE")), "double")

          expect_type(r.tc_vus(true_param=c(alpha1=1,beta1=2,alpha2=6,beta2=.8,
                                              alpha3=8,beta3=8),
                                 model=c("WGW"), method=c("TRUE")), "double")
            expect_type(r.tc_vus(true_param=c(alpha1=7,beta1=2,alpha2=7,beta2=3,
                                              alpha3=7,beta3=4),
                                 model=c("WWW"), method=c("TRUE")), "double")
            expect_type(r.tc_vus(true_param=c(alpha1=4,beta1=1,alpha2=4,beta2=2,
                                              alpha3=4,beta3=3),
                                 model=c("GGG"), method=c("TRUE")), "double")
         expect_type(r.tc_vus(true_param=c(alpha1=1,beta1=.1,alpha2=2,beta2=.4,
                                              alpha3=3,beta3=.1),
                                 model=c("LLL"), method=c("TRUE")), "double")

            expect_type(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                                       1.0066519, 1.0372385),
                                   y=c(1.5200108, 0.0617668, 6.0647578,
                                       0.7594201, 0.3714640),
                                   z=c(3.485613, 4.939489, 6.072339,
                                       3.995163, 2.893617),
              true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
  init_index=c(median(c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385)),
              median(c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640))),
                                   model=c("GWL"),
                                   method=c("TRUE")), "double")
            expect_type(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                                       1.0066519, 1.0372385),
                                   y=c(1.5200108, 0.0617668, 6.0647578,
                                       0.7594201, 0.3714640),
                                   z=c(3.485613, 4.939489, 6.072339,
                                       3.995163, 2.893617),
           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
  init_index=c(median(c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385)),
              median(c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640))),
                                   model=c("GGW"),
                                   method=c("TRUE")), "double")

            expect_type(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                                       1.0066519, 1.0372385),
                                   y=c(1.5200108, 0.0617668, 6.0647578,
                                       0.7594201, 0.3714640),
                                   z=c(3.485613, 4.939489, 6.072339, 3.995163,
                                       2.893617),
                                   true_param=c(alpha1=1,beta1=1,alpha2=1,
                                                beta2=1,alpha3=1,beta3=1),
                                   init_index=c(median(c(0.7736414, 1.0131692,
                                                         0.8667612, 1.0066519,
                                                         1.0372385)),
                                                median(c(1.5200108, 0.0617668,
                                                         6.0647578, 0.7594201,
                                                         0.3714640))),
                                   model=c("WGW"),
                                   method=c("TRUE")), "double")
            expect_type(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                                       1.0066519, 1.0372385),
                                   y=c(1.5200108, 0.0617668, 6.0647578,
                                       0.7594201, 0.3714640),
                                   z=c(3.485613, 4.939489, 6.072339,
                                       3.995163, 2.893617),
                                   true_param=c(alpha1=1,beta1=1,alpha2=1,
                                                beta2=1,alpha3=1,beta3=1),
                                   init_index=c(median(c(0.7736414, 1.0131692,
                                                         0.8667612, 1.0066519,
                                                         1.0372385)),
                                                median(c(1.5200108, 0.0617668,
                                                         6.0647578, 0.7594201,
                                                         0.3714640))),
                                   model=c("WWW"),
                                   method=c("TRUE")), "double")
            expect_type(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                                       1.0066519, 1.0372385),
                                   y=c(1.5200108, 0.0617668, 6.0647578,
                                       0.7594201, 0.3714640),
                                   z=c(3.485613, 4.939489, 6.072339,
                                       3.995163, 2.893617),
                                   true_param=c(alpha1=1,beta1=1,alpha2=1,
                                                beta2=1,alpha3=1,beta3=1),
                                   init_index=c(median(c(0.7736414, 1.0131692,
                                                         0.8667612, 1.0066519,
                                                         1.0372385)),
                                                median(c(1.5200108, 0.0617668,
                                                         6.0647578, 0.7594201,
                                                         0.3714640))),
                                   model=c("GGG"),
                                   method=c("TRUE")), "double")
            expect_type(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                                       1.0066519, 1.0372385),
                                   y=c(1.5200108, 0.0617668, 6.0647578,
                                       0.7594201, 0.3714640),
                                   z=c(3.485613, 4.939489, 6.072339,
                                       3.995163, 2.893617),
                                   true_param=c(alpha1=1,beta1=1,alpha2=1,
                                                beta2=1,alpha3=1,beta3=1),
                                   init_index=c(median(c(0.7736414, 1.0131692,
                                                         0.8667612, 1.0066519,
                                                         1.0372385)),
                                                median(c(1.5200108, 0.0617668,
                                                         6.0647578, 0.7594201,
                                                         0.3714640))),
                                   model=c("LLL"),
                                   method=c("TRUE")), "double")

            expect_type(r.tc_graph(true_param = c(alpha1 = 1, beta1 = 1,
                                                  alpha2 = 1, beta2 = 1,
                                                  alpha3 = 1, beta3 = 1),
                                   model = "GWL",  method = "TRUE"),"list")
            expect_type(r.tc_graph(true_param = c(alpha1 = 1, beta1 = 1,
                                                  alpha2 = 1, beta2 = 1,
                                                  alpha3 = 1, beta3 = 1),
                                   model = "GGW",  method = "TRUE"),"list")


          })


# Test whether the output contains the right number
test_that("functions returns a list with the specified length", {
  expect_length(r.tc_vus(true_param=c(alpha1=2,beta1=.22,alpha2=8,beta2=.8,
                                      alpha3=1,beta3=.1),
                         model=c("GWL"), method=c("TRUE")), 1)
  expect_length(r.tc_vus(true_param=c(alpha1=2,beta1=.2,alpha2=5,beta2=.2,
                                      alpha3=4,beta3=1.9),
                         model=c("GGW"), method=c("TRUE")), 1)

  expect_length(r.tc_vus(true_param=c(alpha1=1,beta1=2,alpha2=6,beta2=.8,
                                      alpha3=8,beta3=8),
                         model=c("WGW"), method=c("TRUE")), 1)
  expect_length(r.tc_vus(true_param=c(alpha1=7,beta1=2,alpha2=7,beta2=3,
                                      alpha3=7,beta3=4),
                         model=c("WWW"), method=c("TRUE")), 1)
  expect_length(r.tc_vus(true_param=c(alpha1=4,beta1=1,alpha2=4,beta2=2,
                                      alpha3=4,beta3=3),
                         model=c("GGG"), method=c("TRUE")), 1)
  expect_length(r.tc_vus(true_param=c(alpha1=1,beta1=.1,alpha2=2,beta2=.4,
                                      alpha3=3,beta3=.1),
                         model=c("LLL"), method=c("TRUE")), 1)

  expect_length(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                               1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578,
                               0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339,
                               3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,
                                        alpha3=1,beta3=1),
                           init_index=c(median(c(0.7736414, 1.0131692,
                                                 0.8667612, 1.0066519,
                                                 1.0372385)),
                                        median(c(1.5200108, 0.0617668,
                                                 6.0647578, 0.7594201,
                                                 0.3714640))),
                           model=c("GWL"),method=c("TRUE")), 25)
  expect_length(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                               1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578,
                               0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339,
                               3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,
                                        alpha3=1,beta3=1),
                           init_index=c(median(c(0.7736414, 1.0131692,
                                                 0.8667612, 1.0066519,
                                                 1.0372385)),
                                        median(c(1.5200108, 0.0617668,
                                                 6.0647578, 0.7594201,
                                                 0.3714640))),
                           model=c("GGW"),method=c("TRUE")), 25)

  expect_length(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                               1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578,
                               0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339,
                               3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,
                                        beta2=1,alpha3=1,beta3=1),
                           init_index=c(median(c(0.7736414, 1.0131692,
                                                 0.8667612, 1.0066519,
                                                 1.0372385)),
                                        median(c(1.5200108,
                                                 0.0617668, 6.0647578,
                                                 0.7594201, 0.3714640))),
                           model=c("WGW"),
                           method=c("TRUE")), 25)
  expect_length(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                               1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578,
                               0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339,
                               3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,
                                        alpha3=1,beta3=1),
                           init_index=c(median(c(0.7736414, 1.0131692,
                                                 0.8667612, 1.0066519,
                                                 1.0372385)),
                                        median(c(1.5200108, 0.0617668,
                                                 6.0647578, 0.7594201,
                                                 0.3714640))),
                           model=c("WWW"),
                           method=c("TRUE")), 25)
  expect_length(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                               1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578,
                               0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339,
                               3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,
                                        beta2=1,alpha3=1,beta3=1),
                           init_index=c(median(c(0.7736414, 1.0131692,
                                                 0.8667612, 1.0066519,
                                                 1.0372385)),
                                        median(c(1.5200108, 0.0617668,
                                                 6.0647578, 0.7594201,
                                                 0.3714640))),
                           model=c("GGG"),
                           method=c("TRUE")), 25)
  expect_length(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                               1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578,
                               0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339,
                               3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,
                                        beta2=1,alpha3=1,beta3=1),
                           init_index=c(median(c(0.7736414, 1.0131692,
                                                 0.8667612, 1.0066519,
                                                 1.0372385)),
                                        median(c(1.5200108, 0.0617668,
                                                 6.0647578, 0.7594201,
                                                 0.3714640))),
                           model=c("LLL"),
                           method=c("TRUE")), 25)
})

# Test whether the output is a vector with the expected size
test_that("functions returns a  vector with the expected size", {

  expect_vector(r.tc_vus(true_param=c(alpha1=2,beta1=.22,alpha2=8,beta2=.8,
                                      alpha3=1,beta3=.1),
                      model=c("GWL"), method=c("TRUE")), ptype=double(),size=1)
  expect_vector(r.tc_vus(true_param=c(alpha1=2,beta1=.2,alpha2=5,beta2=.2,
                                      alpha3=4,beta3=1.9),
                      model=c("GGW"), method=c("TRUE")), ptype=double(),size=1)

expect_vector(r.tc_vus(true_param=c(alpha1=1,beta1=2,alpha2=6,beta2=.8,
                                    alpha3=8,beta3=8),
                         model=c("WGW"), method=c("TRUE")), ptype=double(),
              size=1)
expect_vector(r.tc_vus(true_param=c(alpha1=7,beta1=2,alpha2=7,beta2=3,alpha3=7,
                                    beta3=4),
                         model=c("WWW"), method=c("TRUE")), ptype=double(),
              size=1)
expect_vector(r.tc_vus(true_param=c(alpha1=4,beta1=1,alpha2=4,beta2=2,alpha3=4,
                                      beta3=3),
                         model=c("GGG"), method=c("TRUE")), ptype=double(),
              size=1)
expect_vector(r.tc_vus(true_param=c(alpha1=1,beta1=.1,alpha2=2,beta2=.4,
                                    alpha3=3,beta3=.1),
                       model=c("LLL"), method=c("TRUE")), ptype=double(),size=1)

  expect_vector(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                               1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578,
                               0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339,
                               3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,
                                        alpha3=1,beta3=1),
                           init_index=c(median(c(0.7736414, 1.0131692,
                                                 0.8667612, 1.0066519,
                                                 1.0372385)),
                                        median(c(1.5200108, 0.0617668,
                                                 6.0647578, 0.7594201,
                                                 0.3714640))),
                        model=c("GWL"),method=c("TRUE")), ptype=rbind(),size=5)
  expect_vector(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,
                                        alpha3=1,beta3=1),
                           init_index=c(median(c(0.7736414, 1.0131692,
                                                 0.8667612, 1.0066519,
                                                 1.0372385)),
                                        median(c(1.5200108, 0.0617668,
                                                 6.0647578, 0.7594201,
                                                 0.3714640))),
                           model=c("GGW"),method=c("TRUE")), ptype=rbind(),
                size=5)

  expect_vector(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,
                                        alpha3=1,beta3=1),
                           init_index=c(median(c(0.7736414, 1.0131692,
                                                 0.8667612, 1.0066519,
                                                 1.0372385)),
                                        median(c(1.5200108, 0.0617668,
                                                 6.0647578, 0.7594201,
                                                 0.3714640))),
                           model=c("WGW"),
                           method=c("TRUE")), ptype=rbind(),size=5)
  expect_vector(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,
                                        alpha3=1,beta3=1),
                           init_index=c(median(c(0.7736414, 1.0131692,
                                                 0.8667612, 1.0066519,
                                                 1.0372385)),
                                        median(c(1.5200108, 0.0617668,
                                                 6.0647578, 0.7594201,
                                                 0.3714640))),
                           model=c("WWW"),
                           method=c("TRUE")), ptype=rbind(),size=5)
  expect_vector(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,
                                        alpha3=1,beta3=1),
                           init_index=c(median(c(0.7736414, 1.0131692,
                                                 0.8667612, 1.0066519,
                                                 1.0372385)),
                                        median(c(1.5200108, 0.0617668,
                                                 6.0647578, 0.7594201,
                                                 0.3714640))),
                           model=c("GGG"),
                           method=c("TRUE")), ptype=rbind(),size=5)
  expect_vector(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,
                                        beta2=1,alpha3=1,beta3=1),
                           init_index=c(median(c(0.7736414, 1.0131692,
                                                 0.8667612, 1.0066519,
                                                 1.0372385)),
                                        median(c(1.5200108, 0.0617668,
                                                 6.0647578, 0.7594201,
                                                 0.3714640))),
                           model=c("LLL"),
                           method=c("TRUE")), ptype=rbind(),size=5)
})



# Test whether the output is an error
test_that("functions returns error", {

  expect_error(r.tc_vus(
    init_param = c(-1, 1, 1, 1, 1, 1),model="GWL",method="MLE"),
    "alpha1 value must be greater than 0")
  expect_error(
    r.tc_vus( init_param = c(1, -1, 1, 1, 1, 1),model="GWL",method="MLE"),
    "beta1 value must be greater than 0")
  expect_error(r.tc_vus(
    init_param = c(1, 1, -1, 1, 1, 1),model="GWL",method="MLE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_vus(
    init_param = c(1, 1, 1, -1, 1, 1),model="GWL",method="MLE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_vus(
    init_param = c(1, 1, 1, 1, -1, 1),model="GWL",method="MLE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_vus(
    init_param = c(1, 1, 1, 1, 1, -1),model="GWL",method="MLE"),
               "beta3 value must be greater than 0")

  expect_error(r.tc_vus( true_param  = c(-1, 1, 1, 1, 1, 1),
                         model="GWL",method="TRUE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, -1, 1, 1, 1, 1),
                         model="GWL",method="TRUE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, -1, 1, 1, 1),
                         model="GWL",method="TRUE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, -1, 1, 1),
                         model="GWL",method="TRUE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, -1, 1),
                         model="GWL",method="TRUE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, 1, -1),
                         model="GWL",method="TRUE"),
               "beta3 value must be greater than 0")



  expect_error(r.tc_vus( init_param = c(-1, 1, 1, 1, 1, 1),
                         model="GGW",method="MLE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, -1, 1, 1, 1, 1),
                         model="GGW",method="MLE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, -1, 1, 1, 1),
                         model="GGW",method="MLE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, -1, 1, 1),
                         model="GGW",method="MLE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, -1, 1),
                         model="GGW",method="MLE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, 1, -1),
                         model="GGW",method="MLE"),
               "beta3 value must be greater than 0")

  expect_error(r.tc_vus( true_param  = c(-1, 1, 1, 1, 1, 1),
                         model="GGW",method="TRUE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, -1, 1, 1, 1, 1),
                         model="GGW",method="TRUE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, -1, 1, 1, 1),
                         model="GGW",method="TRUE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, -1, 1, 1),
                         model="GGW",method="TRUE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, -1, 1),
                         model="GGW",method="TRUE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, 1, -1),
                         model="GGW",method="TRUE"),
               "beta3 value must be greater than 0")



  expect_error(r.tc_vus( init_param = c(-1, 1, 1, 1, 1, 1),
                         model="WGW",method="MLE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, -1, 1, 1, 1, 1),
                         model="WGW",method="MLE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, -1, 1, 1, 1),
                         model="WGW",method="MLE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, -1, 1, 1),
                         model="WGW",method="MLE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, -1, 1),
                         model="WGW",method="MLE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, 1, -1),
                         model="WGW",method="MLE"),
               "beta3 value must be greater than 0")

  expect_error(r.tc_vus( true_param  = c(-1, 1, 1, 1, 1, 1),
                         model="WGW",method="TRUE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, -1, 1, 1, 1, 1),
                         model="WGW",method="TRUE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, -1, 1, 1, 1),
                         model="WGW",method="TRUE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, -1, 1, 1),
                         model="WGW",method="TRUE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, -1, 1),
                         model="WGW",method="TRUE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, 1, -1),
                         model="WGW",method="TRUE"),
               "beta3 value must be greater than 0")

  expect_error(r.tc_vus( init_param = c(-1, 1, 1, 1, 1, 1),
                         model="WWW",method="MLE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, -1, 1, 1, 1, 1),
                         model="WWW",method="MLE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, -1, 1, 1, 1),
                         model="WWW",method="MLE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, -1, 1, 1),
                         model="WWW",method="MLE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, -1, 1),
                         model="WWW",method="MLE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, 1, -1),
                         model="WWW",method="MLE"),
               "beta3 value must be greater than 0")

  expect_error(r.tc_vus( true_param  = c(-1, 1, 1, 1, 1, 1),
                         model="WWW",method="TRUE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, -1, 1, 1, 1, 1),
                         model="WWW",method="TRUE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, -1, 1, 1, 1),
                         model="WWW",method="TRUE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, -1, 1, 1),
                         model="WWW",method="TRUE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, -1, 1),
                         model="WWW",method="TRUE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, 1, -1),
                         model="WWW",method="TRUE"),
               "beta3 value must be greater than 0")


  expect_error(r.tc_vus( init_param = c(-1, 1, 1, 1, 1, 1),
                         model="GGG",method="MLE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, -1, 1, 1, 1, 1),
                         model="GGG",method="MLE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, -1, 1, 1, 1),
                         model="GGG",method="MLE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, -1, 1, 1),
                         model="GGG",method="MLE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, -1, 1),
                         model="GGG",method="MLE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, 1, -1),
                         model="GGG",method="MLE"),
               "beta3 value must be greater than 0")

  expect_error(r.tc_vus( true_param  = c(-1, 1, 1, 1, 1, 1),
                         model="GGG",method="TRUE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, -1, 1, 1, 1, 1),
                         model="GGG",method="TRUE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, -1, 1, 1, 1),
                         model="GGG",method="TRUE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, -1, 1, 1),
                         model="GGG",method="TRUE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, -1, 1),
                         model="GGG",method="TRUE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, 1, -1),
                         model="GGG",method="TRUE"),
               "beta3 value must be greater than 0")


  expect_error(r.tc_vus( init_param = c(-1, 1, 1, 1, 1, 1),
                         model="LLL",method="MLE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, -1, 1, 1, 1, 1),
                         model="LLL",method="MLE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, -1, 1, 1, 1),
                         model="LLL",method="MLE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, -1, 1, 1),
                         model="LLL",method="MLE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, -1, 1),
                         model="LLL",method="MLE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, 1, -1),
                         model="LLL",method="MLE"),
               "beta3 value must be greater than 0")

  expect_error(r.tc_vus( true_param  = c(-1, 1, 1, 1, 1, 1),
                         model="LLL",method="TRUE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, -1, 1, 1, 1, 1),
                         model="LLL",method="TRUE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, -1, 1, 1, 1),
                         model="LLL",method="TRUE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, -1, 1, 1),
                         model="LLL",method="TRUE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, -1, 1),
                         model="LLL",method="TRUE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, 1, -1),
                         model="LLL",method="TRUE"),
               "beta3 value must be greater than 0")



  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612,
                               1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578,
                               0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339,
                               3.995163, 2.893617),
                           init_param = c(-1, 1, 1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GWL",
                           method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612,
                               1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578,
                               0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339,
                               3.995163, 2.893617),
                           init_param = c(1, -1, 1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GWL",
                           method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, -1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GWL",
                           method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, -1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GWL",
                           method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, 1, -1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GWL",
                           method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, 1, 1, -1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GWL",
                           method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(-1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GWL",
                          method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(1, -1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GWL",
                          method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, -1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GWL",
                          method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(1, 1, 1, -1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108,
                                                  0.0617668, 6.0647578,
                                                  0.7594201, 0.3714640))),
                          model="GWL",method="TRUE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, 1, 1, -1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GWL",
                          method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, 1, 1, 1, -1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GWL",
                           method="TRUE"), "beta3 value must be greater than 0")




  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(-1, 1, 1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGW",
                           method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, -1, 1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGW",
                           method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, -1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGW",
                           method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, -1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGW",
                           method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, 1, -1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGW",
                           method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, 1, 1, -1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),
                           model="GGW",method="MLE"),
               "beta3 value must be greater than 0")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                              1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578,
                              0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339,
                              3.995163, 2.893617),
                          true_param  = c(-1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414,
                                                  1.0131692, 0.8667612,
                                                  1.0066519, 1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GGW",
                          method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(1, -1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GGW",
                          method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, -1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGW",
                        method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(1, 1, 1, -1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GGW",
                          method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, 1, 1, -1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGW",
                          method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, 1, 1, 1, -1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),
                           model="GGW",method="TRUE"),
               "beta3 value must be greater than 0")









  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(-1, 1, 1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WGW",
                           method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, -1, 1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WGW",
                           method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, -1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WGW",
                           method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, -1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WGW",
                           method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, 1, -1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WGW",
                           method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, 1, 1, -1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WGW",
                           method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(-1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WGW",
                          method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(1, -1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WGW",
                          method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, -1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WGW",
                          method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(1, 1, 1, -1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WGW",
                          method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, 1, 1, -1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WGW",
                        method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, 1, 1, 1, -1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WGW",
                           method="TRUE"), "beta3 value must be greater than 0")






  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(-1, 1, 1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WWW",
                           method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, -1, 1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WWW",
                           method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, -1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WWW",
                           method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, -1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WWW",
                           method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, 1, -1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WWW",
                           method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, 1, 1, -1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WWW",
                           method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(-1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WWW",
                          method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(1, -1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WWW",
                          method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, -1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WWW",
                         method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(1, 1, 1, -1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WWW",
                          method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, 1, 1, -1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WWW",
                        method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, 1, 1, 1, -1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="WWW",
                           method="TRUE"), "beta3 value must be greater than 0")





  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(-1, 1, 1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGG",
                           method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, -1, 1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGG",
                           method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, -1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGG",
                           method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, -1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGG",
                           method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, 1, -1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGG",
                           method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, 1, 1, -1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGG",
                           method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(-1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GGG",
                          method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(1, -1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GGG",
                          method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, -1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGG",
                          method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(1, 1, 1, -1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GGG",
                          method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, 1, 1, -1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGG",
                          method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, 1, 1, 1, -1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="GGG",
                           method="TRUE"), "beta3 value must be greater than 0")





  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(-1, 1, 1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="LLL",
                           method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, -1, 1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="LLL",
                           method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, -1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="LLL",
                           method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, -1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="LLL",
                           method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, 1, -1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="LLL",
                           method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),init_param = c(1, 1, 1, 1, 1, -1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="LLL",
                           method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(-1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="LLL",
                          method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(1, -1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="LLL",
                          method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, -1, 1, 1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="LLL",
                          method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), true_param  = c(1, 1, 1, -1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="LLL",
                          method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, 1, 1, -1, 1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="LLL",
                          method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                               1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                               0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163,
                               2.893617),true_param  = c(1, 1, 1, 1, 1, -1),
                           init_index = c(median(c(0.7736414, 1.0131692,
                                                   0.8667612, 1.0066519,
                                                   1.0372385)),
                                          median(c(1.5200108, 0.0617668,
                                                   6.0647578, 0.7594201,
                                                   0.3714640))),model="LLL",
                           method="TRUE"), "beta3 value must be greater than 0")










  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(-1, 1, 1, 1, 1, 1),
                          model="GWL",method="MLE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, -1, 1, 1, 1, 1),
                          model="GWL",method="MLE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, -1, 1, 1, 1),
                          model="GWL",method="MLE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, -1, 1, 1),
                          model="GWL",method="MLE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, 1, -1, 1),
                          model="GWL",method="MLE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612,
                              1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578,
                              0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339,
                              3.995163, 2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, 1, 1, -1),
                          model="GWL",method="MLE"),
               "beta3 value must be greater than 0")

  expect_error(r.tc_graph( true_param  = c(-1, 1, 1, 1, 1, 1),model="GWL",
                          method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, -1, 1, 1, 1, 1),model="GWL",
                           method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, -1, 1, 1, 1),model="GWL",
                          method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, -1, 1, 1),model="GWL",
                           method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, -1, 1),model="GWL",
                          method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, 1, -1),model="GWL",
                           method="TRUE"), "beta3 value must be greater than 0")



  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(-1, 1, 1, 1, 1, 1),
                          model="GGW",method="MLE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612,
                              1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578,
                              0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339,
                              3.995163, 2.893617),empirical = FALSE,
                          init_param = c(1, -1, 1, 1, 1, 1),
                          model="GGW",method="MLE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612,
                              1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578,
                              0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339,
                              3.995163, 2.893617),empirical = FALSE,
                          init_param = c(1, 1, -1, 1, 1, 1),
                          model="GGW",method="MLE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612,
                              1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578,
                              0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339,
                              3.995163, 2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, -1, 1, 1),
                          model="GGW",method="MLE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612,
                              1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578,
                              0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339,
                              3.995163, 2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, 1, -1, 1),
                          model="GGW",method="MLE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612,
                              1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578,
                              0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339,
                              3.995163, 2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, 1, 1, -1),
                          model="GGW",method="MLE"),
               "beta3 value must be greater than 0")

  expect_error(r.tc_graph( true_param  = c(-1, 1, 1, 1, 1, 1),
                           model="GGW",method="TRUE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, -1, 1, 1, 1, 1),
                           model="GGW",method="TRUE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, -1, 1, 1, 1),
                           model="GGW",method="TRUE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, -1, 1, 1),
                           model="GGW",method="TRUE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, -1, 1),
                           model="GGW",method="TRUE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, 1, -1),
                           model="GGW",method="TRUE"),
               "beta3 value must be greater than 0")



  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(-1, 1, 1, 1, 1, 1),model="WGW",
                          method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, -1, 1, 1, 1, 1),
                          model="WGW",method="MLE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612,
                              1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578,
                              0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339,
                              3.995163, 2.893617),empirical = FALSE,
                          init_param = c(1, 1, -1, 1, 1, 1),model="WGW",
                          method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, -1, 1, 1),model="WGW",
                          method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, 1, -1, 1),model="WGW",
                          method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, 1, 1, -1),
                          model="WGW",method="MLE"),
               "beta3 value must be greater than 0")


  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(-1, 1, 1, 1, 1, 1),model="WWW",
                          method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, -1, 1, 1, 1, 1),model="WWW",
                          method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, -1, 1, 1, 1),model="WWW",
                          method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, -1, 1, 1),model="WWW",
                          method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, 1, -1, 1),model="WWW",
                          method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, 1, 1, -1),model="WWW",
                          method="MLE"), "beta3 value must be greater than 0")



  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(-1, 1, 1, 1, 1, 1),model="GGG",
                          method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, -1, 1, 1, 1, 1),model="GGG",
                          method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, -1, 1, 1, 1),model="GGG",
                          method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, -1, 1, 1),model="GGG",
                          method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, 1, -1, 1),model="GGG",
                          method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, 1, 1, -1),model="GGG",
                          method="MLE"), "beta3 value must be greater than 0")



  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(-1, 1, 1, 1, 1, 1),model="LLL",
                          method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, -1, 1, 1, 1, 1),model="LLL",
                          method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, -1, 1, 1, 1),model="LLL",
                          method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, -1, 1, 1),model="LLL",
                          method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, 1, -1, 1),model="LLL",
                          method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612,
                              1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578,
                              0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339,
                              3.995163, 2.893617),empirical = FALSE,
                          init_param = c(1, 1, 1, 1, 1, -1),model="LLL",
                          method="MLE"), "beta3 value must be greater than 0")



  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(3.485613, 4.939489, 6.072339, 3.995163,
                            2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                        model="GWL",method="MLE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(x),median(c(1.5200108,
                                                            0.0617668,
                                                            6.0647578,
                                                            0.7594201,
                                                            0.3714640))),
                          model="GWL",method="MLE"),
        "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GWL",
                           method="MLE"),
          "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="WGW",
                           method="MLE"),
   "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="WGW",
                           method="AD"),
    "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="WGW",
                           method="CvM"),
      "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="WGW",
                           method="LSE"),
     "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="WGW",
                           method="WLSE"),
      "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="WWW",
                           method="MLE"),
          "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="WWW",
                           method="AD"),
         "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="WWW",
                           method="CvM"),
        "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="WWW",
                           method="LSE"),
        "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="WWW",
                           method="WLSE"),
         "Optimization did not converge.Please check your initial parameters.")




  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGG",
                           method="MLE"),
        "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGG",
                           method="AD"),
         "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGG",
                           method="CvM"),
         "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGG",
                           method="LSE"),
         "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGG",
                           method="WLSE"),
         "Optimization did not converge.Please check your initial parameters.")



  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="LLL",
                           method="MLE"),
        "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="LLL",
                           method="AD"),
        "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="LLL",
                           method="CvM"),
       "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="LLL",
                           method="LSE"),
       "Optimization did not converge.Please check your initial parameters.")
  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="LLL",
                           method="WLSE"),
        "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_graph( true_param = c(-1, 1, 1, 1, 1, 1),model="GWL",
                           method="TRUE"),
       "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, -1, 1, 1, 1, 1),model="GWL",
                           method="TRUE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, -1, 1, 1, 1),model="GWL",
                           method="TRUE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, -1, 1, 1),model="GWL",
                           method="TRUE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, 1, -1, 1),model="GWL",
                           method="TRUE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, 1, 1, -1),model="GWL",
                           method="TRUE"),
               "beta3 value must be greater than 0")



  expect_error(r.tc_graph( true_param = c(-1, 1, 1, 1, 1, 1),model="GGW",
                           method="TRUE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, -1, 1, 1, 1, 1),model="GGW",
                           method="TRUE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, -1, 1, 1, 1),model="GGW",
                           method="TRUE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, -1, 1, 1),model="GGW",
                           method="TRUE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, 1, -1, 1),model="GGW",
                           method="TRUE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, 1, 1, -1),model="GGW",
                           method="TRUE"),
               "beta3 value must be greater than 0")


  expect_error(r.tc_graph( true_param = c(-1, 1, 1, 1, 1, 1),model="WGW",
                           method="TRUE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, -1, 1, 1, 1, 1),model="WGW",
                           method="TRUE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, -1, 1, 1, 1),model="WGW",
                           method="TRUE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, -1, 1, 1),model="WGW",
                           method="TRUE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, 1, -1, 1),model="WGW",
                           method="TRUE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, 1, 1, -1),model="WGW",
                           method="TRUE"),
               "beta3 value must be greater than 0")




  expect_error(r.tc_graph( true_param = c(-1, 1, 1, 1, 1, 1),model="WWW",
                           method="TRUE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, -1, 1, 1, 1, 1),model="WWW",
                           method="TRUE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, -1, 1, 1, 1),model="WWW",
                           method="TRUE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, -1, 1, 1),model="WWW",
                           method="TRUE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, 1, -1, 1),model="WWW",
                           method="TRUE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, 1, 1, -1),model="WWW",
                           method="TRUE"),
               "beta3 value must be greater than 0")



  expect_error(r.tc_graph( true_param = c(-1, 1, 1, 1, 1, 1),model="GGG",
                           method="TRUE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, -1, 1, 1, 1, 1),model="GGG",
                           method="TRUE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, -1, 1, 1, 1),model="GGG",
                           method="TRUE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, -1, 1, 1),model="GGG",
                           method="TRUE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, 1, -1, 1),model="GGG",
                           method="TRUE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, 1, 1, -1),model="GGG",
                           method="TRUE"),
               "beta3 value must be greater than 0")


  expect_error(r.tc_graph( true_param = c(-1, 1, 1, 1, 1, 1),model="LLL",
                           method="TRUE"),
               "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, -1, 1, 1, 1, 1),model="LLL",
                           method="TRUE"),
               "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, -1, 1, 1, 1),model="LLL",
                           method="TRUE"),
               "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, -1, 1, 1),model="LLL",
                           method="TRUE"),
               "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, 1, -1, 1),model="LLL",
                           method="TRUE"),
               "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param = c(1, 1, 1, 1, 1, -1),model="LLL",
                           method="TRUE"),
               "beta3 value must be greater than 0")






  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(3.485613, 4.939489, 6.072339, 3.995163,
                            2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                        model="GWL",method="AD"),
        "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GWL",
                          method="AD"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GWL",
                           method="AD"),
          "Optimization did not converge.Please check your initial parameters.")






  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(3.485613, 4.939489, 6.072339, 3.995163,
                            2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                        model="GWL",method="CvM"),
        "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GWL",
                          method="CvM"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GWL",
                           method="CvM"),
          "Optimization did not converge.Please check your initial parameters.")






  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(3.485613, 4.939489, 6.072339, 3.995163,
                            2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                        model="GWL",method="LSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GWL",
                          method="LSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GWL",
                           method="LSE"),
          "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(0, 0, 0, 0.7594201,
                            0.3714640),
                        z=c(10000,100000, 6.072339, 3.995163,
                            2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                        model="GWL",method="WLSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(0, 0, 0, 0.7594201,
                              0.3714640),
                          z=c(10000,100000, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GWL",
                          method="WLSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),
                           model="GWL",method="WLSE"),
          "Optimization did not converge.Please check your initial parameters.")












  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),model="GGW",
                        method="MLE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612,
                              1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578,
                              0.7594201, 0.3714640),
                          z=c(3.485613, 0, 0, 3.995163,
                              2.893617),
                          init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414,
                                                  1.0131692, 0.8667612,
                                                  1.0066519, 1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GGW",
                          method="MLE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGW",
                           method="MLE"),
          "Optimization did not converge.Please check your initial parameters.")



  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),
                        model="GGW",method="AD"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GGW",
                          method="AD"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGW",
                           method="AD"),
          "Optimization did not converge.Please check your initial parameters.")





  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),model="GGW",
                        method="CvM"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GGW",
                          method="CvM"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGW",
                           method="CvM"),
          "Optimization did not converge.Please check your initial parameters.")







  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),
                        model="GGW",method="LSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GGW",
                          method="LSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGW",
                           method="LSE"),
          "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),model="GGW",
                        method="WLSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="GGW",
                          method="WLSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGW",
                           method="WLSE"),
          "Optimization did not converge.Please check your initial parameters.")













  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),model="WGW",
                method="MLE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 0, 0, 3.995163, 2.893617),
                          init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WGW",
                          method="MLE"),
          "Optimization did not converge.Please check your initial parameters.")




  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),model="WGW",
                        method="AD"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                6.0647578, 0.7594201,
                                                0.3714640))),model="WGW",
                          method="AD"),
          "Optimization did not converge.Please check your initial parameters.")





  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),model="WGW",
                        method="CvM"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WGW",
                          method="CvM"),
          "Optimization did not converge.Please check your initial parameters.")








  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),model="WGW",
                        method="LSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WGW",
                        method="LSE"),
          "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),model="WGW",
                        method="WLSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WGW",
                          method="WLSE"),
          "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),model="WWW",
                      method="MLE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 0, 0, 3.995163, 2.893617),
                          init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WWW",
                          method="MLE"),
          "Optimization did not converge.Please check your initial parameters.")




  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),model="WWW",
                        method="AD"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WWW",
                          method="AD"),
          "Optimization did not converge.Please check your initial parameters.")





  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),model="WWW",
                        method="CvM"),
        "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WWW",
                          method="CvM"),
          "Optimization did not converge.Please check your initial parameters.")








  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),
                        model="WWW",method="LSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WWW",
                          method="LSE"),
          "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),model="WWW",
                        method="WLSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163,
                              2.893617), init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                         median(c(1.5200108, 0.0617668,
                                                  6.0647578, 0.7594201,
                                                  0.3714640))),model="WWW",
                          method="WLSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),model="GGG",
                        method="MLE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 0, 0, 3.995163, 2.893617),
                          init_param = c(1, 1, 1, 1, 1, 1),
                          init_index = c(median(c(0.7736414, 1.0131692,
                                                  0.8667612, 1.0066519,
                                                  1.0372385)),
                                        median(c(1.5200108, 0.0617668,
                                                 6.0647578, 0.7594201,
                                                 0.3714640))),model="GGG",
                          method="MLE"),
          "Optimization did not converge.Please check your initial parameters.")



  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                            1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                            0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),
                        init_param = c(1, 1, 1, 1, 1, 1),model="GGG",
                        method="AD"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519,
                              1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201,
                              0.3714640),
                          z=c(3.485613, 0, 0, 3.995163, 2.893617),
                          init_param = c(1, 1, 1, 1, 1, 1),
                        init_index = c(median(c(0.7736414, 1.0131692,
                                                0.8667612, 1.0066519,
                                                1.0372385)),
                                       median(c(1.5200108, 0.0617668,
                                                6.0647578, 0.7594201,
                                                0.3714640))),model="GGG",
                        method="AD"),
          "Optimization did not converge.Please check your initial parameters.")





  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000),
                        init_param = c(1, 1, 1, 1, 1, 1),
                        model="GGG",method="CvM"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),
                          init_index = c(median(c(10, 110,1110, 11110, 111110)),
                                         median(c(10000000, 0, 4, 4, 4))),
                          model="GGG",method="CvM"),
          "Optimization did not converge.Please check your initial parameters.")







  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000),
                        init_param = c(1, 1, 1, 1, 1, 1),model="GGG",
                        method="LSE"),
        "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),
                          init_index = c(median(c(10, 110,1110, 11110, 111110)),
                                         median(c(10000000, 0, 4, 4, 4))),
                          model="GGG",method="LSE"),
          "Optimization did not converge.Please check your initial parameters.")



  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000),
                        init_param = c(1, 1, 1, 1, 1, 1),
                        model="GGG",method="WLSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),
                          init_index = c(median(c(10, 110,1110, 11110, 111110)),
                                         median(c(10000000, 0, 4, 4, 4))),
                          model="GGG",method="WLSE"),
          "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000),
                        init_param = c(1, 1, 1, 1, 1, 1),
                        model="LLL",method="MLE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),
                          init_index = c(median(c(10, 110,1110, 11110, 111110)),
                                         median(c(10000000, 0, 4, 4, 4))),
                          model="LLL",method="MLE"),
          "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000),
                        init_param = c(1, 1, 1, 1, 1, 1),model="LLL",
                        method="AD"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),
                          init_index = c(median(c(10, 110,1110, 11110, 111110)),
                                         median(c(10000000, 0, 4, 4, 4))),
                          model="LLL",method="AD"),
          "Optimization did not converge.Please check your initial parameters.")




  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000),
                        init_param = c(1, 1, 1, 1, 1, 1),
                        model="LLL",method="CvM"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),
                          init_index = c(median(c(10, 110,1110, 11110, 111110)),
                                         median(c(10000000, 0, 4, 4, 4))),
                          model="LLL",method="CvM"),
          "Optimization did not converge.Please check your initial parameters.")





  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000),
                        init_param = c(1, 1, 1, 1, 1, 1),
                        model="LLL",method="LSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),
                          init_index = c(median(c(10, 110,1110, 11110, 111110)),
                                         median(c(10000000, 0, 4, 4, 4))),
                          model="LLL",method="LSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000),
                        init_param = c(1, 1, 1, 1, 1, 1),
                        model="LLL",method="WLSE"),
          "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),
                          init_index = c(median(c(10, 110,1110, 11110, 111110)),
                                         median(c(10000000, 0, 4, 4, 4))),
                          model="LLL",method="WLSE"),
          "Optimization did not converge.Please check your initial parameters.")


})


test_that("function return a double or list",
          {
            ###GWL
              alpha1 <- 2; beta1 <- 2  # Gamma
              alpha2 <- 3; beta2 <- 1.5 # Weibull
              alpha3 <- 1; beta3 <- 1   # Logistic
              x<-rG(100,alpha1,beta1)
              y<-rW(100,alpha2,beta2)
              z<-rL(100,alpha3,beta3)
  expect_type(r.tc_vus(x=x,y=y,z=z,
              init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                              alpha3=alpha3,beta3=beta3),
                                 model=c("GWL"), method=c("MLE")), "double")


  expect_type(r.tc_vus(x=x,y=y,z=z,
              init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("GWL"), method=c("CvM")), "double")

  expect_type(r.tc_vus(x=x,y=y,z=z,
              init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("GWL"), method=c("LSE")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
              init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("GWL"), method=c("WLSE")), "double")



  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GWL"),
                         method=c("MLE")), "double")

  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GWL"),
                         method=c("CvM")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GWL"),
                         method=c("LSE")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GWL"),
                         method=c("WLSE")), "double")










  ###GGW
  alpha1 <- 2; beta1 <- 2  # Gamma
  alpha2 <- 3; beta2 <- 1.5 # Gamma
  alpha3 <- 3; beta3 <- 1   # Weibull
  x<- rG(100,  alpha1,  beta1)
  y <- rG(100,  alpha2, beta2)
  z <- rW(100,  alpha3,  beta3)


  expect_type(r.tc_vus(x=x,y=y,z=z,
              init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("GGW"), method=c("MLE")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
              init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("GGW"), method=c("AD")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
              init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("GGW"), method=c("CvM")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
              init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                  model=c("GGW"), method=c("LSE")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
              init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("GGW"), method=c("WLSE")), "double")


  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GGW"),
                         method=c("MLE")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GGW"),
                         method=c("AD")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GGW"),
                         method=c("CvM")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GGW"),
                         method=c("LSE")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GGW"),
                         method=c("WLSE")), "double")


  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GGW"),
                         method=c("MLE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GGW"),
                         method=c("AD")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GGW"),
                         method=c("CvM")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GGW"),
                         method=c("LSE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GGW"),
                         method=c("WLSE")), "list")





  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GGW"),
                         method=c("MLE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GGW"),
                         method=c("AD")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GGW"),
                         method=c("CvM")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GGW"),
                         method=c("LSE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GGW"),
                         method=c("WLSE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GGW"),
                         method=c("TRUE")), "list")

###WGW
  alpha1 <- 2; beta1 <- 1   # Weibull
  alpha2 <- 2; beta2 <- 2  # Gamma
  alpha3 <- 6; beta3 <- 9# Weibull
  x<- rW(100,  alpha1,  beta1)
  y <- rG(100,  alpha2, beta2)
  z <- rW(100,  alpha3,  beta3)


  expect_type(r.tc_vus(x=x,y=y,z=z,
               init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("WGW"), method=c("MLE")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
               init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("WGW"), method=c("AD")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
            init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("WGW"), method=c("CvM")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
             init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("WGW"), method=c("LSE")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
            init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("WGW"), method=c("WLSE")), "double")


  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("WGW"),
                         method=c("MLE")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("WGW"),
                         method=c("AD")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("WGW"),
                         method=c("CvM")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("WGW"),
                         method=c("LSE")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("WGW"),
                         method=c("WLSE")), "double")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("WGW"),
                         method=c("MLE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("WGW"),
                         method=c("AD")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("WGW"),
                         method=c("CvM")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("WGW"),
                         method=c("LSE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("WGW"),
                         method=c("WLSE")), "list")







  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("WGW"),
                         method=c("MLE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("WGW"),
                         method=c("AD")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("WGW"),
                         method=c("CvM")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("WGW"),
                         method=c("LSE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("WGW"),
                         method=c("WLSE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("WGW"),
                         method=c("TRUE")), "list")
###WWW
  alpha1 <- 1.5; beta1 <- 1   # Weibull
  alpha2 <- 2; beta2 <- 1.5 # Weibull
  alpha3 <- 3; beta3 <- 2   # Weibull
  x<- rW(100,  alpha1,  beta1)
  y <- rW(100,  alpha2, beta2)
  z <- rW(100,  alpha3,  beta3)


  expect_type(r.tc_vus(x=x,y=y,z=z,
               init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("WWW"), method=c("MLE")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("WWW"), method=c("AD")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
          init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("WWW"), method=c("CvM")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
               init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("WWW"), method=c("LSE")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
             init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("WWW"), method=c("WLSE")), "double")


  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("WWW"),
                         method=c("MLE")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("WWW"),
                         method=c("AD")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("WWW"),
                         method=c("CvM")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("WWW"),
                         method=c("LSE")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("WWW"),
                         method=c("WLSE")), "double")


  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("WWW"),
                         method=c("MLE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("WWW"),
                         method=c("AD")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("WWW"),
                         method=c("CvM")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("WWW"),
                         method=c("LSE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("WWW"),
                         method=c("WLSE")), "list")
  expect_type(r.tc_graph(x,y,z,
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("WWW"),
                         method=c("TRUE")), "list")


  expect_type(r.tc_graph(x,y,z,

                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("WWW"),
                         method=c("MLE")), "list")

  expect_type(r.tc_graph(x,y,z,

                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("WWW"),
                         method=c("AD")), "list")

  expect_type(r.tc_graph(x,y,z,

                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("WWW"),
                         method=c("CvM")), "list")

  expect_type(r.tc_graph(x,y,z,

                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("WWW"),
                         method=c("LSE")), "list")

  expect_type(r.tc_graph(x,y,z,

                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("WWW"),
                         method=c("WLSE")), "list")
  expect_type(r.tc_graph(x,y,z,
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("WWW"),
                         method=c("TRUE")), "list")



###GGG
  alpha1 <- 2; beta1 <- 2  # Gamma
  alpha2 <- 3; beta2 <- 2 # Gamma
  alpha3 <- 4; beta3 <- 3   # Gamma
  x<- rG(100,  alpha1,  beta1)
  y <- rG(100,  alpha2, beta2)
  z <- rG(100,  alpha3,  beta3)


  expect_type(r.tc_vus(x=x,y=y,z=z,
             init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("GGG"), method=c("MLE")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
               init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("GGG"), method=c("AD")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
           init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("GGG"), method=c("CvM")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
          init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("GGG"), method=c("LSE")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
           init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("GGG"), method=c("WLSE")), "double")

  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GGG"),
                         method=c("MLE")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GGG"),
                         method=c("AD")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GGG"),
                         method=c("CvM")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GGG"),
                         method=c("LSE")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GGG"),
                         method=c("WLSE")), "double")


  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GGG"),
                         method=c("MLE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GGG"),
                         method=c("AD")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GGG"),
                         method=c("CvM")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GGG"),
                         method=c("LSE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GGG"),
                         method=c("WLSE")), "list")

  expect_type(r.tc_graph(x,y,z,

                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GGG"),
                         method=c("TRUE")), "list")






  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GGG"),
                         method=c("MLE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GGG"),
                         method=c("AD")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GGG"),
                         method=c("CvM")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GGG"),
                         method=c("LSE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GGG"),
                         method=c("WLSE")), "list")

  expect_type(r.tc_graph(x,y,z,

                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GGG"),
                         method=c("TRUE")), "list")
###LLL
  alpha1 <- 0.5; beta1 <- 1.2   # Logistic
  alpha2 <- 1.5; beta2 <- 0.8   # Logistic
  alpha3 <- 2.5; beta3 <- 1.5   # Logistic
  x<- rL(100,  alpha1,  beta1)
  y <- rL(100,  alpha2, beta2)
  z <- rL(100,  alpha3,  beta3)



  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("LLL"),
                         method=c("MLE")), "double")


  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("LLL"),
                         method=c("MLE")), "list")
  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("LLL"),
                         method=c("MLE")), "list")


  x<-c(-0.6155969 , 0.9064283 , 1.7039646 ,-1.4412522
      ,2.7440984 , 1.5661629  ,1.8858454, -1.6091876, -0.2005690
      ,  1.3679556)

  y<-c(1.57629009  ,1.00407155,  0.58018227 ,-0.72724581
      ,0.56295849  ,3.17322975,  2.66954259  ,2.36832763
      ,  0.14273148 -0.05861623)

  z<-c(7.97855865, 2.11467493, 2.28372587, 0.07101358 ,3.01460801
      ,1.01398177, 0.68835397, 3.70865814, 1.10446529
      , 4.67889817)
  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("LLL"),
                         method=c("WLSE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("LLL"),
                         method=c("WLSE")), "list")

  x<-c(-0.6155969 , 0.9064283 , 1.7039646 ,-1.4412522
      ,2.7440984 , 1.5661629  ,1.8858454, -1.6091876, -0.2005690
      ,  1.3679556)

  y<-c(1.57629009  ,1.00407155,  0.58018227 ,-0.72724581
      ,0.56295849  ,3.17322975,  2.66954259  ,2.36832763
      ,  0.14273148 -0.05861623)

  z<-c(7.97855865, 2.11467493, 2.28372587, 0.07101358 ,3.01460801
      ,1.01398177, 0.68835397, 3.70865814, 1.10446529
      , 4.67889817)

  expect_type(r.tc_vus(x=x,y=y,z=z,
            init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("LLL"), method=c("MLE")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
            init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("LLL"), method=c("AD")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
           init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("LLL"), method=c("CvM")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
          init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("LLL"), method=c("LSE")), "double")
  expect_type(r.tc_vus(x=x,y=y,z=z,
           init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("LLL"), method=c("WLSE")), "double")



  x<-c(-0.6155969 , 0.9064283 , 1.7039646 ,-1.4412522
      ,2.7440984 , 1.5661629  ,1.8858454, -1.6091876, -0.2005690
      ,  1.3679556)

  y<-c(1.57629009  ,1.00407155,  0.58018227 ,-0.72724581
      ,0.56295849  ,3.17322975,  2.66954259  ,2.36832763
      ,  0.14273148 -0.05861623)

  z<-c(7.97855865, 2.11467493, 2.28372587, 0.07101358 ,3.01460801
      ,1.01398177, 0.68835397, 3.70865814, 1.10446529
      , 4.67889817)

  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
  beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("LLL"),
                         method=c("AD")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("LLL"),
                         method=c("CvM")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("LLL"),
                         method=c("LSE")), "double")
  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("LLL"),
                         method=c("WLSE")), "double")


  x<-c(-0.6155969 , 0.9064283 , 1.7039646 ,-1.4412522
      ,2.7440984 , 1.5661629  ,1.8858454, -1.6091876, -0.2005690
      ,  1.3679556)

  y<-c(1.57629009  ,1.00407155,  0.58018227 ,-0.72724581
      ,0.56295849  ,3.17322975,  2.66954259  ,2.36832763
      ,  0.14273148 -0.05861623)

  z<-c(7.97855865, 2.11467493, 2.28372587, 0.07101358 ,3.01460801
      ,1.01398177, 0.68835397, 3.70865814, 1.10446529
      , 4.67889817)

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("LLL"),
                         method=c("AD")), "list")
  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("LLL"),
                         method=c("CvM")), "list")
  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("LLL"),
                         method=c("LSE")), "list")


  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("LLL"),
                         method=c("AD")), "list")
  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("LLL"),
                         method=c("CvM")), "list")
  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("LLL"),
                         method=c("LSE")), "list")
  expect_type(r.tc_graph(x,y,z,
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("LLL"),
                         method=c("TRUE")), "list")

  x<-c(2.2302339,  8.2796185 , 0.3609374 , 4.1563083
      ,10.8396976  ,5.2940392 , 0.8767943 , 0.3260003,  8.4344549
      , 4.9326166)
  y<-c(1.5001992, 1.4021222, 1.6949652, 1.6089466, 2.1491223
      ,2.4842962, 0.6704828 ,0.9369306 ,1.4358653, 1.2232956)

  z<-c(7.97855865, 2.11467493, 2.28372587, 0.07101358 ,3.01460801
      ,1.01398177, 0.68835397, 3.70865814, 1.10446529
      , 4.67889817)
  expect_type(r.tc_vus(x=x,y=y,z=z,
                       init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                    beta2=beta2,
                                    alpha3=alpha3,beta3=beta3),
                       model=c("GWL"), method=c("AD")), "double")

  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GWL"),
                         method=c("AD")), "double")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GWL"),
                         method=c("AD")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GWL"),
                         method=c("CvM")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GWL"),
                         method=c("LSE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GWL"),
                         method=c("WLSE")), "list")



  expect_type(r.tc_index(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         init_index=c(median(x),median(y)),
                         model=c("GWL"),
                         method=c("AD")), "double")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GWL"),
                         method=c("AD")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GWL"),
                         method=c("CvM")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GWL"),
                         method=c("LSE")), "list")

  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GWL"),
                         method=c("WLSE")), "list")
  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=FALSE,model=c("GWL"),
                         method=c("MLE")), "list")
  expect_type(r.tc_graph(x,y,z,
                         init_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GWL"),
                         method=c("MLE")), "list")
  expect_type(r.tc_graph(x,y,z,
                         true_param=c(alpha1=alpha1,beta1=beta1,alpha2=alpha2,
                                      beta2=beta2,alpha3=alpha3,beta3=beta3),
                         empirical=TRUE,model=c("GWL"),
                         method=c("TRUE")), "list")
 }
)

