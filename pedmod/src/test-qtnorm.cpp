#include <testthat.h>
#include "qtnorm.h"
#include <iterator>

namespace {
inline bool is_minus_inf(double const x){
  return std::isinf(x) && x < 0;
}
inline bool is_pos_inf(double const x){
  return std::isinf(x) && x > 0;
}
}

context("qtnorm unit tests") {
  test_that("v gives similar output as TruncatedNormal::Phinv") {
    {
      /*
       u <- 45
       l <- 35
       ps <- c(.0001, .001, .25, .5, .75, .999, .9999)
       TruncatedNormal::Phinv(ps, rep(l, length(ps)), rep(u, length(ps))) |>
       dput()
       */
      constexpr double l{35}, u{45},
                    ps[]{.0001, .001, .25, .5, .75, .999, .9999},
                 truth[]{35.0000028549588, 35.0000285624338, 35.0082118325845, 35.0197824963072, 35.0395538420206, 35.1966521541889, 35.2619595518704};
      size_t const n_test = std::distance(std::begin(ps), std::end(ps));
      for(size_t i = 0; i < n_test; ++i)
        expect_true(std::abs(qtnorm(ps[i], l, u) - truth[i]) < truth[i] * 1e-8);

      expect_true(std::abs(qtnorm(-1, l, u) - l) < std::abs(l) * 1e-12);
      expect_true(std::abs(qtnorm(0, l, u) - l) < std::abs(l) * 1e-12);
      expect_true(std::abs(qtnorm(1, l, u) - u) < std::abs(u) * 1e-12);
      expect_true(std::abs(qtnorm(2, l, u) - u) < std::abs(u) * 1e-12);
    }

    {
      /*
       u <- -35
       l <- -45
       ps <- c(.0001, .001, .25, .5, .75, .999, .9999)
       TruncatedNormal::Phinv(ps, rep(l, length(ps)), rep(u, length(ps))) |>
       dput()
       */
      constexpr double l{-45}, u{-35},
                    ps[]{.0001, .001, .25, .5, .75, .999, .9999},
                 truth[]{-35.2619595518704, -35.1966521541889, -35.0395538420206, -35.0197824963072, -35.0082118325845, -35.0000285624338, -35.0000028549588};
      size_t const n_test = std::distance(std::begin(ps), std::end(ps));
      for(size_t i = 0; i < n_test; ++i)
        expect_true
          (std::abs(qtnorm(ps[i], l, u) - truth[i]) <
            std::abs(truth[i]) * 1e-8);

      expect_true(std::abs(qtnorm(-1, l, u) - l) < std::abs(l) * 1e-12);
      expect_true(std::abs(qtnorm(0, l, u) - l) < std::abs(l) * 1e-12);
      expect_true(std::abs(qtnorm(1, l, u) - u) < std::abs(u) * 1e-12);
      expect_true(std::abs(qtnorm(2, l, u) - u) < std::abs(u) * 1e-12);
    }

    {
      /*
       u <- -2
       l <- -Inf
       ps <- c(.0001, .001, .25, .5, .75, .999, .9999)
       TruncatedNormal::Phinv(ps, rep(l, length(ps)), rep(u, length(ps))) |>
       dput()
       */
      constexpr double l{-std::numeric_limits<double>::infinity()}, u{-2},
                    ps[]{.0001, .001, .25, .5, .75, .999, .9999},
                 truth[]{-4.58453281017075, -4.07761378755269, -2.53096049400871, -2.27760483880946, -2.11858921561289, -2.00042154689362, -2.00004213869856};
      size_t const n_test = std::distance(std::begin(ps), std::end(ps));
      for(size_t i = 0; i < n_test; ++i)
        expect_true
        (std::abs(qtnorm(ps[i], l, u) - truth[i]) <
          std::abs(truth[i]) * 1e-8);

      expect_true(std::isinf(qtnorm(-1, l, u)));
      expect_true(std::isinf(qtnorm(0, l, u)));
      expect_true(std::abs(qtnorm(1, l, u) - u) < std::abs(u) * 1e-12);
      expect_true(std::abs(qtnorm(1.5, l, u) - u) < std::abs(u) * 1e-12);
    }

    {
      /*
       u <- Inf
       l <- 3
       ps <- c(.0001, .001, .25, .5, .75, .999, .9999)
       TruncatedNormal::Phinv(ps, rep(l, length(ps)), rep(u, length(ps))) |>
       dput()
       */
      constexpr double l{3}, u{std::numeric_limits<double>::infinity()},
                    ps[]{.0001, .001, .25, .5, .75, .999, .9999},
                 truth[]{3.00003046042159, 3.00030472955114, 3.08656348283517, 3.20515492059893, 3.39955784447616, 4.69242948373014, 5.14328220229835};
      size_t const n_test = std::distance(std::begin(ps), std::end(ps));
      for(size_t i = 0; i < n_test; ++i)
        expect_true
        (std::abs(qtnorm(ps[i], l, u) - truth[i]) <
          std::abs(truth[i]) * 1e-8);

      expect_true(std::abs(qtnorm(-1, l, u) - l) < std::abs(l) * 1e-12);
      expect_true(std::abs(qtnorm(0, l, u) - l) < std::abs(l) * 1e-12);
      expect_true(std::isinf(qtnorm(1, l, u)));
      expect_true(std::isinf(qtnorm(1.5, l, u)));
    }
  }
}
