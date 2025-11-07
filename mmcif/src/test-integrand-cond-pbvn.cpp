#include <testthat.h>
#include "integrand-cond-pbvn.h"
#include <iterator>

using namespace ghqCpp;

namespace {
/*
 K <- 3L
 set.seed(444)
 Sigma <- rWishart(1, K, diag(1/K, K)) |> drop() |> round(3)
 dput(Sigma)
 V <- runif(2 * K, -1) |> round(3) |> matrix(2)
 dput(V)
 eta <- runif(2, -1) |> round(3)
 dput(eta)
 Psi <- rWishart(1, 2, diag(1/2, 2)) |> drop() |> round(3)
 dput(Psi)

 get_n_remove <- \(x, n){
 out <- x[1:n]
 eval(substitute(out <- out[-(1:n)], list(out = substitute(x), n = n)),
 parent.frame())
 out
 }
 */

constexpr size_t K{3};
constexpr double v_Sigma[]{0.18, 0.037, -0.125, 0.037, 0.198, -0.258, -0.125, -0.258, 1.055},
                     v_V[]{-0.663, 0.786, 0.818, 0.798, 0.066, -0.465},
                   v_eta[]{-0.719, 0.205},
                   v_Psi[]{0.281, -0.398, -0.398, 0.623};

const arma::mat Sigma(v_Sigma, K, K),
                    V(v_V, 2, K),
                  Psi(v_Psi, 2, 2);
const arma::vec eta(v_eta, 2);
} // namespace

context("cond_pbvn works as expected") {
  test_that("log_integrand, log_integrand_grad, and log_integrand_x works") {
    /*
     set.seed(7)
     dput(u <- runif(K, -1) |> round(3))
     f <- \(u)
      log(mvtnorm::pmvnorm(upper = drop(-eta - V %*% crossprod(chol(Sigma), u)),
                           sigma = Psi))
     dput(f(u))
     dput(numDeriv::grad(f, u))
     dput(numDeriv::hessian(f, u))
     */
    constexpr double true_fn{-2.28632890224848},
                   true_gr[]{-1.14948621762232, -1.85860078867959, 0.930786018186607},
                 true_hess[]{-0.433265156276145, -0.637895329246285, 0.338972203522072, -0.637895329246285, -2.45341019244655, 0.785730587422607, 0.338972203522072, 0.785730587422607, -0.319469032176494},
                     point[]{0.978, -0.205, -0.769};

    simple_mem_stack<double> mem;
    cond_pbvn<false> pbvn_term(eta, Psi, V);
    rescale_problem<false> prob(Sigma, pbvn_term);
    {
      double const res{prob.log_integrand(point, mem)};
      expect_true(std::abs(res - true_fn) < std::abs(true_fn) * 1e-8);
    }
    {
      double gr[3];
      double const res{prob.log_integrand_grad(point, gr, mem)};
      expect_true(std::abs(res - true_fn) < std::abs(true_fn) * 1e-8);
      for(unsigned i = 0; i < 3; ++i)
        expect_true(std::abs(gr[i] - true_gr[i]) < std::abs(true_gr[i]) * 1e-5);
    }
    double hess[9];
    prob.log_integrand_hess(point, hess, mem);
    for(unsigned i = 0; i < 9; ++i)
      expect_true
        (std::abs(hess[i] - true_hess[i]) < std::abs(true_hess[i]) * 1e-5);
  }

  test_that("eval works and so does the gradient") {
    /*
# use that the integral is Phi^(2)(-eta; 0, Psi + V.Sigma.V^T)
     dput(fastGHQuad::gaussHermiteData(15))
     dput(mvtnorm::pmvnorm(upper = - eta, sigma = Psi + tcrossprod(V %*% Sigma, V)))

     f <- \(x){
     eta <- get_n_remove(x, length(eta))
     V <- get_n_remove(x, length(V)) |> matrix(2)
     Psi <- c(x[1], x[2], x[2], x[3]) |> matrix(2)
     mvtnorm::pmvnorm(upper = - eta, sigma = Psi + tcrossprod(V %*% Sigma, V))
     }
     gr <- numDeriv::grad(f, c(eta, V, Psi[upper.tri(Psi, TRUE)]))
     gr_Psi <- tail(gr, 3)
     dput(c(head(gr, -3), gr_Psi[1], gr_Psi[2] / 2, gr_Psi[2] / 2, gr_Psi[3]))
     */
    constexpr double true_fn{0.331361013628906},
                     true_gr[]{-0.212317753428759, -0.28707700393814, 0.051595068214951, 0.00473445245376747, 0.00571957683447128, 0.035886450615461, -0.0787970637568067, -0.0630716974273678, -0.126426195471468, 0.0588887189436173, 0.0588887189436173, 0.0353444619363999},
                 ghq_nodes[]{-6.16427243405245, -5.41363635528004, -4.78532036735222, -4.21860944438656, -3.69028287699836, -3.18829492442511, -2.70532023717303, -2.23642013026728, -1.77800112433715, -1.32728070207308, -0.881982756213822, -0.440147298645308, 3.05275368255626e-16, 0.440147298645309, 0.881982756213822, 1.32728070207308, 1.77800112433715, 2.23642013026728, 2.70532023717303, 3.1882949244251, 3.69028287699836, 4.21860944438656, 4.78532036735223, 5.41363635528004, 6.16427243405245},
               ghq_weights[]{2.71192351403844e-17, 1.25881498774651e-13, 6.71963841770629e-11, 1.01703825030185e-08, 6.25703249969108e-07, 1.89159729573406e-05, 0.000315083638745484, 0.00311570872012564, 0.0192430989654089, 0.0768889951758089, 0.203621136678124, 0.363088989275891, 0.439868722169486, 0.36308898927589, 0.203621136678124, 0.0768889951758089, 0.0192430989654089, 0.00311570872012562, 0.000315083638745483, 1.89159729573406e-05, 6.25703249969104e-07, 1.01703825030184e-08, 6.71963841770608e-11, 1.25881498774653e-13, 2.71192351403838e-17};

    simple_mem_stack<double> mem;
    ghq_data dat{ghq_nodes, ghq_weights, 25};

    {
      cond_pbvn<false> pbvn_term(eta, Psi, V);
      rescale_problem<false> prop(Sigma, pbvn_term);

      adaptive_problem prob(prop, mem);
      auto res = ghq(dat, prob, mem);
      expect_true(res.size() == 1);
      expect_true(std::abs(res[0] - true_fn) < std::abs(true_fn) * 1e-5);
    }

    // test the gradient
    cond_pbvn<true> pbvn_term(eta, Psi, V);
    rescale_problem<false> prop(Sigma, pbvn_term);
    adaptive_problem prob(prop, mem);
    auto res = ghq(dat, prob, mem);
    expect_true(res.size() == 7 + 2 * K);

    expect_true(std::abs(res[0] - true_fn) < std::abs(true_fn) * 1e-5);
    for(size_t i = 0; i < 6 + 2 * K; ++i)
      expect_true
        (std::abs(res[i + 1] - true_gr[i]) < std::abs(true_gr[i]) * 1e-3);
  }
}
