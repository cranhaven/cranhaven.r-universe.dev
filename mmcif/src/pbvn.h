#ifndef PBVN_H
#define PBVN_H

#include <RcppArmadillo.h>
#include <array>
#include <R_ext/RS.h> // for F77_NAME and F77_CALL
#include <algorithm>
#include <cmath>
#include "dnorm.h"
#include "pnorm.h"
#include "qnorm.h"

extern "C" {
  double F77_NAME(mvbvu)(double const *dh, double const *dk, double const *r);
}

namespace ghqCpp {
/// computes the integral as mvbvu using Genz extension of Drezner's method
inline double pbvn_Drezner(double const h, double const k, double const rho){
  auto pnrm = [](double const x){
    return pnorm_std(x, 1, 0);
  };

  /* Gauss–Legendre quadrature nodes scale to the interval [0, 1]. I.e.
     gq <- SimSurvNMarker::get_gl_rule(12)
     ord <- order(gq$weight)
     dput((gq$node[ord] + 1) / 2)
     dput(gq$weight[ord] / 2)
   */

  constexpr double nodes6[]{0.966234757101576, 0.033765242898424, 0.830604693233132, 0.169395306766868, 0.619309593041598, 0.380690406958402},
                 weights6[]{0.0856622461895852, 0.0856622461895852, 0.180380786524069, 0.180380786524069, 0.233956967286346, 0.233956967286346},
                  nodes12[]{0.99078031712336, 0.00921968287664043, 0.952058628185237, 0.0479413718147626, 0.884951337097152, 0.115048662902848, 0.793658977143309, 0.206341022856691, 0.68391574949909, 0.31608425050091, 0.562616704255734, 0.437383295744266},
                weights12[]{0.0235876681932559, 0.0235876681932559, 0.0534696629976592, 0.0534696629976592, 0.0800391642716731, 0.0800391642716731, 0.101583713361533, 0.101583713361533, 0.116746268269177, 0.116746268269177, 0.124573522906701, 0.124573522906701},
                  nodes20[]{0.996564299592547, 0.00343570040745256, 0.981985963638957, 0.0180140363610431, 0.956117214125663, 0.0438827858743371, 0.919558485911109, 0.0804415140888906, 0.873165953230075, 0.126834046769925, 0.818026840363258, 0.181973159636742, 0.755433500975414, 0.244566499024587, 0.68685304435771, 0.31314695564229, 0.613892925570823, 0.386107074429178, 0.538263260566749, 0.461736739433251},
                weights20[]{0.00880700356957606, 0.00880700356957606, 0.0203007149001935, 0.0203007149001935, 0.0313360241670545, 0.0313360241670545, 0.0416383707883524, 0.0416383707883524, 0.0509650599086202, 0.0509650599086202, 0.0590972659807592, 0.0590972659807592, 0.0658443192245883, 0.0658443192245883, 0.071048054659191, 0.071048054659191, 0.0745864932363019, 0.0745864932363019, 0.0763766935653629, 0.0763766935653629};

  auto wo_border_correction = [&](double const *nodes, double const *weights,
                                  size_t const n_nodes){
    double const offset{h * h + k * k},
                  slope{2 * h * k},
                     ub{std::asin(rho)};

    double out{};
    for(size_t i = 0; i < n_nodes; ++i){
      double const n{ub * nodes[i]},
               sin_n{std::sin(n)};
      out += weights[i] * std::exp
        (-(offset - slope * sin_n) / (2 * (1 - sin_n * sin_n)));
    }
    out *= ub / (2 * M_PI);

    return out + pnrm(-h) * pnrm(-k);
  };

  if(std::abs(rho) <= .3)
    return wo_border_correction(nodes6, weights6, 6);
  else if(std::abs(rho) <= .75)
    return wo_border_correction(nodes12, weights12, 12);
  else if(std::abs(rho) <= 0.925)
    return wo_border_correction(nodes20, weights20, 20);

  // handle the large absolute correlation. My attempt to implement this was
  // not as numerically stable as Genz
  return F77_CALL(mvbvu)(&h, &k, &rho);
}

/**
 * computes the integral
 *
 *   int_(-inf)^0int_(-inf)^0phi(x; mu, Sigma) dx =
 *     int_(-inf)^(-mu_1)int_(-inf)^(-mu_2)phi(x; 0, Sigma) dx
 *
 * method = 1 yields the method by Drezner further extended by Genz in
 *
 *   https://doi.org/10.1023/B:STCO.0000035304.20635.31
 *
 * method = 0 yields a Gauss–Legendre quadrature based solution. This is less
 * precise and slower.
 */
template<int method = 1>
double pbvn(double const *mu, double const *Sigma){
  static_assert(method == 1 || method == 0, "method is not implemented");

  if constexpr (method == 0){
    if(Sigma[1] < 0){
      double const altered_Sigma[]{Sigma[0], -Sigma[1], -Sigma[2], Sigma[3]};
      double const std_mu[]
        {mu[0] / std::sqrt(Sigma[0]), mu[1] / std::sqrt(Sigma[3])};
      if(std_mu[0] > std_mu[1]){
        double const pnrm_area{pnorm_std(std_mu[0], 1, 0)},
                     mu_altered[]{mu[0], -mu[1]};

        return 1 - pnrm_area - pbvn<method>(mu_altered, altered_Sigma);
      }

      double const pnrm_area{pnorm_std(std_mu[1], 1, 0)},
                mu_altered[]{-mu[0], mu[1]};

      return 1 - pnrm_area - pbvn<method>(mu_altered, altered_Sigma);
    }

    // setup before applying the quadrature rule
    // the, will be, scaled Cholesky decomposition of the covariance matrix
    std::array<double, 3> Sig_chol;

    double const sig1{std::sqrt(Sigma[0])},
                 sig2{std::sqrt(Sigma[3])};
    bool const permuted{-mu[1] / sig2  < -mu[0] / sig1 };
    if(permuted){
      Sig_chol[0] = sig2;
      Sig_chol[1] = Sigma[2] / sig2;
      Sig_chol[2] = std::sqrt(Sigma[0] - Sig_chol[1] * Sig_chol[1]);

    } else {
      Sig_chol[0] = sig1;
      Sig_chol[1] = Sigma[2] / sig1;
      Sig_chol[2] = std::sqrt(Sigma[3] - Sig_chol[1] * Sig_chol[1]);

    }
    if(!std::isfinite(Sig_chol[0]) || !std::isfinite(Sig_chol[2]))
      throw std::invalid_argument("Choleksy decomposition failed");

    // the scaled upper limits to add
    std::array<double, 2> const ubs
      { (permuted ? -mu[1] : -mu[0]) / Sig_chol[0],
        (permuted ? -mu[0] : -mu[1]) / Sig_chol[2] };
    Sig_chol[1] /= Sig_chol[2];

    /* Gauss–Legendre quadrature nodes scale to the interval [0, 1]. I.e.
     n_nodes <- 50L
     stopifnot(n_nodes %% 2L == 0L)
     gq <- SimSurvNMarker::get_gl_rule(n_nodes)
     ord <- order(gq$node)[seq_len(n_nodes %/% 2L)]
     gq <- with(gq, list(node = node[ord], weight = weight[ord]))
     dput((gq$node + 1) / 2)
     log(gq$weight / 2) |> dput()
     */
    constexpr size_t n_nodes{50};
    constexpr double nodes[]{0.00056679778996449, 0.00298401528395464, 0.00732295797599708, 0.013567807446654, 0.021694522378596, 0.0316716905275611, 0.043460721672104, 0.0570160102381935, 0.072285115285027, 0.0892089645703321, 0.1077220835498, 0.127752848886966, 0.149223765646589, 0.17205176715728, 0.196148536407525, 0.221420847742675, 0.247770927546268, 0.275096832512981, 0.303292844051217, 0.332249877290281, 0.361855903110234, 0.391996381561979, 0.422554705000927, 0.453412649219957, 0.484450830836406},
               log_weights[]{-6.53322283985511, -5.6899092746998, -5.24094051811074, -4.93500689320544, -4.70413118267991, -4.51989917726375, -4.36770276999406, -4.23903535290967, -4.12850975550265, -4.03250179468945, -3.94845992500549, -3.87452340980434, -3.80929710971246, -3.75171167066661, -3.70093303857589, -3.65630185246153, -3.61729167704724, -3.5834795327207, -3.55452470170788, -3.53015326113214, -3.51014668424804, -3.49433340776322, -3.48258262178658, -3.47479977715334, -3.47092346849629};

    // do the computation
    double out{};
    double const p_outer{pnorm_std(ubs[0], 1, 0)};
    for(size_t i = 0; i < n_nodes / 2; ++i){
      auto add_term = [&](bool const flip){
        double const val{flip ? 1 - nodes[i] : nodes[i]},
                 z_outer{qnorm_w(val * p_outer, 0, 1, 1, 0)},
                 p_inner{pnorm_std(ubs[1] - Sig_chol[1] * z_outer, 1, 1)};

        out += std::exp(p_inner + log_weights[i]);
      };

      add_term(false);
      add_term(true);
    }

    return p_outer * out;
  }

  double const h{mu[0] / std::sqrt(Sigma[0])},
               k{mu[1] / std::sqrt(Sigma[3])},
             rho{Sigma[1] / std::sqrt(Sigma[0] * Sigma[3])};
  return pbvn_Drezner(h, k, rho);
}

/**
 * computes the derivative of the mean and covariance matrix in of pbvn. For the
 * mean, this is given by
 *
 *   Sigma^(-1)int_(-inf)^0int_(-inf)^0(x - mu)phi(x; mu, Sigma) dx =
 *     Sigma^(-1).int_(-inf)^(-mu_1)int_(-inf)^(-mu_2)x phi(x; 0, Sigma) dx
 *
 * For Sigma, we need to compute
 *
 *   2^(-1)Sigma^(-1)[int_(-inf)^0int_(-inf)^0
 *     ((x - mu).(x - mu)^T - Sigma)phi(x; mu, Sigma) dx]Sigma^(-1) =
 *   2^(-1)Sigma^(-1)[int_(-inf)^(-mu_1)int_(-inf)^(-mu_2)
 *     (x.x^T - Sigma)phi(x; mu, Sigma) dx]Sigma^(-1)
 *
 * the derivatives w.r.t. Sigma are stored as a 2 x 2 matrix ignoring the
 * symmetry. Thus, a 6D array needs to be passed for the gradient.
 *
 * An easier way to compute this is to use the method described in the appendix
 * of
 *
 *   https://doi.org/10.1198/jcgs.2009.07130
 */
template<int method = 1, bool comp_d_Sig = true>
double pbvn_grad(double const *mu, double const *Sigma, double *grad){
  static_assert(method == 1 || method == 0, "method is not implemented");

  if constexpr(method == 1){
    double const Sig_h{std::sqrt(Sigma[0])},
                 Sig_k{std::sqrt(Sigma[3])},
                 h{mu[0] / Sig_h},
                 k{mu[1] / Sig_k},
               rho{Sigma[1] / (Sig_h  * Sig_k)},
               out{pbvn_Drezner(h, k, rho)},
          cond_var{(1 - rho) * (1 + rho)},
           cond_sd{std::sqrt(cond_var)};

    double const dnrms[]{std::exp(dnrm_log(-h)), std::exp(dnrm_log(-k))},
                 pnrms[]
      {pnorm_std((-k + h * rho) / cond_sd, 1, 0),
       pnorm_std((-h + k * rho) / cond_sd, 1, 0)},
                   d_h{dnrms[0] * pnrms[0]},
                   d_k{dnrms[1] * pnrms[1]};

    grad[0] = -d_h / Sig_h;
    grad[1] = -d_k / Sig_k;

    if constexpr(comp_d_Sig){
      constexpr double two_pi{6.28318530717959};
      double const d_rho
        {std::exp(-(h * h - 2 * rho * h * k + k * k) / (2 * cond_var)) /
          (two_pi * cond_sd)};

      grad[2] = (d_h * h - d_rho * rho) / (2 * Sigma[0]);
      grad[3] = d_rho / (Sig_h  * Sig_k) / 2;
      grad[4] = grad[3];
      grad[5] = (d_k * k - d_rho * rho) / (2 * Sigma[3]);
    }

    return out;
  }

  if(Sigma[1] < 0 && mu[0] < 0 && mu[1] < 0){
    double inter_gr[comp_d_Sig ? 6 : 2];
    std::fill(grad, grad + (comp_d_Sig ? 6 : 2), 0);

    double const altered_Sigma[]{Sigma[0], -Sigma[1], -Sigma[2], Sigma[3]};

    double out{1};
    {
      double const mu_altered[]{-mu[0], -mu[1]};
      out -= pbvn_grad<method, comp_d_Sig>(mu_altered, Sigma, inter_gr);
      grad[0] += inter_gr[0];
      grad[1] += inter_gr[1];
      if constexpr(comp_d_Sig){
        grad[2] -= inter_gr[2];
        grad[3] -= inter_gr[3];
        grad[4] -= inter_gr[4];
        grad[5] -= inter_gr[5];
      }
    }
    {
      double const mu_altered[]{mu[0], -mu[1]};
      out -= pbvn_grad<method, comp_d_Sig>(mu_altered, altered_Sigma, inter_gr);
      grad[0] -= inter_gr[0];
      grad[1] += inter_gr[1];
      if constexpr(comp_d_Sig){
        grad[2] -= inter_gr[2];
        grad[3] += inter_gr[3];
        grad[4] += inter_gr[4];
        grad[5] -= inter_gr[5];
      }

    }
    {
      double const mu_altered[]{-mu[0], mu[1]};
      out -= pbvn_grad<method, comp_d_Sig>(mu_altered, altered_Sigma, inter_gr);
      grad[0] += inter_gr[0];
      grad[1] -= inter_gr[1];
      if constexpr(comp_d_Sig){
        grad[2] -= inter_gr[2];
        grad[3] += inter_gr[3];
        grad[4] += inter_gr[4];
        grad[5] -= inter_gr[5];
      }

    }

    return out;
  }

  std::array<double, 3> Sig_chol;

  double const sig1{std::sqrt(Sigma[0])},
               sig2{std::sqrt(Sigma[3])};
  bool const permuted{-mu[1] / sig2  < -mu[0] / sig1 };
  if(permuted){
    Sig_chol[0] = sig2;
    Sig_chol[1] = Sigma[2] / sig2;
    Sig_chol[2] = std::sqrt(Sigma[0] - Sig_chol[1] * Sig_chol[1]);

  } else {
    Sig_chol[0] = sig1;
    Sig_chol[1] = Sigma[2] / sig1;
    Sig_chol[2] = std::sqrt(Sigma[3] - Sig_chol[1] * Sig_chol[1]);

  }
  if(!std::isfinite(Sig_chol[0]) || !std::isfinite(Sig_chol[2]))
    throw std::invalid_argument("Choleksy decomposition failed");

  // the scaled upper limits to add
  std::array<double, 2> const ubs
  { (permuted ? -mu[1] : -mu[0]) / Sig_chol[0],
    (permuted ? -mu[0] : -mu[1]) / Sig_chol[2] };
  double const Sig_12_scaled{Sig_chol[1] / Sig_chol[2]};

  constexpr size_t n_nodes{50};
  constexpr double nodes[]{0.999433202210036, 0.00056679778996449, 0.997015984716045, 0.00298401528395464, 0.992677042024003, 0.00732295797599708, 0.986432192553346, 0.013567807446654, 0.978305477621404, 0.021694522378596, 0.968328309472439, 0.0316716905275611, 0.956539278327896, 0.043460721672104, 0.942983989761807, 0.0570160102381935, 0.927714884714973, 0.072285115285027, 0.910791035429668, 0.0892089645703321, 0.8922779164502, 0.1077220835498, 0.872247151113034, 0.127752848886966, 0.850776234353411, 0.149223765646589, 0.82794823284272, 0.17205176715728, 0.803851463592475, 0.196148536407525, 0.778579152257325, 0.221420847742675, 0.752229072453732, 0.247770927546268, 0.724903167487019, 0.275096832512981, 0.696707155948783, 0.303292844051217, 0.667750122709719, 0.332249877290281, 0.638144096889766, 0.361855903110234, 0.608003618438021, 0.391996381561979, 0.577445294999073, 0.422554705000927, 0.546587350780043, 0.453412649219957, 0.515549169163595, 0.484450830836406},
                 weights[]{0.00145431127657757, 0.00145431127657757, 0.0033798995978727, 0.0033798995978727, 0.00529527419182548, 0.00529527419182548, 0.00719041138074279, 0.00719041138074279, 0.0090577803567447, 0.0090577803567447, 0.0108901215850624, 0.0108901215850624, 0.0126803367850062, 0.0126803367850062, 0.0144214967902676, 0.0144214967902676, 0.016106864111789, 0.016106864111789, 0.0177299178075731, 0.0177299178075731, 0.0192843783062938, 0.0192843783062938, 0.0207642315450738, 0.0207642315450738, 0.0221637521694016, 0.0221637521694016, 0.0234775256519742, 0.0234775256519742, 0.0247004692247332, 0.0247004692247332, 0.0258278515347906, 0.0258278515347906, 0.0268553109444981, 0.0268553109444981, 0.0277788724031063, 0.0277788724031063, 0.0285949628238642, 0.0285949628238642, 0.0293004249066112, 0.0293004249066112, 0.0298925293521327, 0.0298925293521327, 0.0303689854208851, 0.0303689854208851, 0.0307279497951583, 0.0307279497951583, 0.0309680337103416, 0.0309680337103416, 0.0310883083276736, 0.0310883083276736};

  // do the computation
  double const out{pbvn<1>(mu, Sigma)};
  std::fill(grad, grad + (comp_d_Sig ? 6 : 2), 0);
  double * const d_mu{grad},
         * const d_Sig{comp_d_Sig ? grad + 2 : nullptr};
  double const p_outer{pnorm_std(ubs[0], 1, 0)};

  for(size_t i = 0; i < n_nodes; ++i){
    double const z_outer{qnorm_w(nodes[i] * p_outer, 0, 1, 1, 0)},
             u_lim_inner{ubs[1] - Sig_12_scaled * z_outer},
                 p_inner{pnorm_std(u_lim_inner, 1, 0)};

    double const g1_fac{z_outer * p_inner},
      dnorm_u_lim_inner{std::exp(dnrm_log(u_lim_inner))},
      trunc_mean_scaled{-dnorm_u_lim_inner};
    grad[0] += weights[i] * g1_fac;
    grad[1] += weights[i] * trunc_mean_scaled;

    if constexpr (comp_d_Sig){
      d_Sig[0] += weights[i] * g1_fac * z_outer;
      double const off_diag{z_outer * trunc_mean_scaled};
      d_Sig[1] += weights[i] * off_diag;
      double const trunc_sq_moment_scaled
        {p_inner - dnorm_u_lim_inner * u_lim_inner};
      d_Sig[3] += weights[i] * trunc_sq_moment_scaled;
    }
  }

  // handle the derivatives w.r.t. mu
  std::for_each(d_mu, d_mu + 2, [&](double &x){ x *= p_outer; });

  // performs backward substitution
  auto back_sub = [&](double *x){
    x[1] /= Sig_chol[2];
    x[0] = (x[0] - Sig_chol[1] * x[1]) / Sig_chol[0];
  };

  back_sub(d_mu);

  // possibly handle the derivatives w.r.t Sigma
  if constexpr (comp_d_Sig){
    d_Sig[2] = d_Sig[1]; // symmetry
    std::for_each(d_Sig, d_Sig + 4,
                  [&](double &x){ x *= p_outer / 2; });

    // subtract the identity matrix in the diagonal
    d_Sig[0] -= out / 2;
    d_Sig[3] -= out / 2;

    back_sub(d_Sig);
    back_sub(d_Sig + 2);
    std::swap(d_Sig[1], d_Sig[2]); // transpose
    back_sub(d_Sig);
    back_sub(d_Sig + 2);
  }

  if(permuted){
    std::swap(grad[0], grad[1]); // d_mu
    if constexpr (comp_d_Sig)
      std::swap(grad[2], grad[5]); // d_Sigma
  }

  return out;
}

/// computes the Hessian w.r.t. mu. Thus, a 4D array has to be passed
template<int method = 1>
void pbvn_hess(double const *mu, double const *Sigma, double *hess){
  double gr[6];
  pbvn_grad<method, true>(mu, Sigma, gr);

  arma::mat Sig(const_cast<double *>(Sigma), 2, 2, false, true);
  for(unsigned j = 0; j < 2; ++j)
    for(unsigned i = 0; i < 2; ++i)
      hess[i + j * 2] = 2 * gr[i + j * 2 + 2];
}

} // namespace ghqCpp

#endif
