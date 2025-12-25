/*
 * scalar rank-1 lattice rules using fast algorithm
 */
# ifndef GENERATELATTICE_H_
# define GENERATELATTICE_H_
# include "mathfunc.h"
// [[Rcpp::plugins(cpp11)]]


/*
 * generate the lattice rule
 */
MatrixXd generateLattice(int n, // number of points in the lattice rule, scalar, must be prime
                         int p // dimension
                           )
{
  int s_max(p); // number of dimensions
  NumericVector gamma(s_max, 1.0 / s_max);
  NumericVector beta(s_max, 1.0);
  MatrixXd res(n, p);
  int m((n - 1) / 2);
  NumericVector E2(m, 0.0);
  NumericVector cumbeta = cumprod(beta);//cumprodC(beta);
  int g(0);
  try
  {
    g = generateOrp(n);
    if(g == -1){
      throw std::invalid_argument("n must be prime!");
    }
  }catch(std::exception &ex)
  {
    forward_exception_to_r(ex);
    return res;
  }
  NumericVector perm(m, 0.0);
  perm[0] = 1.0;
  for(int j = 1; j < perm.size(); j++)
  {
    perm[j] = ((int)perm[j - 1] * g) % n;
    perm[j] = minTwo(n - perm[j], perm[j]);
  }
  NumericVector temp = perm / n;
  NumericVector psi = calOmega(temp);
  double psi0 = calOmega(0.0);
  ComplexVector fft_psi = FFT(psi);
  NumericVector z(s_max, 0.0);
  NumericVector e2(s_max, 0.0);
  NumericVector q(m, 1.0);
  double q0 = 1;
  //
  Environment base("package:base");
  Function Re = base["Re"];
  int min_index(0);
  double min_E2(0.0);
  //double noise;
  NumericVector temp_psi(m, 1.0);
  int temp_index(0);
  for(int s = 1; s <= s_max; s++)
  {
    ComplexVector fft_q = FFT(q);
    fft_q = fft_q * fft_psi;
    fft_q = IFFT(fft_q);
    E2 = Re(_["z"] = fft_q);
    E2 = E2 / m;
    min_index = which_min(E2);
    min_E2 = E2[min_index];
    if(s==1)
    {
      min_index = 0;
      //noise = abs(E2[0] - min_E2);
    }
    z[s-1] = perm[min_index];
    e2[s-1] = -cumbeta[s-1] +(beta[s-1] * (q0 + 2 * sum(q)) + gamma[s-1]
                                * (psi0 * q0 + 2 * min_E2)) / n;
    temp_index = min_index;
    for(int i = 0; i <= min_index; i++)
    {
      temp_psi[i] = psi[min_index - i];
    }
    temp_index = m - 1;
    for(int i = min_index + 1; i < psi.size(); i++)
    {
      temp_psi[i] = psi[temp_index];
      temp_index--;
    }
    q = (beta[s] + gamma[s] * temp_psi) * q;
    q0 = (beta[s] + gamma[s] * psi0) * q0;
  }
  MatrixXd m_res(n, p);
  VectorXd l_bound(p);
  VectorXd u_bound(p);
  for(int i = 0; i < p; i++)
  {
    l_bound(i) = 1e60;
    u_bound(i) = -1e60;
    for(int j = 0; j < n; j++)
    {
      m_res(j, i) = ((int) z[i] * (j + 1)) % n;
      m_res(j, i) = m_res(j, i) * 1.0 / n;
      l_bound(i) = minTwo(l_bound(i), m_res(j, i));
      u_bound(i) = MaxTwo(u_bound(i), m_res(j, i));
    }
  }
  m_res = 0.5 / n + (1 - 1.0 / n) * (m_res - MatrixXd::Ones(n, 1) * l_bound.transpose()).array() /
    (MatrixXd::Ones(n, 1) * (u_bound - l_bound).transpose()).array();
  return m_res;
}
# endif
