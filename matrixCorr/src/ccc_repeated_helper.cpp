// Thiago de Paula Oliveira
// helper: L, Dm, Z builders and AR(1) precision
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <unordered_map>
using namespace Rcpp;
using arma::mat;

// ---------- tiny utils ----------
static inline int find_col(const std::unordered_map<std::string,int>& pos,
                           const std::string& key) {
  auto it = pos.find(key);
  return (it == pos.end() ? -1 : it->second);
}
static inline int find_inter(const std::unordered_map<std::string,int>& pos,
                             const std::string& a, const std::string& b) {
  int id = find_col(pos, a + ":" + b);
  if (id >= 0) return id;
  return find_col(pos, b + ":" + a);
}

// [[Rcpp::export]]
Rcpp::List build_L_Dm_cpp(
    Rcpp::CharacterVector colnames_X,
    Rcpp::Nullable<Rcpp::CharacterVector> rmet_name,
    Rcpp::Nullable<Rcpp::CharacterVector> rtime_name,
    Rcpp::CharacterVector method_levels,
    Rcpp::CharacterVector time_levels,
    bool has_interaction,
    Rcpp::Nullable<Rcpp::NumericMatrix> Dmat_global
) {
  const bool have_met  = rmet_name.isNotNull()  && method_levels.size() >= 1;
  const bool have_time = rtime_name.isNotNull() && time_levels.size()  >= 1;

  const std::string rmet  = have_met  ? as<std::string>(as<CharacterVector>(rmet_name)[0])  : std::string();
  const std::string rtime = have_time ? as<std::string>(as<CharacterVector>(rtime_name)[0]) : std::string();

  const int nm = have_met  ? (int)method_levels.size() : 0;
  const int nt = have_time ? (int)time_levels.size()   : 0;
  const int nm_eff = (nm >= 2 ? nm : 0);
  const int nt_eff = (nt >= 2 ? nt : 0);

  const int p = colnames_X.size();
  if (p <= 0) stop("Empty design column names.");

  std::unordered_map<std::string,int> pos;
  pos.reserve((size_t)p);
  for (int j=0; j<p; ++j) pos.emplace(std::string(colnames_X[j]), j);

  if (nm_eff == 0) {
    return Rcpp::List::create(
      _["L"]  = R_NilValue,
      _["Dm"] = R_NilValue,
      _["nm"] = nm_eff,
      _["nt"] = nt_eff
    );
  }

  auto make_Dsub = [&](int nt_eff)->mat{
    if (nt_eff == 0) {
      mat D(1,1,arma::fill::ones);
      return D;
    }
    if (Dmat_global.isNotNull()) {
      NumericMatrix Dg(Dmat_global);
      if ((int)Dg.nrow() != nt_eff || (int)Dg.ncol() != nt_eff)
        stop("Dmat_global must be %d x %d for the provided time levels.", nt_eff, nt_eff);
      mat D(&Dg[0], Dg.nrow(), Dg.ncol(), false);
      // soft symmetrize for safety; R already normalizes/psd-guards
      D = 0.5 * (D + D.t());
      return D;
    } else {
      // fallback only used when R passed NULL (e.g., nt<2 or legacy path)
      return arma::eye<mat>(nt_eff, nt_eff);
    }
  };

  // ----- nm==2 fast path -----
  if (nm_eff == 2) {
    const std::string lev2 = as<std::string>(method_levels[1]);
    const std::string met2_name = rmet + lev2;
    const int met2_idx = find_col(pos, met2_name);
    if (met2_idx < 0) stop("Cannot find method column '%s' in design.", met2_name.c_str());

    mat L;
    if (nt_eff == 0) {
      L.zeros(p, 1);
      L(met2_idx, 0) = 1.0;
    } else {
      L.zeros(p, nt_eff);
      for (int t = 0; t < nt_eff; ++t) {
        L(met2_idx, t) = 1.0;
        if (has_interaction && t >= 1) {
          const std::string tlev = as<std::string>(time_levels[t]);
          const std::string tname = rtime + tlev;
          int inter_idx = find_inter(pos, met2_name, tname);
          if (inter_idx < 0)
            stop("Cannot find interaction column for '%s:%s' (or swapped) in design.",
                 met2_name.c_str(), tname.c_str());
          L(inter_idx, t) = 1.0;
        }
      }
    }

    mat Dsub = make_Dsub(nt_eff);
    return Rcpp::List::create(
      _["L"]  = Rcpp::wrap(L),
      _["Dm"] = Rcpp::wrap(Dsub),
      _["nm"] = nm_eff,
      _["nt"] = nt_eff
    );
  }

  // ----- nm>=3 -----
  const int nd = nm_eff * (nm_eff - 1) / 2;
  const int ntime_blocks = std::max(nt_eff, 1);
  const int q = nd * ntime_blocks;

  mat L(p, q, arma::fill::zeros);

  // method main-effect columns (treatment coding; level1 absent)
  std::vector<int> met_col(nm_eff, -1);
  for (int mlev = 1; mlev < nm_eff; ++mlev) {
    const std::string mname = rmet + std::string(method_levels[mlev]);
    met_col[mlev] = find_col(pos, mname);
    if (met_col[mlev] < 0) stop("Cannot find method column '%s' in design.", mname.c_str());
  }

  // interactions
  std::vector< std::vector<int> > inter_col(nm_eff, std::vector<int>(std::max(nt_eff,2), -1));
  if (has_interaction && nt_eff > 0) {
    for (int mlev = 1; mlev < nm_eff; ++mlev) {
      const std::string mname = rmet + std::string(method_levels[mlev]);
      for (int t = 1; t < nt_eff; ++t) {
        const std::string tname = rtime + std::string(time_levels[t]);
        int id = find_inter(pos, mname, tname);
        if (id < 0) stop("Cannot find interaction column for '%s:%s' (or swapped).",
            mname.c_str(), tname.c_str());
        inter_col[mlev][t] = id;
      }
    }
  }

  // fill L
  int col = 0;
  for (int tb = 0; tb < ntime_blocks; ++tb) {
    for (int i = 0; i < nm_eff - 1; ++i) {
      for (int j = i + 1; j < nm_eff; ++j, ++col) {
        if (i == 0 && j > 0) {
          L(met_col[j], col) += 1.0;
        } else if (i > 0 && j == 0) {
          L(met_col[i], col) -= 1.0;
        } else {
          L(met_col[j], col) += 1.0;
          L(met_col[i], col) -= 1.0;
        }
        if (has_interaction && nt_eff > 0 && tb >= 1) {
          if (i == 0 && j > 0) {
            L(inter_col[j][tb], col) += 1.0;
          } else if (i > 0 && j == 0) {
            L(inter_col[i][tb], col) -= 1.0;
          } else {
            L(inter_col[j][tb], col) += 1.0;
            L(inter_col[i][tb], col) -= 1.0;
          }
        }
      }
    }
  }

  mat Dsub = make_Dsub(nt_eff);
  mat Dm = (nd == 1 ? Dsub : arma::kron(Dsub, arma::eye<mat>(nd, nd)));

  return Rcpp::List::create(
    _["L"]  = Rcpp::wrap(L),
    _["Dm"] = Rcpp::wrap(Dm),
    _["nm"] = nm_eff,
    _["nt"] = nt_eff
  );
}

// drop all-zero columns
static inline NumericMatrix drop_zero_columns(const NumericMatrix& Zin) {
  const int n = Zin.nrow(), q = Zin.ncol();
  if (q == 0) return Zin;
  std::vector<int> keep; keep.reserve(q);
  for (int j = 0; j < q; ++j) {
    bool nonzero = false;
    for (int i = 0; i < n; ++i) { if (Zin(i,j) != 0.0) { nonzero = true; break; } }
    if (nonzero) keep.push_back(j);
  }
  if ((int)keep.size() == q) return Zin;
  if (keep.empty()) return NumericMatrix(n, 0);
  NumericMatrix Zout(n, (int)keep.size());
  for (int jj = 0; jj < (int)keep.size(); ++jj) {
    const int j = keep[jj];
    for (int i = 0; i < n; ++i) Zout(i, jj) = Zin(i, j);
  }
  return Zout;
}

// [[Rcpp::export]]
Rcpp::List build_L_Dm_Z_cpp(
    Rcpp::CharacterVector colnames_X,
    Rcpp::Nullable<Rcpp::CharacterVector> rmet_name,
    Rcpp::Nullable<Rcpp::CharacterVector> rtime_name,
    Rcpp::CharacterVector method_levels,
    Rcpp::CharacterVector time_levels,
    bool has_interaction,
    Rcpp::Nullable<Rcpp::NumericMatrix> Dmat_global,
    std::string slope_mode,
    Rcpp::Nullable<Rcpp::NumericVector> slope_var,
    Rcpp::Nullable<Rcpp::IntegerVector> method_codes,
    bool drop_zero_cols
) {
  Rcpp::List Laux = build_L_Dm_cpp(
    colnames_X, rmet_name, rtime_name,
    method_levels, time_levels,
    has_interaction, Dmat_global
  );

  if (slope_mode == "none") {
    return Rcpp::List::create(
      _["L"]  = Laux["L"],
                    _["Dm"] = Laux["Dm"],
                                  _["nm"] = Laux["nm"],
                                                _["nt"] = Laux["nt"],
                                                              _["Z"]  = R_NilValue
    );
  }

  int n = -1;
  if (slope_var.isNotNull()) n = as<NumericVector>(slope_var).size();
  if (method_codes.isNotNull()) {
    int nmeth = as<IntegerVector>(method_codes).size();
    if (n < 0) n = nmeth; else if (n != nmeth)
      stop("method_codes and slope_var must have the same length.");
  }
  if (n <= 0) stop("Cannot infer 'n' for Z; provide 'slope_var' (and 'method_codes' for slope_mode='method').");

  NumericMatrix Z;

  if (slope_mode == "subject") {
    if (!slope_var.isNotNull()) stop("slope_var is required for slope_mode='subject'.");
    NumericVector z = as<NumericVector>(slope_var);
    if ((int)z.size() != n) stop("slope_var length mismatch.");
    Z = NumericMatrix(n, 1);
    for (int i = 0; i < n; ++i) Z(i,0) = (NumericVector::is_na(z[i]) ? 0.0 : (double)z[i]);

  } else if (slope_mode == "method") {
    if (!slope_var.isNotNull())   stop("slope_var is required for slope_mode='method'.");
    if (!method_codes.isNotNull())stop("method_codes is required for slope_mode='method'.");

    NumericVector z = as<NumericVector>(slope_var);
    IntegerVector met = as<IntegerVector>(method_codes);
    if ((int)z.size() != n || (int)met.size() != n)
      stop("Length mismatch for slope_var / method_codes.");

    const int Lm = method_levels.size();
    if (Lm <= 0) stop("slope_mode='method' but no method_levels provided.");

    Z = NumericMatrix(n, Lm);
    for (int i = 0; i < n; ++i) {
      const double zi = NumericVector::is_na(z[i]) ? 0.0 : (double)z[i];
      const int mc = (met[i] == NA_INTEGER ? 0 : (int)met[i]); // 1..Lm
      if (mc >= 1 && mc <= Lm) Z(i, mc-1) = zi;
    }
    if (drop_zero_cols) Z = drop_zero_columns(Z);
  } else {
    stop("Unsupported slope_mode: '", slope_mode, "'");
  }

  return Rcpp::List::create(
    _["L"]  = Laux["L"],
                  _["Dm"] = Laux["Dm"],
                                _["nm"] = Laux["nm"],
                                              _["nt"] = Laux["nt"],
                                                            _["Z"]  = (Z.ncol() == 0 ? R_NilValue : Rcpp::wrap(Z))
  );
}
