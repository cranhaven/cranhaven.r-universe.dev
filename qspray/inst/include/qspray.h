#ifndef ___QSPRAY___
#define ___QSPRAY___

//#undef Rcpp_hpp
//#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <boost/multiprecision/gmp.hpp>
//#include <complex.h>
typedef std::vector<signed int>                             powers;
typedef boost::multiprecision::mpq_rational                 gmpq;
typedef boost::multiprecision::mpz_int                      gmpi;
typedef std::complex<gmpq>                                  qcplx;



// -------------------------------------------------------------------------- //
namespace QSPRAY {

  // -------------------------------------------------------------------------- //
  namespace utils {

    // -------------------------------------------------------------------------- //
    static std::string q2str(gmpq r) {
      const gmpi numer = boost::multiprecision::numerator(r);
      const gmpi denom = boost::multiprecision::denominator(r);
      mpz_t p;
      mpz_init(p);
      mpz_set(p, numer.backend().data());
      mpz_t q;
      mpz_init(q);
      mpz_set(q, denom.backend().data());
      const size_t n = mpz_sizeinbase(p, 10) + 2;
      const size_t d = mpz_sizeinbase(q, 10) + 2;
      char* cnumer = new char[n];
      char* cdenom = new char[d];
      cnumer = mpz_get_str(cnumer, 10, p);
      cdenom = mpz_get_str(cdenom, 10, q);
      std::string snumer = cnumer;
      std::string sdenom = cdenom;
      delete[] cnumer;
      delete[] cdenom;
      mpz_clear(p);
      mpz_clear(q);
      return snumer + "/" + sdenom;
    }

    // -------------------------------------------------------------------------- //
    static void simplifyPowers(powers& pows) {
      int n = pows.size();
      if(n == 0) {
        return;
      }
      n--;
      powers::iterator it = pows.end();
      bool zero = pows[n] == 0;
      while(zero && n > 0) {
        it--;
        n--;
        zero = pows[n] == 0;
      }
      if(zero) {
        pows = {};
      } else {
        pows.erase(it, pows.end());
      }
    }

    // -------------------------------------------------------------------------- //
    static powers growPowers(powers pows, signed int m, signed int n) {
      powers gpows;
      gpows.reserve(n);
      for(signed int i = 0; i < m; i++) {
        gpows.emplace_back(pows[i]);
      }
      for(signed int i = m; i < n; i++) {
        gpows.emplace_back(0);
      }
      return gpows;
    }

  } // end of namespace utils

  class PowersHasher {
   public:
    size_t operator()(const powers& exponents) const {
      // thanks to Steffan Hooper for advice
      std::size_t seed = 0;
      for(auto& i : exponents) {
        seed ^= i + 0x9e3779b9 + (seed << 6) + (seed >> 2);
      }
      return seed;
    }
  };

  template <typename CoeffT>
  using Polynomial = std::unordered_map<powers, CoeffT, PowersHasher>;

  // -------------------------------------------------------------------------- //
  template<typename T>
  class Qspray {

    Polynomial<T> S;

  public:
    // constructors ---------------------------------------
    Qspray()
      : S()
        {}

    Qspray(const Polynomial<T>& S_) 
      : S(S_) 
        {}

    Qspray(T x)
    {
      Polynomial<T> singleton;
      if(x != T(0)) {
        powers emptyVector(0);
        singleton[emptyVector] = x;        
      }
      S = singleton;
    }

    // Qspray(int k)
    // {
    //   Polynomial<T> singleton;
    //   if(k != 0) {
    //     powers emptyVector(0);
    //     singleton[emptyVector] = T(k);        
    //   }
    //   S = singleton;
    // }
    
    // methods --------------------------------------------
    Polynomial<T> get() {
      return S;
    } 

    bool empty() {
      return S.empty();
    }

    bool isNull() { // the same as empty() if the Qspray is clean
      typename Polynomial<T>::const_iterator it;
      T zero(0);
      bool result = true;
      for(it = S.begin(); it != S.end(); ++it) {
        if(it->second != zero) {
          result = false;
          break;
        }
      } 
      return result;
    }

    int numberOfVariables() {
      int d = 0;
      for(const auto& term : S) {
        int n = term.first.size();
        if(n > d) {
          d = n;
        }
      }
      return d;
    }

    bool isConstant() {
      int nterms = S.size();
      bool result = false;
      if(nterms == 0) {
        result = true;
      } else if(nterms == 1) {
        powers emptyVector(0);
        if(auto search = S.find(emptyVector); search != S.end()) {
          result = true;
        }
      }
      return result;
    }

    T constantTerm() {
      powers emptyVector(0);
      return S[emptyVector];
    }

    void clean() {
      T zero(0);
      typename Polynomial<T>::const_iterator it;
      Polynomial<T> SS;
      for(it = S.begin(); it != S.end(); it++) {
        powers pows = it->first;
        T coeff     = it->second;
        if(coeff != zero) {
          utils::simplifyPowers(pows);
          SS[pows] = coeff;
        }
      }
      S = SS;
    }

    bool operator==(const Qspray<T>& Q2) {
      Polynomial<T> SS(S);
      Polynomial<T> S2(Q2.S);
      if(S.size() != S2.size()) {
        return false;
      }
      typename Polynomial<T>::const_iterator it;
      powers pows;
      for(it = S.begin(); it != S.end(); ++it) {
        pows = it->first;
        if(SS[pows] != S2[pows]) {
          return false;
        } else {
          S2.erase(pows);
        }
      }
      // at this point, S2[k] == S[k] for every index 'k' of S;
      // it remains to check that every element of Q has been accounted for:
      if(S2.empty()) {
        return true;
      } else {
        return false;
      }
    }

    bool operator==(Qspray<T>& Q2) {
      const Qspray<T> Q = Qspray(S);
      return Q2 == Q;
    }

    bool operator!=(const Qspray<T>& Q2) {
      Qspray<T> Q = Qspray(S);
      return !(Q == Q2);
    }

    bool operator!=(Qspray<T>& Q2) {
      Qspray<T> Q = Qspray(S);
      return !(Q == Q2);
    }

    Qspray<T> operator-() {
      typename Polynomial<T>::const_iterator it;
      powers pows;  
      for(it = S.begin(); it != S.end(); ++it) {
        S[it->first] = -it->second;
      }
      return Qspray<T>(S);
    }

    Qspray<T> operator+=(const Qspray<T>& Q) {
      Polynomial<T> S2 = Q.S;
      typename Polynomial<T>::const_iterator it;
      powers pows;
      const T zero(0);
      for(it = S2.begin(); it != S2.end(); ++it) {
        pows = it->first;
        S[pows] += it->second;
        if(S[pows] == zero) {
          S.erase(pows);
        }
      }
      return Qspray<T>(S);
    }

    Qspray<T> operator+(const Qspray<T>& Q2) {
      Qspray<T> Q(S);
      Q += Q2;
      return Q;
    }

    // Qspray<T> operator+=(Qspray<T>& Q2) {
    //   const Qspray<T> Q3 = Q2;
    //   Qspray<T> Q(S);
    //   Q += Q3;
    //   S = Q.get();
    //   return Q;
    // }

    // Qspray<T> operator+(Qspray<T>& Q2) {
    //   const Qspray<T> Q3 = Q2;
    //   Qspray<T> Q(S);
    //   return Q + Q3;
    // }

    Qspray<T> operator-=(const Qspray<T>& Q2) {
      Polynomial<T> S2 = Q2.S;
      typename Polynomial<T>::const_iterator it;
      powers pows;
      const T zero(0);
      for(it = S2.begin(); it != S2.end(); ++it) {
        pows = it->first;
        S[pows] -= it->second;
        if(S[pows] == zero) {
          S.erase(pows);
        }
      }
      return Qspray<T>(S);
    }

    Qspray<T> operator-(const Qspray<T>& Q2) {
      Qspray<T> Q(S);
      Q -= Q2;
      return Q;
    }

    // Qspray<T> operator-=(Qspray<T>& Q2) {
    //   const Qspray<T> Q3 = Q2;
    //   Qspray<T> Q(S);
    //   Q -= Q3;
    //   S = Q.get();
    //   return Q;
    // }

    // Qspray<T> operator-(Qspray<T>& Q2) {
    //   const Qspray<T> Q3 = Q2;
    //   Qspray<T> Q(S);
    //   return Q - Q3;
    // }

    Qspray<T> operator*=(const Qspray<T>& Q) {
      Polynomial<T> S2 = Q.S;
      Polynomial<T> Sout;
      typename Polynomial<T>::const_iterator it1, it2;
      typename Polynomial<T>::iterator it;
      const T zero(0);
      powers powssum;
      signed int i;
      for(it1 = S.begin(); it1 != S.end(); ++it1) {
        T r1 = it1->second;
        if(r1 != zero) {
          powers pows1 = it1->first;
          signed int n1 = pows1.size();
          for(it2 = S2.begin(); it2 != S2.end(); ++it2) {
            T r2 = it2->second;
            if(r2 != zero) {
              powers pows2 = it2->first;
              signed int n2 = pows2.size();
              powssum.clear();
              if(n1 < n2) {
                powssum.reserve(n2);
                for(i = 0; i < n1; i++) {
                  powssum.emplace_back(pows1[i] + pows2[i]);
                }
                for(i = n1; i < n2; i++) {
                  powssum.emplace_back(pows2[i]);
                }
              } else if(n1 > n2) {
                powssum.reserve(n1);
                for(i = 0; i < n2; i++) {
                  powssum.emplace_back(pows1[i] + pows2[i]);
                }
                for(i = n2; i < n1; i++) {
                  powssum.emplace_back(pows1[i]);
                }
              } else {
                powssum.reserve(n1);
                for(i = 0; i < n1; i++) {
                  powssum.emplace_back(pows1[i] + pows2[i]);
                }
              }
              Sout[powssum] += r1 * r2;
            }
          }
        }
      }
      // remove the possibly zero terms
      Polynomial<T> SSout;
      for(it = Sout.begin(); it != Sout.end(); ++it) {
        if(it->second != zero) { 
          SSout[it->first] = it->second;
        }
      }
      //
      S = SSout;
      return Qspray<T>(SSout);
    }

    Qspray<T> operator*(const Qspray<T>& Q2) {
      Qspray<T> Q(S);
      Q *= Q2;
      return Q;
    }

    // Qspray<T> operator*=(Qspray<T>& Q2) {
    //   const Qspray<T> Q3 = Q2;
    //   Qspray<T> Q(S);
    //   Q *= Q3;
    //   S = Q.get();
    //   return Q;
    // }

    // Qspray<T> operator*(Qspray<T>& Q2) {
    //   const Qspray<T> Q3 = Q2;
    //   Qspray<T> Q(S);
    //   return Q * Q3;
    // }

    Qspray<T> power(unsigned int n) {
      Polynomial<T> u;
      powers emptyVector(0);
      u[emptyVector] = T(1);
      Qspray<T> Result(u);
      Qspray<T> Q(S);
      unsigned int n0 = n, b = 1, p = 0;
      while(n) {
        if(n & 1) {
          Result *= Q;
          p += b;
          if(p == n0) {
            break;
          }
        }
        n >>= 1;
        Q *= Q;
        b *= 2;
      }
      // S = Result.get();
      return Result;
    }

    void scale(T lambda) {
      typename Polynomial<T>::const_iterator it;
      if(lambda == T(0)) {
        for(it = S.begin(); it != S.end(); ++it) {
          S.erase(it->first);
        }
      } else {
        for(it = S.begin(); it != S.end(); ++it) {
          S[it->first] *= lambda;
        }        
      }
    }
    
    Qspray<T> deriv(std::vector<unsigned int> n) {
      Polynomial<T> Sprime;
      typename Polynomial<T>::const_iterator it;
      powers v;
      signed int j, J, nj;
      signed int N = n.size();
      T zero(0);
      for(it = S.begin(); it != S.end(); ++it) {
        powers exponents = it->first;
        J = exponents.size();
        if(J < N) {
          continue;
        }
        T coeff = it->second;
        for(j = 0; j < N; j++) {
          nj = n[j];
          while((nj > 0) && (coeff != zero)) { // while loop because it might not run at all
            coeff *= exponents[j]; // multiply coeff first, then decrement exponent 
            exponents[j]--;
            nj--;
          }
        }
        if(coeff != zero) {
          v.clear();
          v.reserve(J);
          for(j = 0; j < J; j++) {
            v.emplace_back(exponents[j]);
          }
          QSPRAY::utils::simplifyPowers(v);
          Sprime[v] += coeff;  // increment because v is not row-unique any more
        }
      }  // loop closes
      
      return Qspray<T>(Sprime);
    }

  }; // end class Qspray

  // -------------------------------------------------------------------------- //
  template <typename T>
  static inline Qspray<T> Qlone(unsigned int n) {
    Polynomial<T> S;
    powers pows(n);
    if(n >= 1) {
      pows[n-1] = 1;      
    }
    S[pows] = T(1);
    return Qspray<T>(S);
  }

  // -------------------------------------------------------------------------- //
  static inline Qspray<gmpq> makeQspray(
    const Rcpp::List& Powers, const Rcpp::StringVector& coeffs
  ) {
    Polynomial<gmpq> S;
    for(int i = 0; i < Powers.size(); i++) {
      Rcpp::IntegerVector Exponents = Powers(i);
      gmpq coeff(Rcpp::as<std::string>(coeffs(i)));
      powers pows(Exponents.begin(), Exponents.end());
      S[pows] = coeff;
    }
    return Qspray<gmpq>(S);
  }

  // -------------------------------------------------------------------------- //
  static inline Rcpp::List returnQspray(Qspray<gmpq> Q) {  // to return a list to R
    // *Comment RH*: In this function, returning a zero-row matrix results in a
    // segfault ('memory not mapped').  So we check for 'S' being zero
    // size and, if so, return a special Nil value.  This corresponds to
    // an empty spray object.
    Polynomial<gmpq> S = Q.get();

    if(S.size() == 0) {
      return Rcpp::List::create(Rcpp::Named("powers") = R_NilValue,
                                Rcpp::Named("coeffs") = R_NilValue);
    } else {
      Rcpp::List Powers(S.size());
      powers pows;
      unsigned int row = 0, col = 0;
      Rcpp::StringVector Coeffs(S.size());
      unsigned int i = 0;
      for(auto it = S.begin(); it != S.end(); ++it) {
        pows = it->first;
        Rcpp::IntegerVector Exponents(pows.size());
        col = 0;
        for(auto ci = pows.begin(); ci != pows.end(); ++ci) {
          Exponents(col++) = *ci;
        }
        Powers(row++) = Exponents;
        Coeffs(i++) = QSPRAY::utils::q2str(it->second);
      }
      return Rcpp::List::create(Rcpp::Named("powers") = Powers,
                                Rcpp::Named("coeffs") = Coeffs);
    }
  }

  // template <typename T>
  // Qspray<T> scalarQspray(T x) {
  //   Polynomial<T> singleton;
  //   powers pows(0);
  //   singleton[pows] = x;
  //   return Qspray<T>(singleton);
  // }

  // -------------------------------------------------------------------------- //
  namespace internal {

    static int lexLeadingIndex(std::vector<powers> expnts) {
      const int n = expnts.size();
      if(n == 1) {
        return 0;
      }
      powers v0 = expnts[0];
      int out = 0;
      for(int i = 1; i < n; i++) {
        powers vi = expnts[i];
        bool vimax = std::lexicographical_compare(
          std::begin(v0), std::end(v0), std::begin(vi), std::end(vi)
        );
        if(vimax) {
          out = i;
          v0 = vi;
        }
      }
      return out;
      // int i = 0;
      // while(i < n-1) {
      //   powers vi = expnts[i];
      //   for(int j = i + 1; j < n; j++) {
      //     powers vj = expnts[j];
      //     bool vjmax = std::lexicographical_compare(
      //       std::begin(vi), std::end(vi), std::begin(vj), std::end(vj)
      //     );
      //     if(vjmax) {
      //       i = j - 1;
      //       break;
      //     } else if(j == n-1) {
      //       return i;
      //     }
      //   }
      //   i++;
      // }
      // return i;
    }

    static Rcpp::List leadingTerm(Qspray<gmpq>& Q, int d) {
      Polynomial<gmpq> S = Q.get();
      std::vector<powers> pows;
      std::vector<gmpq>   coeffs;
      pows.reserve(S.size());
      coeffs.reserve(S.size());
      for(const auto& term : S) {
        pows.emplace_back(term.first);
        coeffs.emplace_back(term.second);
      }
      int index = lexLeadingIndex(pows);
      powers leadingPows = pows[index];
      int npows = leadingPows.size();
      if(npows < d) {
        leadingPows = QSPRAY::utils::growPowers(leadingPows, npows, d);
      }
      std::string leadingCoeff = QSPRAY::utils::q2str(coeffs[index]);
      Rcpp::IntegerVector powsRcpp(leadingPows.begin(), leadingPows.end());
      return Rcpp::List::create(
        Rcpp::Named("powers") = powsRcpp,
        Rcpp::Named("coeff")  = leadingCoeff
      );
    }

    static bool divides(Rcpp::List f, Rcpp::List g) {
      Rcpp::IntegerVector pows_f = f["powers"];
      Rcpp::IntegerVector pows_g = g["powers"];
      int n = pows_f.size();
      int i = 0;
      bool out = true;
      while(out && i < n) {
        out = out && (pows_f(i) <= pows_g(i));
        i++;
      }
      return out;
    }

    static Qspray<gmpq> quotient(Rcpp::List f, Rcpp::List g) {
      Rcpp::IntegerVector pows_f = f["powers"];
      std::string coeff_f        = f["coeff"];
      Rcpp::IntegerVector pows_g = g["powers"];
      std::string coeff_g        = g["coeff"];
      gmpq qcoeff_f(coeff_f);
      gmpq qcoeff_g(coeff_g);
      Rcpp::IntegerVector powsRcpp = pows_f - pows_g;
      gmpq qcoeff = qcoeff_f / qcoeff_g;
      Polynomial<gmpq> S;
      powers pows(powsRcpp.begin(), powsRcpp.end());
      QSPRAY::utils::simplifyPowers(pows);
      S[pows] = qcoeff;
      return Qspray<gmpq>(S);
    }

  } // end namespace QSPRAY::internal

  static inline std::pair<Qspray<gmpq>,Qspray<gmpq>> qsprayDivision(
    Qspray<gmpq>& p, Qspray<gmpq>& g
  ) {
    if(g.isConstant()) {
      p.scale(1 / g.constantTerm());
      Qspray<gmpq> r(gmpq(0));
      return std::pair<Qspray<gmpq>,Qspray<gmpq>>(p, r);
    }
    int d = std::max<int>(p.numberOfVariables(), g.numberOfVariables());
    Rcpp::List LTg = internal::leadingTerm(g, d);
    Qspray<gmpq> q;
    Qspray<gmpq> r;
    bool divoccured;
    while(!p.empty()) {
      divoccured = false;
      Rcpp::List LTp = internal::leadingTerm(p, d);
      if(internal::divides(LTg, LTp)) {
        Qspray<gmpq> Qtnt = internal::quotient(LTp, LTg);
        p -= Qtnt * g;
        q += Qtnt;
        divoccured = true;
      } 
      if(!divoccured) {
        Rcpp::IntegerVector powsRcpp = LTp["powers"];
        std::string coeff            = LTp["coeff"];
        gmpq   coef(coeff);
        powers pows(powsRcpp.begin(), powsRcpp.end());
        QSPRAY::utils::simplifyPowers(pows);
        Polynomial<gmpq> LTpspray;
        LTpspray[pows] = coef;
        Qspray<gmpq> ltp(LTpspray);
        r += ltp;
        p -= ltp;
      }
    }
    return std::pair<Qspray<gmpq>,Qspray<gmpq>>(q, r);
  }

} // end namespace QSPRAY

// ---------------------------------------------------------------------------//
#endif
