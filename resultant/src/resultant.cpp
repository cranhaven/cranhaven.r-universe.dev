#include "resultant.h"

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
std::string q2str(CGAL::Gmpq r) {
  CGAL::Gmpz numer = r.numerator();
  CGAL::Gmpz denom = r.denominator();
  size_t n = mpz_sizeinbase(numer.mpz(), 10) + 2;
  size_t d = mpz_sizeinbase(denom.mpz(), 10) + 2;
  char* cnumer = new char[n];
  char* cdenom = new char[d];
  cnumer = mpz_get_str(cnumer, 10, numer.mpz());
  cdenom = mpz_get_str(cdenom, 10, denom.mpz());
  std::string snumer = cnumer;
  std::string sdenom = cdenom;
  delete[] cnumer;
  delete[] cdenom;
  return snumer + "/" + sdenom;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
Poly1 makePoly1(
    Rcpp::IntegerVector Powers, Rcpp::CharacterVector Coeffs
) {
  PT1::Construct_polynomial constructPolynomial;
  int nterms = Coeffs.size();
  std::list<Monomial1> terms;
  for(int i = 0; i < nterms; i++) {
    terms.push_back(
      std::make_pair(
        CGAL::Exponent_vector(Powers(i)),
        CGAL::Gmpq(Rcpp::as<std::string>(Coeffs(i)))
      )
    );
  }
  return constructPolynomial(terms.begin(), terms.end());
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
template <typename PolyX, typename PTX, typename MonomialX>
PolyX makePolyX(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs
) {
  typename PTX::Construct_polynomial constructPolynomial;
  int nterms = Coeffs.size();
  std::list<MonomialX> terms;
  for(int i = 0; i < nterms; i++) {
    Rcpp::IntegerVector powers = Powers(Rcpp::_, i);
    terms.push_back(
      std::make_pair(
        CGAL::Exponent_vector(powers.begin(), powers.end()),
        CGAL::Gmpq(Rcpp::as<std::string>(Coeffs(i)))
      )
    );
  }
  return constructPolynomial(terms.begin(), terms.end());  
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
template <typename PolyX, typename PTX, typename MonomialX>
Rcpp::List getPolynomial(
  PolyX P, int X
) {
  std::list<MonomialX> monomials;
  typename PTX::Monomial_representation mrepr;
  mrepr(P, std::back_inserter(monomials));
  int n = monomials.size();
  Rcpp::IntegerMatrix Powers(X, n);
  Rcpp::CharacterVector Coeffs(n);
  typename std::list<MonomialX>::iterator it_monoms;
  int i = 0;
  for(it_monoms = monomials.begin(); it_monoms != monomials.end(); it_monoms++) {
    CGAL::Exponent_vector exponents = (*it_monoms).first;
    Rcpp::IntegerVector powers(exponents.begin(), exponents.end());
    Powers(Rcpp::_, i) = powers;
    Coeffs(i) = q2str((*it_monoms).second);
    i++;
  }
  return Rcpp::List::create(Rcpp::Named("Powers") = Powers,
                            Rcpp::Named("Coeffs") = Coeffs);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::CharacterVector resultantCPP1(
  Rcpp::IntegerVector PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerVector PowersG, Rcpp::CharacterVector CoeffsG
) {
  Poly1 F = makePoly1(PowersF, CoeffsF);
  Poly1 G = makePoly1(PowersG, CoeffsG);
  PT1::Resultant resultant;
  CGAL::Gmpq r = resultant(F, G);
  Rcpp::CharacterVector out = Rcpp::CharacterVector::create(q2str(r));
  return out;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::CharacterVector resultantCPP2(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool permute
) {
  Poly2 F = makePolyX<Poly2, PT2, Monomial2>(PowersF, CoeffsF);
  Poly2 G = makePolyX<Poly2, PT2, Monomial2>(PowersG, CoeffsG);
  if(permute) {
    PT2::Swap swap;
    F = swap(F, 0, 1);
    G = swap(G, 0, 1);
  }
  PT2::Resultant resultant;
  Poly1 R = resultant(F, G);
  PT1::Degree degree;
  int d = degree(R);
  PT1::Get_coefficient getCoefficient;
  Rcpp::CharacterVector Coeffs(d + 1);
  for(int i = 0; i <= d; i++) {
    Coeffs(i) = q2str(getCoefficient(R, i));
  }
  return Coeffs;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List resultantCPP3(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    Rcpp::IntegerVector permutation
) {
  Poly3 F = makePolyX<Poly3, PT3, Monomial3>(PowersF, CoeffsF);
  Poly3 G = makePolyX<Poly3, PT3, Monomial3>(PowersG, CoeffsG);
  PT3::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  PT3::Resultant resultant;
  Poly2 R = resultant(F, G);
  return getPolynomial<Poly2, PT2, Monomial2>(R, 2);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List resultantCPP4(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    Rcpp::IntegerVector permutation
) {
  Poly4 F = makePolyX<Poly4, PT4, Monomial4>(PowersF, CoeffsF);
  Poly4 G = makePolyX<Poly4, PT4, Monomial4>(PowersG, CoeffsG);
  PT4::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  PT4::Resultant resultant;
  Poly3 R = resultant(F, G);
  return getPolynomial<Poly3, PT3, Monomial3>(R, 3);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List resultantCPP5(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    Rcpp::IntegerVector permutation
) {
  Poly5 F = makePolyX<Poly5, PT5, Monomial5>(PowersF, CoeffsF);
  Poly5 G = makePolyX<Poly5, PT5, Monomial5>(PowersG, CoeffsG);
  PT5::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  PT5::Resultant resultant;
  Poly4 R = resultant(F, G);
  return getPolynomial<Poly4, PT4, Monomial4>(R, 4);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List resultantCPP6(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    Rcpp::IntegerVector permutation
) {
  Poly6 F = makePolyX<Poly6, PT6, Monomial6>(PowersF, CoeffsF);
  Poly6 G = makePolyX<Poly6, PT6, Monomial6>(PowersG, CoeffsG);
  PT6::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  PT6::Resultant resultant;
  Poly5 R = resultant(F, G);
  return getPolynomial<Poly5, PT5, Monomial5>(R, 5);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List resultantCPP7(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    Rcpp::IntegerVector permutation
) {
  Poly7 F = makePolyX<Poly7, PT7, Monomial7>(PowersF, CoeffsF);
  Poly7 G = makePolyX<Poly7, PT7, Monomial7>(PowersG, CoeffsG);
  PT7::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  PT7::Resultant resultant;
  Poly6 R = resultant(F, G);
  return getPolynomial<Poly6, PT6, Monomial6>(R, 6);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List resultantCPP8(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    Rcpp::IntegerVector permutation
) {
  Poly8 F = makePolyX<Poly8, PT8, Monomial8>(PowersF, CoeffsF);
  Poly8 G = makePolyX<Poly8, PT8, Monomial8>(PowersG, CoeffsG);
  PT8::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  PT8::Resultant resultant;
  Poly7 R = resultant(F, G);
  return getPolynomial<Poly7, PT7, Monomial7>(R, 7);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List resultantCPP9(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    Rcpp::IntegerVector permutation
) {
  Poly9 F = makePolyX<Poly9, PT9, Monomial9>(PowersF, CoeffsF);
  Poly9 G = makePolyX<Poly9, PT9, Monomial9>(PowersG, CoeffsG);
  PT9::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  PT9::Resultant resultant;
  Poly8 R = resultant(F, G);
  return getPolynomial<Poly8, PT8, Monomial8>(R, 8);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
template 
  <typename PolyX, typename PTX, typename MonomialX, 
   typename PolyW, typename PTW, typename MonomialW, int W>
Rcpp::List principalSubresultantsCPPX(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  PolyX F = makePolyX<PolyX, PTX, MonomialX>(PowersF, CoeffsF);
  PolyX G = makePolyX<PolyX, PTX, MonomialX>(PowersG, CoeffsG);
  typename PTX::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  std::vector<PolyW> subresultants;
  CGAL::principal_subresultants(F, G, std::back_inserter(subresultants));
  int n = subresultants.size();
  Rcpp::List out(n);
  for(int i = 0; i < n ; i++) {
    out(i) = getPolynomial<PolyW, PTW, MonomialW>(subresultants[i], W);
  }
  return out;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::CharacterVector principalSubresultantsCPP1(
  Rcpp::IntegerVector PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerVector PowersG, Rcpp::CharacterVector CoeffsG
) {
  Poly1 F = makePoly1(PowersF, CoeffsF);
  Poly1 G = makePoly1(PowersG, CoeffsG);
  std::vector<CGAL::Gmpq> subresultants;
  CGAL::principal_subresultants(F, G, std::back_inserter(subresultants));
  int n = subresultants.size();
  Rcpp::CharacterVector out(n);
  for(int i = 0; i < n ; i++) {
    out(i) = q2str(subresultants[i]);
  }
  return out;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSubresultantsCPP2(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  bool permute
) {
  Poly2 F = makePolyX<Poly2, PT2, Monomial2>(PowersF, CoeffsF);
  Poly2 G = makePolyX<Poly2, PT2, Monomial2>(PowersG, CoeffsG);
  if(permute) {
    PT2::Swap swap;
    F = swap(F, 0, 1);
    G = swap(G, 0, 1);
  }
  std::vector<Poly1> subresultants;
  CGAL::principal_subresultants(F, G, std::back_inserter(subresultants));
  int n = subresultants.size();
  Rcpp::List out(n);
  for(int i = 0; i < n ; i++) {
    out(i) = getPolynomial<Poly1, PT1, Monomial1>(subresultants[i], 1);
  }
  return out;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSubresultantsCPP3(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  return principalSubresultantsCPPX<Poly3, PT3, Monomial3, Poly2, PT2, Monomial2, 2>(
    PowersF, CoeffsF, PowersG, CoeffsG, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSubresultantsCPP4(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  return principalSubresultantsCPPX<Poly4, PT4, Monomial4, Poly3, PT3, Monomial3, 3>(
    PowersF, CoeffsF, PowersG, CoeffsG, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSubresultantsCPP5(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  return principalSubresultantsCPPX<Poly5, PT5, Monomial5, Poly4, PT4, Monomial4, 4>(
    PowersF, CoeffsF, PowersG, CoeffsG, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSubresultantsCPP6(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  return principalSubresultantsCPPX<Poly6, PT6, Monomial6, Poly5, PT5, Monomial5, 5>(
    PowersF, CoeffsF, PowersG, CoeffsG, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSubresultantsCPP7(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  return principalSubresultantsCPPX<Poly7, PT7, Monomial7, Poly6, PT6, Monomial6, 6>(
    PowersF, CoeffsF, PowersG, CoeffsG, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSubresultantsCPP8(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  return principalSubresultantsCPPX<Poly8, PT8, Monomial8, Poly7, PT7, Monomial7, 7>(
    PowersF, CoeffsF, PowersG, CoeffsG, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSubresultantsCPP9(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  return principalSubresultantsCPPX<Poly9, PT9, Monomial9, Poly8, PT8, Monomial8, 8>(
    PowersF, CoeffsF, PowersG, CoeffsG, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP1(
    Rcpp::IntegerVector PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerVector PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  Poly1 F = makePoly1(PowersF, CoeffsF);
  Poly1 G = makePoly1(PowersG, CoeffsG);
  Poly1 D;
  if(UTCF) {
    typename PT1::Gcd_up_to_constant_factor gcd_utcf;
    D = gcd_utcf(F, G);  
  } else {
    typename PT1::Gcd gcd;
    D = gcd(F, G);
  }
  return getPolynomial<Poly1, PT1, Monomial1>(D, 1);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
template <typename PolyX, typename PTX, typename MonomialX, int X>
Rcpp::List gcdCPPX(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  PolyX F = makePolyX<PolyX, PTX, MonomialX>(PowersF, CoeffsF);
  PolyX G = makePolyX<PolyX, PTX, MonomialX>(PowersG, CoeffsG);
  PolyX D;
  if(UTCF) {
    typename PTX::Gcd_up_to_constant_factor gcd_utcf;
    D = gcd_utcf(F, G);  
  } else {
    typename PTX::Gcd gcd;
    D = gcd(F, G);
  }
  return getPolynomial<PolyX, PTX, MonomialX>(D, X);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP2(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly2, PT2, Monomial2, 2>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP3(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly3, PT3, Monomial3, 3>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP4(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly4, PT4, Monomial4, 4>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP5(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly5, PT5, Monomial5, 5>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP6(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly6, PT6, Monomial6, 6>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP7(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly7, PT7, Monomial7, 7>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP8(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly8, PT8, Monomial8, 8>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP9(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly9, PT9, Monomial9, 9>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
int numberOfRealRootsCPP(
  Rcpp::IntegerVector Powers, Rcpp::CharacterVector Coeffs
) {
  Poly1 P = makePoly1(Powers, Coeffs);
  return CGAL::number_of_real_roots(P);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List divModCPP1(
  Rcpp::IntegerVector PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerVector PowersG, Rcpp::CharacterVector CoeffsG
) {
  Poly1 F = makePoly1(PowersF, CoeffsF);
  Poly1 G = makePoly1(PowersG, CoeffsG);
  Poly1 q; Poly1 r;
  CGAL::div_mod(F, G, q, r);
  return Rcpp::List::create(
    Rcpp::Named("Q") = getPolynomial<Poly1, PT1, Monomial1>(q, 1),
    Rcpp::Named("R") = getPolynomial<Poly1, PT1, Monomial1>(r, 1)
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
template 
  <typename PolyX, typename PTX, typename MonomialX, int X>
Rcpp::List integralDivisionCPPX(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  bool check
) {
  PolyX F = makePolyX<PolyX, PTX, MonomialX>(PowersF, CoeffsF);
  PolyX G = makePolyX<PolyX, PTX, MonomialX>(PowersG, CoeffsG);
  PolyX Q;
  Rcpp::List out;
  if(check) {
    typename PTX::Divides divides;
    bool d = divides(G, F, Q);
    if(d) {
      out = getPolynomial<PolyX, PTX, MonomialX>(Q, X);
    }     
  } else {
    Q = CGAL::integral_division(F, G);
    out = getPolynomial<PolyX, PTX, MonomialX>(Q, X);
  }
  return out;
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List integralDivisionCPP1(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  bool check
) {
  return integralDivisionCPPX<Poly1, PT1, Monomial1, 1>(
    PowersF, CoeffsF, PowersG, CoeffsG, check
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List integralDivisionCPP2(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  bool check
) {
  return integralDivisionCPPX<Poly2, PT2, Monomial2, 2>(
    PowersF, CoeffsF, PowersG, CoeffsG, check
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List integralDivisionCPP3(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  bool check
) {
  return integralDivisionCPPX<Poly3, PT3, Monomial3, 3>(
    PowersF, CoeffsF, PowersG, CoeffsG, check
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List integralDivisionCPP4(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  bool check
) {
  return integralDivisionCPPX<Poly4, PT4, Monomial4, 4>(
    PowersF, CoeffsF, PowersG, CoeffsG, check
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List integralDivisionCPP5(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  bool check
) {
  return integralDivisionCPPX<Poly5, PT5, Monomial5, 5>(
    PowersF, CoeffsF, PowersG, CoeffsG, check
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List integralDivisionCPP6(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  bool check
) {
  return integralDivisionCPPX<Poly6, PT6, Monomial6, 6>(
    PowersF, CoeffsF, PowersG, CoeffsG, check
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List integralDivisionCPP7(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  bool check
) {
  return integralDivisionCPPX<Poly7, PT7, Monomial7, 7>(
    PowersF, CoeffsF, PowersG, CoeffsG, check
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List integralDivisionCPP8(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  bool check
) {
  return integralDivisionCPPX<Poly8, PT8, Monomial8, 8>(
    PowersF, CoeffsF, PowersG, CoeffsG, check
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List integralDivisionCPP9(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  bool check
) {
  return integralDivisionCPPX<Poly9, PT9, Monomial9, 9>(
    PowersF, CoeffsF, PowersG, CoeffsG, check
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
template 
  <typename PolyX, typename PTX, typename MonomialX, int X>
Rcpp::List SturmHabichtCPPX(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs, int var
) {
  PolyX P = makePolyX<PolyX, PTX, MonomialX>(Powers, Coeffs);
  typename PTX::Sturm_habicht_sequence sturmHabichtSequence;
  std::vector<PolyX> SHsequence;
  sturmHabichtSequence(P, std::back_inserter(SHsequence), var);
//  CGAL::sturm_habicht_sequence(P, std::back_inserter(SHsequence));
  int n = SHsequence.size();
  Rcpp::List out(n);
  for(int i = 0; i < n; i++) {
    out(i) = getPolynomial<PolyX, PTX, MonomialX>(SHsequence[i], X);
  }
  return out;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SturmHabichtCPP1(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs, int var
) {
  return SturmHabichtCPPX<Poly1, PT1, Monomial1, 1>(Powers, Coeffs, var);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SturmHabichtCPP2(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs, int var
) {
  return SturmHabichtCPPX<Poly2, PT2, Monomial2, 2>(Powers, Coeffs, var);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SturmHabichtCPP3(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs, int var
) {
  return SturmHabichtCPPX<Poly3, PT3, Monomial3, 3>(Powers, Coeffs, var);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SturmHabichtCPP4(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs, int var
) {
  return SturmHabichtCPPX<Poly4, PT4, Monomial4, 4>(Powers, Coeffs, var);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SturmHabichtCPP5(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs, int var
) {
  return SturmHabichtCPPX<Poly5, PT5, Monomial5, 5>(Powers, Coeffs, var);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SturmHabichtCPP6(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs, int var
) {
  return SturmHabichtCPPX<Poly6, PT6, Monomial6, 6>(Powers, Coeffs, var);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SturmHabichtCPP7(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs, int var
) {
  return SturmHabichtCPPX<Poly7, PT7, Monomial7, 7>(Powers, Coeffs, var);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SturmHabichtCPP8(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs, int var
) {
  return SturmHabichtCPPX<Poly8, PT8, Monomial8, 8>(Powers, Coeffs, var);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SturmHabichtCPP9(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs, int var
) {
  return SturmHabichtCPPX<Poly9, PT9, Monomial9, 9>(Powers, Coeffs, var);
}


// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
template 
  <typename PolyX, typename PTX, typename MonomialX, 
   typename PolyW, typename PTW, typename MonomialW, int W>
Rcpp::List principalSturmHabichtCPPX(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs,
  Rcpp::IntegerVector permutation
) {
  PolyX P = makePolyX<PolyX, PTX, MonomialX>(Powers, Coeffs);
  typename PTX::Permute permute;
  P = permute(P, permutation.begin(), permutation.end());
  std::vector<PolyW> PSHsequence;
  CGAL::principal_sturm_habicht_sequence(P, std::back_inserter(PSHsequence));
  int n = PSHsequence.size();
  Rcpp::List out(n);
  for(int i = 0; i < n; i++) {
    out(i) = getPolynomial<PolyW, PTW, MonomialW>(PSHsequence[i], W);
  }
  return out;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::CharacterVector principalSturmHabichtCPP1(
  Rcpp::IntegerVector Powers, Rcpp::CharacterVector Coeffs
) {
  Poly1 P = makePoly1(Powers, Coeffs);
  std::vector<CGAL::Gmpq> PSHsequence;
  CGAL::principal_sturm_habicht_sequence(P, std::back_inserter(PSHsequence));
  int n = PSHsequence.size();
  Rcpp::CharacterVector out(n);
  for(int i = 0; i < n; i++) {
    out(i) = q2str(PSHsequence[i]);
  }
  return out;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSturmHabichtCPP2(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs,
  Rcpp::IntegerVector permutation
) {
  return principalSturmHabichtCPPX<Poly2, PT2, Monomial2, Poly1, PT1, Monomial1, 1>(
    Powers, Coeffs, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSturmHabichtCPP3(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs,
  Rcpp::IntegerVector permutation
) {
  return principalSturmHabichtCPPX<Poly3, PT3, Monomial3, Poly2, PT2, Monomial2, 2>(
    Powers, Coeffs, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSturmHabichtCPP4(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs,
  Rcpp::IntegerVector permutation
) {
  return principalSturmHabichtCPPX<Poly4, PT4, Monomial4, Poly3, PT3, Monomial3, 3>(
    Powers, Coeffs, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSturmHabichtCPP5(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs,
  Rcpp::IntegerVector permutation
) {
  return principalSturmHabichtCPPX<Poly5, PT5, Monomial5, Poly4, PT4, Monomial4, 4>(
    Powers, Coeffs, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSturmHabichtCPP6(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs,
  Rcpp::IntegerVector permutation
) {
  return principalSturmHabichtCPPX<Poly6, PT6, Monomial6, Poly5, PT5, Monomial5, 5>(
    Powers, Coeffs, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSturmHabichtCPP7(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs,
  Rcpp::IntegerVector permutation
) {
  return principalSturmHabichtCPPX<Poly7, PT7, Monomial7, Poly6, PT6, Monomial6, 6>(
    Powers, Coeffs, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSturmHabichtCPP8(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs,
  Rcpp::IntegerVector permutation
) {
  return principalSturmHabichtCPPX<Poly8, PT8, Monomial8, Poly7, PT7, Monomial7, 7>(
    Powers, Coeffs, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List principalSturmHabichtCPP9(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs,
  Rcpp::IntegerVector permutation
) {
  return principalSturmHabichtCPPX<Poly9, PT9, Monomial9, Poly8, PT8, Monomial8, 8>(
    Powers, Coeffs, permutation
  );
}


// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
template 
  <typename PolyX, typename PTX, typename MonomialX, int X>
Rcpp::List squareFreeFactorizationCPPX(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs
) {
  const PolyX P = makePolyX<PolyX, PTX, MonomialX>(Powers, Coeffs);
  typename PTX::Square_free_factorize squareFreeFactorize;
  std::vector<std::pair<PolyX,int>> sfFactorization;
  CGAL::Gmpq a;
  squareFreeFactorize(P, std::back_inserter(sfFactorization), a);
  Rcpp::CharacterVector constantFactor = 
    Rcpp::CharacterVector::create(q2str(a));
  int n = sfFactorization.size();
  Rcpp::List factorization(n);
  for(int i = 0; i < n; i++) {
    std::pair<PolyX,int> factor = sfFactorization[i];
    PolyX polynomial = factor.first;
    Rcpp::IntegerVector multiplicity = 
      Rcpp::IntegerVector::create(factor.second);
    factorization(i) = Rcpp::List::create(
      Rcpp::Named("qspray") = 
        getPolynomial<PolyX, PTX, MonomialX>(polynomial, X),
      Rcpp::Named("multiplicity") = multiplicity
    );
  }
  return Rcpp::List::create(
    Rcpp::Named("constantFactor")     = constantFactor,
    Rcpp::Named("nonConstantFactors") = factorization
  );
}


// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List squareFreeFactorizationCPP1(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs
) {
  return squareFreeFactorizationCPPX<Poly1, PT1, Monomial1, 1>(Powers, Coeffs);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List squareFreeFactorizationCPP2(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs
) {
  return squareFreeFactorizationCPPX<Poly2, PT2, Monomial2, 2>(Powers, Coeffs);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List squareFreeFactorizationCPP3(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs
) {
  return squareFreeFactorizationCPPX<Poly3, PT3, Monomial3, 3>(Powers, Coeffs);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List squareFreeFactorizationCPP4(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs
) {
  return squareFreeFactorizationCPPX<Poly4, PT4, Monomial4, 4>(Powers, Coeffs);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List squareFreeFactorizationCPP5(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs
) {
  return squareFreeFactorizationCPPX<Poly5, PT5, Monomial5, 5>(Powers, Coeffs);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List squareFreeFactorizationCPP6(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs
) {
  return squareFreeFactorizationCPPX<Poly6, PT6, Monomial6, 6>(Powers, Coeffs);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List squareFreeFactorizationCPP7(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs
) {
  return squareFreeFactorizationCPPX<Poly7, PT7, Monomial7, 7>(Powers, Coeffs);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List squareFreeFactorizationCPP8(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs
) {
  return squareFreeFactorizationCPPX<Poly8, PT8, Monomial8, 8>(Powers, Coeffs);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List squareFreeFactorizationCPP9(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs
) {
  return squareFreeFactorizationCPPX<Poly9, PT9, Monomial9, 9>(Powers, Coeffs);
}


// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
template 
  <typename PolyX, typename PTX, typename MonomialX, int X>
Rcpp::List subresultantsCPPX(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  int var
) {
  PolyX F = makePolyX<PolyX, PTX, MonomialX>(PowersF, CoeffsF);
  PolyX G = makePolyX<PolyX, PTX, MonomialX>(PowersG, CoeffsG);
  typename PTX::Move move;
  F = move(F, var, X-1);
  G = move(G, var, X-1);
  std::vector<PolyX> subresultants;
  CGAL::polynomial_subresultants(F, G, std::back_inserter(subresultants));
  int n = subresultants.size();
  Rcpp::List out(n);
  for(int i = 0; i < n; i++) {
    PolyX subresultant = move(subresultants[i], X-1, var);
    out(i) = getPolynomial<PolyX, PTX, MonomialX>(subresultant, X);
  }
  return out;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP1(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  int var
) {
  return subresultantsCPPX<Poly1, PT1, Monomial1, 1>(
    PowersF, CoeffsF, PowersG, CoeffsG, var
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP2(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  int var
) {
  return subresultantsCPPX<Poly2, PT2, Monomial2, 2>(
    PowersF, CoeffsF, PowersG, CoeffsG, var
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP3(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  int var
) {
  return subresultantsCPPX<Poly3, PT3, Monomial3, 3>(
    PowersF, CoeffsF, PowersG, CoeffsG, var
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP4(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  int var
) {
  return subresultantsCPPX<Poly4, PT4, Monomial4, 4>(
    PowersF, CoeffsF, PowersG, CoeffsG, var
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP5(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  int var
) {
  return subresultantsCPPX<Poly5, PT5, Monomial5, 5>(
    PowersF, CoeffsF, PowersG, CoeffsG, var
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP6(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  int var
) {
  return subresultantsCPPX<Poly6, PT6, Monomial6, 6>(
    PowersF, CoeffsF, PowersG, CoeffsG, var
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP7(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  int var
) {
  return subresultantsCPPX<Poly7, PT7, Monomial7, 7>(
    PowersF, CoeffsF, PowersG, CoeffsG, var
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP8(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  int var
) {
  return subresultantsCPPX<Poly8, PT8, Monomial8, 8>(
    PowersF, CoeffsF, PowersG, CoeffsG, var
  );
} 

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP9(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  int var
) {
  return subresultantsCPPX<Poly9, PT9, Monomial9, 9>(
    PowersF, CoeffsF, PowersG, CoeffsG, var
  );
} 
