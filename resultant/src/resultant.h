#ifndef __HEADER__
#define __HEADER__

#include <Rcpp.h>

#include "gmp.h"
#include <CGAL/Gmpq.h>
#include <CGAL/Gmpz.h>
#include <CGAL/Polynomial.h>
#include <CGAL/Polynomial_traits_d.h>
#include <CGAL/Polynomial_type_generator.h>
#include <CGAL/polynomial_utils.h>
#include <CGAL/Polynomial/Monomial_representation.h>
#include <CGAL/number_utils.h>

typedef CGAL::Polynomial_type_generator<CGAL::Gmpq, 1>::Type Poly1;
typedef CGAL::Polynomial_traits_d<Poly1>                     PT1;
typedef std::pair<CGAL::Exponent_vector, PT1::Innermost_coefficient_type> Monomial1;
typedef CGAL::Polynomial_type_generator<CGAL::Gmpq, 2>::Type Poly2;
typedef CGAL::Polynomial_traits_d<Poly2>                     PT2;
typedef std::pair<CGAL::Exponent_vector, PT2::Innermost_coefficient_type> Monomial2;
typedef CGAL::Polynomial_type_generator<CGAL::Gmpq, 3>::Type Poly3;
typedef CGAL::Polynomial_traits_d<Poly3>                     PT3;
typedef std::pair<CGAL::Exponent_vector, PT3::Innermost_coefficient_type> Monomial3;
typedef CGAL::Polynomial_type_generator<CGAL::Gmpq, 4>::Type Poly4;
typedef CGAL::Polynomial_traits_d<Poly4>                     PT4;
typedef std::pair<CGAL::Exponent_vector, PT4::Innermost_coefficient_type> Monomial4;
typedef CGAL::Polynomial_type_generator<CGAL::Gmpq, 5>::Type Poly5;
typedef CGAL::Polynomial_traits_d<Poly5>                     PT5;
typedef std::pair<CGAL::Exponent_vector, PT5::Innermost_coefficient_type> Monomial5;
typedef CGAL::Polynomial_type_generator<CGAL::Gmpq, 6>::Type Poly6;
typedef CGAL::Polynomial_traits_d<Poly6>                     PT6;
typedef std::pair<CGAL::Exponent_vector, PT6::Innermost_coefficient_type> Monomial6;
typedef CGAL::Polynomial_type_generator<CGAL::Gmpq, 7>::Type Poly7;
typedef CGAL::Polynomial_traits_d<Poly7>                     PT7;
typedef std::pair<CGAL::Exponent_vector, PT7::Innermost_coefficient_type> Monomial7;
typedef CGAL::Polynomial_type_generator<CGAL::Gmpq, 8>::Type Poly8;
typedef CGAL::Polynomial_traits_d<Poly8>                     PT8;
typedef std::pair<CGAL::Exponent_vector, PT8::Innermost_coefficient_type> Monomial8;
typedef CGAL::Polynomial_type_generator<CGAL::Gmpq, 9>::Type Poly9;
typedef CGAL::Polynomial_traits_d<Poly9>                     PT9;
typedef std::pair<CGAL::Exponent_vector, PT9::Innermost_coefficient_type> Monomial9;

#endif
