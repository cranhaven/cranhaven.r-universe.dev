# Version 6.1.0 (2024-07-29)

- Some minor modifications have been necessary due to the updates of some 
package dependencies. No change for the user.

- The `KostkaNumbers` function has been renamed to `KostkaJackNumbers` and
it has a new argument `alpha`, to compute the Kostka-Jack numbers (Kostka 
numbers with a Jack parameter) with Jack parameter `alpha` (for `alpha=1` 
these are the ordinary Kostka numbers) for all partitions having a given weight.

- New function `KostkaJackNumbersWithGivenLambda`, to compute the Kostka-Jack
numbers with a given Jack parameter and a given partition `lambda`.

- New function `symbolicKostkaJackNumbers`, to get the Kostka-Jack numbers 
with a symbolic Jack parameter for all partitions having a given weight.

- New function `symbolicKostkaJackNumbersWithGivenLambda`, to get the 
Kostka-Jack numbers with a symbolic Jack parameter for a given partition 
`lambda`.

- New function `skewKostkaJackNumbers`, to get skew Kostka-Jack numbers with a 
given Jack parameter.

- New function `symbolicSkewKostkaJackNumbers`, to get skew Kostka-Jack numbers 
with a symbolic Jack parameter.

- New function `JackCombination`, to get a symmetric polynomial as a linear 
combination of some Jack polynomials with a fixed Jack parameter.

- New function `symbolicJackCombination`, to get a symmetric polynomial as a 
linear combination of some Jack polynomials with symbolic Jack parameter.

- New function `SkewJackPol`, to get a skew Jack polynomial with a given Jack
parameter.

- New function `SkewJackSymPol`, to get a skew Jack polynomial with a symbolic 
Jack parameter.

- New function `flaggedSchurPol`, to get a flagged Schur polynomial.

- New function `flaggedSkewSchurPol`, to get a flagged skew Schur polynomial.

- New function `factorialSchurPol`, to get a factorial Schur polynomial.

- New function `SkewFactorialSchurPol`, to get a skew factorial Schur polynomial.

- New function `tSchurPol`, to get a t-Schur polynomial.

- New function `tSkewSchurPol`, to get a skew t-Schur polynomial.

- New function `KostaFoulkesPolynomial`, to get a Kostka-Foulkes polynomial. 
A Kostka-Foulkes polynomial is a univariate polynomial associated to a pair of
integer partitions and its value at 1 is the Kostka number associated to these 
partitions.

- New function `SkewKostkaFoulkesPolynomial`, to get a skew Kostka-Foulkes 
polynomial.

- New function `HallLittlewoodPol`, to get a Hall-Littlewood polynomial. A 
Hall-Littlewood polynomial is a multivariate symmetric polynomial associated 
to an integer partition and depending on a parameter. When the value of this 
parameter is 0, then this is the Schur polynomial, and when the value of this 
parameter is 1, then this is the monomial symmetric polynomial.

- New function `SkewHallLittlewoodPol`, to get a skew Hall-Littlewood 
polynomial. A skew Hall-Littlewood polynomial is a multivariate symmetric 
polynomial associated to a skew integer partition and depending on a parameter. 
When the value of this parameter is 0, then this is the skew Schur polynomial.

- New function `qtKostkaPolynomials`, to get qt-Kostka polynomials, 
aka Kostka-Macdonald polynomials.

- New function `qtSkewKostkaPolynomials`, to get skew qt-Kostka 
polynomials.

- New function `MacdonaldPol`, to get a Macdonald polynomial.

- New function `SkewMacdonaldPol`, to get a skew Macdonald polynomial.

- New function `modifiedMacdonaldPol`, to get a modified Macdonald 
polynomial.

- New function `HallPolynomials`, to get the Hall polynomials.


# Version 6.0.0 (2024-05-03)

- It is now possible to get a Jack polynomial with a symbolic Jack parameter 
in its coefficients. Such polynomials are returned by the function `JackSymPol`. 
This big progress is the reason for which I increased the major component of 
the version of the package.

- Since the functions implemented with **Rcpp** are highly more efficient, 
the functions `Jack`, `JackPol`, `Schur`, `SchurPol`, `Zonal`, `ZonalPol`, 
`ZonalQ`, `ZonalQPol`, have been renamed to `JackR`, `JackPolR`, `SchurR`,
`SchurPolR`, `ZonalR`, `ZonalPolR`, `ZonalQR`, `ZonalQPolR`, and the functions 
`JackCPP`, `JackPolCPP`, `SchurCPP`, `SchurPolCPP`, `ZonalCPP`, `ZonalPolCPP`, 
`ZonalQCPP`, `ZonalQPolCPP` have been renamed to `Jack`, `JackPol`, `Schur`,
`SchurPol`, `Zonal`, `ZonalPol`, `ZonalQ`, `ZonalQPol`.

- New function `LRmult`, to compute the expression of the product of two Schur 
polynomials as a linear combination of Schur polynomials, using the 
Littlewood-Richardson rule.

- New function `LRskew`, to compute the expression of a skew Schur 
polynomial as a linear combination of Schur polynomials, using the 
Littlewood-Richardson rule.

- Based on `LRskew`, the new function `SkewSchurPol` computes the skew Schur 
polynomial of a given skew partition.

- Actually there are four possible Jack polynomials of a given partition for a
given `alpha`, denoted by `J`, `C`, `Q` or `P` (they are the same up to a 
normalization constant). It is now possible to get any of them (the previous
versions only allowed to get the `J` polynomial).


# Version 5.3.0 (2023-07-04)

The Julia stuff has been removed.


# Version 5.2.0 (2023-06-07)

- Now the 'Rcpp' implementations for the evaluation of the polynomials  
(functions `SchurCPP`, `JackCPP`, `ZonalCPP` and `ZonalQCPP`) are not 
restricted to rational numbers: they also allow double numbers.


# Version 5.1.0 (2023-05-08)

- Now there is a 'Rcpp' implementation for the evaluation of the polynomials: 
functions `SchurCPP`, `JackCPP`, `ZonalCPP` and `ZonalQCPP`.


# Version 5.0.1 (2023-01-30)

Changed C++20 to C++17.


# Version 5.0.0 (2023-01-27)

- Now there is a 'Rcpp' implementation of the polynomials: functions 
`SchurPolCPP`, `JackPolCPP`, `ZonalPolCPP` and `ZonalQPolCPP`. It is faster 
than the Julia implementation.


# Version 4.0.0 (2022-12-19)

- The package does not longer depend on the 'gmpoly' package. This dependency 
has been replaced with the 'qspray' package.


# Version 3.0.0 (2022-02-21)

- Now one can use a `bigq` number for `alpha` in `JackPol`, thanks to the 
'gmpoly' package, and one can use `exact=TRUE` with `algorithm=DK` for 
`ZonalPol`, `ZonalQPol` and `SchurPol`.

- Now one can get a `gmpoly` polynomial with Julia. 


# Version 2.0.0 (2022-02-14)

- New function `Jack_julia`, to evaluate the polynomials with Julia.


# Version 1.1.1 (2019-09-16)

- Fixed a test of empty partition

- Added more checks of parameters validity

- Added more unit tests

- Improved documentation


# Version 1.1.0 (2019-09-09)

- Some functions didn't handle the empty partition.


