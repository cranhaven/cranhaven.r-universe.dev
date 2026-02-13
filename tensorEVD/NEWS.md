### Version 0.1.4 (2024-09-02)

- New function 'Sum' to efficiently compute a weighted sum of two matrices
- Warnings from CRAN checks were addressed

### Version 0.1.3 (2024-05-28)

- Bugs fixed: Inplace calculation (parameter 'inplace=TRUE') in Hadamard_cov and Kronecker_cov functions caused an error in v0.1.2
- Warnings from CRAN checks with 'rchk' addressed

### Version 0.1.2 (2024-05-22)

- Change of names of arguments in 'Hadamard' function: rowsA -> IDrowA, colsA -> IDcolA, rowsB -> IDrowB, colsB -> IDcolB
- New function added: 'Hadamard_cov' to penalize a multi-variate covariance matrix. See help(Hadamard_cov)


### Version 0.1.1 (2024-02-07)

- Warnings and notes from CRAN checks addressed
- Pre-calculated EVD can be passed to the 'tensorEVD' function as a list type object as per the 'eigen' function
- Manuscript published: Lopez-Cruz et al., 2024 (doi:10.1093/g3journal/jkae001)


### Version 0.1.0 (2023-11-14)

- First released version
- Pre manuscript version
