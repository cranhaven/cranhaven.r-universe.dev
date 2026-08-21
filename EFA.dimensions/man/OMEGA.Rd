\name{OMEGA}
\alias{OMEGA}
\title{Omega internal consistency reliability coefficients}
\description{Total and hierarchical omega internal consistency reliability 
coefficients computed using multiple possible methods}
\usage{OMEGA(data, corkind = 'pearson',  
      bifactor_kind = c('McD', 'SL', 'SLiD', 'DSL', 
                        'bifactorT', 'bigeominT'),
      EFA_options    = list(extraction = 'minres', 
                            rotation = 'oblimin', 
                            Nfactors = 3),
      schmid_options = list(extraction = 'minres', 
                            rotation = 'oblimin', 
                            N_group_factors = 3),
      delta = .01, min_loading = .2, display = 2)} 
\arguments{
  \item{data}{
  \code{}An all-numeric dataframe where the rows are cases & the columns are 
  the variables, or a correlation matrix with ones on the diagonal. The function 
  internally determines whether the data are a correlation matrix.}

  \item{corkind}{
  \code{}The kind of correlation matrix to be used if data is not a correlation matrix. 
  The options are 'pearson', 'kendall', 'spearman', 'gamma', and 'polychoric'. Required 
  only if the entered data is not a correlation matrix.}

  \item{bifactor_kind}{
  \code{}The bifactor method(s) to be used. The options 
  are 'McD', 'SL', 'SLiD', 'GPA', 'DSL', 'bifactorQ', 'bifactorT', 
  'bigeominQ', and 'bigeominT'). Multiple methods can be specified.}

  \item{EFA_options}{
  \code{}(optional) A list with EFA options when rawdata is provided. The list elements
  must include values for 'extraction', 'rotation', and 'Nfactors'.
  \cr\cr The possibilities for extraction are: 'minres'(the default), 'alpha', 'fullinfo', 
  'gls', 'image', 'ml', 'ols', 'paf', 'uls', and 'wls'.
  \cr\cr The possibilities for rotation are:  
  'varimax', 'bentlerT', 'entropy', 'equamax', 'geominT', 'quartimax', 
  'promax', 'bentlerQ', 'geominQ', 'oblimin' (the default, 'oblimax', 'quartimin', 
  'simplimax', and 'none'.
  \cr\cr Nfactors is the number of factors to be extracted in the preliminary EFA 
  (i.e., prior to the bifactor analysis). 
  \cr It should be equal to the number of group factors + 1.
  }
 
  \item{schmid_options}{
  \code{}(optional) A list with schmid function (from the psych package) options 
  when bifactor_kind is one of 'SL', 'SLiD', or 'DSL'. The list elements
  must include values for 'extraction', 'rotation', and 'N_group_factors'.
  \cr\cr The possibilities for extraction are: 'minres'(the default), 'paf', 'pc', and 'ml'.
  \cr\cr The possibilities for rotation are: 'oblimin' (the default), 'simplimax', 
  'Promax', 'promax', and 'none'.
  \cr\cr N_group_factors is the number of group factors for the bifactor analysis.
  }

  \item{delta}{ 
  \code{} When bifactor_kind = 
  'bigeominT' or 'bigeominQ', delta is a tuning parameter 
  for the Geomin criterion. It acts as a small constant that is added to prevent 
  mathematical problems when a factor loading is exactly zero. 
  Delta is sometimes referred to as 'epsilon'.}
                  
 \item{min_loading}{ 
  \code{} The minimum value of a group factor loading for an item to be
  considered to have a non-negligible contribution to a group factor.
  min_loading only plays a role in some group factor statistics.}
                  
  \item{display}{
  \code{}The results to be displayed in the console: 
  \cr 0 = nothing
  \cr 1 = only the omega and scale coefficients
  \cr 2 (default) = detailed output, including the bifactor model loadings and statistics}
}
\details{
Run one of the following commands for detailed descriptions of 
the omega-total and omega-hierarchical internal consistency reliability 
coefficients and other statistics produced by this function: \cr
\itemize{

  \item{RShowDoc("Coefficient_descriptions_vignettes", package = "EFA.dimensions")} \cr

  \item{vignette("Coefficient_descriptions_vignettes")} \cr
}

For the bifactor_kind argument:

\itemize{		
  \item\strong{McD} produces the McDonald's omega_total based on a one-factor EFA
  rather than on bifactor analyses. omega_hierarchical is not computed.

  \item\strong{SL} produces omega_total and omega_hierarchical based on the Schmid-Leiman
  transformation of initial EFA output. The psych package (Revelle, 2026) is used
  for the Schmid-Leiman transformation.

  \item\strong{SLiD} produces omega_total and omega_hierarchical based on the 
  iterative empirical target rotation of an initial Schmid-Leiman solution method.
  The code was provided by Garcia-Garzon et al. (2021).

  \item\strong{DSL} produces omega_total and omega_hierarchical based on 
  the Direct Schmid-Leiman transformation of initial EFA output, following Waller (2018). 
  The psych package (Revelle, 2026) is used for the Direct Schmid-Leiman transformation.

  \item\strong{bifactorT} produces omega_total and omega_hierarchical based on
  bifactorT rotation implemented in the GPArotation package (Bernaard & Jennrich, 2026).

  \item\strong{bifactorQ} produces omega_total and omega_hierarchical based on
  bifactorQ rotation implemented in the GPArotation package (Bernaard & Jennrich, 2026).

  \item\strong{bigeominT} produces omega_total and omega_hierarchical based on
  bigeominT rotation implemented in the GPArotation package (Bernaard & Jennrich, 2026).

  \item\strong{bigeominQ} produces omega_total and omega_hierarchical based on
  bigeominQ rotation implemented in the GPArotation package (Bernaard & Jennrich, 2026).
}
}
\value{A list with the omega coefficients, the factor loadings, and model fit statistics.
}
\references{ 
      {Bernaards, C. A., & Jennrich, R. I. (2005). Gradient Projection Algorithms 
       and Software for Arbitrary Rotation Criteria in Factor Analysis. 
       \emph{Educational and Psychological Measurement, 65(5),} 676-696.}
\cr\cr {Bernaards, C. A., & Jennrich, R. I. (2026). GPArotation: Gradient Projection 
       Factor Rotation. R package version 2026.4-1, 
       https://CRAN.R-project.org/package=GPArotation}
\cr\cr {Educational Content Team. (2026). McDonald's Omega. Cogn-IQ Encyclopedia. 
        https://pubscience.org/cqep.2025.0048}
\cr\cr {Flora, D. B. (2020). Your coefficient alpha is probably wrong, but which 
       	coefficient omega is right? A tutorial on using R to obtain better reliability estimates. 
       	\emph{Advances in Methods and Practices in Psychological Science, 3(4),} 484501.}
\cr\cr {Garcia-Garzon, E., Abad, F. J., & Garrido, L. E. (2021). On omega  
        hierarchical estimation: A Comparison of Exploratory Bi-Factor Analysis
        Algorithms. \emph{Multivariate Behavioral Research, 56(1),} 101-119.}
\cr\cr {Jennrich, R. I. (2018). Rotation. In P. Irwing, T. Booth, & D. J. 
        Hughes (Eds.), \emph{The Wiley handbook of psychometric testing: A 
        multidisciplinary reference on survey, scale and test development} (pp. 279304). 
 	      Wiley Blackwell. https://doi.org/10.1002/9781118489772.ch10}
\cr\cr {Kalkbrenner, M. T. (2024). Choosing Between Cronbachs Coefficient Alpha, 
        McDonalds Coefficient Omega, and Coefficient H: Confidence Intervals 
        and the Advantages and Drawbacks of Interpretive Guidelines. 
        \emph{Measurement and Evaluation in Counseling and Development, 57(2),} 93105.}
\cr\cr {McNeish, D. (2018). Thanks coefficient alpha, we'll take it from here. 
	      \emph{Psychological Methods, 23(3),} 412433.}
\cr\cr {Revelle, W. (2026). psych: Procedures for Psychological, Psychometric, 
       and Personality Research. R package version 2.6.5, 
       https://CRAN.R-project.org/package=psych}
\cr\cr {Revelle, W., & Condon, D. M. (2019). Reliability from alpha to omega: A tutorial. 
	      \emph{Psychological Assessment, 31(12),} 13951411.}
\cr\cr {Waller, N. G. (2018) Direct Schmid-Leiman Transformations and Rank-Deficient 
        Loadings Matrices. \emph{Psychometrika, 83(4),} 858870.}
}
\author{Brian P. O'Connor }
\examples{
OMEGA(data_RSE, display = 1)
\donttest{
}
}