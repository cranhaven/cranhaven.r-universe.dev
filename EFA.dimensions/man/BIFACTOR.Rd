\name{BIFACTOR}
\alias{BIFACTOR}
\title{Bifactor analysis}
\description{Bifactor analysis with multiple options for data entry and analysis}
\usage{BIFACTOR(loadings = NULL,
         rawdata = NULL,   
         cormat = NULL, Ncases = NULL, corkind = 'pearson', 
         bifactor_kind = 'bifactorT', 
         EFA_options    = list(extraction = 'minres', 
                               rotation = 'oblimin', 
                               Nfactors = 3),
         schmid_options = list(extraction = 'minres', 
                               rotation = 'oblimin', 
                               N_group_factors = 3),
         delta = .01,
         min_loading = .2,
         verbose = TRUE)}
\arguments{
  \item{loadings}{
  \code{}(optional) loadings can be either a bifactor loading matrix or an EFA
  loading matrix. See the Details and Examples sections below.}

  \item{rawdata}{
  \code{}(optional) An all-numeric dataframe where the rows are cases & the columns are 
  the variables. Required only when loadings and cormat are not provided.}

  \item{cormat}{
  \code{}(optional) A correlation matrix with ones on the diagonal.
  Required only when loadings and rawdata are not provided.}

  \item{Ncases}{
  \code{}(optional) The number of cases. Required only when cormat is provided.}

  \item{corkind}{
  \code{}(optional) The kind of correlation matrix to be used when rawdata is provided. 
  The options are 'pearson', 'kendall', 'spearman', 'gamma', and 'polychoric'.}

  \item{bifactor_kind}{
  \code{}(optional) Specify one of the following possible bifactor methods: 
  'bifactorT', 'bifactorQ', 'bigeominT', 'bigeominQ', 'SL', 'SLiD', 'DSL', or 'none'.
  Use 'bifactor_kind = "none"' only when loadings is (already) a bifactor loading matrix.
  \cr Example: bifactor_kind = 'bifactorT'}

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
  }
 
  \item{schmid_options}{
  \code{}(optional) A list with schmid function (from the psych package) options 
  when bifactor_kind is one of 'SL', 'SLiD', or 'DSL'. The list elements
  must include values for 'extraction', 'rotation', and 'N_group_factors'.
  \cr\cr The possibilities for extraction are: 'minres'(the default), 'paf', 'pc', and 'ml'.
  \cr\cr The possibilities for rotation are: 'oblimin' (the default), 'simplimax', 
  'Promax', 'promax' and 'none'.
  \cr\cr N_group_factors is the number of group factors for the bifactor analysis.
  }
  
  \item{delta}{ 
  \code{}When bifactor_kind = 'bigeominT' or 'bigeominQ', delta is a tuning parameter 
  for the Geomin criterion. It acts as a small constant that is added to prevent 
  mathematical problems when a factor loading is exactly zero. 
  Delta is sometimes referred to as 'epsilon'.}
                  
 \item{min_loading}{ 
  \code{}The minimum value of a group factor loading for an item to be
  considered to have a non-negligible contribution to a group factor.
  min_loading only plays a role in the computations for PUC and for
  some group factor statistics.}
                  
  \item{verbose}{
  \code{}Should detailed results be displayed in console? TRUE (default) or FALSE}
}
\details{
  The bifactor analyses can be conducted for three possible data input scenarios.

	\itemize{
    \item
  When "loadings" is provided and when it is (already) a bifactor loading matrix, 
  then bifactor rotations are not conducted and the function merely provides a variety
  of bifactor model, factor, and item statistics. "bifactor_kind" must be set
  to "none" for the function to recognize that loadings is (already) a 
  bifactor loading matrix.

    \item
  When "loadings" is provided and when it consists of EFA loadings (not bifactor 
  loadings), then the function will conduct the requested kind of bifactor
  analysis and provide the bifactor loadings and statistics.

    \item
  The function will conduct the preliminary EFA, the
  bifactor analyses, and produce the bifactor loadings and statistics if either
  "rawdata" or "cormat" are provided, in which case "loadings" should be NULL.
}

  The factor extraction computations for the following methods are conducted
	using the psych package (Revelle, 2026): 'alpha', 'gls', 'minres', 'ols', 'uls', and 'wls'.
	
	The factor extraction computations for 'fullinfo' are conducted
	using the mirt package (Chalmers, 2012). Full-information methods are considered more
	appropriate for item-level data than other factor extraction methods (Wirth & Edwards, 2007).
		
	The factor rotation computations for the following methods are conducted
	using the GPArotation package (Bernaards & Jennrich, 2005, 2026): 'bentlerQ', 
	'bentlerT', 'entropy', 'geominQ', 'geominT', 'oblimax', 'oblimin', 'quartimax', 
	'quartimin', and 'simplimax'.
	
	The 'bifactorT', 'bifactorQ', 'bigeominQ', and 'bigeominQ' computations 
	are conducted using the GPArotation package (Bernaards & Jennrich, 2005, 2026). 
	The 'SL' and 'DSL' bifactor computations 
	are conducted using the psych package (Revelle, 2026). The 'SLiD' bifactor  
	computations are conducted using code from Garcia-Garzon et al. (2021).	
	
	The only possible extraction methods for 'SL', 'SLiD', and 'DSL' bifactor 
	computations are 'paf', 'minres', and 'ml'.	\cr
	
  For bifactor analyses (see Bornovalova, 2020; Garcia-Garzon et al., 2021; &
  Reise et al., 2018, for reviews):

	\itemize{
		
    \item\strong{bifactorT} is an orthogonal bifactor rotation 
    designed for situations where a single, overarching global dimension is expected
    alongside separate sub-domains, and when all factors should be 
    uncorrelated.
        
    \item\strong{bifactorQ} is an oblique bifactor rotation designed for when 
    a strong, overarching global dimension is expected alongside separate 
    sub-domains that are allowed to correlate with each other.

    \item\strong{bigeominT} is an orthogonal bifactor rotation 
    designed for situations where a single, overarching global dimension is expected
    alongside separate sub-domains, and when all factors should be 
    uncorrelated.
        
    \item\strong{bigeominQ} is an oblique bifactor rotation designed for when 
    a strong, overarching global dimension is expected alongside separate 
    sub-domains that are allowed to correlate with each other.

    \item\strong{SL} is an oblique bifactor rotation designed for 
    where there is a broad, overarching factor 
    alongside sub-domains that are allowed to overlap. 
    
    \item\strong{SLiD} an orthogonal bifactor rotation method designed for 
    exploratory bifactor analysis that tries to ensure that 
    once the general factor's variance is pulled out, each item loads 
    onto only one specific group factor. 
        
    \item\strong{DSL} entropy is an orthogonal factor rotation 
    that pushes factor loadings to be either 
    strongly dominant (close to 1.0) or cleanly absent (close to 0.0), 
    reducing the overall informational "noise" of the matrix. \cr
  }

Run one of the following commands for more detailed descriptions of the 
above extraction, rotation, and bifactor methods:

\itemize{

  \item{RShowDoc("EFA_BIFACTOR_vignettes", package = "EFA.dimensions")}

  \item{vignette("EFA_BIFACTOR_vignettes")}
}
}
\value{A list with the bifactor loadings and statistics, including omegas, ECV,
ARPB, FD, coef_H, and rmsr.
}
\references{
	  {Bernaards, C. A., & Jennrich, R. I. (2005). Gradient Projection Algorithms  
	  	and Software for Arbitrary Rotation Criteria in Factor Analysis. 
	  	\emph{Educational and Psychological Measurement, 65(5),} 676-696. 
	  	https://doi.org/10.1177/0013164404272507}
\cr\cr{Bernaards, C. A., & Jennrich, R. I. (2026). GPArotation: Gradient Projection Factor Rotation. 
	R package version 2026.4-1, https://CRAN.R-project.org/package=GPArotation}
\cr\cr {Bornovalova, M. A., Choate, A. M., Fatimah, H., Petersen, K. J., & 
  Wiernik, B. M. (2020). Appropriate Use of Bifactor Analysis in Psychopathology 
  Research: Appreciating Benefits and Limitations. 
  \emph{Biological psychiatry, 88(1),} 1827.}
\cr\cr {Chalmers, R. P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. 
   \emph{Journal of Statistical Software, 48(6),} 129. doi:10.18637/jss.v048.i06.}
\cr\cr {Garcia-Garzon, E., Abad, F. J., & Garrido, L. E. (2021). On omega  
        hierarchical estimation: A Comparison of Exploratory Bi-Factor Analysis
        Algorithms. \emph{Multivariate Behavioral Research, 56(1),} 101-119.}
\cr\cr{Jennrich, R. I. (2018). Rotation. In P. Irwing, T. Booth, & D. J. Hughes (Eds.), \emph{The Wiley handbook 
 	of psychometric testing: A multidisciplinary reference on survey, scale and test development} (pp. 279304). 
 	Wiley Blackwell. https://doi.org/10.1002/9781118489772.ch10}
\cr\cr{Mulaik, S. A. (2010). \emph{Foundations of factor analysis} (2nd ed.). Boca Raton, FL: Chapman 
	and Hall/CRC Press, Taylor & Francis Group.}
\cr\cr {Reise, S.P., Bonifay, W., & Haviland, M.G. (2018). Bifactor Modelling and 
  the Evaluation of Scale Scores. In P. Irwing, T. Booth& D.J. Hughes (eds.),
  \emph{The Wiley Handbook of Psychometric Testing}. John Wiley & Sons.}
\cr\cr{Revelle, W. (2026). psych: Procedures for Psychological, Psychometric, and Personality Research. 
	R package version 2.6.5, https://CRAN.R-project.org/package=psych}
\cr\cr {Sellbom, M., & Tellegen, A. (2019). Factor analysis in psychological assessment research: 
	Common pitfalls and recommendations. 
   \emph{Psychological Assessment, 31(12),} 1428-1441. https://doi.org/10.1037/pas0000623}
\cr\cr {Watts, A. L., Greene, A. L., Ringwald, W., Forbes, M. K., Brandes, C. M., Levin-Aspenson, 
	H. F., & Delawalla, C. (2023). Factor analysis in personality disorders research: Modern issues 
	and illustrations of practical recommendations. 
  \emph{Personality Disorders: Theory, Research, and Treatment, 14(1),} 105-117. 
  https://doi.org/10.1037/per0000581}
\cr\cr {Wirth, R. J., & Edwards, M. C. (2007). Item factor analysis: current approaches and future directions. 
  \emph{Psychological methods, 12(1),} 58-79. https://doi.org/10.1037/1082-989X.12.1.58}
}
\author{Brian P. O'Connor }
\examples{
# using only EFA loadings as input
efa_loadings <- EFA(data=data_RSE, Nfactors=3, extraction = 'ml', verbose=FALSE)$pattern

BIFACTOR(loadings = efa_loadings)  

\donttest{
# when a correlation matrix is provided, but not loadings or rawdata
BIFACTOR(cormat = cor(data_RSE), Ncases = 800, 
         bifactor_kind = 'SL', 
         EFA_options    = list(extraction = 'minres', rotation = 'oblimin', Nfactors = 3),
         schmid_options = list(extraction = 'minres', rotation = 'oblimin', N_group_factors = 3)) 
  
# when rawdata is provided, but not loadings or cormat
BIFACTOR(rawdata = data_RSE,   
         bifactor_kind = 'SL', 
         EFA_options = list(extraction = 'minres', rotation = 'oblimin', Nfactors = 3)) 
}
}
