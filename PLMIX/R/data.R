#' American Psychological Association Data (partial orderings)
#'
#' The popular American Psychological Association dataset (\code{d_apa})
#' contains the results of the voting ballots of the 1980 presidential
#' election. A total of \eqn{N=15449} voters ranked a maximum of \eqn{K=5}
#' candidates, conventionally classified as research psychologists (candidate 1
#' and 3), clinical psychologists (candidate 4 and 5) and community
#' psychologists (candidate 2). The winner of the election was candidate 3. The
#' dataset is composed of partial top orderings of varying lengths. Missing
#' positions are denoted with zero entries.
#'
#'
#' @name d_apa
#' @docType data
#' @format Object of S3 class \code{c("top_ordering","matrix")} gathering a
#' matrix of partial orderings with \eqn{N=15449} rows and \eqn{K=5} columns
#' Each row lists the candidates from the most-liked (\code{Rank_1}) to the
#' least-liked (\code{Rank_5}) in a given voting ballot.
#' @references Mollica, C. and Tardella, L. (2017). Bayesian Plackett-Luce
#' mixture models for partially ranked data. \emph{Psychometrika},
#' \bold{82}(2), pages 442--258, ISSN: 0033-3123, <doi:
#' 10.1007/s11336-016-9530-0>.
#'
#' Diaconis, P. W. (1988). Group representations in probability and statistics.
#' \emph{Lecture Notes-Monograph Series}, pages 94--96.
#'
#' Diaconis, P. W. (1987). Spectral analysis for ranked data. Technical Report
#' 282, Dept of Statistics, Stanford University.
#' @keywords datasets
#' @examples
#'
#' data(d_apa)
#' head(d_apa)
#'
#' ## Subset of complete sequences
#' d_apa_compl=d_apa[rowSums(d_apa!=0)>=(ncol(d_apa)-1),]
#' head(d_apa_compl)
#'
NULL





#' Car Configurator Data (partial orderings)
#'
#' The Car Configurator dataset (\code{d_carconf}) came up from a marketing
#' study aimed at investigating customer preferences toward different car
#' features. A sample of \eqn{N=435} customers were asked to construct their
#' car by using an online configurator system and choose among \eqn{K=6} car
#' modules in order of preference. The car features are labeled as: 1 = price,
#' 2 = exterior design, 3 = brand, 4 = technical equipment, 5 = producing
#' country and 6 = interior design. The survey did not require a complete
#' ranking elicitation, therefore the dataset is composed of partial top
#' orderings of varying lengths. Missing positions are denoted with zero
#' entries.
#'
#'
#' @name d_carconf
#' @docType data
#' @format Object of S3 class \code{c("top_ordering","matrix")} gathering a
#' matrix of partial orderings with \eqn{N=435} rows and \eqn{K=6} columns.
#' Each row lists the car features from the most important (\code{Rank_1}) to
#' the least important (\code{Rank_6}) for a given customer.
#' @references Mollica, C. and Tardella, L. (2017). Bayesian Plackett-Luce
#' mixture models for partially ranked data. \emph{Psychometrika},
#' \bold{82}(2), pages 442--458, ISSN: 0033-3123, <doi:
#' 10.1007/s11336-016-9530-0>.
#'
#' Hatzinger, R. and Dittrich, R. (2012). Prefmod: An R package for modeling
#' preferences based on paired comparisons, rankings, or ratings. \emph{Journal
#' of Statistical Software}, \bold{48}(10), pages 1--31.
#'
#' Dabic, M. and Hatzinger, R. (2009). Zielgruppenadaequate Ablaeufe in
#' Konfigurationssystemen - eine empirische Studie im Automobilmarkt - Partial
#' Rankings. In Hatzinger, R., Dittrich, R. and Salzberger, T. (eds),
#' \emph{Praeferenzanalyse mit R: Anwendungen aus Marketing, Behavioural
#' Finance und Human Resource Management}. Wien: Facultas.
#' @keywords datasets
#' @examples
#'
#' data(d_carconf)
#' head(d_carconf)
#'
#' ## Subset of complete sequences
#' d_carconf_compl=d_carconf[rowSums(d_carconf!=0)>=(ncol(d_carconf)-1),]
#' head(d_carconf_compl)
#'
NULL





#' Dublin West Data (partial orderings)
#'
#' The Dublin West dataset (\code{d_dublinwest}) contains the results of the
#' voting ballots of the 2002 Irish general election from the Dublin West
#' constituency. The Irish voting system allows voters to rank the candidates
#' in order of preferences, rather than only specify the favorite one. In the
#' Dublin West constituency, \eqn{N=29988} voters ranked a maximum of \eqn{K=9}
#' candidates, labeled as: 1 = Bonnie R., 2 = Burton J., 3 = Doherty-Ryan D., 4
#' = Higgins J., 5 = Lenihan B., 6 = McDonald M., 7 = Morrissey T., 8 = Smyth
#' J. and 9 = Terry S.. The dataset is composed of partial top orderings of
#' varying lengths. Missing positions are denoted with zero entries.
#'
#'
#' @name d_dublinwest
#' @docType data
#' @format Object of S3 class \code{c("top_ordering","matrix")} gathering a
#' partial orderings with \eqn{N=29988} rows and \eqn{K=9} columns. Each row
#' lists the candidates from the most-liked (\code{Rank_1}) to the least-liked
#' (\code{Rank_9}) in a given voting ballot.
#' @references Mattei, N. and Walsh, T. (2013) PrefLib: A Library of Preference
#' Data. \emph{Proceedings of Third International Conference on Algorithmic
#' Decision Theory} (ADT 2013). Springer, Lecture Notes in Artificial
#' Intelligence, November 13-15, 2013.
#'
#' Gormley, I. C. and Murphy, T. B. (2009). A grade of membership model for
#' rank data. \emph{Bayesian Analysis}, \bold{4}(2), pages 65--295.
#'
#' Gormley, I. C. and Murphy, T. B. (2008). Exploring Voting Blocs Within the
#' Irish Electorate: A Mixture Modeling Approach. \emph{Journal of the America
#' Statistical Association}, \bold{103}(483), pages 1014--1027.
#' @source The 2002 Dublin West data have been downloaded from
#' \url{https://preflib.github.io/PrefLib-Jekyll/} PrefLib: A Library for Preferences. In that
#' repository, preferences with ties are also included. The original source was
#' publicly available from the Dublin County Returning Officer at the following
#' URL: \url{https://dublincountyreturningofficer.com/}.
#' @keywords datasets
#' @examples
#'
#' data(d_dublinwest)
#' head(d_dublinwest)
#'
#' ## Subset of complete sequences
#' d_dublinwest_compl=d_dublinwest[rowSums(d_dublinwest!=0)>=(ncol(d_dublinwest)-1),]
#' head(d_dublinwest_compl)
#'
NULL





#' Gaming Platforms Data (complete orderings)
#'
#' The Gaming Platforms dataset (\code{d_gaming}) collects the results of a
#' survey conducted on a sample of \eqn{N=91} Dutch students, who were asked to
#' rank \eqn{K=6} gaming platforms in order of preference, namely: 1 = X-Box, 2
#' = PlayStation, 3 = PSPortable, 4 = GameCube, 5 = GameBoy and 6 = Personal
#' Computer. The dataset is composed of complete orderings.
#'
#'
#' @name d_gaming
#' @docType data
#' @format Object of S3 class \code{c("top_ordering","matrix")} gathering a
#' matrix of complete orderings with \eqn{N=91} rows and \eqn{K=6} columns.
#' Each row lists the gaming platforms from the most-liked (\code{Rank_1}) to
#' the least-liked (\code{Rank_6}) for a given student.
#' @references Fok, D., Paap, R. and Van Dijk, B. (2012). A Rank-Ordered Logit
#' Model With Unobserved Heterogeneity In Ranking Capatibilities. \emph{Journal
#' of Applied Econometrics}, \bold{27}(5), pages 831--846.
#' @source The Gaming Platforms dataset in .csv format can be downloaded from
#' \url{http://qed.econ.queensu.ca/jae/2012-v27.5/fok-paap-van_dijk/}. The .csv
#' files contains the preference data in ranking format and some covariates
#' collected for each student.
#' @keywords datasets
#' @examples
#'
#' data(d_gaming)
#' head(d_gaming)
#'
NULL





#' German Sample Data (complete orderings)
#'
#' The German Sample dataset (\code{d_german}) is part of a comparative
#' cross-sectional study on political actions and mass participation involving
#' five Western countries. The dataset regards a sample of \eqn{N=2262} German
#' respondents who were asked to rank \eqn{K=4} political goals in order of
#' desirability, namely: 1 = maintaining order in the nation, 2 = giving people
#' more say in the decisions of government, 3 = fighting rising prices and 4 =
#' protecting freedom of speech. The dataset is composed of complete orderings.
#'
#'
#' @name d_german
#' @docType data
#' @format Object of S3 class \code{c("top_ordering","matrix")} gathering a
#' matrix of complete orderings with \eqn{N=2262} rows and \eqn{K=4} columns.
#' Each row lists the political goals from the most desiderable (\code{Rank_1})
#' to the least desiderable (\code{Rank_4}) for a given respondent.
#' @references Croon, M. A. (1989). Latent class models for the analysis of
#' rankings. In De Soete, G., Feger, H. and Klauer, K. C. (eds), \emph{New
#' Developments in Psychological Choice Modeling}, pages 99--121.
#' North-Holland: Amsterdam.
#'
#' Barnes, S. H. et al. (1979). Political action. Mass participation in five
#' Western democracies. London: Sage.
#' @keywords datasets
#' @examples
#'
#' data(d_german)
#' head(d_german)
#'
NULL





#' NASCAR Data (partial orderings)
#'
#' The NASCAR dataset (\code{d_nascar}) collects the results of the 2002 season
#' of stock car racing held in the United States. The 2002 championship
#' consisted of \eqn{N=36} races, with 43 car drivers competing in each race. A
#' total of \eqn{K=87} drivers participated in the 2002 season, taking part to
#' a variable number of races: some of them competed in all the races, some
#' others in only one. The results of the entire 2002 season were collected in
#' the form of top-43 orderings, where the position of the not-competing
#' drivers in each race is assumed lower than the 43th, but undetermined.
#' Missing positions are denoted with zero entries.
#'
#'
#' @name d_nascar
#' @docType data
#' @format Object of S3 class \code{c("top_ordering","matrix")} gathering a
#' matrix of partial orderings with \eqn{N=36} rows and \eqn{K=87} columns.
#' Each row lists the car drivers from the top position (\code{Rank_1}) to the
#' bottom one (\code{Rank_87}) in a given race. Columns from the 44th to the
#' 87th are filled with zeros, because only 43 drivers competed in each race.
#' @references Caron, F. and Doucet, A. (2012). Efficient Bayesian inference
#' for Generalized Bradley-Terry models. \emph{J. Comput. Graph. Statist.},
#' \bold{21}(1), pages 174--196.
#'
#' Guiver, J. and Snelson, E. (2009). Bayesian inference for Plackett-Luce
#' ranking models. In Bottou, L. and Littman, M., editors, \emph{Proceedings of
#' the 26th International Conference on Machine Learning - ICML 2009}, pages
#' 377--384. Omnipress.
#'
#' Hunter, D. R. (2004). MM algorithms for Generalized Bradley-Terry models.
#' \emph{Ann. Statist.}, \bold{32}(1), pages 384--406.
# @source The NASCAR dataset in the MATLAB format used by Hunter, D. R. (2004)
# can be downloaded from
# \url{http://sites.stat.psu.edu/~dhunter/code/btmatlab/}. At the same link, a
# .xls file with drivers' names is also available.
#' @keywords datasets
#' @examples
#'
#' data(d_nascar)
#' head(d_nascar)
#'
#' ## Compute the number of races for each of the 87 drivers
#' table(c(d_nascar[,1:43]))
#'
#' ## Identify drivers arrived last (43th position) in all the races
#' which(colSums(rank_summaries(d_nascar, format="ordering")$marginals[1:42,])==0)
#'
#' ## Obscure drivers 84, 85, 86 and 87 to get the reduced dataset
#' ## with 83 racers employed by Hunter, D. R. (2004)
#' d_nascar_hunter=d_nascar[,1:83]
#' d_nascar_hunter[is.element(d_nascar_hunter,84:87)]=0
#'
NULL





#' Occupation Data (complete orderings)
#'
#' The Occupation dataset (\code{d_occup}) came up from a survey conducted on
#' graduates from the Technion-Insrael Institute of Tecnology. A sample of
#' \eqn{N=143} graduates were asked to rank \eqn{K=10} professions according to
#' the perceived prestige. The occupations are labeled as: 1 = faculty member,
#' 2 = owner of a business, 3 = applied scientist, 4 = operations researcher, 5
#' = industrial engineer, 6 = manager, 7 = mechanical engineer, 8 = supervisor,
#' 9 = technician and 10 = foreman. The dataset is composed of complete
#' orderings.
#'
#'
#' @name d_occup
#' @docType data
#' @format Object of S3 class \code{c("top_ordering","matrix")} gathering a
#' matrix of complete orderings with \eqn{N=143} rows and \eqn{K=10} columns.
#' Each row lists the professions from the most-liked (\code{Rank_1}) to the
#' least-liked (\code{Rank_10}) for a given graduate.
#' @references
#'
#' Cohen, A. and Mallows, C. L. (1983). Assessing goodness of fit of ranking
#' models to data. \emph{Journal of the Royal Statistical Society: Series D
#' (The Statistician)}, \bold{32}(4), pages 361--374, ISSN: 0039-0526.
#'
#' Cohen, A. (1982). Analysis of large sets of ranking data.
#' \emph{Communications in Statistics -- Theory and Methods}, \bold{11}(3),
#' pages 235--256.
#'
#' Goldberg, A. I. (1976). The relevance of cosmopolitan/local orientations to
#' professional values and behavior. \emph{Sociology of Work and Occupations},
#' \bold{3}(3), pages 331--356.
#' @keywords datasets
#' @examples
#'
#' data(d_occup)
#' head(d_occup)
#'
NULL





#' Rice Voting Data (partial orderings)
#'
#' The Rice Voting dataset (\code{d_rice}) collects the results of the 1992
#' election of a faculty member to serve on the Presidential Search Committee
#' in the Rice University. A total of \eqn{N=300} people casted their vote in
#' the ballots by ranking the \eqn{K=5} candidates in the short list in a
#' preferential manner. The dataset is composed of partial top orderings of
#' varying lengths. Missing positions are denoted with zero entries.
#'
#'
#' @name d_rice
#' @docType data
#' @format Object of S3 class \code{c("top_ordering","matrix")} gathering a
#' matrix of partial orderings with \eqn{N=300} rows and \eqn{K=5} columns.
#' Each row lists the faculty members from the most-liked (\code{Rank_1}) to
#' the least-liked (\code{Rank_5}) in a given voting ballot.
#' @references Marcus, P., Heiser, W. J. and D'Ambrosio, A. (2013). Comparison
#' of heterogeneous probability models for ranking data, Master Thesis, Leiden
#' University.
#'
#' Baggerly, K. A. (1995). Visual estimation of structure in ranked data, PhD
#' thesis, Rice University.
#' @keywords datasets
#' @examples
#'
#' data(d_rice)
#' head(d_rice)
#'
#' ## Subset of complete sequences
#' d_rice_compl=d_rice[rowSums(d_rice!=0)>=(ncol(d_rice)-1),]
#' head(d_rice_compl)
#'
#'
NULL





#' Bayesian Analysis of Finite Mixtures of Plackett-Luce Models for Partial
#' Rankings/Orderings
#'
#' The \pkg{PLMIX} package for R provides functions to fit and analyze finite
#' mixtures of Plackett-Luce models for partial top rankings/orderings within
#' the Bayesian framework. It provides MAP point estimates via EM algorithm and
#' posterior MCMC simulations via Gibbs Sampling. It also fits MLE as a special
#' case of the noninformative Bayesian analysis with vague priors.
#'
#' In addition to inferential techniques, the package assists other fundamental
#' phases of a model-based analysis for partial rankings/orderings, by
#' including functions for data manipulation, simulation, descriptive summary,
#' model selection and goodness-of-fit evaluation.
#'
#' Specific S3 classes and methods are also supplied to enhance the usability
#' and foster exchange with other packages. Finally, to address the issue of
#' computationally demanding procedures typical in ranking data analysis,
#' \pkg{PLMIX} takes advantage of a hybrid code linking the R environment with
#' the C++ programming language.
#'
#' The Plackett-Luce model is one of the most popular and frequently applied
#' parametric distributions to analyze partial top rankings/orderings of a
#' finite set of items. The present package allows to account for unobserved
#' sample heterogeneity of partially ranked data with a model-based analysis
#' relying on Bayesian finite mixtures of Plackett-Luce models. The package
#' provides a suite of functions that covers the fundamental phases of a
#' model-based analysis:
#'
#' \strong{Ranking data manipulation} 
#' \describe{
#' \item{\code{binary_group_ind}}{Binary group membership matrix from
#' the mixture component labels.} 
#' \item{\code{freq_to_unit}}{From the
#' frequency distribution to the dataset of individual orderings/rankings.}
#' \item{\code{make_complete}}{Random completion of partial
#' orderings/rankings data.} 
#' \item{\code{make_partial}}{Censoring of
#' complete orderings/rankings data.} 
#' \item{\code{rank_ord_switch}}{From
#' rankings to orderings and vice-versa.}
#' \item{\code{unit_to_freq}}{From the dataset of individual
#' orderings/rankings to the frequency distribution.} 
#' } 
#' \strong{Ranking data simulation} 
#' \describe{ 
#' \item{\code{rPLMIX}}{Random sample from a
#' finite mixture of Plackett-Luce models.} 
#' } 
#' \strong{Ranking data description}
#' \describe{ 
#' \item{\code{paired_comparisons}}{Paired comparison
#' frequencies.} 
#' \item{\code{rank_summaries}}{Summary statistics of
#' partial ranking/ordering data.} 
#' } 
#' \strong{Model estimation} 
#' \describe{
#' \item{\code{gibbsPLMIX}}{Bayesian analysis with MCMC posterior
#' simulation via Gibbs sampling.} 
#' \item{\code{label_switchPLMIX}}{Label
#' switching adjustment of the Gibbs sampling simulations.}
#' \item{\code{likPLMIX}}{Likelihood evaluation for a mixture of
#' Plackett-Luce models.} 
#' \item{\code{loglikPLMIX}}{Log-likelihood
#' evaluation for a mixture of Plackett-Luce models.}
#' \item{\code{mapPLMIX}}{MAP estimation via EM algorithm.}
#' \item{\code{mapPLMIX_multistart}}{MAP estimation via EM algorithm
#' with multiple starting values.} 
#' } 
#' \strong{Class coercion and membership}
#' \describe{ 
#' \item{\code{as.top_ordering}}{Coercion into top-ordering
#' datasets.} 
#' \item{\code{gsPLMIX_to_mcmc}}{From the Gibbs sampling
#' simulation to an MCMC class object.}
#' \item{\code{is.top_ordering}}{Test for the consistency of input data
#' with a top-ordering dataset.} 
#' } 
#' \strong{S3 class methods} 
#' \describe{
#' \item{\code{plot.gsPLMIX}}{Plot of the Gibbs sampling simulations.}
#' \item{\code{plot.mpPLMIX}}{Plot of the MAP estimates.}
#' \item{\code{print.gsPLMIX}}{Print of the Gibbs sampling simulations.}
#' \item{\code{print.mpPLMIX}}{Print of the MAP estimation algorithm.}
#' \item{\code{summary.gsPLMIX}}{Summary of the Gibbs sampling procedure.} 
#' \item{\code{summary.mpPLMIX}}{Summary of the MAP estimation.} 
#' } 
#' \strong{Model selection} \describe{
#' \item{\code{bicPLMIX}}{BIC value for the MLE of a mixture of
#' Plackett-Luce models.} 
#' \item{\code{selectPLMIX}}{Bayesian model selection criteria.} 
#' } 
#' \strong{Model assessment} 
#' \describe{
#' \item{\code{ppcheckPLMIX}}{Posterior predictive diagnostics.}
#' \item{\code{ppcheckPLMIX_cond}}{Posterior predictive diagnostics
#' conditionally on the number of ranked items.} 
#' } 
#' \strong{Datasets} 
#' \describe{
#' \item{\code{d_apa}}{American Psychological Association Data (partial
#' orderings).} 
#' \item{\code{d_carconf}}{Car Configurator Data (partial
#' orderings).} 
#' \item{\code{d_dublinwest}}{Dublin West Data (partial
#' orderings).} 
#' \item{\code{d_gaming}}{Gaming Platforms Data (complete
#' orderings).} 
#' \item{\code{d_german}}{German Sample Data (complete
#' orderings).} 
#' \item{\code{d_nascar}}{NASCAR Data (partial orderings).}
#' \item{\code{d_occup}}{Occupation Data (complete orderings).}
#' \item{\code{d_rice}}{Rice Voting Data (partial orderings).} 
#' } 
#' Data have to be supplied as an object of class \code{matrix}, where missing
#' positions/items are denoted with zero entries and Rank = 1 indicates the
#' most-liked alternative. For a more efficient implementation of the methods,
#' partial sequences with a single missing entry should be preliminarily filled
#' in, as they correspond to complete rankings/orderings. In the present
#' setting, ties are not allowed. Some quantities frequently recalled in the
#' manual are the following: \describe{ 
#' \item{\eqn{N}}{Sample size.}
#' \item{\eqn{K}}{Number of possible items.}
#' \item{\eqn{G}}{Number of mixture components.}
#' \item{\eqn{L}}{Size of the final posterior MCMC sample (after
#' burn-in phase).} }
#'
# @name data
# @aliases data PLMIX
# @docType package
#' @name PLMIX
#' @keywords package
#' @author Cristina Mollica and Luca Tardella
#'
#' Maintainer: Cristina Mollica <cristina.mollica@@uniroma1.it>
#' @references Mollica, C. and Tardella, L. (2017). Bayesian Plackett-Luce
#' mixture models for partially ranked data. \emph{Psychometrika},
#' \bold{82}(2), pages 442--458, ISSN: 0033-3123,
#' <doi:10.1007/s11336-016-9530-0>.
#'
#' Mollica, C. and Tardella, L. (2014). Epitope profiling via mixture modeling
#' for ranked data. \emph{Statistics in Medicine}, \bold{33}(21), pages
#' 3738--3758, ISSN: 0277-6715,
#' <doi:10.1002/sim.6224/full>.
#NULL
"_PACKAGE"


