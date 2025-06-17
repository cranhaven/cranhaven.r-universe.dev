\encoding{UTF-8}
\name{colonTP}
\alias{colonTP}
\docType{data}
\title{
  Chemotherapy for Stage B/C colon cancer
}
\description{
  These are data from one of the first successful trials of adjuvant chemotherapy for colon cancer. Levamisole is a low-toxicity compound previously used to treat worm infestations in animals; 5-FU is a moderately toxic (as these things go) chemotherapy agent. This data frame reproduces the information in the \code{colon} object of the package \code{survival}, organized in a slightly different way (only one row is used for each individual).
}
\usage{data("colonTP")}
\format{
  A data frame with 929 observations on the following 15 variables.
  \describe{
    \item{\code{time1}}{Disease free survival time (time to recurrence, death, or censoring, whichever occurs first).}
    \item{\code{event1}}{Disease free survival indicator (1=dead or relapsed, 0=alive disease free).}
    \item{\code{Stime}}{Time to death or censoring.}
    \item{\code{event}}{Death indicator (1=dead, 0=alive).}
    \item{\code{rx}}{Treatment - \code{Obs}(ervation), \code{Lev}(amisole), \code{Lev}(amisole)\code{+5-FU}.}
    \item{\code{sex}}{1=male.}
    \item{\code{age}}{Age in years.}
    \item{\code{obstruct}}{Obstruction of colon by tumour.}
    \item{\code{perfor}}{Perforation of colon.}
    \item{\code{adhere}}{Adherence to nearby organs.}
    \item{\code{nodes}}{Number of lymph nodes with detectable cancer.}
    \item{\code{differ}}{Differentiation of tumour (1=well, 2=moderate, 3=poor).}
    \item{\code{extent}}{Extent of local spread (1=submucosa, 2=muscle, 3=seros, 4=contiguous structures).}
    \item{\code{surg}}{Time from surgery to registration (0=short, 1=long).}
    \item{\code{node4}}{More than 4 positive lymph nodes.}
  }
}
\note{
  The study is originally described in Laurie et al. (1989). The main report is found in Moertel et al. (1990). This data set is closet to that of the final report in Moertel et al. (1991). A version of the data with less follow-up time was used in the paper by Lin (1994). This data set has been used for illustration purposes in a number of papers, including Meira-Machado et al. (2015) and de Uña-Álvarez and Meira-Machado (2015).
}
%%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%%}
\references{
  de Uña-Álvarez J. and Meira-Machado L. (2015). Nonparametric estimation of transition probabilities in a non-Markov illness-death model: a comparative study. \emph{Biometrics} \bold{71}, 364--375.
  
  Laurie J. A., Moertel C. G., Fleming T. R., Wieand H. S., Leigh J. E., Rubin J., McCormack G. W., Gerstner J. B., Krook J. E. and Malliard J. (1989). Surgical adjuvant therapy of large-bowel carcinoma: An evaluation of levamisole and the combination of levamisole and fluorouracil: The North Central Cancer Treatment Group and the Mayo Clinic. \emph{Journal of Clinical Oncology} \bold{7(10)}, 1447--1456.
  
  Lin D. Y. (1994). Cox regression analysis of multivariate failure time data: the marginal approach. \emph{Statistics in Medicine} \bold{13(21)}, 2233--2247.
  
  Meira-Machado L., de Uña-Álvarez J. and Datta S. (2015). Nonparametric estimation of conditional transition probabilities in a non-Markov illness-death model. Computational Statistics \bold{30}, 377--397.
  
  Moertel C. G., Fleming T. R., MacDonald J. S., Haller D. G., Laurie J. A., Goodman P. J., Ungerleider J. S., Emerson W. A., Tormey D. C., Glick J. H., Veeder M. H. and Maillard J. A. (1990). Levamisole and fluorouracil for adjuvant therapy of resected colon carcinoma. \emph{New England Journal of Medicine} \bold{322(6)}, 352--358.
  
  Moertel C. G., Fleming T. R., MacDonald J. S., Haller D. G., Laurie J. A., Tangen C. M., Ungerleider J. S., Emerson W. A., Tormey D. C., Glick J. H., Veeder M. H. and Maillard J. A. (1991). Fluorouracil plus Levamisole as an effective adjuvant therapy after resection of stage II colon carcinoma: a final report. \emph{Annals of Internal Medicine} \bold{122(5)}, 321--326.
}
\examples{
data(colonTP)
## maybe str(colonTP) ; plot(colonTP) ...
}
\keyword{datasets}
