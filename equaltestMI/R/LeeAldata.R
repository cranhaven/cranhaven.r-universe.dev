#' Lee and Al Otaiba (2015) early literacy skills in four socioeconomic groups
#'
#' This data set contains means and covariance matrices of early literacy skills measured in four different sociodemographic groups. The results are reported in Table 1 of Lee and Al Otaiba (2015).
#'
#' Six variables used in Lee and Al Otaiba (2015) to measure literacy constructs, including (1) letter-name fluency, (2) letter-sound fluency, (3) blending, (4) elision, (5) real words spelling, and (6) pseudo-words spelling.
#'
#' Following from Snow’s (2006) definition of componential skills and the work of Schatschneider, Fletcher, Francis, Carlson, and Foorman (2004) on National Early Literacy Panel (NELP), the six variables aim to measure three aspects of literacy constructs: (1) alphabet knowledge, (2) phonological awareness, and (3) spelling.
#'
#' The four sociodemographic groups are: (1) boys who are ineligible for FRL (n=78); (2) boys who are eligible for FRL (n=65); (3) girls who are ineligible for FRL (n=175); and (4) girls who are eligible for FRL (n=165).
#'
#' @usage data(LeeAlOtaiba)
#'
#' @format A list of four data frames. Every data frame is of dimension 7 by 6. The first row contains sample means of the six variables. The next six rows contain sample covariance matrix. The colomn names of the data frame are the names of the six variables. The row names of the data frame contain a label 'mean' and the names of the six variables.
#'
#' @keywords LeeAlOtaiba
#'
#' @references Lee, J. A. C., & Al Otaiba, S. (2015). Socioeconomic and gender group differences in early literacy skills: A multiple-group confirmatory factor analysis approach. Educational Research and Evaluation, 21(1), 40–59. https://doi.org/10.1080/13803611.2015.1010545
#'
#' @examples
#' data(LeeAlOtaiba)
#' # If one wants to extract the two groups used in the paper:
#' Group1 <- LeeAlOtaiba[[1]]
#' Group2 <- LeeAlOtaiba[[2]]
"LeeAlOtaiba"


