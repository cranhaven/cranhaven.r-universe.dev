\name{labels}
\alias{labels}
\docType{data}
\title{
Labour Relation Taxonomy labels
}
\description{
This dataset is for internal use of the lar package only. It attaches the
official Labour Relations Taxonomy labels to data read in by the read.lar
function.}
\format{
  A data frame with 49 observations on the following 4 variables.
  \describe{
    \item{\code{lr.cat}}{a factor with levels \code{-1} \code{1} \code{10} \code{1012} \code{11} \code{12} \code{12013} \code{12013014} \code{12013014018} \code{12014} \code{12014018} \code{1201404} \code{12015} \code{1213} \code{13} \code{13012} \code{13014} \code{14} \code{14012} \code{14015} \code{14018} \code{142} \code{143} \code{15} \code{16} \code{17} \code{171} \code{172} \code{18} \code{181} \code{182} \code{183} \code{2} \code{3} \code{4} \code{4014} \code{405} \code{405012} \code{5} \code{5012014} \code{5014} \code{508014} \code{5a} \code{5b} \code{6} \code{7} \code{7018} \code{8} \code{9}}
    \item{\code{lr.txt.1}}{a factor with levels \code{Commodified} \code{EitherOr} \code{Non working} \code{Reciprocal} \code{Tributary} \code{Unknown}}
    \item{\code{lr.txt.2}}{a factor with levels \code{Community} \code{EitherOr} \code{Household} \code{Market} \code{Non-market} \code{Unknown}}
    \item{\code{lr.txt.3.ext}}{a factor with levels \code{Affluent (2)} \code{Cannot work (1)} \code{EitherOr} \code{Employers (13)} \code{Household} \code{Indent.lbr. (15)} \code{Indent.lbr. (9)} \code{Kin non-prod. (5b)} \code{Kin prod. (5)} \code{Kin prod. (5a)} \code{Leading prod. (4)} \code{Multiple} \code{Obligatory lbr. (8)} \code{Redist.lbr. (7)} \code{Self-Employed (12)} \code{Serfs (10)} \code{Serfs (16)} \code{Servants (6)} \code{Slaves (11)} \code{Slaves (17)} \code{Unemployed (3)} \code{Unknown} \code{Wage-Earners (14)} \code{Wage-Earners (18)}}
  }
}
\details{
Currently (lar package version 0.1), the labels still contain a label for labrel 5, but users should be aware that currently only labrels 5a and 5b should be used.
}
\source{
https://collab.iisg.nl/c/document_library/get_file?p_l_id=273223&folderId=277142&name=DLFE-186117.pdf}
\references{
International Institute of Social History. 2014. Codebook and manual for gathering and entering data in the database of the global collaboratory on the history of labour relations 1500-2000. Version 13, January 14}

\keyword{datasets}
