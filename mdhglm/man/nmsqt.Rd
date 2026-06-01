\name{nmsqt}
\alias{nmsqt}
\docType{data}
\title{
National Merit Scholarship Qualifying Test Data for Twins
}
\description{
Loehlin and Nichols (1976) presented the national merit twins data including
extensive questionnaires from 839 adolescent twins. The twins
were identified among the roughly 600,000 US high school juniors who
took the national merit scholarship qualifying test (NMSQT) in 1962.
They were diagnosed as identical (509 pairs) or same-sex fraternal (330
pairs) by a brief mail questionnaire and later completed a 1082-item
questionnaire covering a variety of behaviors, attitudes, personality, life
experiences, health, vocational preferences, etc., plus the 480-item California
psychological inventory. Twins' scores on the NMSQT and their five subscales are also included. 
The 285-item questionnaire filled out by
the parent was mainly focused on the life histories and experiences of
the twins.
}
\usage{data("nmsqt")}
\format{
  A data frame with 1536 observations on the following 11 variables.
  \describe{
    \item{\code{number}}{data number for 1536 obervations}
    \item{\code{pairnum}}{twin number for 768 pairs}
    \item{\code{x1}}{=0 (male), =1 (female)}
    \item{\code{x2}}{mother's educational level; =1 (less than or equal to 8th grade), =2 (part high school),
=3 (high school grad), =4 (part college), =5 (college grad), =6 (graduate degree)}
    \item{\code{x3}}{father's educational level; =1 (less than or equal to 8th grade), =2 (part high school),
=3 (high school grad), =4 (part college), =5 (college grad), =6 (graduate degree)}
    \item{\code{x4}}{family income level; =1 (less than equal to $5000), =2 ($5000 to $7499),
=3 ($7500 to $9999), =4 ($10000 to $14999), =5 ($15000 to $19999), =6 ($20000 to $24999), =7 (more than $25000).}
    \item{\code{y1}}{NMSQT score for English (0-100)}
    \item{\code{y2}}{NMSQT score mathematics (0-100)}
    \item{\code{y3}}{NMSQT score social science (0-100)}
    \item{\code{y4}}{NMSQT score natural science (0-100)}
    \item{\code{x5}}{zygosity; =0 (identical), =1 (fraternal)}
  }
}
\references{
Loehlin, J.C. and Nichols, R.C. (1976). Heredity, Environments and
Personality: A Study of 850 Twins. University of Texas Press, Austin.
}

