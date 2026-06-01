\name{cog}
\alias{cog}
\docType{data}
\title{Vascular Cognitive Impairment Data}
\description{
Lee, Nelder, and Pawitan (2016) considered the Vascular Cognitive Impairment
(VCI) data. As the elderly population increases, the proportion
of stroke, one of main geriatric diseases also rises. The increase of the
number of patients with stroke results in the rise of that of patients with
the VCI which cognitive function is declined due to stroke. Through an
early intervention for the VCI, the cognitive function can be improved.
They considered standardized VCI scores for four domains (response
variables): executive (y1), memory (y2), visuospatial (y3) and language
(y4). The purpose of the study is to examine the effects of ten demographic
and ten acute neuroimaging variables on the cognitive function
in the ischemic stroke patients.
}
\usage{data("cog")}
\format{
  A data frame with 1139 observations on the following 26 variables.
  \describe{
    \item{\code{x1}}{integer of age/10}
    \item{\code{x2}}{=1 (male), =0 (female)}
    \item{\code{x3}}{=0 (none), =1 (elementary) , =2 (middle), =3 (high), =4 (over college)}
    \item{\code{x4}}{=1 (hypertension), =0 (none)}
    \item{\code{x5}}{=1 (diabetes mellitus), =0 (none)}
    \item{\code{x6}}{=1 (atrial brillation), =0 (none)}
    \item{\code{x7}}{=1 (history of stroke), =0 (none)}
    \item{\code{x8}}{national institute of health stroke scale score at admission}
    \item{\code{x9}}{time interval from stroke onset to rst K-VCIHS-NP}
    \item{\code{x10}}{=1 (IQCODE>=3.6), =0 (otherwise)}
    \item{\code{x11}}{left or bilateral involvement}
    \item{\code{x12}}{lesion multiplicity in acute DWI imaging}
    \item{\code{x13}}{cortical involvement of acute lesions}
    \item{\code{x14}}{cortical involvement of chronic territorial infarction}
    \item{\code{x15}}{Periventricular white matter lesions (PVWM); =0 (PVWN 0, 1), =1 (PVWN 2, 3)}
    \item{\code{x16}}{Subcortical white matter lesions (SCWM); = 0 (SCWM 0, 1), = 1 (SCWM 2, 3)}
    \item{\code{x17}}{presence of lacunes}
    \item{\code{x18}}{presence of cerebral microbleeds}
    \item{\code{x19}}{Medial temporal lobe atrophy (MTA); = 1 (MTA 2), = 0 (not 2)}
    \item{\code{x20}}{= 1 (MTA 3, 4), = 0 (not 3, 4)}
    \item{\code{y1}}{executive}
    \item{\code{y2}}{memory}
    \item{\code{y3}}{visuospatial}
    \item{\code{y4}}{language}
    \item{\code{id}}{patients identifier}
  }
}
\references{
Lee, Y., Nelder, J.A. and Pawitan, Y. (2016). Generalized Linear Models
with Random Effects (2nd Edition). Chapman and Hall, Boca Raton, 2nd edition.
}

