\name{condat}
\docType{data}
\alias{condat}
\title{Data on DTP and convulsions}
\description{
The data comprise ages in days at measles, mumps and rubella (MMR) vaccination, Haemophilus influenzae type b (Hib) booster or catch-up vaccination,  and febrile convulsion. There are 2435 convulsions in 2201 children. The ages have been jittered.
}
\usage{condat}
\format{A data frame containing 2435 rows and 11 columns. The column names are 'case' (individual identifier), 'sta' (age on first day of the observation period), 'end' (age on last day of the observation period), 'conv' (age at convulsion),  'hib' (age at Hib booster/catch-up vaccination), 'mmr' (age at MMR vaccination), 'sex' (1 for males, 2 for females), 'gap' (days from convulsion to next convulsion within the same case, or to end of observation), 'cen' (0 if last admission for this case, 1 otherwise), 'rec' (within-case event number), 'ngrp' (1 if case has a unique event, 2 if case has 2+ events).
}
%\source{}
\references{Farrington P., Whitaker H., and Ghebremichael-Weldeselassie Y. (2018). Self-controlled Case Series Studies: A modelling Guide with R. Boca Raton: Chapman & Hall/CRC Press. }
\keyword{datasets}
