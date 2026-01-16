\name{epipageShort}
\Rdversion{1.1}
\alias{epipageShort}
\docType{data}

\title{~ Data: epipageShort ~}

\description{
  A subset of the longitudinal study EPIPAGE.
}

\usage{data(epipageShort)}

\format{

  \describe{
    \item{\code{id}}{unique idenfier for each patient.}
    \item{\code{gender}}{Male or Female.}
    \item{\code{sdq3}}{ score of the Strengths and Difficulties Questionnaire at 3 years old.}
    \item{\code{sdq4}}{ score of the Strengths and Difficulties Questionnaire at 4 years old.}
    \item{\code{sdq5}}{ score of the Strengths and Difficulties Questionnaire at 5 years old.}
    \item{\code{sdq8}}{ score of the Strengths and Difficulties Questionnaire at 8 years old.}
  }
}

\details{
  The EPIPAGE cohort, funded by INSERM and the French general health
  authority, is a multi-regional French follow-up survey of severely
  premature children. It included more than 4000 children born at less
  than 33 weeks gestational age, and two control samples of children,
  respectively born at 33-34 weeks of gestational age and born full
  term. The general objectives were to study short and long term motor,
  cognitive and behavioural outcomes in these children, and to determine
  the impact of medical practice, care provision and organization of
  perinatal care, environment, family circle and living conditions on
  child health and development. About 2600 children born severely
  premature and 400 and 600 controls respectively were followed up to
  the age of 5 years and then to the age of 8.

  The SDQ is a behavioral questionnaire for children and adolescents
  ages 4 through 16 years old. It measures the severity of the
  disability (higher score indicate higher disability). 

  The database belongs to the INSERM unit U953 (P.Y. Ancel).
  which has agreed to include the variable SDQ in the library.
}

\references{
  \describe{
    \item{[lar08]}{Larroque B, Ancel P, Marret S, Marchand L, André M, Arnaud C, Pierrat V, Rozé J, Messer J, Thiriez
      G, et al. (2008). "Neurodevelopmental disabilities and special care of 5-year-old children born
      before 33 weeks of gestation (the EPIPAGE study): a longitudinal cohort study." The Lancet,
      371(9615), 813-820.}

    \item{[lau11]}{Laurent C, Kouanfack C, Laborde-Balen G, Aghokeng A, Mbougua J, Boyer S, Carrieri M, Mben J,
      Dontsop M, Kazé S, et al. (2011). "Monitoring of HIV viral loads, CD4 cell counts, and clinical
      assessments versus clinical monitoring alone for antiretroviral therapy in rural district hospitals
      in Cameroon (Stratall ANRS 12110/ESTHER): a randomised non-inferiority trial." The Lancet
      Infectious Diseases, 11(11), 825-833.}
  }
}

\examples{
data(epipageShort)
str(epipageShort)
}

\keyword{datasets}
\keyword{documentation}


