\name{covid19_control}
\alias{covid19_control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    COVID19 Parameters Control Data Structure
}
\description{
    The COVID19 model uses parameters which are specific to the COVID19 model. This data structure affirms that required parameters are supplied, provides default values for optional parameters, and validates the data of each parameter.
}
\usage{
    covid19_control(
        input_N_pops=NULL,
        input_S_pops=NULL,
        input_E_pops=NULL,
        input_I_asym_pops=NULL,
        input_I_presym_pops=NULL,
        input_I_sym_pops=NULL,
        input_I_home_pops=NULL,
        input_I_hosp_pops=NULL,
        input_I_icu1_pops=NULL,
        input_I_icu2_pops=NULL,
        input_R_pops=NULL,
        input_D_pops=NULL,
        frac_beta_asym=0.55,
        frac_beta_hosp=0.05,
        delta=1/3.0,
        recov_a=1/6.0,
        recov_p=1/2.0,
        recov_s=1/6.0,
        recov_home=1/3.0,
        recov_icu1=1/8.0,
        recov_icu2=1/4.0,
        asym_rate=0.40,
        sym_to_icu_rate=0.015
    )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{input_N_pops}{
        Integer Vector representing the total population for each county.
    }
    \item{input_S_pops}{
        Integer Vector representing the susceptible population for each county.
    }
    \item{input_E_pops}{
        Integer Vector representing the exposed population for each county.
    }
    \item{input_I_asym_pops}{
        Integer Vector representing the asymptomatic infected population for each county.
    }
    \item{input_I_presym_pops}{
        Integer Vector representing the presymptomatic infected population for each county.
    }
    \item{input_I_sym_pops}{
        Integer Vector representing the symptomatic infected population for each county.
    }
    \item{input_I_home_pops}{
        Integer Vector representing the infected and isolated at home population for each county.
    }
    \item{input_I_hosp_pops}{
        Integer Vector representing the hospitalized population for each county.
    }
    \item{input_I_icu1_pops}{
        Integer Vector representing the population for each county in ICU.
    }
    \item{input_I_icu2_pops}{
        Integer Vector representing the population for each county in ICU recovery.
    }
    \item{input_R_pops}{
        Integer Vector representing the recovered population for each county.
    }
    \item{input_D_pops}{
        Integer Vector representing the deaths for each county.
    }
    \item{frac_beta_asym}{
        An adjustment to beta accounting for asymptomatic individuals being less likely to transmit than symptomatic individuals. Default value is 0.55. Must be greater than zero and less than or equal to one.
    }
    \item{frac_beta_hosp}{
        An adjustment to beta accounting for hospitalized individuals being less likely to transmit than non-hospitalized individuals. Default value is 0.05. Must be greater than zero and less than or equal to one.
    }
    \item{delta}{
        Incubation period. Default value is 1/3.0. Must be greater than or equal to zero. Values above one are unusual.
    }
    \item{recov_a}{
        Recovery rate of asymptomatic individuals. Default value is 1/6.0. Must be greater than or equal to zero. Values above one are unusual.
    }
    \item{recov_p}{
        Recovery rate of presymptomatic individuals. Default value is 1/2.0. Must be greater than or equal to zero. Values above one are unusual.
    }
    \item{recov_s}{
        Recovery rate of symptomatic individuals. Default value is 1/6.0. Must be greater than or equal to zero. Values above one are unusual.
    }
    \item{recov_home}{
        Recovery rate of infected individuals in home isolation. Default value is 1/3.0. Must be greater than or equal to zero. Values above one are unusual.
    }
    \item{recov_icu1}{
        Recovery rate of individuals in ICU. Default value is 1/8.0. Must be greater than or equal to zero. Values above one are unusual.
    }
    \item{recov_icu2}{
        Recovery rate of individuals in ICU recovery. Default value is 1/4.0. Must be greater than or equal to zero. Values above one are unusual.
    }
    \item{asym_rate}{
        Proportion of presymtomatic individuals entering the asymptomatic stage. Default value is 0.40. Must be greater than zero and less than or equal to one.
    }
    \item{sym_to_icu_rate}{
        Proportion of symptomatic individuals entering ICU. Default value is 0.015. Must be greater than zero and less than or equal to one.
    }
}
\details{
    Defines a set of parameters specific to the COVID19 model. Adjustments to the model calculations can be made by specifying any or all of the optional parameters. If an optional parameter is not set, it will use the default value as specified above. If a parameter is outside the specified limits, execution will stop and an error message will be displayed. Some parameters may have values greater than one. While these situations may be unusual, execution will not stop but a warning message will be displayed.

    Note: At least one of \code{input_N_pops} or \code{input_S_pops} must be supplied. If one is not supplied, it will be calculated within the class.

    Note: At least one of \code{input_E_pops}, \code{input_I_asym_pops}, \code{input_I_presym_pops}, \code{input_I_sym_pops}, \code{input_I_home_pops}, \code{input_I_hosp_pops}, \code{input_I_icu1_pops}, and \code{input_I_icu2_pops} must be supplied with a nonzero population. Any of these population parameters not supplied will be assumed to be a vector of zeroes.
}
\value{
    Returns a named list of vectors that must be supplied to \code{\link{model_interface}}.
}
\author{
Seth Borkovec, \email{stb224@nau.edu}; Joseph Mihaljevic, \email{joseph.mihaljevic@nau.edu}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    \code{\link{seir_control}}
}
\examples{

## Data set for the examples:
N_pops <- rep(100000, 10)
E_pops <- c(0, 1, 0, 3, 2, 0, 14, 3, 0, 0)
S_pops <- N_pops - E_pops
I_asym_pops <- rep(0, 10)
I_presym_pops <- rep(0, 10)
I_sym_pops <- c(1, 0, 11, 0, 0, 5, 0, 0, 9, 0)
I_home_pops <- rep(0, 10)
I_hosp_pops <- rep(0, 10)
I_icu1_pops <- rep(0, 10)
I_icu2_pops <- rep(0, 10)
R_pops <- rep(0, 10)
D_pops <- rep(0, 10)

## Example using the default parameters:
covid19_control <- covid19_control(input_S_pops         = S_pops,
                                   input_E_pops         = E_pops)

## Example specifying some parameters:
covid19_control <- covid19_control(input_N_pops         = N_pops,
                                   input_I_sym_pops     = I_sym_pops,
                                   input_I_home_pops    = I_home_pops,
                                   input_I_hosp_pops    = I_hosp_pops,
                                   input_I_icu2_pops    = I_icu2_pops,
                                   input_D_pops         = D_pops,
                                   frac_beta_hosp       = 0.03)

## Example specifying all possible parameters:
covid19_control <- covid19_control(input_S_pops         = S_pops,
                                   input_E_pops         = E_pops,
                                   input_I_asym_pops    = I_asym_pops,
                                   input_I_presym_pops  = I_presym_pops,
                                   input_I_sym_pops     = I_sym_pops,
                                   input_I_home_pops    = I_home_pops,
                                   input_I_hosp_pops    = I_hosp_pops,
                                   input_I_icu1_pops    = I_icu1_pops,
                                   input_I_icu2_pops    = I_icu2_pops,
                                   input_R_pops         = R_pops,
                                   input_D_pops         = D_pops,
                                   frac_beta_asym       = 0.50,
                                   frac_beta_hosp       = 0.06,
                                   delta                = 0.25,
                                   recov_a              = 0.57,
                                   recov_p              = 0.62,
                                   recov_s              = 0.11,
                                   recov_home           = 0.28,
                                   recov_icu1           = 0.12,
                                   recov_icu2           = 0.29,
                                   asym_rate            = 0.65,
                                   sym_to_icu_rate      = 0.122)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{  }% use one of  RShowDoc("KEYWORDS")
