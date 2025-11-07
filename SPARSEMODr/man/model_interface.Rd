\name{model_interface}
\alias{model_interface}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{

Universal Model Interface to the SPARSE-MOD Models

}

\description{

    \code{model_interface} determines which SPARSE-MOD model to run based on the arguments and runs the specified model.

}

\usage{
    model_interface(
        control,
        arg.list
    )
}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{control}{
        Either a \code{covid19_control} or \code{seir_control} named list. The control used will determine which model to run.
    }
    \item{arg.list}{
        A named list of arguments used in all models including:
        \itemize{
            \item{\code{input_dist_mat}}
            \item{\code{input_census_area}}
            \item{\code{input_realz_seeds}}
            \item{\code{input_tw}}: A time window object (see \code{\link{time_windows}})
            \item{\code{trans_type}}: Transmission type (see details below)
            \item{\code{dd_trans_monod_k}}: The Monod equation parameter 'k', for when transmission is density-dependent (see details below)
            \item{\code{stoch_sd}}: The standard deviation of the stochastic transmission rate (see details below)
        }
    }

}

\details{
%%  ~~ If necessary, more details than the description above ~~
    This is the universal interface to all of the SPARSE-MOD models. Currently the models available are the COVID-19 Model, and the SEIR Model.

    The SPARSE-MOD COVID-19 Model describes transmission using 11 classes of individuals. Please see the vignettes for a more detailed explanation of the model structure.

    The SPARSE-MOD SEIR Model describes transmission using 4 classes of individuals. Please see the vignettes for a more detailed explanation of the model structure.

    Transmisison types: The day-specific transmission rate (beta) must be supplied. We allow for two transmission types:
    \enumerate{
        \item{Frequency-dependent transmission: In this case the transmission function is: \deqn{beta_scaled = beta / pop_N.} This is calculated per sub-population. 
        }
        \item{Density-dependent transmission: In this case, we allow a user-defined Monod equation to scale the beta term by sub-population density, where \eqn{pop_density = pop_N / census_area} :
            \deqn{
                beta_scaled = (beta * pop_dens / (dd_trans_monod_k + pop_dens)) / pop_N
            }
        }
    }

    Stochastic transmission: We implement daily stochastic variation in the transmission rate that scales with the number of infectious individuals in the focal population. In other words, as the number of infectious individuals increases, the variation in transmission rate reduces, emphasizing that stochasticity has larger effects in smaller populations (i.e., larger effects when there are few infectious individuals). To implement this stochasticity, we draw a random variate from a normal distribution with a mean of zero and a standard deviation of \code{stoch_sd}, and this random variate is termed \code{noise}. We calculate the total number of infectious individuals across infectious sub-classes (e.g., Pre-symptomatic, Hospital, etc.), and this variable is termed \code{infect_sum}. The functional form of stochasticity is then:
    \deqn{beta_realized = | beta_scaled * (1 + (noise / sqrt(infect_sum))) |}

    See \code{\link{Movement}} for details of how movement dynamics are implemented and controlled in the model.
}
\value{
    Two named lists:
    \enumerate{
        \item{\code{pops}: Integer vectors that provide the number of individuals in each model class at each time step. Different realizations of the model are distinguised by the user-provided values for the random seeds.}

        \item{\code{events}: Integer vectors that provide the number of individuals that newly transitioned to specific, key model classes at each time step. Different realizations of the model are distinguised by the user-provided values for the random seeds.

        For the COVID-19 model, these event vectors are defined as:
            \itemize{
                \item{\code{pos}: Number of newly positive individuals. Sum of new asymptomatic and pre-symptomatic individuals.}
                \item{\code{sym}: Number of newly symptomatic individuals.}
                \item{\code{total_hosp}: Number of newly hospitalized individuals. Sum of new Hospital admits and new Symptomatic-to-ICU1 admits.}
                \item{\code{total_icu}: Number of new ICU admits. Sum of new Symptomatic-to-ICU1 admits and new Hospital-to-ICU1 admits.}
                \item{\code{n_death}: Number of newly deceased individuals.}
            }
        For the SEIR model, these event vectors are defined as:
            \itemize{
                \item{\code{birth}: Number of newly susceptible hosts through the process of reproduction.}
                \item{\code{exposed}: Number of newly exposed hosts.}
                \item{\code{infectious}: Number of newly infectious hosts.}
                \item{\code{recov}: Number of newly recovered hosts.}
                \item{\code{death}: Number of newly deceased hosts.}
            }
        }
    }
}
\author{
    Joseph Mihaljevic, \email{joseph.mihaljevic@nau.edu}\cr
    Seth Borkovec, \email{stb224@nau.edu}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    \code{\link{model_parallel}}, \code{\link{time_windows}}, \code{\link{covid19_control}}, \code{\link{seir_control}}
}
\examples{
## See vignettes for more detailed work-ups.

##########################################
## See model_parallel()
## for an example to run realizations in parallel

##########################################
# Required for run:
require(lubridate)

## Using supplied example data:

# Read in the example data:
ex_dir <- system.file(
  "extdata", "sparsemodr_example.Rdata", package="SPARSEMODr", mustWork=TRUE)
load(ex_dir)
n_pop <- length(dat_list[["pop_N"]])

# Set up realizations:
realz_seeds <- 1:2
n_realz <- length(realz_seeds)

# Set up time windows (see time_windows for other ways to do this)
input_beta <-           c(   0.3,   0.3,  0.08,  0.08,  0.15)
input_dist_phi <-       c(   200,   200,    20,   150,   150)
input_m <-              c( 0.002, 0.002, 0.002,  0.02,  0.02)
input_imm_frac <-       c(   0.0,   0.0,   0.0,  0.02,  0.02)
# Window intervals
start_dates = c(mdy("1-1-20"),  mdy("2-1-20"),  mdy("2-16-20"), mdy("3-11-20"),  mdy("3-22-20"))
end_dates =   c(mdy("1-31-20"), mdy("2-15-20"), mdy("3-10-20"), mdy("3-21-20"), mdy("5-1-20"))


# User creates the time_windows object here
tw <- time_windows(beta = input_beta,
                   dist_phi = input_dist_phi,
                   m = input_m,
                   imm_frac = input_imm_frac,
                   start_dates = start_dates,
                   end_dates = end_dates)

# Randomly generate initial conditions for
# EXPOSED class:
E_pops <- vector("numeric", length = n_pop)
n_initial_E <- 40
# (more exposed in larger populations)
these_E <- sample.int(n_pop,
                     size = n_initial_E,
                     replace = TRUE,
                     prob = dat_list$pop_N)
for(i in 1:n_initial_E){
  E_pops[these_E[i]] <- E_pops[these_E[i]] + 1
}

# Inputs for the models
N_pops <- as.integer(dat_list[["pop_N"]])
S_pops <- N_pops - E_pops

# User created control list of parameters
covid19_control <- covid19_control(input_N_pops = N_pops,
                                   input_S_pops = S_pops,
                                   input_E_pops = E_pops)

arg.list <- list(
    input_dist_mat =    dat_list$dist_vec,
    input_census_area = dat_list$census_area,
    input_tw =          tw,
    input_realz_seeds = realz_seeds
)

# Using all default parameter values,
# these are the minimum inputs
covid_model_output <-
  model_interface(
    control = covid19_control,
    arg.list
  )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
