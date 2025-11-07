\name{Movement}
\alias{Movement}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Movement dynamics in SPARSEMODr models.
}
\description{
    The SPARSEMODr models allow for spatially explicit movement dynamics between focal populations, and for 'visitation' from outside of the focal populations of interest.
}

\details{

    The meta-population of interest is defined by the focal populations supplied by the user in \code{\link{model_interface}}. Movement between focal populations within the meta-population is implemented as daily visitation (e.g., commuting). Specifically, individuals can move to a new focal population and can influence the local transmission dynamics for that day, but then individuals return to their focal population before the model simulates the next day's events. Every day, immigrants from outside of the meta-population can also visit the focal populations and influence transmission.

    \emph{Movement within the meta-population}

    We assume that susceptible and infectious individuals can move between focal populations. In the COVID-19 model, we further assume that only individuals in the Susceptible, Asymptomatic, and Pre-symptomatic classes are moving. This is because we assume individuals that are Symptomatic, Home (isolating) or in the hospital (Hospital, ICU1, ICU2) will not be moving outside of their focal population.

    In general, susceptible individuals in a focal population can become exposed to the pathogen by infectious visitors from the meta-population or by infectious visitors from outside of the meta-population (below). Similarly, susceptible individuals can visit a population within the meta-community but outside of their focal population, at which point these susceptible individuals may become exposed by resident infectious individuals.

    Movement frequency is controlled by parameter \code{m} in the model, and this rate can be updated daily to simulate changes in movement patterns over time (see \code{\link{time_windows}}). In the model differential equations, \code{m} is the per-capita rate of movement outside of the focal population. The inverse of \code{m} therefore corresponds to the average number of days between an individual's movement events.

    When an individual moves outside of their focal population, the model assigns this individual to a new focal population using a dispersal kernel. For now, we implement a simple distance-based dispersal kernal in the form:
    \code{ prob_move[i][j] = 1 / exp(dist_mat[i][j] / dist_phi). }
Here, as is convention, \code{prob_move[i][j]} corresponds to the probability of individuals in population \code{j} moving to population \code{i}. The \code{dist_phi} is user-defined and can be updated daily in the simulaiton (see \code{\link{time_windows}}).

    On each day in the model simulation, the tau-leaping algorithm calculates the number of individuals in each class that will move outside of their focal population. We determine which individuals will move to which outside population using a random draw from a multinomial probability distribution, using the \code{prob_move[i][j]} that are calculated as above. Once individuals are assigned to their new, temporary populations, then transmission can occur dependent upon the local composition of infectious individuals.

    \emph{Immigration from outside of the meta-population}

    The model allows for outside visitors to enter the system temporarily, with visitors updated daily. In this case, the user can define parameter \code{imm_frac}, the value of which can be updated daily (see \code{\link{time_windows}}). The \code{imm_frac} is the proportion of the focal population that may constitute visitors on any given day. For example if for a given focal population, \code{pop_N = 1000} and \code{imm_frac = 0.05}, an average of 50 visitors may arrive on a given day. The exact number of visitors on a given day is determined by drawing from a Poisson distribution. Then, the number of \emph{infectious} visitors from that group is assumed to be proportional to the number of infectious \emph{residents} in the focal population. In other words, we assume that the pathogen is present in 'outsider' populations at similar prevalence as the focal population. The exact number of infectious visitors is then determined again by a Poisson draw. After visitors arrive at the focal population, transmission between susceptible residents and infectious visitors is determined.
}

\seealso{
    \code{\link{model_interface}}, \code{\link{model_parallel}}, \code{\link{time_windows}}
}
