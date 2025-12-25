\name{RunModel}
\alias{RunModel}
\title{
Run the mcmc sampler for the model.
}
\description{
Run the mcmc sampler for the model.
}
\usage{
RunModel(orig,mc,hyper,para,output,synindex,individual_variable_index,
    household_variable_index,HHhead_at_group_level,weight_option,struc_weight,MissData,
    Parallel)
}
\arguments{
  \item{orig}{A list object including all the necessary data variables needed by the sampler.; output of the \verb{initData} function.}
  \item{mc}{A list specifying the number of mcmc iterations, burn-in, thinning and the effective sample size.}
  \item{hyper}{Hyper parameters for the prior distributions.}
  \item{para}{A list of the initial values of the parameters; output of the \verb{initParameters} function.}
  \item{output}{A list of output parameters to be saved; output of the \verb{initOutput} function.}
  \item{synindex}{A vector of iteration indexes for sampling synthetic data. length(\verb{synindex}) is the number of synthetic data needed. }
  \item{individual_variable_index}{Vector of column indexes for the individual-level variables.}
  \item{household_variable_index}{Vector of column indexes for the group-level variables.}
  \item{HHhead_at_group_level}{Logical indicator for whether or not to move the household head to the household level. Set to TRUE to move the household head and FALSE otherwise.}
  \item{weight_option}{Logical indicator for whether or not to cap the number of impossible households to sample and re-weight the multinomial counts within each latent class back to the expected truth. Set to TRUE to use the weigthting option nd FALSE otherwise.}
  \item{struc_weight}{Vector specifying the weights to be used for each household size. The weights must be ordered by household sizes and no household must be excluded.}
  \item{MissData}{A list that stores all the info related to missing data. Default to NULL for no missing data.}
  \item{Parallel}{
  Logical indicator for running the function in parallel mode.
}
}

\details{
This function runs the mcmc sampler for the NDPMPM model and generates posterior samples of parameters. It also generates synthetic data when needed.

Please note that:
\itemize{
\item{The minimum household size for this mcmc sampler is 2 because households of size 1 do not violate the structural zeros specified in this package. Also, moving the household head to the household level is not possible for households of size 1.}
\item{Each variable included must be recoded to start from 1.}
\item{Moving the household head to the household level and setting the HHhead_at_group_level option to TRUE speeds up the sampler significantly.}
\item{Setting the weight_option to TRUE and specifying weights also speeds up the sampler but the exact rate of speedup depends on the specific weights.}
}

Our example data set contains a sample of 2000 households and seven variables from the 2012 American Community Survey data. The variables are described below:
\itemize{
\item{ownership (ownership of dwelling): 1 = owned or being bought (loan), 2 = rented.}
\item{householdsize (household size): 2 = 2 people, 3 = 3 people, 4 = 4 people, 5 = 5 people, 6 = 6 people.}
\item{sex (gender): 1 = male, 2 = female.}
\item{race: 1 = white, 2 = black, 3 = American Indian or Alaska Native, 4 = Chinese, 5 = Japanese, 6 = other Asian/Pacific Islander,
7 = other race, 8 = two major races, 9 = three/more major races.}
\item{hisp (Hispanic origin). 1 = not Hispanic, 2 = Mexican, 3 = Puerto Rican, 4 = Cuban, 5 = other.}
\item{age: 1 = 0 (less then one year old), 2 = 1, 3 = 2,   . . . , 94 = 93}
\item{relate (relationship to the household head): 1 = head/householder, 2 = spouse, 3 = child, 4 = child-in-law, 5 = parent, 6 = parent-in-
law, 7 = sibling, 8 = sibling-in-law, 9 = grandchild, 10 = other relatives, 11 = partner, friend, visitor, 12 = other non-relatives}
}
Subsequent data sets must follow this structure because of the predefined list of structural zeros or users can modify the list of structural zeros by downloading the package source, making changes only to the checkconstraints_imp.cpp file and re-building the package.
}

\value{
\item{synData}{The list of synthetic data when the length(\verb{synindex}) > 0.}
\item{output}{The list of posterior samples for the parameters included in \verb{output}.}
}


\author{
Quanli Wang, Olanrewaju Akande
}

\keyword{mcmc}
\keyword{model}

