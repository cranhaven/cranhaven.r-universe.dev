\name{feedback.loop}
\alias{feedback.loop}
\alias{feedback.loop.matrix}

\title{Feedback Loop Analysis}

\description{Calculates the complete hierarchical feedback loop as described in Sonis et al. (1995). A feed back loop is complete if it contains all region-sector pairs. Much like a sudoku puzzle, there may only be one identified cell in each row and one identified cell in each column per loop. The loops are hierarchical in the sense that first loop maximizes the intermediate transactions given the aforementioned constraints.

There are TWO functions for RAM concerns. A singular function storing all feedback loop matrices grows at rate n^3. Alternatively, constructing feedback loop matrices one at a time translates to the output of \code{feedback.loop} growth rate of roughly 2n^2.

Note: A feedback loop solves the Linear Programming Assignment problem.

Warning: Computation time depends on size of the system. A progress bar is printed.}

\usage{feedback.loop(io, agg.sectors, agg.regions, n.loops)
feedback.loop.matrix(fl, loop)}

\arguments{
  \item{io}{An object of class \code{InputOutput} calculated from \code{\link{as.inputoutput}}}
  \item{agg.sectors}{An option to aggregate the sectors to compare regions only. Default is \code{FALSE}.}
  \item{agg.regions}{An option to aggregate the regions to compare sectors only. Default is \code{FALSE}.}
  \item{n.loops}{The number of loops you wish to calculate. The default is \code{"all"}. Must either be an integer or \code{"all"}}
  \item{fl}{An object of class \code{FeedbackLoop} created from \code{feedback.loop}}
  \item{loop}{The loop from which you want the selector matrix.}
}
\details{The feedback loop solves the following optimization problem:

\deqn{max_S vec(Z)'vec(S)}
such that:
\deqn{i) A_{col}vec(S) = vec(1)}
\deqn{ii) A_{row}vec(S) = vec(1)}
\deqn{iii) vec(0) \le vec(S) \le vec(1)}

where \eqn{Z} is the intermediate transaction matrix from \code{io}, \eqn{S} is a selctor matrix of the cells in \eqn{Z}, \eqn{A_{col}} is a constraint matrix to ensure only one cell per column is selected, \eqn{A_{row}} is a constraint matrix to ensure only one cell per row is selected, and constraint \eqn{iii)} ensures the values in the selector matrix are either one or zero. 

After each loop, the selected cells are set to an extremely negative number to prevent selection in the next loop.

See the documentation on http://www.real.illinois.edu/ for more details and interpretation of the loops.
}

\value{
Produces a nested list: \code{fl}
\item{fl}{Contains \code{"value"}, \code{"loop_1"}, \code{"loop_2"}, ..., and \code{"loop_n"}}
\item{value}{Contains a vector of the total value of intermediate transactions for each loop.}
\item{loop_i}{Contains a list over each loop's subloops. Retrieve by calling \code{fl$loop_i$subloop_j}. Note each loop will likely have a different number of subloops.}


}
\references{Sonis, M., Hewings, G. J., & Gazel, R (1995). The structure of multi-regional trade flows: hierarchy, feedbacks and spatial linkages. \emph{The Annals of Regional Science}, 29(4) 409-430.
}

\author{
John J. P. Wade, Xiuli Liu
}


\seealso{ 
\code{\link{as.inputoutput}}
}
\examples{
##########################
# The base feedback loop #
##########################
data(toy.IO)
class(toy.IO)

fbl = feedback.loop(toy.IO)
fbl$loop_1

fl_3 = feedback.loop.matrix(fbl, 3)
heatmap.io(fl_3, RS_label = toy.IO$RS_label)

fbl$value
fbl$per = fbl$value / sum(fbl$value) * 100

obj = data.frame(x = 1:length(fbl$per), y = fbl$per)

ggplot(obj, aes(x = x, y = y)) + 
  geom_line() + geom_point() +
  labs(x = 'Loop', y = 'Percent', title = 'Proportion of Total Intermediate Transactions per Loop')

###############################
# An aggregated feedback loop #
###############################
fbl_agg = feedback.loop(toy.IO, agg.regions = TRUE)
io_agg  = agg.region(toy.IO, regions = 'all', newname = 'magic')

fl_agg_1 = feedback.loop.matrix(fbl_agg, loop = 1)

heatmap.io(fl_agg_1, RS_label = io_agg$RS_label)
}

