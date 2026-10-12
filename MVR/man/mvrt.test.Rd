\name{mvrt.test}

\alias{mvrt.test}

\title{Function for Computing Mean-Variance Regularized T-test Statistic and Its Significance}

\description{
    End-user function for computing MVR t-test statistic and its significance (p-value) 
    under sample group homoscedasticity or heteroscedasticity assumption. 
    
    Return an object of class "\code{mvrt.test}". Offers the option of parallel computation for 
    improved efficiency.
}

\usage{
    mvrt.test(data, 
              obj=NULL,
              block,
              tolog = FALSE, 
              nc.min = 1, 
              nc.max = 30, 
              pval = FALSE, 
              replace = FALSE, 
              n.resamp = 100, 
              parallel = FALSE,
              conf = NULL,
              verbose = TRUE, 
              seed = NULL)
}

\arguments{
  \item{data}{\code{numeric} \code{matrix} of untransformed (raw) data, 
              where samples are by rows and variables (to be clustered) are by columns, 
              or an object that can be coerced to such a \code{matrix} (such as a \code{numeric} \code{vector} 
              or a \code{data.frame} with all \code{numeric} columns). 
              Missing values (\code{NA}), NotANumber values (\code{NaN}) or Infinite values (\code{Inf}) are not allowed.}
  \item{obj}{Object of class \code{mvr} returned by \code{\link[MVR]{mvr}}.}
  \item{block}{\code{character} or \code{numeric} \code{vector}, or \code{factor} of group membership indicator variable 
              (grouping/blocking variable) of length the data sample size with as many different values or \code{levels} 
              as the number of data sample groups. Defaults to single group situation. See details.}
  \item{tolog}{\code{logical} scalar. Is the data to be log2-transformed first? Optional, defaults to \code{FALSE}.
             Note that negative or null values will be changed to 1 before taking log2-transformation.}
  \item{nc.min}{Positive \code{integer} scalar of the minimum number of clusters, defaults to 1}
  \item{nc.max}{Positive \code{integer} scalar of the maximum number of clusters, defaults to 30}
  \item{pval}{\code{logical} scalar. Shall p-values be computed? If not, \code{n.resamp} and \code{replace} will be ignored.
              If \code{FALSE} (default), t-statistic only will be computed,  
              If \code{TRUE}, exact (permutation test) or approximate (bootstrap test) p-values will be computed.}
  \item{replace}{\code{logical} scalar. Shall permutation test (default) or bootstrap test be computed?
                 If \code{FALSE} (default), permutation test will be computed with null permutation distribution,
                 If \code{TRUE}, bootstrap test will be computed with null bootstrap distribution.}
  \item{n.resamp}{Positive \code{integer} scalar of the number of resamplings to compute (default=100) by permutation or 
                  bootstsrap (see details).}
  \item{parallel}{\code{logical} scalar. Is parallel computing to be performed? Optional, defaults to \code{FALSE}.}           
    \item{conf}{
        \code{list} of 5 fields containing the parameters values needed for creating the parallel backend (cluster configuration).
        See details below for usage. Optional, defaults to \code{NULL}, but all fields are required if used:
        \itemize{
            \item \code{type} : \code{character} \code{vector} specifying the cluster type ("SOCKET", "MPI").
            \item \code{spec} : A specification (\code{character} \code{vector} or \code{integer} scalar) appropriate to the type of cluster.
            \item \code{homogeneous} : \code{logical} scalar to be set to \code{FALSE} for inhomogeneous clusters.
            \item \code{verbose} : \code{logical} scalar to be set to \code{FALSE} for quiet mode.
            \item \code{outfile} : \code{character} \code{vector} of an output log file name to direct the stdout and stderr
                  connection output from the workernodes. "" indicates no redirection.
        }
    }
  \item{verbose}{\code{logical} scalar. Is the output to be verbose? Optional, defaults to \code{TRUE}.}
  \item{seed}{Positive \code{integer} scalar of the user seed to reproduce the results.}
}

\details{
    Argument \code{block} will be converted to a factor, whose levels will match the data groups. It defaults to a single group situation, 
    that is, under the assumption of equal variance between sample groups. All group sample sizes must be greater than 1, 
    otherwise the program will stop.

    Argument \code{nc.max} currently defaults to 30. Empirically, we found that this is enough for most datasets tested.
    This depends on (i) the dimensionality/sample size ratio \eqn{\frac{p}{n}}, (ii) the signal/noise ratio, and 
    (iii) whether a pre-transformation has been applied (see \cite{Dazard, J-E. and J. S. Rao (2012)} for more details).
    See the cluster diagnostic function \code{\link[MVR]{cluster.diagnostic}} for more details, whether larger values of \code{nc.max} may be required.

    To save un-necessary computations, previously computed MVR clustering can be provided through option \code{obj} (i.e. \code{obj} is fully 
    specified as a \code{\link[MVR]{mvr}} object). In this case, arguments \code{data}, \code{block}, \code{tolog}, \code{nc.min}, \code{nc.max} 
    are ignored. If \code{obj} is fully specified (i.e. an object of class "\code{mvr}" returned by \code{\link[MVR]{mvr}}), the 
    the MVR clustering provided by \code{obj} will be used for the computation of the regularized t-test statistics. 
    If \code{obj}=\code{NULL}, a MVR clustering computation for the regularized t-test statistics and/or p-values will be performed.
    
    The function \code{mvrt.test} relies on the R package \pkg{parallel} to create a parallel backend within an R session, enabling access to a cluster
    of compute cores and/or nodes on a local and/or remote machine(s) and scaling-up with the number of CPU cores available and efficient parallel 
    execution. To run a procedure in parallel (with parallel RNG), argument \code{parallel} is to be set to \code{TRUE} and argument \code{conf} 
    is to be specified (i.e. non \code{NULL}). Argument \code{conf} uses the options described in function \code{makeCluster} of the R packages 
    \pkg{parallel} and \pkg{snow}. \pkg{PRIMsrc} supports two types of communication mechanisms between master and worker processes: 
    'Socket' or  'Message-Passing Interface' ('MPI'). In \pkg{PRIMsrc}, parallel 'Socket' clusters use sockets communication mechanisms only 
    (no forking) and are therefore available on all platforms, including Windows, while parallel 'MPI' clusters use high-speed interconnects 
    mechanism in networks of computers (with distributed memory) and are therefore available only in these architectures. A parallel 'MPI' 
    cluster also requires R package \pkg{Rmpi} to be installed first. Value \code{type} is used to setup a cluster of type 'Socket' ("SOCKET") 
    or 'MPI' ("MPI"), respectively. Depending on this type, values of \code{spec} are to be used alternatively:
    \itemize{
        \item For 'Socket' clusters (\code{conf$type="SOCKET"}), \code{spec} should be a \code{character} \code{vector} naming the hosts on which 
              to run the job; it can default to a unique local machine, in which case, one may use the unique host name "localhost".
              Each host name can potentially be repeated to the number of CPU cores available on the local machine.
              It can also be an \code{integer} scalar specifying the number of processes to spawn on the local machine; 
              or a list of machine specifications (a character value named host specifying the name or address of the host to use).
        \item For 'MPI' clusters (\code{conf$type="MPI"}), \code{spec} should be an \code{integer} scalar 
              specifying the total number of processes to be spawned across the network of available nodes, counting the workernodes and masternode.
    }
    The actual creation of the cluster, its initialization, and closing are all done internally. For more details, 
    see the reference manual of R package \pkg{snow} and examples below.

    When random number generation is needed, the creation of separate streams of parallel RNG per node is done internally by 
    distributing the stream states to the nodes. For more details, see the vignette of R package \pkg{parallel}. 
    The use of a seed allows to reproduce the results within the same type of session: the same seed will reproduce the same results within 
    a non-parallel session or within a parallel session, but it will not necessarily give the exact same results (up to sampling variability)
    between a non-parallelized and parallelized session due to the difference of management of the seed between the two (see parallel RNG and 
    value of returned seed below).
    
    In case p-values are desired (\code{pval}=\code{TRUE}), the use of a cluster is highly recommended. 
    It is ideal for computing embarassingly parallel tasks such as permutation or bootstrap resamplings.
    Note that in case both regularized t-test statistics and p-values are desired, in order to maximize computational efficiency and 
    avoid multiple configurations (since a cluster can only be configured and used one session at a time, which otherwise would result in a run stop), 
    the cluster configuration will only be used for the parallel computation of p-values, but not for the MVR clustering computation 
    of the regularized t-test statistics.
}

\value{
    \item{statistic}{\code{vector}, of size the number of variables, 
                     where entries are the t-statistics values of each variable.}
    \item{p.value}{\code{vector}, of size the number of variables, 
                   where entries are the p-values (if requested, otherwise \code{NULL} value) of each variable.}
    \item{seed}{User seed(s) used: \code{integer} of a single value, if parallelization is used. 
                \code{integer} \code{vector} of values, one for each replication, if parallelization is not used.}
}

\author{
    \itemize{
        \item "Jean-Eudes Dazard, Ph.D." \email{jean-eudes.dazard@case.edu}
        \item "Hua Xu, Ph.D." \email{huaxu77@gmail.com}
        \item "Alberto Santana, MBA." \email{ahs4@case.edu}
    }
    Maintainer: "Jean-Eudes Dazard, Ph.D." \email{jean-eudes.dazard@case.edu}    
}

\references{
    \itemize{
        \item Dazard J-E. and J. S. Rao (2010). "\emph{Regularized Variance Estimation and Variance Stabilization of High-Dimensional Data.}"
              In JSM Proceedings, Section for High-Dimensional Data Analysis and Variable Selection. 
              Vancouver, BC, Canada: American Statistical Association IMS - JSM, 5295-5309.
        \item Dazard J-E., Hua Xu and J. S. Rao (2011). "\emph{R package MVR for Joint Adaptive Mean-Variance Regularization and Variance Stabilization.}"
              In JSM Proceedings, Section for Statistical Programmers and Analysts. 
              Miami Beach, FL, USA: American Statistical Association IMS - JSM, 3849-3863.
        \item Dazard J-E. and J. S. Rao (2012). "\emph{Joint Adaptive Mean-Variance Regularization and Variance Stabilization of High Dimensional Data.}"
              Comput. Statist. Data Anal. 56(7):2317-2333.
    }
}

\section{Acknowledgments}{
    This work made use of the High Performance Computing Resource in the Core Facility for Advanced Research Computing at Case Western Reserve University. 
    This project was partially funded by the National Institutes of Health (P30-CA043703).
}

\note{
    End-user function.
}

\seealso{
    \itemize{
        \item \code{makeCluster} (R package \pkg{parallel})
        \item \code{eBayes} (R package \pkg{limma}) Bayesian Regularized t-test statistic \cite{Smyth, 2004}
        \item \code{samr} (R package \pkg{samr}) SAM Regularized t-test statistic \cite{Tusher et al., 2001, Storey, 2003}
        \item \code{matest} (R package \pkg{maanova}) James-Stein shrinkage estimator-based Regularized t-test statistic \cite{Cui et al., 2005}
        \item \code{ebam} (R package \pkg{siggenes}) Empirical Bayes Regularized z-test statistic \cite{Efron, 2001}
        \item \code{bayesT} Hierarchical Bayesian Regularized t-test statistic \cite{Baldi et al., 2001}
    }
}

\examples{
#================================================
# Loading the library and its dependencies
#================================================
library("MVR")

\dontrun{
    #===================================================
    # MVR package news
    #===================================================
    MVR.news()

    #================================================
    # MVR package citation
    #================================================
    citation("MVR")

    #===================================================
    # Loading of the Synthetic and Real datasets
    # Use help for descriptions
    #===================================================
    data("Synthetic", "Real", package="MVR")
    ?Synthetic
    ?Real
}

#================================================
# Regularized t-test statistics (Synthetic dataset) 
# Multi-Group Assumption
# Assuming unequal variance between groups
# With option to use prior MVR clustering results
# Without computation of p-values
# Without cluster usage
#================================================
nc.min <- 1
nc.max <- 10
probs <- seq(0, 1, 0.01)
n <- 10
GF <- factor(gl(n = 2, k = n/2, length = n), 
             ordered = FALSE, 
             labels = c("G1", "G2"))
mvr.obj <- mvr(data = Synthetic, 
               block = GF, 
               tolog = FALSE, 
               nc.min = nc.min, 
               nc.max = nc.max, 
               probs = probs,
               B = 100,
               parallel = FALSE, 
               conf = NULL,
               verbose = TRUE,
               seed = 1234)
mvrt.obj <- mvrt.test(data = NULL,
                      obj = mvr.obj,
                      block = NULL,
                      pval = FALSE,
                      replace = FALSE,
                      n.resamp = 100,
                      parallel = FALSE,
                      conf = NULL,
                      verbose = TRUE,
                      seed = 1234)       
\dontrun{
    #===================================================
    # Examples of parallel backend parametrization 
    #===================================================
    if (require("parallel")) {
       print("'parallel' is attached correctly \n")
    } else {
       stop("'parallel' must be attached first \n")
    }
    #===================================================
    # Example #1 - Quad core PC 
    # Running WINDOWS with SOCKET communication
    #===================================================
    cpus <- parallel::detectCores(logical = TRUE)
    conf <- list("spec" = rep("localhost", cpus),
                 "type" = "SOCKET",
                 "homo" = TRUE,
                 "verbose" = TRUE,
                 "outfile" = "")
    #===================================================
    # Example #2 - Master node + 3 Worker nodes cluster
    # Running LINUX with SOCKET communication
    # All nodes equipped with identical setups of 
    # multicores (8 core CPUs per machine for a total of 32)
    #===================================================
    masterhost <- Sys.getenv("HOSTNAME")
    slavehosts <- c("compute-0-0", "compute-0-1", "compute-0-2")
    nodes <- length(slavehosts) + 1
    cpus <- 8
    conf <- list("spec" = c(rep(masterhost, cpus),
                            rep(slavehosts, cpus)),
                 "type" = "SOCKET",
                 "homo" = TRUE,
                 "verbose" = TRUE,
                 "outfile" = "")
    #===================================================
    # Example #3 - Multinode of multicore per node cluster
    # Running LINUX with SLURM scheduler and MPI communication
    # Below, variable 'cpus' is the total number 
    # of requested core CPUs, which is specified from  
    # within a SLURM script.
    #===================================================
    if (require("Rmpi")) {
        print("'Rmpi' is attached correctly \n")
    } else {
        stop("'Rmpi' must be attached first \n")
    }
    cpus <- as.numeric(Sys.getenv("SLURM_NTASKS"))
    conf <- list("spec" = cpus,
                 "type" = "MPI",
                 "homo" = TRUE,
                 "verbose" = TRUE,
                 "outfile" = "")
    #===================================================
    # Mean-Variance Regularization (Real dataset)
    # Multi-Group Assumption
    # Assuming unequal variance between groups
    #===================================================
    nc.min <- 1
    nc.max <- 30
    probs <- seq(0, 1, 0.01)
    n <- 6
    GF <- factor(gl(n = 2, k = n/2, length = n), 
                 ordered = FALSE, 
                 labels = c("M", "S"))
    mvr.obj <- mvr(data = Real, 
                   block = GF, 
                   tolog = FALSE, 
                   nc.min = nc.min, 
                   nc.max = nc.max, 
                   probs = probs,
                   B = 100, 
                   parallel = TRUE, 
                   conf = conf,
                   verbose = TRUE,
                   seed = 1234)
    #===================================================
    # Regularized t-test statistics (Real dataset) 
    # Multi-Group Assumption
    # Assuming unequal variance between groups
    # With option to use prior MVR clustering results
    # With computation of p-values
    #===================================================
    mvrt.obj <- mvrt.test(data = NULL,
                          obj = mvr.obj,
                          block = NULL,
                          pval = TRUE,
                          replace = FALSE,
                          n.resamp = 100,
                          parallel = TRUE,
                          conf = conf,
                          verbose = TRUE,
                          seed = 1234)
    }
}

\keyword{Mean-Variance Estimators}
\keyword{Regularized Test Statistics}
\keyword{Parallel Programming}
\keyword{High Performance Computing}
