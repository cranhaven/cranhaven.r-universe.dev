#' simdata
#'
#' A function for simulating data under various topologies for continuous and
#' mixed data.
#'
#' @param graph A character string of the graph for which data will be
#' simulated. The graphs that can be chosen are m1_ge, m1_gv, m1_cp, m1_cc,
#' m1_iv, m2_ge, m2_gv, m2_cp, m2_cc, m2_iv, m3_gv, m3_cp, m3_cc, m3_iv, mp_ge,
#' mp_gv, gn4, gn5, gn8, gn11, layer, layer_cp, layer_iv, and star.
#'
#' The following figures show the graph for each of the topologies listed above.
#' The nodes with a circle around the name are normally distributed and the
#' nodes with a diamond around the name are distributed multinomial. The nodes
#' labeled with a C represent confounding variables. The Principle of Mendelian
#' Randomization (PMR) can be used on graphs with discrete and continuous
#' random variables. This introduces the constraint that the continuous random
#' variables cannot be parents of discrete random variables and edges between
#' these types of variables only have two states: absent and directed with the
#' discrete random variable as the parent.
#'
#' m1_ge - Topology M1 with three continuous random variables. In this case M1
#' has two other Markov equivalent graphs.
#'
#' m1_gv - Topology M1 with one discrete random variable U and two continuous
#' random variables. When using the PMR this graph does not have any other
#' Markov equivalent graphs.
#'
#' We consider three types of confounding variables (Yang et al., 2017):
#'
#' m1_cp - Topology M1 with n common parent confounding variables.
#'
#' m1_cc - Topology M1 with n common child confounding variables.
#'
#' m1_iv - Topology M1 with n intermediate confounding variables.
#'
#' \if{html}{\figure{m1.png}{options: width="75\%" alt="Figure: m1.png"}}
#' \if{latex}{\figure{m1.pdf}{options: width=12cm}}
#'
#' m2_ge - Topology M2 with three continuous random variables. This graph is a v
#' structure and does not have any other Markov equivalent graphs.
#'
#' m2_gv - Topolog M2 with one discrete random variable U and two continuous
#' random variables. This graph is a v structure and does not have any other
#' Markov equivalent graphs.
#'
#' m2_cp - Topology M2 with n common parent confounding variables.
#'
#' m2_cc - Topology M2 with n common child confounding variables.
#'
#' m2_iv - Topology M2 with n intermediate confounding variables.
#'
#' \if{html}{\figure{m2.png}{options: width="75\%" alt="Figure: m2.png"}}
#' \if{latex}{\figure{m2.pdf}{options: width=12cm}}
#'
#' m3_gv - Topolog M3 with one discrete random variable U and two continuous
#' random variables.
#'
#' m3_cp - Topology M3 with n common parent confounding variables.
#'
#' m3_cc - Topology M3 with n common child confounding variables.
#'
#' m3_iv - Topology M3 with n intermediate confounding variables.
#'
#' \if{html}{\figure{m3.png}{options: width="75\%" alt="Figure: m3.png"}}
#' \if{latex}{\figure{m3.pdf}{options: width=12cm}}
#'
#' mp_ge - The multi-parent topology with continuous random variables. This
#' graph is made up of multiple v structures and has no other Markov equivalent
#' graphs.
#'
#' mp_gv - The multi-parent topology with one discrete random variable. This
#' graph is made up of multiple v structures and has no other Markov equivalent
#' graphs.
#'
#' \if{html}{\figure{mp.png}{options: width="55\%" alt="Figure: mp.png"}}
#' \if{latex}{\figure{mp.pdf}{options: width=7cm}}
#'
#' gn4 - Topology GN4 is formed by combining topologies M1 and M2. The Markov
#' equivalence class for this topology is made up of three different graphs.
#'
#' gn5 - Topology GN5 has three other Markov equivalent graphs.
#'
#' gn8 - Topology GN8 has three overlapping cycles, two v structures, and two
#' other Markov equivalent graphs.
#'
#' gn11 - Topology GN11 has two sub-graphs separated by a v structure at node
#' T6.
#'
#' \if{html}{\figure{gn.png}{options: width="55\%" alt="Figure: gn.png"}}
#' \if{latex}{\figure{gn.pdf}{options: width=7cm}}
#'
#' layer - The layer topology has no other Markov equivalent graphs when using
#' the PMR and is made up of multiple M1 topologies.
#'
#' layer_cp - The layer topology with 2 common parent confounding variables.
#'
#' layer_iv - The layer topology with 2 intermediate confounding variables.
#'
#' \if{html}{\figure{layer.png}{options: width="75\%" alt="Figure: layer.png"}}
#' \if{latex}{\figure{layer.pdf}{options: width=12cm}}
#'
#' star - The star topology has no other Markov equivalent graphs when using the
#' PMR and is made up of multiple M1 topologies.
#'
#' \if{html}{\figure{star.png}{options: width="25\%" alt="Figure: star.png"}}
#' \if{latex}{\figure{star.pdf}{options: width=2.5cm}}
#'
#' @param N The number of observations to simulate.
#'
#' @param b0 The mean of the variable if it does not have any parents. If the
#' variable has one or more parents it is the slope in the linear model that is
#' the mean of the normally distributed variables.
#'
#' @param ss The coefficient of the parent nodes (if there are any) in the
#' linear model that is the mean of the normally distributed variables. This
#' coefficient is referred to as the signal strength.
#'
#' @param s The standard deviation of the normal distribution.
#'
#' @param p The probability of success for a binomial random variable.
#'
#' @param q The frequency of the reference allele.
#'
#' @param ssc The signal strength of the confounding variables.
#'
#' @param nConfounding The number of confounding variables to simulate.
#'
#' @return A matrix with the variables across the columns and the observations
#' down the rows.
#'
#' @references Yang, F., Wang, J., The GTEx Consortium, Pierce, B. L., and
#' Chen, L. S. (2017).
#' Identifying cis-mediators for trans-eQTLs across many human tissues using
#' genomic mediation analysis. \emph{Genome Res.} 27, 1859-1871.
#'
#' @examples
#' # Generate data under topology GN4.
#' data_gn4 <- simdata(graph = 'gn4',
#'                     N = 500,
#'                     b0 = 1,
#'                     ss = 1,
#'                     s = 1)
#'
#' # Display the first few rows of the data.
#' data_gn4[1:5, ]
#'
#' # Generate data under topology M1 with 3 intermediate confounding variables.
#' data_m1_iv <- simdata(graph = 'm1_iv',
#'                       N = 500,
#'                       b0 = 0,
#'                       ss = 1,
#'                       s = 1,
#'                       q = 0.1,
#'                       ssc = 0.2,
#'                       nConfounding = 3)
#'
#' # Show the first few rows of the data.
#' data_m1_iv[1:5, ]
#'
#' @export
#'
simdata <- function (graph = 'gn4',
                     N = 500,
                     b0 = 0,
                     ss = 1,
                     s = 1,
                     p = 0.6,
                     q = 0.45,
                     ssc = 0.2,
                     nConfounding = 2) {

  switch(graph,

         # m1_ge ---------------------------------------------------------------

         'm1_ge' = {

           T1 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           return (cbind(T1, T2, T3))

         },

         # m1_gv ---------------------------------------------------------------

         'm1_gv' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           return (cbind(U, T1, T2))

         },

         # m1_cph --------------------------------------------------------------

         'm1_cph' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss))

           return (cbind(U, T1, T2, W))

         },

         # m1_cp ---------------------------------------------------------------

         'm1_cp' = {

           U <- sMulti(N = N,
                       q = q)

           # create a vector with the signal strength of the parents for T1
           ssT1 <- vector(mode = 'numeric',
                          length = nConfounding + 1)

           ssT1[[1]] <- ss

           # Create a list with the data of the parents for T1 as the elements
           # of the list.
           parT1 <- vector(mode = 'list',
                           length = nConfounding + 1)

           parT1[[1]] <- U

           # create a vector with the signal strength of the parents for T2
           ssT2 <- vector(mode = 'numeric',
                          length = nConfounding + 1)

           ssT2[[1]] <- ss

           # Create a list with the data of the parents for T2 as the elements
           # of the list.
           parT2 <- vector(mode = 'list',
                           length = nConfounding + 1)

           # create a list to hold the data for the confounding variables
           con <- matrix(nrow = N,
                         ncol = nConfounding)

           # add column names to the matrix of confounding variables
           colnames(con) <- paste0('C', 1:nConfounding)

           # Simulate the data for the hidden variables
           for (a in 1:nConfounding) {

             con[, a] <- sNorm(N = N,
                               b0 = b0,
                               s = s)

             # Add the signal strength of the confounding variable to the ssT1
             # vector.
             ssT1[[a + 1]] <- ssc

             # Add the data of the current confounding variable to the parT2
             # list.
             parT1[[a + 1]] <- con[, a]

             # Add the signal strength of the confounding variable to the ssT2
             # vector.
             ssT2[[a + 1]] <- ssc

             # Add the data of the current confounding variable to the parT2
             # list.
             parT2[[a + 1]] <- con[, a]

           }

           T1 <- cNorm(N = N,
                       parentData = parT1,
                       b0 = b0,
                       b1 = ssT1,
                       s = s)

           # Fill in the first element of parT2 with data generated from T1.
           parT2[[1]] <- T1

           T2 <- cNorm(N = N,
                       parentData = parT2,
                       b0 = b0,
                       b1 = ssT2,
                       s = s)

           return (cbind(U, T1, T2, con))

         },

         # m1_cc ---------------------------------------------------------------

         'm1_cc' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           # create a list to hold the data for the confounding variables
           con <- matrix(nrow = N,
                         ncol = nConfounding)

           # add column names to the matrix of confounding variables
           colnames(con) <- paste0('C', 1:nConfounding)

           # Simulate the data for the hidden variables
           for (a in 1:nConfounding) {

             con[, a] <- cNorm(N = N,
                               parentData = list(T1, T2),
                               b0 = b0,
                               b1 = c(ssc, ssc),
                               s = s)

           }

           return (cbind(U, T1, T2, con))

         },

         # m1_iv ---------------------------------------------------------------

         'm1_iv' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           # create a list to hold the data for the confounding variables
           con <- matrix(nrow = N,
                         ncol = nConfounding)

           # add column names to the matrix of confounding variables
           colnames(con) <- paste0('C', 1:nConfounding)

           # create a vector with the signal strength of the parents for T2
           ssT2 <- vector(mode = 'numeric',
                          length = nConfounding + 1)

           ssT2[[1]] <- ss

           # Create a list with the data of the parents for T2 as the elements
           # of the list.
           parT2 <- vector(mode = 'list',
                           length = nConfounding + 1)

           parT2[[1]] <- T1

           # Simulate the data for the hidden variables
           for (a in 1:nConfounding) {

             con[, a] <- cNorm(N = N,
                               parentData = list(T1),
                               b0 = b0,
                               b1 = c(ssc),
                               s = s)

             # Add the signal strength of the confounding variable to the ssT2
             # vector.
             ssT2[[a + 1]] <- ssc

             # Add the data of the current confounding variable to the parT2
             # list.
             parT2[[a + 1]] <- con[, a]

           }

           T2 <- cNorm(N = N,
                       parentData = parT2,
                       b0 = b0,
                       b1 = ssT2,
                       s = s)

           return (cbind(U, T1, T2, con))

         },

         # m2_ge ---------------------------------------------------------------

         'm2_ge' = {

           T1 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T3 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1, T3),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           return (cbind(T1, T2, T3))

         },

         # m2_gv ---------------------------------------------------------------

         'm2_gv' = {

           U <- sMulti(N = N,
                       q = q)

           T2 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T1 <- cNorm(N = N,
                       parentData = list(U, T2),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           return (cbind(U, T1, T2))

         },

         # m2_cp ---------------------------------------------------------------

         'm2_cp' = {

           U <- sMulti(N = N,
                       q = q)

           # create a vector with the signal strength of the parents for T1
           ssT1 <- vector(mode = 'numeric',
                          length = nConfounding + 2)

           # Fill in the first and second elements of the ssT1 vector. Both
           # elements will be ss becasue the first two parents of T1 are U and
           # T2.
           ssT1[[1]] <- ss
           ssT1[[2]] <- ss

           # Create a list with the data of the parents for T1 as the elements
           # of the list.
           parT1 <- vector(mode = 'list',
                           length = nConfounding + 2)

           # The first parent of T1 is U.
           parT1[[1]] <- U

           # create a vector with the signal strength of the parents for T2
           ssT2 <- vector(mode = 'numeric',
                          length = nConfounding)

           # Create a list with the data of the parents for T2 as the elements
           # of the list.
           parT2 <- vector(mode = 'list',
                           length = nConfounding)

           # create a list to hold the data for the confounding variables
           con <- matrix(nrow = N,
                         ncol = nConfounding)

           # add column names to the matrix of confounding variables
           colnames(con) <- paste0('C', 1:nConfounding)

           # Simulate the data for the hidden variables
           for (a in 1:nConfounding) {

             con[, a] <- sNorm(N = N,
                               b0 = b0,
                               s = s)

             # Add the signal strength of the confounding variable to the ssT1
             # vector.
             ssT1[[a + 2]] <- ssc

             # Add the data of the current confounding variable to the parT2
             # list.
             parT1[[a + 2]] <- con[, a]

             # Add the signal strength of the confounding variable to the ssT2
             # vector.
             ssT2[[a]] <- ssc

             # Add the data of the current confounding variable to the parT2
             # list.
             parT2[[a]] <- con[, a]

           }

           T2 <- cNorm(N = N,
                       parentData = parT2,
                       b0 = b0,
                       b1 = ssT2,
                       s = s)

           # Fill in the second element of the parT1 list with the data from T2
           parT1[[2]] <- T2

           T1 <- cNorm(N = N,
                       parentData = parT1,
                       b0 = b0,
                       b1 = ssT1,
                       s = s)

           return (cbind(U, T1, T2, con))

         },

         # m2_cc ---------------------------------------------------------------

         'm2_cc' = {

           U <- sMulti(N = N,
                       q = q)

           T2 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T1 <- cNorm(N = N,
                       parentData = list(U, T2),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           # create a list to hold the data for the confounding variables
           con <- matrix(nrow = N,
                         ncol = nConfounding)

           # add column names to the matrix of confounding variables
           colnames(con) <- paste0('C', 1:nConfounding)

           # Simulate the data for the hidden variables
           for (a in 1:nConfounding) {

             con[, a] <- cNorm(N = N,
                               parentData = list(T1, T2),
                               b0 = b0,
                               b1 = c(ssc, ssc),
                               s = s)

           }

           return (cbind(U, T1, T2, con))

         },

         # m2_iv ---------------------------------------------------------------

         'm2_iv' = {

           U <- sMulti(N = N,
                       q = q)

           T2 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           # create a list to hold the data for the confounding variables
           con <- matrix(nrow = N,
                         ncol = nConfounding)

           # add column names to the matrix of confounding variables
           colnames(con) <- paste0('C', 1:nConfounding)

           # create a vector with the signal strength of the parents for T1
           ssT1 <- vector(mode = 'numeric',
                          length = nConfounding + 2)

           # The first two elements will be the signal strength of U and T2.
           ssT1[[1]] <- ss
           ssT1[[2]] <- ss

           # Create a list with the data of the parents for T1 as the elements
           # of the list.
           parT1 <- vector(mode = 'list',
                           length = nConfounding + 2)

           # The first to elements will be the data from U and T2.
           parT1[[1]] <- U
           parT1[[2]] <- T2

           # Simulate the data for the hidden variables
           for (a in 1:nConfounding) {

             con[, a] <- cNorm(N = N,
                               parentData = list(T2),
                               b0 = b0,
                               b1 = c(ssc),
                               s = s)

             # Add the signal strength of the confounding variable to the ssT1
             # vector.
             ssT1[[a + 2]] <- ssc

             # Add the data of the current confounding variable to the parT1
             # list.
             parT1[[a + 2]] <- con[, a]

           }

           T1 <- cNorm(N = N,
                       parentData = parT1,
                       b0 = b0,
                       b1 = ssT1,
                       s = s)

           return (cbind(U, T1, T2, con))

         },

         # m3_gv ---------------------------------------------------------------

         'm3_gv' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           return (cbind(U, T1, T2))

         },

         # m3_cph --------------------------------------------------------------

         'm3_cph' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T1, T2),
                       b0 = b0,
                       b1 = c(ss, ss, ss))

           return (cbind(U, T1, T2, W))

         },

         # m3_cp ---------------------------------------------------------------

         'm3_cp' = {

           U <- sMulti(N = N,
                       q = q)

           # create a vector with the signal strength of the parents for T1
           ssT1 <- vector(mode = 'numeric',
                          length = nConfounding + 1)

           ssT1[[1]] <- ss

           # Create a list with the data of the parents for T1 as the elements
           # of the list.
           parT1 <- vector(mode = 'list',
                           length = nConfounding + 1)

           parT1[[1]] <- U

           # create a vector with the signal strength of the parents for T2
           ssT2 <- vector(mode = 'numeric',
                          length = nConfounding + 1)

           ssT2[[1]] <- ss

           # Create a list with the data of the parents for T2 as the elements
           # of the list.
           parT2 <- vector(mode = 'list',
                           length = nConfounding + 1)

           # create a list to hold the data for the confounding variables
           con <- matrix(nrow = N,
                         ncol = nConfounding)

           # add column names to the matrix of confounding variables
           colnames(con) <- paste0('C', 1:nConfounding)

           # Simulate the data for the hidden variables
           for (a in 1:nConfounding) {

             con[, a] <- sNorm(N = N,
                               b0 = b0,
                               s = s)

             # Add the signal strength of the confounding variable to the ssT1
             # vector.
             ssT1[[a + 1]] <- ssc

             # Add the data of the current confounding variable to the parT2
             # list.
             parT1[[a + 1]] <- con[, a]

             # Add the signal strength of the confounding variable to the ssT2
             # vector.
             ssT2[[a + 1]] <- ssc

             # Add the data of the current confounding variable to the parT2
             # list.
             parT2[[a + 1]] <- con[, a]

           }

           T1 <- cNorm(N = N,
                       parentData = parT1,
                       b0 = b0,
                       b1 = ssT1,
                       s = s)

           # Fill in the first element of parT2 with data generated from U.
           parT2[[1]] <- U

           T2 <- cNorm(N = N,
                       parentData = parT2,
                       b0 = b0,
                       b1 = ssT2,
                       s = s)

           return (cbind(U, T1, T2, con))

         },

         # m3_cc ---------------------------------------------------------------

         'm3_cc' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           # create a list to hold the data for the confounding variables
           con <- matrix(nrow = N,
                         ncol = nConfounding)

           # add column names to the matrix of confounding variables
           colnames(con) <- paste0('C', 1:nConfounding)

           # Simulate the data for the hidden variables
           for (a in 1:nConfounding) {

             con[, a] <- cNorm(N = N,
                               parentData = list(T1, T2),
                               b0 = b0,
                               b1 = c(ssc, ssc),
                               s = s)

           }

           return (cbind(U, T1, T2, con))

         },

         # m3_iv ---------------------------------------------------------------

         'm3_iv' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           # create a list to hold the data for the confounding variables
           con <- matrix(nrow = N,
                         ncol = nConfounding)

           # add column names to the matrix of confounding variables
           colnames(con) <- paste0('C', 1:nConfounding)

           # create a vector with the signal strength of the parents for T2
           ssT2 <- vector(mode = 'numeric',
                          length = nConfounding + 1)

           ssT2[[1]] <- ss

           # Create a list with the data of the parents for T2 as the elements
           # of the list.
           parT2 <- vector(mode = 'list',
                           length = nConfounding + 1)

           parT2[[1]] <- U

           # Simulate the data for the hidden variables
           for (a in 1:nConfounding) {

             con[, a] <- cNorm(N = N,
                               parentData = list(T1),
                               b0 = b0,
                               b1 = c(ssc),
                               s = s)

             # Add the signal strength of the confounding variable to the ssT2
             # vector.
             ssT2[[a + 1]] <- ssc

             # Add the data of the current confounding variable to the parT2
             # list.
             parT2[[a + 1]] <- con[, a]

           }

           T2 <- cNorm(N = N,
                       parentData = parT2,
                       b0 = b0,
                       b1 = ssT2,
                       s = s)

           return (cbind(U, T1, T2, con))

         },

         # m4_ge ---------------------------------------------------------------

         'm4_ge' = {

           T1 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           # Generate data for T2 and T3 with a v structure at T3.
           T2_a <- cNorm(N = N,
                         parentData = list(T1),
                         b0 = b0,
                         b1 = c(ss),
                         s = s)

           T3_a <- cNorm(N = N,
                         parentData = list(T1, T2_a),
                         b0 = b0,
                         b1 = c(ss, ss),
                         s = s)

           # Generate data for T2 and T3 with a v structure at T2.
           T3_b <- cNorm(N = N,
                         parentData = list(T1),
                         b0 = b0,
                         b1 = c(ss),
                         s = s)

           T2_b <- cNorm(N = N,
                         parentData = list(T1, T3_b),
                         b0 = b0,
                         b1 = c(ss, ss),
                         s = s)

           # Create a vector representing a coin toss.
           coin_flip <- sample(x = 0:1,
                               size = N,
                               replace = TRUE)

           # Create empty vectors for T2 and T3.
           T2 <- vector(mode = 'numeric',
                        length = N)

           T3 <- vector(mode = 'numeric',
                        length = N)

           # Create a vector for T2 that is a combination of T2_a and T2_b.
           T2[coin_flip == 0] <- T2_a[coin_flip == 0]
           T2[coin_flip == 1] <- T2_b[coin_flip == 1]

           # Create a vector for T3 that is a combination of T3_a and T3_b.
           T3[coin_flip == 0] <- T3_a[coin_flip == 0]
           T3[coin_flip == 1] <- T3_b[coin_flip == 1]

           return (cbind(T1, T2, T3))

         },

         # m4_gv ---------------------------------------------------------------

         'm4_gv' = {

           U <- sMulti(N = N,
                       q = q)

           # Generate data for T1 and T2 with a v structure at T2.
           T1_a <- cNorm(N = N,
                         parentData = list(U),
                         b0 = b0,
                         b1 = c(ss),
                         s = s)

           T2_a <- cNorm(N = N,
                         parentData = list(U, T1_a),
                         b0 = b0,
                         b1 = c(ss, ss),
                         s = s)

           # Generate data for T1 and T2 with a v structure at T1.
           T2_b <- cNorm(N = N,
                         parentData = list(U),
                         b0 = b0,
                         b1 = c(ss),
                         s = s)

           T1_b <- cNorm(N = N,
                         parentData = list(U, T2_b),
                         b0 = b0,
                         b1 = c(ss, ss),
                         s = s)

           # Create a vector of 0s and 1s to represent flipping a coin.
           coin_flip <- sample(x = 0:1,
                               size = N,
                               replace = TRUE)

           # Create empty vectors for T1 and T2.
           T1 <- vector(mode = 'numeric',
                        length = N)

           T2 <- vector(mode = 'numeric',
                        length = N)

           # Create a vector for T1 that is a combination of T1_a and T1_b.
           T1[coin_flip == 0] <- T1_a[coin_flip == 0]
           T1[coin_flip == 1] <- T1_b[coin_flip == 1]

           # Create a vector for T2 that is a combination of T2_a and T2_b.
           T2[coin_flip == 0] <- T2_a[coin_flip == 0]
           T2[coin_flip == 1] <- T2_b[coin_flip == 1]

           return (cbind(U, T1, T2))

         },

         # mp_ge ---------------------------------------------------------------

         'mp_ge' = {

           T1 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T2 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T3 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T1, T2, T3),
                       b0 = b0,
                       b1 = c(ss, ss, ss),
                       s = s)

           return (cbind(T1, T2, T3, T4))

         },

         # mp_gv ---------------------------------------------------------------

         'mp_gv' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T2 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U, T1, T2),
                       b0 = b0,
                       b1 = c(ss, ss, ss),
                       s = s)

           return (cbind(U, T1, T2, T3))

         },

         # mp_cph --------------------------------------------------------------

         'mp_cph' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T2 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U, T1, T2),
                       b0 = b0,
                       b1 = c(ss, ss, ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T1, T2),
                       b0 = b0,
                       b1 = c(ss, ss, ss))

           return (cbind(U, T1, T2, T3, W))

         },

         # gn4 -----------------------------------------------------------------

         'gn4' = {

           T1 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T1, T4),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           return (cbind(T1, T2, T3, T4))

         },

         # gn5 -----------------------------------------------------------------

         'gn5' = {

           T1 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T3, T4),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           return (cbind(T1, T2, T3, T4, T5))

         },

         # gn6 -----------------------------------------------------------------

         'gn6' = {

           T1 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T2, T6),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           return (cbind(T1, T2, T3, T4, T5, T6))

         },

         # gn6_gv --------------------------------------------------------------

         'gn6_gv' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T2, T6),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(U, T3),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           return (cbind(U, T1, T2, T3, T4, T5, T6))

         },

         # gn6_cph -------------------------------------------------------------

         'gn6_cph' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T2, T6),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(U, T3),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T1, T4, T5),
                       b0 = b0,
                       b1 = c(ss, ss, ss))

           return (cbind(U, T1, T2, T3, T4, T5, T6, W))

         },

         # gn8 -----------------------------------------------------------------

         'gn8' = {

           T1 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T1, T5),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T6),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T8 <- cNorm(N = N,
                       parentData = list(T1, T5),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           return (cbind(T1, T2, T3, T4, T5, T6, T7, T8))

         },

         # gn8_gv --------------------------------------------------------------

         'gn8_gv' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T1, T5),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T6),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T8 <- cNorm(N = N,
                       parentData = list(T1, T5),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           return (cbind(U, T1, T2, T3, T4, T5, T6, T7, T8))

         },

         # gn8_cph -------------------------------------------------------------

         'gn8_cph' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T1, T5),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T6),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T8 <- cNorm(N = N,
                       parentData = list(T1, T5),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T1, T3, T5, T7),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss))

           return (cbind(U, T1, T2, T3, T4, T5, T6, T7, T8, W))

         },

         # gn8_cph_2 -------------------------------------------------------------

         'gn8_cph_2' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T1, T5),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T6),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T8 <- cNorm(N = N,
                       parentData = list(T1, T5),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T1, T3, T7),
                       b0 = b0,
                       b1 = c(ss, ss, ss))

           return (cbind(U, T1, T2, T3, T4, T5, T6, T7, T8, W))

         },

         # gn11 ----------------------------------------------------------------

         'gn11' = {

           T1 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T7 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T4),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T8 <- cNorm(N = N,
                       parentData = list(T7),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T9 <- cNorm(N = N,
                       parentData = list(T8),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T10 <- cNorm(N = N,
                        parentData = list(T9),
                        b0 = b0,
                        b1 = c(ss),
                        s = s)

           T11 <- cNorm(N = N,
                        parentData = list(T10),
                        b0 = b0,
                        b1 = c(ss),
                        s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T5, T7),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           return (cbind(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11))

         },

         # gn13 ----------------------------------------------------------------

         'gn13' = {

           # Simulate data for the source nodes first.
           U1 <- sMulti(N = N,
                        q = q)

           U2 <- sMulti(N = N,
                        q = q)

           U3 <- sMulti(N = N,
                        q = q)

           U4 <- sMulti(N = N,
                        q = q)

           U5 <- sMulti(N = N,
                        q = q)

           T6 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T7 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           # Simulate data for all other nodes with data for parent nodes
           # simulated before the data is simulated for the child nodes. In
           # other words, the order the data are simulated depends on who the
           # parents and children are for each node.
           T8 <- cNorm(N = N,
                       parentData = list(T7),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(U5, T8),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T1 <- cNorm(N = N,
                       parentData = list(U1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(U2, T1),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U3, T1, T6),
                       b0 = b0,
                       b1 = c(ss, ss, ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(U4, T6, T7),
                       b0 = b0,
                       b1 = c(ss, ss, ss),
                       s = s)

           return (cbind(U1, U2, U3, U4, U5,
                         T1, T2, T3, T4, T5,
                         T6, T7, T8))

         },

         # gn13_cph ------------------------------------------------------------

         'gn13_cph' = {

           # Simulate data for the source nodes first.
           U1 <- sMulti(N = N,
                        q = q)

           U2 <- sMulti(N = N,
                        q = q)

           U3 <- sMulti(N = N,
                        q = q)

           U4 <- sMulti(N = N,
                        q = q)

           U5 <- sMulti(N = N,
                        q = q)

           T6 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T7 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           # Simulate data for all other nodes with data for parent nodes
           # simulated before the data is simulated for the child nodes. In
           # other words, the order the data are simulated depends on who the
           # parents and children are for each node.
           T8 <- cNorm(N = N,
                       parentData = list(T7),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(U5, T8),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T1 <- cNorm(N = N,
                       parentData = list(U1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(U2, T1),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U3, T1, T6),
                       b0 = b0,
                       b1 = c(ss, ss, ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(U4, T6, T7),
                       b0 = b0,
                       b1 = c(ss, ss, ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U1, U3, T2, T4, T5),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss, ss))

           return (cbind(U1, U2, U3, U4, U5,
                         T1, T2, T3, T4, T5,
                         T6, T7, T8, W))

         },

         # gn13_cph_2 ----------------------------------------------------------

         'gn13_cph_2' = {

           # Simulate data for the source nodes first.
           U1 <- sMulti(N = N,
                        q = q)

           U2 <- sMulti(N = N,
                        q = q)

           U3 <- sMulti(N = N,
                        q = q)

           U4 <- sMulti(N = N,
                        q = q)

           U5 <- sMulti(N = N,
                        q = q)

           T6 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T7 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           # Simulate data for all other nodes with data for parent nodes
           # simulated before the data is simulated for the child nodes. In
           # other words, the order the data are simulated depends on who the
           # parents and children are for each node.
           T8 <- cNorm(N = N,
                       parentData = list(T7),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(U5, T8),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T1 <- cNorm(N = N,
                       parentData = list(U1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(U2, T1),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U3, T1, T6),
                       b0 = b0,
                       b1 = c(ss, ss, ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(U4, T6, T7),
                       b0 = b0,
                       b1 = c(ss, ss, ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T1, T2, T4, T5, T7),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss, ss))

           return (cbind(U1, U2, U3, U4, U5,
                         T1, T2, T3, T4, T5,
                         T6, T7, T8, W))

         },

         # cas_gv_3 ------------------------------------------------------------

         'cas_gv_3' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T1, T2, T3),
                       b0 = b0,
                       b1 = c(1, 1, 1))

           return (cbind(U, T1, T2, T3, W))

         },

         # cas_gv_3_1 ----------------------------------------------------------

         'cas_gv_3_1' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T1, T2, T3),
                       b0 = b0,
                       b1 = c(1, 1, 1, 1))

           return (cbind(U, T1, T2, T3, W))

         },

         # cas_gv_4 ------------------------------------------------------------

         'cas_gv_4' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T1, T2, T3, T4),
                       b0 = b0,
                       b1 = c(1, 1, 1, 1))

           return (cbind(U, T1, T2, T3, T4, W))

         },

         # cas_gv_4_1 ----------------------------------------------------------

         'cas_gv_4_1' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T1, T2, T3, T4),
                       b0 = b0,
                       b1 = c(1, 1, 1, 1, 1))

           return (cbind(U, T1, T2, T3, T4, W))

         },

         # cas_gv_5 ------------------------------------------------------------

         'cas_gv_5' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T4),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T1, T2, T3, T4, T5),
                       b0 = b0,
                       b1 = c(1, 1, 1, 1, 1))

           return (cbind(U, T1, T2, T3, T4, T5, W))

         },

         # cas_gv_5_1 ----------------------------------------------------------

         'cas_gv_5_1' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T4),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T1, T2, T3, T4, T5),
                       b0 = b0,
                       b1 = c(1, 1, 1, 1, 1, 1))

           return (cbind(U, T1, T2, T3, T4, T5, W))

         },

         # ffl_gv_3 ------------------------------------------------------------

         'ffl_gv_3' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T1, T2),
                       b0 =   b0,
                       b1 = c(ss, ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T1, T2, T3),
                       b0 = b0,
                       b1 = c(ss, ss, ss))

           return (cbind(U, T1, T2, T3, W))

         },

         # ffl_gv_3_1 ----------------------------------------------------------

         'ffl_gv_3_1' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T1, T2),
                       b0 =   b0,
                       b1 = c(ss, ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T1, T2, T3),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss))

           return (cbind(U, T1, T2, T3, W))

         },

         # ffl_gv_4 ------------------------------------------------------------

         'ffl_gv_4' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T1, T2),
                       b0 =   b0,
                       b1 = c(ss, ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T1, T2, T3, T4),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss))

           return (cbind(U, T1, T2, T3, T4, W))

         },

         # ffl_gv_4_1 ----------------------------------------------------------

         'ffl_gv_4_1' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T1, T2),
                       b0 =   b0,
                       b1 = c(ss, ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T1, T2, T3, T4),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss, ss))

           return (cbind(U, T1, T2, T3, T4, W))

         },

         # ffl_gv_5 ------------------------------------------------------------

         'ffl_gv_5' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T1, T2),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T4),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T1, T2, T3, T4, T5),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss, ss))

           return (cbind(U, T1, T2, T3, T4, T5, W))

         },

         # ffl_gv_5_1 ----------------------------------------------------------

         'ffl_gv_5_1' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T1, T2),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T4),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T1, T2, T3, T4, T5),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss, ss, ss))

           return (cbind(U, T1, T2, T3, T4, T5, W))

         },

         # layer ---------------------------------------------------------------

         'layer' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1, T2),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           return (cbind(U, T1, T2, T3, T4, T5, T6, T7))

         },



         # layer_cph_3 ---------------------------------------------------------

         'layer_cph_3' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1, T2),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T5, T7),
                       b0 = b0,
                       b1 = c(ss, ss, ss))

           return (cbind(U, T1, T2, T3, T4, T5, T6, T7, W))

         },

         # layer_cph_4 ---------------------------------------------------------

         'layer_cph_4' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1, T2),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T3, T5, T7),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss))

           return (cbind(U, T1, T2, T3, T4, T5, T6, T7, W))

         },

         # layer_cph_4_0 -------------------------------------------------------

         'layer_cph_4_0' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1, T2),
                       b0 = b0,
                       b1 = c(ss, ss),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T4, T6, T7),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss))

           return (cbind(U, T1, T2, T3, T4, T5, T6, T7, W))

         },

         # layer_cph_4_1 -------------------------------------------------------

         'layer_cph_4_1' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T3, T5, T7),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss))

           return (cbind(U, T1, T3, T4, T5, T7, W))

         },

         # layer_cph_4_2 -------------------------------------------------------

         'layer_cph_4_2' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T3, T5),
                       b0 = b0,
                       b1 = c(ss, ss, ss))

           return (cbind(U, T1, T3, T4, T5, T7, W))

         },

         # layer_cph_4_3 -------------------------------------------------------

         'layer_cph_4_3' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T3, T5, T7),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss))

           return (cbind(U, T1, T3, T5, T7, W))

         },

         # layer_cph_4_4 -------------------------------------------------------

         'layer_cph_4_4' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T3, T5),
                       b0 = b0,
                       b1 = c(ss, ss, ss))

           return (cbind(U, T1, T3, T5, T7, W))

         },

         # layer_cph_4_5 -------------------------------------------------------

         'layer_cph_4_5' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(U, T3, T1, T7),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss))

           return (cbind(U, T1, T3, T7, W))

         },

         # layer_cp ------------------------------------------------------------

         'layer_cp' = {

           U <- sMulti(N = N,
                       q = q)

           C1 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           C2 <- sNorm(N = N,
                       b0 = b0,
                       s = s)

           T1 <- cNorm(N = N,
                       parentData = list(U, C1),
                       b0 = b0,
                       b1 = c(ss, ssc),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(U, C2),
                       b0 = b0,
                       b1 = c(ss, ssc),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1, T2, C1),
                       b0 = b0,
                       b1 = c(ss, ss, ssc),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T2, C2),
                       b0 = b0,
                       b1 = c(ss, ssc),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           return (cbind(U, T1, T2, T3, T4, T5, T6, T7, C1, C2))

         },

         # layer_iv ------------------------------------------------------------

         'layer_iv' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           C1 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ssc),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           C2 <- cNorm(N = N,
                       parentData = list(T2),
                       b0 = b0,
                       b1 = c(ssc),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1, T2, C1),
                       b0 = b0,
                       b1 = c(ss, ss, ssc),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T2, C2),
                       b0 = b0,
                       b1 = c(ss, ssc),
                       s = s)

           T7 <- cNorm(N = N,
                       parentData = list(T3),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           return (cbind(U, T1, T2, T3, T4, T5, T6, T7, C1, C2))

         },

         # star ----------------------------------------------------------------

         'star' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           return (cbind(U, T1, T2, T3, T4, T5))

         },

         # star_cph ------------------------------------------------------------

         'star_cph' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T1, T2, T4),
                       b0 = b0,
                       b1 = c(ss, ss, ss))

           return (cbind(U, T1, T2, T3, T4, T5, W))

         },

         # star_cph_2 ----------------------------------------------------------

         'star_cph_2' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T2, T3, T4, T5),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss))

           return (cbind(U, T1, T2, T3, T4, T5, W))

         },

         # star_cph_3 ----------------------------------------------------------

         'star_cph_3' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T2, T3, T4, T5, T6),
                       b0 = b0,
                       b1 = c(ss, ss, ss, ss, ss))

           return (cbind(U, T1, T2, T3, T4, T5, T6, W))

         },

         # star_cph_3_1 --------------------------------------------------------

         'star_cph_3_1' = {

           U <- sMulti(N = N,
                       q = q)

           T1 <- cNorm(N = N,
                       parentData = list(U),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T2 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T3 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T4 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T5 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           T6 <- cNorm(N = N,
                       parentData = list(T1),
                       b0 = b0,
                       b1 = c(ss),
                       s = s)

           W <- cBinom(N = N,
                       parentData = list(T2, T3, T4, T5, T6),
                       b0 = b0,
                       b1 = c(1, 1, 1, 1, 1))

           return (cbind(U, T1, T2, T3, T4, T5, T6, W))

         })

}
