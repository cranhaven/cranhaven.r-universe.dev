#' @include layer.R
NULL

#' Leabra network class
#'
#' Class to simulate a biologically realistic network of neurons
#' (\code{\link{unit}s}) organized in \code{\link{layer}s}
#'
#' This class simulates a biologically realistic artificial neuronal network in
#' the Leabra framework (e.g. O'Reilly et al., 2016). It consists of several
#' \code{\link{layer}} objects in the variable (field) \code{layers} and some
#' network-specific variables.
#'
#' @references O'Reilly, R. C., Munakata, Y., Frank, M. J., Hazy, T. E., and
#'   Contributors (2016). Computational Cognitive Neuroscience. Wiki Book, 3rd
#'   (partial) Edition. URL: \url{http://ccnbook.colorado.edu}
#'
#' @references Have also a look at
#'   \url{https://grey.colorado.edu/emergent/index.php/Leabra} (especially the
#'   link to the 'MATLAB' code) and \url{https://en.wikipedia.org/wiki/Leabra}
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for calculating changes
#'   of activation in a network of neurons organized in \code{\link{layer}}s.
#' @format \code{\link{R6Class}} object.
#'
#' @examples
#' # create a small network with 3 layers
#' dim_lays <- list(c(2, 5), c(2, 10), c(2, 5))
#' cxn <- matrix(c(0, 0, 0,
#'                 1, 0, 0.2,
#'                 0, 1, 0), nrow = 3, byrow = TRUE)
#' net <- network$new(dim_lays, cxn)
#'
#' net$m_in_s # private values cannot be accessed
#' # if you want to see alle variables, you need to use the function
#' net$get_network_vars(show_dynamics = TRUE, show_constants = TRUE)
#' # if you want to see a summary of all units (with layer information) without
#' # constant values
#' net$get_layer_and_unit_vars(show_dynamics = TRUE, show_constants = FALSE)
#'
#' # let us create 10 random inputs for layer 1 and 3
#' inputs <- net$create_inputs(c(1, 3), 10)
#' inputs # a list of lists
#'
#' # the input in layer 1 should be associated with the output in layer 3; we
#' # can use error driven learning to achieve this
#'
#' # first we will need the input for the minus phase (where no correct output
#' # is presented; layer 3 is NULL)
#' inputs_minus <- lapply(inputs, function(x) replace(x, 3, list(NULL)))
#' inputs_minus # layer 3 is indeed NULL
#' # now we can learn with default parameters; we will run 10 epochs,
#' # inputs_plus is equivalent to inputs; the output will be activations after
#' # each trial for the wohle network; this might take a while depending on your
#' # system
#' n_epochs <- 10
#' \dontrun{
#' output <- lapply(seq(n_epochs),
#'                  function(x) net$learn_error_driven(inputs_minus,
#'                                                     inputs,
#'                                                     lrate = 0.5))
#' # let's compare the actual output with what should have been learned we can
#' # use the method mad_per_epoch for this; it will calculate the mean absolute
#' # distance for each epoch; we are interested in layer 3
#' mad <- net$mad_per_epoch(output, inputs, 3)
#' # the error should decrease with increasing epoch number
#' plot(mad)}
#'
#' @field layers A list of \code{\link{layer}} objects.
#' @field lrate Learning rate, gain factor for how much the connection weights
#'   should change when the method \code{chg_wt()} is called.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(dim_lays, cxn, g_i_gain = rep(2, length(dim_lays)),
#'   w_init_fun = function(x) runif(x, 0.3, 0.7), w_init = NULL)}}{Creates an
#'   object of this class with default parameters.
#'     \describe{
#'       \item{\code{dim_lays}}{List of number pairs for rows and columns of the
#'       layers, e.g. \code{list(c(5, 5), c(10, 10), c(5, 5))} for a 25 x 100 x
#'       25 network.
#'       }
#'
#'       \item{\code{cxn}}{Matrix specifying connection strength between layers,
#'       if layer j sends projections to layer i, then \code{cxn[i, j] =
#'       strength > 0} and 0 otherwise. Strength specifies the relative strength
#'       of that connection with respect to the other projections to layer i.
#'       }
#'
#'       \item{\code{g_i_gain}}{Vector of inhibitory conductance gain values for
#'       every layer. This comes in handy to control overall level of inhibition
#'       of specific layers. Default is 2 for every layer.
#'       }
#'
#'       \item{\code{w_init_fun}}{Function that specifies how random weights
#'       should be created, default value is to generate weights between 0.3 and
#'       0.7 from a uniform distribution. It is close to 0.5 because the weights
#'       are contrast enhanced internally, so will actually be in a wider range.
#'       }
#'
#'       \item{\code{w_init}}{Matrix of initial weight matrices (like a cell
#'       array in 'MATLAB'), this is analogous to \code{cxn}, i.e.
#'       \code{w_init[i, j]} contains the initial weight matrix for the
#'       connection from layer j to i.  If you specify a \code{w_init},
#'       \code{w_init_fun} is ignored. You can use this if you want to have full
#'       control over the weight matrix.
#'       }
#'     }
#'   }
#'
#'   \item{\code{cycle(ext_inputs, clamp_inp)}}{Iterates one time step
#'   with the network object with external inputs.
#'
#'     \describe{
#'       \item{\code{ext_inputs}}{A list of matrices; ext_inputs[[i]] is a
#'       matrix that for layer i specifies the external input to each of its
#'       units. An empty matrix (\code{NULL}) denotes no input to that layer.
#'       You can also use a vector instead of a matrix, because the matrix is
#'       vectorized anyway.
#'       }
#'
#'       \item{\code{clamp_inp}}{Logical variable; TRUE: external inputs are
#'       clamped to the activities of the units in the layers, FALSE: external
#'       inputs are summed to excitatory conductance values (note: not to the
#'       activation) of the units in the layers.
#'       }
#'     }
#'   }
#'
#'   \item{\code{chg_wt()}}{Changes the weights of the entire network with the
#'   XCAL learning equation.
#'   }
#'
#'   \item{\code{reset(random = F)}}{Sets the activation of all units in all
#'   layers to 0, and sets all activation time averages to that value. Used to
#'   begin trials from a random stationary point. The activation values may also
#'   be set to a random value.
#'
#'     \describe{
#'       \item{\code{random}}{Logical variable, if TRUE set activation randomly
#'       between .05 and .95, if FALSE set activation to 0, which is the
#'       default.
#'       }
#'     }
#'   }
#'
#'   \item{\code{create_inputs(which_layers, n_inputs, prop_active =
#'   0.3)}}{Returns a list of length \code{n_inputs} with random input patterns
#'   (either 0.05, or. 0.95) for the layers specified in \code{which_layers}.
#'   All other layers will have an input of NULL.
#'     \describe{
#'       \item{\code{which_layers}}{Vector of layer numbers, for which you want
#'       to create random inputs.
#'       }
#'
#'       \item{\code{n_inputs}}{Single numeric value, how many inputs should be
#'       created.
#'       }
#'
#'       \item{\code{prop_active}}{Average proportion of active units in the
#'       input patterns, default is 0.3.
#'       }
#'     }
#'   }
#'
#'   \item{\code{learn_error_driven(inputs_minus, inputs_plus, lrate = 0.1,
#'   n_cycles_minus = 50, n_cycles_plus = 25, random_order = FALSE,
#'   show_progress = TRUE)}}{Learns to
#'   associate specific inputs with specific outputs in an error-driven fashion.
#'
#'     \describe{
#'       \item{\code{inputs_minus}}{Inputs for the minus phase (the to be
#'       learned output is not presented).
#'       }
#'
#'       \item{\code{inputs_plus}}{Inputs for the plus phase (the to be learned
#'       output is presented).
#'       }
#'
#'       \item{\code{lrate}}{Learning rate, default is 0.1.
#'       }
#'
#'       \item{\code{n_cycles_minus}}{How many cycles to run in the minus phase,
#'       default is 50.
#'       }
#'
#'       \item{\code{n_cycles_plus}}{How many cycles to run in the plus phase,
#'       default is 25.
#'       }
#'
#'       \item{\code{random_order}}{Should the order of stimulus presentation be
#'       randomized? Default is FALSE.
#'       }
#'
#'       \item{\code{show_progress}}{Whether progress of learning should be
#'       shown. Default is TRUE.
#'       }
#'     }
#'   }
#'
#'  \item{\code{learn_self_organized(inputs, lrate = 0.1, n_cycles = 50,
#'  random_order = FALSE, show_progress = TRUE)}}{Learns to categorize inputs in
#'  a self-organized fashion.
#'     \describe{
#'       \item{\code{inputs}}{Inputs for cycling.
#'       }
#'
#'       \item{\code{lrate}}{Learning rate, default is 0.1.
#'       }
#'
#'       \item{\code{n_cycles}}{How many cycles to run, default is 50.
#'       }
#'
#'       \item{\code{random_order}}{Should the order of stimulus presentation be
#'       randomized? Default is FALSE.
#'       }
#'
#'       \item{\code{show_progress}}{Whether progress of learning should be
#'       shown. Default is TRUE.
#'       }
#'     }
#'   }
#'
#'    \item{\code{test_inputs = function(inputs, n_cycles = 50, show_progress =
#'    FALSE)}}{Tests inputs without changing the weights (without learning).
#'    This is usually done after several learning runs.
#'
#'     \describe{
#'       \item{\code{inputs}}{Inputs for cycling.
#'       }
#'
#'       \item{\code{n_cycles}}{How many cycles to run, default is 50.
#'       }
#'
#'       \item{\code{show_progress}}{Whether progress of learning should be
#'       shown. Default is FALSE.
#'       }
#'     }
#'   }
#'
#'   \item{\code{mad_per_epoch(outs_per_epoch, inputs_plus,
#'   layer)}}{Calculates mean absolute distance for two lists of activations
#'   for a specific layer. This can be used to compare whether the network has
#'   learned what it was supposed to learn.
#'
#'     \describe{
#'       \item{\code{outs_per_epoch}}{Output activations for entire network for
#'       each trial for every epoch. This is what the network produced on its
#'       own.}
#'
#'       \item{\code{inputs_plus}}{Original inputs for the plus phase. This is
#'       what the network was supposed to learn.
#'       }
#'
#'       \item{\code{layer}}{Single numeric, for which layer to calculate the
#'       mean absolute distance. Usually, this is the "output" layer.
#'       }
#'     }
#'   }
#'
#'   \item{\code{set_weights(weights)}}{Sets new weights for entire network,
#'   useful to load networks that have already learned and thus very specific
#'   weights.
#'     \describe{
#'       \item{\code{weights}}{Matrix of matrices (like a cell array in
#'       'MATLAB') with new weight values.
#'       }
#'     }
#'   }
#'
#'   \item{\code{get_weights()}}{Returns the complete weight matrix, \code{w[i,
#'   j]} contains the weight matrix for the projections from layer j to layer i.
#'   Note that this is a matrix of matrices (equivalent to a 'MATLAB' cell
#'   array).
#'   }
#'
#'   \item{\code{get_layer_and_unit_vars(show_dynamics = T, show_constants =
#'   F)}}{Returns a data frame with the current state of all layer and unit
#'   variables. Every row is a unit. You can choose whether you want dynamic
#'   values and / or constant values. This might be useful if you want to
#'   analyze what happens in the network overall, which would otherwise not be
#'   possible, because most of the variables (fields) are private in the layer
#'   and unit class.
#'
#'     \describe{
#'       \item{\code{show_dynamics}}{Should dynamic values be shown? Default is
#'       TRUE.
#'       }
#'
#'       \item{\code{show_constants}}{Should constant values be shown? Default
#'       is FALSE.
#'       }
#'     }
#'   }
#'
#'   \item{\code{get_network_vars(show_dynamics = T, show_constants =
#'   F)}}{Returns a data frame with 1 row with the current state of the
#'   variables in the network. You can choose whether you want dynamic values
#'   and / or constant values. This might be useful if you want to analyze what
#'   happens in a network, which would otherwise not be possible, because some
#'   of the variables (fields) are private in the network class. There are some
#'   additional variables in the network class that cannot be extracted this way
#'   because they are matrices; if it is necessary to extract them, look at the
#'   source code.
#'
#'     \describe{
#'       \item{\code{show_dynamics}}{Should dynamic values be shown? Default is
#'       TRUE.
#'       }
#'
#'       \item{\code{show_constants}}{Should constant values be shown? Default
#'       is FALSE.
#'       }
#'     }
#'   }
#' }
#'
#' @importFrom plyr aaply
#' @importFrom plyr ldply
network <-  R6::R6Class("network",
  public = list(
    initialize = function(dim_lays, cxn,
                          lrate = 0.1,
                          g_i_gain = rep(2, length(dim_lays)),
                          w_init_fun = function(x) runif(x, 0.3, 0.7),
                          w_init = NULL
    ){
      private$dim_lays <- dim_lays

      private$cxn <- cxn
      private$normalize_cxn()
      private$cxn_greater_zero <- apply(private$cxn, c(1, 2),
                                        function(x) ifelse(x > 0, 1, 0))

      private$g_i_gain <- g_i_gain
      self$lrate <- lrate

      private$n_lays <- length(dim_lays)

      private$create_lays()
      private$set_all_unit_numbers()

      ifelse(is.null(w_init),
             private$create_rnd_wt_matrix(w_init_fun),
             private$w_init <- w_init)

      private$set_all_w_vars()

      private$test_argument_dims()


      private$second_test_argument_dims()

      private$create_wt_for_lays()
      invisible(self)
      },

    cycle = function(ext_inputs, clamp_inp){
      private$ext_inputs <- lapply(ext_inputs, c)
      private$is_ext_inputs_valid()
      private$has_layer_ext_input <- !sapply(private$ext_inputs, is.null)
      lay_no_ext_input_or_unclamped <-
        !private$has_layer_ext_input | !clamp_inp

      if (clamp_inp == T) private$clamp_ext_inputs()

      intern_inputs <- private$find_intern_inputs()
      private$cycle_with_unclamped_lays(intern_inputs, ext_inputs,
                                        lay_no_ext_input_or_unclamped)
      invisible(self)
    },

    chg_wt = function(){
      private$updt_recip_avg_act_n()
      lapply(self$layers, function(x) x$updt_unit_avg_l())

      # Extracting the averages for all layers
      avgs <- lapply(self$layers,
                     function(x) x$get_unit_act_avgs())
      avg_l <- lapply(avgs, function(x) x$avg_l)
      avg_m <- lapply(avgs, function(x) x$avg_m)
      avg_s_with_m <- lapply(avgs, function(x) x$avg_s_with_m)
      avg_l_lrn <- lapply(private$n_units_in_lays,
                          function (x) rep(private$avg_l_lrn, x))

      # For each connection matrix, calculate the intermediate vars
      is_cxn_null <- apply(private$cxn_greater_zero, c(1, 2),
                          function(x) ifelse(x == 0, return(NULL), return(x)))

      avg_s_with_m_snd <- private$get_snd_avg(avg_s_with_m, is_cxn_null)
      avg_s_with_m_rcv <- private$get_rcv_avg(avg_s_with_m, is_cxn_null)
      avg_m_snd <- private$get_snd_avg(avg_m, is_cxn_null)
      avg_m_rcv <- private$get_rcv_avg(avg_m, is_cxn_null)

      avg_l_rcv <- private$get_rcv_avg(avg_l, is_cxn_null)
      avg_l_lrn_rcv <- private$get_rcv_avg(avg_l_lrn, is_cxn_null)

      s_hebb <- private$m_mapply(function(x, y) x %o% y,
                         avg_s_with_m_rcv, avg_s_with_m_snd)
      m_hebb <- private$m_mapply(function(x, y) x %o% y, avg_m_rcv, avg_m_snd)

      # this is independent of sending; only the receiving neuron's long term
      # average is important for self-organized learning
      l_rcv <- private$m_mapply(function(x, y) matrix(rep(x, y), ncol = y),
                                avg_l_rcv, private$n_units_in_snd_lays)

      avg_l_lrn_rcv <- private$m_mapply(function(x, y) matrix(rep(x, y),
                                                              ncol = y),
                                avg_l_lrn_rcv,
                                private$n_units_in_snd_lays)
      # weights changing
      dwt_m <- private$m_mapply(function(x, y) private$get_dwt(x, y),
                                s_hebb,
                                m_hebb)
      dwt_l <- private$m_mapply(function(x, y) private$get_dwt(x, y),
                                s_hebb,
                                l_rcv)

      # multiply long term average by individual unit scaling factor

      dwt_l <- private$m_mapply(function(x, y) x * y, dwt_l, avg_l_lrn_rcv)

      # multiply medium average by gain for medium (versus long)
      dwt_m <- apply(dwt_m, c(1, 2), function(x) x[[1]] * private$gain_e_lrn)

      # combine both dwts and multiply by learning rate
      dwt <- private$m_mapply(function(x, y) x + y, dwt_m, dwt_l)
      dwt <- apply(dwt, c(1, 2), function(x) x[[1]] * self$lrate)

      # replace empty cells with null, then use cbind to generate the dwt for
      # every layer in a list
      dwt_null <- apply(dwt, c(1, 2), function(x)
        ifelse(private$isempty(x[[1]]), return(NULL), return(x[[1]])))
      dwt_list <- apply(dwt_null, 1, function(x) Reduce(cbind, x))

      private$set_new_exp_bounded_wts(dwt_list)
      lapply(self$layers, function(x) x$set_ce_weights())
      invisible(self)
    },

    reset = function(random = F){
      lapply(self$layers, function(x) x$reset(random))
      invisible(self)
    },

    set_weights = function(weights){
      # This function receives a cell array weights, which is like the cell
      # array w_init in the constructor: weights{i,j} is the weight matrix with
      # the initial weights for the cxn from layer j to layer i. The weights are
      # set to the values of weights. This whole function is a slightly modified
      # copypasta of the constructor.

      private$w_init <- weights

      w_empty <- matrix(sapply(weights, private$isempty), nrow = nrow(weights))

      private$is_dim_w_init_equal_to_dim_cxn()
      private$con_lays_have_wt()
      private$uncon_lays_have_no_wt()

      private$lay_dims_corresp_wt_dims()

      ## Now we set the weights
      # first find how many units project to the layer in all the network
      lay_inp_n <- apply(private$n_units_in_snd_lays, 1, sum)

      # make one weight matrix for every layer by collapsing receiving layer
      # weights columnwise, only do this for layers that receive something at
      # all, which is already handled by Reduce.
      wts <- apply(weights, 1, function(x) Reduce("cbind", x))
      Map(function(x, y) x$weights <- y, self$layers, wts)

      # set the contrast-enhanced version of the weights
      lapply(self$layers, function(x) x$set_ce_weights())
      invisible(self)
    },

    get_weights = function(){
      w <- matrix(Map(private$get_weight_for_one_connection, private$cxn,
                      private$w_index_low, private$w_index_up, self$layers),
                  ncol = ncol(private$w_index_low))
    },

    get_layer_and_unit_vars = function(show_dynamics = T, show_constants = F){
      unit_vars <- lapply(self$layers, function(x)
        x$get_unit_vars(show_dynamics = show_dynamics,
                        show_constants = show_constants))
      layer_vars <- lapply(self$layers, function(x)
        x$get_layer_vars(show_dynamics = show_dynamics,
                         show_constants = show_constants))
      comb <- Map(function(x, y) cbind(x, y), unit_vars, layer_vars)
      ldply(comb, data.frame)
    },

    get_network_vars = function(show_dynamics = T, show_constants = F){
      df <- data.frame(network_number = 1)
      dynamic_vars <- data.frame(
        lrate = self$lrate,
        m1 = private$get_m1()
      )
      constant_vars <-
        data.frame(
          n_lays = private$n_lays,
          n_units_in_net = private$n_units_in_net,
          gain_e_lrn = private$gain_e_lrn,
          d_thr = private$d_thr,
          d_rev = private$d_rev
        )
      if (show_dynamics == T) df <- cbind(df, dynamic_vars)
      if (show_constants == T) df <- cbind(df, constant_vars)
      return(df)
    },

    create_inputs = function(which_layers, n_inputs, prop_active = 0.3){
      lapply(1:n_inputs, function(x)
        private$create_one_input(private$dim_lays, which_layers, prop_active))
    },

    learn_error_driven = function(inputs_minus, inputs_plus, lrate = 0.1,
                                  n_cycles_minus = 50, n_cycles_plus = 25,
                                  show_progress = T, random_order = FALSE){
      order <- seq(inputs_minus)
      if (random_order == TRUE) order <- sample(order)

      private$gain_e_lrn <- 0
      private$avg_l_lrn <- 1
      private$gain_e_lrn <- 1
      private$avg_l_lrn <- 0.0004
      outs <- mapply(private$learn_one_pattern_error_driven,
                     inputs_minus[order],
                     inputs_plus[order],
                     pattern_number = seq(length(inputs_minus)),
                     MoreArgs = list(n_cycles_minus = n_cycles_minus,
                                     n_cycles_plus = n_cycles_plus,
                                     lrate = lrate,
                                     number_of_patterns = length(inputs_minus),
                                     show_progress = show_progress),
                     SIMPLIFY = F)
      return(outs[order(order)])
    },

    learn_self_organized = function(inputs, lrate = 0.1,
                                    n_cycles = 50, show_progress = T,
                                    random_order = FALSE){
      order <- seq(inputs)
      if (random_order == TRUE) order <- sample(order)
      private$gain_e_lrn <- 0
      private$avg_l_lrn <- 1
      outs <- mapply(private$lrn_one_ptrn_self_org,
                     inputs[order],
                     pattern_number = seq(length(inputs)),
                     MoreArgs = list(n_cycles_minus = n_cycles,
                                     lrate = lrate,
                                     number_of_patterns = length(inputs),
                                     show_progress = show_progress),
                     SIMPLIFY = F)
      return(outs[order(order)])
    },

    test_inputs = function(inputs, n_cycles = 50, show_progress = F){
      outs <- mapply(private$test_one_input,
                     inputs,
                     pattern_number = seq(length(inputs)),
                     MoreArgs = list(n_cycles_minus = n_cycles,
                                     number_of_patterns = length(inputs),
                                     show_progress = show_progress),
                     SIMPLIFY = F)
      return(outs)
    },

    mad_per_epoch = function(outs_per_epoch, inputs_plus, layer){
      sapply(outs_per_epoch, private$mad_for_one_epoch, inputs_plus, layer)
    },

    # fields -------------------------------------------------------------------
    lrate = 0.1,  # learning rate for XCAL
    layers = list()
  ),

  # private --------------------------------------------------------------------
  private = list(
    # functions for learning stimuli -------------------------------------------
    # trial
    #
    # runs several cycles with one input (for all layers)
    #
    # input is a list with the length being the number of layers
    #
    # lrate is the learning rate
    #
    # clamp_inp whether input should be clamped
    #
    # reset whether the network should be reset to a stationary point before
    # cycling (this can be extended by using the random parameter in reset
    # if one wants random activations)
    #
    # returns invisible self
    run_trial = function(input, n_cycles, reset = F){
      if (reset == T) self$reset()
      for (i in seq(n_cycles)) self$cycle(input, clamp_inp = T)
      invisible(self)
    },

    # learn_one_pattern_error_driven
    #
    # it does what it says, learning a pattern in error driven style
    #
    # input_minus one input for all layers, without the correct output
    #
    # input_plus one input for all layers, with correct output
    #
    # n_cycles_minus number of cycles for minus phase
    #
    # n_cycles_plus number of cycles for plus phase
    #
    # lrate learning rate
    #
    # clamp_inp whether input should be clamped
    #
    # reset whether the network should be reset to a stationary point before
    # cycling (this can be extended by using the random parameter in reset
    # if one wants random activations)
    #
    # returns activations of all units after minus phase (before learning)
    learn_one_pattern_error_driven = function(input_minus, input_plus,
                                              pattern_number,
                                              number_of_patterns,
                                              n_cycles_minus = 50,
                                              n_cycles_plus = 25,
                                              lrate = 0.1,
                                              reset = T,
                                              show_progress = T){
      # minus phase
      private$run_trial(input_minus, n_cycles_minus, reset = reset)

      output <- lapply(self$layers, function(x) x$get_unit_acts())

      # plus phase, do not reset the network!
      private$run_trial(input_plus, n_cycles_plus, reset = F)

      self$lrate <- lrate
      # change weights
      self$chg_wt()

      # show progress
      if (show_progress == T){
        if (pattern_number == number_of_patterns) {
          cat(".\n")
        } else {
            cat(".")
        }
      }
      return(output)
    },

    # lrn_one_ptrn_self_org
    #
    # same as above, bu for self organized learning (without plus phase)
    lrn_one_ptrn_self_org = function(input_minus, pattern_number,
                                              number_of_patterns,
                                              n_cycles_minus = 50,
                                              lrate = 0.1,
                                              reset = T,
                                              show_progress = T){
      # minus phase
      private$run_trial(input_minus, n_cycles_minus, reset = reset)

      output <- lapply(self$layers, function(x) x$get_unit_acts())

      self$lrate <- lrate
      # change weights
      self$chg_wt()

      # show progress
      if (show_progress == T){
        if (pattern_number == number_of_patterns) {
          cat(".\n")
        } else {
          cat(".")
        }
      }

      return(output)
    },

    # test_one_input
    #
    # present one pattern without learning (for testing)
    test_one_input = function(input_minus, pattern_number, number_of_patterns,
                              n_cycles_minus = 50,
                              reset = T, show_progress = F){
      # minus phase
      private$run_trial(input_minus, n_cycles_minus, reset = reset)

      output <- lapply(self$layers, function(x) x$get_unit_acts())

      # show progress
      if (show_progress == T){
        if (pattern_number == number_of_patterns) {
          cat(".\n")
        } else {
          cat(".")
        }
      }

      return(output)
    },

    # functions to create random inputs-----------------------------------------
    create_one_input = function(dim_lays, which_layers, prop_active = 0.3){
      prob <- c(prop_active, 1 - prop_active)
      inputs <- mapply(private$create_one_input_for_one_layer,
                       private$n_units_in_lays,
                       private$inv_which(which_layers, private$n_lays),
                       MoreArgs = list(prob = prob))
      inputs
    },

    inv_which = function(indices, totlength){
      is.element(seq_len(totlength), indices)
    },

    create_one_input_for_one_layer = function(n, has_layer_input, prob){
      result <- ifelse(has_layer_input,
                       return(sample(c(.95, 0.05), n, replace = T,
                                     prob = prob)),
                       return(NULL))
      return(result)
    },

    # functions to create random weights ---------------------------------------
    create_rnd_wt_matrix = function(fun = runif){
      private$w_init <- matrix(mapply(private$create_wt_matrix_for_one_cxn,
                                      private$cxn,
                                      private$n_units_in_snd_lays,
                                      private$n_units_in_rcv_lays,
                                      MoreArgs = list(fun = fun)),
                               nrow = nrow(private$cxn))
    },

    create_wt_matrix_for_one_cxn = function(cxn, n_rcv_units, n_snd_units, fun){
      ifelse(cxn > 0,
             return(matrix(fun(n_rcv_units * n_snd_units), nrow = n_snd_units)),
             return(NULL))
    },

    # first argument test in constructor ---------------------------------------
    test_argument_dims = function(){
      private$has_every_layer_g_i_gain()
      private$is_cxn_quadratic()
      private$is_nrow_cxn_equal_to_n_lays()
      private$is_dim_w_init_equal_to_dim_cxn()
      private$are_there_negative_cxn()
    },

    has_every_layer_g_i_gain = function(){
      if (length(private$g_i_gain) != length(private$dim_lays)){
        error <- paste("You have to specify g_i_gain for every layer, your
                       g_i_gain vector has length", length(private$g_i_gain),
                       "but you have ", private$n_lays, " layers.")
        stop(error)
      }

    },

    is_cxn_quadratic = function(){
      if (nrow(private$cxn) != ncol(private$cxn)){
        error <- (paste("Cannot create network. Connection matrix must have the
                        same number of rows and columns. It has ",
                        nrow(private$cxn), " rows and ", ncol(private$cxn), "
                        columns.", sep = ""))
        stop(error)
      }
    },

    is_nrow_cxn_equal_to_n_lays = function(){
      if (nrow(private$cxn) != length(private$dim_lays)){
        error <- (paste("Cannot create network. You have ",
                        length(private$dim_lays), " layer(s) and ",
                        nrow(private$cxn), " rows in the ", "connection matrix.
                        You need to specify the connection for every layer, so
                        the number of rows and columns in the connection matrix
                        must equal the number of layers.", sep = ""))
        stop(error)
      }
    },

    is_dim_w_init_equal_to_dim_cxn = function(){
      if (sum(dim(private$w_init) == dim(private$cxn)) < 2){
        error <- paste("Cannot create network. You have an initial matrix of
                       weight matrices with dims of ",
                       paste(dim(private$w_init), collapse = ", "), ", and a
                       connection matrix with dims of ",
                       paste(dim(private$cxn), collapse = ", "), ". They have to
                       be identical.", sep = "")
        stop(error)
      }
    },

    are_there_negative_cxn = function(){
      if (min(private$cxn) < 0){
        error <- paste("Cannot create network. Negative projection strengths
                       between layers are not allowed in connection matrix.")
        stop(error)
      }
    },

    # other constructor functions ----------------------------------------------
    # the receiving layer's strengths are normalized, such that every receiving
    # layer has a sum of strengths from all other layers of 1, if there are any
    # connections at all.
    normalize_cxn = function(){
      private$cxn <- aaply(private$cxn, 1,
                                 function(x) if (sum(x) > 0) x / sum(x) else x)
    },

    create_lays = function(){
      self$layers <- mapply(function(x, y) layer$new(x, y), private$dim_lays,
                          private$g_i_gain)
      Map(function(x, y) x$layer_number <- y,
          self$layers,
          1:length(self$layers))
    },

    # there are a couple of variables that count how many units are in the
    # network, in the layers and for every connection
    set_all_unit_numbers = function(){
      private$n_units_in_lays <- sapply(self$layers, function(x) x$n)
      private$n_units_in_net <- Reduce("+", private$n_units_in_lays)
      private$set_n_units_in_snd_lays()
      private$set_n_units_in_rcv_lays()
    },

    set_n_units_in_snd_lays = function(){
      result <- matrix(rep(private$n_units_in_lays,
                 length(private$n_units_in_lays)
        ), ncol = length(private$n_units_in_lays), byrow = T
      )
      result <- result * private$cxn_greater_zero
      private$n_units_in_snd_lays <- result
    },

    set_n_units_in_rcv_lays = function(){
      result <- matrix(rep(private$n_units_in_lays,
                           length(private$n_units_in_lays)),
           ncol = length(private$n_units_in_lays))
      result <- result * private$cxn_greater_zero
      private$n_units_in_rcv_lays <- result
    },

    set_all_w_vars = function(){
      private$w_init_empty <- matrix(sapply(private$w_init, private$isempty),
                                    nrow = nrow(private$w_init))
      private$w_index_low <- aaply(private$n_units_in_snd_lays, 1,
                                         function(x) head(c(1, cumsum(x) + 1),
                                                          -1))
      private$w_index_up <- aaply(private$n_units_in_snd_lays, 1,
                                        function(x) cumsum(x))
    },

    create_wt_for_lays = function(){
      wts <- apply(private$w_init, 1, function(x) Reduce("cbind", x))
      Map(function(x, y) if (!private$isempty(y)) x$weights <- y,
          self$layers,
          wts)
      lapply(self$layers, function(x) x$set_ce_weights())
    },

    # second argument test in constructor---------------------------------------
    # translates matrix into character version for error output
    matrix_to_character = function(x){
      apply(which(x == T, arr.ind = T), 1,
            function(x) paste("[", paste(x, collapse = ", "), "] ", sep = ""))
    },

    second_test_argument_dims = function(){
      private$con_lays_have_wt()
      private$uncon_lays_have_no_wt()
      private$lay_dims_corresp_wt_dims()
    },

    con_lays_have_wt = function(){
      cxn_no_w <- private$cxn > 0 & private$w_init_empty
      if (sum(cxn_no_w) > 0) {
        stop(paste("Connected layers have no weight matrix. Check the following
                   row(s) and column(s) ([row, column]) in initial weight matrix
                   and connection matrix: \n"),
                   private$matrix_to_character(cxn_no_w))
      }
    },

    uncon_lays_have_no_wt = function(){
      w_no_cxn <- private$cxn == 0 & !private$w_init_empty
      if (sum(w_no_cxn) > 0) {
        stop(paste("Non-Connected layers have weight matrix. Check the following
                   row(s) and column(s) ([row, column]) in initial connection
                   and weight matrix: \n"),
                   private$matrix_to_character(w_no_cxn))
      }
    },

    lay_dims_corresp_wt_dims = function(){
      w_dim_lay_dim <- mapply(
        private$one_l_dim_corr_wt_dim,
        private$w_init,
        private$n_units_in_rcv_lays,
        private$n_units_in_snd_lays)
      w_dim_lay_dim <- matrix(w_dim_lay_dim, nrow = nrow(private$w_init))

      if (sum(w_dim_lay_dim) < length(w_dim_lay_dim))
        stop(paste("dims of weights and layers are inconsistent. Check the
                   following row(s) and column(s) ([row, column]) in initial
                   weight matrix and number of units in the corresponding
                   layers.", "\n"), private$matrix_to_character(w_dim_lay_dim))
    },

    one_l_dim_corr_wt_dim =
      function(w_init, lay_n_rcv, lay_n_send){
        if (private$isempty(w_init) & lay_n_rcv == 0 & lay_n_send == 0){
          return(T)
        }
        sum(dim(w_init) == c(lay_n_rcv, lay_n_send)) == 2
      },

    # cycle subfunctions -------------------------------------------------------
    is_ext_inputs_valid = function(){
      private$is_ext_inputs_a_list()
      private$ext_inp_equal_n_lays()
    },

    is_ext_inputs_a_list = function(){
      if (!is.list(private$ext_inputs)){
        stop(paste("First argument to cycle function should be a list of
                   matrices or vectors."))
      }
    },

    ext_inp_equal_n_lays = function(){
      if (length(private$ext_inputs) != private$n_lays){
        stop(paste("Number of layers inconsistent with number of inputs in
                   network cycle. If a layer should not have an input, just use
                   NULL for that layer."))
      }
    },

    clamp_ext_inputs = function(){
      Map(function(x, y) if (!is.null(y)) x$clamp_cycle(y), self$layers,
          private$ext_inputs)
    },

    find_intern_inputs = function(){
      activeness_scaled_acts <- lapply(self$layers,
                                       function(x) x$get_unit_scaled_acts())
      rcv_lays_cxn <- unlist(apply(private$cxn, 1, list), recursive = F)
      lapply(rcv_lays_cxn, private$cxn_scale_acts, activeness_scaled_acts)
    },

    cxn_scale_acts = function(rcv_lay_cxn, activeness_scaled_acts){
      unlist(c(private$mult_list_vec(activeness_scaled_acts[rcv_lay_cxn > 0],
                                     rcv_lay_cxn[rcv_lay_cxn > 0])))
    },

    cycle_with_unclamped_lays = function(intern_inputs, ext_inputs,
                                         lays_unclamped){
      Map(private$cycle_with_one_unclamped_lay, self$layers, intern_inputs,
          ext_inputs, lays_unclamped)
      invisible(self)
    },

    cycle_with_one_unclamped_lay = function(lay, intern_inputs, ext_inputs,
                                            lay_unclamped){
      if (lay_unclamped) lay$cycle(intern_inputs, ext_inputs)
      invisible(self)
    },

    # chg_wt subfuctions -------------------------------------------------------
    #
    # get_dwt
    #
    # get weight changes with the "check mark" function in XCAL
    #
    # x vector of abscissa values, this is the hebbian short term activation of
    # receiving and sending unit
    #
    # the vector of threshold values with the same size as x, this is either the
    # hebbian medium term activation of receiving and sending unit or the long
    # term receiving unit activation
    #
    get_dwt = function(x, th){
      f <- rep(0, length(x))
      idx1 <- (x > private$d_thr) & (x < private$d_rev * th)
      idx2 <- x >= private$d_rev * th
      f[idx1] <- private$get_m1() * x[idx1]
      f[idx2] <- x[idx2] - th[idx2]
      return(f)
    },

    # get_weight_for_one_connection
    #
    # extracts weights for one connection
    #
    # cxn: connection matrix (receiving is rows, sending is columns, value is
    # strength)
    #
    # lower: lower index to extract column, which corresponds to the sending
    # layer's first unit
    #
    # upper: upper index to extract column, which corresponds to the sending
    # layer's last unit
    #
    # lay: the receiving layer (to actually get the weights)
    get_weight_for_one_connection = function(cxn, lower, upper, lay){
      ifelse(cxn > 0, return(lay$weights[, lower:upper]), return(NULL))
    },

    # updt_recip_avg_act_n
    #
    # Updates the avg_act_inert and recip_avg_act_n variables, these variables
    # update at the end of plus phases instead of cycle by cycle. This version
    # assumes full connectivity when updating recip_avg_act_n. If partial
    # connectivity were to be used, this should have the calculation in
    # WtScaleSpec::SLayActScale, in LeabraConSpec.cpp
    #
    # recip_avg_act_n is a scaling factor: more active layers are balanced, such
    # that every layer sends approximately the same average scaled activation to
    # other layers.
    #
    updt_recip_avg_act_n = function(){

      lapply(self$layers, function(x) x$updt_recip_avg_act_n())
      invisible(self)
    },

    # get_m1
    #
    # obtains the m1 factor: the slope of the left-hand line in the "check mark"
    # XCAL function.
    #
    get_m1 = function(){
      m <- (private$d_rev - 1) / private$d_rev
      return(m)
    },

    set_new_exp_bounded_wts = function(dwt_list){
      mapply(private$set_exp_bnd_wts_for_lay, dwt_list, self$layers)
    },

    set_exp_bnd_wts_for_lay = function(dwt, lay){
      if (!private$isempty(lay$weights)){
        lay$weights <- lay$weights + dwt * ifelse(dwt > 0,
                                                  1 - lay$weights,
                                                  lay$weights)
      }
    },

    get_snd_avg = function(avg, is_cxn_null){
      avg <- matrix(rep(avg, length(avg)), ncol = length(avg), byrow = T)
      private$m_mapply(function(x, y) x * y, is_cxn_null, avg)
    },

    get_rcv_avg = function(avg, is_cxn_null){
      avg <- matrix(rep(avg, length(avg)), ncol = length(avg), byrow = F)
      private$m_mapply(function(x, y) x * y, is_cxn_null, avg)
    },

    # general functions --------------------------------------------------------

    # isempty
    #
    # check whether object is empty by looking at the length
    #
    isempty = function(x){
      length(x) == 0
    },

    # mult_list_vec
    #
    # multiplies a list of vectors with constants from a vector with the length
    # of
    # the list
    mult_list_vec = function(x, y){
      mapply("*", x, y)
    },

    # m_mapply
    #
    # mapply version for matrix of matrices ('MATLAB' cell arrays), returns a
    # matrix
    m_mapply = function(fun, x, y){
      matrix(mapply(fun, x, y), ncol = ncol(x))
    },

    # extract_list_number
    #
    # extracts a specific list element
    extract_list_number = function(x, y){
      lapply(x, function(x) x[[y]])
    },

    # mad_for_one_epoch
    #
    # calculates mean absolute distance for one layer from two activation lists
    # (e.g. one is the correct pattern, the other is what you get in the
    # network); you need to specify which layer you want to extract
    #
    # output is the mean absolute distance for each epoch
    mad_for_one_epoch = function(epoch_outs, epoch_inputs_plus, layer){
      outs_layer <- private$extract_list_number(epoch_outs, layer)
      inputs_plus_layer <- private$extract_list_number(epoch_inputs_plus, layer)
      mean(unlist(Map(function(x, y) abs(x - y),
                      outs_layer,
                      inputs_plus_layer)),
           na.rm = T)
    },

    # fields -------------------------------------------------------------------
    dim_lays = NULL,
    cxn = NULL,
    w_init = NULL,
    n_lays = NULL,  # number of layers
    n_units_in_net = NULL,
    n_units_in_lays = NULL,

    # constants
    gain_e_lrn = 1, # proportion of error-driven learning in XCAL
    d_thr = 0.0001, # threshold for XCAL "check mark" function
    d_rev = 0.1, # reversal value for XCAL "check mark" function
    # g_i_gain for layers, to control overall inhibition in a specific layer
    g_i_gain = 2,
    #dependent
    #m1 = NULL, # the slope in the left part of XCAL's "check mark"
    cxn_greater_zero = matrix(), # binary version of connection
    # number of units of receiving layer in connection matrix
    n_units_in_snd_lays = matrix(),
    # number of units of sending layer in connection matrix
    n_units_in_rcv_lays = matrix(),
    w_index_low = matrix(), # lower index to extract weights from layer matrix
    # in connection format
    w_index_up = matrix(), # upper index to extract weights from layer matrix in
    # connection format
    w_init_empty = NULL,
    ext_inputs = NULL,
    has_layer_ext_input = NULL,
    # In Leabra documentation, this constant value can be used instead instead
    # of computing, but note that it is not needed for output layers and for
    # purely self-organized layers it will reduce amount of learning
    # substantially.
    avg_l_lrn = 0.0004
  )
)
