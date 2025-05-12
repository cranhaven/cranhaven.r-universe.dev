#' @include unit.R
NULL

#' Leabra layer class
#'
#' This class simulates a biologically realistic layer of neurons in the
#' Leabra framework. It consists of several \code{\link{unit}} objects
#' in the variable (field) \code{units} and some layer-specific
#' variables.
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
#'   of activation in a layer of neurons.
#' @format \code{\link{R6Class}} object
#'
#' @examples
#' l <- layer$new(c(5, 5)) # create a 5 x 5 layer with default leabra values
#'
#' l$g_e_avg # private values cannot be accessed
#' # if you want to see alle variables, you need to use the function
#' l$get_layer_vars(show_dynamics = TRUE, show_constants = TRUE)
#' # if you want to see a summary of all units without constant values
#' l$get_unit_vars(show_dynamics = TRUE, show_constants = FALSE)
#'
#' # let us clamp the activation of the 25 units to some random values between
#' # 0.05 and 0.95
#' l <- layer$new(c(5, 5))
#' activations <- runif(25, 0.05, .95)
#' l$avg_act
#' l$clamp_cycle(activations)
#' l$avg_act
#' # what happened to the unit activations?
#' l$get_unit_acts()
#' # compare with activations
#' activations
#' # scaled activations are scaled by the average activation of the layer and
#' # should be smaller
#' l$get_unit_scaled_acts()
#'
#' @field units A list with all \code{\link{unit}} objects of the layer.
#' @field avg_act The average activation of all units in the layer
#' (this is an active binding).
#' @field n Number of units in layer.
#' @field weights A receiving x sending weight matrix, where the receiving units
#'   (rows) has the current weight values for the sending units (columns). The
#'   weights will be set by the \code{\link{network}} object, because they
#'   depend on the connection to other layers.
#' @field ce_weights Sigmoidal contrast-enhanced version of the weight matrix
#'   \code{weights}. These weights will be set by the \code{\link{network}}
#'   object.
#' @field layer_number Layer number in network (this is 1 if you create
#' a layer on your own, without the network class).
#'
#' @section Methods:
#' \describe{
#'
#'   \item{\code{new(dim, g_i_gain = 2)}}{Creates an object of this class with
#'   default parameters.
#'
#'     \describe{
#'       \item{\code{dim}}{A pair of numbers giving the dimensions (rows and
#'       columns) of the layer.}
#'
#'       \item{\code{g_i_gain}}{Gain factor for inhibitory conductance, if you
#'       want less activation in a layer, set this higher.}
#'     }
#'   }
#'
#'   \item{\code{get_unit_acts()}}{Returns a vector with the activations of all
#'   units of a layer.
#'   }
#'
#'   \item{\code{get_unit_scaled_acts()}}{Returns a vector with the scaled
#'   activations of all units of a layer. Scaling is done with
#'   \code{recip_avg_act_n}, a reciprocal function of the number of active
#'   units.
#'   }
#'
#'   \item{\code{cycle(intern_input, ext_input)}}{Iterates one time step with
#'   layer object.
#'     \describe{
#'        \item{\code{intern_input}}{Vector with inputs from all other layers.
#'        Each input has already been scaled by a reciprocal function of the
#'        number of active units (\code{recip_avg_act_n}) of the sending layer
#'        and by the connection strength between the receiving and sending
#'        layer. The weight matrix \code{ce_weights} is multiplied with this
#'        input vector to get the excitatory conductance for each unit in the
#'        layer.
#'        }
#'
#'        \item{\code{ext_input}}{Vector with inputs not coming from another
#'        layer, with length equal to the number of units in this layer. If
#'        empty (\code{NULL}), no external inputs are processed. If the external
#'        inputs are not clamped, this is actually an excitatory conductance
#'        value, which is added to the conductance produced by the internal
#'        input and weight matrix.
#'        }
#'     }
#'   }
#'
#'   \item{\code{clamp_cycle(activations)}}{Iterates one time step with layer
#'   object with clamped activations, meaning that activations are
#'   instantaneously set without time integration.
#'
#'     \describe{
#'       \item{\code{activations}}{Activations you want to clamp to the units in
#'       the layer.
#'       }
#'     }
#'   }
#'
#'   \item{\code{get_unit_act_avgs()}}{Returns a list with the short, medium and
#'   long term activation averages of all units in the layer as vectors. The
#'   super short term average is not returned, and the long term average is not
#'   updated before being returned (this is done in the function \code{chg_wt()}
#'   with the method\code{updt_unit_avg_l}). These averages are used by the
#'   network class to calculate weight changes.
#'   }
#'
#'   \item{\code{updt_unit_avg_l()}}{Updates the long-term average
#'   (\code{avg_l}) of all units in the layer, usually done after a plus phase.
#'   }
#'
#'   \item{\code{updt_recip_avg_act_n()}}{Updates the \code{avg_act_inert} and
#'   \code{recip_avg_act_n} variables, these variables update before the weights
#'   are changed instead of cycle by cycle. This version of the function assumes
#'   full connectivity between layers.
#'   }
#'
#'   \item{\code{reset(random = FALSE)}}{Sets the activation and activation
#'   averages of all units to 0. Used to begin trials from a stationary point.
#'
#'     \describe{
#'       \item{\code{random}}{Logical variable, if TRUE the activations are set
#'       randomly between .05 and .95 for every unit instead of 0.
#'       }
#'     }
#'   }
#'
#'   \item{\code{set_ce_weights()}}{Sets contrast enhanced weight values.
#'   }
#'
#'   \item{\code{get_unit_vars(show_dynamics = TRUE, show_constants =
#'   FALSE)}}{Returns a data frame with the current state of all unit variables
#'   in the layer. Every row is a unit. You can choose whether you want dynamic
#'   values and / or constant values. This might be useful if you want to
#'   analyze what happens in units of a layer, which would otherwise not be
#'   possible, because most of the variables (fields) are private in the unit
#'   class.
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
#'   \item{\code{get_layer_vars(show_dynamics = TRUE, show_constants =
#'   FALSE)}}{Returns a data frame with 1 row with the current state of the
#'   variables in the layer. You can choose whether you want dynamic values and
#'   / or constant values. This might be useful if you want to analyze what
#'   happens in a layer, which would otherwise not be possible, because some of
#'   the variables (fields) are private in the layer class.
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
layer <- R6::R6Class("layer",
  #public ----------------------------------------------------------------------
  public = list(
    # constructor
    initialize = function(dim, g_i_gain = 2){
      if (length(c(dim)) == 2){
        self$n <- prod(dim)
        unit1 <- unit$new()
        # use cloning
        self$units <- lapply(seq(self$n), function(x) unit1$clone(deep = TRUE))
      } else{
        stop("dim argument should be of the type c(rows, columns)")
      }

      Map(function(x, y) x$unit_number <- y, self$units, 1:self$n)
      # recip_avg_act_n is initialized with number of units that are > 0.4
      # (n_act_lrgr_forty)
      private$g_i_gain <- g_i_gain
      private$avg_act_inert <- self$avg_act
      n_act_lrgr_forty <- max(c(sum(self$get_unit_acts() > 0.4), 1))
      private$recip_avg_act_n <- 1 / (n_act_lrgr_forty + 2)
      private$g_fbi <- private$g_fbi_gain * self$avg_act
      private$g_ffi <- private$g_ffi_gain * max(c(private$g_e_avg -
                                                    private$g_ffi_thr, 0))
      invisible(self)
    },

    get_unit_acts = function(){
      sapply(self$units, function(x) x$activation)
    },

    get_unit_scaled_acts = function(){
      private$recip_avg_act_n * self$get_unit_acts()
    },

    cycle = function(intern_input, ext_input = NULL){
      # obtaining the excitatory conductance because of input
      # contrast enhanced weights are used
      g_e_per_unit <- self$ce_weights %*% intern_input
      if (!private$isempty(ext_input)) g_e_per_unit <- g_e_per_unit + ext_input

      # obtaining inhibitory conductance
      private$g_e_avg <- mean(g_e_per_unit)
      private$g_ffi <- private$g_ffi_gain * max(c(private$g_e_avg -
                                                    private$g_ffi_thr, 0))
      private$g_fbi <- private$g_fbi + private$g_fbi_dt *
        (private$g_fbi_gain * self$avg_act - private$g_fbi)
      g_i <- private$g_i_gain * (private$g_ffi + private$g_fbi)

      # calling the cycle method for all units
      Map(function(x, y, z) x$cycle(y, z), self$units, g_e_per_unit, g_i)
      invisible(self)
    },

    clamp_cycle = function(activations){
      Map(function(x, y) x$clamp_cycle(y), self$units, activations)
      # updating inhibition for the next cycle
      private$g_fbi <- private$g_fbi_gain * self$avg_act
      invisible(self)
    },

    get_unit_act_avgs = function(){
      avg_s <- sapply(self$units, function(x) x$avg_s)
      avg_m <- sapply(self$units, function(x) x$avg_m)
      avg_l <- sapply(self$units, function(x) x$avg_l)

      # obtaining avg_s_with_m
      avg_s_with_m <- private$m_avg_prc_in_s_avg * avg_m +
        (1 - private$m_avg_prc_in_s_avg) * avg_s

      list(
        "avg_s" = avg_s,
        "avg_m" = avg_m,
        "avg_l" = avg_l,
        "avg_s_with_m" = avg_s_with_m
      )
    },

    updt_unit_avg_l = function(){
      lapply(self$units, function(x) x$updt_avg_l())
      invisible(self)
    },

    updt_recip_avg_act_n = function(){
      private$avg_act_inert <- private$avg_act_inert +
        private$avg_act_inert_dt * (self$avg_act - private$avg_act_inert)
      n_units_avg_act <- max(round(private$avg_act_inert * private$n), 1)
      private$recip_avg_act_n <- 1 / (n_units_avg_act + 2)
      invisible(self)
    },

    reset = function(random = FALSE){
      lapply(self$units, function(x) x$reset(random = random))
      invisible(self)
    },

    set_ce_weights = function(){
      self$ce_weights <- 1 / (1 + (private$ce_off * (1 - self$weights) /
                                     self$weights) ^ private$ce_gain)
      invisible(self)
    },

    get_unit_vars = function(show_dynamics = TRUE, show_constants = FALSE){
      unit_vars_list <- lapply(self$units, function(x)
                               x$get_vars(show_dynamics, show_constants))
      unit_vars_df <- do.call(rbind, unit_vars_list)
      return(unit_vars_df)
    },

    get_layer_vars = function(show_dynamics = TRUE, show_constants = FALSE){
      df <- data.frame(layer = self$layer_number)
      dynamic_vars <- data.frame(
        avg_act = self$avg_act,
        avg_act_inert = private$avg_act_inert,
        g_e_avg = private$g_e_avg,
        g_fbi = private$g_fbi,
        g_ffi = private$g_ffi,
        recip_avg_act_n = private$recip_avg_act_n,
        ce_off = private$ce_off,
        ce_gain = private$ce_gain
      )
      constant_vars <-
        data.frame(
          n = self$n,
          g_ffi_gain           = private$g_ffi_gain,
          g_ffi_thr            = private$g_ffi_thr,
          g_fbi_gain           = private$g_fbi_gain,
          g_fbi_dt             = private$g_fbi_dt,
          g_i_gain             = private$g_i_gain,
          avg_act_inert_dt     = private$avg_act_inert_dt
        )
      if (show_dynamics == TRUE) df <- cbind(df, dynamic_vars)
      if (show_constants == TRUE) df <- cbind(df, constant_vars)
      return(df)
    },

    # fields -------------------------------------------------------------------
    n = NULL,          # number of units
    # An n x i weights matrix, where the n-th row has the
    # current weights values for all inputs coming to the n-th unit
    weights = NULL,
    # contrast-enhanced version of weights. ce_weights = SIG(weights).
    ce_weights = NULL,
    units = NULL,      # a list with all the unit objects of the layer
    layer_number = 1   # number of layer in the network
  ),

  # private --------------------------------------------------------------------
  private = list(
    # isempty
    #
    # check whether object is empty by looking at the length
    #
    isempty = function(x){
      length(x) == 0
    },
    # fields -------------------------------------------------------------------
    # dynamic ------------------------------------------------------------------

    # recip_avg_act_n is the scaling factor for the outputs coming out from THIS
    # layer. Notice this is different from C++ version. Only updated with
    # updt_recip_avg_act_n.
    recip_avg_act_n = NULL,
    avg_act_inert = NULL,

    g_e_avg = 0,       # average g_e for all units during last cycle
    g_fbi = NULL,      # feedback inhibition
    g_ffi = NULL,      # feedforward inhibition

    # constant -----------------------------------------------------------------
    g_ffi_gain = 1,          # gain for feedforward inhibition
    g_ffi_thr = 0.1,         # threshold for feedforward inhibition
    g_fbi_gain = 0.5,        # gain for feedback inhibition
    g_fbi_dt = 1 / 1.4,      # time step for fb inhibition (fb_tau = 1.4)
    g_i_gain = 2,            # overall gain on inhibition
    avg_act_inert_dt = 0.01,  # time step constant for updating avg_act_inert
    m_avg_prc_in_s_avg = 0.1, # proportion of medium to short term avg
    ce_off =  1,        # "offset" in the SIG function for contrast enhancement
    ce_gain = 6         # gain in the SIG function for contrast enhancement
  ),
  # active ---------------------------------------------------------------------
  active = list(
    # dependent
    #
    # avg_act returns the average activation of all units
    #
    avg_act = function(){
      mean(self$get_unit_acts())
    })
)
