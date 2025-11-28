#' @title
#'  Massively trace backwards thermal-hydraulic regime for district
#'  heating network
#'
#' @family Regime tracing
#'
#' @description
#'  Trace values of thermal-hydraulic regime (temperature, pressure,
#'  flow rate, and other) in the bunched pipeline against the flow direction using
#'  user-provided values of \emph{specific heat loss power}.
#'
#'  Algorithm also suits for partially measurable district heating network with
#'  massive data lack conditions, when there are no temperature and pressure
#'  sensor readings on the majority of terminal nodes.
#'
#' @details
#'  They consider the topology of district heating network represented by
#'  \code{\link{m325testbench}}:
#'
#'  \figure{m325tracebw0.png}
#'
#'  The network may be partially sensor-equipped too:
#'  
#'  \figure{m325tracebwp.png}
#'
#'  In latter case no more than two nodes must be equipped with pressure and temperature
#'  sensors whereas for other nodes only flow rate sensors must be installed.
#'
#'  Tracing starts from sensor-equipped nodes and goes backwards, i.e against
#'  the flow direction.
#'
#'  Though some input arguments are natively vectorized their individual values
#'  all relate to common part of district heating network, i.e. associated with
#'  common object. It is due to isomorphism between vector representation and
#'  directed graph of this network. For more details of isomorphic topology
#'  description see \code{\link{m325testbench}}.
#'
#'  Before tracing starts for the next node, previously calculated values of
#'  thermal-hydraulic parameters are aggregated by either averaging or
#'  by median. The latter seems more robust for avoiding strong influence of
#'  possible outliers which may come from actual heating transfer anomalies,
#'  erroneous sensor readings or wrong pipeline specifications.
#'
#'  Aggregation for values of flow rate at the node is always \code{\link{sum}}.
#'
#' @param sender
#'    identifier of the node which heat carrier flows out.
#'    Type: any type that can be painlessly coerced to character by
#'    \code{\link{as.character}}.
#'
#' @param acceptor
#'    identifier of the node which heat carrier flows in. According to topology
#'    of test bench considered this identifier should be unique for every row.
#'    Type: any type that can be painlessly coerced to character by
#'    \code{\link{as.character}}.
#'
#' @param temperature
#'   Sensor-measured temperature of heat carrier (water) sensor-measured on 
#'   the terminal acceptor node, [\emph{°C}].
#'    Use \code{NA_float_}s for (terminal) nodes without temperature sensor.
#'    Type: \code{\link{assert_double}}.
#'
#' @param pressure
#'    Sensor-measured
#'    \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute pressure}
#'    of heat carrier (water) inside the pipe (i.e. acceptor's incoming edge),
#'    [\emph{MPa}].
#     Use \code{NA_float_}s for (terminal) nodes without pressure sensor.
#'    Type: \code{\link{assert_double}}.
#'
#' @param flow_rate
#'    Sensor-measured amount of heat carrier (water) on terminal node that is
#'    transferred by pipe (i.e. acceptor's incoming edge) during a period, [\emph{ton/hour}]. 
#'    Type: \code{\link{assert_double}}.
#'    Use \code{NA_float_}s for nodes without flow rate sensor.
#'
#' @param d
#'    internal diameter of pipe (i.e.diameter of acceptor's incoming edge),
#'    [\emph{mm}].
#'    Type: \code{\link{assert_double}}.
#'
#' @param len
#'    pipe length (i.e. length of acceptor's incoming edge), [\emph{m}].
#'    Type: \code{\link{assert_double}}.
#'
#' @param loss
#'    user-provided value of \emph{specific heat loss} power for each pipe, 
#'    [\emph{kcal/m/h}]. Values of the argument can be obtained experimentally,
#'    or taken from regulatory documents. 
#'    Type: \code{\link{assert_double}}.
#'
#' @param roughness
#'    roughness of internal wall of pipe (i.e. acceptor's incoming edge), [\emph{m}].
#'    Type: \code{\link{assert_double}}.
#'
#' @param inlet
#'    elevation of pipe inlet, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param outlet
#'    elevation of pipe outlet, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param method
#'    method of determining \emph{Darcy friction factor}:
#'    \itemize{
#'      \item \code{romeo}
#'      \item \code{vatankhan}
#'      \item \code{buzelli}
#'    }
#'    Type: \code{\link{assert_choice}}.
#'    For more details see \code{\link{dropp}}.
#'
#' @param opinion
#'    method for aggregating values of regime parameters on each node for the
#'    next tracing step:
#'    \describe{
#'       \item{\code{mean}}{values of parameter are averaged before the next
#'       tracing step}
#'       \item{\code{median}}{median of parameter values are used for the next
#'       tracing step}
#'     }
#'    Type: \code{\link{assert_choice}}.
#'
#' @param verbose
#'    logical indicator: should they watch tracing process on console?
#'    Type: \code{\link{assert_flag}}.
#'
#' @param csv
#'    logical indicator: should they incrementally dump results to \emph{csv}-
#'    file while tracing?
#'    Type: \code{\link{assert_flag}}.
#'
#' @param file
#'    name of \emph{csv}-file which they dump results to.
#'    Type: \code{\link{assert_character}} of length 1 that can be used safely
#'    to create a file and write to it.
#'
#' @return
#'    \code{\link{data.frame}} containing results (detailed log) of tracing in
#'    \href{https://en.wikipedia.org/wiki/Wide_and_narrow_data}{narrow format}:
#'    \describe{
#'       \item{\code{node}}{
#'         \emph{Tracing job}. Identifier of the node which regime parameters is
#'         calculated for. Values in this vector are identical to those in
#'         argument \code{acceptor}. 
#'         Type: \code{\link{assert_character}}.
#'       }
#'
#'       \item{\code{tracing}}{
#'         \emph{Tracing job}. Identifiers of nodes from which regime parameters
#'         are traced for the given node. Identifier \code{sensor} is used when
#'         values of regime parameters for the node are sensor readings.
#'         Type: \code{\link{assert_character}}.
#'       }
#'
#'       \item{\code{backward}}{
#'          \emph{Tracing job}. Identifier of tracing direction. It constantly
#'          equals to \code{TRUE}. 
#'          Type: \code{\link{assert_logical}}.
#'        }
#'
#'       \item{\code{aggregation}}{
#'          \emph{Tracing job}. Identifier of aggregation method: \emph{span}, \emph{median}, \emph{mean}, or \emph{identity}. Type: \code{\link{assert_character}}.
#'        }
#'       \item{\code{loss}}{
#'          \emph{Traced thermal hydraulic regime}. Normative specific heat loss power of adjacent pipe, [\emph{kcal/m/h}]. Type: \code{\link{assert_double}}.
#'        }
#'       \item{\code{flux}}{
#'          \emph{Traced thermal hydraulic regime}. Normative heat flux of adjacent pipe, [\emph{W/m^2}]. Type: \code{\link{assert_double}}.
#'        }
#'       \item{\code{Q}}{
#'         \emph{Traced thermal hydraulic regime}. Normative heat loss of adjacent pipe per day, [\emph{kcal}]. 
#'         Type: \code{\link{assert_character}}.
#'       }
#'
#'       \item{\code{temperature}}{
#'         \emph{Traced thermal hydraulic regime}. Traced temperature of heat
#'         carrier (water) that is associated with the node, [\emph{°C}].
#'         Type: \code{\link{assert_double}}.
#'       }
#'
#'       \item{\code{pressure}}{
#'         \emph{Traced thermal hydraulic regime}. Traced pressure of heat
#'         carrier (water) that is associated with the node, [\emph{MPa}].
#'         Type: \code{\link{assert_double}}.
#'       }
#'
#'       \item{\code{flow_rate}}{
#'          \emph{Traced thermal hydraulic regime}. Traced flow rate of heat
#'          carrier (water) that is associated with the node, [\emph{ton/hour}].
#'          Type: \code{\link{assert_double}}.
#'       }
#'
#'       \item{\code{job}}{
#'          \emph{Tracing job}. Value of tracing job counter. 
#'          Type: \code{\link{assert_count}}.
#'       }
#'  }
#'  Type: \code{\link{assert_data_frame}}.
#'
#' @examples
#' library(pipenostics)
#'
#' # It is possible to run without specification of argument values:
#' m325tracebw()
#'
#' # Consider isomorphic representation of District Heating Network graph:
#' DHN <- pipenostics::m325testbench
#' 
#' # * Adapt units:
#' DHN$d <- 1e3*DHN$d  # convert [m] to [mm]
#' 
#' # * Adapt node identifiers for ordering representation simplification:
#' DHN[["sender"]]   <- sprintf("N%02i", DHN[["sender"]])
#' DHN[["acceptor"]] <- sprintf("N%02i", DHN[["acceptor"]])
#'
#' # * Provided actual values of specific heat loss power (say, field measurements) for each
#' #   pipe in DHN, [kcal/m/h]:
#'  actual_loss <- c(
#'                # acceptor:
#'       96.236,  #  1 
#'       96.288,  #  2
#'       70.584,  #  3
#'      116.045,  #  4
#'       70.734,  #  5
#'       96.211,  #  6
#'       78.400,  #  7
#'      116.016,  #  8
#'       28.115,  #  9
#'       24.918,  # 10
#'      116.679,  # 11
#'        0.000,  # 12, may be unmeasured!
#'      153.134,  # 13 
#'       96.733,  # 14 
#'       96.600,  # 15 
#'      116.667,  # 16
#'       24.960,  # 17
#'      115.923,  # 18
#'       28.166,  # 19
#'       96.123,  # 20
#'       77.824,  # 21
#'      115.946,  # 22
#'       70.690,  # 23
#'       96.184,  # 24
#'       96.236,  # 25
#'       70.540   # 26
#'  )
#' 
#' # * Remove inappropriate attributes of the graph:
#' DHN.1 <- DHN[, setdiff(colnames(DHN), c("year", "insulation", "laying", "beta", "exp5k"))]
#'
#' # * Trace thermal-hydraulic regime for DHN:
#' tracebw_report <- do.call("tracebw", c(as.list(DHN.1), list(loss = actual_loss)))
#' 
#' # * If the actual values of specific heat loss power presented above are close 
#' #   to those in Minenergo-325, then the results of regime tracing match the 
#' #   normative procedure:
#' m325_report <- do.call("m325tracebw", DHN)
#'
#' stopifnot(
#'    all.equal(tracebw_report$temperature, m325_report$temperature, tolerance = 1e-4),
#'    all.equal(tracebw_report$pressure   , m325_report$pressure   , tolerance = 1e-4),
#'    all.equal(tracebw_report$flow_rate  , m325_report$flow_rate  , tolerance = 1e-4)
#'   )
#'
#' @export
tracebw <- function(sender = 6,
                    acceptor = 7,
                    temperature = 70.0,
                    pressure = pipenostics::mpa_kgf(6),
                    flow_rate = 20,
                    d = 100,
                    len = 72.446,
                    loss = 78.4,
                    roughness = 1e-3,
                    inlet = .5,
                    outlet = 1.0,
                    method = "romeo",
                    opinion = "median",
                    verbose = TRUE,
                    csv = FALSE,
                    file = "tracebw.csv") {
  # Trace thermal-hydraulic regime  ----
  .func_name <- "tracebw"
  
  # Meters in 1 millimeter
  METER <- 1e-3  # [m/mm]

  # Hours in 1 day
  DAY <- 24  # [h] 

  # Validate function input ----
  checkmate::assert_true(all(!is.na(acceptor)))
  acceptor <- as.character(acceptor)
  checkmate::assert_true(!any(duplicated(acceptor)))  # only single income edge!
  n <- length(acceptor)
  sender <- as.character(sender)
  checkmate::assert_character(sender, any.missing = FALSE, len = n)
  checkmate::assert_double(
    temperature,
    lower = 0,
    upper = 350,
    finite = TRUE,
    any.missing = TRUE,
    len = n
  )
  checkmate::assert_double(
    pressure,
    lower = 8.4e-2,
    upper = 100,
    finite = TRUE,
    any.missing = TRUE,
    len = n
  )
  checkmate::assert_double(
    flow_rate,
    lower = 1e-3,
    upper = 1e5,
    finite = TRUE,
    any.missing = TRUE,
    len = n
  )
  norms <- pipenostics::m325nhldata  # use brief name
  checkmate::assert_double(
    d,
    lower = min(norms[["diameter"]]),
    upper = max(norms[["diameter"]]),
    finite = TRUE,
    any.missing = FALSE,
    len = n
  )
  rm(norms)
  checkmate::assert_double(
    len,
    lower = 0,
    finite = TRUE,
    any.missing = FALSE,
    len = n
  )
    checkmate::assert_double(
    loss,
    lower       = 0,
    upper       = 1500,
    any.missing = FALSE,
    len         = n
  )
  checkmate::assert_double(
    roughness,
    lower = 0,
    upper = .2,
    any.missing = FALSE,
    len = n
  )
  checkmate::assert_double(
    outlet,
    lower = 0,
    finite = TRUE,
    any.missing = FALSE,
    len = n
  )
  checkmate::assert_double(
    inlet,
    lower = 0,
    finite = TRUE,
    any.missing = FALSE,
    len = n
  )
  checkmate::assert_choice(method, c("romeo", "vatankhan", "buzelli"))
  checkmate::assert_flag(verbose)
  checkmate::assert_choice(opinion, c("median", "mean"))
  checkmate::assert_flag(csv)
  if (csv) {
    checkmate::assert_character(
      basename(file),
      pattern = "^[[:alnum:]_\\.\\-]+$",
      any.missing = FALSE,
      len = 1L
    )  # check for validness of file name!
    checkmate::assert_path_for_output(file)
  }
  # Validate method aspects ----
  is_terminal_node <- !(acceptor %in% sender)
  checkmate::assert_double(
    temperature[is_terminal_node],
    any.missing = TRUE,
    all.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    pressure[is_terminal_node],
    any.missing = TRUE,
    all.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(flow_rate[is_terminal_node], any.missing = FALSE, min.len = 1L)
  # only paired values of temperature and pressure
  checkmate::assert_true(all(is.na(temperature) == is.na(pressure)))

  # Conf: list of aggregation functions ----
  agg_func <- list(
    span   = function(x) {
      y <- stats::na.omit(x)
      if (length(y) > 0L)
        max(y) - min(y)
      else
        NA_real_
    },
    median = function(x)
      stats::median(x, na.rm = TRUE),
    mean   = function(x) {
      y <- stats::na.omit(x)
      if (length(y) > 0L)
        mean(y)
      else
        NA_real_
    }
  )

  # Calculate additional regime parameters:
  is_temperature_sensored <- !is.na(temperature)
  flux <- Q <- rep.int(NA_real_, length(temperature))
  

  ## Heat flux, [W/m^2]
  flux[is_temperature_sensored] <- pipenostics::flux_loss(
    x   = loss[is_temperature_sensored],
    d   = d[is_temperature_sensored] * METER ,
    wth = pipenostics::wth_d(d[is_temperature_sensored])
  )

  ## Heat loss per day, [kcal]
  Q[is_temperature_sensored] <- loss[is_temperature_sensored] * len[is_temperature_sensored] * DAY

  # Start backward tracing ----
  time_stamp_posixct <- Sys.time()
  node_state <- with(list(), {
    v <- table(c(sender, acceptor)) - 1L
    v[!(names(v) %in% acceptor)] <- -1L
    # Conventional values:
    #   <0 - already processed,
    #    0 - should be processed,
    #   >0 - not yet processed, number of not processed successors
    v
  })
  if (verbose)
    cat(
      sprintf(
        "\n%s %s | start backward tracing; segments %i;",
        time_stamp_posixct,
        .func_name,
        n
      )
    )
  rm(n)

  job_num <- 0L
  # Set up job log columns ----
  job_log <- data.frame(
    node = acceptor,
    tracing = "sensor",
    backward = TRUE,
    aggregation = "identity",
    loss,
    flux,
    Q,
    temperature,
    pressure,
    flow_rate,
    job = job_num
  )[is_terminal_node, ]
  rm(is_terminal_node)

  if (csv)
    cat(paste(colnames(job_log), collapse = ","), "\n", file = file)

  # Start up job while-cycle ----
  while (any(node_state == 0L)) {
    if (verbose)
      cat(sprintf(
        "\n%s %s | start job; job %i;",
        time_stamp_posixct,
        .func_name,
        job_num
      ))

    is_node_ready <- node_state == 0L
    ready_nodes <-
      names(is_node_ready)[is_node_ready]  # acceptor names
    if (verbose)
      cat(
        sprintf(
          "\n%s %s | now process; %i node(s); [%s]",
          time_stamp_posixct,
          .func_name,
          length(ready_nodes),
          paste(ready_nodes, collapse = ",")
        )
      )

    # Make up job aggregations ----
    agg_log <- lapply(structure(names(agg_func), names = names(agg_func)),
                      function(func_name, log_df) {
                        tracing <-
                          tapply(log_df[["tracing"]], log_df[["node"]], "paste", collapse = "|")
                        data.frame(
                          node = names(tracing),
                          tracing = tracing,
                          backward = TRUE,
                          aggregation = func_name,
                          loss = tapply(log_df[["loss"]], log_df[["node"]], agg_func[[func_name]]),
                          flux = tapply(log_df[["flux"]], log_df[["node"]], agg_func[[func_name]]),
                          Q = tapply(log_df[["Q"]], log_df[["node"]], agg_func[[func_name]]),
                          temperature = tapply(log_df[["temperature"]], log_df[["node"]], agg_func[[func_name]]),
                          pressure = tapply(log_df[["pressure"]], log_df[["node"]], agg_func[[func_name]]),
                          flow_rate = tapply(log_df[["flow_rate"]], log_df[["node"]], sum),
                          job = job_num
                        )
                      }  ,
                      log_df = job_log[job_log[["node"]] %in% ready_nodes &
                                         job_log[["aggregation"]] == "identity", ])


    job_log <- rbind(job_log, do.call("rbind", agg_log))

    # Dump log to file ----
    if (csv)
      utils::write.table(
        job_log[job_log[["job"]] == job_num, ],
        file = file,
        append = TRUE,
        quote = FALSE,
        sep = ",",
        col.names = FALSE,
        row.names = FALSE
      )

    # Calculate ThHy-regime ----
    is_processed_pipe <- which(acceptor %in% ready_nodes)
    regime <-
      job_log[job_log[["job"]] == job_num &
                job_log[["aggregation"]] == opinion, ]
    checkmate::assert_true(all(acceptor[is_processed_pipe] %in% regime[["node"]]) &&
                             all(regime[["node"]] %in% acceptor[is_processed_pipe]))
    regime_index <-
      match(acceptor[is_processed_pipe], regime[["node"]])

    is_tp_sensored <- !is.na(regime[["temperature"]][regime_index])
    # *is_tp_sensored* also valid for pressure since paired NAs has been checked
    if (verbose)
      cat(
        sprintf(
          "\n%s %s | seen tracing; [%i/%i] are TP-sensor-equipped;",
          time_stamp_posixct,
          .func_name,
          sum(is_tp_sensored),
          length(is_tp_sensored)
        )
      )
    # * specific heat loss power tracing ----
    if (verbose)
      cat(sprintf("\n%s %s | tracing loss;;", time_stamp_posixct, .func_name))
    this_sender_loss <- rep.int(NA_real_, length(regime_index))

    if (any(is_tp_sensored)) {
      this_sender_loss[is_tp_sensored] <- loss[is_processed_pipe][is_tp_sensored]
      if (verbose)
        cat(
          sprintf(
            "\n%s %s | OK! Specific heat loss power traced from %i nodes;[%s];",
            time_stamp_posixct,
            .func_name,
            sum(is_tp_sensored),
            paste(regime[["node"]][regime_index][is_tp_sensored], collapse = ",")
          )
        )
    } else {
      if (verbose)
        cat(
          sprintf(
            "\n%s %s | SKIP! Specific heat loss power could not be traced from nodes [%s];; ",
            time_stamp_posixct,
            .func_name,
            paste(regime[["node"]][regime_index][!is_tp_sensored], collapse = ",")
          )
        )
    }

    # * heat flux tracing ----
    if (verbose)
      cat(sprintf("\n%s %s | tracing heat flux;;", time_stamp_posixct, .func_name))
    this_sender_flux <- rep.int(NA_real_, length(regime_index))

    if (any(is_tp_sensored)) {
      this_sender_flux[is_tp_sensored] <- pipenostics::flux_loss(
          x   =  this_sender_loss[is_tp_sensored],
          d   = d[is_processed_pipe][is_tp_sensored] * METER,
          wth = pipenostics::wth_d(d[is_processed_pipe][is_tp_sensored])
      )
      if (verbose)
        cat(
          sprintf(
            "\n%s %s | OK! Heat flux traced from %i nodes;[%s];",
            time_stamp_posixct,
            .func_name,
            sum(is_tp_sensored),
            paste(regime[["node"]][regime_index][is_tp_sensored], collapse = ",")
          )
        )
    } else {
      if (verbose)
        cat(
          sprintf(
            "\n%s %s | SKIP! Heat flux could not be traced from nodes [%s];; ",
            time_stamp_posixct,
            .func_name,
            paste(regime[["node"]][regime_index][!is_tp_sensored], collapse = ",")
          )
        )
    }

    # * Heat loss per day tracing ----
    if (verbose)
      cat(sprintf("\n%s %s | tracing heat loss per day;;", time_stamp_posixct, .func_name))
    this_sender_Q <- rep.int(NA_real_, length(regime_index))

    if (any(is_tp_sensored)) {
      this_sender_Q[is_tp_sensored] <- loss[is_processed_pipe][is_tp_sensored] * len[is_processed_pipe][is_tp_sensored] * DAY
      if (verbose)
        cat(
          sprintf(
            "\n%s %s | OK! Heat loss per day traced from %i nodes;[%s];",
            time_stamp_posixct,
            .func_name,
            sum(is_tp_sensored),
            paste(regime[["node"]][regime_index][is_tp_sensored], collapse = ",")
          )
        )
    } else {
      if (verbose)
        cat(
          sprintf(
            "\n%s %s | SKIP! Heat loss per day could not be traced from nodes [%s];; ",
            time_stamp_posixct,
            .func_name,
            paste(regime[["node"]][regime_index][!is_tp_sensored], collapse = ",")
          )
        )
    }


    # * Temperature drop ----
    if (verbose)
      cat(sprintf(
        "\n%s %s | tracing temperature;;",
        time_stamp_posixct,
        .func_name
      ))
    this_sender_temperature <- rep.int(NA_real_, length(regime_index))

    if (any(is_tp_sensored)) {
      this_sender_temperature[is_tp_sensored] <-
        regime[["temperature"]][regime_index][is_tp_sensored] + 
          pipenostics::dropt(
            regime[["temperature"]][regime_index][is_tp_sensored],
            regime[["pressure"]][regime_index][is_tp_sensored],
            regime[["flow_rate"]][regime_index][is_tp_sensored],
            this_sender_Q[is_tp_sensored]/DAY
          )
      if (verbose)
        cat(
          sprintf(
            "\n%s %s | OK! Temperature traced from %i nodes;[%s];",
            time_stamp_posixct,
            .func_name,
            sum(is_tp_sensored),
            paste(regime[["node"]][regime_index][is_tp_sensored], collapse = ",")
          )
        )
    } else {
      if (verbose)
        cat(
          sprintf(
            "\n%s %s | SKIP! Temperature could not be traced from nodes [%s];; ",
            time_stamp_posixct,
            .func_name,
            paste(regime[["node"]][regime_index][!is_tp_sensored], collapse = ",")
          )
        )
    }

    # * Pressure drop ----
    if (verbose)
      cat(sprintf(
        "\n%s %s | tracing pressure;;",
        time_stamp_posixct,
        .func_name
      ))
    this_sender_pressure <- rep.int(NA_real_, length(regime_index))

    if (any(is_tp_sensored)) {
      this_sender_pressure[is_tp_sensored] <-
        regime[["pressure"]][regime_index][is_tp_sensored] + pipenostics::dropp(
          regime[["temperature"]][regime_index][is_tp_sensored],
          regime[["pressure"]][regime_index][is_tp_sensored],
          regime[["flow_rate"]][regime_index][is_tp_sensored],
          d[is_processed_pipe][is_tp_sensored] * METER,
          len[is_processed_pipe][is_tp_sensored],
          roughness[is_processed_pipe][is_tp_sensored],
          inlet[is_processed_pipe][is_tp_sensored],
          outlet[is_processed_pipe][is_tp_sensored],
          method
        )
      cat(
        if (verbose)
          sprintf(
            "\n%s %s | OK! Pressure traced from %i nodes;[%s];",
            time_stamp_posixct,
            .func_name,
            sum(is_tp_sensored),
            paste(regime[["node"]][regime_index][is_tp_sensored], collapse = ",")
          )
      )
    } else {
      if (verbose)
        cat(
          sprintf(
            "\n%s %s | SKIP! Pressure could not be traced from nodes [%s];; ",
            time_stamp_posixct,
            .func_name,
            paste(regime[["node"]][regime_index][!is_tp_sensored], collapse = ",")
          )
        )
    }

    # * flow_rate loss ----
    if (verbose)
      cat(sprintf(
        "\n%s %s | tracing flow_rate;;",
        time_stamp_posixct,
        .func_name
      ))
    this_sender_flow_rate <-
      regime[["flow_rate"]][regime_index] + 0

    rm(regime_index, regime)

    # Log ThHy-regime ----
    job_log <- rbind(
      job_log,
      data.frame(
        node = sender[is_processed_pipe],
        tracing = acceptor[is_processed_pipe],
        backward = TRUE,
        aggregation = "identity",
        loss        = this_sender_loss,
        flux        = this_sender_flux,
        Q           = this_sender_Q,
        temperature = this_sender_temperature,
        pressure = this_sender_pressure,
        flow_rate = this_sender_flow_rate,
        # All above data is for the next job:
        job = job_num + 1L
      )
    )
    rm(
      this_sender_loss,
      this_sender_flux,
      this_sender_Q,
      this_sender_temperature,
      this_sender_pressure,
      this_sender_flow_rate,
      is_processed_pipe
    )

    # Recalculate node state ----
    node_state[is_node_ready] <- node_state[is_node_ready] - 1L
    n <- table(sender[acceptor %in% ready_nodes])
    node_state[names(n)] <- node_state[names(n)] - n
    rm(ready_nodes, n)
    if (verbose)
      cat(
        sprintf(
          "\n%s %s | finish job; job %i; processed node(s) %i",
          time_stamp_posixct,
          .func_name,
          job_num,
          sum(is_node_ready)
        )
      )
    job_num <- job_num + 1L
  }  # end while
  rm(is_node_ready)

  # Log last cycle data ----
  if (csv)
    utils::write.table(
      job_log[job_log[["job"]] == job_num &
                job_log[["node"]] %in% acceptor, ],
      file = file,
      append = TRUE,
      quote = FALSE,
      sep = ",",
      col.names = FALSE,
      row.names = FALSE
    )

  if (verbose)
    cat(sprintf(
      "\n%s %s | finish backward tracing;;\n",
      time_stamp_posixct,
      .func_name
    ))
  # Finish backward tracing ----
  job_log[job_log[["job"]] != job_num,]
}
