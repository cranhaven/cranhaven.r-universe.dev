#' @title
#'  Massively trace forwards thermal-hydraulic regime for district
#'  heating network
#'
#' @family Regime tracing
#'
#' @description
#'  Trace values of thermal-hydraulic regime (temperature, pressure,
#'  flow rate, and other) in the bunched pipeline along the flow direction using
#'  user-provided values of \emph{specific heat loss power}.
#'
#' @details
#'  They consider the topology of district heating network represented by
#'  \code{\link{m325testbench}}:
#'
#'  \figure{tracefw.png}
#'
#'  Tracing starts from sensor-equipped root node and goes forward, i.e along
#'  the flow direction. Function \code{\link{traceline}} serves under the
#'  hood for tracing identified linear segments from root node to every
#'  terminal node. Hence they only need root node to be equipped with sensors.
#'  Sensors at other nodes are redundant in forward tracing, since the tracing
#'  algorithm by no means consider them for tracing.
#'
#'  Moreover in the forward tracing algorithm they assume the flow of heat
#'  carrier is distributed proportionally to the cross-sectional area of the
#'  outgoing pipeline. Actually, a lot of reasons may cause significant
#'  deviations from this assumption. As a result, the sequence of paired
#'  backward/forward tracing may be divergent for regime parameters.
#'
#'  Though some input arguments are natively vectorized their individual values
#'  all relate to common part of district heating network, i.e. associated with
#'  common object. It is due to isomorphism between vector representation and
#'  directed graph of this network. For more details of isomorphic topology
#'  description see \code{\link{m325testbench}}.
#'
#'  They are welcome to couple the algorithm with functionality of
#'  \href{https://CRAN.R-project.org/package=data.table}{data.table}.
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
#'    Sensor-measured temperature of heat carrier (water) sensor-measured on
#'    the root node, [\emph{°C}].
#'    Use \code{NA_float_}s for nodes without temperature sensor.
#'    Type: \code{\link{assert_double}}.
#'
#' @param pressure
#'    Sensor-measured
#'    \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute pressure}
#'    of heat carrier (water) inside the pipe on the root node, [\emph{MPa}].
#'    Use \code{NA_float_}s for nodes without pressure sensor.
#'    Type: \code{\link{assert_double}}.
#'
#' @param flow_rate
#'    Sensor-measured amount of heat carrier (water) on root node that is
#'    transferred by pipe during a period, [\emph{ton/hour}].
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
#'    user-provided value of \emph{specific heat loss} power for each pipe in tracing
#'    path, [\emph{kcal/m/h}]. Values of the argument can be obtained experimentally,
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
#' @param elev_tol
#'    maximum allowed discrepancy between adjacent outlet and inlet elevations
#'    of two subsequent pipes in the traced path, [\emph{m}].
#'    Type: \code{\link{assert_number}}.
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
#' @param use_cluster
#'    utilize functionality of parallel processing on multi-core CPU.
#'    Type: \code{\link{assert_flag}}.
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
#'         \emph{Tracing job}. Identifier of tracing direction. It constantly
#'         equals to \code{FALSE}.
#'         Type: \code{\link{assert_logical}}.
#'       }
#'
#'       \item{\code{aggregation}}{
#'         \emph{Tracing job}. Identifier of the aggregation method associated
#'         with traced values. For forward tracing the only option is
#'         \code{identity}.
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
#'          For forward tracing value of \code{job} counts the number of traced paths from root node.
#'          Type: \code{\link{assert_count}}.
#'       }
#'  }
#'  Type: \code{\link{assert_data_frame}}.
#'
#' @examples
#' library(pipenostics)
#'
#' # Minimum two nodes should be in district heating network graph:
#' tracefw(verbose = FALSE)
#'
#' # Consider isomorphic representation of District Heating Network graph:
#' DHN <- pipenostics::m325testbench
#'
#' # * remove irrelevant parameters from the test bench
#' DHN[c("year", "insulation", "laying", "beta", "exp5k")] <- NULL
#' DHN[c("temperature", "pressure", "flow_rate")] <- NA_real_
#'
#' # * avoid using numeric identifiers for nodes:
#' DHN$sender   <- sprintf("N%02i", DHN$sender)
#' DHN$acceptor <- sprintf("N%02i", DHN$acceptor)
#'
#' # * alter units:
#' DHN$d <- 1e3 * DHN$d  # convert [m] to [mm]
#'
#' # * provide current regime parameters for root node
#' root_node <- 12
#' DHN[root_node, "temperature"] <-  70.4942576978  # [°C]
#' DHN[root_node, "pressure"]    <-   0.6135602014  # [MPa]
#' DHN[root_node, "flow_rate"]   <- 274.0           # [ton/hour]
#'
#' # * provide actual values of specific heat loss power, [kcal/m/h], for each
#' # segment N01 - N26. Since N12 is a root node, the specific heat loss
#' # power for this acceptor is set to 0 (or may be any other numeric value).
#' actual_loss <- c(
#'   96.8,  96.8,  71.2, 116.7, 71.3,  96.8, 78.5, 116.7, 28.6, 24.5, 
#'  116.7,   0.0, 153.2,  96.8, 96.8, 116.7, 24.5, 116.7, 28.6, 96.8, 
#'   78.5, 116.7,  71.3,  96.8, 96.8,  71.1
#' )
#'
#' # Trace the test bench forward for the first time:
#' fw_report <- do.call(
#'   "tracefw", c(as.list(DHN), list(loss = actual_loss), verbose = FALSE, elev_tol = .5)
#' )
#'
#' @export
tracefw <- function(sender = c(0, 1),
                    acceptor = c(1, 2),
                    temperature = c(70.0, NA_real_),
                    pressure = c(pipenostics::mpa_kgf(6), NA_real_),
                    flow_rate = c(20, NA_real_),
                    d = rep_len(100, 2),
                    len = rep_len(72.446, 2),
                    loss =  rep_len(78.4, 2),
                    roughness = rep_len(1e-3, 2),
                    inlet = c(.5, 1),
                    outlet = c(1.0, 1),
                    elev_tol = 0.1,
                    method = "romeo",
                    verbose = TRUE,
                    csv = FALSE,
                    file = "tracefw.csv",
                    use_cluster = FALSE) {
  # Perform forward tracing ----
  .func_name <- "tracefw"

  # Assertions ----
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
  checkmate::assert_flag(csv)
  if (csv) {
    checkmate::assert_character(
      basename(file),
      pattern = "^[[:alnum:]_\\.\\-]+$",
      any.missing = FALSE,
      len = 1
    )  # check for validness of file name!
    checkmate::assert_path_for_output(file)
  }

  # Configuration ----
  time_stamp_posixct <- Sys.time()

  if (verbose)
    cat(
      sprintf(
        "\n%s %s | start forward tracing; segments %i;",
        time_stamp_posixct,
        .func_name,
        n
      )
    )
  rm(n)

  # Compute discharges ----
  discharge <-
    structure(1 - d ^ 2 / tapply(d ^ 2, sender, sum)[sender], names = acceptor)

  # List search paths ----
  tracing_path <- pipenostics::flowls(sender, acceptor, use_cluster)
  checkmate::assert_list(
    tracing_path,
    types = "integerish",
    any.missing = FALSE,
    min.len = 1,
    max.len = length(acceptor)
  )

  root_node <- tracing_path[[1]][[1]]
  checkmate::assert_count(root_node, positive = TRUE)

  # Validate initial data ----
  checkmate::assert_double(temperature[[root_node]], any.missing = FALSE, len = 1L)
  checkmate::assert_double(pressure[[root_node]], any.missing = FALSE, len = 1L)
  checkmate::assert_double(flow_rate[[root_node]], any.missing = FALSE, len = 1L)


  job_log <- data.frame(
    node = acceptor[root_node],
    tracing = "sensor",
    backward = FALSE,
    aggregation = "identity",
    loss = NA_real_,
    flux = NA_real_,
    Q    = NA_real_,
    temperature = temperature[root_node],
    pressure = pressure[root_node],
    flow_rate = flow_rate[root_node],
    job = 0L
  )

  if (csv)
    utils::write.table(
      job_log,
      file = file,
      append = FALSE,
      quote = FALSE,
      sep = ",",
      col.names = TRUE,
      row.names = FALSE
    )

  # Trace searched paths ----
  for (job_num in seq_along(tracing_path)) {
    if (verbose)
      cat(
        sprintf(
          "\n%s %s | now process; %i node(s); [%s]",
          time_stamp_posixct,
          .func_name,
          length(tracing_path[[job_num]]),
          paste(acceptor[tracing_path[[job_num]]], collapse = ",")
        )
      )

    current_path <- tracing_path[[job_num]][-1]
    checkmate::assert_integer(
      current_path,
      lower = 1L,
      upper = length(acceptor),
      any.missing = FALSE,
      min.len = 1,
      max.len = length(acceptor),
      unique = TRUE
    )

    regime <- traceline(
      temperature[root_node],
      pressure[root_node],
      flow_rate[root_node],
      discharge[current_path],
      d[current_path],
      len[current_path],
      loss[current_path],
      roughness[current_path],
      inlet[current_path],
      outlet[current_path],
      elev_tol = elev_tol,
      method = method,
      forward = TRUE,
      absg = FALSE
    )
    regime <- as.data.frame(regime)
    regime[["node"]] <- acceptor[current_path]
    regime[["tracing"]] <- sender[current_path]
    regime[["backward"]] <- FALSE
    regime[["aggregation"]] <- "identity"
    regime[["job"]] <- job_num
    job_log <- rbind(job_log, regime)
    job_log <- job_log[!duplicated(job_log[,setdiff(colnames(job_log), "job")]), ]

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
  }

  if (verbose)
    cat(sprintf(
      "\n%s %s | finish forward tracing;;\n",
      time_stamp_posixct,
      .func_name
    ))
  job_log
}
