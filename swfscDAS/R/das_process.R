#' Process DAS data
#'
#' Process DAS data (the output of \code{\link{das_read}}),
#'   including extracting state and condition information for each DAS event
#'
#' @param x an object of class \code{das_dfr},
#'   an object that can be coerced to class \code{das_dfr},
#'   or a character (filepath) which is first passed to \code{\link{das_read}}
#' @param ... passed to \code{\link{das_read}} if \code{x} is a character.
#'   Otherwise ignored
#' @param days.gap numeric of length 1; default is \code{20}.
#'   Time gap (in days) used to identify a new cruise in concatenated DAS files,
#'   and thus also when state/condition information
#'   (cruise number, weather, Bft, Mode, etc) is reset
#' @param reset.event logical; default is \code{TRUE}.
#'   Indicates if state/condition information (weather, Bft, Mode, etc)
#'   should be reset to \code{NA}
#'   if there is an applicable event with an \code{NA} for that state/condition
#' @param reset.effort logical; default is \code{TRUE}.
#'   Indicates if state/condition information should be reset to \code{NA}
#'   when beginning a new continuous effort section. See Details section
#' @param reset.day logical; default is \code{TRUE}.
#'   Indicates if state/condition information should be reset to \code{NA}
#'   at the beginning of each day. This argument should only
#'   be set to \code{FALSE} for comparison with older methods, such as REPORT
#' @param add.dtll.sight logical indicating if the DateTime (dt)
#' and latitude and longitude (ll)
#'   columns should be added to the sighting events (?, 1, 2, 3, 4, 5, 6, 7, and 8)
#'   from the corresponding (immediately preceding) A event
#'
#' @details If \code{x} is a character,
#'   it is assumed to be a filepath and first passed to \code{\link{das_read}}.
#'   This output is then passed to \code{das_process}.
#'
#'   DAS data is event-based, meaning most events indicate when a
#'   state or weather condition changes.
#'   For instance, a 'V' event indicates when one or more sea state viewing conditions
#'   (such as Beaufort sea state) change, and these conditions
#'   are the same for subsequent events until the next 'V' event.
#'   For each state/condition: a new column is created,
#'   the state/condition information is extracted from relevant events,
#'   and extracted information is propagated to appropriate subsequent rows (events).
#'   Thus, each row in the output data frame contains all
#'   pertinent state/condition information for that row.
#'
#'   The following assumptions/decisions are made during processing:
#'   \itemize{
#'     \item Event codes are expected to be one of the following:
#'       #, *, ?, 1, 2, 3, 4, 5, 6, 7, 8, A, B, C, E, F, k, K, M, N,
#'       P, Q, r, R, s, S, t, V, W, g, G, p, X, Y, Z
#'     \item All '#' events (deleted events) are removed
#'     \item r events are converted to R events with non-standard effort;
#'       see \code{\link{das_format_pdf}} for more details
#'     \item An event is considered 'on effort' if it is 1) an R event,
#'       2) a B event immediately preceding an R event, or
#'       3) between corresponding R and E events (not including the E event).
#'       The 'EffortDot' column is not used when determining on effort data.
#'       Note that effort is reset to 'off effort' at the beginning of a new day.
#'     \item All state/condition information is reset at the beginning of each cruise.
#'       New cruises are identified using \code{days.gap}.
#'     \item All state/condition information relating to B, R, P, V, N, and W events
#'       are reset every time there is a BR event sequence if \code{reset.effort == TRUE},
#'       because in WinCruz a BR event sequence should always be a BRPVNW event sequence.
#'       An event sequence means that all of the events have the same Lat/Lon/DateTime info,
#'       and thus previous values for conditions set during the event sequence should not
#'       carry over to any part of the event sequence.
#'     \item 'OffsetGMT' is converted to an integer. Values are expected to be
#'       consistent within a day for each cruise, so events will have an
#'       OffsetGMT value if there is any B event with the offset data on
#'       the same day, whether that event is before or after the B event.
#'       Thus, if any date/cruise combinations have multiple OffsetGMT values
#'       in the data, then a warning message will be printed and the
#'       OffsetGMT values will be all NA (for the entire output).
#'
#'     \item 'Mode' is capitalized, and 'Mode' values of \code{NA} are assigned a value of "C"
#'     \item 'EffType' is capitalized, and values of \code{NA} are assigned a value of "S"
#'     \item 'ESWsides' represents the number of sides being searched during that effort section -
#'       a value of \code{NA} (for compatibility with older data) or "F" means 2 sides are being searched,
#'       and a value of "H" means 1 side is being searched.
#'       ESWsides will be \code{NA} for values that are not one of "F", \code{NA}, or "H"
#'     \item 'Glare': \code{TRUE} if 'HorizSun' is 11, 12 or 1 and 'VertSun' is 2 or 3,
#'       or if 'HorizSun' is 12 and 'VertSun' is 1;
#'       \code{NA} if 'HorizSun' or 'VertSun' is \code{NA};
#'       otherwise \code{FALSE}
#'     \item Missing values are \code{NA} rather than \code{-1}
#'   }
#'
#' @return A \code{das_df} object, which is also a data frame.
#'   It consists of the input data frame, i.e. the output of \code{\link{das_read}},
#'   with the following columns added:
#'   \tabular{lll}{
#'     \emph{State/condition}        \tab \emph{Column name} \tab \emph{Data source}\cr
#'     On/off effort                 \tab OnEffort  \tab B/R and E events\cr
#'     Cruise number                 \tab Cruise    \tab Event: B; Column: Data1\cr
#'     Effort mode                   \tab Mode      \tab Event: B; Column: Data2\cr
#'     GMT offset of DateTime data   \tab OffsetGMT \tab Event: B; Column: Data3\cr
#'     Effort type                   \tab EffType   \tab Event: R; Column: Data1\cr
#'     Number of sides with observer \tab ESWSide   \tab Event: R; Column: Data2\cr
#'     Course (ship direction)       \tab Course    \tab Event: N; Column: Data1\cr
#'     Speed (ship speed, knots)     \tab SpdKt     \tab Event: N; Column: Data2\cr
#'     Beaufort sea state            \tab Bft       \tab Event: V; Column: Data1\cr
#'     Swell height (ft)             \tab SwellHght \tab Event: V; Column: Data2\cr
#'     Wind speed (knots)            \tab WindSpdKt \tab Event: V; Column: Data5\cr
#'     Rain/fog/haze code            \tab RainFog   \tab Event: W; Column: Data1\cr
#'     Horizontal sun (clock system) \tab HorizSun  \tab Event: W; Column: Data2\cr
#'     Vertical sun (clock system)   \tab VertSun   \tab Event: W; Column: Data3\cr
#'     Glare                         \tab Glare     \tab HorizSun and VertSun\cr
#'     Visibility (nm)               \tab Vis       \tab Event: W; Column: Data5\cr
#'     Left observer                 \tab ObsL      \tab Event: P; Column: Data1\cr
#'     Data recorder                 \tab Rec       \tab Event: P; Column: Data2\cr
#'     Right observer                \tab ObsR      \tab Event: P; Column: Data3\cr
#'     Independent observer          \tab ObsInd    \tab Event: P; Column: Data4\cr
#'   }
#'
#'   OffsetGMT represents the difference in hours between the DateTime data
#'   (which should be in local time) and GMT (i.e., UTC).
#'
#'   Internal warning messages are printed with row numbers of the input file
#'   (NOT of the output data frame) of unexpected event codes and r events,
#'   as well as if there is are potential issues with the number and/or order
#'   of R and E events
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' das_process(y)
#'
#' y.read <- das_read(y)
#' das_process(y.read)
#' das_process(y.read, reset.effort = FALSE)
#'
#' @export
das_process <- function(x, ...) UseMethod("das_process")


#' @name das_process
#' @export
das_process.character <- function(x, ...) {
  das_process(das_read(x, ...), ...)
}


#' @name das_process
#' @export
das_process.data.frame <- function(x, ...) {
  das_process(as_das_dfr(x), ...)
}


#' @name das_process
#' @export
das_process.das_dfr <- function(x, days.gap = 20, reset.event = TRUE,
                                reset.effort = TRUE, reset.day = TRUE,
                                add.dtll.sight = TRUE, ...)
{
  #----------------------------------------------------------------------------
  # Check that there are no Event columns with NA
  #   This is here rather than as_das_dfr b/c users should still be
  #   able to get the das_read output
  if (any(is.na(x$Event)))
    stop("To be processed, the Event column must not contain any NAs. ",
         "Should you use the 'skip' argument in das_read? ",
         "The following contain NA Event entries:\n",
         .print_file_line(x$file_das, x$line_num, which(is.na(x$Event))))

  # Prep 1
  x <- x[x$Event != "#", ]
  rownames(x) <- NULL # for debugging purposes


  #----------------------------------------------------------------------------
  ### Input checks
  stopifnot(
    inherits(days.gap, c("integer", "numeric")),
    length(days.gap) == 1,
    days.gap > 0,
    inherits(reset.day, "logical")
  )

  # Check event codes
  event.acc <- c("*", "?", 1:8, "A", "B", "C", "E", "F", "k", "K", "M", "N",
                 "P", "Q", "r", "R", "s", "S", "t", "V", "W",
                 "G", "g", "p", "X", "Y", "Z")
  if (!all(x$Event %in% event.acc))
    warning("This DAS data contains unexpected event codes, ",
            "which could break other package functions. ",
            "Please address this issue before continuing.\n",
            paste0("Expected event codes (case sensitive): ",
                   paste(event.acc, collapse = ", "), "\n"),
            "The following contain unexpected event codes:\n",
            .print_file_line(x$file_das, x$line_num, which(!(x$Event %in% event.acc))),
            immediate. = TRUE)


  #----------------------------------------------------------------------------
  # Prep
  ### 'Convert' r events to R events
  if (any(x$Event == "r")) {
    x.r.which <- which(x$Event == "r")
    warning("The provided file contains 'r' events. Is this on purpose? ",
            "These events will be CONVERTED to R events with non-standard effort, ",
            "i.e. with an \"N\" value in the Data1 column, in the output. ",
            paste("There are", length(x.r.which), "r events at the following:\n"),
            .print_file_line(x$file_das, x$line_num, which(x$Event == "r")),
            immediate. = TRUE)

    x$Data1[x.r.which] <- "N"
    x$Event[x.r.which] <- "R"
  }

  ### Determine effort using B/R and E events
  nDAS <- nrow(x)

  idx.B <- which(x$Event == "B")
  idx.R <- which(x$Event == "R")
  idx.E <- which(x$Event == "E")

  if (length(idx.E) != length(idx.R)) {
    warning("There are not an equal number of 'R' and 'E' events in the ",
            "provided DAS file; should this be fixed before processing?")
  } else if (!all(idx.E - idx.R > 0) | !all(head(idx.E, -1) < idx.R[-1])) {
    warning("Error: Not all 'R' events are followed by 'E' events before ",
            "another R event; should this be fixed before processing?")
  }


  #----------------------------------------------------------------------------
  # Determine 'reset' rows, i.e. rows that mark a new cruise in concatenated
  #   DAS file, or a new day for conditions
  dt.na <- is.na(x$DateTime)

  ### Determine indices where time-date change is by more than 'day.gap' days
  ###   Used to recognize data from new cruise in concatenated DAS files
  time.diff <- rep(NA, nDAS)
  time.diff[!dt.na] <- c(NA, abs(diff(x$DateTime[!dt.na]))) / (60*60*24)

  idx.new.cruise <- c(1, which(time.diff > days.gap))

  ### Determine row numbers of new days in x;
  ###   these will include idxs of new cruises. Used when reset.day is TRUE
  idx.nona.new <- which(diff(day(x$DateTime)[!dt.na]) != 0) + 1
  idx.new.day <- c(1, seq_len(nDAS)[!dt.na][idx.nona.new])

  if (!all(idx.new.cruise %in% idx.new.day))
    warning("Warning: not all new cruises row indices were new day indices - ",
            "is the data formatted correctly?",
            immediate. = TRUE)


  #----------------------------------------------------------------------------
  ### Extract GMT Offsets for dates + cruise numbers
  x.offset <- x %>%
    filter(.data$Event == "B") %>%
    mutate(Date = as.Date(.data$DateTime),
           Cruise = as.numeric(.data$Data1),
           OffsetGMT = as.integer(.data$Data3)) %>%
    select(.data$Date, .data$Cruise, .data$OffsetGMT) %>%
    distinct()

  x.offset.summ <- x.offset %>%
    group_by(.data$Date, .data$Cruise) %>%
    summarise(offsetgmt_uq = n_distinct(.data$OffsetGMT))



  if (any(x.offset.summ$offsetgmt_uq != 1)) {
    x.offset.mult <- x.offset.summ %>% filter(.data$offsetgmt_uq > 1)
    warning("The following dates + cruise numbers have multiple OffsetGMT ",
            "values (Field 3 of B events), and thus the OffsetGMT column ",
            "will contain only NAs in the output data frame:\n",
            paste(paste(x.offset.mult$Date, x.offset.mult$Cruise, sep = " + "),
                  collapse = "\n"),
            immediate. = TRUE)
    x.offset$OffsetGMT <- NA_integer_
  }


  #----------------------------------------------------------------------------
  # Add columns for helpful info that is pertinent for a series of records,
  #   such as Beaufort but not sigting cue

  #--------------------------------------------------------
  ### Create vectors with data where values change/are reset
  event.B <- x$Event == "B"
  event.N <- x$Event == "N"
  event.R <- x$Event == "R"
  event.E <- x$Event == "E"
  event.V <- x$Event == "V"
  event.W <- x$Event == "W"
  event.P <- x$Event == "P"

  event.B.preR <- (x$Event == "B") & (c(x$Event[-1], NA) == "R")

  init.val <- as.numeric(rep(NA, nDAS))
  event.na <- ifelse(reset.event, -9999, NA)

  Cruise    <- .process_num(init.val, x, "Data1", event.B, event.na)
  Mode      <- .process_chr(init.val, x, "Data2", event.B, "C")
  EffType   <- .process_chr(init.val, x, "Data1", event.R, "S")
  ESWsides  <- .process_chr(init.val, x, "Data2", event.R, "F")

  Course    <- .process_num(init.val, x, "Data1", event.N, event.na)
  SpdKt     <- .process_num(init.val, x, "Data2", event.N, event.na)
  Bft       <- .process_num(init.val, x, "Data1", event.V, event.na)
  SwellHght <- .process_num(init.val, x, "Data2", event.V, event.na)
  WindSpdKt <- .process_num(init.val, x, "Data5", event.V, event.na)
  RainFog   <- .process_num(init.val, x, "Data1", event.W, event.na)
  HorizSun  <- .process_num(init.val, x, "Data2", event.W, event.na)
  VertSun   <- .process_num(init.val, x, "Data3", event.W, event.na)
  Vis       <- .process_num(init.val, x, "Data5", event.W, event.na)

  ObsL <- .process_chr(init.val, x, "Data1", event.P, event.na)
  Rec  <- .process_chr(init.val, x, "Data2", event.P, event.na)
  ObsR <- .process_chr(init.val, x, "Data3", event.P, event.na)
  ObsInd <-  .process_chr(init.val, x, "Data4", event.P, event.na)

  Eff <- as.logical(init.val)
  Eff[sort(unique(c(idx.new.cruise, idx.new.day)))] <- FALSE
  Eff[event.B.preR | event.R] <- TRUE
  Eff[event.E] <- FALSE

  # Additional processing done after for loop

  # Determine reset rows for effort reset
  idx.eff <- sort(unique(c(which(event.B.preR), idx.R)))


  #--------------------------------------------------------
  ### Loop through data for 'carry-over info' that applies to subsequent events
  # idx.new.cruise always includes 1, so don't need to pre-set Last.. objects
  for (i in 1:nDAS) {
    # Reset all info when starting data for a new cruise
    if (i %in% idx.new.cruise) {
      LastEff <- LastEMode <- LastEType <- LastESW <-
        LastCourse <- LastSpdKt <-
        LastBft <- LastSwH <- LastWSpdKt <-
        LastRF <- LastHS <- LastVS <- LastVis <-
        LastObsL <- LastRec <- LastObsR <- LastObsInd <-
        LastCruise <- NA
    }

    # Reset applicable info (aka all but 'LastCruise') when starting a new day
    if ((i %in% idx.new.day) & reset.day) {
      LastEff <- LastEMode <- LastEType <- LastESW <-
        LastCourse <- LastSpdKt <-
        LastBft <- LastSwH <- LastWSpdKt <-
        LastRF <- LastHS <- LastVS <- LastVis <-
        LastObsL <- LastRec <- LastObsR <- LastObsInd <- NA
    }

    # Reset applicable info (all RPVNW-related) when starting BR/R event sequence
    if ((i %in% idx.eff) & reset.effort) {
      LastEType <- LastESW <-  LastCourse <- LastSpdKt <-
        LastBft <- LastSwH <- LastWSpdKt <-
        LastRF <- LastHS <- LastVS <- LastVis <-
        LastObsL <- LastRec <- LastObsR <- LastObsInd <- NA
    }

    # Set/pass along 'carry-over info'
    if (is.na(Cruise[i]))    Cruise[i] <- LastCruise else LastCruise <- Cruise[i] #Cruise
    if (is.na(Mode[i]))      Mode[i] <- LastEMode    else LastEMode <- Mode[i]    #Mode
    if (is.na(Course[i]))    Course[i] <- LastCourse else LastCourse <- Course[i] #Course
    if (is.na(SpdKt[i]))     SpdKt[i] <- LastSpdKt   else LastSpdKt <- SpdKt[i]   #Speed
    if (is.na(EffType[i]))   EffType[i] <- LastEType else LastEType <- EffType[i] #Effort type
    if (is.na(ESWsides[i]))  ESWsides[i] <- LastESW  else LastESW <- ESWsides[i]  #Sides being surveyed
    if (is.na(Bft[i]))       Bft[i] <- LastBft       else LastBft <- Bft[i]       #Beaufort
    if (is.na(SwellHght[i])) SwellHght[i] <- LastSwH else LastSwH <- SwellHght[i] #Swell height
    if (is.na(WindSpdKt[i])) WindSpdKt[i] <- LastWSpdKt else LastWSpdKt <- WindSpdKt[i] #Wind speed
    if (is.na(RainFog[i]))   RainFog[i] <- LastRF    else LastRF <- RainFog[i]    #Rain or fog
    if (is.na(HorizSun[i]))  HorizSun[i] <- LastHS   else LastHS <- HorizSun[i]   #Horizontal sun
    if (is.na(VertSun[i]))   VertSun[i] <- LastVS    else LastVS <- VertSun[i]    #Vertical sun
    if (is.na(Vis[i]))       Vis[i] <- LastVis       else LastVis <- Vis[i]       #Visibility
    if (is.na(ObsL[i]))      ObsL[i] <- LastObsL     else LastObsL <- ObsL[i]     #Left obs
    if (is.na(Rec[i]))       Rec[i] <- LastRec       else LastRec <- Rec[i]       #Recorder
    if (is.na(ObsR[i]))      ObsR[i] <- LastObsR     else LastObsR <- ObsR[i]     #Right obs
    if (is.na(ObsInd[i]))    ObsInd[i] <- LastObsInd else LastObsInd <- ObsInd[i] #Independent obs
    if (is.na(Eff[i]))       Eff[i] <- LastEff       else LastEff <- Eff[i]       #Effort
  }


  #--------------------------------------------------------
  ### Post-processing
  tmp <- list(
    Cruise = Cruise, Mode = Mode,
    EffType = EffType, ESWsides = ESWsides, Course = Course, SpdKt = SpdKt,
    Bft = Bft, SwellHght = SwellHght, WindSpdKt = WindSpdKt,
    RainFog = RainFog, HorizSun = HorizSun, VertSun = VertSun, Vis = Vis,
    OnEffort = Eff,
    ObsL = ObsL, Rec = Rec, ObsR = ObsR, ObsInd = ObsInd
  )

  # Replace event.reset values with NAs
  if (reset.event) {
    tmp <- lapply(tmp, function(j) {
      j[j == -9999] <- NA
      j
    })
  }

  # Post-for loop variable processing
  tmp$Glare <- ifelse(
    is.na(tmp$HorizSun) | is.na(tmp$VertSun), NA,
    (tmp$HorizSun %in% c(11, 12, 1) & tmp$VertSun %in% c(2, 3)) |
      (tmp$HorizSun %in% 12 & tmp$VertSun %in% 1)
  ) #Per JVR notes, NA Glare should be FALSE to match Abund

  tmp$Mode <- as.character(toupper(tmp$Mode))
  tmp$EffType <- as.character(tmp$EffType)
  tmp$ESWsides <- case_when(tmp$ESWsides == "F" ~ 2, tmp$ESWsides == "H" ~ 1)
  # tmp$RainFog <- as.logical(ifelse(is.na(tmp$RainFog), NA, tmp$RainFog %in% c(2:4)))

  # In case any are all NAs
  tmp$ObsL <- as.character(tmp$ObsL)
  tmp$Rec <- as.character(tmp$Rec)
  tmp$ObsR <- as.character(tmp$ObsR)
  tmp$ObsInd <- as.character(tmp$ObsInd)


  #----------------------------------------------------------------------------
  # Add DateTime/Lat/Lon info to ?, 1, 2, 3, 4, 5, 6, 7, 8 events
  if (add.dtll.sight) {
    event.tmp <- c("?", 1:8)
    x$a_idx <- cumsum(x$Event == "A")
    x$idx <- seq_along(x$Event)
    x.key <- x %>%
      filter(.data$Event == "A") %>%
      select(.data$a_idx, .data$DateTime, .data$Lat, .data$Lon)
    x.tmp <- x %>%
      filter(.data$Event %in% event.tmp) %>%
      select(-.data$DateTime, -.data$Lat, -.data$Lon) %>%
      left_join(x.key, by = "a_idx") %>%
      select(!!names(x))

    x <- x %>%
      filter(!(.data$Event %in% event.tmp)) %>%
      bind_rows(x.tmp) %>%
      arrange(.data$idx) %>%
      select(-.data$idx, -.data$a_idx)
    rm(x.key, x.tmp)
  }


  #----------------------------------------------------------------------------
  ### Create and order data frame to return
  cols.tokeep <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort",
    "Cruise", "Mode", "OffsetGMT", "EffType", "ESWsides", "Course", "SpdKt",
    "Bft", "SwellHght", "WindSpdKt",
    "RainFog", "HorizSun", "VertSun", "Glare", "Vis",
    "ObsL", "Rec", "ObsR", "ObsInd",
    paste0("Data", 1:12), "EffortDot", "EventNum", "file_das", "line_num"
  )

  data.frame(x, tmp, stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(.data$DateTime)) %>%
    left_join(x.offset, by = c("Cruise", "Date")) %>%
    select(!!cols.tokeep) %>%
    as_das_df()
}




#' Internal functions for swfscAirDAS
#'
#' These functions are exported only to be used internally by swfscAirDAS.
#' They implement functionality that is used when processing both
#' DAS and AirDAS data
#'
#' @name swfscAirDAS-internals
#' @param init.val ignore
#' @param das.df ignore
#' @param col.name ignore
#' @param event.curr ignore
#' @param event.na ignore
#' @export
.process_num <- function(init.val, das.df, col.name, event.curr, event.na) {
  # Helper function for _process function - extract numeric values
  toreturn <- init.val
  toreturn[event.curr] <- ifelse(
    is.na(as.numeric(das.df[event.curr, col.name])),
    event.na, as.numeric(das.df[event.curr, col.name])
  )

  toreturn
}

#' @name swfscAirDAS-internals
#' @export
.process_chr <- function(init.val, das.df, col.name, event.curr, event.na) {
  # Helper function for _process function - extract character values
  toreturn <- init.val
  toreturn[event.curr] <- ifelse(
    is.na(das.df[event.curr, col.name]),
    event.na, das.df[event.curr, col.name]
  )

  toreturn
}
