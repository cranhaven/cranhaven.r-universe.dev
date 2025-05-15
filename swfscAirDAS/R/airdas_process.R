#' Process aerial survey DAS data
#'
#' Process AirDAS data (the output of \code{\link{airdas_read}}), 
#'   including extracting state and condition information for each AirDAS event
#'
#' @param x an object of class \code{airdas_dfr} object, 
#'   an object that can be coerced to class \code{airdas_dfr},
#'   or a character (filepath) which is first passed to \code{\link{airdas_read}}
#' @param ... passed to \code{\link{airdas_read}} if \code{x} is a character.
#'   Otherwise ignored
#' @param days.gap.part numeric of length 1; 
#'   time gap (in days) used to identify when a 'partial reset' is performed, 
#'   i.e. when propagated info (weather, observers, etc) is reset. 
#'   Default is 30 minutes; must be less than or equal to \code{days.gap.full}
#' @param days.gap.full numeric of length 1; 
#'   time gap (in days) used to identify when a 'full reset; is performed, 
#'   i.e. when all info (transect number and propagated info) is reset. 
#'   Default is 12 hours; must be greater than \code{days.gap.part}
#' @param gap.message logical; default is \code{FALSE}.
#'   Indicates if messages should be printed detailing which row(s) of the 
#'   output data frame were partially or fully reset
#' @param reset.transect logical; default is \code{TRUE}.
#'   Indicates if propagated info (weather, observers, etc) should be reset 
#'   to \code{NA} when beginning a new transect. See Details section
#' @param trans.upper logical; indicates if all transect codes should be 
#'   capitalized using \code{\link[base:chartr]{toupper}}.
#'   Default is \code{FALSE}
#'
#' @details If \code{x} is a character, it is assumed to be a filepath and 
#'   first passed to \code{\link{airdas_read}}. 
#'   This output is then processed.
#'   
#'   This function cannot handle concatenated airdas_dfr objects of multiple file types. 
#'   In other words, AirDAS data must be processed and then concatenated.
#'      
#'   AirDAS data is event-based, meaning most events indicate when a state or weather condition changes.
#'   For instance, a 'W' event indicates when one or more weather conditions 
#'   (such as Beaufort sea state) change, and the weather conditions 
#'   are the same for subsequent events until the next 'W' event.
#'   For each state/condition: a new column is created, 
#'   the state/condition information is extracted from relevant events, 
#'   and extracted information is propagated to appropriate subsequent rows (events). 
#'   Thus, each row in the output data frame contains all 
#'   pertinent state/condition information for that row.
#'   
#'   The following assumptions/decisions are made during processing:
#'   \itemize{
#'     \item All '#' events (deleted events) are removed
#'     \item 'DateTime', 'Lat', and 'Lon' information are added to '1' events where applicable
#'     \item Effort is determined as follows: T/R events turns effort on, 
#'       and O/E events turn effort off. 
#'       T/R events themselves will be on effort, while O/E events will be off effort.
#'       The 'EffortDot' column is ignored
#'     \item 'HKR' values are converted to lower case. "Y" values are considered to be "H" values
#'     \item Observer ('ObsL', 'ObsB', 'ObsR', 'Rec') values are converted to lower case
#'     \item Viewing condition ('VLI', 'VLO', 'VB', 'VRI', 'VRO') values are converted to lower case
#'     \item Missing values are \code{NA} rather than \code{-1}
#'   }
#'   
#'   Normally, a T event (to indicate starting/resuming a transect)
#'   is immediately followed by a VPAW event series, creating a TVPAW event series.
#'   The \code{reset.transect} argument causes the conditions set in the VPAW event series
#'   (Beaufort, viewing conditions, altitude, etc.) to be reset to \code{NA} at each T event
#'   
#' @return An \code{airdas_df} object, which is also a data frame.
#'   It consists of the input data frame, i.e. the output of \code{\link{airdas_read}},
#'   with the following columns added:
#'   \tabular{lll}{
#'     \emph{State/condition}            \tab \emph{Column name} \tab \emph{Notes}\cr
#'     On/off effort                     \tab OnEffort\cr
#'     Transect code                     \tab Trans\cr
#'     Beaufort sea state                \tab Bft\cr
#'     Percent overcast (cloud cover)    \tab CCover\cr
#'     Jellyfish code                    \tab Jelly \tab not in PHOCOENA data\cr
#'     Horizontal sun (clock system)     \tab HorizSun\cr
#'     Vertical sun (clock system)       \tab VertSun \tab only in PHOCOENA data\cr
#'     Haze/Kelp/Red tide code           \tab HKR\cr
#'     Haze (from HKR code)              \tab Haze\cr
#'     Kelp (from HKR code)              \tab Kelp\cr
#'     Red tide (from HKR code)          \tab RedTide\cr
#'     Altitude (feet)                   \tab AltFt\cr
#'     Speed (knots)                     \tab SpKnot\cr
#'     Left observer                     \tab ObsL\cr
#'     Belly observer                    \tab ObsB\cr
#'     Right observer                    \tab ObsR\cr
#'     Data recorder                     \tab Rec\cr
#'     Viewing condition - left inside   \tab VLI\cr
#'     Viewing condition - left outside  \tab VLO\cr
#'     Viewing condition - belly         \tab VB\cr
#'     Viewing condition - right inside  \tab VRI\cr
#'     Viewing condition - right outside \tab VRO\cr
#'   }
#'   
#'   See \code{\link{airdas_format_pdf}} for which data columns the condition information 
#'   is extracted form for each file type. 
#'   In addition, warnings are printed with line numbers of unexpected event codes
#'
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' airdas_process(y, trans.upper = FALSE)
#' 
#' y.read <- airdas_read(y)
#' airdas_process(y.read)
#'
#' @export
airdas_process <- function(x, ...) UseMethod("airdas_process")


#' @name airdas_process
#' @export
airdas_process.character <- function(x, ...) {
  airdas_process(airdas_read(x, ...), ...)
}


#' @name airdas_process
#' @export
airdas_process.data.frame <- function(x, ...) {
  airdas_process(as_airdas_dfr(x), ...)
}


#' @name airdas_process
#' @export
airdas_process.airdas_dfr <- function(x, days.gap.part = 0.5/24, days.gap.full = 12/24, 
                                      gap.message = FALSE, reset.transect = TRUE, 
                                      trans.upper = FALSE, ...) 
{ 
  #----------------------------------------------------------------------------
  # Input checks
  file.type <- unique(x$file_type)
  if (length(file.type) != 1) 
    stop("airdas_process can only data of a single file type. ", 
         "Please process, and then concatenate files.")
  
  stopifnot(
    length(days.gap.part) == 1, 
    length(days.gap.full) == 1, 
    inherits(days.gap.part, c("integer", "numeric")), 
    inherits(days.gap.full, c("integer", "numeric")), 
    inherits(gap.message, "logical")
  )
  
  if (days.gap.part > days.gap.full) 
    stop("days.gap.part must be less than or equal to days.gap.full")
  
  # Check that there are no Event columns with NA is done in as_airdas_dfr
  
  
  #----------------------------------------------------------------------------
  # Remove '#' (deleted) events
  x <- x[x$Event != "#", ]
  rownames(x) <- NULL
  
  
  #----------------------------------------------------------------------------
  # Fill in Lat/Lon/DateTime of '1' events
  event.1 <- which(x$Event == 1)
  stopifnot(all(event.1 > 1))
  
  if (length(event.1) > 0) {
    x$Lat[event.1] <- x$Lat[event.1 - 1]
    x$Lon[event.1] <- x$Lon[event.1 - 1]
    x$DateTime[event.1] <- x$DateTime[event.1 - 1]
  }
  
  
  #----------------------------------------------------------------------------
  ### Determine new days for reset of columns
  nDAS <- nrow(x)
  
  dt.na <- is.na(x$DateTime)
  time.diff <- rep(NA, nDAS)
  time.diff[!dt.na] <- c(NA, abs(diff(x$DateTime[!dt.na]))) / (60*60*24)
  
  # Soft reset (not transect num) for time gaps > days.gap.part
  idx.reset.part <- c(1, which(time.diff > days.gap.part))
  # Full reset (including transect num) for time gaps > days.gap.full
  idx.reset.full <- c(1, which(time.diff > days.gap.full))
  
  if (gap.message) {
    message("A 'partial reset' was performed at the following row indicies ", 
            "of the output data frame: ", 
            idx.reset.part)
    message("A 'full reset' was performed at the following row indicies ", 
            "of the output data frame: ", 
            idx.reset.full)
  }
  
  if (!all(idx.reset.full %in% idx.reset.part)) {
    warning("Warning: not all full resets were in partial resets; ", 
            "check days.gap.. arguments?", immediate. = TRUE)
  }
  
  if (!all(which(!duplicated(x$file_das)) %in% idx.reset.part)) {
    warning("Warning: not all new flight row indices were new file indices", 
            immediate. = TRUE)
  }
  
  
  #----------------------------------------------------------------------------
  ### Create vectors with data where values change/are reset
  init.val <- as.numeric(rep(NA, nDAS))
  event.na <- -9999
  
  
  event.A <- x$Event == "A"
  event.E <- x$Event == "E"
  event.O <- x$Event == "O"
  event.P <- x$Event == "P"
  event.R <- x$Event == "R"
  event.T <- x$Event == "T"
  event.V <- x$Event == "V"
  event.W <- x$Event == "W"
  
  HKR      <- .process_chr(init.val, x, "Data1", event.W, event.na)
  CCover   <- .process_num(init.val, x, "Data2", event.W, event.na)
  Bft      <- .process_num(init.val, x, "Data3", event.W, event.na)
  Jelly    <- switch(file.type, 
                     caretta = .process_num(init.val, x, "Data4", event.W, event.na), 
                     turtle  = .process_num(init.val, x, "Data4", event.W, event.na), 
                     init.val)
  HorizSun <- switch(file.type, 
                     phocoena = .process_num(init.val, x, "Data4", event.W, event.na), 
                     caretta = .process_num(init.val, x, "Data5", event.W, event.na), 
                     turtle  = .process_num(init.val, x, "Data5", event.W, event.na), 
                     init.val)
  VertSun  <- switch(file.type, 
                     phocoena = .process_num(init.val, x, "Data5", event.W, event.na), 
                     init.val)
  
  ObsL <- .process_chr(init.val, x, "Data1", event.P, event.na)
  ObsB <- .process_chr(init.val, x, "Data2", event.P, event.na)
  ObsR <- .process_chr(init.val, x, "Data3", event.P, event.na)
  Rec <-  .process_chr(init.val, x, "Data4", event.P, event.na)
  
  AltFt  <- .process_num(init.val, x, "Data1", event.A, event.na)
  SpKnot <- .process_num(init.val, x, "Data2", event.A, event.na)
  
  VLI <- .process_chr(init.val, x, "Data1", event.V, event.na)
  VLO <- .process_chr(init.val, x, "Data2", event.V, event.na)
  VB  <- .process_chr(init.val, x, "Data3", event.V, event.na)
  VRI <- .process_chr(init.val, x, "Data4", event.V, event.na)
  VRO <- .process_chr(init.val, x, "Data5", event.V, event.na)
  
  Trans <- .process_chr(init.val, x, "Data1", event.T, event.na)
  if (trans.upper) Trans <- toupper(Trans)
  Trans[event.O] <- event.na
  
  Eff <- as.logical(init.val)
  Eff[idx.reset.full] <- FALSE # For resets immediately after O or E events
  Eff[event.O | event.E] <- FALSE
  Eff[event.T | event.R] <- TRUE
  
  # Additional processing done after for loop
  
  # Determine reset rows for effort reset
  idx.eff <- which(event.T)
  
  
  #----------------------------------------------------------------------------
  # Loop through data for 'carry-over info' that applies to subsequent events
  # idx.new.cruise always includes 1, so don't need to pre-set Last.. objects
  for (i in seq_len(nDAS)) {
    # Reset data when necessary
    if (i %in% idx.reset.part) {
      LastBft <- LastCCover <- LastJelly <- LastHorizSun <- LastVertSun <- LastHKR <-
        LastObsL <- LastObsB <- LastObsR <- LastRec <- LastAltFt <- LastSpKnot <-
        LastVLI <- LastVLO <- LastVB <- LastVRI <- LastVRO <- LastEff <- NA
    }
    
    # Reset transect number only when it is a new day;
    #   all of idx.reset.full are in idx.reset.part
    if (i %in% idx.reset.full) LastTrans <- NA
    
    if ((i %in% idx.eff) & reset.transect) {
      LastBft <- LastCCover <- LastJelly <- LastHorizSun <- LastVertSun <- LastHKR <-
        LastObsL <- LastObsB <- LastObsR <- LastRec <- LastAltFt <- LastSpKnot <-
        LastVLI <- LastVLO <- LastVB <- LastVRI <- LastVRO <- NA
    }
    
    # Set/pass along 'carry-over info'
    if (is.na(HKR[i]))      HKR[i] <- LastHKR           else LastHKR <- HKR[i]           #Haze/Kelp/Red tide
    if (is.na(CCover[i]))   CCover[i] <- LastCCover     else LastCCover <- CCover[i]     #Cloud cover
    if (is.na(Bft[i]))      Bft[i] <- LastBft           else LastBft <- Bft[i]           #Beaufort
    if (is.na(Jelly[i]))    Jelly[i] <- LastJelly       else LastJelly <- Jelly[i]       #Jellyfish
    if (is.na(HorizSun[i])) HorizSun[i] <- LastHorizSun else LastHorizSun <- HorizSun[i] # Horizontal sun
    if (is.na(VertSun[i]))  VertSun[i] <- LastVertSun   else LastVertSun <- VertSun[i]   # Vertical sun
    if (is.na(ObsL[i]))     ObsL[i] <- LastObsL         else LastObsL <- ObsL[i]         #Observer - left
    if (is.na(ObsB[i]))     ObsB[i] <- LastObsB         else LastObsB <- ObsB[i]         #Observer - belly
    if (is.na(ObsR[i]))     ObsR[i] <- LastObsR         else LastObsR <- ObsR[i]         #Observer - right
    if (is.na(Rec[i]))      Rec[i] <- LastRec           else LastRec <- Rec[i]           #Recorder
    if (is.na(AltFt[i]))    AltFt[i] <- LastAltFt       else LastAltFt <- AltFt[i]       #Altitude (ft)
    if (is.na(SpKnot[i]))   SpKnot[i] <- LastSpKnot     else LastSpKnot <- SpKnot[i]     #Speed (knots)
    if (is.na(VLI[i]))      VLI[i] <- LastVLI           else LastVLI <- VLI[i]           #Visibility - left inner
    if (is.na(VLO[i]))      VLO[i] <- LastVLO           else LastVLO <- VLO[i]           #Visibility - left outer
    if (is.na(VB[i]))       VB[i] <- LastVB             else LastVB <- VB[i]             #Visibility - belly
    if (is.na(VRI[i]))      VRI[i] <- LastVRI           else LastVRI <- VRI[i]           #Visibility - right inner
    if (is.na(VRO[i]))      VRO[i] <- LastVRO           else LastVRO <- VRO[i]           #Visibility - right outer
    if (is.na(Trans[i]))    Trans[i] <- LastTrans       else LastTrans <- Trans[i]       #Transect number
    if (is.na(Eff[i]))      Eff[i] <- LastEff           else LastEff <- Eff[i]           #Effort
  }
  
  # Post-processing
  tmp <- list(
    Bft = Bft, CCover = CCover, Jelly = Jelly, 
    HorizSun = HorizSun, VertSun = VertSun, HKR = HKR, 
    Haze = grepl("h", HKR, ignore.case = TRUE) | grepl("y", HKR, ignore.case = TRUE), 
    Kelp = grepl("h", HKR, ignore.case = TRUE), 
    RedTide = grepl("r", HKR, ignore.case = TRUE), 
    AltFt = AltFt, SpKnot = SpKnot, 
    ObsL = ObsL, ObsB = ObsB, ObsR = ObsR, Rec = Rec, 
    VLI = VLI, VLO = VLO, VB = VB, VRI = VRI, VRO = VRO, 
    Trans = Trans, OnEffort = Eff
  )
  
  # Replace event.reset values with NAs
  tmp <- lapply(tmp, function(j) {
    j[j == -9999] <- NA
    j
  })
  
  # Coerce NA OnEffort values to FALSE
  tmp$OnEffort <- ifelse(is.na(tmp$OnEffort), FALSE, tmp$OnEffort)
  
  # Convert values to lower case
  tmp$HKR <- tolower(tmp$HKR)
  
  tmp$ObsL <- tolower(tmp$ObsL)
  tmp$ObsB <- tolower(tmp$ObsB)
  tmp$ObsR <- tolower(tmp$ObsR)
  tmp$Rec  <- tolower(tmp$Rec)
  
  tmp$VLI <- tolower(tmp$VLI)
  tmp$VLO <- tolower(tmp$VLO)
  tmp$VB  <- tolower(tmp$VB)
  tmp$VRI <- tolower(tmp$VRI)
  tmp$VRO <- tolower(tmp$VRO)
  
  
  #----------------------------------------------------------------------------
  # Warning check for accepted event codes
  event.acc <- c("*", 1, "A", "C", "E", "O", "P", "R", "s", "S", "t", 
                 "T", "V", "W")
  if (!all(x$Event %in% event.acc))
    warning("This DAS data contains unexpected event codes, ",
            "which could break other package functions. ",
            "Please address this issue before continuing.\n",
            paste0("Expected event codes (case sensitive): ",
                   paste(event.acc, collapse = ", "), "\n"),
            "The following contain unexpected event codes:\n",
            .print_file_line(x$file_das, x$line_num, which(!(x$Event %in% event.acc))))
  
  
  #----------------------------------------------------------------------------
  cols.toreturn <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort", "Trans", 
    "Bft", "CCover", "Jelly", "HorizSun", "VertSun", 
    "HKR", "Haze", "Kelp", "RedTide", "AltFt", "SpKnot", 
    "ObsL", "ObsB", "ObsR", "Rec", "VLI", "VLO", "VB", "VRI", "VRO", 
    "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7",
    "EffortDot", "EventNum", "file_das", "line_num", "file_type"
  )
  
  as_airdas_df(
    data.frame(x, tmp, stringsAsFactors = FALSE, row.names = NULL) %>%
      select(!!cols.toreturn)
  )
}
