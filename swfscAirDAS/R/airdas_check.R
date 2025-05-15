#' Check AirDAS file
#'
#' Check that AirDAS file has accepted formatting and values
#'
#' @param file filename(s) of one or more AirDAS files
#' @param file.type character; indicates the program used to create \code{file}.
#'   Must be one of: "turtle", "caretta", "survey", or "phocoena" (case
#'   sensitive). Default is "turtle". Passed to \code{\link{airdas_read}}
#' @param skip integer: see \code{\link[readr]{read_fwf}}. Default is 0. Passed
#'   to \code{\link{airdas_read}}
#' @param file.out character; filename to which to write the error log. Should
#'   be a text or CSV file. Default is \code{NULL}
#' @param sp.codes character; filename of .dat file from which to read accepted
#'   species codes. If \code{NULL}, default (internal) file will be used.
#'   Default is \code{NULL}
#' @param print.transect logical; indicates if a table with all the transect
#'   numbers in the \code{x} should be printed using \code{\link[base]{table}}.
#'   Default is \code{TRUE}
#'
#' @details The default (internal) \code{sp.codes} file is located at
#' \code{system.file("SpCodesAirDAS.dat", package = "swfscAirDAS")}.
#'
#' To see the checks performed by this function, you can access the PDF locally
#' at \code{system.file("AirDAS_check.pdf", package = "swfscAirDAS")}, or online
#' at
#' \url{https://github.com/swfsc/swfscAirDAS/blob/master/inst/AirDAS_check.pdf}
#'
#' Checks that are not done by this function that may be of interest:
#' \itemize{
#'   \item Check for valid fish ball/mola/jelly/crab pot codes
#'   \item Check that datetimes are sequential, meaning they 
#'     1) are the same as or 2) come after the previous event
#' }
#'
#' @return A data frame with five columns that list information about errors
#' found in the AirDAS files: the file name, line number, index (row number)
#' from the \code{airdas_read(file)} data frame, 'ID' (pre-Data# columns from
#' the DAS file), and description of the issue. This data frame is sorted by the
#' 'Description' column. If there are multiple issues with the same line, the
#' issue descriptions are concatenated together using \code{paste(..., collapse
#' = "; ")}
#'
#' If \code{print.transect} is \code{TRUE}, then the output of
#' \code{table(x$Data1[x$Event == "T"], useNA = "always")}, where \code{x} is
#' the output of \code{airdas_read(file, ...)} is printed
#'
#' If \code{file.out} is not \code{NULL}, then the error log is also written to
#' the file (e.g., a .txt or .csv file) specified by \code{file.out}
#'
#' @seealso \url{https://swfsc.github.io/swfscAirDAS/}
#'
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' if (interactive()) airdas_check(y, print.transect = TRUE)
#'
#' @export
airdas_check <- function(file, file.type = c("turtle", "caretta", "phocoena"),  
                         skip = 0, file.out = NULL, 
                         sp.codes = NULL, print.transect = TRUE) {
  
  file.type <- match.arg(file.type)
  
  if (length(unique(file)) != length(file))
    warning("Not all files are unique - this likely will cause an error in ", 
            "airdas_check. Please ensure all files are unique.", 
            immediate. = TRUE)
  
  #----------------------------------------------------------------------------
  ### Read and process file
  error.out <- data.frame(
    File = NA, LineNum = NA, Idx = NA, ID = NA, Description = NA,
    stringsAsFactors = FALSE
  )
  
  message("Reading AirDAS data")
  x <- suppressWarnings(airdas_read(file, file.type = file.type, skip = skip))
  x$idx <- seq_along(x$Event)
  x <- as_airdas_dfr(x)
  
  id.lines.idx <- switch(file.type, caretta = 39, phocoena = 45, turtle = 39)
  x.lines.list <- lapply(file, function(i) {
    if (skip > 0) readLines(i)[-c(1:skip)] else readLines(i)
  })
  x.lines <- substr(do.call(c, x.lines.list), 1, id.lines.idx)
  
  message("Processing AirDAS data")
  x.proc <- suppressWarnings(airdas_process(x)) %>% 
    left_join(select(x, .data$file_das, .data$line_num, .data$idx), 
              by = c("file_das", "line_num"))
  x.proc <- as_airdas_df(x.proc)
  
  
  if ((nrow(x) != length(x.lines)) | (nrow(x) < nrow(x.proc)))
    stop("Error reading and processing AirDAS files. ", 
         "Please try checking only a single file, or contact the developer")
  
  rm(id.lines.idx, x.lines.list)
  
  
  #----------------------------------------------------------------------------
  ### Process sp.codes file
  message("Reading and processing SpCodes file")
  if (is.null(sp.codes)) 
    sp.codes <- system.file("SpCodesAirDAS.dat", package = "swfscAirDAS")
  
  sp.acc.df <- read_fwf(
    sp.codes, 
    col_positions = fwf_positions(start = c(1, 10, 43), end = c(6, 42, NA)),
    col_types = cols(.default = col_character()),
    trim_ws = TRUE, skip = 3, skip_empty_rows = FALSE
  )
  
  sp.acc <- sp.acc.df[[1]]
  sp.acc.all <- c(sp.acc, toupper(sp.acc))
  
  if (!all(nchar(sp.acc) == 2))
    warning("Some species codes from sp.codes are not two charcters. ", 
            "Did you load the correct species code .dat file?", 
            immediate. = TRUE)
  
  
  #----------------------------------------------------------------------------
  message("Checking DAS file")
  
  ### Check event codes
  event.acc <- c("#", "*", 1, "A", "C", "E", "O", 
                 "P", "R", "s", "S", "t", "T", "V", "W")
  ev.which <- which(!(x$Event %in% event.acc))
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, ev.which, "The event code is not recognized")
  )
  
  ### Check lat/lon coordinates - coords added to 1 events in processed data
  lat.which <- which(!between(x.proc$Lat, -90, 90))
  lon.which <- which(!between(x.proc$Lat, -180, 1800))
  
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, lat.which, "The latitude value is not between -90 and 90"), 
    .check_list(x, x.lines, lon.which, "The longitude value is not between -180 and 180")
  )
  
  
  #----------------------------------------------------------------------------
  ### Check that effort dot matches effort determined by T/R to E/O events
  ###   PHOCOENA data has no effort dots
  if (file.type != "phocoena") {
    x.proc.no1 <- x.proc[x.proc$Event != 1, ] #1 events do not have effort dots
    edot.which <- x.proc.no1$idx[(x.proc.no1$OnEffort != x.proc.no1$EffortDot)]
    
    error.out <- rbind(
      error.out,
      .check_list(x, x.lines, edot.which, 
                  "Effort dot does not match T/R to O/E effort")
    )
    rm(x.proc.no1)
  }
  
  
  #----------------------------------------------------------------------------
  ### Check that: 
  ###   1) there are not consecutive T or O events
  x.trans <- x[x$Event %in% c("T", "O"), ]
  x.trans.str <- paste(x.trans$Event, collapse = "")
  tt.str.which <- gregexpr("TT", x.trans.str)[[1]]
  oo.str.which <- gregexpr("OO", x.trans.str)[[1]]
  
  t.which <- x.trans$idx[tt.str.which[tt.str.which != -1] + 1]
  o.which <- x.trans$idx[oo.str.which[oo.str.which != -1] + 1]
  
  ### 2) data is OFF effort before a T or R event
  # idx.tr <- which(x$Event %in% c("T", "R"))
  # x.all.preTR <- x.all[idx.tr-1, ]
  # tr.which <- x.all.preTR$idx[x.all.preTR$OnEffort] + 1
  idx.proc.tr <- which(x.proc$Event %in% c("T", "R"))
  x.proc.preTR <- x.proc[idx.proc.tr - 1, ]
  tr.which <- x.proc.preTR$idx[x.proc.preTR$OnEffort] + 1
  
  ### 3) data is ON effort before an E event (O event after E is ok)
  idx.proc.e <- which(x.proc$Event == "E")
  x.proc.preE <- x.proc[idx.proc.e - 1, ]
  e.which <- x.proc.preE$idx[!x.proc.preE$OnEffort] + 1
  
  rm(x.trans, x.trans.str, tt.str.which, oo.str.which, 
     x.proc.preTR, x.proc.preE)
  
  
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, t.which, 
                paste("Duplicate T event: there is no O event between", 
                      "this T event and the previous T event")), 
    .check_list(x, x.lines, o.which, 
                paste("Duplicate O event: there is no T event between", 
                      "this O event and the previous O event")), 
    .check_list(x, x.lines, tr.which, 
                "Attempt to resume effort (T/R event) when already on effort"),
    .check_list(x, x.lines, e.which, 
                "Attempt to end effort (E event) when already off effort")
  )
  
  
  #----------------------------------------------------------------------------
  ### Check that file ends off effort, and after an "O" rather than "E" event
  idx.proc.o <- which(x.proc$Event == "O")
  if (length(idx.proc.e) == 0) idx.proc.e <- 0 # In case file has no E events
  if (tail(x.proc, 1)$OnEffort | (tail(idx.proc.o, 1) < tail(idx.proc.e, 1))) {
    end.which <- nrow(x)
    
    error.out <- rbind(
      error.out,
      .check_list(x, x.lines, end.which, 
                  paste("The file ends on effort, and/or the", 
                        "last off effort event is an E rather than O"))
    )
  }
  
  
  #----------------------------------------------------------------------------
  ### Check that Data# columns are right-justifed for selected events
  # *, R, E, O events should only have NAs (no data) - this is checked next
  event.tofilt <- c("C", "*", "R", "E", "O", "#")
  x.filt <- x %>% 
    filter(!(.data$Event %in% event.tofilt))
  
  call.read <- switch(
    file.type, phocoena = .airdas_read_phocoena, survey = .airdas_read_survey,
    caretta = .airdas_read_turtle, turtle = .airdas_read_turtle
  )
  
  x.tmp.filt.data <- do.call(
    rbind, 
    lapply(file, function(i) {
      call.read(i, skip = skip, tz = "UTC", file.type = file.type)
    })
  ) %>% 
    select(-.data$DateTime) %>% 
    filter(!(.data$Event %in% event.tofilt)) %>% 
    select(starts_with("Data")) %>% 
    mutate(Data7 = substr(.data$Data7, 1, 5))
  
  x.tmp.which <- lapply(1:7, function(i) {
    x1 <- trimws(x.tmp.filt.data[[i]], which = "left")
    x2 <- trimws(x.tmp.filt.data[[i]], which = "both")
    which(x1 != x2)
  })
  
  r.which <- x.filt$idx[sort(unique(unlist(x.tmp.which)))]
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, r.which, "Data column(s) are not right-justified")
  )
  
  rm(event.tofilt, x.filt, call.read, x.tmp.filt.data, x.tmp.which)
  
  
  #----------------------------------------------------------------------------
  ### Print transect codes
  if (print.transect) {
    cat("Transects:", collapse = "")
    print(table(x$Data1[x$Event == "T"], useNA = "always"))
  }
  
  
  #----------------------------------------------------------------------------
  #----------------------------------------------------------------------------
  ### Check that value type of values in Data# columns are as expected for
  ###   columns that are added in das_process
  ###   Note that for fields with specific number requirements, 
  ###   a separate check_numeric() is not necessary
  
  # If PHOCOENA data, nothing in Data6-7 columns for all but C events
  txt.na.phoc <- paste("For PHOCOENA data, Data6 and Data7 columns should be NA", 
                       "for all but C events")
  idx.na.phoc <- if (file.type == "phocoena") {
    .check_isna(x.proc, event.acc[!(event.acc %in% "C")], paste0("Data", 6:7))
  } else {
    integer(0)
  }
  
  # "*", R, E, and O events have no data
  idx.na <- .check_isna(x.proc, c("*", "R", "E", "O"), paste0("Data", 1:7))
  txt.na <- "R, E, O, and * events should have no data in the Data# columns"
  
  
  # Viewing conditions
  patt <- c("e", "g", "o", "p")
  idx.view <- .check_character(x.proc, "V", paste0("Data", 1:5), c(patt, toupper(patt)), 2)
  txt.view <- "Viewing conditions (Data1-5 of V events) must be one of: e, g, p, or o"
  rm(patt)
  
  # Altitude and altitude
  idx.alt <- .check_numeric(x.proc, "A", "Data1")
  txt.alt <- "Altitude (Data1 of A events) cannot be converted to a numeric"
  
  idx.spd <- .check_numeric(x.proc, "A", "Data2")
  txt.spd <- "Speed (Data2 of A events) cannot be converted to a numeric"
  
  idx.altspd.nona <- .check_nona(x.proc, "A", c("Data1", "Data2"))
  txt.altspd.nona <- "Altitude and speed (Data1-2 of A events) must not be NA"
  
  # HKR
  patt <- c("h", "k", "r", "n")
  if (file.type == "phocoena") patt <- c(patt, "y") #y means h in early years
  x.tmp <- x.proc
  x.tmp$Data1 <- .gsub_multi(c(patt, toupper(patt)), "", x.tmp$Data1)
  
  idx.hkr <- .check_character(as_airdas_df(x.tmp), "W", "Data1", c(""), 2)
  txt.hkr <- paste(
    "HKR must consist of only the following characters:", 
    "n, h, k, r, or NA (or y for PHOCOENA data)"
  )
  rm(x.tmp, patt)
  
  # Percent overcast; no check_numeric() needed
  cc.num <- c(as.character(0:100), sprintf("%02d", 0:100), sprintf("%03d", 0:100))
  idx.cc <- .check_character(x.proc, "W", "Data2", cc.num, 2)
  txt.cc <- "Percent overcast (Data2 of W events) must be a whole number between 0 and 100"
  rm(cc.num)
  
  # Beaufort; no check_numeric() needed
  idx.bft <- .check_character(x.proc, "W", "Data3", c(0:9), 2)
  txt.bft <- "Beaufort (Data3 of W events) must be a whole number between 0 and 9"
  
  # Jellyfish
  txt.jelly <- "Jellyfish code (Data4 of W events) must be one of 0, 1, 2, or 3"
  idx.jelly <- if (file.type == "phocoena") {
    integer(0)
  } else {
    .check_character(x.proc, "W", "Data4", c(0, 1, 2, 3), 2)
  }
  
  # Horizontal sun; no check_numeric() needed
  data.hsun <- switch(file.type, caretta = 5, phocoena = 4, turtle = 5)
  acc.hsun <- c(0:12, sprintf("%02d", 0:9))
  idx.hsun <- .check_character(x.proc, "W", paste0("Data", data.hsun), acc.hsun, 2)
  txt.hsun <- paste("Horizontal sun must be one of", 
                    paste(c(0:12, NA), collapse = ", "))
  rm(acc.hsun)
  
  # Vertical sun; no check_numeric() needed
  txt.vsun <- paste("Vertical sun (Data5 of W events) must be one of", 
                    paste(c(0:4, NA), collapse = ", "))
  idx.vsun <- if (file.type == "phocoena") {
    .check_character(x.proc, "W", "Data5", c(0:4, sprintf("%02d", 0:4)), 2)
  } else {
    integer(0)
  }
  
  # Observers
  idx.obs <- .check_character_length(x.proc, "P", c("Data1", "Data2", "Data3", "Data4"), 2, 3)
  txt.obs <- "Observer entries (Data1-4 of P events) must be exactly two characters"
  
  x.p <- x %>% filter(.data$Event == "P")
  x.p.data <- select(x.p, .data$Data1, .data$Data2, .data$Data3, .data$Data4)
  x.p.which <- apply(x.p.data, 1, function(i) any(duplicated(na.omit(i))))
  
  idx.obs.dup <- x.p$idx[x.p.which]
  txt.obs.dup <- "One or more observer entries (Data1-4 of P events) are duplicated"
  rm(x.p, x.p.data, x.p.which)
  
  
  # Add text to error.out as needed and return
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, idx.na.phoc, txt.na.phoc),
    .check_list(x, x.lines, idx.view, txt.view),
    .check_list(x, x.lines, idx.alt, txt.alt),
    .check_list(x, x.lines, idx.spd, txt.spd),
    .check_list(x, x.lines, idx.altspd.nona, txt.altspd.nona),
    .check_list(x, x.lines, idx.hkr, txt.hkr),
    .check_list(x, x.lines, idx.cc, txt.cc),
    .check_list(x, x.lines, idx.bft, txt.bft),
    .check_list(x, x.lines, idx.jelly, txt.jelly),
    .check_list(x, x.lines, idx.hsun, txt.hsun),
    .check_list(x, x.lines, idx.vsun, txt.vsun),
    .check_list(x, x.lines, idx.obs, txt.obs), 
    .check_list(x, x.lines, idx.obs.dup, txt.obs.dup)
  )
  
  
  #----------------------------------------------------------------------------
  ### Marine mammal sightings (S events)
  
  #--------------------------------------------------------
  # Angle
  data.s.ang <- paste0(
    "Data", switch(file.type, caretta = 3, phocoena = 4, turtle = 3)
  )
  ang.acc <- c(-90:90, sprintf("%03d", -9:-1), sprintf("%02d", 0:9))
  idx.s.ang <- .check_character(x.proc, "S", data.s.ang, ang.acc, 2)
  txt.s.ang <- "Angle must be a whole number between -90 and 90"
  
  # Group size
  data.s.gs <- paste0(
    "Data", switch(file.type, caretta = 4, phocoena = 3, turtle = 4)
  )
  gs.acc <- c(1:5000, sprintf("%02d", 1:9), sprintf("%03d", 1:99))
  idx.s.gs <- .check_character(x.proc, "S", data.s.gs, gs.acc, 1)
  txt.s.gs <- "Group size must be a whole number between 1 and 5000"
  
  # Species codes
  data.s.sp1 <- paste0("Data", switch(file.type, caretta = 5, phocoena = 2, turtle = 5))
  data.s.sp <- paste0("Data", switch(file.type, caretta = 5:7, phocoena = 2, turtle = 5:7))
  
  idx.s.sp1 <- x$idx[is.na(x[[data.s.sp1]]) & x$Event == "S"]
  txt.s.sp1 <- "The first species entry for an S event must not be NA"
  
  idx.s.sp <- .check_character(x.proc, "S", data.s.sp, sp.acc.all, 3)
  txt.s.sp <- "Species codes that are not NA must be specified in sp.codes"
  
  # Observer
  data.s.obs <- paste0(
    "Data", switch(file.type, caretta = 2, phocoena = 5, turtle = 2)
  )
  idx.s.obs <- .check_character_length(x.proc, "S", data.s.obs, 2, 2)
  txt.s.obs <- "Sighting observer code must be exactly two characters"
  
  # Sighting observer code is one of currently listed observers (what about "ZZ"??)
  #   Only checks sighting observers w/exactly 2 characters
  obs.code <- x.proc %>% 
    filter(.data$Event == "S", 
           !(.data$idx %in% idx.s.obs)) %>% 
    select(.data$ObsL, .data$ObsB, .data$ObsR, .data$Rec, 
           obs_curr = !!data.s.obs, .data$idx, angle_curr = !!data.s.ang) %>% 
    mutate(s_obs_code = case_when(.data$obs_curr == .data$ObsL ~ 1, 
                                  .data$obs_curr == .data$ObsB ~ 2, 
                                  .data$obs_curr == .data$ObsR ~ 3, 
                                  .data$obs_curr == .data$Rec ~ 4))
  
  idx.s.obs.code <- obs.code$idx[is.na(obs.code$s_obs_code)]
  txt.s.obs.code <- paste(
    "Sighting observer codes with two characters must be one of", 
    "the current observers, as specified by the most recent P event"
  )
  
  # Angle must be negative if sighting observer is left observer, 
  #   and positive if right observer. 
  #   Only checks for idx w/out previous angle/obs issue, and for non-NA angles
  ang.lr <- obs.code %>% 
    filter(!(.data$idx %in% c(idx.s.obs.code, idx.s.ang)), 
           !is.na(.data$angle_curr)) %>% 
    mutate(angle_curr = as.numeric(.data$angle_curr), 
           angle_issue1 = .data$angle_curr > 0 & .data$s_obs_code == 1, 
           angle_issue2 = .data$angle_curr < 0 & .data$s_obs_code == 3, 
           angle_issue = .data$angle_issue1 | .data$angle_issue2)
  
  idx.s.anglr <- ang.lr$idx[ang.lr$angle_issue]
  txt.s.anglr <- paste(
    "Sighting angle is positive with right observer sighting,", 
    "or negative with left observer sighting"
  )
  
  rm(obs.code, ang.lr)
  
  
  #--------------------------------------------------------
  error.out <- rbind(
    error.out,
    .check_list(x, x.lines, idx.s.ang, txt.s.ang),
    .check_list(x, x.lines, idx.s.gs, txt.s.gs),
    .check_list(x, x.lines, idx.s.sp1, txt.s.sp1), 
    .check_list(x, x.lines, idx.s.sp, txt.s.sp), 
    .check_list(x, x.lines, idx.s.obs, txt.s.obs), 
    .check_list(x, x.lines, idx.s.obs.code, txt.s.obs.code), 
    .check_list(x, x.lines, idx.s.anglr, txt.s.anglr)
  )
  
  
  #----------------------------------------------------------------------------
  ### Marine mammal resights and 1 events - for non-PHOCOENA data
  
  if (file.type != "phocoena") {
    x.s <- x[x$Event == "S", ]
    x.1 <- x[x$Event == "1", ]
    
    #------------------------------------------------------
    ### Multi-species info (1)  
    # Percentages are numeric
    idx.1.num <- .check_numeric(x.proc, "1", paste0("Data", 5:7))
    txt.1.num <- "A species percentage cannot be converted to a numeric"
    
    # If percentages are numeric, do they sum to 100?
    txt.1.sum <- "The species percentages must sum to 100"
    if (length(idx.1.num) == 0) {
      x.1.data <- select(x.1, .data$Data5, .data$Data6, .data$Data7)
      x.1.which <- apply(x.1.data, 1, function(i) {
        !isTRUE(all.equal(100, sum(as.numeric(i), na.rm = TRUE)))
      })
      
      idx.1.sum <- x.1$idx[x.1.which]
      rm(x.1.data, x.1.which)
      
    } else {
      idx.1.sum <- integer(0)
    }
    
    # Are other 1 event columns NA
    idx.1.na <- .check_isna(x, "1", paste0("Data", 1:4))
    txt.1.na <- "Data1-4 column(s) for 1 events must be NA (blank)"
    
    idx.1.na2 <- .check_isna(x, "1", c("DateTime", "Lat", "Lon"))
    txt.1.na2 <- "DateTime, Lat, or Lon for 1 events must be NA (blank)"
    
    # Every S code with multiple species has a 1 code immediately after it
    x.s$mult_count <- apply(x.s, 1, function(i) {
      sum(!is.na(i[c("Data5", "Data6", "Data7")]))
    })
    x.1$mult_count <- apply(x.1, 1, function(i) {
      sum(!is.na(i[c("Data5", "Data6", "Data7")]))
    })
    
    mult.s <- x.s$idx[x.s$mult_count > 1]
    
    idx.mult.s <- mult.s[!(mult.s %in% (x.1$idx - 1))]
    txt.mult.s <- paste("The S event has multiple species codes,", 
                        "but no corresponding 1 event")
    
    # For every 1 code with n non-NA sighting percentages, 
    #   the event before is an S event with n non-NA species codes
    d1 <- x$Event[x$idx %in% (x.1$idx - 1)] != "S"
    d2 <- x.s$mult_count[x.s$idx %in% (x.1$idx - 1)] != x.1$mult_count
    idx.mult.1 <- x.1$idx[d1 | d2]
    txt.mult.1 <- paste("The preceding event is not an S event with", 
                        "an equal number of species codes")
    rm(mult.s, d1, d2)
    
    
    #------------------------------------------------------
    ### Resights (s)
    idx.sre.ang <- .check_character(x.proc, "s", "Data2", ang.acc, 2)
    txt.sre.ang <- "Angle must be a whole number between -90 and 90"
    
    idx.sre.na <- .check_isna(x, "s", paste0("Data", 3:7))
    txt.sre.na <- "Data3-7 column(s) for s events must be NA (blank)"
    
    
    #------------------------------------------------------
    # Add to error.out
    error.out <- rbind(
      error.out,
      .check_list(x, x.lines, idx.1.num, txt.1.num),
      .check_list(x, x.lines, idx.1.sum, txt.1.sum),
      .check_list(x, x.lines, idx.1.na, txt.1.na),
      .check_list(x, x.lines, idx.1.na2, txt.1.na2),
      .check_list(x, x.lines, idx.mult.s, txt.mult.s),
      .check_list(x, x.lines, idx.mult.1, txt.mult.1),
      .check_list(x, x.lines, idx.sre.ang, txt.sre.ang),
      .check_list(x, x.lines, idx.sre.na, txt.sre.na)
    )
    
    rm(x.s, x.1)
  }
  
  
  #----------------------------------------------------------------------------
  ### Turtle sightings - for non-PHOCOENA data
  
  if (file.type != "phocoena") {
    # Angle
    data.t.ang <- switch(file.type, caretta = 3, turtle = 2)
    idx.t.ang <- .check_character(x.proc, "t", paste0("Data", data.t.ang), ang.acc, 2)
    txt.t.ang <- "Turtle angle must be a whole number between -90 and 90"
    
    # Group size
    idx.t.gs <- if (file.type == "caretta") { #TURTLE doesn't have group size field
      .check_character(x.proc, "t", "Data4", c(1:10, sprintf("%02d", 1:9)), 1)
    } else { #Must be TURTLE
      integer(0)
    }
    txt.t.gs <- "Turtle group size must be a whole number between 1 and 10"
    
    # Turtle species code
    data.t.sp <- paste0("Data", switch(file.type, caretta = 5, turtle = 3))
    idx.t.sp <- .check_character(x.proc, "t", data.t.sp, sp.acc.all, 1)
    txt.t.sp <- "Turtle species codes must not be NA and be specified in sp.codes"
    
    # Observer
    data.t.obs <- switch(file.type, caretta = 2, turtle = 1)
    idx.t.obs <- .check_character_length(x.proc, "t", paste0("Data", data.t.obs), 2, 2)
    txt.t.obs <- "Turtle sighting observer code must be exactly two characters"
    
    # Turtle size
    data.t.size <- switch(file.type, caretta = 6, turtle = 4)
    acc.size <- c(1:9, sprintf("%02d", 1:9))
    
    if (file.type == "caretta") {
      acc.size <- c(acc.size, "s", "m", "l")
      idx.t.size <- .check_character(x.proc, "t", paste0("Data", data.t.size), acc.size, 2)
      txt.t.size <- paste("Turtle size must be either a whole number between 1 and 9,", 
                          "or one of s, m, or l")
    } else if (file.type == "turtle") {
      idx.t.size <- .check_character(x.proc, "t", paste0("Data", data.t.size), acc.size, 2)
      txt.t.size <- "Turtle size must be a whole number between 1 and 9"
    }
    
    
    # Travel direction
    idx.t.dir <- if (file.type == "turtle") {
      acc.dir <- c(0:360, sprintf("%02d", 0:9), sprintf("%03d", 0:99))
      .check_character(x.proc, "t", paste0("Data", 5), acc.dir, 2)
    } else {
      integer(0)
    }
    txt.t.dir <- "Turtle travel direction must be between 0 and 360"
    
    # Tail visible
    data.t.tail <- switch(file.type, caretta = 7, turtle = 6)
    idx.t.tail <- .check_character(x.proc, "t", paste0("Data", data.t.tail), c("y", "n", "u"), 2)
    txt.t.tail <- "Turtle tail visible is not one of y, n, u, or NA"
    
    # NA check 
    idx.t.na <- if (file.type == "turtle") {
      .check_isna(x.proc, "t", paste0("Data", 7))
    } else {
      integer(0)
    }
    txt.t.na <- "Data7 for TURTLE t events must be NA (blank)"
    
    
    # Add to error.out
    error.out <- rbind(
      error.out,
      .check_list(x, x.lines, idx.t.ang, txt.t.ang),
      .check_list(x, x.lines, idx.t.gs, txt.t.gs),
      .check_list(x, x.lines, idx.t.sp, txt.t.sp),
      .check_list(x, x.lines, idx.t.obs, txt.t.obs),
      .check_list(x, x.lines, idx.t.size, txt.t.size),
      .check_list(x, x.lines, idx.t.dir, txt.t.dir),
      .check_list(x, x.lines, idx.t.tail, txt.t.tail),
      .check_list(x, x.lines, idx.t.na, txt.t.na)
    )
  }
  
  
  #----------------------------------------------------------------------------
  #----------------------------------------------------------------------------
  ### Remove first line and return
  if (nrow(error.out) == 1) {
    bevs <- c(
      "beer!", "glass of wine!", "margarita!", "vodka soda!", "gin and tonic!"
    )
    to.return <- data.frame(
      File = NA, LineNum = NA, Idx = NA, ID = NA, 
      Description = paste("No errors found, enjoy your", sample(bevs, 1)), 
      stringsAsFactors = FALSE
    )
    
  } else {
    to.return <- error.out %>% 
      slice(-1) %>% 
      group_by(.data$File, .data$LineNum, .data$Idx, .data$ID) %>%
      summarise(Description = paste(sort(.data$Description), collapse = "; ")) %>%
      ungroup() %>%
      arrange(.data$Description)
  }
  row.names(to.return) <- NULL
  
  if (!is.null(file.out)) write.csv(to.return, file = file.out)
  
  to.return
}
