#' Aerial DAS sightings
#'
#' Extract sighting information from aerial DAS data
#'
#' @param x \code{airdas_df} object; output from \code{\link{airdas_process}}, 
#'   or a data frame that can be coerced to a \code{airdas_df} object
#' @param ... ignored
#' @param angle.min numeric; the minimum (absolute value) angle 
#'   for which to consider a sighting a standard sighting. Default is 12
#' 
#' @details AirDAS events contain specific information in the 'Data#' columns,
#'   with the information depending on the event code and file type for that row.
#'   This function extracts relevant data for sighting events, and returns a
#'   data frame with dedicated columns for each piece of sighting information. 
#'   It can handle multiple file types in \code{x}; for instance, 
#'   \code{x} could be processed PHOCOENA and TURTLE data
#'   combined using \code{\link[base:cbind]{rbind}}. 
#'   See \code{\link{airdas_format_pdf}} for more information about the expected
#'   events and event formats, depending on the file type.
#'   
#'   All species codes are converted to lower case using \code{\link[base:chartr]{tolower}}.
#'    
#'   Abbreviations used in column names include: Gs = group size, Sp = species, 
#'   Mixed = mixed species (multi-species) sighting. 
#'   In addition, note that multi-species group sizes are rounded to 
#'   the nearest whole number using \code{round(, 0)}
#'   
#'   A 'sighting by a standard observer' ('ObsStd') is a sighting 
#'   made by ObsL, ObsB, or ObsR (not the data recorder or pilot).   
#'   A 'standard sighting' ('SightStd') is a sighting 
#'   that was made while on effort, by a standard observer, 
#'   and with the absolute value of the angle of declination 
#'   being greater than or equal to \code{angle.min}.   
#'   Resights (Events 's') are not considered standard events, 
#'   and thus both 'ObsStd' and 'SightStd' will be \code{NA} for 's' events.
#'
#' @return Data frame with 1) the columns from \code{x}, excluding the 'Data#' columns,
#'   and 2) columns with sighting information extracted from 'Data#' columns as described below.
#'   The data frame has one row for each sighting, or one row for each 
#'   species of each sighting if it is a multi-species (mixed) sighting.
#'   
#'   Added sighting information columns:
#'   \tabular{lll}{
#'     \emph{Sighting information}       \tab \emph{Column name} \tab \emph{Notes}\cr
#'     Sighting number                   \tab SightNo\cr
#'     Observer that made the sighting   \tab Obs\cr
#'     Angle of declination              \tab Angle    \tab Left is negative\cr
#'     Sighting by standard observer     \tab ObsStd   \tab Logical; described in Details\cr
#'     Standard sighting                 \tab SightStd \tab Logical; described in Details\cr
#'     Mixed species sighting            \tab Mixed    \tab Logical\cr
#'     Species code                      \tab SpCode   \tab All characters converted to lower case\cr
#'     Group size of school              \tab GsTotal  \tab Only different from GsSp for mixed species sightings\cr
#'     Group size of species             \tab GsSp\cr
#'     Turtle length (feet if numeric)   \tab TurtleSize      \tab \code{NA} for non-"t" events; may be character or numeric\cr
#'     Turtle travel direction (degrees) \tab TurtleDirection \tab \code{NA} for non-"t" events\cr
#'     Turtle tail visible?              \tab TurtleTail      \tab \code{NA} for non-"t" events\cr
#'   }
#'   
#'   The TurtleSize will be of class character is there is any 
#'   CARETTA data in \code{x}, and of class numeric otherwise.
#'
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' y.proc <- airdas_process(y)
#' 
#' airdas_sight(y.proc)
#'
#' @export
airdas_sight <- function(x, ...) UseMethod("airdas_sight")


#' @name airdas_sight
#' @export
airdas_sight.data.frame <- function(x, ...) {
  airdas_sight(as_airdas_df(x), ...)
}


#' @name airdas_sight
#' @export
airdas_sight.airdas_df <- function(x, angle.min = 12, ...) {
  #----------------------------------------------------------------------------
  ### Filter for and extract sighting data
  event.sight <- c("S", "s", "t")
  event.sight.info <- "1"
  
  ### Filter for sighting-related data
  sight.df <- x %>% 
    filter(.data$Event %in% c(event.sight, event.sight.info)) %>% 
    mutate(sight_cumsum = cumsum(.data$Event %in% event.sight))
  
  stopifnot(
    length(event.sight.info) == 1, 
    is.numeric(angle.min),
    angle.min >= 0, angle.min <= 90
  )
  
  
  #----------------------------------------------------------------------------
  ### For every sighting event paired with a '1' event, split the sighting
  ###   into multiple lines. 
  ### If no '1' events, sight.mult will be list() and thus not change sight.df
  sight.cumsum.mult <- sight.df$sight_cumsum[sight.df$Event %in% event.sight.info]
  
  sight.mult <- lapply(sight.cumsum.mult, function(i, sight.df) {
    curr.df <- sight.df %>% filter(.data$sight_cumsum == i)
    stopifnot(identical(curr.df$Event, c("S", "1")))
    
    # Extract species and group size info
    gs.total <- as.numeric(curr.df$Data4[1])
    sp.all <- c(curr.df$Data5[1], curr.df$Data6[1], curr.df$Data7[1])
    sp.perc.all <- c(
      as.numeric(curr.df$Data5[2]), as.numeric(curr.df$Data6[2]), 
      as.numeric(curr.df$Data7[2])
    )
    sp.num.all <- round(sp.perc.all / 100 * gs.total, 0)
    
    # Warning if species percentages do not sum to 100
    if (!isTRUE(all.equal(sum(sp.perc.all, na.rm = TRUE), 100)))
      warning("The multispecies sightings percentages do not sum to 100 ", 
              "for the following sighting: ",
              .print_file_line(curr.df$file_das, curr.df$line_num, 1))
    
    
    # Create df with one row for each species
    bind_rows(curr.df[1, ], curr.df[1, ], curr.df[1, ]) %>% 
      mutate(Data4 = as.character(sp.num.all), Data5 = sp.all, 
             Data6 = NA, Data7 = NA, 
             GsTotal = gs.total, Mixed = TRUE) %>% 
      filter(!is.na(.data$Data4))
  }, sight.df = sight.df)
  
  # Add multi-species sightings back into sight.df
  sight.df <- sight.df %>% 
    filter(!(.data$sight_cumsum %in% sight.cumsum.mult)) %>% 
    mutate(GsTotal = ifelse(.data$Event == "S", as.numeric(.data$Data4), NA), 
           Mixed = ifelse(.data$Event == "s", NA, FALSE)) %>% 
    bind_rows(sight.mult) %>% 
    arrange(.data$sight_cumsum) %>% 
    mutate(idx = seq_along(.data$sight_cumsum)) %>% 
    select(-.data$sight_cumsum)
  
  
  #----------------------------------------------------------------------------
  stopifnot(all(sight.df$Event %in% event.sight))
  
  ### 1) Extract processed AirDAS variables
  sight.info <- sight.df %>% 
    select(-!!paste0("Data", 1:7), -.data$GsTotal, -.data$Mixed)
  
  ### 2) Extract sighting information based on file type
  sight.df.all <- bind_rows(
    .airdas_sight_phocoena(filter(sight.df, .data$file_type == "phocoena"), angle.min), 
    .airdas_sight_caretta(filter(sight.df, .data$file_type == "caretta"), angle.min),
    .airdas_sight_turtle(filter(sight.df, .data$file_type == "turtle"), angle.min)
  )
  
  # CARETTA data uses "l", "m", "s" code for TurtleSize
  if (!any(sight.df$file_type == "caretta")) 
    sight.df.all$TurtleSize <- as.numeric(sight.df.all$TurtleSize)
  
  #----------------------------------------------------------------------------
  # Join data frames and return
  sight.info %>% 
    left_join(sight.df.all, by = "idx") %>% 
    mutate(SpCode = tolower(.data$SpCode)) %>%
    select(-.data$idx)
}


###############################################################################
# Helper functions, for consistency across .airdas_sight_ functions below
.obsStd_lgl <- function(Event, Obs, ObsL, ObsB, ObsR) {
  if_else(Event == "s", na_lgl,  Obs %in% c(ObsL, ObsB, ObsR))
}

.sightStd_lgl <- function(OnEffort, ObsStd, Event, Angle, angle.min) {
  if_else(
    Event == "s", 
    na_lgl, 
    OnEffort & ObsStd & (Event %in% c("S", "t")) & (abs(Angle) >= angle.min)
  )
}



###############################################################################
# Extract sighting data from data created using PHOCOENA program
.airdas_sight_phocoena <- function(sight.df, angle.min) {
  if (!all(sight.df$Event == "S")) 
    stop("Error in processing: not all sighitng rows with file_type ", 
         "PHOCOENA are Event S. ", 
         "Please reprocess data and/or submit an issue")
  
  sight.df %>% 
    mutate(SightNo = .data$Data1, 
           SpCode = .data$Data2, 
           GsTotal = as.numeric(.data$Data3), 
           Angle = as.numeric(.data$Data4), 
           Obs = .data$Data5, 
           GsSp = .data$GsTotal, 
           ObsStd = pmap_lgl(list(.data$Event, .data$Obs, .data$ObsL, 
                                  .data$ObsB, .data$ObsR), 
                             .obsStd_lgl), 
           SightStd = .sightStd_lgl(.data$OnEffort, .data$ObsStd, .data$Event, 
                                    .data$Angle, angle.min), 
           TurtleSize = NA_character_, 
           TurtleDirection = as.numeric(NA), 
           TurtleTail = NA_character_) %>% 
    select(.data$idx, .data$SightNo, 
           .data$Obs, .data$Angle, .data$ObsStd, .data$SightStd, 
           .data$Mixed, .data$SpCode, .data$GsTotal, .data$GsSp, 
           .data$TurtleSize, .data$TurtleDirection, .data$TurtleTail)
}


# Extract sighting data from data created using CARETTA program
.airdas_sight_caretta <- function(sight.df, angle.min) {
  if (!all(sight.df$Event %in% c("S", "t", "s")))
    stop("Error in sight function - incorrect codes. ", 
         "Please report this as an issue")
  
  sight.info.all <- sight.df %>% 
    mutate(SightNo = .data$Data1, 
           Obs = case_when(.data$Event == "S" ~ .data$Data2,
                           .data$Event == "t" ~ .data$Data2), 
           Angle = as.numeric(case_when(.data$Event == "S" ~ .data$Data3,
                                        .data$Event == "s" ~ .data$Data2, 
                                        .data$Event == "t" ~ .data$Data3)), 
           ObsStd = pmap_lgl(list(.data$Event, .data$Obs, .data$ObsL, 
                                  .data$ObsB, .data$ObsR), 
                             .obsStd_lgl), 
           SightStd = .sightStd_lgl(.data$OnEffort, .data$ObsStd, .data$Event, 
                                    .data$Angle, angle.min), 
           SpCode = case_when(.data$Event == "S" ~ .data$Data5,
                              .data$Event == "t" ~ .data$Data5), 
           GsSp = case_when(.data$Event == "S" ~ as.numeric(.data$Data4),
                            .data$Event == "t" ~ as.numeric(.data$Data4)), 
           GsTotal = case_when(.data$Event == "S" ~ .data$GsTotal, 
                               .data$Event == "t" ~ .data$GsSp)) %>% 
    select(.data$idx, .data$SightNo, 
           .data$Obs, .data$Angle, .data$ObsStd, .data$SightStd, 
           .data$Mixed, .data$SpCode, .data$GsTotal, .data$GsSp)
  
  sight.info.t <- sight.df %>% 
    filter(.data$Event == "t") %>%
    mutate(TurtleSize = .data$Data6, 
           TurtleDirection = as.numeric(NA), 
           TurtleTail = .data$Data7) %>% 
    select(.data$idx, .data$TurtleSize, .data$TurtleDirection, 
           .data$TurtleTail)
  
  left_join(sight.info.all, sight.info.t, by = "idx")
}


# Extract sighting data from data created using TURTLE program
.airdas_sight_turtle <- function(sight.df, angle.min) {
  if (!all(sight.df$Event %in% c("S", "t", "s")))
    stop("Error in sight function - incorrect codes. ", 
         "Please report this as an issue")
  
  sight.info.all <- sight.df %>% 
    mutate(SightNo = as.character(ifelse(.data$Event == "t", NA, .data$Data1)), 
           # ^ is for when there are 0 rows to ensure character class
           Obs = case_when(.data$Event == "S" ~ .data$Data2,
                           .data$Event == "t" ~ .data$Data1), 
           Angle = as.numeric(case_when(.data$Event == "S" ~ .data$Data3,
                                        .data$Event == "s" ~ .data$Data2, 
                                        .data$Event == "t" ~ .data$Data2)), 
           ObsStd = pmap_lgl(list(.data$Event, .data$Obs, .data$ObsL, 
                                  .data$ObsB, .data$ObsR), 
                             .obsStd_lgl), 
           SightStd = .sightStd_lgl(.data$OnEffort, .data$ObsStd, .data$Event, 
                                    .data$Angle, angle.min), 
           SpCode = case_when(.data$Event == "S" ~ .data$Data5,
                              .data$Event == "t" ~ .data$Data3), 
           GsSp = case_when(.data$Event == "S" ~ as.numeric(.data$Data4),
                            .data$Event == "t" ~ 1), 
           GsTotal = case_when(.data$Event == "S" ~ .data$GsTotal, 
                               .data$Event == "t" ~ 1)) %>% 
    select(.data$idx, .data$SightNo, 
           .data$Obs, .data$Angle, .data$ObsStd, .data$SightStd, 
           .data$Mixed, .data$SpCode, .data$GsTotal, .data$GsSp)
  
  sight.info.t <- sight.df %>% 
    filter(.data$Event == "t") %>%
    mutate(TurtleSize = .data$Data4, 
           TurtleDirection = as.numeric(.data$Data5), 
           TurtleTail = .data$Data6) %>% 
    select(.data$idx, .data$TurtleSize, .data$TurtleDirection, 
           .data$TurtleTail)
  
  left_join(sight.info.all, sight.info.t, by = "idx")
}
