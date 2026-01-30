#' Import raw ring-width measurements from the ITRDB
#'
#' @param dir.src
#' Character string specifying the directory path or URL where the RWL file
#' is located. Users typically provide a local directory containing a
#' user-downloaded ITRDB file, but an online source may also be used.
#'
#' @param rwl
#' Character string giving the file name of the RWL file.
#'
#' @details
#' The International Tree-Ring Data Bank (ITRDB) is maintained by the NOAA
#' National Centers for Environmental Information (NCEI) as part of the
#' World Data Service for Paleoclimatology.
#' See: \url{https://www.ncei.noaa.gov/products/paleoclimatology/tree-ring}
#'
#' Raw ring-width measurement files (`.rwl`) distributed through the ITRDB
#' follow the *Tucson fixed-width format*. In this convention, header records
#' encode site-level metadata (e.g., site identifier, site name, species code,
#' geographic information, and temporal coverage), followed by ring-width
#' measurements stored in a decadal layout (ten annual values per line).
#'
#' \strong{Record 1}
#' \itemize{
#'   \item Columns 1--6: Site ID
#'   \item Columns 10--61: Site name
#'   \item Columns 62--65: Species code
#'   \item Optional ID fields
#' }
#'
#' \strong{Record 2}
#' \itemize{
#'   \item Columns 1--6: Site ID
#'   \item Columns 10--22: State / country
#'   \item Columns 23--30: Species
#'   \item Columns 41--45: Elevation
#'   \item Columns 48--57: Latitude--longitude
#'   \item Columns 62--63: Measurement type code
#'   \item Columns 68--76: First and last year
#' }
#'
#' Latitude--longitude values are expressed in degrees and minutes
#' (`ddmm` or `dddmm`).
#'
#' \strong{Record 3}
#' \itemize{
#'   \item Columns 1--6: Site ID
#'   \item Columns 10--72: Investigators
#'   \item Columns 73--80: Optional completion date
#' }
#'
#' @return
#' A list with two elements:
#' \itemize{
#'   \item \code{header}: a table containing site-level metadata
#'   \item \code{rwl}: a table containing raw ring-width measurements
#' }
#'
#' @export
#'
#' @examples
#' ## Online example (not run to avoid timeout and internet dependency)
#' \donttest{
#' dir.src <- "https://www.ncei.noaa.gov/pub/data/paleo/treering/measurements/northamerica/canada"
#' rwl <- "cana615.rwl"
#' dt.rwl <- read_rwl(dir.src, rwl)
#' }
#'
#' ## Local example using packaged data
#' file <- system.file("extdata", "cana615.rwl", package = "growthTrendR")
#' stopifnot(file != "")
#' dir.src <- dirname(file)
#' rwl <- basename(file)
#' dt.rwl <- read_rwl(dir.src, rwl)


read_rwl <-function (dir.src, rwl) {
  if (grepl("^https?://", dir.src))
    # treat as URL
    path <- paste0(dir.src, "/", rwl) else
      path <- file.path(dir.src, rwl)
  con <- if (grepl("^https?://", path)) curl::curl(path) else file(path)
  # on.exit(close(con))

  on.exit({
    if (isOpen(con)) close(con)
  }, add = TRUE)

  # Now try to read
  lines <- tryCatch(
    readLines(con, warn = FALSE),
    error = function(e) stop("Could not read from connection: ", e$message)
  )



  dt.lines <- data.table::data.table(lines = lines)
  # dt.lines <- data.table(lines = readLines(con <- file(file.path(dir.src, rwl))))
  # close(con)
  message(paste0("reading ", rwl))
  comments <- ""
  dt.lines <- dt.lines[!(trimws(lines, which = "both") == "")]
  if (nrow(dt.lines) < 5) stop(paste0("not enough lines, please check "))
# 1st line
  line1 <- trimws(dt.lines[1,]$lines, which = "right")
  # Record #1: 1-6 Site ID, 10-61 Site Name, 62-65 Species Code
  site_ID1.0 <- trimws(str_sub(line1,1,6));
  site_name.s <-  strsplit(str_sub(line1, 10), "(?<=\\S)\\s\\s\\s", perl = TRUE)[[1]][1]
  rest.spc <-trimws(str_sub(line1, 10 + nchar(site_name.s)), which = "both")

  spc_code.s <-  strsplit(rest.spc, "(?<=\\S)\\s\\s", perl = TRUE)[[1]][1]
  L1_aux.s <- trimws(str_remove(rest.spc, spc_code.s), which = "both")

  if (nchar(spc_code.s) != 4 | str_detect(spc_code.s, "\\s") > 0) {
    message(paste0(" check spc_code$ ", spc_code.s))
            comments <- paste0(comments, " spc_code")
}
  # spc_code.0 <- trimws(str_sub(line1,62,65), which = "both")

  line2 <- trimws(dt.lines[2,]$lines, which = "both")
  # Record #2: 1-6 Site ID, 10-22 State/Country, 23-30 Species, 41-45 Elevation, 48-57 Lat-Long, 62-63 measurement type code**, 68-76 1st & last Year
  # Note: lat-lons are in degrees and minutes, ddmm or dddmm
  site_ID2.0 <- trimws(str_sub(line2,1,6));
  rest.all <- str_sub(line2, 10)
  # Stat_con.0 <- trimws(str_sub(line2,10,22))


  stat_con.s <- strsplit(str_sub(line2, 10), "(?<=\\S)\\s\\s", perl = TRUE)[[1]][1]
  # consider "British Columbia" as the only case that can seperate from species by 1 space
  if (nchar(stat_con.s) > nchar("British Columbia") & tolower(str_sub(stat_con.s, 1,16)) == "british columbia" ) stat_con.s <- str_sub(stat_con.s, 1,16)
  rest4 <-trimws(str_remove(rest.all, stat_con.s), which = "both")
  rest4.split <- strsplit(rest4, "(?<=\\S)\\s\\s", perl = TRUE)[[1]]

  if (length(rest4.split) < 5) {
    message(paste0(" check line2$stat_con$", stat_con.s))
    comments <- paste0(comments, " stat_con")
}
  species.s <- trimws(rest4.split[1], which = "both")
  pos.N <- regexpr("[0-9]", species.s)
  if (pos.N > 0) {
    species.s <- trimws(str_sub(species.s, 1, (pos.N - 1)), which = "both")
    message(paste0(" check line2$species$", species.s))
    comments <- paste0(comments, " species")
  pos.N <- NULL
  }
  rest3 <-trimws(str_remove(rest4, species.s), which = "both")
  rest3.split <- strsplit(rest3, "(?<=\\S)\\s\\s", perl = TRUE)[[1]]

  if (length(rest3.split) < 4) {
    message(paste0(" check line2$species$", species.s))
    comments <- paste0(comments, " species")
    }

  elev.s <- strsplit(rest3, "(?<=\\S)\\s", perl = TRUE)[[1]][1]

  rest2 <- trimws(str_remove(rest3, elev.s), which = "both")
  lat_lon.s <-  strsplit(rest2, "(?<=\\S)\\s\\s", perl = TRUE)[[1]][1]
  lat.s <- as.numeric(strsplit(lat_lon.s, "-|\\s")[[1]][1])
  lon.s <- as.numeric(strsplit(lat_lon.s, "-|\\s")[[1]][2]) * -1
  if (lat.s > 1000) {lat.s <- lat.s/100; lon.s <- lon.s/100} else {
    message(paste0(rwl, " check lat-lon"))
    comments <- paste0(comments, " format_lat-lon" )
    }
  if (is.na(lat.s)| is.na(lon.s)) {
    message(paste0(rwl, " check lat-lon"))
    comments <- paste0(comments, " missing_lat-lon" )
    }

  rest1 <-  trimws(str_remove(rest2, lat_lon.s), which = "both")
  meas_type.s <-  strsplit(rest1, "(?<=\\S)\\s\\s", perl = TRUE)[[1]][1]

  rest0 <-  trimws(str_remove(rest1, meas_type.s), which = "both")
  yspan.s <- strsplit(rest0, "(?<=\\S)\\s\\s", perl = TRUE)[[1]][1]
  ystart.s <- as.numeric(strsplit(yspan.s, "-|\\s")[[1]][1])
  yend.s <- as.numeric(strsplit(yspan.s, "-|\\s")[[1]][2])
  if (is.na(ystart.s) | is.na(yend.s)) {
    message(paste0(rwl, " check year span"))
    comments <- paste0(comments, " yspan")
}
  L2_aux.s <- trimws(str_remove(rest0, yspan.s), which = "both")

  line3 <- dt.lines[3,]$lines
  # Record #3: 1-6 Site ID, 10-72 Investigators, 73-80 optional completion date
  site_ID3.0 <- trimws(str_sub(line2,1,6));
  # investigators.0 <- trimws(str_sub(line3,10,72))
  investigators.s <- strsplit(str_sub(line3,10), "(?<=\\S)\\s\\s\\s\\s\\s", perl = TRUE)[[1]][1]
  # L3_aux.0 <- trimws(str_sub(line3,73,80))
  L3_aux.s <- trimws(str_remove(str_sub(line3,10), investigators.s), which = "both")

  site_IDs <- c(site_ID1.0, site_ID2.0, site_ID3.0)

  if (any(duplicated(site_IDs, fromLast = TRUE))) site_ID.s <- site_IDs[duplicated(site_IDs, fromLast = TRUE)][1] else site_ID.s <- site_ID1.01.0
  if (any(duplicated(site_IDs, fromLast = TRUE)) == FALSE) {
    message(paste0(rwl, " different site_ID: ", paste(site_IDs, collapse = " ")))
    comments <- paste0(comments, " dif_IDs")
  }
  # if (comments == "chk ") comments <- ""

  dt.header <- data.table(fn = rwl, L1 =line1, L2 =line2,L3 =line3,
                          site_ID = site_ID.s, site_name = site_name.s, spc_code = spc_code.s, L1_aux = L1_aux.s,
                        Stat_con = stat_con.s, species = species.s, elev = elev.s, lat = lat.s, lon = lon.s,
                        meas_type = meas_type.s, ystart = ystart.s, yend = yend.s,L2_aux = L2_aux.s,
                        investigators = investigators.s,  L3_aux = L3_aux.s,
                        comments.header = comments)

 # }# end read_header



# Data row format:
#   Core ID Number columns 1-6
# Decade columns 9-12
# Data Values columns 13-73, 6 columns/measurement, 10(I6)
# Optional Site ID columns 74-78}

  dt.rwl <- dt.lines[-c(1:3)]
  comments.rw <- ""
  # line4 +
  decade.c <- trimws(str_sub(dt.rwl[1,],9,12), which = "both")

  if (grepl("^[0-9]+$", decade.c, perl = T) == FALSE ) {
    message(paste0(i, " pls check line4 ", rwl, " decade: ", decade.c))
    comments.rw <- paste0(comments.rw, " decade.C")

  }
  if (as.numeric(decade.c) < ystart.s | as.numeric(decade.c) > yend.s){
    message(paste0(" pls check line4 ", rwl, " decade: ", decade.c, " out of year span: ", ystart.s, " - ", yend.s))
    comments.rw <- paste0(comments.rw, " decade.N")
  }



          # dt.rwl[, verify := FALSE]
          # if (rwl %in% c("cana577.rwl",  "cana579.rwl"))
          #   # dt.rwl[str_sub(trimws(lines, which = "both"),-3,-1 ) == "gap"]
          # dt.rwl[str_sub(trimws(lines, which = "both"),-3,-1 ) == "gap", lines:= trimws(str_sub(lines, 1,-4), which = "both")]
          #
          # if (rwl %in% c("cana594.rwl")) dt.rwl[,lines:=str_replace(lines, "\\(", " ")]

  # dt.rwl[, id.core:=tstrsplit(lines, " ", fixed = TRUE, keep = 1L)]
  dt.rwl[, id.core:=trimws(str_sub(lines, 1,8))]
  dt.rwl[, rest := trimws(str_sub(lines, 9), which = "both")]

 # spcial characters in lines
  if (nrow(dt.rwl[grepl("\t|\\(", lines)]) > 0) {
    dt.rwl[grepl("\t|\\(", lines),specialchar:= 1]
    message(paste0("special character in ", rwl, "  ", paste(unique(dt.rwl[specialchar == 1]$id.core), collapse = " ")))
    comments.rw <- paste0(comments.rw, " special_character")
            dt.rwl[specialchar == 1, lines := trimws(gsub("\t|\\(", " ", lines), which = "both")]
    dt.rwl[specialchar == 1, id.core:=tstrsplit(lines, " ", fixed= TRUE, keep = 1L)]
    dt.rwl[specialchar == 1, rest:=trimws(str_remove(lines, id.core), which = "both")]
    dt.rwl[,specialchar:= NULL]

    }



  dt.rwl[, decade.yr:= tstrsplit(rest, " ", fixed = TRUE, keep = 1L)]
  dt.rwl[, rw:= trimws(str_remove(rest, decade.yr), which = "both")]
  if (nrow(dt.rwl[nchar(decade.yr) > 4]) > 0) {
    message(paste0("non numermic in ", rwl , "$decade.yr: ", paste(dt.rwl[nchar(decade.yr) > 4]$decade.yr, collapse = " ") ))
    # remove non-numeric
    dt.rwl[nchar(decade.yr) > 4, decade.yr:=gsub("[^0-9]", "", decade.yr) ]
    }


  if (nrow(dt.rwl[is.na(decade.yr) | decade.yr == "" | decade.yr == " "]) > 0){
    stop (paste0(rwl, " missing decade.yr"))
    comments.rw <- paste0(comments.rw, " decade.yr")
  }
  dt.rwl[, nc.yr:= nchar(trimws(decade.yr, which = "both"))]


             # # format id year
             #  if (rwl == "can674.rwl") dt.rwl[nc.yr != 4, decade.yr:= str_sub(lines, 9,12)][nc.yr != 4, nc.yr:= nchar(trimws(decade.yr, which = "both"))][,verify:=TRUE]
             # # year < 1000
             # if (rwl %in% c("can682.rwl", "cana600.rwl", "cana650.rwl","cana651.rwl", "cana652.rwl")) dt.rwl[,verify := TRUE]
             # if (rwl %in% c("can689.rwl","cana588.rwl")) dt.rwl[nc.yr!=4,id.core:= str_sub(lines, 1,6)][nc.yr!=4,decade.yr:= str_sub(lines, 8,11)][,verify := TRUE]
             # if ( nrow(dt.rwl[nc.yr != 4& verify == FALSE]) > 0 )  {
             # print(dt.rwl[nc.yr != 4])
             #   print(rwl)
             #      stop("check format id year")
             #
             #  }

  dt.rwl[, decade.yr:= as.numeric(decade.yr)]
  if (nrow(dt.rwl[is.na(decade.yr)]) > 0){
    stop(paste0("missing decade year in "), rwl)
  }
  # dt.rwl[, dif.decade:= decade.yr - shift(decade.yr), by = .(id.core)]
  # dt.rwl[, id2 := seq_len(.N),by = c("id.core")]
  dt.rwl[, block_id := rleid(id.core)]
  if (nrow(dt.rwl[, .N, by = .(block_id, id.core)][, .N, by = .(id.core)][N>1]) > 0){
  # if (nrow(dt.rwl[id2 > 2 & dif.decade != 10]) > 0) {
    message(paste0(" detected multiple sections-1"))
    # print(dt.rwl[id2 > 2 & dif.decade != 10])
    comments.rw <- paste0(comments.rw, " multi_section")
  }

  dt.rwl[, rw.rest := rw]; i.rw <- -1;
  while (nrow(dt.rwl[!(is.na(rw.rest) | trimws(rw.rest, which = "both") == "")]) > 0){
   i.rw <- i.rw + 1
   dt.rwl[!is.na(rest), rw.tmp:= tstrsplit(rw.rest, " ", fixed= TRUE, keep = 1L)]
  dt.rwl[!is.na(rw.tmp), rw.rest:= trimws(str_remove(rw.rest, rw.tmp), which = "both")]
  if (i.rw < 10) dt.rwl[!is.na(rw.tmp), rw.tmp:= as.numeric(rw.tmp)]

  setnames(dt.rwl, "rw.tmp", paste0("V", i.rw))

  }
  if (str_detect(toString(names(dt.rwl)), "V10") ){
    message(paste0("extra columns in ", rwl))
    comments.rw <- paste0(comments.rw, " extra_columns")
  }

  dt.rwl.long <- melt(dt.rwl[, c("block_id", "id.core", "decade.yr", paste0("V",0:9))],
                      id.vars=c("block_id", "id.core", "decade.yr"),

                      variable.name="v.idx",
                      value.name="rw_int")[!is.na(rw_int)]

  dt.rwl.long[,rw.N:= as.numeric(rw_int)]
  dt.rwl.long[,year:=decade.yr + as.integer(str_sub(v.idx, 2,2))]

  setorder(dt.rwl.long, block_id, id.core, year)
  id.core.scale <- dt.rwl.long[, .SD[.N], by = c("block_id", "id.core")]
  if (nrow(id.core.scale[!(rw.N %in% c(999,-9999))]) > 0) {
    stop(paste0("check scale ", paste(id.core.scale[!(rw.N %in% c(999,-9999))]$rw.N, collapse = " " ), " in ", rwl))
    }
  id.core.scale[, rw.sc:= ifelse(rw.N==-9999, 0.001, 0.01)]
  if (nrow(id.core.scale[, .N, by = .(rw.sc)]) > 1) {
    message(paste0("check multiple scale in ", rwl))
    comments.rw <- paste0(comments.rw, " multi_scale")
    }
  dt.rwl.long<- dt.rwl.long[!id.core.scale, on = .(block_id, id.core, year)]
  setnames(id.core.scale, "rw.N", "rw.N.sc")
  dt.rwl.long <- dt.rwl.long[id.core.scale[,c("block_id", "id.core", "rw.N.sc", "rw.sc")], on = c("block_id", "id.core")]
  # remove ending
  dt.end <- dt.rwl.long[ rw.N == rw.N.sc]
  if (nrow(dt.end) > 0) {
    message(paste0("ending ", unique(dt.end$rw.N.sc), "was found in the middle of series"))
    comments.rw <- paste0(comments.rw, " multi-section-0")
  dt.rwl.long <- dt.rwl.long[ rw.N != rw.N.sc]
  }
  dt.rwl.long [,rw_mm:= rw.N*rw.sc]
  # names(dt.rwl.long)
  if (nrow(dt.rwl.long[rw_mm < 0 | is.na(rw_mm)]) > 0){
    message(paste0(" detected negative or missing rw"))
    comments.rw <- paste0(comments.rw, " neg_or_NA_rw")
  }
  if (nrow(dt.rwl.long[, .N, by = .(block_id, id.core, year)][N >1]) > 0){
    message(paste0("duplicated id.core-year ", rwl))
    comments.rw <- paste0(comments.rw, " dup_id_year")
  }
  dt.rwl.long <- dt.rwl.long[, c("id.core", "year", "rw_mm")][, fn:=rwl]
# print(paste0(rwl, " ", comments.rw))
# if (nchar(comments.rw) < 7) comments.rw <- ""
dt.header$comments.rw <- comments.rw
  return(list(dt.header = dt.header, dt.rw_long = dt.rwl.long))
}




#' calculate annual basal area increment

#' @description
#' calculate basal area (cm2) and basal area increment (cm2)
#'
#'
#' @param data data in long format containing at least 3 columns: id, year, rw
#'
#'
#' @return add 3 columns to the original input data, ageC for cambial age, ba_cm2_t_1 for basal area of the previous year in cm2, and bai_cm2 for annual basal area increment in cm2
#'
#' @export calc_bai
#' @examples
#'  # generate data
#' dt.rw <- data.table::data.table(
#'   uid_radius = rep(paste0("R", 1:3), each = 5),
#'   year      = rep(2001:2005, times = 3),
#'   rw_mm     = round(runif(15, 0.5, 3.5),2)
#' )
#' data.table::setorder(dt.rw, uid_radius, year)
#'  # calculate bai
#' dt.rw <- calc_bai(dt.rw)
#'


calc_bai <- function(data){
  setDT(data)
  required_cols <- c("uid_radius", "year", "rw_mm")
  if (!all(required_cols %in% names(data))) {
    stop("Missing required columns: ",
         paste(setdiff(required_cols, names(data)), collapse = ", "))
  }
  med.rw <- median(as.numeric(data$rw_mm, na.rm = TRUE))
  # cat("please assure the unit of ", rw, " is mm")

  if (nrow(data[, .N, by = c("uid_radius", "year")][N>1]) > 0) stop (paste0("uid_radius-year is not a unique key, please verify..."))

  if (med.rw > 10) message(paste0("median of rw ", med.rw, " seems too big, assure it's in mm"))
  if (med.rw < 1) message(paste0("median of rw ", med.rw, " seems too small, assure it's in mm"))

  setorderv(data, c("uid_radius", "year"))

  data <- data %>%
    group_by(uid_radius) %>%
    mutate(
      ageC = row_number(),
      radius = cumsum(as.numeric(rw_mm))
    ) %>%
    mutate(
      radius_prev = c(0, utils::head(radius, -1))
    ) %>%
    ungroup()
  data <- as.data.table(data)
 # data[, `:=`(ageC = seq_len(.N),
  #                radius = cumsum(eval(parse(text = rw))),  # Cumulative radius (assumes RW is added each year)
  #                radius_prev = data.table::shift(cumsum(eval(parse(text = rw))), fill = 0)), by = eval(id)]  # Previous radius (shifted)

  # Compute previous BA in cm2
  data[, ba_cm2_t_1 := pi * (radius_prev^2)/100]

  data[, bai_cm2 := pi * (radius^2 - radius_prev^2)/100]
  data[bai_cm2 < 0]

  # Drop the radius columns if not needed
  data[, c("radius", "radius_prev") := NULL]
  return(data)
}

#' Prepare tree-ring data for downstream analysis
#'
#' This function prepares tree-ring data including ring-width
#' measurements, and optionally computes basal area increment (BAI)
#' and merges climate variables.
#'
#' @param dt.samples_trt A list of tree-ring data formatted by
#' \code{CFS_format()}.
#' @param dt.clim An optional data frame containing climate variables,
#' joined by \code{site_id} and \code{year}. Default is \code{NULL}.
#' @param calbai Logical. If \code{TRUE}, basal area increment (BAI) is
#' computed. Default is \code{TRUE}.
#'
#' @examples
#'
#' # loading processed data
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
#' # climate
#' dt.clim <- data.table::fread(system.file("extdata", "dt.clim.csv", package = "growthTrendR"))
#' # pre-data for model
#' dt.samples_clim <- prepare_samples_clim(dt.samples_trt, dt.clim)

#' @return A data frame or data.table containing tree-ring measurements,
#' optionally including basal area increment and merged climate variables.
#'
#' @export
prepare_samples_clim <- function(dt.samples_trt, dt.clim= NULL,calbai = TRUE) {

  # 1. samples_long
  dt.samples_long <- merge(
    dt.samples_trt$tr_all_wide[, c("uid_site", "site_id", "species",
                                   "uid_tree", "uid_sample",
                                   "sample_id", "radius_id", "uid_radius")],
    dt.samples_trt$tr_all_long$tr_7_ring_widths,
    by = "uid_radius"
  )

  data.table::setorder(dt.samples_long, uid_site, uid_radius, year)

  # 2. compute BAI
  if (calbai == TRUE){
  dt.samples_long <- calc_bai(dt.samples_long)
  }

  if (!is.null(dt.clim)){
    # 3. merge climate
    dt.samples_long <- merge(
      dt.samples_long,
      dt.clim,
      by = c("site_id", "year")
    )

    data.table::setorder(dt.samples_long, uid_site, uid_radius, year)
  }

  return(dt.samples_long)
}


#' Select one representative radius per tree
#'
#' Internal function to select a single measurement, sample, and radius
#' per tree based on the following rules:
#' 1. Keep the latest measurement per tree.
#' 2. Keep the sample closest to 1.3 m height per measurement.
#' 3. Keep the radius with the most ring-width series per sample.
#'
#' Basic QA checks are performed to detect duplicated uid_radius in
#' metadata and duplicated (uid_radius, year) combinations in ring-width
#' data. These checks are informative and do not stop execution.
#'
#' @param dt.meta data.table. Metadata table containing tree, sample,
#'   and radius information.
#' @param dt.rwl data.table. Ring-width table containing yearly values
#'   per radius.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{meta.sel}{Filtered metadata table.}
#'     \item{rw.sel}{Filtered ring-width table.}
#'   }
#'
#' @keywords internal
#' @noRd
sel_trees <- function(dt.meta, dt.rwl) {

  # ---------------------------
  # 0. Check required columns
  # ---------------------------
  meta_cols <- c(
    "uid_tree", "uid_meas", "meas_date",
    "uid_sample", "sample_ht_m", "uid_radius"
  )

  rwl_cols <- c("uid_radius", "year")

  missing_meta <- setdiff(meta_cols, names(dt.meta))
  missing_rwl  <- setdiff(rwl_cols, names(dt.rwl))

  if (length(missing_meta) > 0) {
    stop("dt.meta missing columns: ", paste(missing_meta, collapse = ", "))
  }

  if (length(missing_rwl) > 0) {
    stop("dt.rwl missing columns: ", paste(missing_rwl, collapse = ", "))
  }

  data.table::setDT(dt.meta)
  data.table::setDT(dt.rwl)

  # ---------------------------
  # 1. QA checks (informative)
  # ---------------------------
  if (nrow(dt.meta[, .N, by = uid_radius][N > 1]) > 0) {
    message("Multiple rows per uid_radius in dt.meta")
  }

  if (nrow(dt.rwl[, .N, by = .(uid_radius, year)][N > 1]) > 0) {
    message("Duplicate (uid_radius, year) in dt.rwl")
  }

  # ---------------------------
  # 2. Keep last measurement per tree
  # ---------------------------
  dt.unique.meas <- dt.meta[, .N, by = .(uid_tree, uid_meas, meas_date)]

  if (any(duplicated(dt.unique.meas, by = "uid_tree"))) {
    data.table::setorder(dt.unique.meas, uid_tree, meas_date)
    dt.unique.meas <- dt.unique.meas[, .SD[.N], by = uid_tree]
    dt.meta.sel <- merge(
      dt.meta,
      dt.unique.meas[, "uid_meas"],
      by = "uid_meas"
    )
  } else {
    dt.meta.sel <- dt.meta
  }

  # ---------------------------
  # 3. Sample closest to 1.3 m
  # ---------------------------
  dt.meta.sel[, dif1.3 := abs(sample_ht_m - 1.3)]

  dt.unique.sample <- dt.meta.sel[
    , .N, by = .(uid_tree, uid_meas, uid_sample, dif1.3)
  ]

  if (any(duplicated(dt.unique.sample, by = "uid_meas"))) {
    data.table::setorder(dt.unique.sample, uid_meas, dif1.3)
    dt.unique.sample <- dt.unique.sample[, .SD[1], by = uid_meas]
    dt.meta.sel <- merge(
      dt.meta.sel,
      dt.unique.sample[, "uid_sample"],
      by = "uid_sample"
    )
  }

  # ---------------------------
  # 4. Radius with most series
  # ---------------------------
  dt.unique.radius <- merge(
    dt.rwl,
    dt.meta.sel[, c("uid_sample", "uid_radius")],
    by = "uid_radius"
  )[, .(nseries = .N), by = .(uid_sample, uid_radius)]

  if (any(duplicated(dt.unique.radius, by = "uid_sample"))) {
    data.table::setorder(dt.unique.radius, uid_sample, -nseries)
    dt.unique.radius <- dt.unique.radius[, .SD[1], by = uid_sample]
    dt.meta.sel <- merge(
      dt.meta.sel,
      dt.unique.radius[, "uid_radius"],
      by = "uid_radius"
    )
  }

  # ---------------------------
  # 5. Subset ring-width table
  # ---------------------------
  dt.rwl.sel <- merge(
    dt.rwl,
    dt.meta.sel[, "uid_radius"],
    by = "uid_radius"
  )

  # ---------------------------
  # Return
  # ---------------------------
  list(
    meta.sel = dt.meta.sel,
    rw.sel   = dt.rwl.sel
  )
}

