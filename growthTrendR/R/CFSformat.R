
#' Convert tree-ring data into CFS-TRenD format
#'
#' @param data  a list, first is input data in wide format; second is a flat sequence referring to the column indices of ring measurement variables
#' @param usage 1: for submission data, 2: for cfstrend id structure to perform the analyse
#' @param out.csv  output csv file (default is NULL, otherwise to specify the directory to output)


#' @description
#' converts tree-ring data from various formats into a format compatible with hierarchical structure of the CFS-TRenD data collection (Girardin et al., 2021).
#'
#' @references Girardin, M.P., Guo, X.J., Metsaranta, J., Gervais, D., Campbell, E., Arsenault, A., Isaac-Rentone, M., Harvey, J.E., Bhatti, J., Hogg, E.A. 2021. A national tree-ring repository for Canadian forests (CFS-TRenD): structure, synthesis and applications. Environmental Reviews, 29 (999), 1-17. https://doi.org/10.1139/er-2020-0099

#' @return A list of 3 elements:
#' 1) A list containing seven tables compatible with CFS-TRenD data structure;
#' 2) A data table containing all the meta-data and ring width measurement in wide format;
#' 3) A data table for the percentage of completeness of each variable.

#'
#' @export
#' @examples
#'
#' # ring measurement
#' dt.samples <- data.table::fread(
#' system.file("extdata", "dt.samples.csv", package = "growthTrendR"))

#' # formatting the users' data conformed to CFS-TRenD data structure
#' dt.samples_trt <- CFS_format(data = list(dt.samples, 39:68), usage = 1, out.csv = NULL)

#' # save it to extdata for further use
#' # saveRDS(dt.samples_trt, file = "inst/extdata/dt.samples_trt.rds")

#'
CFS_format <- function (data, usage, out.csv = NULL) {
  if (length(data) != 2) {
    stop("pls verify data is a list with 2 elements, first is input data in wide format; second is a flat sequence referring to the column indices of ring measurement variables")
  }
  if (!(is.data.frame(data[[1]])| is.data.table(data[[1]]))) stop("the first item of data must be the complete data in wide format")
  if (is.list(data[[2]] | is.null(data[[2]]) | class(data[[2]]) != "integer" )) stop("the second item of data must be a flat sequence referring to the column indices of ring measurement variables")
  if (!(usage %in% c(1,2))) stop ("usage must be set to 1 for data submission and 2 for acquiring cfstrend data structure")

      names.data.o <- names(data[[1]])
      # if (any(str_detect(names.data.o, "uid_radius.tmp"))) stop("please rename the column 'uid_radius.tmp'")

      # Check the columns starting with uid_, these are reserved column names

      uid_matches <- names.data.o[str_detect(names.data.o, str_c( c("uid_project", "uid_site", "uid_tree", "uid_meas", "uid_sample", "uid_radius", "uid_radius.tmp"), collapse = "|"))]

      # Print the detected uid_matches

      if (length(uid_matches) > 0) stop(paste0("please rename the column(s): ", paste(uid_matches, collapse = ", ")))

      cols_rw <- names.data.o[data[[2]]]

      # pattern <- "^[A-Za-z]+[0-9]+$"
     if (!(all(grepl("^[0-9]+$", cols_rw)) | all(grepl("^[A-Za-z]+[0-9]+$", cols_rw)))) stop("colname of rw accepts 2 formats: year in numeric(NNNN) or prefix-year (CNNNN)")
      idx.y5 <- as.numeric(str_extract(cols_rw,"\\(?[0-9,.]+\\)?")) > 9999
      if (any(idx.y5)) stop(paste0("please check the colname(s): ", paste(cols_rw[idx.y5], collapse = ", ")))

       # separate the data into 2

    # meta data
    dt.meta <- data.table(uid_radius.tmp = 1:nrow(data[[1]]),data[[1]][,-data[[2]], with = FALSE])
    # tree ring
    # dt.tr <- data.table(uid_radius.tmp = 1:nrow(data[[1]]),radius_id = data[[1]]$radius_id, data[[1]][,data[[2]], with = FALSE])
    #
    # dt.rwl <- melt(dt.tr, id.vars = c("uid_radius.tmp", "radius_id"))[!is.na(value)]



    # dt.rwl <- melt(dt.tr, id.vars = c("uid_radius.tmp", "radius_id"))[!is.na(value)]
    #
    # setnames(dt.rwl, "value", "rw_mm")
    # dt.rwl[, year:= as.numeric(str_extract(variable,"\\(?[0-9,.]+\\)?"))]


  meta.all0 <- variables.cfstrend()
  meta.all <- meta.all0[!str_detect(Variable, "uid_")]
  setdiff(meta.all0$Variable, meta.all$Variable)
  # must-have meta variables for tr_1 to tr_6
  cols.musthave <-as.character( meta.all[Required == 1 & !(table %in% c("tr_7_ring_widths", "tr_8_uids_updated"))]$Variable)
  dt.meta$submission_id <- 1
  if (usage == 2) {
    dt.meta$open_data <- FALSE
    cols.musthave <- c("project_name", "site_id", "latitude", "longitude", "tree_id", "species", "sample_id", "radius_id")
}
  add.Vars <- setdiff(cols.musthave, names(dt.meta))

if (length(add.Vars) > 0) {
  message("step 1: checking mandatory columns: please verify the the following columns.... ")
  message( paste(add.Vars, collapse = ", "))
  return()
}
  # checking variable completeness
  for (i in seq_along(cols.musthave)){
    if (nrow(dt.meta[is.na(get(cols.musthave[i]))]) > 0) message (paste0(cols.musthave[i]), " not complete, please check")
  }

  message("you have filled all the mandatory information")
  dt.tr <- data.table(uid_radius.tmp = 1:nrow(data[[1]]), data[[1]][,"radius_id"], data[[1]][,data[[2]], with = FALSE])

  dt.rwl <- melt(dt.tr, id.vars = c("uid_radius.tmp", "radius_id"))[!is.na(value)]

  setnames(dt.rwl, "value", "rw_mm")
  dt.rwl[, year:= as.numeric(str_extract(variable,"\\(?[0-9,.]+\\)?"))]
  # for un-mandatory columns

  meta.all$nofill <- 0
  meta.all[Variable %in% c("year_range", "rw_ystart", "rw_yend"),nofill := 1]

  if (length(setdiff(as.character(meta.all[nofill == 0 & !(table %in% c("tr_7_ring_widths", "tr_8_uids_updated"))]$Variable), colnames(dt.meta))) == 0) message("you have filled all the un-mandatory information") else{
  message("\n the following are unmandatory information and not found in your data")

  # not required, and not reported, prompt to fill with NA
    fill.meta <- meta.all[nofill == 0 & !(table %in% c("tr_7_ring_widths", "tr_8_uids_updated")) & !(Variable %in% colnames(dt.meta))]
  # i.table <- i.table[, .(var = paste(Variable, collapse = ",")), by = table]

    message(capture.output(fill.meta[, .(var = paste(Variable, collapse = ",")), by = table]), sep = "\n")

  # for (i in 1:6){
  #   # ith table
  #   i.table <- meta.all[str_detect(table, paste0("tr_", i)) & Required != 1 & !(Variable %in% names.data.o) ]
  #   if (nrow(i.table) > 0) {
  #     message(paste0(unique(i.table$table), ": ", paste(i.table$Variable, collapse = ", ")))
  # }
  #   }
  # message("if you have filled all the information, go ahead to fill other variables")


    user_input <- readline(prompt = "do you have info of the above variables? Y to return to complete info, or N to allow the system to fill with NA  (Y/N) : ")
    if (toupper(user_input) == "Y") return() else{
    # fill.meta <- meta.all[!(nofill == 1 &  Required != 1 & Variable %in% names(dt.meta))]
    vars.fill <- as.character(fill.meta$Variable)
    FormatV.fill <- as.character(fill.meta$FormatV)

    lapply( seq_along(vars.fill), function(i) dt.meta[, c(vars.fill[i]) := eval(parse(text = FormatV.fill[i]))])
  }
  }

  # trend variables

  dt.new <- copy(dt.meta)
  setDF(dt.new)
  dt.new[dt.new == ''] <- NA

  setDT(dt.new)
  # dt.new[, uid_project := .GRP, by = .(project_name)]
  dt.new[, uid_project := as.integer(factor(project_name))]
  # chk site
  site_LL <- dt.new[, .N, by = .(site_id, latitude, longitude)]
  dup.LL <- site_LL[, .N, by = .(site_id)][N>1]
  dup.site <- site_LL[, .N, by = .(latitude, longitude)][N>1]
  if (nrow(dup.LL) > 0) stop(paste0("site_id: ", paste(dup.LL$site_id, collapse = ", "), " associated with multiple lat-lon. please verify..."))
  if (nrow(dup.site) > 0) {
    dup.site[, coord:= paste0("(", longitude, ", ", latitude, ")")]
    stop(paste0("lon-lat: ", paste(dup.site$coord, collapse = ", "), " associated with multiple site_id. please verify..."))
  }
# Canada's boundaries are roughly:
#
# Latitude range: 41.7°N to 83.1°N
# Longitude range: 52.6°W to 141.0°W
# source:  https://www12.statcan.gc.ca/census-recensement/2011/ref/dict/geo016-eng.cfm
if (all(c("latitude", "longitutde") %in% names(dt.new))){
  if (min(dt.new$latitude) < 41.7 | max(dt.new$latitude) > 83.1) stop(paste0("latitude not in range: (41.7 , 83.1)"))
  if (min(dt.new$longitude) < -141 | max(dt.new$longitude) > -52.6) stop(paste0("longitude not in range: (-141 , -52.6)"))

  # dt.new[, uid_site := .GRP, by = c("site_id", "latitude", "longitude")]
  dt.new$uid_site <- as.integer(factor(interaction(
    dt.new$site_id,
    dt.new$latitude,
    dt.new$longitude,
    drop = TRUE
  )))
  }else{
  # dt.new[, uid_site := .GRP, by = .(site_id)]
    dt.new[, uid_site := as.integer(factor(site_id))]

    }
  chk.spc<-dt.new[, .N, by = .(site_id, tree_id, species)][, .N, by = .(site_id, tree_id)][N > 1]

  if (nrow(chk.spc) > 0) {
    chk.spc[, site.tree:= paste0(site_id, "$", tree_id)]
    stop(paste0("site$tree: ", paste(chk.spc$site.tree, collapse = ", ")), " associated with multiple species, please verify...")
    }
  # chk if species is included in species_nficode
  # species_nficode is the species list of tree source, and was saved as internal data
  # chk.spc2 <- setdiff(unique(dt.new$species), species_nficode$nfi_species_code)
  # if (length(chk.spc2)  > 0) stop(paste0("species: ", paste(chk.spc2, collapse = ", "), " not recognized, please verify..."))

  # dt.new[, uid_tree := .GRP, by = .(uid_project, uid_site, tree_id)]
  dt.new <- merge(dt.new, dt.new[, .(uid_tree= .N), by = .(uid_project, uid_site, tree_id)], by = c("uid_project", "uid_site", "tree_id"))
#
#   dt.new$uid_tree <- as.integer(factor(interaction(
#      dt.new$uid_project,
#      dt.new$uid_site,
#      dt.new$tree_id,
#     drop = TRUE
#   )))

  # dt.new[, uid_meas := .GRP, by = .(uid_tree, meas_no)]
  dt.new[, meas_no2:= ifelse(is.na(meas_no), -999, meas_no)]
  dt.new$uid_meas <- as.integer(factor(interaction(
    dt.new$uid_tree,
    dt.new$meas_no2,
    drop = TRUE
  )))

  # dt.new[, uid_sample := .GRP, by = .(uid_meas, sample_id)]
  dt.new$uid_sample <- as.integer(factor(interaction(
    dt.new$uid_meas,
    dt.new$sample_id,
    drop = TRUE
  )))

  # dt.new[, uid_radius := .GRP, by = .(uid_sample, radius_id)]
  dt.new$uid_radius <- as.integer(factor(interaction(
    dt.new$uid_sample,
    dt.new$radius_id,
    drop = TRUE
  )))

  # ys <- dt.rwl[, .(rw_ystart = min(year), rw_yend = max(year)), by = c("uid_radius.tmp")]# Using aggregate()
  ys <- aggregate(
    year ~ uid_radius.tmp,
    data = dt.rwl,
    FUN = function(x) c(rw_ystart = min(x), rw_yend = max(x))
  )
  # Unpack the matrix column into separate columns
  ys <- cbind(ys[, "uid_radius.tmp", drop = FALSE], as.data.frame(ys$year))

  setorder(dt.rwl, uid_radius.tmp, year)
  # dt.rwl[, ydif:= year - data.table::shift(year), by = "uid_radius.tmp"]
  dt.rwl$ydif <- safe_group_diff(
    dt.rwl$year,
    dt.rwl$uid_radius.tmp
  )
  # dt.rwl[, rwinc:= rw_mm - data.table::shift(rw_mm), by = "uid_radius.tmp"]
  dt.rwl$rwinc <- safe_group_diff(
    dt.rwl$rw_mm,
    dt.rwl$uid_radius.tmp
  )
  chk.ydif <- dt.rwl[, .SD[-1], by = uid_radius.tmp][is.na(ydif)][, .N, by = .(radius_id)]
  if (nrow(chk.ydif) > 0) stop(paste0("radius_id: ", paste(chk.ydif$radius_id, collapse = ", "), " with missing year, please verify..." ))
  # str(dt.rwl)
  chk.rw <- dt.rwl[ rw_mm < 0][, .N, by = .(radius_id)]
  if (nrow(chk.rw) > 0) stop(paste0("radius_id: ", paste(chk.ydif$radius_id, collapse = ", "), " with negative rw measurement, please verify..." ))

  dt.new <- dt.new[ys, on = .(uid_radius.tmp)]
  dt.new[, c("ymin.proj", "ymax.proj"):= .(min(rw_ystart), max(rw_yend) ), by = .(uid_project)]
  # dt.new[, yr.meas:= max(ys), by = .(uid_meas)]
  dt.new <- merge(dt.new, dt.new[, .(yr.meas= max(ys)), by = uid_meas], by = "uid_meas")
  dt.new[is.na(meas_date), meas_date:= paste0(yr.meas, "-00-00")]
  dt.new[, year_range:= paste0(ymin.proj, " ; ", ymax.proj)]
  #

  # in 7 tables
  fn7 <- sort(unique(meta.all0$table))

  for (i.tbl in 1:6){
    ord <- unlist(meta.all0[table == fn7[i.tbl]]$Variable)
    tmp <- dt.new[, .N, by = eval(ord)][, N:=NULL]
     assign(fn7[i.tbl], tmp)
    # check completeness
     if (i.tbl == 4) tmp[meas_no == "-999" | meas_no == -999, meas_no:=NA]
     if (i.tbl == 6) tmp[bark_thickness_mm == 999, bark_thickness_mm:= NA_real_]
    setDF(tmp)
     # tmp<-tmp[,!grepl( "uid_" , names( tmp ) )]
    tmp[tmp == ''] <- NA

    v_count <-sapply(tmp, function(y) sum(length(which(!is.na(y)))))
    v_count <- round(v_count / dim(tmp)[1] * 100, 1)

    v_count <- data.table(a = matrix(v_count, ncol = 1))
    names(v_count) <- "pct"
    v_count$var <- paste0("tr", i.tbl, "_", ord)
    # remove uids
    v_count <- v_count[!str_detect(var, "uid_") ]
    setcolorder(v_count, c("var", "pct"))
      if (i.tbl == 1)
        complete_vars <- v_count else
          complete_vars <- rbind(complete_vars, v_count)


  }



  tr_7_ring_widths <- merge(dt.new[, c("uid_radius.tmp", "uid_radius")], dt.rwl, by = "uid_radius.tmp")[, c("uid_radius","year" , "rw_mm")]
  setorder(tr_7_ring_widths, uid_radius,year)
  tr_7_ring_widths <- tr_7_ring_widths[, as.character(meta.all0[table == fn7[7]]$Variable), with = FALSE]

  # ylast <- tr_7_ring_widths[, .(ystart = min(year), ylast= max(year)), by = .(uid_radius)]
# wide format
  dt.new <- dt.new[, unique(meta.all0[!(table %in% c("tr_7_ring_widths", "tr_8_uids_updated"))]$Variable), with = FALSE]
  # dt.new <- merge(dt.new, ylast, by = "uid_radius")
  tr_all_wide <- dcast(tr_7_ring_widths, uid_radius ~ year, value.var = "rw_mm")
  tr_all_wide <- merge(dt.new, tr_all_wide, by = "uid_radius")
  if (!is.null(out.csv)){

      if (!(dir.exists(out.csv))) dir.create(out.csv, recursive = TRUE)

      for (i.tbl in 1:7){
        utils::write.csv(eval(parse(text = fn7[i.tbl])), file =file.path(out.csv, paste0(fn7[i.tbl], ".csv" )), na = "",  row.names = FALSE)

      }
      utils::write.csv(tr_all_wide, file =file.path(out.csv, paste0("tr_all_wide", ".csv" )), na = "",  row.names = FALSE)
      utils::write.csv(complete_vars, file =file.path(out.csv, paste0("complete_vars", ".csv" )), na = "",  row.names = FALSE)


      }
  result <- list(  tr_all_long = list(tr_1_projects = tr_1_projects, tr_2_sites= tr_2_sites, tr_3_trees = tr_3_trees, tr_4_meas = tr_4_meas, tr_5_samples = tr_5_samples,
                                      tr_6_radiuses = tr_6_radiuses,  tr_7_ring_widths = tr_7_ring_widths), tr_all_wide = tr_all_wide, complete_vars = complete_vars)

  class(result) <- "cfs_format"
  return( result )

}



#' frequency distributions by geo-location per species
#'
#' @param tr_meta   meta table from function CFS_format()
#' @param freq.label_data description of tr_meta
#' @param freq.uid_level which uid level to count(uid_project, uid_site, uid_tree, uid_meas, uid_sample, uid_radius)
#' @param freq.cutoff_year cut-off year for a subset which series were recorded on or after
#' @param freq.geo_resolution resolution of longitude and latitude in degree, default: c(5,5)

#' @return a data table of counts of uid by latitude-longitude per species
#' @export CFS_freq
#'
#' @examples
#'
#'
#' # treated ring measurement
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
#' # Compute frequency statistics at radius level
#' dt.freq <- CFS_freq(
#'   dt.samples_trt$tr_all_wide,
#'   freq.label_data = "demo-samples",
#'   freq.uid_level  = "uid_radius"
#' )
#' class(dt.freq)



CFS_freq <- function(tr_meta, freq.label_data = "", freq.uid_level = "uid_radius", freq.cutoff_year = -999,freq.geo_resolution = NULL){
  if (!(freq.uid_level %in% c("uid_project", "uid_site", "uid_tree", "uid_meas", "uid_sample", "uid_radius"))) stop("freq.uid_level should be in c('uid_project', 'uid_site', 'uid_tree', 'uid_meas', 'uid_sample', 'uid_radius')")


  dt.meta.sel <- tr_meta[rw_yend >= freq.cutoff_year, c(unique(c("uid_radius", freq.uid_level)), "longitude", "latitude",  "species"), with = FALSE]

  if (is.null(freq.geo_resolution) ) {
    freq.geo_resolution[1] <- (max(tr_meta$longitude) - min(tr_meta$longitude))/4
    freq.geo_resolution[2] <- (max(tr_meta$latitude) - min(tr_meta$latitude))/4
  }
  dt.meta.sel[,lon:=round(longitude/freq.geo_resolution[1], 0)*freq.geo_resolution[1]]
  dt.meta.sel[, lat:= round(latitude/freq.geo_resolution[2], 0)*freq.geo_resolution[2]]

  uids.sll <- dt.meta.sel[, .N, by = c("lat", "lon", "species", freq.uid_level)][,N:= NULL]
  uids.spc <- uids.sll[,.N, by = .(species)]
  setorder(uids.spc, -N)
  uids.spc[, pct.species := N/sum(N)]
  uids.spc[, pct.species := round(pct.species * 100,0)]
  uids.spc[, ord:= .I]
  uids.sll <- uids.sll[uids.spc[,c("species", "pct.species", "N", "ord")], on = .(species)]

  dist_uids <- uids.sll[, .(nuids = .N), by = .(ord, species, pct.species, N, lat, lon)]


  dist_uids <- dcast(dist_uids, ord + species + N + pct.species + lat ~ lon, value.var = "nuids")
  if (freq.cutoff_year > 0) dist_uids <- data.table(uid_label = paste0(freq.uid_level, "_yr", freq.cutoff_year), dist_uids) else
    dist_uids <- data.table(uid_label = freq.uid_level, dist_uids)
  freq.parms = list(freq.label_data = freq.label_data,  freq.uid_level = freq.uid_level, freq.cutoff_year = freq.cutoff_year,freq.geo_resolution = freq.geo_resolution)
  setorder(dist_uids, -pct.species, species, -lat)
  result <- list(dist_uids = dist_uids, freq.parms = freq.parms)
  class(result) <- "cfs_freq"

  return(result)
}


