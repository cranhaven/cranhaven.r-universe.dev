#' #' Prepare variable names and column order for each table
#'
#' Internal function to return variable descriptions for all tables.
#'
#' @return A data.table of variable descriptions of all tables
#' @keywords internal
#' @noRd
variables.cfstrend <- function(){
  # data structure version, it changes with the structure changes,
  Ver.stru <- "s1.2"
meta.Projects <-data.table(Variable = c( "uid_project", "submission_id", "project_name", "description",  "year_range", "reference", "open_data",  "contact1", "contact2"),
                         Format = c("integer", "integer", "character",  "character",  "character", "character",  "logical","character","character"),
                          Description = c("unique project identification used in treeSource","submission identification", "original project identification",  "Project description",
                                          "year range of the project", "References", "open to public","Name of primary person responsible for the data","Name of secondary person responsible for the data"),
                         Required = c(0,1,1,2,2,2,1,1,2)
)[, col.ord := seq_len(.N)]

meta.Sites <-data.table(Variable = c( "uid_site",  "site_id",    "latitude" ,      "longitude",  "datasource", "investigators", "province_iso_code"),
                           Format = c("integer", "character",     "numeric",         "numeric", "character", "character", "character"),

                           Description = c("unique site identification used in treeSource",
                                           "original site identification",   "  decimal degrees (positive for N)",
                                  "decimal degrees (negative for W)", "inventory/target", "Investigators", "province iso code"),
                           Required = c(0,1,1,1,1,2,1)
)[, col.ord := seq_len(.N)]


meta.Trees <-data.table(Variable = c( "uid_tree", "uid_site", "tree_id",  "species", "uid_project"),
                       Format = c("integer","integer","character",  "character","integer"),
                       Description = c("unique tree identification used in treeSource", "unique site identification used in treeSource","original tree identification",
                                       "use 7 character species codes from NFI with no spaces (e.g.; PICEMAR; POPUTRE)", "unique project identification used in treeSource"),
                       Required = c(0,0,1,1,0)
                      )[, col.ord := seq_len(.N)]



meta.Meas <-data.table(Variable = c( "uid_meas", "uid_tree",  "meas_no",   "meas_date", "status",    "dbh_cm",    "ht_tot_m"),
                       Format = c("integer","integer", "integer", "character",  "character", "numeric","numeric"),
                       Description = c("unique measurement identification used in treeSource", "unique tree identification used in treeSource",
                                       "original measurement identification, 0 for missing info", "measurement date, last year in tree_RW in case missing",
                                       "health status: LIVE/DEAD","Stem diameter (cm) at breast height (ca. 1.3 m)", "Total tree height (m)"),
                       Required = c(0,0,2,2,2,2,2)
)[, col.ord := seq_len(.N)]


meta.Samples <-data.table(Variable = c( "uid_sample", "uid_meas", "sample_id", "sample_type",   "sample_ht_m",     "sample_diameter_cm"),
                         Format = c( "integer",  "integer", "character", "character",    "numeric",   "numeric"),
                         Description = c("unique sample identification used in treeSource", "unique measurement identification used in treeSource", "sample ID",
                                        "sample type: core/disk",  "height at point of sample (m)", "diameter at point of sample (cm)"),
                         Required = c(0,0,1,2,2,2)
)[, col.ord := seq_len(.N)]


meta.Radius <-data.table(Variable = c( "uid_radius", "uid_sample", "radius_id", "cofecha_id", "ring_meas_method",
                                      "crossdating_visual", "crossdating_validation", "age_corrected", "bark_thickness_mm", "radius_inside_cm",
                                      "dtc_measured_mm", "dtc_estimated_mm", "rw_ystart", "rw_yend",  "comments" ),
                           Format = c( "integer",  "integer", "character",  "character", "character",
                                       "logical", "character", "numeric",  "numeric", "numeric",
                                        "numeric" , "numeric", "numeric" , "numeric", "character"),
                           Description = c("unique radius/core identification used in treeSource", "unique sample identification used in treeSource",
                                          "original radius/core identification, \"O\" for missing", "identification used in cofecha",  "ring measurement method: windendro/velmex/coorecorder",
                                          "visaully cross-dated? yes/no", "validation cross-dated by: CDENDRO/COFECHA",
                                          "Corrected age (with rings added based on fresh DTC divided by fresh pith increment)",
                                          "thickness of the bark (mm)", "Core length corrected to Fresh tree dimensions outside bark (cm)",
                                          "Measured distance from start of earliest ring to centre (mm)",
                                          "Gap-filled distance from start of earliest ring to centre (mm)",
                                          "starting year of the core", "ending year of the core", "user notes on the core"),
                           Required = c(0,0,1,2,2,1,1,2,2,2,2,2,2,2,2)
                           )[, col.ord := seq_len(.N)]


# v1.2 uid_ringwidth not necessary, using uid_radius + year as primary key
meta.Ringwidths <-data.table(Variable = c("uid_radius", "year",  "rw_mm"),
                         Format = c(  "integer",  "integer", "numeric"),
                         Description = c( "unique radius/core identification used in treeSource",
                                         "Year", "ring width (mm)"),
                         Required = c(0,1,1)
)[, col.ord := seq_len(.N)]

# V1.2 uids being removed
meta.uids_updated <-data.table(Variable = c("modification_id", "action", "uid_affected", "uid_level",  "submission_id", "uid_project",  "ver_firstupdated",  "ver_lastexist", "reason", "investigator"),
                               Format = c(  "integer","character",   "integer",  "character", "integer",  "integer", "character","character","character","character"),
                               Description = c("modification id", "Delete (D) or Modify (M) ring widths", "uid affected", "the level of uid_affected below(including) which all uids be affected(uid_XXX)",
                                               "submission_id which uids_affected belongs to",
                                                "uid_project which uid_affected belongs to","the first version in which uid_affected get updated",
                                                "the last version before uid_affected get updated", "reason for updating uid_affected", "investigator"),
                               Required = c(1,1,0,1,1,0,1,1,1,1)
)[, col.ord := seq_len(.N)]




# ts.lst <- c("tr_1_projects", "tr_2_sites", "tr_3_trees", "tr_4_meas", "tr_5_samples", "tr_6_radiuses", "tr_7_ring_widths")

meta.all <- rbind(meta.Projects[,table := "tr_1_projects"],
                  meta.Sites[,table := "tr_2_sites"],
                  meta.Trees[,table := "tr_3_trees"],
                  meta.Meas[,table := "tr_4_meas"],
                  meta.Samples[,table := "tr_5_samples"],
                  meta.Radius[,table := "tr_6_radiuses"],
                  meta.Ringwidths[,table := "tr_7_ring_widths"],
                  meta.uids_updated[,table := "tr_8_uids_updated"]
                  )
# meta.all[, col.ord:=seq_len(.N), by = .(table)]
meta.all$ver.structure <- Ver.stru
# print(is.data.table(meta.all))
# Separate the elements with commas

meta.all[Format == "integer", FormatV:= "NA_integer_"]
meta.all[Format == "character", FormatV:= "NA_character_"]
meta.all[Format == "numeric", FormatV:= "NA_real_"]
meta.all[Format == "logical", FormatV:= "NA"]

# meta.all <- meta.all[!str_detect(Variable, "uid_")]

# ord
# {
#   ord.project <- as.character(meta.Projects$Variable)
#   ord.site <- as.character(meta.Sites$Variable)
#   ord.tree <- as.character(meta.Trees$Variable)
#   ord.meas <- as.character(meta.Meas$Variable)
#   ord.sample <- as.character(meta.Samples$Variable)
#   ord.radius <- as.character(meta.Radius$Variable)
#   ord.width <- as.character(meta.Ringwidths$Variable)
#   ord.all <- list(ord.project, ord.site, ord.tree, ord.meas, ord.sample, ord.radius, ord.width)
# }
return(meta.all)
}


# update.YN <- 0
#
# if (update.YN == 1){
#   str.require <- data.table(Value = c(0,1,2), comments = c("treeSource ID", "must-have variables", "good-to-have variables"))
# # rm(list=ls(pattern="^meta\\."))
# # update data structure
# list.meta <- c("meta.Projects", "meta.Sites", "meta.Trees", "meta.Meas", "meta.Samples", "meta.Radius", "meta.Ringwidths")
# require(XLConnect)
# wb <- XLConnect::loadWorkbook("E:/Git/tr/documents/data structure.xlsx", create = TRUE)
#
# #creating sheets within an Excel workbook
# for (i in 1:7){
#   XLConnect::createSheet(wb, name = list.meta[i])
#   clearSheet(wb, sheet = list.meta[i])
#   XLConnect::writeWorksheet(wb, eval(parse(text = list.meta[i])), sheet = list.meta[i], startRow = 1, startCol = 1)
#   XLConnect::setColumnWidth(wb, sheet = list.meta[i], column = c(1,2,3,4), width = c(6000, 3000, 18000, 2500))
#
# }
# }
# #writing into sheets within an Excel workbook :
# XLConnect::createSheet(wb, name = "info")
# XLConnect::writeWorksheet(wb, paste0("Version: ", Ver.df), sheet = "info",  header = FALSE,startRow = 2, startCol = 1)
# XLConnect::writeWorksheet(wb, "Required value", sheet = "info", header = FALSE, startRow = 5, startCol = 1)
# XLConnect::writeWorksheet(wb, str.require, sheet = "info", header = FALSE, startRow = 6, startCol = 1)
# setColumnWidth(wb, sheet = "info", column = c(1,2), width = c(3500, 8000))
# XLConnect::saveWorkbook(wb)
#
# }




