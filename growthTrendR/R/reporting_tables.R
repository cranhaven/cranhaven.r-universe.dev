



#' Generate table for species report
#'
#' This function generates the data table for data reporting at project-species level .
#' @param tr_6i data table of meta data for a specific species
#' @param dt.spc_radii data table of tree ring data for a specific species

#'
#' @return a data table.
# #' @export

#' @keywords internal
#' @noRd
table_spc <- function(tr_6i, dt.spc_radii){

  dt.radii.spc <- merge(tr_6i, dt.spc_radii, by = "uid_radius")
  # dt.radii.spc <- dt.radii[species == spc.lst[i.spc]]
  dt.summary.0 <- tr_6i[, .(lat = paste0(round(min(latitude),1), " ", round(max(latitude),1)),
                            lon = paste0(round(min(longitude),1), " ", round(max(longitude),1))), by = .(species)]

  setnames(dt.summary.0, c("lat", "lon"), c("Latitude range", "Longitude range"))
  dt.summary.0a <- melt(dt.summary.0, measure.vars = names(dt.summary.0), variable.name = "Category", value.name = "Value")





  dt.summary.1 <- dt.radii.spc[, .(Yspan = paste0(min(ymin), " ", max(ymax)), Nsites = length(unique(uid_site)), Ntrees = length(unique(uid_tree)),
                                   Nsamples = length(unique(uid_sample)), N = .N,
                                   summ_rw = paste0(round(mean(rw.mean), 1), " \u00B1 ", round(sd(rw.mean), 1), " ( ", round(min(rw.min), 1), ", ", round(max(rw.max), 1), " )"),
                                   summ_len = paste0(round(mean(N), 1), " \u00B1 ", round(sd(N), 1), " ( ", round(min(N), 1), ", ", round(max(N), 1), " )"),
                                   summ_ar1 = paste0(round(mean(ar1_rw), 1), " \u00B1 ", round(sd(ar1_rw), 1), " ( ", round(min(ar1_rw), 1), ", ", round(max(ar1_rw), 1), " )"))]

  dt.summary.1[, (names(dt.summary.1)) := lapply(.SD, as.character)]
  setnames(dt.summary.1, c("Yspan","Nsites", "Ntrees", "Nsamples", "N", "summ_rw", "summ_len", "summ_ar1"), c("Year span", "Number of sites", "Number of trees", "Number of samples", "Number of series", "summary rw(mm)**", "summary len.series**", "summary AR1**"  ))

  dt.summary.1a <- melt(dt.summary.1, measure.vars = names(dt.summary.1), variable.name = "Category", value.name = "Value")

  dt.summary.2a <- rbind(dt.summary.0a, dt.summary.1a)


  if ("qa_code" %in% names(dt.radii.spc))  {
    dt.Npass<- data.table(Category = "Number of series (pass)*", Value = nrow(dt.radii.spc[qa_code == "pass"]), ord = 8.5 )} else{
      dt.Npass<- data.table(Category = "Number of series (pass)*", Value = "Not Applicable", ord = 8.5 )
    }
  dt.summary.2a[, ord:=.I]
  summary_spc <- rbind(dt.summary.2a, dt.Npass)
  setorder(summary_spc, ord)
  summary_spc[,ord:=NULL]
  # dt.summary.3a$species <- unique(tr_6ispc$species)
  return(summary_spc )
}



#' Generate table for site report including the kNN result
#'
#' This function generates the data table for data reporting at project-species-site level .
#' @param dt.radii.spc result table from table_spc_site_radii
#' @param rw_ref data table of reference for kNN, in wide format
#' @param scale.max_dist_km maximum distance to search the neighbors in km
#' @param scale.N_nbs number of neighbor to search

#'
#' @return a data table.
# #' @export

#' @keywords internal
#' @noRd
table_spc_site <- function(dt.radii.spc, rw_ref,  scale.N_nbs, scale.max_dist_km){
  dt.site_radii <-merge(rw_ref[,c("uid_site", "site_id", "species", "latitude", "longitude", "uid_tree", "uid_sample", "uid_radius")],
                        dt.radii.spc[, c("uid_radius", "N", "rw.mean", "rw.min", "rw.max")], by = "uid_radius")
  # dt.site.stats <- dt.site[, .(Ntrees = length(unique(uid_tree)), Nsamples = length(unique(uid_sample)), Nradius = .N), by = .(uid_site, site_id, longitude, latitude, species)]


  dt.site.stats <- dt.site_radii[ ,.(
    Ntrees = length(unique(uid_tree)), Nsamples = length(unique(uid_sample)), Nradius = .N,
    summ_len = paste0(round(mean(N), 1), " \u00B1 ", round(sd(N), 1), " ( ", round(min(N), 1), ", ", round(max(N), 1), " )"),
    summ_rw = paste0(round(mean(rw.mean), 1), " \u00B1 ", round(sd(rw.mean), 1), " ( ", round(min(rw.min), 1), ", ", round(max(rw.max), 1), " )")),
    by = .(uid_site, site_id, longitude, latitude, species)]
  # setnames(dt.site.stats, c("summ_rw", "summ_len"), c("summary rw(mm)**", "summary len.series**"))

  # head(dt.site.stats)
  if (length(unique(dt.site.stats$uid_site)) < scale.N_nbs + 1) {
    message("not sufficiant reference sites")
    dt.site.out <- dt.site.stats
  } else{


    list.scale <- CFS_scale(target_site = dt.site.stats[ ,c("species", "site_id")], ref_sites = rw_ref, scale.max_dist_km = scale.max_dist_km, scale.N_nbs = scale.N_nbs)

    dt.scale.all <- data.table::rbindlist(
      lapply(list.scale, `[[`, "ratio.median"),
      use.names = TRUE, fill = TRUE
    )
    # summary table of spc

    dt.site.out <- merge(dt.site.stats, dt.scale.all[, c("uid_site", "rw.median", "rw.median.nbs", "ratio_median")], all.x = TRUE,  by = "uid_site")
  }
  return(dt.site.out)
}

