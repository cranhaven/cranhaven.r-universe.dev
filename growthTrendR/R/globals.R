# to avoid warnings in checking for data.table variables
utils::globalVariables(c( ":=", ".", "!!"))
utils::globalVariables(c( "Format", "FormatV"))

utils::globalVariables(c(
  "N", "Required", "Variable", "latitude", "longitude", "meas_no2",
  "meas_no","project_name", "radius_id", "sample_id", "site_id", "tr_1_projects", "tr_2_sites",
  "tr_3_trees", "tr_4_meas", "tr_5_samples", "tr_6_radiuses", "tree_id", "uid_meas",
  "uid_project", "uid_radius", "uid_sample", "uid_site", "uid_tree",
  "coord", "rwinc", "site.tree", "species_nficode", "ydif",
  "aic",  "decade.yr",  "dif.decade",   "fit.s.ageC", "fn",   "i",
  "id.core",    "id2",  "lines","nc.yr",  "rest", "rw", "i.na",
  "rw.N", "rw.rest",    "rw.sc","rw.tmp",     "rw_int" ,    "rw_mm","se.fit.bai",
  "se.fit.s.ageC", "selected",   "site_ID1.01.0", "specialchar",   "v.idx",
  "lat", "lon", "meas_date", "ord", "species", "uid_radius.tmp", "value", "variable",
  "year_range", "ymax", "ymax.proj", "ymin", "ymin.proj", "yr.meas",
  "rw_yend", "rw_ystart","nofill",

  # CFS_scale
  "rw.median",  "yr.max", "yr.mn", "ratio_median", "rw.median.nbs", "size_class",
  "list.tbl", "pct.species", "spc.pct", "nuids", "bark_thickness_mm",
  "block_id","rw.N.sc","dist_to_chk_m",

  # CFS_qa
  "acf.trt", "ccf.ord", "max_lag", "max_ccf", "SampleID.chr", "RawRing", "RW_trt", "Year",
  "mean.rw.dif","mean.rw", "dt.trt.wide", "qa_code", "col.ord",
  "SampleID",  "colr", "id.label", "rw.treated",
   "isolated", "main_max", "main_min", "year_max",   "year_min",
  # gamm_main
  "ageC", "res.normalized", "res.normalized.LL",  "res.resp_normalized",
  "lat_use", "lon_use", "start.event", "aicc", "importance_pct",
  # calc_bai
  "ba_cm2_t_1", "bai_cm2", "radius", "radius_prev", "med.rw",
  # tab_...
  "ar1_rw", "corr_mean_rw", "mean_rw",  "pcorr_mean_rw", "rw.max", "rw.mean","rw.min", "result",
  # CFS_scale
  "chk.site", "dist_to_chk_km",
  # plot_mapping
  "species.inuse", "non_year_cols",
  # sel_trees
  "dif1.3", "nseries", "sample_ht_m" ,
  #plot_resp
  "lwr", "upr", "maxv", "minv",'group',"x","group", "y.resp", 'byterm', 'term', 'fit'

  )
)


