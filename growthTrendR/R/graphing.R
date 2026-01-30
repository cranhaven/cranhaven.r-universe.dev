#' plotting quality assessment per radius
#' @description
#' Plotting the time series and cross-correlation to contrast each radius with the chronologies, using both raw ring-width measurements and treated series.
#'
#' @param qa.trt object "cfs_qa" from CFS_qa() function.
#' @param qa.out_series series_id list to be plotted, default "all" for plotting the graphs for all.

#' @return A named list of tree-ring series, where each element corresponds to
#' a series requested via \code{qa.out_series}. Each element is itself a list
#' containing four \code{ggplot} objects:
#' \describe{
#'   \item{plot.raw.series}{Raw tree-ring series vs year.}
#'   \item{plot.trt.series}{Treated (detrended/standardized) series vs year.}
#'   \item{plot.raw.ccf}{Cross-correlation function of the raw series.}
#'   \item{plot.trt.ccf}{Cross-correlation function of the treated series.}
#' }

#' @export plot_qa
#' @examples
#' \donttest{
#'
#' # loading processed data
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
#' # data processing
#' dt.samples_long <- prepare_samples_clim(
#' dt.samples_trt = dt.samples_trt, calbai = FALSE )

#' # rename to the reserved column name
#' data.table::setnames(
#' dt.samples_long,
#' c("sample_id", "year", "rw_mm"),
#' c("SampleID", "Year" ,"RawRing"))

#' # assign treated series
#' # users can decide their own treated series
#'
#' # for rhub::rhub_check() on macos VECTOR_ELT issues
#' data.table::setorder(dt.samples_long, SampleID, Year)
#' dt.samples_long$RW_trt <-
#'   ave(
#'   as.numeric(dt.samples_long$RawRing),
#'   dt.samples_long$SampleID,
#'   FUN = function(x)
#'   if (length(x) > 1L) c(NA_real_, diff(x)) else NA_real_
#'   )

#' # quality check on radius alignment based on the treated series
#' dt.qa <-CFS_qa(dt.input = dt.samples_long, qa.label_data = "demo-samples",
#' qa.label_trt = "difference", qa.min_nseries = 5)
#' plots.lst <- plot_qa(dt.qa, qa.out_series = "X003_101_005")
#'}
#'
plot_qa <- function(qa.trt, qa.out_series = "all" ) {

  if (!inherits(qa.trt, "cfs_qa")) stop("please check the input of qa.trt, make sure it's the result of CFS_qa() function")
  dt.trt.series <- qa.trt$dt.plots$dt.trt.series
  dt.raw.series <- qa.trt$dt.plots$dt.raw.series


  dt.trt.ccf <- qa.trt$dt.plots$dt.trt.ccf
  dt.raw.ccf <- qa.trt$dt.plots$dt.raw.ccf


  samples.all <- str_split_fixed(colnames(dt.trt.series)[3:ncol(dt.trt.series)], "\\_",2)[,2]

  if (all(tolower(qa.out_series) == "all") == TRUE) idx.lst <- seq_along(samples.all) else {
    idx.lst <- sapply(qa.out_series, function(item) match(item, samples.all))
    idx.lst <- idx.lst[!is.na(idx.lst)]

  }


  if (any(is.na(idx.lst))) stop("please verify qa.out_series, some or all cannot be found...")

  # ccf of all samples with the chronologies
  plot.trt.series <- lapply(2+idx.lst, create_plot.series, data = dt.trt.series)
  plot.raw.series <- lapply(2+idx.lst, create_plot.series, data = dt.raw.series)
  names(plot.trt.series) <- samples.all[idx.lst]
  names(plot.raw.series) <- samples.all[idx.lst]


  plot.trt.ccf <- lapply(1+idx.lst, create_barplot, data = dt.trt.ccf)
  plot.raw.ccf <- lapply(1+idx.lst, create_barplot, data = dt.raw.ccf)

  names(plot.trt.ccf) <- samples.all[idx.lst]
  names(plot.raw.ccf) <- samples.all[idx.lst]

  # print(plot.trt.series[[2]])
  # print(plot.raw.ccf[[2]])
  # print(plot.trt.ccf[[2]])
  # plot.lst <- list(plot.raw.series, plot.trt.series, plot.raw.ccf, plot.trt.ccf )
  # plot.lst <- list(plot.raw.series = plot.raw.series, plot.trt.series = plot.trt.series, plot.raw.ccf = plot.raw.ccf, plot.trt.ccf = plot.trt.ccf)


  # plot.lst <- generate.plots(master.trt, dt.input)


  return(list(plot.raw.series = plot.raw.series, plot.trt.series = plot.trt.series, plot.raw.ccf = plot.raw.ccf, plot.trt.ccf = plot.trt.ccf))


}



#' @keywords internal
#' @noRd
#'
create_plot.series <- function(icol, data) {
  if (str_detect(deparse(substitute(data)), "trt")) {

    label <- "treated "}else{

      label <- "raw"}
  sampleID<- names(data)[icol]
  p <- ggplot(data, aes(x = Year)) +
    geom_line(aes(y = get(sampleID), color = 'Tree'), na.rm = TRUE) +
    geom_point(aes(y = get(sampleID), color = 'Tree'), shape = 21, size = 2, fill = "white", na.rm = TRUE) +
    geom_line(aes(y = get(names(data)[2]), color = 'Chron'), na.rm = TRUE) +
    geom_point(aes(y = get(names(data)[2]), color = 'Chron'), shape = 21, size = 2, fill = "white", na.rm = TRUE) +
    labs(
      title = paste(label," ", str_sub(sampleID, 3, -1)),
      # title = bquote(bold(.(a_d)) ~ .(label) ~ .(str_sub(sampleID, 3))),
      #
      x = "Year", y = paste0(label, " rw (mm)"),
      color = "Series") +
    theme_minimal()
  return(p)
}


#' @keywords internal
#' @noRd
#'
create_barplot <- function(icol, data) {
  sampleID.chr <- names(data)[icol]
  if (str_detect(deparse(substitute(data)), "trt")) {
    parts <- str_split_fixed(names(data)[icol], "\\$",3)
    test <- parts[, 2]
    lagmax <- as.integer(parts[, 3])
    sampleID.o <- parts[, 1]

    label <- "treated"
    dt.clrs <- data.table(lag = -10:10)
    dt.clrs[, colr:= ifelse(lag == lagmax, "red", "black")]
    dt.clrs[lag == 0 & colr == 'black', colr:= "blue"]
    clrs <- setNames(as.character(dt.clrs$colr), as.character(dt.clrs$lag))
  } else {

    label <- "raw"
    test <- ""
    sampleID.o <- str_split_fixed(sampleID.chr, "\\_",2)[,2]
    dt.clrs <- data.table(lag = -10:10)
    dt.clrs[, colr:=  "black"]
    dt.clrs[lag == 0 , colr:= "blue"]
    clrs <- setNames(as.character(dt.clrs$colr), as.character(dt.clrs$lag))
  }


  # Create the bar plot
  p <- ggplot(data, aes(x = as.factor(lag), y = get(sampleID.chr), fill = as.factor(lag))) +
    geom_bar(stat = "identity",na.rm = TRUE) +
    scale_fill_manual(values = clrs) +
    # scale_fill_manual(values = clrs) +
    labs(
      # title = bquote(bold(.(a_d)) ~ "ccf_" * .(label) ~ .(sampleID.o) ~ .(test)),
      # title = bquote(bold(.(var)) ~ "ccf" * .(label))
      title = ifelse(label == "raw", "", paste( " qa_code: ", test)),
      x = "lag", y = paste0("correlation with ", label, " chronologies"),
      color = "Series") +
    ylim(-1, 1) +
    theme_minimal() +
    theme(legend.position = "none")
  return(p)
}





#' plot frequency distribution by geo-location per species
#' @description
#' This function plots the frequency distribution by geo-location for each species
#'
#' @param dt.freq a table with class "cfs_freq", resulting from function CFS_freq()
#' @param out.species species list, default is 'all' to output the frequency distribution for all species
#' @return A list of \code{ggplot} objects corresponding to the species
#' requested via \code{out.species}. Each element of the list contains
#' a faceted spatial plot of tree locations, with point size proportional
#' to the number of samples.

#' @export plot_freq
#' @examples
#'
#' # loading processed data
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))

#' dt.freq <- CFS_freq(
#' dt.samples_trt$tr_all_wide,
#' freq.label_data = "demo-samples",
#' freq.uid_level = "uid_radius")
#'
#' plots.lst <- plot_freq(dt.freq)
#'
#'

plot_freq <- function(dt.freq, out.species = "all" ) {
  if (!inherits(dt.freq, "cfs_freq")) stop("please check the input of dt.freq, make sure it's the result of CFS_freq() function")
  uid_label <- str_split(unique(dt.freq$dist_uids$uid_label), "_yr",2)[[1]]
  # if (!is.na(uid_label[2])) title.tmp <- paste0( str_sub(uid_label[1], 5), " distribution by year ",uid_label[2]) else
  #   title.tmp <- paste0( str_sub(uid_label[1], 5), " distribution")
  # print(uid_label)
  dist.uids <- melt(dt.freq$dist_uids, id.vars = c("uid_label", "species" , "ord", "N", "pct.species" , "lat"),
                  variable.name = "lon",
                  value.name = "nuids")[!is.na(nuids)]
  dist.uids[, lon:= as.numeric(as.character(lon))]
  setorder(dist.uids, ord, lon, lat)
  if (all(tolower(out.species) == "all")) data.tmp <- dist.uids else{

    data.tmp <- dist.uids[species %in% out.species]
  }





  if (nrow(data.tmp) == 0) stop("cannot find qa.out_series, please verify if they exist in sample.lst")
  # plot.lst <- generate.plots(master.trt, dt.input)



  data.tmp[, spc.pct := paste0(species, " N:", N, ", ", pct.species, "% (", ord, ")")]
  data.tmp$spc.pct <- factor(data.tmp$spc.pct, levels = unique(data.tmp$spc.pct[order(data.tmp$ord)]))
  # Get the unique spc.pct values
  spc_pcts <- unique(data.tmp$spc.pct)

  # Split into chunks
  chunks <- split(spc_pcts, ceiling(seq_along(spc_pcts) / 2))


plt.lst <- list()
  for (i in seq_along(chunks)) {
    # Subset data for the current chunk
    data.i <- data.tmp[data.tmp$spc.pct %in% chunks[[i]], ]

    p1 <- ggplot(data.i, aes(x = lon, y = lat, size = nuids)) + facet_wrap(~spc.pct, ncol = 1, nrow = 2) +
      geom_point(alpha = 0.6, color = "darkblue") +
      scale_size_continuous(range = c(1, 10)) + # Adjust size range as needed
      scale_x_continuous(breaks = sort(unique(data.tmp$lon))) + # Set x-axis ticks to unique values of 'lat'

      theme_minimal() +
      theme(strip.text = element_text(size = 16),# Increase the size of facet labels
            panel.grid.minor = element_blank() , # Remove minor grid lines
            plot.title = element_text(size = 25), # Set title size
            plot.margin = margin(t = 10, r = 10, b = 30, l = 30, unit = "pt"), # Adjust plot margins
            plot.caption = element_text(hjust = 0, face = "italic")) + # Customize caption appearance

      labs(
        # title = title.tmp,
           x = "Longitude",
           y = "Latitude",
           caption = paste0("Data source: ", dt.freq$freq.parms$label_data) , # Add the data source caption
           size = paste0("n.", str_sub(uid_label[1], 5), "s"))

    plt.lst[[i]] <- p1

  }

return(plt.lst)
}



#' plot median of ring width of the target site and its neighbours
#' @description
#' This function plots the time series and the geographical distribution of the median ring-width measurements to contrast the target site with its neighbouring sites.
#' @param dt.scale a table with class "cfs_scale", resulting from function CFS_scale()
#' @return A named list with two \code{ggplot} objects showing the contrast
#' between the target site and its neighboring sites:
#' \describe{
#'   \item{plot.year}{A time-series plot of median ring width by year.}
#'   \item{plot.ll}{A spatial plot showing the geographic location of the sites,
#'   with point size proportional to the magnitude of site-level ring-width measurements.}
#' }

#' @export plot_scale
#' @examples
#' # loading formatted
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
#' all.sites <- dt.samples_trt$tr_all_wide[,.N, by = c("species", "uid_site", "site_id")][, N:=NULL]

#' # e.g. taking the target sites
#' target_site <- all.sites[c(1,2), -"uid_site"]

#' ref.sites <- merge(
#' dt.samples_trt$tr_all_wide[,c("species", "uid_site", "site_id",
#'  "latitude","longitude", "uid_radius")],
#' dt.samples_trt$tr_all_long$tr_7_ring_widths, by = c("uid_radius"))

#' dt.scale <- CFS_scale( target_site = target_site, ref_sites = ref.sites,
#' scale.label_data_ref = "demo-samples", scale.max_dist_km = 200, scale.N_nbs = 2)

#' plots.lst <- plot_scale(dt.scale[[1]])

plot_scale <- function(dt.scale){
  check_optional_deps()
  if (!inherits(dt.scale, "cfs_scale")) stop("please check the input of dt.scale, make sure it's the result of CFS_scale() function")
  g1 <- ggplot(dt.scale$dt.plots[[1]], aes(x = year, y = rw.median, group = uid_site)) +

    geom_line(aes(color = factor(ifelse(ord == 0, "Red group", "Blue group"))),
              alpha = 0.6, linewidth = 1) +

    scale_color_manual(values = c("Red group" = "red", "Blue group" = "darkblue"),
                       labels = c("Red group" = "target", "Blue group" = "neigh."),
                       name = "") +
    theme_minimal() +
    theme(legend.position = "right",
          plot.caption = element_text(hjust = 0, face = "italic")) +  # Position the legend on the right

    labs(
      y = "rw.median (mm)"

    )


  dt <- dt.scale$dt.plots[["med.site"]]

  # Compute automatic contrast: small contrast if values are close
  rng <- range(dt$rw.median, na.rm = TRUE)
  spread <- diff(rng)

  # Define size range automatically
  # small spread → small size variation
  size_min <- 4
  size_max <- 4 + pmin(spread, 1) * 3
  # when spread is small (<1), range is small
  # when spread is large, range expands up to +3

  g2 <- ggplot(dt,
         aes(x = longitude, y = latitude, size = rw.median)) +
    geom_point(aes(color = ifelse(ord == 0, "red", "darkblue")),
               alpha = 1) +
    scale_color_identity() +
    scale_size_continuous(
      name = "rw median (mm)",
      range = c(size_min, size_max)
    )+
    guides(size = guide_legend(override.aes = list(color = "darkblue"))) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3), expand = expansion(mult = 0.3)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5), expand = expansion(mult = 0.2)) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 16),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 18),
      legend.text = element_text(color = "darkblue"),
      legend.title = element_text(color = "darkblue")
    ) +
    labs(
      x = "Longitude",
      y = "Latitude"
    )


  return(list(plot.year = g1, plot.ll = g2))

}




#' plot data summary and location
#' @description
#' This function plots the site location and frequency distribution of series length and ring width measurement per species. It's used for generating the data report
#'
#' @param data a list of 2 tables : 1st is meta data with 1 species only ; 2nd is the ring width measurement in long-format

#' @keywords internal
#' @noRd

plot_ds <- function(data){
  check_optional_deps()
  dt.tr <- data[[1]]
  dt.rw <- data[[2]]
  spc <- unique(dt.tr$species)

  # shp <- st_read(system.file("extdata", "Mapping", "province.shp", package = "growthTrendR"))
  # canada_lcc <- st_transform(shp, crs = 3347)
  p.rw_hist <- ggplot(dt.rw, aes(x = rw_mm)) +
    geom_histogram(binwidth = 1, color = "black", fill =  "lightgreen") +
    labs(title = "ring width", x = "ring width (mm)", y = "Frequency") +
    theme_classic()
  # `map_bounds` <- st_bbox(shp)
  p.age <- ggplot(dt.rw, aes(x = rw_yend - rw_ystart + 1)) +
    geom_bar(fill = "lightblue", color = "lightblue") +
    labs(title = paste0("series length"), x = "series length", y = "Frequency") +
    theme_minimal()


    tmp <- dt.tr[, .N, by = .(longitude, latitude)]
  pts <- tmp %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84") %>%
    sf::st_cast("POINT")

  # shp file in sysdata.rda
  p.loc <- ggplot() +
    geom_sf(data = shp, fill = NA, color = "lightgrey", alpha = 0) + # Polygons
    geom_sf(data = pts, color = "blue", size = 2) +          # Points
    labs(title = "site location ") +
    # annotate("text",
    #              x = map_bounds["xmin"] + 0.1 * (map_bounds["xmax"] - map_bounds["xmin"]), # Slightly inset from the left
    #   y = map_bounds["ymax"] + 0.2 * (map_bounds["ymax"] - map_bounds["ymin"]), # Slightly above the top
    #
    #          label = paste0("location"),
    #          hjust = 0, vjust = 1, color = "black", size = 7) +   # Credits
    coord_sf(expand = FALSE) +                                  # Add graticules with labels
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "grey", linetype = "dotted"), # Graticule styling
      panel.grid.minor = element_blank(),                                   # No minor lines
      axis.text = element_text(size = 7),                                 # Graticule label styling
      axis.ticks = element_line(size = 0.5),
      axis.title = element_blank()
    )

  # p.rw <- ggplot(dt.rw, aes(x = year, y = rw_mm)) +
  #   geom_point(color = "lightgrey") +
  #   labs(title = "ring width(mm) ", y = "ring width (mm)", x = "year") +
  #   theme_classic()

  # Compute equal-width breaks
  breaks_lat <- seq(min(dt.tr$latitude), max(dt.tr$latitude), length.out = 4)

  # Midpoints of the intervals
  mids_lat <- utils::head(breaks_lat, -1) + diff(breaks_lat)/2

  # Assign each value the midpoint of its class
  dt.tr[, lat := mids_lat[cut(latitude, breaks = breaks_lat, include.lowest = TRUE, labels = FALSE)]]


  # Compute equal-width breaks
  breaks_lon <- seq(min(dt.tr$longitude), max(dt.tr$longitude), length.out = 4)

  # Midpoints of the intervals
  mids_lon <- utils::head(breaks_lon, -1) + diff(breaks_lon)/2

  # Assign each value the midpoint of its class
  dt.tr[, lon := mids_lon[cut(longitude, breaks = breaks_lon, include.lowest = TRUE, labels = FALSE)]]


  dist.uids <- dt.tr[, .(nuids = .N), by = .(lat, lon)]
  # dist.uids[, ord:= 1]
  # setorder(dist.uids, ord, lon, lat)
  setorder(dist.uids, lon, lat)
  p.freq <- ggplot(dist.uids, aes(x = lon, y = lat, size = nuids))  +
    geom_point(alpha = 0.6, color = "darkblue") +

    scale_x_continuous(expand = expansion(mult = 0.1)) +
    scale_y_continuous(expand = expansion(mult = 0.1)) +


    theme_minimal() +
  # +
    # theme(strip.text = element_text(size = 16),# Increase the size of facet labels
          # panel.grid.minor = element_blank() , # Remove minor grid lines
          # plot.title = element_text(size = 25), # Set title size
          # plot.margin = margin(t = 10, r = 10, b = 30, l = 30, unit = "pt"), # Adjust plot margins
          # plot.caption = element_text(hjust = 0, face = "italic")) + # Customize caption appearance

    labs(
      title = "Count by Location",
      x = "Longitude",
      y = "Latitude",
      size = paste0("N.series"))

  p.ispc<-  (p.loc | p.freq)/ (p.age | p.rw_hist) +
    plot_annotation(
      title = paste0("Species: ", spc) ,
      tag_levels = 'a',
      tag_suffix = ")") &
    theme(
      plot.title = element_text(face = "bold"),
      plot.tag = element_text(face = "bold")


      # plot.margin = margin(t = 10, r = 10, b = 30, unit = "pt") # Adjust plot margins
    )
  return(p.ispc)
}


#' plot multiple series by wrap_facet, and put them in a list
#' @description
#' This function plots dynamic xyplot per category using wrap_facet. each facet contains nrow*ncol plots. It's used for generating the data report
#'
#' @param data a data table including all the necessary columns in varcols
#' @param varcols a list containing 3 column names for x, y and facet category.
#' @param xylabels a list containing the labels for x-axis and y-axis
#' @param nrow number of plots per row
#' @param ncol number of plots per col

#' @keywords internal
#' @noRd
plot_facet <- function(data, varcols, xylabels, nrow, ncol) {
  check_optional_deps()
  # Calculate the total number of pages
  # print(paste0(nrow, " ", ncol))
  total_pages <- ceiling(length(unique(data[[varcols[[3]]]])) / (nrow*ncol))
  # print(total_pages)

  # Generate plots for each page
  plot_list <- lapply(1:total_pages, function(page) {


    ggplot(data, aes(x = .data[[varcols[[1]]]], y = .data[[varcols[[2]]]])) +
      geom_point(color = "lightgrey") +
      ggforce::facet_wrap_paginate(
        ~.data[[varcols[[3]]]],
        ncol = ncol,
        nrow = nrow,
        page = page
      ) +
      scale_x_continuous(labels = scales::number_format(scale = 1)) +  # Ensure continuous year labels
      labs(x = xylabels[[1]], y = xylabels[[2]]) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  # 2 per page
  if ((floor(total_pages/2))*2 != total_pages) plot_list[[total_pages+1]] <- ggplot() +
    theme(
      panel.background = element_blank(),  # No background
      plot.background = element_blank(),   # No plot area background
      axis.ticks = element_blank(),        # No axis ticks
      axis.text = element_blank(),         # No axis text
      axis.title = element_blank()         # No axis titles
    )

  return(plot_list)
}






#' Generate Automated Reports
#'
#' Creates HTML reports from various analysis objects using predefined
#' R Markdown templates. The function automatically selects the appropriate template
#' based on the input object's class and renders a comprehensive report with
#' visualizations and analysis results.
#'
#' @param robj An R object containing analysis results from functions in this package.
#'   The object's class determines which report template is used. Supported
#'   classes depend on available templates in the package.
#' @param data_report.reports_sel Numeric vector.
#'   Specifies which sections of the data report to include in the output:
#'   1 = project level; 2 = species level; 3 = site level; 4 = radius level.
#'   The default is `c(1, 2, 3, 4)`, which includes all sections.
#' @param output_file Character string. Optional path and filename for the output
#'   HTML file. If NULL (default), the report is generated with an automatic
#'   filename and opened in RStudio viewer.
#' @param ... Additional parameters passed to the R Markdown template. Available
#'   parameters vary by template type and are filtered to only include those
#'   recognized by the selected template.
#'
#' @return Invisibly returns the file path of the generated report.
#' The function is primarily called for its side effect of generating the report file.
#'
#' @export
#' @examples
#'
#' # loading processed data
#' \donttest{

#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
#' # genereate data summary report
#' outfile_data <- tempfile(fileext = ".html")
#' generate_report(robj = dt.samples_trt, output_file = outfile_data)
#'}
generate_report <- function(robj, data_report.reports_sel = c(1,2,3,4), output_file = NULL, ...) {

  check_optional_deps()
  robj_class <- class(robj)[1]
  tpl_info <- get_template_and_params(robj_class)

  if (tpl_info$template == "unknown") {
    stop("No report template defined for class ", robj_class)
  }

  # Path to the Rmd template
  # rmd_file <- system.file("reports", paste0(tpl_info$template, ".Rmd"), package = "myPackage")

  # Collect all parameters passed by user
  user_params <- list(...)

  # Base params always include the object itself
  base_params <- list(robj = robj, data_report.reports_sel = data_report.reports_sel)

  # Merge user params into base
  all_params <- utils::modifyList(base_params, user_params)

  # Filter params only to those allowed by the template
  final_params <- all_params[names(all_params) %in% tpl_info$params]

  # print(type.templt)
  # if (type.templt == "unknown") stop("The input is not supported by this procedure, please check...") else
  # Path to the R Markdown template
   # rmd_file <- file.path("C:/Users/xjguo/Documents/Rpackages/growthTrendR/inst/rmd", paste0("template_", tpl_info$template, ".Rmd"))
  rmd_file <- system.file("rmd", paste0("template_", tpl_info$template, ".Rmd"), package = "growthTrendR" )


  # Check if the template exists
  if (rmd_file == "") {
    stop(paste0("Template not found! Ensure ", paste0("template_", tpl_info$template, ".Rmd"),  "is in inst/rmd/ directory."))
  }


  result <- rmarkdown::render(
    input = rmd_file,
    output_file = output_file,
    params = final_params,
    envir = new.env(parent = globalenv())
  )

  if (is.null(output_file)) {
    rstudioapi::viewer(result)
  }

  return(invisible(result))
}


#' @keywords internal
#' @noRd
#'
get_template_and_params <- function(robj_class) {
  class_to_template <- c(
    cfs_model = "model_report",
    cfs_format = "data_report",
    cfs_scale_list = "scale",
    cfs_freq = "freq",
    cfs_qa = "qa",
    cfs_map = "map"
  )

  template_params <- list(
    model_report = c("robj", "param1", "param2"),
    data_report  = c("robj","data_report.reports_sel", "qa.min_nseries", "qa.label_data", "qa.label_trt",
                     "scale.max_dist_km", "scale.N_nbs"),
    scale        = c("robj"),
    freq         = c("robj", "freq.out_species"),
    qa           = c("robj", "qa.out_series"),
    gif          = c("robj", "animation_fps", "data.crs", "png.text", "rmd_output")
  )

  template_name <- class_to_template[[robj_class]]
  if (is.null(template_name)) template_name <- "unknown"

  allowed_params <- template_params[[template_name]]

  list(template = template_name, params = allowed_params)
}

#' Plot smooth terms prediction with confidence intervals
#'
#' This function generates ggplot objects for each smooth term in a GAM model.
#' Predictions vary one smooth term at a time, keeping all other terms fixed at reference values.
#' Confidence intervals are computed using \code{ci_resp()}, supporting multiple methods:
#' "delta_link", "delta_resp", "bootstrap_link", "bootstrap_resp", and "posterior".
#' Small samples automatically trigger the "posterior" method for more robust CIs.
#'
#' @param robj R object from the modelling functions.
#' @param ... Additional arguments passed to \code{ci_resp()}.
#'
#' @return A list of ggplot objects, one per smooth term.
#'
#' @examples
#'
#' # loading processed data
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
#' # climate
#' dt.clim <- data.table::fread(system.file("extdata", "dt.clim.csv", package = "growthTrendR"))
#' # pre-data for model
#' dt.samples_clim <- prepare_samples_clim(dt.samples_trt, dt.clim)

#' dt.m <- dt.samples_clim[ageC >1]

#' # gamm_site model
#' m.spatial <-gamm_spatial(
#'   data = dt.m, resp_scale = "resp_log",
#'   m.candidates = c( "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + s(FFD)"))


#' plots.lst <- plot_resp(m.spatial)
#'
#'

#' @export
plot_resp <- function(robj, ...) {
  check_optional_deps()
  if (is.list(robj$model) && ("gamm" %in% class(robj$model))) model <- robj$model$gam else model <- robj$model

  model_data <- model$model  # original data

  # remove matrix
  model_data <- as.data.frame(model_data)[ , !sapply(model_data, is.matrix), drop = FALSE]
  model_data <- as.data.frame(model_data)[ , !sapply(model_data, is.list), drop = FALSE]

  resp_scale <- robj$model$resp_scale
  if (length(resp_scale) == 0 )stop("cannot identify response variable scale")
  is_log_model <- resp_scale %in% c("resp_gamma", "resp_log")

  if (length(is_log_model) == 0 )stop("cannot identify response variable scale")
  # pass is_log_model to gam object for ci_resp
# model$is_log_model <- is_log_model
# model$resp_scale <- resp_scale
  # Extract smooth terms
  dt.vars <- data.table::rbindlist(lapply(model$smooth, function(sm) {
    data.table(
      form     = sm$label,
      variable = sm$term[1],
      byterm   = if (sm$by == "NA") NA_character_ else sm$by
    )
  }))
  dt.vars <- dt.vars[, .N, by = .(variable, byterm)][, N := NULL]
  dt.vars[, term := ifelse(is.na(byterm), variable, paste0(variable, "_by_", byterm))]
  setDT(model_data)
  # Response variable
  response_var <- all.vars(formula(model))[1]
  if (resp_scale == "resp_log") {
    response_var <- gsub("^log\\(|\\)$", "", response_var)

    model_data$y.resp <- exp(model_data[[names(model_data)[1]]])} else{

      model_data$y.resp <- model_data[[names(model_data)[1]]]
    }
  all.vars(formula(model))[1]
  # removes the random effects for graphing
  dt.vars <- dt.vars[variable != "uid_site.fac"]
  # names(model$model)
  # Generate plots for each smooth term
  p_list <- lapply(seq_len(nrow(dt.vars)), function(i.var) {

    var <- dt.vars[i.var, ]$variable
    byterm <- dt.vars[i.var, ]$byterm
    term_name <- dt.vars[i.var, ]$term

    if (!var %in% names(model_data)) return(NULL)
    if (!is.na(byterm) && !byterm %in% names(model_data)) return(NULL)

    terms_to_predict <- if (!is.na(byterm)) c(var, byterm) else var

    # Generate newdata grid using ggpredict (others fixed)



      nd <- ggeffects::data_grid(model, terms_to_predict)

    setDT(nd)
    nd[,x:=get(var)]
    if (!is.na(byterm)){
      ranges <- model_data[, .(minv = min(get(var), na.rm = TRUE),
                               maxv = max(get(var), na.rm = TRUE)),
                           by = byterm]
      nd <- merge(nd, ranges, by = byterm)

      nd[, group:=get(byterm)]
      nd <-nd[x >= minv & x <= maxv]
    }


    # nd$is_log_model <- is_log_model


    # Compute CI using ci_resp()
    dt.pred <- ci_resp(model, newdata = nd)

    # Combine predictions and CI
    # dt.pred <- cbind(nd, ci_dt)

    # Plot
    p <- ggplot()
    if (!is.null(byterm) & !is.na(byterm) ) {
      p <- p +
        geom_line(data = dt.pred, aes(x = x, y = fit, color = group), linewidth = 1) +
        geom_ribbon(data = dt.pred, aes(x = x, ymin = lwr, ymax = upr, fill = group),
                    alpha = 0.1, show.legend = FALSE) +
        geom_point(data = model_data, aes(x = !!sym(var),
                                          y = y.resp,
                                          color = !!sym(byterm)), alpha = 0.6, size = 1)


      p <- p +
        labs(x = var, y = response_var, title = term_name, subtitle = paste("CI method:", unique(dt.pred$ci_method)),
             color = byterm) +
        theme_minimal()

    } else {
      p <- p +
        geom_line(data = dt.pred, aes(x = x, y = fit), color = "steelblue", linewidth = 1) +
        geom_ribbon(data = dt.pred, aes(x = x, ymin = lwr, ymax = upr), fill = "steelblue", alpha = 0.1) +
        geom_point(data = model_data, aes(x = !!sym(var),
                                          y = y.resp),
                   alpha = 0.6, size = 1, color = "gray30")

      p <- p +
        labs(x = var, y = response_var, title = term_name, subtitle = paste("CI method:", unique(dt.pred$ci_method))) +
        theme_minimal()

    }


    # ci_subtitle <- paste("CI method:", unique(dt.pred$ci_method))


    return(p)
  })

  return(p_list)
}

