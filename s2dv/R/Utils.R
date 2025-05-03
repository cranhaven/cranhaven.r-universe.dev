#'@importFrom abind abind
#'@import plyr ncdf4
#'@importFrom grDevices png jpeg pdf svg bmp tiff
#'@importFrom easyVerification convert2prob

## Function to tell if a regexpr() match is a complete match to a specified name
.IsFullMatch <- function(x, name) {
  x > 0 && attributes(x)$match.length == nchar(name)
}

.ConfigReplaceVariablesInString <- function(string, replace_values, 
                                            allow_undefined_key_vars = FALSE) {
  # This function replaces all the occurrences of a variable in a string by 
  # their corresponding string stored in the replace_values.
  if (length(strsplit(string, "\\$")[[1]]) > 1) {
    parts <- strsplit(string, "\\$")[[1]]
    output <- ""
    i <- 0
    for (part in parts) {
      if (i %% 2 == 0) {
        output <- paste0(output, part)
      } else {
        if (part %in% names(replace_values)) {
          output <- paste0(output,
                           .ConfigReplaceVariablesInString(replace_values[[part]], 
                                                           replace_values, 
                                                           allow_undefined_key_vars))
        } else if (allow_undefined_key_vars) {
          output <- paste0(output, "$", part, "$")
        } else {
          stop('Error: The variable $', part, 
               '$ was not defined in the configuration file.', sep = '')
        }
      }
      i <- i + 1
    }
    output
  } else {
    string
  }
}

.KnownLonNames <- function() {
  known_lon_names <- c('lon', 'longitude', 'x', 'i', 'nav_lon')
  return(known_lon_names)
}

.KnownLatNames <- function() {
  known_lat_names <- c('lat', 'latitude', 'y', 'j', 'nav_lat')
  return(known_lat_names)
}

.t2nlatlon <- function(t) {
  ## As seen in cdo's griddes.c: ntr2nlat()
  nlats <- (t * 3 + 1) / 2
  if ((nlats > 0) && (nlats - trunc(nlats) >= 0.5)) {
    nlats <- ceiling(nlats)
  } else {
    nlats <- round(nlats)
  }
  if (nlats %% 2 > 0) {
    nlats <- nlats + 1
  }
  ## As seen in cdo's griddes.c: compNlon(), and as specified in ECMWF
  nlons <- 2 * nlats
  keep_going <- TRUE
  while (keep_going) {
    n <- nlons
    if (n %% 8 == 0) n <- trunc(n / 8)
    while (n %% 6 == 0) n <- trunc(n / 6)
    while (n %% 5 == 0) n <- trunc(n / 5)
    while (n %% 4 == 0) n <- trunc(n / 4)
    while (n %% 3 == 0) n <- trunc(n / 3)
    if (n %% 2 == 0) n <- trunc(n / 2)
    if (n <= 8) {
      keep_going <- FALSE
    } else {
      nlons <- nlons + 2
      if (nlons > 9999) {
        stop("Error: pick another gaussian grid truncation. ",
             "It doesn't fulfill the standards to apply FFT.")
      }
    }
  }
  c(nlats, nlons)
}

.nlat2t <- function(nlats) {
  trunc((nlats * 2 - 1) / 3)
}

.LoadDataFile <- function(work_piece, explore_dims = FALSE, silent = FALSE) {
  # The purpose, working modes, inputs and outputs of this function are
  # explained in ?LoadDataFile
  #suppressPackageStartupMessages({library(ncdf4)})
  #suppressPackageStartupMessages({library(bigmemory)})
  #suppressPackageStartupMessages({library(plyr)})
  # Auxiliar function to convert array indices to lineal indices
  arrayIndex2VectorIndex <- function(indices, dims) {
    if (length(indices) > length(dims)) {
      stop("Error: indices do not match dimensions in arrayIndex2VectorIndex.")
    }
    position <- 1
    dims <- rev(dims)
    indices <- rev(indices)
    for (i in seq_along(indices)) {
      position <- position + (indices[i] - 1) * prod(dims[-(1:i)])
    }
    position
  }
  
  found_file <- NULL
  dims <- NULL
  grid_name <- units <- var_long_name <- NULL
  is_2d_var <- array_across_gw <- NULL
  data_across_gw <- NULL

  filename <- work_piece[['filename']]
  namevar <- work_piece[['namevar']]
  output <- work_piece[['output']]
  # The names of all data files in the directory of the repository that match 
  # the pattern are obtained.
  if (any(grep("^http", filename))) {
    is_url <- TRUE
    files <- filename
    ## TODO: Check that the user is not using shell globbing exps.
  } else {
    is_url <- FALSE
    files <- Sys.glob(filename)
  }

  # If we don't find any, we leave the flag 'found_file' with a NULL value.
  if (length(files) > 0) {
    # The first file that matches the pattern is chosen and read.
    filename <- head(files, 1)
    filein <- filename
    found_file <- filename
    mask <- work_piece[['mask']]

    if (!silent && explore_dims) {
      .message(paste("Exploring dimensions...", filename))
    ##} else {
    ##  cat(paste("* Reading & processing data...", filename, '\n'))
    ##}
    }

    # We will fill in 'expected_dims' with the names of the expected dimensions of
    # the data array we'll retrieve from the file.
    expected_dims <- NULL
    remap_needed <- FALSE
    # But first we open the file and work out whether the requested variable is 2d
    fnc <- nc_open(filein)
    if (!(namevar %in% names(fnc$var))) {
      stop("Error: The variable", namevar, "is not defined in the file", filename)
    }
    var_long_name <- fnc$var[[namevar]]$longname
    units <- fnc$var[[namevar]]$units
    file_dimnames <- unlist(lapply(fnc$var[[namevar]][['dim']], '[[', 'name'))
    # The following two 'ifs' are to allow for 'lon'/'lat' by default, instead of 
    # 'longitude'/'latitude'.
    if (!(work_piece[['dimnames']][['lon']] %in% file_dimnames) &&
        (work_piece[['dimnames']][['lon']] == 'longitude') &&
        ('lon' %in% file_dimnames)) {
      work_piece[['dimnames']][['lon']] <- 'lon'
    }
    if (!(work_piece[['dimnames']][['lat']] %in% file_dimnames) &&
        (work_piece[['dimnames']][['lat']] == 'latitude') &&
        ('lat' %in% file_dimnames)) {
      work_piece[['dimnames']][['lat']] <- 'lat'
    }
    if (is.null(work_piece[['is_2d_var']])) {
      is_2d_var <- all(c(work_piece[['dimnames']][['lon']], 
                         work_piece[['dimnames']][['lat']]) %in%
                       unlist(lapply(fnc$var[[namevar]][['dim']], 
                                     '[[', 'name')))
    } else {
      is_2d_var <- work_piece[['is_2d_var']]
    }
    if ((is_2d_var || work_piece[['is_file_per_dataset']])) {
      if (Sys.which("cdo")[[1]] == "") {
        stop("Error: CDO libraries not available")
      }

     cdo_version <- 
       strsplit(suppressWarnings(
        system2("cdo", args = '-V', stderr = TRUE))[[1]], ' ')[[1]][5]

      cdo_version <- 
        as.numeric_version(unlist(strsplit(cdo_version, "[A-Za-z]", fixed = FALSE))[[1]])

    }
    # If the variable to load is 2-d, we need to determine whether:
    #  - interpolation is needed
    #  - subsetting is requested
    if (is_2d_var) {
      ## We read the longitudes and latitudes from the file.
      lon <- ncvar_get(fnc, work_piece[['dimnames']][['lon']])
      lat <- ncvar_get(fnc, work_piece[['dimnames']][['lat']])
      first_lon_in_original_file <- lon[1]
      # If a common grid is requested or we are exploring the file dimensions
      # we need to read the grid type and size of the file to finally work out the 
      # CDO grid name.
      if (!is.null(work_piece[['grid']]) || explore_dims) {
        # Here we read the grid type and its number of longitudes and latitudes
        file_info <- system(paste('cdo -s griddes', filein, '2> /dev/null'), intern = TRUE)
        grids_positions <- grep('# gridID', file_info)
        if (length(grids_positions) < 1) {
          stop("The grid should be defined in the files.")
        }
        grids_first_lines <- grids_positions + 2
        grids_last_lines <- c((grids_positions - 2)[-1], length(file_info))
        grids_info <- as.list(seq_along(grids_positions))
        grids_info <- lapply(grids_info, 
                             function (x) file_info[grids_first_lines[x]:grids_last_lines[x]])
        grids_info <- lapply(grids_info, function (x) gsub("  *", " ", x))
        grids_info <- lapply(grids_info, function (x) gsub("^ | $", "", x))
        grids_info <- lapply(grids_info, function (x) unlist(strsplit(x, " | = ")))
        grids_types <- unlist(lapply(grids_info, function (x) x[grep('gridtype', x) + 1]))
        grids_matches <- unlist(lapply(grids_info, function (x) {
          nlons <- if (any(grep('xsize', x))) {
                     as.numeric(x[grep('xsize', x) + 1])
                   } else {
                     NA
                   }
          nlats <- if (any(grep('ysize', x))) {
                    as.numeric(x[grep('ysize', x) + 1])
                  } else {
                    NA
                  }
          result <- FALSE
          if (!anyNA(c(nlons, nlats))) {
            if ((nlons == length(lon)) && 
                (nlats == length(lat))) {
              result <- TRUE
            }
          }
          result
        }))
        grids_matches <- grids_matches[which(grids_types %in% c('gaussian', 'lonlat'))]
        grids_info <- grids_info[which(grids_types %in% c('gaussian', 'lonlat'))]
        grids_types <- grids_types[which(grids_types %in% c('gaussian', 'lonlat'))]
        if (length(grids_matches) == 0) {
          stop("Error: Only 'gaussian' and 'lonlat' grids supported. See e.g: cdo sinfo ", filename)
        }
        if (sum(grids_matches) > 1) {
          if ((all(grids_types[which(grids_matches)] == 'gaussian') || 
               all(grids_types[which(grids_matches)] == 'lonlat')) && 
               all(unlist(lapply(grids_info[which(grids_matches)], identical, 
                                 grids_info[which(grids_matches)][[1]])))) {
            grid_type <- grids_types[which(grids_matches)][1]
          } else {
            stop("Error: Load() can't disambiguate: ",
                 "More than one lonlat/gaussian grids with the same size as ",
                 "the requested variable defined in ", filename)
          }
        } else if (sum(grids_matches) == 1) {
          grid_type <- grids_types[which(grids_matches)]
        } else {
          stop("Unexpected error.")
        }
        grid_lons <- length(lon)
        grid_lats <- length(lat)
        # Convert to CDO grid name as seen in cdo's griddes.c: nlat2ntr()
        if (grid_type == 'lonlat') {
          grid_name <- paste0('r', grid_lons, 'x', grid_lats)
        } else {
          grid_name <- paste0('t', .nlat2t(grid_lats), 'grid')
        }
        if (is.null(work_piece[['grid']])) {
          .warning(paste0("Detect the grid type to be '", grid_name, "'. ",
                          "If it is not expected, assign parameter 'grid' to avoid wrong result."))
        }
      }
      # If a common grid is requested, we will also calculate its size which we will use
      # later on.
      if (!is.null(work_piece[['grid']])) {
        # Now we calculate the common grid type and its lons and lats
        if (any(grep('^t\\d{1,+}grid$', work_piece[['grid']]))) {
          common_grid_type <- 'gaussian'
          common_grid_res <- as.numeric(strsplit(work_piece[['grid']], '[^0-9]{1,+}')[[1]][2])
          nlonlat <- .t2nlatlon(common_grid_res)
          common_grid_lats <- nlonlat[1]
          common_grid_lons <- nlonlat[2]
        } else if (any(grep('^r\\d{1,+}x\\d{1,+}$', work_piece[['grid']]))) {
          common_grid_type <- 'lonlat'
          common_grid_lons <- as.numeric(strsplit(work_piece[['grid']], '[^0-9]{1,+}')[[1]][2])
          common_grid_lats <- as.numeric(strsplit(work_piece[['grid']], '[^0-9]{1,+}')[[1]][3])
        } else {
          stop("Error: Only supported grid types in parameter 'grid' are t<RES>grid and r<NX>x<NY>")
        }
      } else {
        ## If no 'grid' is specified, there is no common grid.
        ## But these variables are filled in for consistency in the code.
        common_grid_lons <- length(lon)
        common_grid_lats <- length(lat)
      }
      first_common_grid_lon <- 0
      last_common_grid_lon <- 360 - 360 / common_grid_lons
      ## This is not true for gaussian grids or for some regular grids, but 
      ## is a safe estimation
      first_common_grid_lat <- -90
      last_common_grid_lat <- 90
      # And finally determine whether interpolation is needed or not
      remove_shift <- FALSE
      if (!is.null(work_piece[['grid']])) {
        if ((grid_lons != common_grid_lons) || 
            (grid_lats != common_grid_lats) || 
            (grid_type != common_grid_type) ||
            (lon[1] != first_common_grid_lon)) { 
          if (grid_lons == common_grid_lons && grid_lats == common_grid_lats &&
              grid_type == common_grid_type && lon[1] != first_common_grid_lon) {
            remove_shift <- TRUE
          }
          remap_needed <- TRUE
          common_grid_name <- work_piece[['grid']]
        }
      } else if ((lon[1] != first_common_grid_lon) && explore_dims && 
                 !work_piece[['single_dataset']]) {
        remap_needed <- TRUE
        common_grid_name <- grid_name
        remove_shift <- TRUE
      }
      if (remap_needed && (work_piece[['remap']] == 'con') && 
          (cdo_version >= as.numeric_version('1.7.0'))) {
        work_piece[['remap']] <- 'ycon'
      }
      if (remove_shift && !explore_dims) {
        if (!is.null(work_piece[['progress_amount']])) {
          cat("\n")
        }
        .warning(paste0("The dataset with index ", 
            tail(work_piece[['indices']], 1), " in '", 
            work_piece[['dataset_type']], 
            "' doesn't start at longitude 0 and will be re-interpolated in order ",
            "to align its longitudes with the standard CDO grids definable with ",
            "the names 't<RES>grid' or 'r<NX>x<NY>', which are by definition ",
            "starting at the longitude 0.\n"))
        if (!is.null(mask)) {
          .warning(paste0("A mask was provided for the dataset with index ",    
              tail(work_piece[['indices']], 1), " in '",
              work_piece[['dataset_type']], 
              "'. This dataset has been re-interpolated to align its longitudes to ",
              "start at 0. You must re-interpolate the corresponding mask to align ",
              "its longitudes to start at 0 as well, if you haven't done so yet. ",
              "Running cdo remapcon,", common_grid_name, 
              " original_mask_file.nc new_mask_file.nc will fix it.\n"))
        }
      }
      if (remap_needed && (grid_lons < common_grid_lons || grid_lats < common_grid_lats)) {
        if (!is.null(work_piece[['progress_amount']])) {
          cat("\n")
        }
        if (!explore_dims) {
          .warning(paste0("The dataset with index ", tail(work_piece[['indices']], 1), 
                     " in '", work_piece[['dataset_type']], "' is originally on ",
                     "a grid coarser than the common grid and it has been ",
                     "extrapolated. Check the results carefully. It is ",
                     "recommended to specify as common grid the coarsest grid ",
                     "among all requested datasets via the parameter 'grid'.\n"))
        }
      }
      # Now calculate if the user requests for a lonlat subset or for the 
      # entire field
      lonmin <- work_piece[['lon_limits']][1]
      lonmax <- work_piece[['lon_limits']][2]
      latmin <- work_piece[['lat_limits']][1]
      latmax <- work_piece[['lat_limits']][2]
      lon_subsetting_requested <- FALSE
      lonlat_subsetting_requested <- FALSE
      if (lonmin <= lonmax) {
        if ((lonmin > first_common_grid_lon) || (lonmax < last_common_grid_lon)) {
          lon_subsetting_requested <- TRUE
        }
      } else {
        if ((lonmin - lonmax) > 360 / common_grid_lons) {
          lon_subsetting_requested <- TRUE
        } else {
          gap_width <- floor(lonmin / (360 / common_grid_lons)) - 
                       floor(lonmax / (360 / common_grid_lons))
          if (gap_width > 0) { 
            if (!(gap_width == 1 && (lonmin %% (360 / common_grid_lons) == 0) && 
                  (lonmax %% (360 / common_grid_lons) == 0))) {
              lon_subsetting_requested <- TRUE
            }
          }
        }
      }
      if ((latmin > first_common_grid_lat) || (latmax < last_common_grid_lat)
          || (lon_subsetting_requested)) {
        lonlat_subsetting_requested <- TRUE
      }
      # Now that we know if subsetting was requested, we can say if final data
      # will go across greenwich
      if (lonmax < lonmin) {
        data_across_gw <- TRUE
      } else {
        data_across_gw <- !lon_subsetting_requested
      }

      # When remap is needed but no subsetting, the file is copied locally
      # so that cdo works faster, and then interpolated.
      # Otherwise the file is kept as is and the subset will have to be 
      # interpolated still.
      if (!lonlat_subsetting_requested && remap_needed) {
        nc_close(fnc)
        filecopy <- tempfile(pattern = "load", fileext = ".nc")
        file.copy(filein, filecopy)
        filein <- tempfile(pattern = "loadRegridded", fileext = ".nc")
        # "-L" is to serialize I/O accesses. It prevents potential segmentation fault in the 
        # underlying hdf5 library.
        system(paste0("cdo -L -s remap", work_piece[['remap']], ",", 
                      common_grid_name, 
                      " -selname,", namevar, " ", filecopy, " ", filein, 
                      " 2>/dev/null"))
        file.remove(filecopy)
        work_piece[['dimnames']][['lon']] <- 'lon'
        work_piece[['dimnames']][['lat']] <- 'lat'
        fnc <- nc_open(filein)
        lon <- ncvar_get(fnc, work_piece[['dimnames']][['lon']])
        lat <- ncvar_get(fnc, work_piece[['dimnames']][['lat']])
      }

      # Read and check also the mask
      if (!is.null(mask)) {
        ###mask_file <- tempfile(pattern = 'loadMask', fileext = '.nc')
        if (is.list(mask)) {
          if (!file.exists(mask[['path']])) {
            stop("Error: Couldn't find the mask file", mask[['path']])
          }
          mask_file <- mask[['path']]
          ###file.copy(work_piece[['mask']][['path']], mask_file)
          fnc_mask <- nc_open(mask_file)
          vars_in_mask <- sapply(fnc_mask$var, '[[', 'name')
          if ('nc_var_name' %in% names(mask)) {
            if (!(mask[['nc_var_name']] %in% 
                  vars_in_mask)) {
              stop("Error: couldn't find variable", mask[['nc_var_name']], 
                   "in the mask file", mask[['path']])
            }
          } else {
            if (length(vars_in_mask) != 1) {
              stop("Error: one and only one non-coordinate variable should be ",
                   "defined in the mask file", 
                   mask[['path']], 
                   "if the component 'nc_var_name' is not specified. ",
                   "Currently found: ", 
                   toString(vars_in_mask), ".")
            } else {
              mask[['nc_var_name']] <- vars_in_mask
            }
          }
          if (sum(fnc_mask$var[[mask[['nc_var_name']]]]$size > 1) != 2) {
            stop("Error: the variable '", 
                 mask[['nc_var_name']], 
                 "' must be defined only over the dimensions '", 
                 work_piece[['dimnames']][['lon']], "' and '", 
                 work_piece[['dimnames']][['lat']], 
                 "' in the mask file ", 
                 mask[['path']])
          }
          mask <- ncvar_get(fnc_mask, mask[['nc_var_name']], collapse_degen = TRUE)
          nc_close(fnc_mask)
        ###  mask_lon <- ncvar_get(fnc_mask, work_piece[['dimnames']][['lon']])
        ###  mask_lat <- ncvar_get(fnc_mask, work_piece[['dimnames']][['lat']])
        ###} else {
        ###  dim_longitudes <- ncdim_def(work_piece[['dimnames']][['lon']], "degrees_east", lon)
        ###  dim_latitudes <- ncdim_def(work_piece[['dimnames']][['lat']], "degrees_north", lat)
        ###  ncdf_var <- ncvar_def('LSM', "", list(dim_longitudes, dim_latitudes), NA, 'double')
        ###  fnc_mask <- nc_create(mask_file, list(ncdf_var))
        ###  ncvar_put(fnc_mask, ncdf_var, work_piece[['mask']])
        ###  nc_close(fnc_mask)
        ###  fnc_mask <- nc_open(mask_file)
        ###  work_piece[['mask']] <- list(path = mask_file, nc_var_name = 'LSM')
        ###  mask_lon <- lon
        ###  mask_lat <- lat
        ###}
      ###}
        ### Now ready to check that the mask is right
        ##if (!(lonlat_subsetting_requested && remap_needed)) {
        ###  if ((dim(mask)[2] != length(lon)) || (dim(mask)[1] != length(lat))) {
        ###    stop(paste("Error: the mask of the dataset with index ", 
        ###         tail(work_piece[['indices']], 1), " in '", 
        ###         work_piece[['dataset_type']], "' is wrong. ", 
        ###         "It must be on the common grid if the selected output type is 'lonlat', ",
        ###         "'lon' or 'lat', or 'areave' and 'grid' has been specified. It must be on ",
        ###         "the grid of the corresponding dataset if the selected output type is ",
        ###         "'areave' and no 'grid' has been specified. For more information ",
        ###         "check ?Load and see help on parameters 'grid', 'maskmod' and ",
        ###         "'maskobs'.", sep = ""))
        ###  }
        ###if (!(identical(mask_lon, lon) && identical(mask_lat, lat))) {
        ###  stop(paste0("Error: the longitudes and latitudes in the masks must be ",
        ###       "identical to the ones in the corresponding data files if output = 'areave' ",
        ###       " or, if the selected output is 'lon', 'lat' or 'lonlat', the longitudes in ",
        ###       "the mask file must start by 0 and the latitudes must be ordered from ",
        ###       "highest to lowest. See\n  ", 
        ###     work_piece[['mask']][['path']], " and ", filein))
        ###}
        }
      }

      lon_indices <- seq_along(lon)
      if (!(lonlat_subsetting_requested && remap_needed)) {
        lon[which(lon < 0)] <- lon[which(lon < 0)] + 360
      }
      if (lonmax >= lonmin) {
        lon_indices <- lon_indices[which(((lon %% 360) >= lonmin) & ((lon %% 360) <= lonmax))]
      } else if (!remap_needed) {
        lon_indices <- lon_indices[which(((lon %% 360) <= lonmax) | ((lon %% 360) >= lonmin))]
      }
      lat_indices <- which(lat >= latmin & lat <= latmax)
      ## In most of the cases the latitudes are ordered from -90 to 90. 
      ## We will reorder them to be in the order from 90 to -90, so mostly 
      ## always the latitudes are reordered.
      ## TODO: This could be avoided in future.
      if (lat[1] < lat[length(lat)]) {
        lat_indices <- lat_indices[rev(seq_along(lat_indices))]
      }
      if (!is.null(mask) && !(lonlat_subsetting_requested && remap_needed)) {
        if ((dim(mask)[1] != length(lon)) || (dim(mask)[2] != length(lat))) {
          stop("Error: the mask of the dataset with index ", tail(work_piece[['indices']], 1), 
               " in '", work_piece[['dataset_type']], "' is wrong. It must be on the ",
               "common grid if the selected output type is 'lonlat', 'lon' or 'lat', ",
               "or 'areave' and 'grid' has been specified. It must be on the grid of ",
               "the corresponding dataset if the selected output type is 'areave' and ",
               "no 'grid' has been specified. For more information check ?Load and see ",
               "help on parameters 'grid', 'maskmod' and 'maskobs'.")
        }
        mask <- mask[lon_indices, lat_indices]
      }
      ## If the user requests subsetting, we must extend the lon and lat limits if possible
      ## so that the interpolation after is done properly
      maximum_extra_points <- work_piece[['remapcells']]
      if (lonlat_subsetting_requested && remap_needed) {
        if ((maximum_extra_points > (head(lon_indices, 1) - 1)) ||
            (maximum_extra_points > (length(lon) - tail(lon_indices, 1)))) {
          ## if the requested number of points goes beyond the left or right
          ## sides of the map, we need to take the entire map so that the 
          ## interpolation works properly
          lon_indices <- seq_along(lon)
        } else {
          extra_points <- min(maximum_extra_points, head(lon_indices, 1) - 1)
          if (extra_points > 0) {
            lon_indices <- 
              c((head(lon_indices, 1) - extra_points):(head(lon_indices, 1) - 1), lon_indices)
          }
          extra_points <- min(maximum_extra_points, length(lon) - tail(lon_indices, 1))
          if (extra_points > 0) {
            lon_indices <- c(lon_indices, 
                             (tail(lon_indices, 1) + 1):(tail(lon_indices, 1) + extra_points))
          }
        }
        min_lat_ind <- min(lat_indices)
        max_lat_ind <- max(lat_indices)
        extra_points <- min(maximum_extra_points, min_lat_ind - 1)
        if (extra_points > 0) {
          if (lat[1] < tail(lat, 1)) {
            lat_indices <- c(lat_indices, (min_lat_ind - 1):(min_lat_ind - extra_points))
          } else {
            lat_indices <- c((min_lat_ind - extra_points):(min_lat_ind - 1), lat_indices)
          }
        }
        extra_points <- min(maximum_extra_points, length(lat) - max_lat_ind)
        if (extra_points > 0) {
          if (lat[1] < tail(lat, 1)) {
            lat_indices <- c((max_lat_ind + extra_points):(max_lat_ind + 1), lat_indices)
          } else {
            lat_indices <- c(lat_indices, (max_lat_ind + 1):(max_lat_ind + extra_points))
          }
        }
      }
      lon <- lon[lon_indices]
      lat <- lat[lat_indices]
      expected_dims <- c(work_piece[['dimnames']][['lon']],
                         work_piece[['dimnames']][['lat']])
    } else {
      lon <- 0
      lat <- 0
    }
    # We keep on filling the expected dimensions
    var_dimnames <- unlist(lapply(fnc$var[[namevar]][['dim']], '[[', 'name'))
    nmemb <- nltime <- NULL
    ## Sometimes CDO renames 'members' dimension to 'lev'
    old_members_dimname <- NULL
    if (('lev' %in% var_dimnames) && !(work_piece[['dimnames']][['member']] %in% var_dimnames)) {
      old_members_dimname <- work_piece[['dimnames']][['member']]
      work_piece[['dimnames']][['member']] <- 'lev'
    }
    if (work_piece[['dimnames']][['member']] %in% var_dimnames) {
      nmemb <- fnc$var[[namevar]][['dim']][[match(work_piece[['dimnames']][['member']], 
                                                  var_dimnames)]]$len
      expected_dims <- c(expected_dims, work_piece[['dimnames']][['member']])
    } else {
      nmemb <- 1
    }
    if (length(expected_dims) > 0) {
      dim_matches <- match(expected_dims, var_dimnames)
      if (anyNA(dim_matches)) {
        if (!is.null(old_members_dimname)) {
          expected_dims[which(expected_dims == 'lev')] <- old_members_dimname
        }
        stop("Error: the expected dimension(s)", 
             toString(expected_dims[which(is.na(dim_matches))]), 
             "were not found in", filename)
      }
      time_dimname <- var_dimnames[-dim_matches]
    } else {
      time_dimname <- var_dimnames
    }
    if (length(time_dimname) > 0) {
      if (length(time_dimname) == 1) {
        nltime <- fnc$var[[namevar]][['dim']][[match(time_dimname, var_dimnames)]]$len
        expected_dims <- c(expected_dims, time_dimname)
        dim_matches <- match(expected_dims, var_dimnames)
      } else {
        if (!is.null(old_members_dimname)) {
          expected_dims[which(expected_dims == 'lev')] <- old_members_dimname
        }
        stop("Error: the variable ", namevar, 
             " is defined over more dimensions than the expected (", 
             toString(c(expected_dims, 'time')), 
             "). It could also be that the members, longitude or latitude ",
             "dimensions are named incorrectly. In that case, either rename ",
             "the dimensions in the file or adjust Load() to recognize the actual ",
             "name with the parameter 'dimnames'. See file ", filename)
      }
    } else {
      nltime <- 1
    }

    # Now we must retrieve the data from the file, but only the asked indices.
    # So we build up the indices to retrieve.
    # Longitudes or latitudes have been retrieved already.
    if (explore_dims) {
      # If we're exploring the file we only want one time step from one member, 
      # to regrid it and work out the number of longitudes and latitudes.
      # We don't need more.
      members <- 1
      ltimes_list <- list(1)
    } else {
      # The data is arranged in the array 'tmp' with the dimensions in a 
      # common order:
      #   1) Longitudes 
      #   2) Latitudes
      #   3) Members (even if is not a file per member experiment)
      #   4) Lead-times
      if (work_piece[['is_file_per_dataset']]) {
        time_indices <- 1:nltime
        mons <- strsplit(system(paste('cdo showmon ', filein, 
                         ' 2>/dev/null'), intern = TRUE), split = ' ')
        years <- strsplit(system(paste('cdo showyear ', filein, 
                          ' 2>/dev/null'), intern = TRUE), split = ' ')
        mons <- as.numeric(mons[[1]][which(mons[[1]] != "")])
        years <- as.numeric(years[[1]][which(years[[1]] != "")])
        time_indices <- ts(time_indices, start = c(years[1], mons[1]), 
                           end = c(years[length(years)], mons[length(mons)]),
                           frequency = 12)
                ltimes_list <- list()
        for (sdate in work_piece[['startdates']]) {
          selected_time_indices <- window(time_indices, start = c(as.numeric(
                                   substr(sdate, 1, 4)), as.numeric(substr(sdate, 5, 6))), 
                                   end = c(3000, 12), frequency = 12, extend = TRUE)
          selected_time_indices <- selected_time_indices[work_piece[['leadtimes']]]
          ltimes_list <- c(ltimes_list, list(selected_time_indices))
        }
      } else {
        ltimes <- work_piece[['leadtimes']]
        #if (work_piece[['dataset_type']] == 'exp') {
          ltimes_list <- list(ltimes[which(ltimes <= nltime)])
        #}
      }
      ## TODO: Put, when reading matrices, this kind of warnings
      #  if (nmember < nmemb) {
      #    cat("Warning:
      members <- 1:work_piece[['nmember']]
      members <- members[which(members <= nmemb)]
    }

    # Now, for each list of leadtimes to load (usually only one list with all leadtimes), 
    # we'll join the indices and retrieve data
    found_disordered_dims <- FALSE
    for (ltimes in ltimes_list) {
      if (is_2d_var) {
        start <- c(min(lon_indices), min(lat_indices))
        end <- c(max(lon_indices), max(lat_indices))
        if (lonlat_subsetting_requested && remap_needed) {
          subset_indices <- list(min(lon_indices):max(lon_indices) - min(lon_indices) + 1,
                                 lat_indices - min(lat_indices) + 1)
          dim_longitudes <- ncdim_def(work_piece[['dimnames']][['lon']], "degrees_east", lon)
          dim_latitudes <- ncdim_def(work_piece[['dimnames']][['lat']], "degrees_north", lat)
          ncdf_dims <- list(dim_longitudes, dim_latitudes)
        } else {
          subset_indices <- list(lon_indices - min(lon_indices) + 1,
                                 lat_indices - min(lat_indices) + 1)
          ncdf_dims <- list()
        }
        final_dims <- c(length(subset_indices[[1]]), length(subset_indices[[2]]), 1, 1)
      } else {
        start <- end <- NULL
        subset_indices <- list()
        ncdf_dims <- list()
        final_dims <- c(1, 1, 1, 1)
      }
      
      if (work_piece[['dimnames']][['member']] %in% expected_dims) {
        start <- c(start, head(members, 1))
        end <- c(end, tail(members, 1))
        subset_indices <- c(subset_indices, list(members - head(members, 1) + 1))
        dim_members <- ncdim_def(work_piece[['dimnames']][['member']], "", members)
        ncdf_dims <- c(ncdf_dims, list(dim_members))
        final_dims[3] <- length(members)
      }
      if (time_dimname %in% expected_dims) {
        if (!all(is.na(ltimes))) {
          start <- c(start, head(ltimes[which(!is.na(ltimes))], 1))
          end <- c(end, tail(ltimes[which(!is.na(ltimes))], 1))
          subset_indices <- c(subset_indices, 
                              list(ltimes - head(ltimes[which(!is.na(ltimes))], 1) + 1))
        } else {
          start <- c(start, NA)
          end <- c(end, NA)
          subset_indices <- c(subset_indices, list(ltimes))
        }
        dim_time <- ncdim_def(time_dimname, "", seq_along(ltimes), unlim = TRUE)
        ncdf_dims <- c(ncdf_dims, list(dim_time))
        final_dims[4] <- length(ltimes)
      }
      count <- end - start + 1
      start <- start[dim_matches]
      count <- count[dim_matches]
      subset_indices <- subset_indices[dim_matches]
      # Now that we have the indices to retrieve, we retrieve the data
      if (prod(final_dims) > 0) {
        tmp <- take(ncvar_get(fnc, namevar, start, count, 
                    collapse_degen = FALSE), 
                    seq_along(subset_indices), subset_indices)
        # The data is regridded if it corresponds to an atmospheric variable. When
        # the chosen output type is 'areave' the data is not regridded to not 
        # waste computing time unless the user specified a common grid.
        if (is_2d_var) {
          ###if (!is.null(work_piece[['mask']]) && !(lonlat_subsetting_requested && remap_needed)) {
          ###  mask <- take(ncvar_get(fnc_mask, work_piece[['mask']][['nc_var_name']], 
          ###               start[dim_matches[1:2]], count[dim_matches[1:2]],
          ###               collapse_degen = FALSE), 1:2, subset_indices[dim_matches[1:2]])
          ###}
          if (lonlat_subsetting_requested && remap_needed) {
            filein <- tempfile(pattern = "loadRegridded", fileext = ".nc")
            filein2 <- tempfile(pattern = "loadRegridded2", fileext = ".nc")
            ncdf_var <- ncvar_def(namevar, "", ncdf_dims[dim_matches], 
                                  fnc$var[[namevar]]$missval, 
                                  prec = if (fnc$var[[namevar]]$prec == 'int') {
                                           'integer'
                                         } else {
                                           fnc$var[[namevar]]$prec
                                         })
            scale_factor <- ifelse(fnc$var[[namevar]]$hasScaleFact, fnc$var[[namevar]]$scaleFact, 1)
            add_offset <- ifelse(fnc$var[[namevar]]$hasAddOffset, fnc$var[[namevar]]$addOffset, 0)
            if (fnc$var[[namevar]]$hasScaleFact || fnc$var[[namevar]]$hasAddOffset) {
              tmp <- (tmp - add_offset) / scale_factor
            }
            #nc_close(fnc)
            fnc2 <- nc_create(filein2, list(ncdf_var))
            ncvar_put(fnc2, ncdf_var, tmp)
            if (add_offset != 0) {
              ncatt_put(fnc2, ncdf_var, 'add_offset', add_offset)
            }
            if (scale_factor != 1) {
              ncatt_put(fnc2, ncdf_var, 'scale_factor', scale_factor)
            }
            nc_close(fnc2)
            system(paste0("cdo -L -s -sellonlatbox,", if (lonmin > lonmax) {
                                                     "0,360,"
                                                   } else {
                                                     paste0(lonmin, ",", lonmax, ",")
                                                   }, latmin, ",", latmax,
                   " -remap", work_piece[['remap']], ",", common_grid_name, 
                   " ", filein2, " ", filein, " 2>/dev/null"))
            file.remove(filein2)
            fnc2 <- nc_open(filein)
            sub_lon <- ncvar_get(fnc2, 'lon')
            sub_lat <- ncvar_get(fnc2, 'lat')
            ## We read the longitudes and latitudes from the file.
            ## In principle cdo should put in order the longitudes
            ## and slice them properly unless data is across greenwich
            sub_lon[which(sub_lon < 0)] <- sub_lon[which(sub_lon < 0)] + 360
            sub_lon_indices <- seq_along(sub_lon)
            if (lonmax < lonmin) {
              sub_lon_indices <- sub_lon_indices[which((sub_lon <= lonmax) | (sub_lon >= lonmin))]
            }
            sub_lat_indices <- seq_along(sub_lat)
            ## In principle cdo should put in order the latitudes
            if (sub_lat[1] < sub_lat[length(sub_lat)]) {
              sub_lat_indices <- rev(seq_along(sub_lat))
            }
            final_dims[c(1, 2)] <- c(length(sub_lon_indices), length(sub_lat_indices))
            subset_indices[[dim_matches[1]]] <- sub_lon_indices
            subset_indices[[dim_matches[2]]] <- sub_lat_indices

            tmp <- take(ncvar_get(fnc2, namevar, collapse_degen = FALSE), 
                        seq_along(subset_indices), subset_indices)

            if (!is.null(mask)) {
              ## We create a very simple 2d netcdf file that is then interpolated to the common
              ## grid to know what are the lons and lats of our slice of data
              mask_file <- tempfile(pattern = 'loadMask', fileext = '.nc')
              mask_file_remap <- tempfile(pattern = 'loadMask', fileext = '.nc')
              dim_longitudes <- ncdim_def(work_piece[['dimnames']][['lon']], 
                                          "degrees_east", c(0, 360))
              dim_latitudes <- ncdim_def(work_piece[['dimnames']][['lat']], 
                                         "degrees_north", c(-90, 90))
              ncdf_var <- ncvar_def('LSM', "", list(dim_longitudes, dim_latitudes), NA, 'double')
              fnc_mask <- nc_create(mask_file, list(ncdf_var))
              ncvar_put(fnc_mask, ncdf_var, array(rep(0, 4), dim = c(2, 2)))
              nc_close(fnc_mask)
              system(paste0("cdo -L -s remap", work_piece[['remap']], ",", 
                            common_grid_name,
                     " ", mask_file, " ", mask_file_remap, " 2>/dev/null"))
              fnc_mask <- nc_open(mask_file_remap)
              mask_lons <- ncvar_get(fnc_mask, 'lon')
              mask_lats <- ncvar_get(fnc_mask, 'lat')
              nc_close(fnc_mask)
              file.remove(mask_file, mask_file_remap)
              if ((dim(mask)[1] != common_grid_lons) || (dim(mask)[2] != common_grid_lats)) {
                stop("Error: the mask of the dataset with index ", 
                     tail(work_piece[['indices']], 1), " in '", 
                     work_piece[['dataset_type']], 
                     "' is wrong. It must be on the common grid if the ",
                     "selected output type is 'lonlat', 'lon' or 'lat', ",
                     "or 'areave' and 'grid' has been specified. It must ",
                     "be on the grid of the corresponding dataset if the ",
                     "selected output type is 'areave' and no 'grid' has been ",
                     "specified. For more information check ?Load and see help ",
                     "on parameters 'grid', 'maskmod' and 'maskobs'.")
              }
              mask_lons[which(mask_lons < 0)] <- mask_lons[which(mask_lons < 0)] + 360
              if (lonmax >= lonmin) {
                mask_lon_indices <- which((mask_lons >= lonmin) & (mask_lons <= lonmax))
              } else {
                mask_lon_indices <- which((mask_lons >= lonmin) | (mask_lons <= lonmax))
              }
              mask_lat_indices <- which((mask_lats >= latmin) & (mask_lats <= latmax))
              if (sub_lat[1] < sub_lat[length(sub_lat)]) {
                mask_lat_indices <- mask_lat_indices[rev(seq_along(mask_lat_indices))]
              }
              mask <- mask[mask_lon_indices, mask_lat_indices]
            }
            sub_lon <- sub_lon[sub_lon_indices]
            sub_lat <- sub_lat[sub_lat_indices]
            ###  nc_close(fnc_mask)
            ###  system(paste0("cdo -s -sellonlatbox,", if (lonmin > lonmax) {
            ###                                           "0,360,"
            ###                                         } else {
            ###                                           paste0(lonmin, ",", lonmax, ",")
            ###                                         }, latmin, ",", latmax,
            ###         " -remap", work_piece[['remap']], ",", common_grid_name, 
            ###This is wrong: same files  
            ###         " ", mask_file, " ", mask_file, " 2>/dev/null", sep = ""))
            ###  fnc_mask <- nc_open(mask_file) 
            ###  mask <- take(ncvar_get(fnc_mask, work_piece[['mask']][['nc_var_name']],
            ###               collapse_degen = FALSE), 1:2, subset_indices[dim_matches[1:2]])
            ###}
          }
        }
        if (is.unsorted(dim_matches)) {
          if (!found_disordered_dims && 
              rev(work_piece[['indices']])[2] == 1 && 
              rev(work_piece[['indices']])[3] == 1) {
            found_disordered_dims <- TRUE
            .warning(paste0("The dimensions for the variable ", namevar, 
                            " in the files of the experiment with index ", 
                            tail(work_piece[['indices']], 1), 
                            " are not in the optimal order for loading with Load(). ",
                            "The optimal order would be '", 
                            toString(expected_dims), 
                            "'. One of the files of the dataset is stored in ", filename))
          }
          tmp <- aperm(tmp, dim_matches)
        }
        dim(tmp) <- final_dims
        # If we are exploring the file we don't need to process and arrange
        # the retrieved data. We only need to keep the dimension sizes.
        if (is_2d_var && lonlat_subsetting_requested && remap_needed) {
          final_lons <- sub_lon
          final_lats <- sub_lat
        } else {
          final_lons <- lon
          final_lats <- lat
        }
        if (explore_dims) {
          if (work_piece[['is_file_per_member']]) {
            ## TODO: When the exp_full_path contains asterisks and is file_per_member
            ##       members from different datasets may be accounted.
            ##       Also if one file member is missing the accounting will be wrong.
            ##       Should parse the file name and extract number of members.
            if (is_url) {
              nmemb <- NULL
            } else {
              nmemb <- length(files)
            }
          }
          dims <- list(member = nmemb, ftime = nltime, lon = final_lons, lat = final_lats)
        } else {
        # If we are not exploring, then we have to process the retrieved data
          if (is_2d_var) {
            tmp <- apply(tmp, c(3, 4), function(x) {
              # Disable of large values.
              if (!is.na(work_piece[['var_limits']][2])) {
                x[which(x > work_piece[['var_limits']][2])] <- NA
              }
              if (!is.na(work_piece[['var_limits']][1])) {
                x[which(x < work_piece[['var_limits']][1])] <- NA
              }
              if (!is.null(mask)) {
                x[which(mask < 0.5)] <- NA
              }
  
              if (output == 'areave' || output == 'lon') {
                weights <- InsertDim(cos(final_lats * pi / 180), 1, 
                                     length(final_lons), name = 'lon')
                weights[which(is.na(x))] <- NA
                if (output == 'areave') {
                  weights <- weights / mean(weights, na.rm = TRUE)
                  mean(x * weights, na.rm = TRUE) 
                } else {
                  weights <- weights / InsertDim(MeanDims(weights, 2, na.rm = TRUE), 2, 
                                                 length(final_lats), name = 'lat')
                  MeanDims(x * weights, 2, na.rm = TRUE)
                }
              } else if (output == 'lat') {
                MeanDims(x, 1, na.rm = TRUE)
              } else if (output == 'lonlat') {
                signif(x, 5)
              }
            })
            if (output == 'areave') {
              dim(tmp) <- c(1, 1, final_dims[3:4])
            } else if (output == 'lon') {
              dim(tmp) <- c(final_dims[1], 1, final_dims[3:4])
            } else if (output == 'lat') {
              dim(tmp) <- c(1, final_dims[c(2, 3, 4)])
            } else if (output == 'lonlat') {
              dim(tmp) <- final_dims
            }
          }
          var_data <- attach.big.matrix(work_piece[['out_pointer']])
          if (work_piece[['dims']][['member']] > 1 && nmemb > 1 && 
              work_piece[['dims']][['ftime']] > 1 && 
              nltime < work_piece[['dims']][['ftime']]) {
            work_piece[['indices']][2] <- work_piece[['indices']][2] - 1
            for (jmemb in members) {
              work_piece[['indices']][2] <- work_piece[['indices']][2] + 1
              out_position <- arrayIndex2VectorIndex(work_piece[['indices']], work_piece[['dims']])
              out_indices <- out_position:(out_position + length(tmp[, , jmemb, ]) - 1)
              var_data[out_indices] <- as.vector(tmp[, , jmemb, ])
            }
            work_piece[['indices']][2] <- work_piece[['indices']][2] - tail(members, 1) + 1
          } else {
            out_position <- arrayIndex2VectorIndex(work_piece[['indices']], work_piece[['dims']])
            out_indices <- out_position:(out_position + length(tmp) - 1)
            a <- aperm(tmp, c(1, 2, 4, 3))
            as.vector(a)
            var_data[out_indices] <- as.vector(aperm(tmp, c(1, 2, 4, 3)))
          }
          work_piece[['indices']][3] <- work_piece[['indices']][3] + 1
        }
      }
    }
    nc_close(fnc)    
    if (is_2d_var) {
      if (remap_needed) {
        array_across_gw <- FALSE
        file.remove(filein)
        ###if (!is.null(mask) && lonlat_subsetting_requested) {
        ###  file.remove(mask_file)
        ###}
      } else {
        if (first_lon_in_original_file < 0) {
          array_across_gw <- data_across_gw
        } else {
          array_across_gw <- FALSE
        }
      }
    }    
  }
  if (explore_dims) {
    list(dims = dims, is_2d_var = is_2d_var, grid = grid_name, 
         units = units, var_long_name = var_long_name, 
         data_across_gw = data_across_gw, array_across_gw = array_across_gw)
  } else {
    ###if (!silent && !is.null(progress_connection) && !is.null(work_piece[['progress_amount']])) {
    ###  foobar <- writeBin(work_piece[['progress_amount']], progress_connection)
    ###}
    if (!silent && !is.null(work_piece[['progress_amount']])) {
      message(work_piece[['progress_amount']], appendLF = FALSE)
    }
    found_file
  }
}

.LoadSampleData <- function(var, exp = NULL, obs = NULL, sdates, 
                            nmember = NULL, nmemberobs = NULL, 
                            nleadtime = NULL, leadtimemin = 1, 
                            leadtimemax = NULL, storefreq = 'monthly', 
                            sampleperiod = 1, lonmin = 0, lonmax = 360, 
                            latmin = -90, latmax = 90, output = 'areave', 
                            method = 'conservative', grid = NULL, 
                            maskmod = vector("list", 15), 
                            maskobs = vector("list", 15), 
                            configfile = NULL, suffixexp = NULL, 
                            suffixobs = NULL, varmin = NULL, varmax = NULL, 
                            silent = FALSE, nprocs = NULL) {
  ## This function loads and selects sample data stored in sampleMap and 
  ## sampleTimeSeries and is used in the examples instead of Load() so as
  ## to avoid nco and cdo system calls and computation time in the stage 
  ## of running examples in the CHECK process on CRAN.
  selected_start_dates <- match(sdates, c('19851101', '19901101', '19951101', 
                                          '20001101', '20051101'))
  start_dates_position <- 3
  lead_times_position <- 4

  if (output == 'lonlat') {
    sampleData <- s2dv::sampleMap
    if (is.null(leadtimemax)) {
      leadtimemax <- dim(sampleData$mod)[lead_times_position]
    }
    selected_lead_times <- leadtimemin:leadtimemax

    dataOut <- sampleData
    dataOut$mod <- sampleData$mod[, , selected_start_dates, selected_lead_times, , ]
    dataOut$obs <- sampleData$obs[, , selected_start_dates, selected_lead_times, , ]
  } else if (output == 'areave') {
    sampleData <- s2dv::sampleTimeSeries
    if (is.null(leadtimemax)) {
      leadtimemax <- dim(sampleData$mod)[lead_times_position]
    }
    selected_lead_times <- leadtimemin:leadtimemax

    dataOut <- sampleData
    dataOut$mod <- sampleData$mod[, , selected_start_dates, selected_lead_times]
    dataOut$obs <- sampleData$obs[, , selected_start_dates, selected_lead_times]
  }

  dims_out <- dim(sampleData$mod)
  dims_out[start_dates_position] <- length(selected_start_dates)
  dims_out[lead_times_position] <- length(selected_lead_times)
  dim(dataOut$mod) <- dims_out

  dims_out <- dim(sampleData$obs)
  dims_out[start_dates_position] <- length(selected_start_dates)
  dims_out[lead_times_position] <- length(selected_lead_times)
  dim(dataOut$obs) <- dims_out

  invisible(list(mod = dataOut$mod, obs = dataOut$obs, 
                 lat = dataOut$lat, lon = dataOut$lon))
}

.ConfigGetDatasetInfo <- function(matching_entries, table_name) {
  # This function obtains the information of a dataset and variable pair,
  # applying all the entries that match in the configuration file.
  if (table_name == 'experiments') {
    id <- 'EXP'
  } else {
    id <- 'OBS'
  }
  defaults <- c(paste0('$DEFAULT_', id, '_MAIN_PATH$'), 
                paste0('$DEFAULT_', id, '_FILE_PATH$'), 
                '$DEFAULT_NC_VAR_NAME$', '$DEFAULT_SUFFIX$', 
                '$DEFAULT_VAR_MIN$', '$DEFAULT_VAR_MAX$')
  info <- NULL

  for (entry in matching_entries) {
    if (is.null(info)) {
      info <- entry[-1:-2]
      info[which(info == '*')] <- defaults[which(info == '*')]
    } else {
      info[which(entry[-1:-2] != '*')] <- entry[-1:-2][which(entry[-1:-2] != '*')]
    }
  }

  info <- as.list(info)
  names(info) <- c('main_path', 'file_path', 'nc_var_name', 'suffix', 'var_min', 'var_max')  
  info
}

.ReplaceGlobExpressions <- function(path_with_globs, actual_path, 
                                    replace_values, tags_to_keep, 
                                    dataset_name, permissive) {
  # The goal of this function is to replace the shell globbing expressions in
  # a path pattern (that may contain shell globbing expressions and Load() 
  # tags) by the corresponding part of the real existing path.
  # What is done actually is to replace all the values of the tags in the 
  # actual path by the corresponding $TAG$
  #
  # It takes mainly two inputs. The path with expressions and tags, e.g.:
  #   /data/experiments/*/$EXP_NAME$/$VAR_NAME$/$VAR_NAME$_*$START_DATE$*.nc
  # and a complete known path to one of the matching files, e.g.:
  #   /data/experiments/ecearth/i00k/tos/tos_fc0-1_19901101_199011-199110.nc
  # and it returns the path pattern but without shell globbing expressions:
  #   /data/experiments/ecearth/$EXP_NAME$/$VAR_NAME$/$VAR_NAME$_fc0-1_$START_DATE$_199011-199110.nc
  #
  # To do that, it needs also as inputs the list of replace values (the 
  # association of each tag to their value).
  #
  # All the tags not present in the parameter tags_to_keep will be repalced.
  #
  # Not all cases can be resolved with the implemented algorithm. In an
  # unsolvable case a warning is given and one possible guess is returned.
  #
  # In some cases it is interesting to replace only the expressions in the
  # path to the file, but not the ones in the file name itself. To keep the
  # expressions in the file name, the parameter permissive can be set to 
  # TRUE. To replace all the expressions it can be set to FALSE.
  clean <- function(x) {
    if (nchar(x) > 0) {
      x <- gsub('\\\\', '', x)
      x <- gsub('\\^', '', x)
      x <- gsub('\\$', '', x)
      x <- unname(sapply(strsplit(x, '[', fixed = TRUE)[[1]], function(y) gsub('.*]', '.', y)))
      do.call(paste0, as.list(x))
    } else {
      x
    }
  }

  strReverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")

  if (permissive) {
    actual_path_chunks <- strsplit(actual_path, '/')[[1]]
    actual_path <- paste(actual_path_chunks[-length(actual_path_chunks)], collapse = '/')
    file_name <- tail(actual_path_chunks, 1)
    if (length(actual_path_chunks) > 1) {
      file_name <- paste0('/', file_name)
    }
    path_with_globs_chunks <- strsplit(path_with_globs, '/')[[1]]
    path_with_globs <- paste(path_with_globs_chunks[-length(path_with_globs_chunks)], 
                             collapse = '/')
    path_with_globs <- .ConfigReplaceVariablesInString(path_with_globs, replace_values)
    file_name_with_globs <- tail(path_with_globs_chunks, 1)
    if (length(path_with_globs_chunks) > 1) {
      file_name_with_globs <- paste0('/', file_name_with_globs)
    }
    right_known <- head(strsplit(file_name_with_globs, '*', fixed = TRUE)[[1]], 1)
    right_known_no_tags <- .ConfigReplaceVariablesInString(right_known, replace_values)
    path_with_globs_rx <- utils::glob2rx(paste0(path_with_globs, right_known_no_tags))
    match <- regexpr(gsub('$', '', path_with_globs_rx, fixed = TRUE), 
                     paste0(actual_path, file_name))
    if (match != 1) {
      stop("Incorrect parameters to replace glob expressions. ",
           "The path with expressions does not match the actual path.")
    }
    if (attr(match, 'match.length') - nchar(right_known_no_tags) < nchar(actual_path)) {
      path_with_globs <- paste0(path_with_globs, right_known_no_tags, '*')
      file_name_with_globs <- sub(right_known, '/*', file_name_with_globs)
    } 
  }
  path_with_globs_rx <- utils::glob2rx(path_with_globs)
  values_to_replace <- NULL
  tags_to_replace_starts <- NULL
  tags_to_replace_ends <- NULL
  give_warning <- FALSE
  for (tag in tags_to_keep) {
    matches <- gregexpr(paste0('$', tag, '$'), path_with_globs_rx, fixed = TRUE)[[1]]
    lengths <- attr(matches, 'match.length')
    if (!(length(matches) == 1 && matches[1] == -1)) {
      for (i in seq_along(matches)) {
        left <- NULL
        if (matches[i] > 1) {
          left <- 
            .ConfigReplaceVariablesInString(substr(path_with_globs_rx, 1, 
                                                   matches[i] - 1), replace_values)
          left_known <- 
            strReverse(head(strsplit(strReverse(left), 
                            strReverse('.*'), fixed = TRUE)[[1]], 1))
        }
        right <- NULL
        if ((matches[i] + lengths[i] - 1) < nchar(path_with_globs_rx)) {
          right <- 
            .ConfigReplaceVariablesInString(substr(path_with_globs_rx, 
                                                   matches[i] + lengths[i], 
                                                   nchar(path_with_globs_rx)),
                                                   replace_values)
          right_known <- head(strsplit(right, '.*', fixed = TRUE)[[1]], 1)
        }
        match_limits <- NULL
        if (!is.null(left)) {
          left_match <- regexpr(paste0(left, replace_values[[tag]], right_known), actual_path)
          match_len <- attr(left_match, 'match.length')
          left_match_limits <- 
            c(left_match + match_len - 1 - nchar(clean(right_known)) - 
                nchar(replace_values[[tag]]) + 1,
              left_match + match_len - 1 - nchar(clean(right_known)))
          if (!(left_match < 1)) {
            match_limits <- left_match_limits
          }
        }
        right_match <- NULL
        if (!is.null(right)) {
          right_match <- regexpr(paste0(left_known, replace_values[[tag]], right), actual_path)
          match_len <- attr(right_match, 'match.length')
          right_match_limits <- 
            c(right_match + nchar(clean(left_known)),
              right_match + nchar(clean(left_known)) + 
                nchar(replace_values[[tag]]) - 1)
          if (is.null(match_limits) && !(right_match < 1)) {
            match_limits <- right_match_limits
          }
        }
        if (!is.null(right_match) && !is.null(left_match)) {
          if (!identical(right_match_limits, left_match_limits)) {
            give_warning <- TRUE
          }
        }
        if (is.null(match_limits)) {
          stop("Too complex path pattern specified for ", dataset_name,
               ". Specify a simpler path pattern for this dataset.")
        }
        values_to_replace <- c(values_to_replace, tag)
        tags_to_replace_starts <- c(tags_to_replace_starts, match_limits[1])
        tags_to_replace_ends <- c(tags_to_replace_ends, match_limits[2])
      }
    }
  }

  if (length(tags_to_replace_starts) > 0) {
    reorder <- sort(tags_to_replace_starts, index.return = TRUE)
    tags_to_replace_starts <- reorder$x
    values_to_replace <- values_to_replace[reorder$ix]
    tags_to_replace_ends <- tags_to_replace_ends[reorder$ix]
    while (length(values_to_replace) > 0) {
      actual_path <- paste0(substr(actual_path, 1, head(tags_to_replace_starts, 1) - 1),
                           '$', head(values_to_replace, 1), '$',
                           substr(actual_path, head(tags_to_replace_ends, 1) + 1, 
                                  nchar(actual_path)))
      extra_chars <- nchar(head(values_to_replace, 1)) + 2 - 
                           (head(tags_to_replace_ends, 1) - 
                              head(tags_to_replace_starts, 1) + 1)
      values_to_replace <- values_to_replace[-1]
      tags_to_replace_starts <- tags_to_replace_starts[-1]
      tags_to_replace_ends <- tags_to_replace_ends[-1]
      tags_to_replace_starts <- tags_to_replace_starts + extra_chars
      tags_to_replace_ends <- tags_to_replace_ends + extra_chars
    }
  }

  if (give_warning) {
    .warning(paste0("Too complex path pattern specified for ", dataset_name, 
                    ". Double check carefully the '$Files' fetched for this dataset ",
                    "or specify a simpler path pattern."))
  }

  if (permissive) {
    paste0(actual_path, file_name_with_globs)
  } else {
    actual_path
  }
}

.FindTagValue <- function(path_with_globs_and_tag, actual_path, tag) {
  tag <- paste0('\\$', tag, '\\$')
  path_with_globs_and_tag <- paste0('^', path_with_globs_and_tag, '$')
  parts <- strsplit(path_with_globs_and_tag, '*', fixed = TRUE)[[1]]
  parts <- as.list(grep(tag, parts, value = TRUE))
  longest_couples <- NULL
  pos_longest_couples <- NULL
  found_value <- NULL
  for (i in seq_along(parts)) {
    parts[[i]] <- strsplit(parts[[i]], tag)[[1]]
    if (length(parts[[i]]) == 1) {
      parts[[i]] <- c(parts[[i]], '')
    }
    len_parts <- sapply(parts[[i]], nchar)
    len_couples <- len_parts[-length(len_parts)] + len_parts[2:length(len_parts)]
    pos_longest_couples <- c(pos_longest_couples, which.max(len_couples))
    longest_couples <- c(longest_couples, max(len_couples))
  }
  chosen_part <- which.max(longest_couples)
  parts[[chosen_part]] <- 
    parts[[chosen_part]][pos_longest_couples[chosen_part]:(pos_longest_couples[chosen_part] + 1)]
  if (nchar(parts[[chosen_part]][1]) >= nchar(parts[[chosen_part]][2])) {
    if (nchar(parts[[chosen_part]][1]) > 0) {
      matches <- gregexpr(parts[[chosen_part]][1], actual_path)[[1]]
      if (length(matches) == 1) {
        match_left <- matches
        actual_path <- 
          substr(actual_path, match_left + attr(match_left, 'match.length'), nchar(actual_path))
      }
    }
    if (nchar(parts[[chosen_part]][2]) > 0) {
      matches <- gregexpr(parts[[chosen_part]][2], actual_path)[[1]]
      if (length(matches) == 1) {
        match_right <- matches
        found_value <- substr(actual_path, 0, match_right - 1)
      }
    }
  } else {
    if (nchar(parts[[chosen_part]][2]) > 0) {
      matches <- gregexpr(parts[[chosen_part]][2], actual_path)[[1]]
      if (length(matches) == 1) {
        match_right <- matches
        actual_path <- substr(actual_path, 0, match_right - 1)
      }
    }
    if (nchar(parts[[chosen_part]][1]) > 0) {
      matches <- gregexpr(parts[[chosen_part]][1], actual_path)[[1]]
      if (length(matches) == 1) {
        match_left <- matches
        found_value <- 
          substr(actual_path, match_left + attr(match_left, 'match.length'), 
                 nchar(actual_path))
      }
    }
  }
  found_value
}

.FilterUserGraphicArgs <- function(excludedArgs, ...) {
  # This function filter the extra graphical parameters passed by the user in
  # a plot function, excluding the ones that the plot function uses by default.
  # Each plot function has a different set of arguments that are not allowed to
  # be modified.
  args <- list(...)
  userArgs <- list()
  for (name in names(args)) {
      if ((name != "") & !is.element(name, excludedArgs)) {
          # If the argument has a name and it is not in the list of excluded
          # arguments, then it is added to the list that will be used
          userArgs[[name]] <- args[[name]]
      } else {
        .warning(paste0("the argument '", name, "' can not be 
        modified and the new value will be ignored"))
      }
  }
  userArgs
}

.SelectDevice <- function(fileout, width, height, units, res) {
  # This function is used in the plot functions to check the extension of the 
  # files where the graphics will be stored and select the right R device to 
  # save them.
  # If the vector of filenames ('fileout') has files with different 
  # extensions, then it will only accept the first one, changing all the rest 
  # of the filenames to use that extension.

  # We extract the extension of the filenames: '.png', '.pdf', ...
  ext <- regmatches(fileout, regexpr("\\.[a-zA-Z0-9]*$", fileout))

  if (length(ext) != 0) {
    # If there is an extension specified, select the correct device
    ## units of width and height set to accept inches
    if (ext[1] == ".png") {
      saveToFile <- function(fileout) {
        png(filename = fileout, width = width, height = height, res = res, units = units)
      }
    } else if (ext[1] == ".jpeg") {
      saveToFile <- function(fileout) {
        jpeg(filename = fileout, width = width, height = height, res = res, units = units)
      }
    } else if (ext[1] %in% c(".eps", ".ps")) {
      saveToFile <- function(fileout) {
        postscript(file = fileout, width = width, height = height)
      }
    } else if (ext[1] == ".pdf") {
      saveToFile <- function(fileout) {
        pdf(file = fileout, width = width, height = height)
      }
    } else if (ext[1] == ".svg") {
      saveToFile <- function(fileout) {
        svg(filename = fileout, width = width, height = height)
      }
    } else if (ext[1] == ".bmp") {
      saveToFile <- function(fileout) {
        bmp(filename = fileout, width = width, height = height, res = res, units = units)
      }
    } else if (ext[1] == ".tiff") {
      saveToFile <- function(fileout) {
        tiff(filename = fileout, width = width, height = height, res = res, units = units)
      }
    } else {
      .warning("file extension not supported, it will be used '.eps' by default.")
      ## In case there is only one filename
      fileout[1] <- sub("\\.[a-zA-Z0-9]*$", ".eps", fileout[1])
      ext[1] <- ".eps"
      saveToFile <- function(fileout) {
        postscript(file = fileout, width = width, height = height)
      }
    }
    # Change filenames when necessary
    if (any(ext != ext[1])) {
      .warning(paste0("some extensions of the filenames provided in 'fileout' ",
                      "are not ", ext[1],
                      ". The extensions are being converted to ", ext[1], "."))
      fileout <- sub("\\.[a-zA-Z0-9]*$", ext[1], fileout)
    }
  } else {
    # Default filenames when there is no specification
    .warning("there are no extensions specified in the filenames, default to '.eps'")
    fileout <- paste0(fileout, ".eps")
    saveToFile <- postscript
  }

  # return the correct function with the graphical device, and the correct 
  # filenames
  list(fun = saveToFile, files = fileout)
}

.message <- function(...) {
  # Function to use the 'message' R function with our custom settings
  # Default: new line at end of message, indent to 0, exdent to 3, 
  #  collapse to \n*
  args <- list(...)

  ## In case we need to specify message arguments
  if (!is.null(args[["appendLF"]])) {
    appendLF <- args[["appendLF"]]
  } else {
    ## Default value in message function
    appendLF <- TRUE
  } 
  if (!is.null(args[["domain"]])) {
    domain <- args[["domain"]]
  } else {
    ## Default value in message function
    domain <- NULL
  }
  args[["appendLF"]] <- NULL
  args[["domain"]] <- NULL

  ## To modify strwrap indent and exdent arguments
  if (!is.null(args[["indent"]])) {
    indent <- args[["indent"]]
  } else {
    indent <- 0
  }
  if (!is.null(args[["exdent"]])) {
    exdent <- args[["exdent"]]
  } else {
    exdent <- 3
  }
  args[["indent"]] <- NULL
  args[["exdent"]] <- NULL

  ## To modify paste collapse argument
  if (!is.null(args[["collapse"]])) {
    collapse <- args[["collapse"]]
  } else {
    collapse <- "\n*"
  }
  args[["collapse"]] <- NULL

  ## Message tag
  if (!is.null(args[["tag"]])) {
    tag <- args[["tag"]]
  } else {
    tag <- "* "
  }
  args[["tag"]] <- NULL

  tmp <- paste0(tag, 
                paste(strwrap(args, indent = indent, exdent = exdent), collapse = collapse))
  message(tmp, appendLF = appendLF, domain = domain)
}

.warning <- function(...) {
  # Function to use the 'warning' R function with our custom settings
  # Default: no call information, indent to 0, exdent to 3, 
  #  collapse to \n
  args <- list(...)

  ## In case we need to specify warning arguments
  if (!is.null(args[["call."]])) {
    call <- args[["call."]]
  } else {
    ## Default: don't show info about the call where the warning came up
    call <- FALSE
  }
  if (!is.null(args[["immediate."]])) {
    immediate <- args[["immediate."]]
  } else {
    ## Default value in warning function
    immediate <- FALSE
  }
  if (!is.null(args[["noBreaks."]])) {
    noBreaks <- args[["noBreaks."]]
  } else {
    ## Default value warning function
    noBreaks <- FALSE
  }
  if (!is.null(args[["domain"]])) {
    domain <- args[["domain"]]
  } else {
    ## Default value warning function
    domain <- NULL
  }
  args[["call."]] <- NULL
  args[["immediate."]] <- NULL
  args[["noBreaks."]] <- NULL
  args[["domain"]] <- NULL

  ## To modify strwrap indent and exdent arguments
  if (!is.null(args[["indent"]])) {
    indent <- args[["indent"]]
  } else {
    indent <- 0
  }
  if (!is.null(args[["exdent"]])) {
    exdent <- args[["exdent"]]
  } else {
    exdent <- 3
  }
  args[["indent"]] <- NULL
  args[["exdent"]] <- NULL

  ## To modify paste collapse argument
  if (!is.null(args[["collapse"]])) {
    collapse <- args[["collapse"]]
  } else {
    collapse <- "\n!"
  }
  args[["collapse"]] <- NULL

  ## Warning tag
  if (!is.null(args[["tag"]])) {
    tag <- args[["tag"]]
  } else {
    tag <- "! Warning: "
  }
  args[["tag"]] <- NULL

  tmp <- paste0(tag, 
                paste(strwrap(args, indent = indent, exdent = exdent), collapse = collapse))
  warning(tmp, call. = call, immediate. = immediate, 
          noBreaks. = noBreaks, domain = domain)
}

.IsColor <- function(x) {
  res <- try(col2rgb(x), silent = TRUE)
  return(!is(res, "try-error"))
}

# This function switches to a specified figure at position (row, col) in a layout.
# This overcomes the bug in par(mfg = ...). However the mode par(new = TRUE) is 
# activated, i.e., all drawn elements will be superimposed. Additionally, after 
# using this function, the automatical pointing to the next figure in the layout
# will be spoiled: once the last figure in the layout is drawn, the pointer won't 
# move to the first figure in the layout.
# Only figures with numbers other than 0 (when creating the layout) will be
# accessible.
# Inputs: either row and col, or n and mat
.SwitchToFigure <- function(row = NULL, col = NULL, n = NULL, mat = NULL) {
  if (!is.null(n) && !is.null(mat)) {
    if (!is.numeric(n) || length(n) != 1) {
      stop("Parameter 'n' must be a single numeric value.")
    }
    n <- round(n)
    if (!is.array(mat)) {
      stop("Parameter 'mat' must be an array.")
    }
    target <- which(mat == n, arr.ind = TRUE)[1, ]
    row <- target[1]
    col <- target[2]
  } else if (!is.null(row) && !is.null(col)) {
    if (!is.numeric(row) || length(row) != 1) {
      stop("Parameter 'row' must be a single numeric value.")
    }
    row <- round(row)
    if (!is.numeric(col) || length(col) != 1) {
      stop("Parameter 'col' must be a single numeric value.")
    }
    col <- round(col)
  } else {
    stop("Either 'row' and 'col' or 'n' and 'mat' must be provided.")
  }
  next_attempt <- c(row, col)
  par(mfg = next_attempt)
  i <- 1
  layout_size <- par('mfrow')
  layout_cells <- matrix(1:prod(layout_size), layout_size[1], layout_size[2], 
                         byrow = TRUE)
  while (any((par('mfg')[1:2] != c(row, col)))) {
    next_attempt <- which(layout_cells == i, arr.ind = TRUE)[1, ]
    par(mfg = next_attempt)
    i <- i + 1
    if (i > prod(layout_size)) {
      stop("Figure not accessible.")
    }
  }
  plot(0, type = 'n', axes = FALSE, ann = FALSE)
  par(mfg = next_attempt)
}

# Function to permute arrays of non-atomic elements (e.g. POSIXct)
.aperm2 <- function(x, new_order) {
  old_dims <- dim(x)
  attr_bk <- attributes(x)
  if ('dim' %in% names(attr_bk)) {
    attr_bk[['dim']] <- NULL
  }
  if (is.numeric(x)) {
    x <- aperm(x, new_order)
  } else {
    y <- array(seq_along(x), dim = dim(x))
    y <- aperm(y, new_order)
    x <- x[as.vector(y)]
  }
  dim(x) <- old_dims[new_order]
  attributes(x) <- c(attributes(x), attr_bk)
  x
}

# This function is a helper for the function .MergeArrays.
# It expects as inputs two named numeric vectors, and it extends them
# with dimensions of length 1 until an ordered common dimension
# format is reached.
# The first output is dims1 extended with 1s.
# The second output is dims2 extended with 1s.
# The third output is a merged dimension vector. If dimensions with
# the same name are found in the two inputs, and they have a different
# length, the maximum is taken.
.MergeArrayDims <- function(dims1, dims2) {
  new_dims1 <- NULL
  new_dims2 <- NULL
  while (length(dims1) > 0) {
    if (names(dims1)[1] %in% names(dims2)) {
      pos <- which(names(dims2) == names(dims1)[1])
      dims_to_add <- rep(1, pos - 1)
      if (length(dims_to_add) > 0) {
        names(dims_to_add) <- names(dims2[1:(pos - 1)])
      }
      new_dims1 <- c(new_dims1, dims_to_add, dims1[1])
      new_dims2 <- c(new_dims2, dims2[1:pos])
      dims1 <- dims1[-1]
      dims2 <- dims2[-(1:pos)]
    } else {
      new_dims1 <- c(new_dims1, dims1[1])
      new_dims2 <- c(new_dims2, 1)
      names(new_dims2)[length(new_dims2)] <- names(dims1)[1]
      dims1 <- dims1[-1]
    }
  }
  if (length(dims2) > 0) {
    dims_to_add <- rep(1, length(dims2))
    names(dims_to_add) <- names(dims2)
    new_dims1 <- c(new_dims1, dims_to_add)
    new_dims2 <- c(new_dims2, dims2)
  }
  list(new_dims1, new_dims2, pmax(new_dims1, new_dims2))
}

# This function takes two named arrays and merges them, filling with
# NA where needed.
# dim(array1)
#          'b'   'c'         'e'   'f'
#           1     3           7     9
# dim(array2)
#    'a'   'b'         'd'         'f'   'g'
#     2     3           5           9     11
# dim(.MergeArrays(array1, array2, 'b'))
#    'a'   'b'   'c'   'e'   'd'   'f'   'g'
#     2     4     3     7     5     9     11
.MergeArrays <- function(array1, array2, along) {
  if (!(is.null(array1) || is.null(array2))) {
    if (!(identical(names(dim(array1)), names(dim(array2))) &&
        identical(dim(array1)[-which(names(dim(array1)) == along)],
                  dim(array2)[-which(names(dim(array2)) == along)]))) {
      new_dims <- .MergeArrayDims(dim(array1), dim(array2))
      dim(array1) <- new_dims[[1]]
      dim(array2) <- new_dims[[2]]
      for (j in seq_along(dim(array1))) {
        if (names(dim(array1))[j] != along) {
          if (dim(array1)[j] != dim(array2)[j]) {
            if (which.max(c(dim(array1)[j], dim(array2)[j])) == 1) {
              na_array_dims <- dim(array2)
              na_array_dims[j] <- dim(array1)[j] - dim(array2)[j]
              na_array <- array(dim = na_array_dims)
              array2 <- abind(array2, na_array, along = j)
              names(dim(array2)) <- names(na_array_dims)
            } else {
              na_array_dims <- dim(array1)
              na_array_dims[j] <- dim(array2)[j] - dim(array1)[j]
              na_array <- array(dim = na_array_dims)
              array1 <- abind(array1, na_array, along = j)
              names(dim(array1)) <- names(na_array_dims)
            }
          }
        }
      }
    }
    if (!(along %in% names(dim(array2)))) {
      stop("The dimension specified in 'along' is not present in the ",
           "provided arrays.")
    }
    array1 <- abind(array1, array2, along = which(names(dim(array1)) == along))
    names(dim(array1)) <- names(dim(array2))
  } else if (is.null(array1)) {
    array1 <- array2
  }
  array1
}

# only can be used in Trend(). Needs generalization or be replaced by other function.
.reorder <- function(output, time_dim, dim_names) {
  # Add dim name back
  if (is.null(dim(output))) {
    dim(output) <- c(stats = length(output))
  } else {  #is an array
    if (length(dim(output)) == 1) {
      if (!is.null(names(dim(output)))) {
        dim(output) <- c(1, dim(output))
        names(dim(output))[1] <- time_dim
      } else {
        names(dim(output)) <- time_dim
      }
    } else {  # more than one dim
      if (names(dim(output))[1] != "") {
        dim(output) <- c(1, dim(output))
        names(dim(output))[1] <- time_dim
      } else {   #regular case
        names(dim(output))[1] <- time_dim
      }
    }
  }
  # reorder
  pos <- match(dim_names, names(dim(output)))
  output <- aperm(output, pos)
  names(dim(output)) <- dim_names
  names(dim(output))[names(dim(output)) == time_dim] <- 'stats'
  return(output)
}

# to be used in AMV.R, TPI.R, SPOD.R, GSAT.R and GMST.R
.Indices <- function(data, type, monini, indices_for_clim, 
                     fmonth_dim, sdate_dim, year_dim, month_dim, na.rm) {
  
  if (type == 'dcpp') {
    
    fyear_dim <- 'fyear'
    data <- Season(data = data, time_dim = fmonth_dim,
                   monini = monini, moninf = 1, monsup = 12,
                   method = mean, na.rm = na.rm)
    names(dim(data))[which(names(dim(data)) == fmonth_dim)] <- fyear_dim
    
    if (identical(indices_for_clim, FALSE)) { ## data is already anomalies
      
      anom <- data
      
    } else { ## Different indices_for_clim for each forecast year (to use the same calendar years)
      
      n_fyears <- as.numeric(dim(data)[fyear_dim])
      n_sdates <- as.numeric(dim(data)[sdate_dim])
      
      if (is.null(indices_for_clim)) { ## climatology over the whole (common) period
        first_years_for_clim <- n_fyears : 1
        last_years_for_clim <- n_sdates : (n_sdates - n_fyears + 1)
      } else { ## indices_for_clim specified as a numeric vector
        first_years_for_clim <- seq(from = indices_for_clim[1], by = -1, length.out = n_fyears)
        last_years_for_clim <- 
          seq(from = indices_for_clim[length(indices_for_clim)], 
              by = -1, length.out = n_fyears)
      }
      
      data <- s2dv::Reorder(data = data, order = c(fyear_dim, sdate_dim))
      anom <- array(data = NA, dim = dim(data))
      for (i in 1:n_fyears) {
        clim <- mean(data[i, first_years_for_clim[i]:last_years_for_clim[i]], na.rm = na.rm)
        anom[i, ] <- data[i, ] - clim
      }
    }
    
  } else if (type %in% c('obs', 'hist')) {
    
    data <- multiApply::Apply(data = data, target_dims = month_dim, 
                              fun = mean, na.rm = na.rm)$output1
    
    if (identical(indices_for_clim, FALSE)) { ## data is already anomalies
      clim <- 0
    } else if (is.null(indices_for_clim)) { 
      ## climatology over the whole period
      clim <- multiApply::Apply(data = data, target_dims = year_dim, fun = mean, 
                                na.rm = na.rm)$output1
    } else { 
      ## indices_for_clim specified as a numeric vector
      clim <- multiApply::Apply(data = ClimProjDiags::Subset(x = data, along = year_dim,
                                                             indices = indices_for_clim),
                                target_dims = year_dim, fun = mean, na.rm = na.rm)$output1
    }
    
    anom <- data - clim
    
  } else {
    stop('type must be dcpp, hist or obs')
  }
  
  return(anom)
}

#TODO: Remove from s2dv when PlotLayout can get colorbar info from plotting function directly. 
#      The function is temporarily here because PlotLayout() needs to draw the colorbars of
#      PlotMostLikelyQuantileMap().
#Draws Color Bars for Categories
#A wrapper of s2dv::ColorBar to generate multiple color bars for different 
#categories, and each category has different color set.
GradientCatsColorBar <- function(nmap, brks = NULL, cols = NULL, vertical = TRUE, subsampleg = NULL,
                                 bar_limits, var_limits = NULL,
                                 triangle_ends = NULL, plot = TRUE,
                                 draw_separators = FALSE,
                                 bar_titles = NULL, title_scale = 1, 
                                 label_scale = 1, extra_margin = rep(0, 4),
                                 ...) {
  # bar_limits
  if (!is.numeric(bar_limits) || length(bar_limits) != 2) {
    stop("Parameter 'bar_limits' must be a numeric vector of length 2.")
  }

  # Check brks
  if (is.null(brks) || (is.numeric(brks) && length(brks) == 1)) {
    num_brks <- 5
    if (is.numeric(brks)) {
      num_brks <- brks
    }
    brks <- seq(from = bar_limits[1], to = bar_limits[2], length.out = num_brks)
  }
  if (!is.numeric(brks)) {
    stop("Parameter 'brks' must be a numeric vector.")
  }
  # Check cols
  col_sets <- list(c("#A1D99B", "#74C476", "#41AB5D", "#238B45"),
                   c("#6BAED6FF", "#4292C6FF", "#2171B5FF", "#08519CFF"),
                   c("#FFEDA0FF", "#FED976FF", "#FEB24CFF", "#FD8D3CFF"),
                   c("#FC4E2AFF", "#E31A1CFF", "#BD0026FF", "#800026FF"),
                   c("#FCC5C0", "#FA9FB5", "#F768A1", "#DD3497"))
  if (is.null(cols)) {
    if (length(col_sets) >= nmap) {
      chosen_sets <- 1:nmap
      chosen_sets <- chosen_sets + floor((length(col_sets) - length(chosen_sets)) / 2)
    } else {
      chosen_sets <- array(seq_along(col_sets), nmap)
    }
    cols <- col_sets[chosen_sets]
  } else {
    if (!is.list(cols)) {
      stop("Parameter 'cols' must be a list of character vectors.")
    }
    if (!all(sapply(cols, is.character))) {
      stop("Parameter 'cols' must be a list of character vectors.")
    }
    if (length(cols) != nmap) {
      stop("Parameter 'cols' must be a list of the same length as the number of ",
           "maps in 'maps'.")
    }
  }
  for (i in seq_along(cols)) {
    if (length(cols[[i]]) != (length(brks) - 1)) {
      cols[[i]] <- grDevices::colorRampPalette(cols[[i]])(length(brks) - 1)
    }
  }

  # Check bar_titles
  if (is.null(bar_titles)) {
    if (nmap == 3) {
      bar_titles <- c("Below normal (%)", "Normal (%)", "Above normal (%)")
    } else if (nmap == 5) {
      bar_titles <- c("Low (%)", "Below normal (%)",
                         "Normal (%)", "Above normal (%)", "High (%)")
    } else {
      bar_titles <- paste0("Cat. ", 1:nmap, " (%)")
    }
  }

  if (plot) {
    for (k in 1:nmap) {
      s2dv::ColorBar(brks = brks, cols = cols[[k]], vertical = FALSE, subsampleg = subsampleg,
#                     bar_limits = bar_limits, var_limits = var_limits,
                     triangle_ends = triangle_ends, plot = TRUE,
                     draw_separators = draw_separators,
                     title = bar_titles[[k]], title_scale = title_scale,
                     label_scale = label_scale, extra_margin = extra_margin)
    }
  } else {
    #TODO: col_inf and col_sup
    return(list(brks = brks, cols = cols))
  }

}

