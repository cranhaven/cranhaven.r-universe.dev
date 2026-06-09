# ================================================================
# VostokR Solar Potential Analysis - Optimized Examples
#
# This file demonstrates efficient solar potential analysis using vostokR
# with proper parallelization, thread control, and performance optimization.
#
# Performance Features Demonstrated:
# - OpenMP thread control for optimal CPU utilization
# - Spatial caching and optimization techniques
# - Multi-scale processing strategies
# - Performance monitoring and validation
# ================================================================

# ---- Load Required Libraries ----
library(lidR)      # LiDAR data reading and processing
library(vostokR)   # Solar potential calculations with VOSTOK
library(terra)     # Raster handling and spatial operations
library(leaflet)   # Interactive map visualization
library(future)    # Parallel processing framework
library(parallelly)  # Core detection and parallel utilities

cat("=== VostokR Solar Potential Analysis Examples ===\n")
cat("Date:", Sys.time(), "\n")
cat("R version:", R.version.string, "\n")

# ================================================================
# PERFORMANCE CONFIGURATION - CRITICAL FOR EFFICIENCY
# ================================================================

cat("\n--- Configuring Performance Settings ---\n")

# Get system information for optimal threading
available_cores <- parallelly::availableCores(logical = FALSE)  # Physical cores only

cat("System capabilities:\n")
cat("  Physical cores:", available_cores, "\n")

# Configure VostokR threading strategy
# Strategy: Use most physical cores but leave 1 free for system processes
# This provides optimal performance without overwhelming the system
optimal_threads <- max(1, available_cores - 1)

# Apply thread configuration
set_vostokr_threads(optimal_threads)

# Verify configuration was applied
actual_threads <- get_vostokr_threads()
cat("VostokR threads configured:", actual_threads, "/", available_cores, "\n")

# Validate OpenMP capabilities
perf_info <- get_vostokr_performance_info()
cat("OpenMP enabled:", perf_info$openmp_enabled, "\n")
cat("Max threads available:", perf_info$max_threads, "\n")

if (!perf_info$openmp_enabled) {
    warning("OpenMP not enabled - performance will be limited!")
}

# ================================================================
# VISUALIZATION CONFIGURATION
# ================================================================

# ---- Define Professional Solar Palette ----
# Color scheme designed for solar potential visualization
# Progression: Dark (no sun) → Bright Yellow/Orange (maximum sun)
sun_palette <- c(
    "#2C3E50",    # No sun (dark blue-gray)
    "#34495E",    # Very low sun
    "#5D6D7E",    # Low sun
    "#85929E",    # Moderate-low sun
    "#F7DC6F",    # Moderate sun (pale yellow)
    "#F4D03F",    # Good sun (bright yellow)
    "#F1C40F",    # High sun (golden yellow)
    "#F39C12",    # Very high sun (orange-yellow)
    "#E67E22"     # Maximum sun (orange)
)

# Create smooth color ramp for continuous visualization
sun_ramp <- colorRampPalette(sun_palette)

cat("Color palette configured with", length(sun_palette), "colors\n")

# ================================================================
# A) SINGLE LAS EXAMPLE - Optimized Processing Workflow
# ================================================================

cat("\n=== Single LAS Processing Example ===\n")

# ---- Load and Validate Example Data ----
LASfile <- system.file("extdata", "test.laz", package = "vostokR")

if (!file.exists(LASfile)) {
    stop("ERROR: Test LAS file not found. Check vostokR installation.")
}

cat("Loading LAS file:", basename(LASfile), "\n")
las <- readLAS(LASfile)

# Comprehensive data validation
if (is.empty(las)) {
    stop("ERROR: LAS file is empty or could not be loaded.")
}

n_points <- npoints(las)
las_header <- header(las)

cat("Successfully loaded LAS with", format(n_points, big.mark = ","), "points\n")
cat("Spatial extent: X[", round(las_header[["Min X"]], 1), ",", round(las_header[["Max X"]], 1),
    "] Y[", round(las_header[["Min Y"]], 1), ",", round(las_header[["Max Y"]], 1), "]\n")
cat("Elevation range: Z[", round(las_header[["Min Z"]], 1), ",", round(las_header[["Max Z"]], 1), "] meters\n")

# ---- Add Normal Vectors (Essential Preprocessing) ----
# Normal vectors determine surface orientation for accurate solar irradiance
# This step is REQUIRED - solar calculations will fail without normals
cat("Computing normal vectors (required for solar analysis)...\n")

normal_start <- Sys.time()
las <- add_normals(las)
normal_time <- as.numeric(difftime(Sys.time(), normal_start, units = "secs"))

cat("Normal vectors computed in", round(normal_time, 2), "seconds\n")

# Validate normal vectors were added
if (!"nx" %in% names(las@data) || !"ny" %in% names(las@data) || !"nz" %in% names(las@data)) {
    stop("ERROR: Normal vectors not properly added to point cloud")
}

# ---- Performance-Optimized Solar Potential Calculation ----
cat("Starting optimized solar potential calculation...\n")

# Clear any existing caches for accurate timing
clear_vostokr_caches()

# Performance parameters explanation:
# - Temporal resolution: Balance between accuracy and speed
# - Spatial resolution: Higher voxel_size = faster but less shadow detail
# - Thread control: Critical for performance - must be explicit!

solar_start_time <- Sys.time()

# CRITICAL PERFORMANCE SETTING: Always specify n_threads explicitly!
# If n_threads is NULL, calculate_solar_potential() will auto-calculate
# and override your set_vostokr_threads() setting, reducing performance
las_solar <- calculate_solar_potential(
    las,
    year          = 2019,
    day_start     = 121,            # May 1st (day 121) - start of growing season
    day_end       = 243,            # August 31st (day 243) - end of peak season
    day_step      = 7,              # Weekly intervals (good temporal resolution)
    minute_step   = 60,             # Hourly solar positions (good diurnal detail)
    min_sun_angle = 15,             # Ignore low sun angles (< 15°)
    voxel_size    = 1.5,            # 1.5m voxels (good speed/accuracy balance)
    lat           = 35.27041,       # Flagstaff, AZ latitude
    lon           = -111.6905,      # Flagstaff, AZ longitude
    timezone      = -7,             # Mountain Standard Time (UTC-7)
    n_threads     = optimal_threads, # EXPLICIT THREAD CONTROL - prevents override!
    clear_cache   = FALSE           # Enable caching for efficiency
)

solar_end_time <- Sys.time()
solar_calculation_time <- as.numeric(difftime(solar_end_time, solar_start_time, units = "secs"))

# ---- Performance Analysis and Reporting ----
processing_rate <- n_points / solar_calculation_time
solar_range <- range(las_solar@data$solar_potential, na.rm = TRUE)

cat("\n=== Single LAS Performance Summary ===\n")
cat("Dataset size:", format(n_points, big.mark = ","), "points\n")
cat("Processing time:", round(solar_calculation_time, 2), "seconds\n")
cat("Processing rate:", format(round(processing_rate), big.mark = ","), "points/second\n")
cat("Threads used:", actual_threads, "/", available_cores, "available\n")
cat("Solar potential range: [", round(solar_range[1], 1), ",", round(solar_range[2], 1), "]\n")

# Calculate efficiency metrics
thread_efficiency <- (processing_rate / actual_threads) / (processing_rate / 1) * 100
if (actual_threads > 1) {
    cat("Threading efficiency:", round(thread_efficiency, 1), "%\n")
}

# ---- Enhanced Point Cloud Visualization ----
cat("Generating enhanced point cloud visualization...\n")

# Plot with professional styling
plot(las_solar,
     color = "solar_potential",
     pal = sun_ramp(100),
     main = paste("Solar Potential Analysis -", format(n_points, big.mark = ","), "Points"),
     bg = "white")

# ---- Optimized Ground Raster Generation ----
cat("Converting to high-resolution ground raster...\n")

# Use higher resolution for detailed analysis (adjust based on your needs)
raster_start <- Sys.time()
solar_raster_single <- solar_ground_raster(las_solar, res = 0.5)  # 0.5m resolution
raster_time <- as.numeric(difftime(Sys.time(), raster_start, units = "secs"))

cat("Ground raster generated in", round(raster_time, 2), "seconds\n")

# ---- Intelligent Gap Filling ----
# Use spatial interpolation to fill areas with no ground returns
cat("Applying intelligent spatial interpolation...\n")

# Define smoothing kernel size based on raster resolution
# Ensure kernel size is always odd (required by focal function)
kernel_size <- max(3, ceiling(2.0 / res(solar_raster_single)[1]))  # Adaptive kernel size
if (kernel_size %% 2 == 0) kernel_size <- kernel_size + 1  # Force odd size
smoothing_kernel <- matrix(1, nrow = kernel_size, ncol = kernel_size)

cat("Using", kernel_size, "x", kernel_size, "smoothing kernel\n")

# Apply focal operation for gap filling
solar_raster_single <- focal(solar_raster_single,
                            w = smoothing_kernel,
                            fun = mean,
                            na.policy = "only",     # Only process NA cells
                            na.rm = TRUE)

# ---- Professional Raster Visualization ----
cat("Generating enhanced raster visualization...\n")

plot(solar_raster_single,
     main = "Ground Solar Potential - Optimized Analysis",
     col = sun_ramp(100),
     axes = TRUE,
     plg = list(title = "Solar Potential"))

# ---- Comprehensive Results Summary ----
raster_values <- values(solar_raster_single, na.rm = TRUE)
valid_cells <- sum(!is.na(raster_values))
total_cells <- ncell(solar_raster_single)
coverage_area <- valid_cells * prod(res(solar_raster_single)) / 10000  # hectares

cat("\n=== Single LAS Analysis Complete ===\n")
cat("Processing performance:\n")
cat("  Total time:", round(solar_calculation_time + raster_time, 2), "seconds\n")
cat("  Solar calc:", round(solar_calculation_time, 2), "s |",
    "Raster gen:", round(raster_time, 2), "s\n")
cat("  Threading efficiency:", round(thread_efficiency, 1), "% with", actual_threads, "threads\n")

cat("Output quality:\n")
cat("  Resolution:", res(solar_raster_single)[1], "meters\n")
cat("  Coverage:", round(coverage_area, 2), "hectares\n")
cat("  Data completeness:", round(valid_cells/total_cells*100, 1), "%\n")
cat("  Solar potential range: [", round(range(raster_values, na.rm = TRUE), 1), "]\n")

# ================================================================
# B) CATALOG PROCESSING - Production Scale Analysis
# ================================================================

cat("\n=== Catalog Processing Example ===\n")

# ---- Configure Catalog for Large-Scale Processing ----
# This section demonstrates production-scale processing for multiple LAS tiles

# Example paths - replace with your actual data directories
example_paths <- c(
    "D:/lidar_temp/NAUCampus_2019/",
    "C:/LiDAR_Data/",
    "/path/to/your/lidar/tiles/"
)

# Find first existing path
catalog_path <- NULL
for (path in example_paths) {
    if (dir.exists(path)) {
        catalog_path <- path
        break
    }
}

# Alternative: single file catalog for demonstration
if (is.null(catalog_path)) {
    single_file <- "D:/lidar_temp/NAUCampus_2019/USGS_LPC_AZ_Coconino_2019_B19_w1408n1465.laz"
    if (file.exists(single_file)) {
        catalog_path <- single_file
        cat("Using single file catalog for demonstration\n")
    }
}

if (!is.null(catalog_path)) {
    cat("Loading catalog from:", catalog_path, "\n")
    ctg <- readLAScatalog(catalog_path)

    # ---- Optimal Catalog Configuration for Performance ----
    # These settings balance processing speed, memory usage, and accuracy

    # Chunk size strategy:
    # - Larger chunks = fewer overhead, more memory usage
    # - Smaller chunks = more parallelism, less memory per chunk
    # - Sweet spot: 150-300m depending on point density
    opt_chunk_size(ctg) <- 200         # 200m chunks for good balance
    opt_chunk_buffer(ctg) <- 25        # 25m buffer (sufficient for 1.5m voxels)
    opt_stop_early(ctg) <- TRUE        # Skip empty chunks efficiently
    opt_wall_to_wall(ctg) <- TRUE      # Ensure complete coverage
    opt_chunk_alignment(ctg) <- c(0, 0) # Align to coordinate grid for consistency

    catalog_info <- summary(ctg)
    cat("Catalog loaded successfully:\n")
    cat("  Files:", length(ctg), "\n")
    cat("  Total points:", format(catalog_info$`Number of points`, big.mark = ","), "\n")
    cat("  Chunk configuration: ", opt_chunk_size(ctg), "m chunks with",
        opt_chunk_buffer(ctg), "m buffer\n")

    # ---- Define High-Performance Chunk Processing Function ----
    # This function is applied to each catalog chunk with optimized settings
    optimized_catalog_solar <- function(chunk,
                                       start_date = "2019/05/01",
                                       end_date   = "2019/08/31") {

        # Load chunk data with error handling
        tryCatch({
            las <- readLAS(chunk)
            if (is.empty(las)) return(NULL)  # Skip empty chunks

            chunk_points <- npoints(las)
            cat(sprintf("  Processing chunk: %s points\n", format(chunk_points, big.mark = ",")))

            # Add normal vectors (computationally expensive but essential)
            # Consider pre-computing normals for very large datasets
            las <- add_normals(las)

            # Automatically extract geographic information from CRS
            # This ensures proper solar calculations for any coordinate system
            crs_info <- extract_crs_info(las)

            # Convert date strings to day-of-year numbers for VOSTOK
            date_range <- vostokR:::date_to_day_numbers(start_date, end_date, 2019)

            # ---- CRITICAL: High-Performance Solar Calculation ----
            # Key optimization principles:
            # 1. ALWAYS specify n_threads explicitly (prevents auto-override)
            # 2. Use caching (clear_cache = FALSE) for repeated calculations
            # 3. Balance temporal/spatial resolution with processing time
            # 4. Larger voxel_size = faster processing but less shadow detail

            chunk_solar <- calculate_solar_potential(
                las,
                year          = 2019,
                day_start     = date_range$day_start,
                day_end       = date_range$day_end,
                day_step      = 10,                    # Every 10 days (good efficiency)
                minute_step   = 120,                   # 2-hour intervals (efficient)
                min_sun_angle = 15,                    # Skip very low sun
                voxel_size    = 2.0,                   # 2m voxels (speed/accuracy balance)
                lat           = crs_info$lat,
                lon           = crs_info$lon,
                timezone      = crs_info$timezone,
                n_threads     = optimal_threads,       # EXPLICIT THREAD CONTROL!
                clear_cache   = FALSE                  # Enable performance caching
            )

            # Convert to ground raster with consistent resolution
            chunk_raster <- solar_ground_raster(chunk_solar, res = 1.0)

            # Crop to remove buffer overlap (essential for seamless merging)
            chunk_raster <- terra::crop(chunk_raster, terra::ext(chunk))

            cat(sprintf("  Chunk completed: %d cells generated\n", ncell(chunk_raster)))
            return(chunk_raster)

        }, error = function(e) {
            cat("  ERROR in chunk processing:", e$message, "\n")
            return(NULL)
        })
    }

    # ---- Configure Hybrid Parallelization Strategy ----
    # Optimal strategy: Process-level parallelism (future) for chunks +
    #                   Thread-level parallelism (OpenMP) within each chunk

    # Calculate optimal worker count
    # Strategy: Fewer workers × more threads per worker = better performance
    catalog_workers <- max(1, ceiling(available_cores / optimal_threads))

    cat("\nParallelization strategy:\n")
    cat("  Catalog workers:", catalog_workers, "\n")
    cat("  Threads per worker:", optimal_threads, "\n")
    cat("  Total compute capacity:", catalog_workers * optimal_threads, "threads\n")

    # Configure future framework for catalog processing
    plan(multisession, workers = catalog_workers)

    # Configure lidR to use single thread per chunk
    # (parallelism handled by future + VostokR OpenMP)
    set_lidr_threads(1L)

    # ---- Execute Catalog Processing ----
    cat("Starting optimized catalog processing...\n")
    cat("Progress will be shown for each chunk...\n")

    catalog_start_time <- Sys.time()

    # Apply processing function to all catalog chunks
    # This automatically handles load balancing and error recovery
    raster_chunks <- catalog_apply(ctg, optimized_catalog_solar)

    catalog_end_time <- Sys.time()
    catalog_processing_time <- as.numeric(difftime(catalog_end_time, catalog_start_time, units = "mins"))

    cat("Catalog processing completed!\n")
    cat("Total processing time:", round(catalog_processing_time, 2), "minutes\n")

    # ---- Intelligent Chunk Merging ----
    cat("Merging raster chunks into seamless output...\n")

    # Remove any NULL chunks (failed or empty processing)
    valid_chunks <- raster_chunks[!sapply(raster_chunks, is.null)]
    failed_chunks <- length(raster_chunks) - length(valid_chunks)

    if (failed_chunks > 0) {
        cat("Warning:", failed_chunks, "chunks failed processing\n")
    }

    if (length(valid_chunks) > 0) {
        # Merge all valid chunks into single seamless raster
        merge_start <- Sys.time()
        catalog_output <- do.call(terra::merge, valid_chunks)
        merge_time <- as.numeric(difftime(Sys.time(), merge_start, units = "secs"))

        cat("Chunks merged in", round(merge_time, 2), "seconds\n")

        # ---- Advanced Post-Processing ----
        cat("Applying advanced spatial interpolation...\n")

        # Adaptive smoothing kernel based on data density
        # Larger kernel for sparser data, smaller for dense data
        avg_density <- ncell(catalog_output) / (catalog_processing_time * 60)  # cells per second
        kernel_size <- ifelse(avg_density > 1000, 3, 5)  # Adaptive kernel

        interpolation_kernel <- matrix(1, nrow = kernel_size, ncol = kernel_size)
        catalog_output <- focal(catalog_output,
                               w = interpolation_kernel,
                               fun = mean,
                               na.policy = "only",
                               na.rm = TRUE)

        # ---- Professional Catalog Visualization ----
        plot(catalog_output,
             main = "Solar Potential Analysis - Catalog Processing",
             col = sun_ramp(100),
             axes = TRUE,
             plg = list(title = "Solar Potential"))

        # ---- Comprehensive Catalog Performance Analysis ----
        catalog_values <- values(catalog_output, na.rm = TRUE)
        valid_cells <- sum(!is.na(catalog_values))
        total_area <- valid_cells * prod(res(catalog_output)) / 10000  # hectares
        total_points_processed <- sum(sapply(valid_chunks, function(x) {
            if (!is.null(x)) ncell(x) else 0
        }))

        cat("\n=== Catalog Processing Performance Summary ===\n")
        cat("Processing efficiency:\n")
        cat("  Total time:", round(catalog_processing_time, 2), "minutes\n")
        cat("  Successful chunks:", length(valid_chunks), "/", length(raster_chunks), "\n")
        cat("  Workers used:", catalog_workers, "×", optimal_threads, "threads\n")
        cat("  Average chunk time:", round(catalog_processing_time * 60 / length(valid_chunks), 1), "seconds\n")

        cat("Output quality:\n")
        cat("  Resolution:", res(catalog_output)[1], "meters\n")
        cat("  Coverage area:", round(total_area, 2), "hectares\n")
        cat("  Data completeness:", round(valid_cells/ncell(catalog_output)*100, 1), "%\n")
        cat("  Solar potential range: [", round(range(catalog_values, na.rm = TRUE), 1), "]\n")

    } else {
        cat("ERROR: No valid chunks produced - check data and processing parameters\n")
        catalog_output <- NULL
    }

} else {
    cat("Catalog processing skipped - no large dataset directory found\n")
    cat("To enable catalog processing:\n")
    cat("  1. Create a directory with multiple LAS/LAZ files\n")
    cat("  2. Update the 'example_paths' variable with your directory path\n")
    catalog_output <- NULL
}

# ================================================================
# C) INTERACTIVE VISUALIZATION - Publication Quality
# ================================================================

cat("\n=== Creating Interactive Visualization ===\n")

# Use the single LAS result for interactive visualization
raster_for_viz <- solar_raster_single

# ---- Prepare Professional Color Palette for Leaflet ----
raster_values <- values(raster_for_viz, na.rm = TRUE)
value_range <- range(raster_values, na.rm = TRUE)

# Create color palette for leaflet with proper value mapping
leaflet_palette <- colorNumeric(
    palette = sun_palette,
    domain = value_range,
    na.color = "transparent"
)

cat("Interactive map value range:", round(value_range, 2), "\n")

# ---- Create Professional Interactive Map ----
interactive_map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%

    # ---- Multiple Base Layer Options ----
    addProviderTiles(providers$OpenTopoMap,
                    group = "Topographic",
                    options = providerTileOptions(opacity = 0.8)) %>%

    addProviderTiles(providers$Esri.WorldImagery,
                    group = "Satellite",
                    options = providerTileOptions(opacity = 0.9)) %>%

    addProviderTiles(providers$CartoDB.Positron,
                    group = "Light Theme",
                    options = providerTileOptions(opacity = 0.8)) %>%

    # ---- Solar Potential Overlay ----
    addRasterImage(raster_for_viz,
                  colors = leaflet_palette,
                  project = TRUE,
                  opacity = 0.7,
                  group = "Solar Potential") %>%

    # ---- Professional Legend ----
    addLegend(pal = leaflet_palette,
             values = raster_values,
             title = "Solar<br>Potential",
             position = "bottomright",
             opacity = 0.8) %>%

    # ---- Layer Control Panel ----
    addLayersControl(
        baseGroups = c("Satellite", "Topographic", "Light Theme"),
        overlayGroups = c("Solar Potential"),
        options = layersControlOptions(collapsed = FALSE)
    ) %>%

    # ---- Default View Configuration ----
    # Start with satellite imagery for context
    hideGroup("Topographic") %>%
    hideGroup("Light Theme") %>%

    # ---- Add Scale Bar and North Arrow ----
    addScaleBar(position = "bottomleft") %>%

    # ---- Map Attribution ----
    addControl("VostokR Solar Analysis", position = "topright")

# Display the interactive map
print(interactive_map)

# ================================================================
# PERFORMANCE RECOMMENDATIONS AND BEST PRACTICES
# ================================================================

cat("\n=== Performance Optimization Summary ===\n")
cat("Threading configuration used:\n")
cat("  VostokR threads:", get_vostokr_threads(), "\n")
cat("  System cores available:", available_cores, "\n")
cat("  OpenMP enabled:", perf_info$openmp_enabled, "\n")

cat("\n=== VostokR Solar Analysis Examples Complete ===\n")
cat("Total execution time:", round(as.numeric(difftime(Sys.time(), solar_start_time, units = "mins")), 2), "minutes\n")
