# Speed Comparison: Color Walk vs Exact
# Tests the performance of different morphing modes

message("== Speed Comparison for lapr morphing modes ==\n")

suppressPackageStartupMessages({
  library(magick)
  library(png)
})

# Assume lapr functions are available
# If running standalone, you'll need to source utils_morph.R and have C++ compiled

# Create synthetic test images of different sizes
create_test_pair <- function(size) {
  H <- W <- size
  arrA <- array(0, c(H, W, 3))
  arrB <- array(0, c(H, W, 3))
  
  # Create varied color patterns
  for (y in 1:H) {
    for (x in 1:W) {
      # Image A: radial gradient with some structure
      r <- sqrt((x - H/2)^2 + (y - W/2)^2) / (H/2)
      arrA[y, x, 1] <- (1 - r) * 255
      arrA[y, x, 2] <- (x / W) * 255
      arrA[y, x, 3] <- (y / H) * 255
      
      # Image B: different pattern
      arrB[y, x, 1] <- (y / H) * 255
      arrB[y, x, 2] <- (1 - r) * 255
      arrB[y, x, 3] <- (x / W) * 255
    }
  }
  
  tmpA <- tempfile(fileext = ".png")
  tmpB <- tempfile(fileext = ".png")
  writePNG(arrA / 255, tmpA)
  writePNG(arrB / 255, tmpB)
  list(A = tmpA, B = tmpB)
}

# Test configurations
test_configs <- list(
  list(mode = "exact",      args = list(alpha = 1, beta = 0, patch_size = 1L), label = "exact_β=0"),
  list(mode = "exact",      args = list(alpha = 1, beta = 0.05, patch_size = 3L), label = "exact_patch3"),
  list(mode = "color_walk", args = list(quantize_bits = 6L), label = "color_walk_q6"),
  list(mode = "color_walk", args = list(quantize_bits = 5L), label = "color_walk_q5"),
  list(mode = "color_walk", args = list(quantize_bits = 7L), label = "color_walk_q7")
)

# Image sizes to test (keep under 100×100 for exact mode - limit is 10,000 pixels)
sizes <- c(16, 32, 64, 96)

# Results storage
results <- list()

cat("\n")
cat(sprintf("%-10s %-20s %12s %12s %15s\n", 
            "Size", "Mode", "Time (sec)", "Pixels", "μs/pixel"))
cat(strrep("-", 70), "\n")

for (size in sizes) {
  # Create test images
  pair <- create_test_pair(size)
  n_pixels <- size * size
  
  for (cfg in test_configs) {
    mode_label <- if (!is.null(cfg$label)) cfg$label else cfg$mode
    
    # Time the morphing operation
    timing <- system.time({
      tryCatch({
        do.call(lapr::pixel_morph, c(list(
          imgA = pair$A,
          imgB = pair$B,
          n_frames = 1L,  # Just test assignment computation
          fps = 1L,
          format = "gif",
          outfile = tempfile(fileext = ".gif"),
          show = FALSE,
          lap_method = "jv",
          maximize = FALSE,
          downscale_steps = 0L
        ), list(mode = cfg$mode), cfg$args))
      }, error = function(e) {
        message("Error for ", mode_label, " at size ", size, ": ", e$message)
      })
    })
    
    elapsed <- timing["elapsed"]
    us_per_pixel <- (elapsed * 1e6) / n_pixels
    
    cat(sprintf("%-10s %-20s %12.4f %12d %15.2f\n", 
                paste0(size, "×", size), 
                mode_label, 
                elapsed, 
                n_pixels,
                us_per_pixel))
    
    results[[length(results) + 1]] <- list(
      size = size,
      mode = cfg$mode,
      label = mode_label,
      time_sec = elapsed,
      n_pixels = n_pixels,
      us_per_pixel = us_per_pixel
    )
  }
  
  cat("\n")
}

# Summary statistics
cat("\n=== SUMMARY ===\n\n")

# Convert to data frame for easier analysis
df <- do.call(rbind, lapply(results, function(x) {
  data.frame(
    size = x$size,
    mode = x$mode,
    label = x$label,
    time_sec = x$time_sec,
    n_pixels = x$n_pixels,
    us_per_pixel = x$us_per_pixel,
    stringsAsFactors = FALSE
  )
}))

# Average time per mode across all sizes
cat("Average time per mode (across all sizes):\n")
for (mode_label in unique(df$label)) {
  subset_df <- df[df$label == mode_label, ]
  avg_time <- mean(subset_df$time_sec)
  avg_us <- mean(subset_df$us_per_pixel)
  cat(sprintf("  %-20s: %.4f sec (%.2f μs/pixel)\n", mode_label, avg_time, avg_us))
}

cat("\n")

# Scaling analysis (how does time grow with N?)
cat("Scaling behavior (time vs N):\n")
for (mode_label in unique(df$label)) {
  subset_df <- df[df$label == mode_label, ]
  if (nrow(subset_df) >= 2) {
    # Simple ratio: time at largest / time at smallest
    times <- subset_df$time_sec[order(subset_df$size)]
    sizes <- subset_df$size[order(subset_df$size)]
    if (times[1] > 0) {
      size_ratio <- sizes[length(sizes)] / sizes[1]
      time_ratio <- times[length(times)] / times[1]
      # Estimate exponent: time ~ N^k, so k = log(time_ratio) / log(size_ratio^2)
      k <- log(time_ratio) / log(size_ratio^2)
      cat(sprintf("  %-20s: ~O(N^%.2f)  [%d×%d is %.1fx slower than %d×%d]\n", 
                  mode_label, k,
                  sizes[length(sizes)], sizes[length(sizes)],
                  time_ratio,
                  sizes[1], sizes[1]))
    }
  }
}

cat("\n")
cat("Note: Exact modes should be ~O(N^2.5-3) due to LAP solving on N×N matrix\n")
cat("      Color walk should be ~O(K*M^2.5) where K=num_colors, M=avg_pixels_per_color\n")
cat("      With good quantization, K << N, making color_walk much faster for large N\n")
