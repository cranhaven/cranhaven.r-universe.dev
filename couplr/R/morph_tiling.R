# R/square_tiling_morph.R
# Efficient square-tiling local LAP implementation for morphing
# This file contains helper functions for pixel_morph when patch_size > 1

# Note: These are internal functions (start with .) so they don't need @export

# -------------------------------------------------------------------
# Square Tiling Generation
# -------------------------------------------------------------------

#' Generate deterministic square tiles for an image
#' 
#' Creates a tiling structure with:
#' - Bulk region filled with P×P tiles (top-left anchored)
#' - Right strip with largest squares <= remaining width
#' - Bottom strip with largest squares <= remaining height
#' 
#' @param W Image width
#' @param H Image height
#' @param P Maximum tile size (default 3)
#' @return List of tiles, each with (x0, y0, size)
#' @noRd
.generate_square_tiles <- function(W, H, P = 3L) {
  tiles <- list()
  covered <- matrix(FALSE, nrow = H, ncol = W)

  # Handle edge case: P larger than smallest image dimension
  # In this case, use 1x1 tiles (each pixel is its own tile)
  if (P > min(W, H)) {
    tiles <- list()
    for (x0 in seq(0, W - 1)) {
      for (y0 in seq(0, H - 1)) {
        tiles[[length(tiles) + 1L]] <- list(x0 = x0, y0 = y0, size = 1L)
      }
    }
    return(tiles)
  }

  # 1. Bulk region - fill with P×P tiles
  core_w <- (W %/% P) * P
  core_h <- (H %/% P) * P

  # Skip bulk if core dimensions are 0
  if (core_w >= P && core_h >= P) {
    for (x0 in seq(0, core_w - P, by = P)) {
    for (y0 in seq(0, core_h - P, by = P)) {
      tiles[[length(tiles) + 1L]] <- list(
        x0 = x0,
        y0 = y0, 
        size = P
      )
      # Mark as covered
      for (dx in 0:(P-1)) {
        for (dy in 0:(P-1)) {
          covered[y0 + dy + 1L, x0 + dx + 1L] <- TRUE
        }
      }
    }
  }
  }  # End of core_w >= P && core_h >= P check

  # 2. Right strip - tile with largest squares possible
  if (core_w < W) {
    remaining_width <- W - core_w
    x0 <- core_w
    
    for (y0 in seq(0, core_h - 1, by = 1)) {
      if (covered[y0 + 1L, x0 + 1L]) next
      
      # Find largest square that fits
      max_size <- min(remaining_width, core_h - y0)
      for (size in min(P, max_size):1L) {
        if (y0 + size <= core_h && x0 + size <= W) {
          # Check if all cells are free
          all_free <- TRUE
          for (dx in 0:(size-1)) {
            for (dy in 0:(size-1)) {
              if (covered[y0 + dy + 1L, x0 + dx + 1L]) {
                all_free <- FALSE
                break
              }
            }
            if (!all_free) break
          }
          
          if (all_free) {
            tiles[[length(tiles) + 1L]] <- list(
              x0 = x0,
              y0 = y0,
              size = size
            )
            # Mark as covered
            for (dx in 0:(size-1)) {
              for (dy in 0:(size-1)) {
                covered[y0 + dy + 1L, x0 + dx + 1L] <- TRUE
              }
            }
            break
          }
        }
      }
    }
  }
  
  # 3. Bottom strip - tile with largest squares possible  
  if (core_h < H) {
    remaining_height <- H - core_h
    y0 <- core_h
    
    for (x0 in seq(0, W - 1, by = 1)) {
      if (covered[y0 + 1L, x0 + 1L]) next
      
      # Find largest square that fits
      max_size <- min(remaining_height, W - x0)
      for (size in min(P, max_size):1L) {
        if (x0 + size <= W && y0 + size <= H) {
          # Check if all cells are free
          all_free <- TRUE
          for (dx in 0:(size-1)) {
            for (dy in 0:(size-1)) {
              if (covered[y0 + dy + 1L, x0 + dx + 1L]) {
                all_free <- FALSE
                break
              }
            }
            if (!all_free) break
          }
          
          if (all_free) {
            tiles[[length(tiles) + 1L]] <- list(
              x0 = x0,
              y0 = y0,
              size = size
            )
            # Mark as covered
            for (dx in 0:(size-1)) {
              for (dy in 0:(size-1)) {
                covered[y0 + dy + 1L, x0 + dx + 1L] <- TRUE
              }
            }
            break
          }
        }
      }
    }
  }
  
  # 4. Any remaining uncovered pixels become 1×1 tiles
  for (y in 0:(H-1)) {
    for (x in 0:(W-1)) {
      if (!covered[y + 1L, x + 1L]) {
        tiles[[length(tiles) + 1L]] <- list(
          x0 = x,
          y0 = y,
          size = 1L
        )
      }
    }
  }
  
  tiles
}

# -------------------------------------------------------------------
# Local LAP Solver
# -------------------------------------------------------------------

#' Solve local LAP for a single tile
#' 
#' Computes pixel indices for the tile, builds local cost matrix,
#' solves LAP, and returns assignments.
#' 
#' @param tile List with (x0, y0, size)
#' @param A_planar Planar RGB data for image A
#' @param B_planar Planar RGB data for image B
#' @param H Image height
#' @param W Image width
#' @param alpha Color weight (default 1)
#' @param beta Spatial weight (default 0.1)
#' @param method LAP method (default "jv")
#' @return Integer vector of B indices for each A pixel in tile (1-based)
#' @noRd
.solve_tile_lap <- function(tile, A_planar, B_planar, H, W, 
                            alpha = 1, beta = 0.1, method = "jv") {
  x0 <- tile$x0
  y0 <- tile$y0
  size <- tile$size
  N <- H * W
  
  # Get pixel indices for this tile (R column-major: i = x*H + y + 1)
  indices <- integer(size * size)
  idx_count <- 0L
  for (dx in 0:(size-1)) {
    for (dy in 0:(size-1)) {
      x <- x0 + dx
      y <- y0 + dy
      idx_count <- idx_count + 1L
      indices[idx_count] <- x * H + y + 1L  # R column-major, 1-based
    }
  }
  
  # For 1×1 tiles, no LAP needed - direct assignment
  if (size == 1L) {
    return(indices)
  }
  
  # Extract colors for pixels in this tile
  n_pixels <- length(indices)
  colors_A <- matrix(0, nrow = n_pixels, ncol = 3)
  colors_B <- matrix(0, nrow = n_pixels, ncol = 3)
  
  for (i in seq_len(n_pixels)) {
    idx <- indices[i]
    colors_A[i, 1] <- A_planar[idx] / 255
    colors_A[i, 2] <- A_planar[idx + N] / 255
    colors_A[i, 3] <- A_planar[idx + 2*N] / 255
    colors_B[i, 1] <- B_planar[idx] / 255
    colors_B[i, 2] <- B_planar[idx + N] / 255
    colors_B[i, 3] <- B_planar[idx + 2*N] / 255
  }
  
  # Build local cost matrix
  # Color distance
  Cc <- as.matrix(stats::dist(rbind(colors_A, colors_B)))[1:n_pixels, (n_pixels+1):(2*n_pixels)]
  
  # Spatial distance (within tile, normalized by tile size)
  if (beta > 0) {
    coords <- matrix(0, nrow = n_pixels, ncol = 2)
    for (i in seq_len(n_pixels)) {
      idx <- indices[i]
      y_coord <- (idx - 1L) %% H
      x_coord <- (idx - 1L) %/% H
      coords[i, 1] <- x_coord
      coords[i, 2] <- y_coord
    }
    Cs <- as.matrix(stats::dist(coords)) / (size * sqrt(2))  # Normalize by tile diagonal
    Cs <- Cs[1:n_pixels, 1:n_pixels]  # Same indices map to same indices in B tile
  } else {
    Cs <- matrix(0, nrow = n_pixels, ncol = n_pixels)
  }
  
  # Combined cost
  C <- alpha * Cc + beta * Cs
  
  # Solve LAP (assuming .lap_assign is available from original code)
  if (!exists(".lap_assign")) {
    # Fallback to basic Hungarian if .lap_assign not available
    # This would need the actual LAP solver
    stop("LAP solver .lap_assign not found")
  }
  
  perm <- .lap_assign(C, method = method, maximize = FALSE) + 1L
  
  # Return B indices for each A pixel
  indices[perm]
}

# -------------------------------------------------------------------
# Main Square Tiling Solver
# -------------------------------------------------------------------

#' Solve morphing using square tiling with local LAPs
#' 
#' Replaces the hierarchical patch pipeline with efficient local solving.
#' Each tile is solved independently with a small LAP.
#' 
#' @param A_planar Planar RGB data for image A
#' @param B_planar Planar RGB data for image B
#' @param H Image height
#' @param W Image width
#' @param max_tile_size Maximum tile size (default 3)
#' @param alpha Color weight (default 1)
#' @param beta Spatial weight (default 0.1)
#' @param method LAP method (default "jv")
#' @param maximize Whether to maximize (default FALSE)
#' @return Integer vector of assignments (1-based B indices for each A pixel)
#' @noRd
.square_tiling_solver <- function(A_planar, B_planar, H, W,
                                 max_tile_size = 3L,
                                 alpha = 1, beta = 0.1,
                                 method = "jv", maximize = FALSE) {
  N <- H * W
  
  # Generate deterministic square tiles
  tiles <- .generate_square_tiles(W, H, P = max_tile_size)
  
  # Initialize global assignment vector
  assignment <- rep(NA_integer_, N)
  
  # Process each tile with local LAP
  for (tile in tiles) {
    # Get A pixel indices for this tile
    x0 <- tile$x0
    y0 <- tile$y0
    size <- tile$size
    
    indices_A <- integer(size * size)
    idx_count <- 0L
    for (dx in 0:(size-1)) {
      for (dy in 0:(size-1)) {
        x <- x0 + dx
        y <- y0 + dy
        idx_count <- idx_count + 1L
        indices_A[idx_count] <- x * H + y + 1L  # R column-major, 1-based
      }
    }
    
    # Solve local LAP for this tile
    indices_B <- .solve_tile_lap(tile, A_planar, B_planar, H, W, 
                                 alpha, beta, method)
    
    # Write assignments to global vector
    for (i in seq_along(indices_A)) {
      assignment[indices_A[i]] <- indices_B[i]
    }
  }
  
  # Sanity check - fill any remaining with identity
  unassigned <- which(is.na(assignment))
  if (length(unassigned) > 0) {
    warning(sprintf("Found %d unassigned pixels, filling with identity", 
                   length(unassigned)))
    assignment[unassigned] <- unassigned
  }
  
  as.integer(as.vector(assignment))
}

# -------------------------------------------------------------------
# Integration Functions
# -------------------------------------------------------------------
