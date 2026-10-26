# R/utils_morph.R
# Helpers for pixel-level morphing (exact / color_walk / auto)

# -------------------------------------------------------------------
# Basic helpers
# -------------------------------------------------------------------

#' @noRd
.has_namespace <- function(pkg) requireNamespace(pkg, quietly = TRUE)

# -------------------------------------------------------------------
# Image <-> array conversions
# -------------------------------------------------------------------

#' Convert magick image to H x W x 3 integer array in 0-255
#' @noRd
.to_array_rgb <- function(img) {
  a <- magick::image_data(img, channels = "rgb")
  d <- dim(a)
  if (length(d) != 3) stop("magick::image_data did not return a 3D array")

  # Convert to integer 0..255
  if (is.character(a)) {
    a <- array(as.integer(strtoi(a, base = 16L)), dim = d)
  } else if (is.raw(a)) {
    storage.mode(a) <- "integer"
  } else if (is.numeric(a)) {
    mx <- suppressWarnings(max(a, na.rm = TRUE))
    if (is.finite(mx) && mx <= 1.00001) a <- a * 255
    a <- round(a); storage.mode(a) <- "integer"
  } else {
    storage.mode(a) <- "integer"
  }

  # magick returns [channel, width, height] = [3, W, H]
  if (d[1] == 3 && d[3] != 3) {
    W <- d[2]; H <- d[3]
    out <- array(0L, dim = c(H, W, 3L))
    for (ch in 1:3) out[,,ch] <- t(a[ch,,])
  } else if (d[3] == 3) {
    out <- a
  } else if (d[2] == 3) {
    W <- d[1]; H <- d[3]
    out <- array(0L, dim = c(H, W, 3L))
    for (ch in 1:3) out[,,ch] <- t(a[,ch,])
  } else {
    stop("Unexpected RGB array dims: ", paste(d, collapse = " x "))
  }
  storage.mode(out) <- "integer"
  out
}

#' Convert H x W x 3 array to planar format (RRR...GGG...BBB...)
#' @noRd
.to_planar_rgb <- function(rgb_arr) {
  H <- dim(rgb_arr)[1]; W <- dim(rgb_arr)[2]
  planar <- numeric(H * W * 3L)
  for (ch in 1:3) {
    offset <- (ch - 1L) * H * W
    planar[(offset + 1L):(offset + H * W)] <- as.vector(rgb_arr[,,ch])
  }
  planar
}

# -------------------------------------------------------------------
# C++ glue (robust wrappers: prefer new _symbols; fallback to *cpp names)
# -------------------------------------------------------------------

.call_or <- function(sym, fallback, ...) {
  if (exists(sym, mode = "function")) {
    get(sym)(...)
  } else if (exists(fallback, mode = "function")) {
    get(fallback)(...)
  } else {
    stop("Neither ", sym, " nor ", fallback, " is available")
  }
}

#' @noRd
.cpp_palette_info <- function(Ap, Bp, H, W, bits) {
  .call_or("_color_palette_info", "color_palette_info_cpp",
           Ap, Bp, as.integer(H), as.integer(W), as.integer(bits))
}

#' @noRd
.cpp_spatial_cost <- function(idxA, idxB, H, W) {
  .call_or("_spatial_cost_matrix", "spatial_cost_matrix_cpp",
           as.integer(idxA), as.integer(idxB), as.integer(H), as.integer(W))
}

# RGB distance between two pixel sets, on the 0-1 scale the color walk prices
# in: one n_a x n_b table per channel, summed, rooted. This is the color walk's
# hot path and both of its call sites build the same matrix.
#' @noRd
.rgb_cost_matrix <- function(A_rgb, idxA, B_rgb, idxB) {
  d2 <- matrix(0, nrow = length(idxA), ncol = length(idxB))
  for (ch in 1:3) {
    d <- outer(A_rgb[idxA, ch], B_rgb[idxB, ch], `-`) / 255
    d2 <- d2 + d * d
  }
  sqrt(d2)
}

#' @noRd
.cpp_compute_pixel_cost <- function(Ap, Bp, H, W, alpha, beta) {
  .call_or("_compute_pixel_cost", "compute_pixel_cost_cpp",
           Ap, Bp, as.integer(H), as.integer(W), as.numeric(alpha), as.numeric(beta))
}

#' @noRd
.cpp_downscale <- function(planar, H, W, Hn, Wn) {
  .call_or("_downscale_image", "downscale_image_cpp",
           planar, as.integer(H), as.integer(W), as.integer(Hn), as.integer(Wn))
}

#' @noRd
.cpp_upscale_assignment <- function(asg_scaled, H, W, Hs, Ws) {
  .call_or("_upscale_assignment", "upscale_assignment_cpp",
           as.integer(asg_scaled), as.integer(H), as.integer(W), as.integer(Hs), as.integer(Ws))
}

#' @noRd
.cpp_render_morph <- function(Ap, Bp, asg0, H, W, nF) {
  .call_or("_morph_pixel_level_impl", "morph_pixel_level_cpp",
           Ap, Bp, as.integer(asg0), as.integer(H), as.integer(W), as.integer(nF))
}

# -------------------------------------------------------------------
# Downscale helpers for orchestration
# -------------------------------------------------------------------

#' Compute downscaled images, return both originals and downscaled with dims
#' @noRd
.downscale_both <- function(A_planar, B_planar, H, W, steps) {
  if (is.null(steps) || steps <= 0L) {
    return(list(Hs = as.integer(H), Ws = as.integer(W),
                A_s = A_planar, B_s = B_planar,
                H  = as.integer(H), W  = as.integer(W)))
  }
  Hs <- as.integer(max(8L, floor(H / (2^steps))))
  Ws <- as.integer(max(8L, floor(W / (2^steps))))
  A_s <- .cpp_downscale(A_planar, H, W, Hs, Ws)
  B_s <- .cpp_downscale(B_planar, H, W, Hs, Ws)
  list(Hs = Hs, Ws = Ws, A_s = A_s, B_s = B_s, H = as.integer(H), W = as.integer(W))
}

#' Upscale a downscaled assignment back to original resolution
#' @noRd
.upscale_assignment <- function(assign_s, H, W, Hs, Ws) {
  .cpp_upscale_assignment(assign_s, H, W, Hs, Ws)
}

# -------------------------------------------------------------------
# LAP glue -- normalize outputs
# -------------------------------------------------------------------

#' Solve LAP and return a consistent **0-based** column index per row
#' @noRd
.lap_assign <- function(C, method = "jv", maximize = FALSE) {
  if (!exists("lap_solve", mode = "function") && !exists("lap_solve_batch", mode = "function"))
    stop("No lap_solve / lap_solve_batch available")

  res <- if (exists("lap_solve", mode = "function")) {
    lap_solve(C, method = method, maximize = maximize)
  } else {
    lap_solve_batch(list(C), method = method, maximize = maximize)[[1L]]
  }

  n <- nrow(C)
  # Direct integer vector?
  if (is.integer(res) && length(res) == n) return(res - 1L)     # assume 1-based -> 0-based
  if (is.numeric(res) && length(res) == n) return(as.integer(round(res)) - 1L)

  # tibble/data.frame with source/target
  if (is.data.frame(res) && all(c("source","target") %in% names(res))) {
    perm <- integer(n); perm[res$source] <- res$target
    perm[perm == 0L] <- seq_len(n)[perm == 0L]
    return(as.integer(perm - 1L))
  }

  # common list shapes
  if (is.list(res)) {
    for (nm in c("assignment","perm","match")) {
      if (!is.null(res[[nm]]) && length(res[[nm]]) == n) {
        v <- res[[nm]]
        v <- if (is.integer(v)) v else as.integer(round(v))
        return(v - 1L)
      }
    }
  }
  stop("LAP solver returned an unsupported structure")
}

# -------------------------------------------------------------------
# Patch helpers
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Exact / Patch solves (R orchestrates LAP; C++ only builds costs)
# -------------------------------------------------------------------

#' Exact pixel-level: returns **1-based** assignment (A->B)
#' @noRd
.exact_cost_and_solve <- function(A_planar, B_planar, H, W, alpha = 1, beta = 0,
                                  method = "jv", maximize = FALSE) {
  C <- .cpp_compute_pixel_cost(A_planar, B_planar, H, W, alpha, beta)
  .lap_assign(C, method = method, maximize = maximize) + 1L
}

# -------------------------------------------------------------------
# Palette pipelines
# -------------------------------------------------------------------

# Build spatial assignments for palette pairs (returns index pairs)
.build_spatial_assignments_for_pairs <- function(info, pairs, H, W, method = "jv", maximize = FALSE) {
  if (nrow(pairs) == 0L) return(list(i_idx = integer(), j_idx = integer()))
  groupsA <- info$groupsA; groupsB <- info$groupsB
  i_all <- integer(0); j_all <- integer(0)
  for (r in seq_len(nrow(pairs))) {
    ia <- pairs$ia[[r]]; ib <- pairs$ib[[r]]; k <- pairs$k[[r]]
    if (k <= 0L) next
    idxA <- as.integer(groupsA[[ia]]); idxB <- as.integer(groupsB[[ib]])
    if (!length(idxA) || !length(idxB)) next
    idxA <- idxA[seq_len(min(k, length(idxA)))]
    idxB <- idxB[seq_len(min(k, length(idxB)))]
    Csp  <- .cpp_spatial_cost(idxA, idxB, H, W)
    perm <- .lap_assign(Csp, method = method, maximize = maximize)
    i_all <- c(i_all, idxA); j_all <- c(j_all, idxB[perm + 1L])
  }
  list(i_idx = i_all, j_idx = j_all)
}

# Assemble assignment from (i,j) pairs (1-based target); unfilled remain -1
.assemble_assignment <- function(N, i_idx, j_idx) {
  assign <- rep.int(-1L, N)
  if (length(i_idx) && length(j_idx)) {
    take <- min(length(i_idx), length(j_idx))
    if (take > 0L) assign[i_idx[seq_len(take)]] <- as.integer(j_idx[seq_len(take)])
  }
  assign
}

# Identity fill helper
.fill_unassigned_identity <- function(assign) {
  N <- length(assign)
  z <- which(assign < 0L)
  if (length(z)) assign[z] <- z
  assign
}

# Exact-identity palette pairs (quantized equality)
.palette_pairs_identity <- function(info) {
  A <- info$colorsA_rgb; B <- info$colorsB_rgb
  cA <- as.integer(info$countsA); cB <- as.integer(info$countsB)
  keyA <- paste(A[,1], A[,2], A[,3], sep = "_")
  keyB <- paste(B[,1], B[,2], B[,3], sep = "_")
  mapB <- seq_along(keyB); names(mapB) <- keyB
  ia_vec <- integer(0); ib_vec <- integer(0); k_vec <- integer(0)
  for (ia in seq_along(keyA)) {
    key <- keyA[[ia]]
    if (key %in% names(mapB)) {
      ib <- mapB[[key]]
      ia_vec <- c(ia_vec, ia)
      ib_vec <- c(ib_vec, ib)
      k_vec  <- c(k_vec, min(cA[[ia]], cB[[ib]]))
    }
  }
  if (!length(ia_vec)) return(data.frame(ia = integer(), ib = integer(), k = integer()))
  data.frame(ia = ia_vec, ib = ib_vec, k = k_vec)
}

# Color walk: process A colors in fixed order, match to nearest free B pixels by pure color distance
.solve_color_walk_pipeline <- function(A_planar, B_planar, H, W, quantize_bits = 5,
                                       method = "jv", maximize = FALSE) {
  info <- .cpp_palette_info(A_planar, B_planar, H, W, quantize_bits)
  N <- H * W
  
  # Extract full-resolution RGB for all pixels (0-255 range)
  A_rgb <- matrix(A_planar, nrow = N, ncol = 3)  # N x 3
  B_rgb <- matrix(B_planar, nrow = N, ncol = 3)  # N x 3
  
  # Sort A color groups by frequency (descending) for deterministic processing
  groupsA <- info$groupsA
  countsA <- info$countsA
  color_order <- order(countsA, decreasing = TRUE)
  
  # Initialize assignment and free set
  assignment <- rep(NA_integer_, N)  # will store 1-based B indices
  freeB <- rep(TRUE, N)
  
  # Process each A color group in order
  for (ia in color_order) {
    idxA <- as.integer(groupsA[[ia]])  # 1-based indices
    if (!length(idxA)) next
    
    # Get currently free B pixels
    idxB_free <- which(freeB)  # 1-based indices
    if (!length(idxB_free)) break  # no more free B pixels
    
    nA <- length(idxA)
    nB <- length(idxB_free)
    
    # Warn if this color group is very large (memory could be an issue)
    # Use as.numeric to avoid integer overflow in multiplication
    matrix_size <- as.numeric(nA) * as.numeric(nB)
    
    if (matrix_size > 1e8) {  # ~100M entries = ~800MB
      warning(sprintf(
        "Large color group: %d A pixels x %d B pixels. Using spatial fallback to avoid memory issues.",
        nA, nB
      ), call. = FALSE)
      # Use spatial matching as fallback for huge groups
      Csp <- .cpp_spatial_cost(idxA, idxB_free, H, W)
      match <- .lap_assign(Csp, method = method, maximize = FALSE)
    } else {
      # Normal color-based matching
      C_color <- .rgb_cost_matrix(A_rgb, idxA, B_rgb, idxB_free)
      match <- .lap_assign(C_color, method = method, maximize = FALSE)
    }
    
    # Apply assignments
    take <- min(nA, nB)
    if (take > 0) {
      for (i in seq_len(take)) {
        a_idx <- idxA[i]
        b_idx <- idxB_free[match[i] + 1L]
        assignment[a_idx] <- b_idx
        freeB[b_idx] <- FALSE
      }
    }
  }
  
  # Handle any remaining unassigned A pixels (shouldn't happen with equal sizes, but be safe)
  remainA <- which(is.na(assignment))
  if (length(remainA) && any(freeB)) {
    idxB_free <- which(freeB)
    nA <- length(remainA)
    nB <- length(idxB_free)
    C_color <- .rgb_cost_matrix(A_rgb, remainA, B_rgb, idxB_free)
    match <- .lap_assign(C_color, method = method, maximize = FALSE)
    for (i in seq_along(remainA)) {
      assignment[remainA[i]] <- idxB_free[match[i] + 1L]
    }
  }
  
  # Final fallback: any still unassigned -> identity
  still_na <- which(is.na(assignment))
  if (length(still_na)) assignment[still_na] <- still_na
  
  as.integer(assignment)  # return 1-based indices
}

# Identity palette pipeline
.solve_color_match_pipeline <- function(A_planar, B_planar, H, W, quantize_bits = 5,
                                        method = "jv", maximize = FALSE,
                                        fill_identity_for_unmatched = TRUE) {
  info  <- .cpp_palette_info(A_planar, B_planar, H, W, quantize_bits)
  pairs <- .palette_pairs_identity(info)
  pj    <- .build_spatial_assignments_for_pairs(info, pairs, H, W, method = method, maximize = maximize)
  assign <- .assemble_assignment(N = H * W, pj$i_idx, pj$j_idx)
  if (fill_identity_for_unmatched) assign <- .fill_unassigned_identity(assign)
  assign
}
