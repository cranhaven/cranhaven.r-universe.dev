// src/morph_pixel_level.cpp
// Pixel morphing helpers (no in-C++ LAP):
// - color_palette_info(...)      : palette groups + palette-level color distances
// - spatial_cost_matrix(...)     : spatial distance matrix for two pixel index sets
// - compute_pixel_cost(...)      : full exact H*W x H*W pixel cost (color+spatial)
// - downscale_image(...), upscale_assignment(...)
// - analyze_color_overlap(...)
// - morph_pixel_level_impl(...)  : renderer given a final assignment
//
// LAP selection/solving is done in R (same solver for color and spatial).
//
// NOTE: Color distances are normalized to [0..sqrt(3)] by scaling channels to [0,1].
//       Spatial distances are normalized to [0..1] by dividing by the image diagonal.
//       This makes alpha/beta commensurate: alpha=1, beta in [0,1] behaves as expected.
//

#include "../core/lap_utils_rcpp.h"
// INDEXING CONVENTION: All functions use COLUMN-MAJOR indexing to match R's planar data layout.
//       For an image with height H and width W:
//       - Linear index: i = x*H + y
//       - Coordinates from index: y = i % H, x = i / H
//       This matches R's as.vector() behavior on [H,W,C] arrays.

#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>
#include <unordered_map>
#include <map>
#include <cstdint>

using namespace Rcpp;

// -----------------------------------------------------------------------------
// COLOR KEY STRUCTURES
// -----------------------------------------------------------------------------

struct ColorKey {
  uint32_t key;
  ColorKey() : key(0) {}
  ColorKey(uint8_t r, uint8_t g, uint8_t b)
    : key((uint32_t(r) << 16) | (uint32_t(g) << 8) | uint32_t(b)) {}
  bool operator==(const ColorKey& other) const { return key == other.key; }
  bool operator<(const ColorKey& other) const { return key < other.key; }
};

struct ColorKeyHash {
  std::size_t operator()(const ColorKey& k) const {
    return std::hash<uint32_t>()(k.key);
  }
};

inline ColorKey make_color_key(double r, double g, double b, int bits_per_channel) {
  const int max_q = (1 << bits_per_channel) - 1;

  auto q255 = [&](double v) {
    // clamp to [0,255]
    double vc = std::max(0.0, std::min(255.0, v));
    // normalize to [0,1]
    double t = vc / 255.0;
    // quantize to [0..max_q]
    int qi = (int)std::floor(t * max_q + 0.5);
    qi = std::max(0, std::min(max_q, qi));
    // map back to [0,255]
    int v255 = (int)std::floor(qi * (255.0 / max_q) + 0.5);
    return (uint8_t)std::max(0, std::min(255, v255));
  };

  return ColorKey(q255(r), q255(g), q255(b));
}


// -----------------------------------------------------------------------------
// COLOR PALETTES + COLOR DISTANCE (for R-side LAP)
// -----------------------------------------------------------------------------
//
// Returns a List with:
//   colorsA_rgb: NumericMatrix (nA x 3) in 0..255
//   colorsB_rgb: NumericMatrix (nB x 3) in 0..255
//   groupsA    : List of IntegerVector (pixel indices for each palette entry in A)
//   groupsB    : List of IntegerVector (pixel indices for each palette entry in B)
//   countsA    : IntegerVector
//   countsB    : IntegerVector
//   color_dist : NumericMatrix (nA x nB) Euclidean distance in [0..sqrt(3)]
//
List color_palette_info(const NumericVector& pixelsA,
                        const NumericVector& pixelsB,
                        int H, int W,
                        int quantize_bits) {
  const int N  = H * W;
  const int HW = H * W;

  if (H <= 0 || W <= 0 ||
      pixelsA.size() < static_cast<R_xlen_t>(3) * HW ||
      pixelsB.size() < static_cast<R_xlen_t>(3) * HW) {
    Rcpp::stop("color_palette_info: pixel buffers must hold 3*H*W values (H, W > 0).");
  }

  // Build palettes as ordered maps for stable indexing
  std::map<ColorKey, std::vector<int>> paletteA, paletteB;
  for (int i = 0; i < N; ++i) {
    ColorKey k = make_color_key(pixelsA[i], pixelsA[i + HW], pixelsA[i + 2 * HW], quantize_bits);
    paletteA[k].push_back(i + 1);  // Convert to 1-based for R
  }
  for (int j = 0; j < N; ++j) {
    ColorKey k = make_color_key(pixelsB[j], pixelsB[j + HW], pixelsB[j + 2 * HW], quantize_bits);
    paletteB[k].push_back(j + 1);  // Convert to 1-based for R
  }

  // Freeze keys into vectors
  std::vector<ColorKey> colorsA, colorsB;
  colorsA.reserve(paletteA.size());
  colorsB.reserve(paletteB.size());
  for (const auto& p : paletteA) colorsA.push_back(p.first);
  for (const auto& p : paletteB) colorsB.push_back(p.first);

  const int nA = (int)colorsA.size();
  const int nB = (int)colorsB.size();

  NumericMatrix colorsA_rgb(nA, 3), colorsB_rgb(nB, 3);
  IntegerVector countsA(nA), countsB(nB);
  List groupsA(nA), groupsB(nB);

  for (int ia = 0; ia < nA; ++ia) {
    uint8_t r = (colorsA[ia].key >> 16) & 0xFF;
    uint8_t g = (colorsA[ia].key >> 8)  & 0xFF;
    uint8_t b =  colorsA[ia].key        & 0xFF;
    colorsA_rgb(ia, 0) = (double)r;
    colorsA_rgb(ia, 1) = (double)g;
    colorsA_rgb(ia, 2) = (double)b;
    const auto& grp = paletteA[colorsA[ia]];
    countsA[ia] = (int)grp.size();
    groupsA[ia] = IntegerVector(grp.begin(), grp.end());
  }
  for (int ib = 0; ib < nB; ++ib) {
    uint8_t r = (colorsB[ib].key >> 16) & 0xFF;
    uint8_t g = (colorsB[ib].key >> 8)  & 0xFF;
    uint8_t b =  colorsB[ib].key        & 0xFF;
    colorsB_rgb(ib, 0) = (double)r;
    colorsB_rgb(ib, 1) = (double)g;
    colorsB_rgb(ib, 2) = (double)b;
    const auto& grp = paletteB[colorsB[ib]];
    countsB[ib] = (int)grp.size();
    groupsB[ib] = IntegerVector(grp.begin(), grp.end());
  }

  // Palette-level color distance (Euclidean in 0..1 per channel)
  NumericMatrix color_dist(nA, nB);
  for (int ia = 0; ia < nA; ++ia) {
    double rA = colorsA_rgb(ia, 0) / 255.0;
    double gA = colorsA_rgb(ia, 1) / 255.0;
    double bA = colorsA_rgb(ia, 2) / 255.0;
    for (int ib = 0; ib < nB; ++ib) {
      double rB = colorsB_rgb(ib, 0) / 255.0;
      double gB = colorsB_rgb(ib, 1) / 255.0;
      double bB = colorsB_rgb(ib, 2) / 255.0;
      double dr = rA - rB, dg = gA - gB, db = bA - bB;
      color_dist(ia, ib) = std::sqrt(dr * dr + dg * dg + db * db);
    }
  }

  return List::create(
    _["colorsA_rgb"] = colorsA_rgb,
    _["colorsB_rgb"] = colorsB_rgb,
    _["groupsA"]     = groupsA,
    _["groupsB"]     = groupsB,
    _["countsA"]     = countsA,
    _["countsB"]     = countsB,
    _["color_dist"]  = color_dist
  );
}

// -----------------------------------------------------------------------------
// SPATIAL COST for a given pair of pixel index sets (for R-side LAP)
// -----------------------------------------------------------------------------
// FIXED: Uses column-major indexing (i = x*H + y)
NumericMatrix spatial_cost_matrix(const IntegerVector& idxA,
                                  const IntegerVector& idxB,
                                  int H, int W) {
  const int nA = idxA.size();
  const int nB = idxB.size();
  NumericMatrix C(nA, nB);
  const double diag = std::sqrt(double(W) * double(W) + double(H) * double(H));

  for (int i = 0; i < nA; ++i) {
    int a = idxA[i] - 1;  // Convert from R's 1-based to 0-based
    int yA = a % H;       // FIXED: column-major
    int xA = a / H;       // FIXED: column-major
    for (int j = 0; j < nB; ++j) {
      int b = idxB[j] - 1;  // Convert from R's 1-based to 0-based
      int yB = b % H;       // FIXED: column-major
      int xB = b / H;       // FIXED: column-major
      double dx = double(xA - xB), dy = double(yA - yB);
      C(i, j) = std::sqrt(dx * dx + dy * dy) / diag; // normalized [0..~1]
    }
  }
  return C;
}

// -----------------------------------------------------------------------------
// DOWNSCALING UTILITIES
// -----------------------------------------------------------------------------

struct DownscaleInfo {
  int H_orig, W_orig;
  int H_scaled, W_scaled;
  int steps;
};

DownscaleInfo compute_downscale(int H, int W, int steps) {
  DownscaleInfo info;
  info.H_orig = H;
  info.W_orig = W;
  info.steps  = steps;

  // Apply downscaling, but don't go below 8x8
  int H_scaled = H;
  int W_scaled = W;
  int actual   = 0;

  for (int i = 0; i < steps; ++i) {
    int H_next = H_scaled / 2;
    int W_next = W_scaled / 2;
    if (H_next < 8 || W_next < 8) break;
    H_scaled = H_next;
    W_scaled = W_next;
    actual++;
  }

  info.H_scaled = H_scaled;
  info.W_scaled = W_scaled;
  info.steps    = actual;
  return info;
}

// FIXED: Uses column-major indexing throughout
NumericVector downscale_image(const NumericVector& pixels, int H, int W, int H_new, int W_new) {
  const int HW     = H * W;
  const int HW_new = H_new * W_new;

  if (H <= 0 || W <= 0 || H_new <= 0 || W_new <= 0 ||
      pixels.size() < static_cast<R_xlen_t>(3) * HW) {
    Rcpp::stop("downscale_image: pixels must hold 3*H*W values and all dims > 0.");
  }

  NumericVector result(HW_new * 3);

  double scale_y = double(H) / H_new;
  double scale_x = double(W) / W_new;

  for (int y_new = 0; y_new < H_new; ++y_new) {
    for (int x_new = 0; x_new < W_new; ++x_new) {
      // Map to original coordinates
      int y_orig = static_cast<int>(y_new * scale_y);
      int x_orig = static_cast<int>(x_new * scale_x);

      // Clamp
      y_orig = std::min(y_orig, H - 1);
      x_orig = std::min(x_orig, W - 1);

      int idx_orig = x_orig * H + y_orig;        // FIXED: column-major
      int idx_new  = x_new * H_new + y_new;      // FIXED: column-major

      result[idx_new]               = pixels[idx_orig];
      result[idx_new + HW_new]      = pixels[idx_orig + HW];
      result[idx_new + 2 * HW_new]  = pixels[idx_orig + 2 * HW];
    }
  }
  return result;
}

// FIXED: Uses column-major indexing throughout
IntegerVector upscale_assignment(const IntegerVector& assignment,
                                 int H_orig, int W_orig,
                                 int H_scaled, int W_scaled) {
  const int N_orig   = H_orig * W_orig;

  if (H_orig <= 0 || W_orig <= 0 || H_scaled <= 0 || W_scaled <= 0 ||
      assignment.size() < static_cast<R_xlen_t>(H_scaled) * W_scaled) {
    Rcpp::stop("upscale_assignment: assignment must hold H_scaled*W_scaled entries (all dims > 0).");
  }

  IntegerVector out  = no_init(N_orig);

  double scale_y = double(H_scaled) / H_orig;
  double scale_x = double(W_scaled) / W_orig;

  for (int y_orig = 0; y_orig < H_orig; ++y_orig) {
    for (int x_orig = 0; x_orig < W_orig; ++x_orig) {
      // Map to scaled coordinates
      int y_scaled = static_cast<int>(y_orig * scale_y);
      int x_scaled = static_cast<int>(x_orig * scale_x);

      // Clamp
      y_scaled = std::min(y_scaled, H_scaled - 1);
      x_scaled = std::min(x_scaled, W_scaled - 1);

      int idx_scaled        = x_scaled * H_scaled + y_scaled;  // FIXED: column-major
      int assigned_scaled   = assignment[idx_scaled];

      // Map assigned pixel back to original resolution
      int y_assigned_scaled = assigned_scaled % H_scaled;      // FIXED: column-major
      int x_assigned_scaled = assigned_scaled / H_scaled;      // FIXED: column-major

      double y_assigned_orig = y_assigned_scaled / scale_y;
      double x_assigned_orig = x_assigned_scaled / scale_x;

      int y_assigned = static_cast<int>(y_assigned_orig);
      int x_assigned = static_cast<int>(x_assigned_orig);

      y_assigned = std::min(y_assigned, H_orig - 1);
      x_assigned = std::min(x_assigned, W_orig - 1);

      int idx_orig = x_orig * H_orig + y_orig;                 // FIXED: column-major
      out[idx_orig] = x_assigned * H_orig + y_assigned;        // FIXED: column-major
    }
  }
  return out;
}

// -----------------------------------------------------------------------------
// COLOR OVERLAP ANALYSIS
// -----------------------------------------------------------------------------

List analyze_color_overlap(const NumericVector& pixelsA,
                           const NumericVector& pixelsB,
                           int H, int W,
                           int quantize_bits) {
  const int N  = H * W;
  const int HW = H * W;

  if (H <= 0 || W <= 0 ||
      pixelsA.size() < static_cast<R_xlen_t>(3) * HW ||
      pixelsB.size() < static_cast<R_xlen_t>(3) * HW) {
    Rcpp::stop("analyze_color_overlap: pixel buffers must hold 3*H*W values (H, W > 0).");
  }

  std::unordered_map<ColorKey, int, ColorKeyHash> colorsA;
  std::unordered_map<ColorKey, int, ColorKeyHash> colorsB;

  for (int i = 0; i < N; ++i) {
    ColorKey key = make_color_key(pixelsA[i], pixelsA[i + HW], pixelsA[i + 2 * HW], quantize_bits);
    colorsA[key]++;
  }
  for (int j = 0; j < N; ++j) {
    ColorKey key = make_color_key(pixelsB[j], pixelsB[j + HW], pixelsB[j + 2 * HW], quantize_bits);
    colorsB[key]++;
  }

  int matched = 0;
  int overlap_colors = 0;
  for (const auto& kv : colorsA) {
    auto it = colorsB.find(kv.first);
    if (it != colorsB.end()) {
      overlap_colors++;
      matched += std::min(kv.second, it->second);
    }
  }

  // Jaccard similarity on unique-color sets
  int union_colors = (int)colorsA.size() + (int)colorsB.size() - overlap_colors;
  double coverage = (union_colors > 0) ? (double)overlap_colors / (double)union_colors : 0.0;

  return List::create(
    _["coverage"]           = coverage,
    _["matched_pixels"]     = matched,
    _["unique_colors_A"]    = (int)colorsA.size(),
    _["unique_colors_B"]    = (int)colorsB.size(),
    _["overlapping_colors"] = overlap_colors
  );
}

// -----------------------------------------------------------------------------
// COMPUTE PIXEL COST (for exact mode)  -- NORMALIZED COLOR + SPATIAL
// -----------------------------------------------------------------------------
// FIXED: Uses column-major indexing (i = x*H + y)
NumericMatrix compute_pixel_cost(const NumericVector& pixelsA,
                                 const NumericVector& pixelsB,
                                 int H, int W,
                                 double alpha, double beta) {
  const int N  = H * W;
  const int HW = H * W;

  if (H <= 0 || W <= 0 ||
      pixelsA.size() < static_cast<R_xlen_t>(3) * HW ||
      pixelsB.size() < static_cast<R_xlen_t>(3) * HW) {
    Rcpp::stop("compute_pixel_cost: pixel buffers must hold 3*H*W values (H, W > 0).");
  }

  NumericMatrix cost(N, N);
  const double diag = std::sqrt(double(W) * double(W) + double(H) * double(H));

  for (int i = 0; i < N; ++i) {
    int y_a = i % H;      // FIXED: column-major
    int x_a = i / H;      // FIXED: column-major

    // Normalize per-channel to [0,1] so color_dist is in [0..sqrt(3)]
    double r_a = pixelsA[i + 0 * HW] / 255.0;
    double g_a = pixelsA[i + 1 * HW] / 255.0;
    double b_a = pixelsA[i + 2 * HW] / 255.0;

    for (int j = 0; j < N; ++j) {
      int y_b = j % H;    // FIXED: column-major
      int x_b = j / H;    // FIXED: column-major

      double r_b = pixelsB[j + 0 * HW] / 255.0;
      double g_b = pixelsB[j + 1 * HW] / 255.0;
      double b_b = pixelsB[j + 2 * HW] / 255.0;

      double dr = r_a - r_b;
      double dg = g_a - g_b;
      double db = b_a - b_b;
      double color_dist = std::sqrt(dr * dr + dg * dg + db * db); // [0..sqrt(3)]

      double dx = double(x_a - x_b);
      double dy = double(y_a - y_b);
      double spatial_dist = std::sqrt(dx * dx + dy * dy) / diag;   // [0..1]

      cost(i, j) = alpha * color_dist + beta * spatial_dist;
    }
  }
  return cost;
}

// -----------------------------------------------------------------------------
// BILINEAR SPLATTING
// -----------------------------------------------------------------------------
// FIXED: Uses column-major indexing (idx = x*H + y)
inline void splat_pixel(NumericVector& frame,
                        std::vector<double>& weight_accum,
                        int H, int W,
                        double x, double y,
                        double r, double g, double b) {
  int x0 = (int)std::floor(x);
  int y0 = (int)std::floor(y);
  int x1 = x0 + 1;
  int y1 = y0 + 1;

  double fx = x - x0;
  double fy = y - y0;

  double w00 = (1.0 - fx) * (1.0 - fy);
  double w10 = fx * (1.0 - fy);
  double w01 = (1.0 - fx) * fy;
  double w11 = fx * fy;

  const int HW = H * W;

  if (x0 >= 0 && x0 < W && y0 >= 0 && y0 < H) {
    int idx = x0 * H + y0;  // FIXED: column-major
    frame[idx + 0 * HW] += r * w00;
    frame[idx + 1 * HW] += g * w00;
    frame[idx + 2 * HW] += b * w00;
    weight_accum[idx] += w00;
  }

  if (x1 >= 0 && x1 < W && y0 >= 0 && y0 < H) {
    int idx = x1 * H + y0;  // FIXED: column-major
    frame[idx + 0 * HW] += r * w10;
    frame[idx + 1 * HW] += g * w10;
    frame[idx + 2 * HW] += b * w10;
    weight_accum[idx] += w10;
  }

  if (x0 >= 0 && x0 < W && y1 >= 0 && y1 < H) {
    int idx = x0 * H + y1;  // FIXED: column-major
    frame[idx + 0 * HW] += r * w01;
    frame[idx + 1 * HW] += g * w01;
    frame[idx + 2 * HW] += b * w01;
    weight_accum[idx] += w01;
  }

  if (x1 >= 0 && x1 < W && y1 >= 0 && y1 < H) {
    int idx = x1 * H + y1;  // FIXED: column-major
    frame[idx + 0 * HW] += r * w11;
    frame[idx + 1 * HW] += g * w11;
    frame[idx + 2 * HW] += b * w11;
    weight_accum[idx] += w11;
  }
}

// -----------------------------------------------------------------------------
// MORPHING IMPLEMENTATION (renderer; assignment is provided by R)
// -----------------------------------------------------------------------------

// COLUMN-MAJOR transport renderer:
// - pixelsA, pixelsB: numeric planar RGB (length H*W*3)
// - assignment: 0-based indices into [0..H*W-1]
// - indexing convention: i = x*H + y  (x = column, y = row)
//
// Each pixel i moves along a straight line from (xs,ys) to (xd,yd)
// while its color is interpolated from A[i] to B[j].

// pixelsA, pixelsB: planar RGB, length = H * W * 3
// assignment: 0-based, length = H * W, using column-major indexing:
//   i = x * H + y   (x = column, y = row)
// Output: list of n_frames, each with RGBA (H * W * 4) + H, W

// pixelsA, pixelsB: planar RGB, length = H * W * 3
// assignment: 0-based, length = H * W, using column-major indexing:
//   i = x * H + y   (x = column, y = row)
// Output: list of n_frames, each with RGBA (H * W * 4) + H, W

// ASSIGNMENT CONVENTION:
//   assignment[i] = j means:
//     Pixel at linear index i in A (source) moves to linear index j (destination).
//     (0-based indices, column-major: idx = x*H + y)
//
// TRANSPORT SEMANTICS:
//   Pixels keep their color from A (no color blending with B).
//   Intermediate frames use bilinear splatting for motion blur.
//   Final frame is sharp transport-only (no splatting).

Rcpp::List morph_pixel_level_impl(const NumericVector& pixelsA,
                                  const NumericVector& pixelsB,
                                  const IntegerVector& assignment,
                                  int H, int W,
                                  int n_frames) {
  const int HW = H * W;
  const int N  = HW;

  // Basic size checks
  if ((int)pixelsA.size() != HW * 3 || (int)pixelsB.size() != HW * 3) {
    LAP_ERROR("pixelsA and pixelsB must be length H*W*3.");
  }
  if ((int)assignment.size() != N) {
    LAP_ERROR("assignment must have length H*W.");
  }

  Rcpp::List frames(n_frames);

  // Degenerate case: 1 frame → return sharp transport of A
  if (n_frames <= 1) {
    Rcpp::NumericVector frame(HW * 4);
    
    // Initialize to zero/transparent
    for (int idx = 0; idx < HW; ++idx) {
      frame[idx + 0 * HW] = 0.0;
      frame[idx + 1 * HW] = 0.0;
      frame[idx + 2 * HW] = 0.0;
      frame[idx + 3 * HW] = 0.0;
    }

    // Track which destinations were written
    std::vector<bool> written(HW, false);
    int overlaps = 0;

    // Transport A's pixels to their assigned positions
    for (int i = 0; i < HW; ++i) {
      int j = assignment[i];  // 0-based destination
      if (j < 0 || j >= HW) {
        LAP_ERROR("assignment index out of range.");
      }
      
      // Check for overlaps
      if (written[j]) {
        overlaps++;
      }
      
      // Copy A's color to destination position j
      frame[j + 0 * HW] = pixelsA[i + 0 * HW];  // R from A
      frame[j + 1 * HW] = pixelsA[i + 1 * HW];  // G from A
      frame[j + 2 * HW] = pixelsA[i + 2 * HW];  // B from A
      frame[j + 3 * HW] = 255.0;                 // Full opacity
      
      written[j] = true;
    }
    
    // Count holes
    int holes = 0;
    for (int idx = 0; idx < HW; ++idx) {
      if (!written[idx]) {
        holes++;
      }
    }
    
    // Warn if not a permutation
    if (overlaps > 0 || holes > 0) {
      Rcpp::warning(
        "Result has %d overlaps and %d holes (%.1f%% coverage). "
        "Assignment is not a permutation. "
        "For pixel-perfect results, use downscale_steps=0 and mode='exact'.",
        overlaps, holes, 100.0 * (double)(HW - holes) / (double)HW
      );
    }

    frames[0] = Rcpp::List::create(
      Rcpp::_["data"] = frame,
      Rcpp::_["H"]    = H,
      Rcpp::_["W"]    = W
    );
    return frames;
  }

  for (int t = 0; t < n_frames; ++t) {
    const bool is_final_frame = (t == n_frames - 1);
    const double tau = (double)t / (double)(n_frames - 1); // 0 → 1

    // RGBA frame: R,G,B,A each of length HW
    Rcpp::NumericVector frame(HW * 4);

    if (is_final_frame) {
      // ---- FINAL FRAME: Sharp transport-only (no splatting) ----
      
      // Initialize to zero/transparent
      for (int idx = 0; idx < HW; ++idx) {
        frame[idx + 0 * HW] = 0.0;
        frame[idx + 1 * HW] = 0.0;
        frame[idx + 2 * HW] = 0.0;
        frame[idx + 3 * HW] = 0.0;
      }
      
      // Track which destinations were written (for permutation validation)
      std::vector<bool> written(HW, false);
      int overlaps = 0;
      
      // Direct transport: each pixel from A goes to its assigned position
      for (int i = 0; i < N; ++i) {
        int j = assignment[i];  // 0-based destination
        if (j < 0 || j >= N) {
          LAP_ERROR("assignment index out of range.");
        }
        
        // Check for overlaps (multiple sources → same destination)
        if (written[j]) {
          overlaps++;
        }
        
        // Copy A's color directly to position j (no blending)
        // Note: If overlaps occur, last write wins
        frame[j + 0 * HW] = pixelsA[i + 0 * HW];  // R
        frame[j + 1 * HW] = pixelsA[i + 1 * HW];  // G
        frame[j + 2 * HW] = pixelsA[i + 2 * HW];  // B
        frame[j + 3 * HW] = 255.0;                 // A
        
        written[j] = true;
      }
      
      // Count holes (destinations never written)
      int holes = 0;
      for (int idx = 0; idx < HW; ++idx) {
        if (!written[idx]) {
          holes++;
          // Leave holes transparent (already initialized to 0)
        }
      }
      
      // Warn if assignment is not a permutation
      if (overlaps > 0 || holes > 0) {
        Rcpp::warning(
          "Final frame has %d overlaps and %d holes (%.1f%% coverage). "
          "Assignment is not a permutation. "
          "This typically occurs with downscaling or tiled modes. "
          "For pixel-perfect results, use downscale_steps=0 and mode='exact'.",
          overlaps, holes, 100.0 * (double)(HW - holes) / (double)HW
        );
      }
      
    } else {
      // ---- INTERMEDIATE FRAMES: Motion blur via bilinear splatting ----
      
      std::vector<double> weight_accum(HW, 0.0);

      // Transport each pixel along its LAP-assigned path
      for (int i = 0; i < N; ++i) {
        int j = assignment[i];  // 0-based
        if (j < 0 || j >= N) {
          LAP_ERROR("assignment index out of range.");
        }

        // Column-major index: i = x*H + y
        int ys = i % H;   // row
        int xs = i / H;   // col

        int yd = j % H;
        int xd = j / H;

        // Interpolated position along path
        double x = (1.0 - tau) * (double)xs + tau * (double)xd;
        double y = (1.0 - tau) * (double)ys + tau * (double)yd;

        // Colors from source (A) - no color blending
        double rA = pixelsA[i + 0 * HW];
        double gA = pixelsA[i + 1 * HW];
        double bA = pixelsA[i + 2 * HW];

        // Splat into RGBA frame (creates motion blur)
        splat_pixel(frame, weight_accum, H, W, x, y, rA, gA, bA);
      }

      // Normalize RGB and set alpha from weight_accum
      for (int idx = 0; idx < HW; ++idx) {
        double w = weight_accum[idx];

        if (w > 1e-6) {
          // Normalize transported RGB mass
          frame[idx + 0 * HW] /= w;
          frame[idx + 1 * HW] /= w;
          frame[idx + 2 * HW] /= w;

          // Opaque where some mass passed
          frame[idx + 3 * HW] = 255.0;
        } else {
          // No mass: keep RGB = 0 and alpha = 0 (fully transparent)
          frame[idx + 0 * HW] = 0.0;
          frame[idx + 1 * HW] = 0.0;
          frame[idx + 2 * HW] = 0.0;
          frame[idx + 3 * HW] = 0.0;
        }
      }
    }

    frames[t] = Rcpp::List::create(
      Rcpp::_["data"] = frame,
      Rcpp::_["H"]    = H,
      Rcpp::_["W"]    = W
    );
  }

  return frames;
}
