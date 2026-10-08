# AV1R

[![R-hub check on the R Consortium cluster](https://github.com/r-hub2/separate-jaguar-AV1R/actions/workflows/rhub-rc.yaml/badge.svg)](https://github.com/r-hub2/separate-jaguar-AV1R/actions/workflows/rhub-rc.yaml)

**AV1 video encoding for biological microscopy data.**

AV1R is an R package for biologists that converts legacy microscopy video formats (H.264/H.265, AVI/MJPEG, TIFF stacks) to the modern AV1 codec with minimal quality loss.

## Why AV1R?

- **Compress large TIFF stacks** from confocal microscopy, time-lapse, and EBImage workflows from hundreds of gigabytes to manageable sizes — without noticeable loss of information.
- **Re-encode MP4 files** exported from CellProfiler, ImageJ/Fiji, and microscope software with ~2x better compression at the same visual quality.
- **Standardise legacy recordings** — convert old AVI (MJPEG) and H.265 files to a single patent-free format suited for long-term archival.

## GPU acceleration

AV1R automatically selects the best available backend:

| Priority | Backend | How |
|----------|---------|-----|
| 1 | **VAAPI** | `av1_vaapi` via FFmpeg — best speed/quality (AMD/Intel) |
| 2 | **Vulkan** | `VK_KHR_VIDEO_ENCODE_AV1` — fastest, CQP only |
| 3 | **CPU (SVT-AV1)** | `libsvtav1` via FFmpeg |

### Tested hardware

| GPU | Driver | Status |
|-----|--------|--------|
| AMD Radeon RX 9070 (RDNA4) | Mesa RADV (GFX1201) | Working |

Vulkan AV1 encode headers are bundled in `src/vk_video/`, so no SDK upgrade is needed at build time. Builds with any Vulkan SDK >= 1.3.275. Runtime support depends on GPU driver.

### Benchmark (30 s clip, 1920x1080, CRF 28, AMD RX 9070)

| Backend | Time (s) | Size (MB) | Compression | SSIM | FPS | vs CPU |
|---------|----------|-----------|-------------|------|-----|--------|
| Vulkan  | 2.9      | 3.31      | 1.9x | 0.9339 | 245.9 | 4.0x |
| VAAPI   | 2.0      | 3.97      | 1.6x | 0.9609 | 363.3 | 5.9x |
| CPU (SVT-AV1) | 11.7 | 8.09  | 0.8x | 0.9645 | 61.6 | 1.0x |

Compression = input clip size / encoded size. SSIM >= 0.97 visually transparent.
vs CPU = CPU time / backend time (higher = faster).

Reproduce: `Rscript inst/examples/benchmark_backends.R`

## Quick Start

```r
library(AV1R)

# Convert microscopy recording
convert_to_av1("recording.mp4", "recording_av1.mp4")

# TIFF stack with custom settings
convert_to_av1("stack.tif", "stack.mp4", av1r_options(crf = 20))

# Batch convert entire experiment folder
convert_folder("experiment/", "compressed/")

# Check what backend will be used
detect_backend()
```

## GPU encoding

```r
# Check GPU availability
vulkan_available()
vulkan_devices()

# Force GPU backend
convert_to_av1("input.mp4", "output.mp4", av1r_options(backend = "vulkan"))

# Force CPU backend
convert_to_av1("input.mp4", "output.mp4", av1r_options(backend = "cpu"))
```

Each backend uses the quality control approach best suited to its rate control:

| Backend | Video input | TIFF/image input |
|---------|-------------|------------------|
| CPU (SVT-AV1) | CRF directly | CRF directly |
| Vulkan | CRF directly (CQP) | CRF directly (CQP) |
| VAAPI | VBR at 55% of input bitrate | CQP (CRF mapped to QP 0–255) |

Frames smaller than the hardware minimum coded extent are automatically
scaled up proportionally (no distortion). Audio from the original file
is preserved automatically.

### CRF equivalence across backends (TIFF, mitosis.tif, 510 frames)

CRF is a unified quality parameter (0 = best, 63 = worst). Since each backend
uses a different internal quantizer scale, AV1R maps CRF automatically.
The table below shows equivalent settings calibrated by file size:

| CRF | CPU (KB) | VAAPI QP | VAAPI (KB) | Vulkan CRF | Vulkan (KB) |
|----:|--------:|---------:|----------:|-----------:|----------:|
|   1 |  3815.5 |        8 |    3800.8 |          2 |    3369.2 |
|   3 |  2766.5 |       11 |    2822.4 |          2 |    3369.2 |
|   5 |  1983.7 |       14 |    1982.1 |          3 |    2064.3 |
|   8 |   924.4 |       24 |     952.6 |          5 |     955.0 |
|  10 |   578.7 |       31 |     597.1 |          7 |     548.9 |
|  15 |   264.4 |       50 |     278.7 |         11 |     260.5 |
|  20 |   162.1 |       75 |     156.5 |         15 |     163.8 |
|  30 |    84.4 |      110 |      82.0 |         30 |      61.9 |
|  40 |    53.2 |      150 |      53.3 |         40 |      46.9 |
|  50 |    38.8 |      190 |      38.8 |         50 |      37.6 |
|  63 |    25.4 |      255 |      29.5 |         63 |      30.5 |

Reproduce: `Rscript inst/examples/calibrate_vaapi_cqp.R`

**Note:** Vulkan uses CQP (constant quantizer) rate control — the only mode
currently working on RADV. CQP does not adapt to scene complexity, so Vulkan
produces slightly lower SSIM (~0.93) compared to CPU/VAAPI (~0.96). This is
a driver limitation; VBR/CRF support will improve quality when RADV implements
it.

## Supported Input Formats

| Format | Extension |
|--------|-----------|
| H.264 / MP4 | `.mp4`, `.mkv`, `.mov` |
| H.265 / HEVC | `.mp4`, `.mkv` |
| AVI / MJPEG | `.avi` |
| FLV | `.flv` |
| MPEG-1/2 | `.mpg`, `.mpeg` |
| WebM (VP8/VP9) | `.webm` |
| TIFF stack | `.tif`, `.tiff` |
| TIFF sequence | `frame%04d.tif` |

## System Requirements

- **FFmpeg >= 4.4** with `libsvtav1` (required for CPU encoding)
- **libvulkan-dev** (optional, for GPU encoding on Linux)

```bash
# Ubuntu 22.04+
sudo apt install ffmpeg libvulkan-dev
```

## TIFF stacks

TIFF stacks use near-lossless CRF 5 by default (`tiff_crf`). Output resolution can be controlled via `tiff_scale`.

> **Note for quantitative microscopy:** AV1 is a lossy codec even at CRF 1.
> If your workflow relies on pixel intensity measurements (fluorescence
> quantification, ratiometric imaging, colocalization analysis), lossy
> compression will alter the data. For such use cases, keep the original
> TIFF/PNG files for analysis and use AV1R only for visualization, sharing,
> and archival of preview copies. Lossless alternatives: TIFF, PNG, FFV1.

```r
# 2x magnification (proportional, no distortion)
convert_to_av1("stack.tif", "stack.mp4", av1r_options(tiff_scale = 2))

# Fit into 1920x1080 bounding box (proportional)
convert_to_av1("stack.tif", "stack.mp4", av1r_options(tiff_scale = c(1920, 1080)))
```

See `inst/examples/tiff_scale.R` for more examples.

## Options

```r
av1r_options(
  crf        = 28,    # quality: 0 (best) - 63 (worst)
  preset     = 8,     # speed: 0 (slow/best) - 13 (fast/worst), CPU only
  threads    = 0,     # 0 = auto, CPU only
  tiff_crf   = 5,     # CRF for TIFF input (near-lossless)
  tiff_scale = NULL,  # NULL, multiplier (e.g. 2), or c(width, height)
  verbose    = TRUE,  # informational messages
  backend    = "auto" # "auto", "cpu", "vaapi", or "vulkan"
)
```

## License

MIT

---

[GitHub](https://github.com/Zabis13/AV1R) · [Issues](https://github.com/Zabis13/AV1R/issues)
