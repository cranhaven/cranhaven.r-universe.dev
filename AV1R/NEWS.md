# AV1R 0.1.3

## TIFF scaling

* New `tiff_scale` parameter in `av1r_options()` for explicit control of
  TIFF/image sequence output resolution. Supports multiplier (e.g. `2` for 2x)
  or bounding box (e.g. `c(1920, 1080)` to fit into 1080p). Aspect ratio is
  always preserved — no distortion.
* Frames smaller than hardware minimum (VAAPI: 320x128) are automatically
  scaled up proportionally across all backends (CPU, VAAPI, Vulkan).
* Example script: `inst/examples/tiff_scale.R`.

## Batch conversion

* New example script `inst/examples/batch_convert.R` for batch folder
  conversion with summary table (input/output size, compression ratio).
* `tiff_crf` parameter (default 5) automatically applied to TIFF/image
  input for near-lossless microscopy encoding. Overrides `crf`.
* `verbose` option controls informational messages.

## Quality control architecture

* Each backend now uses the approach best suited to its rate control:
  - **CPU (SVT-AV1)**: CRF passed directly — SVT-AV1 adapts to content
  - **Vulkan**: CRF passed directly as QP — CQP mode (RADV limitation)
  - **VAAPI**: 55% of input video bitrate via ffprobe — adapts to content
* Removed calibration table approach (0.1.3-dev): empirical testing showed
  fixed CRF→bitrate tables are content-dependent and break on different
  input formats (MP4 vs AVI). Adaptive approach is simpler and more robust.
* Vulkan CQP limitation documented: RADV only supports constant quantizer
  (no VBR/CRF), resulting in ~0.93 SSIM vs ~0.96 for CPU/VAAPI at the same
  bitrate. This is a driver limitation, not a bug.

## CPU encoding fixes

* SVT-AV1 preset and thread count now passed via `-svtav1-params` instead
  of `-preset` and `-threads` which had no effect on libsvtav1.
* Added `-pix_fmt yuv420p` for explicit pixel format.

## Audio preservation

* CPU path now preserves audio tracks from input files (`-c:a copy`)
  instead of stripping them (`-an`). TIFF inputs still use `-an` since
  they have no audio.

## Benchmark improvements

* Added "vs CPU" column showing speedup relative to CPU backend.
* Added calibration scripts: `calibrate_crf.R`, `calibrate_raw.R`,
  `calibrate_vaapi_bitrate.R` for measuring backend response curves.

# AV1R 0.1.2

## Minimum coded extent handling

* GPU encoder now queries `minCodedExtent` from the Vulkan driver before
  opening the ffmpeg decode pipe. Input frames smaller than the hardware
  minimum (e.g. 170×196 vs driver minimum 320×128) are automatically
  scaled up to the minimum supported resolution.
* Vulkan context is initialized before the ffmpeg pipe so that the
  corrected dimensions are used for both decoding and encoding.
* Fixed segfault in `av1r_vulkan_query_min_extent()`: the
  `VkVideoEncodeAV1CapabilitiesKHR` structure was missing from the pNext
  chain passed to `vkGetPhysicalDeviceVideoCapabilitiesKHR`, causing
  RADV to write to an unmapped address.

## CRF / QIndex mapping

* CRF-to-QIndex mapping changed from linear (`crf * 4`) to nonlinear LUT
  approximating aomenc `--cq-level`. Linear mapping gave qIndex 112 at
  CRF 28 which was too low for real compression on RADV. The new mapping
  gives qIndex 175 at CRF 28, matching expected AV1 compression ratios.
* On RADV, VBR mode is reported as supported but produces huge files;
  DISABLED mode (CQP) with `constantQIndex` is the only working path.

# AV1R 0.1.1

## Vulkan AV1 GPU Encoding — fully working

* **GPU encoding now works end-to-end** on AMD RDNA4 (RX 9070, RADV GFX1201)
  via Vulkan `VK_KHR_VIDEO_ENCODE_AV1`.
* Two-queue architecture: transfer queue (buffer→image upload) + encode queue
  (video encoding) with semaphore synchronization. Fixes `VK_ERROR_DEVICE_LOST`
  on GPUs where the encode queue has no TRANSFER capability.
* Correct NV12 upload: Y plane and UV plane copied as separate
  `vkCmdCopyBufferToImage` calls with proper `bufferRowLength` for each plane.
* Sequence Header OBU automatically prepended via
  `vkGetEncodedVideoSessionParametersKHR` — output plays in all players.
* Rate control: DISABLED mode (CQP) with `constantQIndex` derived from CRF
  (`qIndex = crf * 4`). VBR/CBR modes are reported as supported by RADV but
  have no effect; CQP is the only working mode.
* Audio track from the original file is preserved during IVF→MP4 muxing.
* Dynamic loading of all Vulkan Video KHR extension functions via
  `vkGetInstanceProcAddr`/`vkGetDeviceProcAddr` for compatibility with
  SDK < 1.3.290.

## Multi-page TIFF support

* Multi-page TIFF stacks are now extracted to a temporary PNG sequence via
  the `magick` package before encoding. Both GPU and CPU paths support this.
* `magick` added to `Suggests` (required only for multi-page TIFF input).
* Image sequences (printf patterns like `frame%06d.png`) are passed to ffmpeg
  with `-framerate` for correct decoding.
* Tested on 510-frame 16-bit grayscale confocal microscopy TIFF stack
  (32.7 MB → 3.8 MB at CRF 1, 8.6x compression with quality preservation).

## CRAN Fixes

* Removed redundant "A tool for" from Description.
* Single-quoted software and API names in Title and Description per CRAN policy.
* Replaced `\dontrun{}` with `\donttest{}` in all examples.
* Examples now write to `tempdir()` instead of user's home filespace.

## Bundled Headers

* Bundled Khronos `VK_KHR_video_encode_av1` headers (`src/vk_video/`) — package
  no longer requires Vulkan SDK >= 1.3.290 at build time; builds against any
  Vulkan SDK >= 1.3.275 with the bundled polyfill.
* `configure` now detects bundled AV1 encode headers and enables GPU path
  automatically.

## Initial Release

* CPU encoding via FFmpeg (`libsvtav1`).
* Automatic backend selection: GPU (Vulkan) → CPU (FFmpeg).
* Supports H.264, H.265, AVI/MJPEG, TIFF stacks, and TIFF sequences as input.
* `convert_to_av1()` for single-file and batch conversion.
* `detect_backend()`, `vulkan_available()`, `vulkan_devices()` for diagnostics.
