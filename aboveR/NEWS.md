# aboveR 1.0.0

## New Features

* **Terrain derivatives:** `slope_aspect()`, `hillshade()`, `contour_lines()`,
  and `zonal_stats()` for slope, aspect, shaded relief, contour generation,
  and general-purpose zonal statistics.

* **Flood analysis:** `flood_inundation()` for binary inundation masks,
  `flood_depth()` for water depth rasters, and `height_above_drainage()`
  implementing the HAND model for flood susceptibility mapping.

* **Mining analysis:** `bench_detection()` identifies flat mining benches
  adjacent to highwall faces using slope segmentation.

* **Engineering export:** `export_landxml()` writes LandXML TIN surfaces
  for Civil 3D, Bentley OpenRoads, and Trimble. `export_stl()` writes STL
  meshes for 3D printing terrain models.

* **KyFromAbove counties:** `kfa_county_bbox()` and `kfa_list_counties()`
  provide bounding box lookup for all 120 Kentucky counties.

* **Color ramps:** `change_colors()`, `terrain_colors()`, and `flood_colors()`
  are now exported for use in custom visualizations.

* **`has_s3_access()`** is now exported for use in `@examplesIf` guards.

## Security

* Added `sanitize_filename()` to prevent path traversal in cached file names.
* Added `validate_kfa_url()` to verify download URLs originate from the
  KyFromAbove S3 bucket.
* Added `safe_download()` with configurable timeout (300s default), retry
  with exponential backoff (3 attempts), and file size limits (500 MB default).
* `kfa_read_dem()`, `kfa_read_pointcloud()`, and `kfa_tile_index()` now use
  the hardened download functions.

## Documentation

* Added `CITATION.cff` and `.zenodo.json` for DOI-based citation.
* Added community health files: `CODE_OF_CONDUCT.md`, `CONTRIBUTING.md`,
  `SECURITY.md`, `SUPPORT.md`, `ATTRIBUTION.md`, `CHANGELOG.md`,
  `DEVELOPMENT.md`, `DISCLAIMER.md`, `GOVERNANCE.md`, `MAINTAINERS.md`,
  `PRIVACY.md`, `RESPONSIBLE_USE.md`.
* Added GitHub issue and PR templates.

## Infrastructure

* Added Codecov integration for test coverage reporting.
* Added Dependabot for GitHub Actions dependency updates.
* Added CodeQL security scanning workflow.
* Added dependency review workflow for pull requests.
* Added stale issue/PR bot.

# aboveR 0.1.0

* Initial CRAN release.
* Core analysis: `terrain_change()`, `change_by_zone()`, `estimate_volume()`,
  `impoundment_curve()`, `terrain_profile()`, `boundary_terrain_profile()`,
  `classify_highwall()`, `reclamation_progress()`, `surface_roughness()`,
  `detect_channels()`, `pond_sedimentation()`.
* KyFromAbove access: `kfa_find_tiles()`, `kfa_tile_index()`,
  `kfa_read_dem()`, `kfa_read_pointcloud()`, `kfa_read_ortho()`,
  `kfa_stac_search()`.
* Bundled sample data for examples and testing.
* Getting started vignette.
