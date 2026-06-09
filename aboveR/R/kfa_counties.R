# kfa_counties — aboveR
# Kentucky county bounding box lookup for KyFromAbove tile discovery

#' Get Bounding Box for a Kentucky County
#'
#' Returns a bounding box (in EPSG:4326) for a Kentucky county by name.
#' Useful for quickly querying KyFromAbove tiles without constructing
#' an AOI manually.
#'
#' @param county Character. County name (case-insensitive). Partial
#'   matching is supported.
#'
#' @returns A numeric vector `c(xmin, ymin, xmax, ymax)` in EPSG:4326.
#'
#' @export
#'
#' @examples
#' # Get bounding box for Fayette County
#' bbox <- kfa_county_bbox("Fayette")
#' print(bbox)
#'
#' @examplesIf aboveR::has_s3_access()
#' # Use directly with kfa_find_tiles
#' tiles <- kfa_find_tiles(kfa_county_bbox("Fayette"), product = "dem")
kfa_county_bbox <- function(county) {
  if (!is.character(county) || length(county) != 1) {
    stop("`county` must be a single character string.", call. = FALSE)
  }

  counties <- kfa_county_list()
  idx <- grep(paste0("^", county), names(counties), ignore.case = TRUE)

  if (length(idx) == 0) {
    # Try partial match
    idx <- grep(county, names(counties), ignore.case = TRUE)
  }

  if (length(idx) == 0) {
    stop(
      "County '", county, "' not found. Use kfa_list_counties() to see ",
      "available counties.",
      call. = FALSE
    )
  }

  if (length(idx) > 1) {
    matches <- names(counties)[idx]
    stop(
      "Multiple counties match '", county, "': ",
      paste(matches, collapse = ", "), "\n",
      "Please be more specific.",
      call. = FALSE
    )
  }

  counties[[idx]]
}

#' List Available Kentucky Counties
#'
#' Returns a character vector of all 120 Kentucky county names.
#'
#' @returns Character vector of county names sorted alphabetically.
#'
#' @export
#'
#' @examples
#' counties <- kfa_list_counties()
#' head(counties)
kfa_list_counties <- function() {
  sort(names(kfa_county_list()))
}

#' Internal County Bounding Box Data
#'
#' @returns Named list of bbox vectors (xmin, ymin, xmax, ymax) in EPSG:4326.
#' @noRd
kfa_county_list <- function() {
  list(
    Adair       = c(-85.62, 36.95, -85.02, 37.23),
    Allen       = c(-86.35, 36.65, -85.96, 36.96),
    Anderson    = c(-85.19, 37.88, -84.87, 38.12),
    Ballard     = c(-89.22, 36.89, -88.81, 37.12),
    Barren      = c(-86.27, 36.79, -85.74, 37.13),
    Bath        = c(-83.98, 38.03, -83.57, 38.30),
    Bell        = c(-83.90, 36.60, -83.46, 36.88),
    Boone       = c(-84.88, 38.76, -84.62, 39.10),
    Bourbon     = c(-84.32, 38.07, -83.98, 38.34),
    Boyd        = c(-82.87, 38.23, -82.58, 38.57),
    Boyle       = c(-84.96, 37.53, -84.63, 37.75),
    Bracken     = c(-84.22, 38.58, -83.91, 38.80),
    Breathitt   = c(-83.58, 37.43, -83.17, 37.78),
    Breckinridge = c(-86.49, 37.66, -86.02, 37.95),
    Bullitt     = c(-85.85, 37.85, -85.48, 38.10),
    Butler      = c(-86.70, 37.04, -86.32, 37.32),
    Caldwell    = c(-87.88, 37.01, -87.50, 37.27),
    Calloway    = c(-88.53, 36.50, -88.05, 36.76),
    Campbell    = c(-84.50, 38.85, -84.21, 39.10),
    Carlisle    = c(-88.97, 36.78, -88.58, 36.99),
    Carroll     = c(-85.22, 38.49, -84.95, 38.72),
    Carter      = c(-83.33, 38.18, -82.87, 38.50),
    Casey       = c(-85.20, 37.15, -84.82, 37.47),
    Christian   = c(-87.71, 36.63, -87.07, 37.07),
    Clark       = c(-84.37, 37.90, -84.04, 38.12),
    Clay        = c(-83.94, 37.07, -83.49, 37.42),
    Clinton     = c(-85.23, 36.63, -84.92, 36.93),
    Crittenden  = c(-88.23, 37.14, -87.83, 37.43),
    Cumberland  = c(-85.65, 36.63, -85.30, 36.89),
    Daviess     = c(-87.25, 37.57, -86.84, 37.87),
    Edmonson    = c(-86.38, 37.12, -86.02, 37.35),
    Elliott     = c(-83.35, 38.05, -83.01, 38.30),
    Estill      = c(-84.12, 37.65, -83.77, 37.92),
    Fayette     = c(-84.62, 37.96, -84.29, 38.18),
    Fleming     = c(-83.92, 38.25, -83.51, 38.52),
    Floyd       = c(-82.95, 37.43, -82.57, 37.76),
    Franklin    = c(-85.01, 38.14, -84.68, 38.40),
    Fulton      = c(-89.28, 36.50, -88.81, 36.68),
    Gallatin    = c(-85.02, 38.64, -84.72, 38.82),
    Garrard     = c(-84.73, 37.53, -84.42, 37.77),
    Grant       = c(-84.80, 38.57, -84.46, 38.79),
    Graves      = c(-88.83, 36.59, -88.39, 36.94),
    Grayson     = c(-86.34, 37.30, -85.90, 37.62),
    Green       = c(-85.70, 37.18, -85.30, 37.44),
    Greenup     = c(-83.07, 38.38, -82.66, 38.73),
    Hancock     = c(-86.89, 37.67, -86.53, 37.91),
    Hardin      = c(-86.14, 37.57, -85.67, 37.89),
    Harlan      = c(-83.44, 36.70, -83.01, 36.96),
    Harrison    = c(-84.59, 38.32, -84.22, 38.59),
    Hart        = c(-86.04, 37.14, -85.63, 37.46),
    Henderson   = c(-87.80, 37.57, -87.35, 37.88),
    Henry       = c(-85.29, 38.32, -84.96, 38.57),
    Hickman     = c(-89.16, 36.50, -88.79, 36.79),
    Hopkins     = c(-87.67, 37.15, -87.20, 37.50),
    Jackson     = c(-84.10, 37.33, -83.80, 37.62),
    Jefferson   = c(-85.95, 38.02, -85.41, 38.38),
    Jessamine   = c(-84.69, 37.77, -84.42, 37.97),
    Johnson     = c(-83.05, 37.69, -82.60, 37.99),
    Kenton      = c(-84.68, 38.86, -84.42, 39.10),
    Knott       = c(-83.12, 37.28, -82.73, 37.55),
    Knox        = c(-84.14, 36.77, -83.67, 37.07),
    Larue       = c(-85.82, 37.46, -85.51, 37.68),
    Laurel      = c(-84.30, 36.93, -83.88, 37.28),
    Lawrence    = c(-82.90, 37.87, -82.49, 38.24),
    Lee         = c(-83.82, 37.50, -83.51, 37.72),
    Leslie      = c(-83.60, 37.00, -83.20, 37.32),
    Letcher     = c(-83.02, 36.97, -82.62, 37.25),
    Lewis       = c(-83.58, 38.35, -83.08, 38.72),
    Lincoln     = c(-84.87, 37.35, -84.50, 37.63),
    Livingston  = c(-88.45, 37.00, -88.02, 37.28),
    Logan       = c(-87.12, 36.65, -86.62, 36.98),
    Lyon        = c(-88.22, 36.88, -87.87, 37.12),
    Madison     = c(-84.52, 37.57, -84.12, 37.87),
    Magoffin    = c(-83.25, 37.58, -82.90, 37.87),
    Marion      = c(-85.53, 37.46, -85.17, 37.68),
    Marshall    = c(-88.53, 36.82, -88.17, 37.07),
    Martin      = c(-82.65, 37.48, -82.31, 37.77),
    Mason       = c(-83.95, 38.52, -83.57, 38.78),
    McCracken   = c(-88.80, 36.93, -88.46, 37.15),
    McCreary    = c(-84.62, 36.52, -84.15, 36.85),
    McLean      = c(-87.35, 37.43, -87.02, 37.64),
    Meade       = c(-86.33, 37.82, -85.92, 38.10),
    Menifee     = c(-83.65, 37.83, -83.37, 38.05),
    Mercer      = c(-84.95, 37.68, -84.62, 37.92),
    Metcalfe    = c(-85.72, 36.85, -85.42, 37.10),
    Monroe      = c(-85.78, 36.63, -85.38, 36.88),
    Montgomery  = c(-84.06, 37.97, -83.77, 38.18),
    Morgan      = c(-83.50, 37.77, -83.10, 38.07),
    Muhlenberg  = c(-87.32, 37.12, -86.92, 37.42),
    Nelson      = c(-85.73, 37.72, -85.29, 37.97),
    Nicholas    = c(-84.18, 38.25, -83.87, 38.47),
    Ohio        = c(-87.02, 37.30, -86.55, 37.60),
    Oldham      = c(-85.63, 38.32, -85.31, 38.52),
    Owen        = c(-84.98, 38.42, -84.66, 38.68),
    Owsley      = c(-83.70, 37.38, -83.42, 37.58),
    Pendleton   = c(-84.52, 38.57, -84.22, 38.82),
    Perry       = c(-83.35, 37.05, -82.97, 37.40),
    Pike        = c(-82.65, 37.25, -82.02, 37.68),
    Powell      = c(-83.95, 37.73, -83.65, 37.93),
    Pulaski     = c(-84.82, 36.87, -84.28, 37.23),
    Robertson   = c(-84.17, 38.42, -83.93, 38.55),
    Rockcastle  = c(-84.53, 37.30, -84.18, 37.55),
    Rowan       = c(-83.62, 38.10, -83.25, 38.35),
    Russell     = c(-85.22, 36.82, -84.82, 37.10),
    Scott       = c(-84.68, 38.22, -84.40, 38.50),
    Shelby      = c(-85.42, 38.12, -85.05, 38.38),
    Simpson     = c(-86.72, 36.63, -86.42, 36.85),
    Spencer     = c(-85.48, 37.92, -85.20, 38.12),
    Taylor      = c(-85.55, 37.32, -85.23, 37.55),
    Todd        = c(-87.25, 36.65, -86.83, 36.95),
    Trigg       = c(-88.05, 36.63, -87.63, 36.93),
    Trimble     = c(-85.48, 38.52, -85.18, 38.72),
    Union       = c(-88.05, 37.55, -87.58, 37.82),
    Warren      = c(-86.62, 36.82, -86.12, 37.10),
    Washington  = c(-85.30, 37.65, -85.02, 37.88),
    Wayne       = c(-85.10, 36.63, -84.68, 36.95),
    Webster     = c(-87.78, 37.38, -87.38, 37.60),
    Whitley     = c(-84.42, 36.65, -83.92, 36.97),
    Wolfe       = c(-83.62, 37.63, -83.30, 37.85),
    Woodford    = c(-84.85, 37.88, -84.62, 38.10)
  )
}
