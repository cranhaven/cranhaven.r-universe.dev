# function to convert a rasters extent to a polygon
rasterextent2polygon <- function(x) {
    y <- sf::st_as_sfc(sf::st_bbox(x))
    sf::st_crs(y) <- sf::st_crs(x)
    return(y)
}

# function to convert an extent to a polygon
extent2polygon <- function(x, crs) {
    y <- sf::st_as_sfc(sf::st_bbox(x))
    sf::st_crs(y) <- crs
    return(y)
}

isUTM32 <- function(x) {
    if (sf::st_crs(x) == sf::st_crs(25832)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

isUTM33 <- function(x) {
    if (sf::st_crs(x) == sf::st_crs(25833)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

comp_ext <- function(e1, e2) {
    any(
        c(terra::xmin(e1) > terra::xmin(e2),
          terra::xmax(e1) < terra::xmax(e2),
          terra::ymin(e1) > terra::ymin(e2),
          terra::ymax(e1) < terra::ymax(e2))
    )
}
