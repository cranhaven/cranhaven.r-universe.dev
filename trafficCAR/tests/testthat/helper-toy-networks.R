## Quick interactive plot run in console to check shape

quick_plot <- function(name_of_toy_road){
  plot(sf::st_geometry(name_of_toy_road), col = "black", lwd = 2)
  points(sf::st_coordinates(sf::st_cast(toy_roads, "POINT")), pch = 19, col = "red")
}

## ex: quick_plot(toy_L)

## ----------------------------------------------------------------------------------

# L-shaped network w/ two line segments, three nodes (endpoints + intersection)

toy_L <- sf::st_sf(
  geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)

# T-junction network (3 segments, 4 nodes: one degree-3 intersection)

toy_T <- sf::st_sf(
  geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, -1, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)

# square loop network (4 segments, 4 nodes, one cycle)

toy_square <- sf::st_sf(
  geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, 1, 0, 1), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0, 1, 0, 0), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)

# 2x2 grid network (6 segments, 9 nodes, planar grid)

toy_grid <- sf::st_sf(
  geometry = sf::st_sfc(
    # horizontal
    sf::st_linestring(matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0, 1, 2, 1), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0, 2, 2, 2), ncol = 2, byrow = TRUE)),
    # vertical
    sf::st_linestring(matrix(c(0, 0, 0, 2), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, 0, 1, 2), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(2, 0, 2, 2), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)

# disconnected network (2 components, 2 segments, 4 nodes)

toy_disconnected <- sf::st_sf(
  geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(3, 0, 4, 0), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)

# freeway with on-ramp (merge)

toy_on_ramp <- sf::st_sf(
  geometry = sf::st_sfc(
    # main freeway
    sf::st_linestring(matrix(c(-3, 0, 3, 0), ncol = 2, byrow = TRUE)),

    # on-ramp
    sf::st_linestring(matrix(c(
      -1, -2,
      -0.5, -1,
      0, 0
    ), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)

# freeway with off-ramp (diverge); NEEDS FIX

toy_off_ramp <- sf::st_sf(
  geometry = sf::st_sfc(
    # main freeway
    sf::st_linestring(matrix(c(
      -5, 0,
      0, 0,
      5, 0
    ), ncol = 2, byrow = TRUE)),

    # off-ramp
    sf::st_linestring(matrix(c(
      0, 0,
      1, 0.4,
      2, 0.8
    ), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)

# partial cloverleaf interchange (2 ramps + 2 crossing freeways)
# testing crossings, mixed open/closed ramps of moderate complexity

toy_partial_cloverleaf <- sf::st_sf(
  geometry = sf::st_sfc(
    # horizontal freeway
    sf::st_linestring(matrix(c(-3, 0, 3, 0), ncol = 2, byrow = TRUE)),

    # vertical freeway
    sf::st_linestring(matrix(c(0, -3, 0, 3), ncol = 2, byrow = TRUE)),

    # northeast ramp (quarter loop)
    sf::st_linestring(matrix(c(
      1, 0,
      2, 1,
      1, 2
    ), ncol = 2, byrow = TRUE)),

    # southwest ramp (quarter loop)
    sf::st_linestring(matrix(c(
      -1, 0,
      -2, -1,
      -1, -2
    ), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)

# freeway cloverleaf interchange (4 loops + 2 crossing freeways)
# testing multiple cycles, crossings without endpoints, high node degree

toy_cloverleaf <- sf::st_sf(
  geometry = sf::st_sfc(
    # horizontal freeway
    sf::st_linestring(matrix(c(-3, 0, 3, 0), ncol = 2, byrow = TRUE)),
    # vertical freeway
    sf::st_linestring(matrix(c(0, -3, 0, 3), ncol = 2, byrow = TRUE)),

    # northeast loop
    sf::st_linestring(matrix(c(
      1, 0,
      2, 1,
      1, 2,
      0, 1,
      1, 0
    ), ncol = 2, byrow = TRUE)),

    # northwest loop
    sf::st_linestring(matrix(c(
      -1, 0,
      -2, 1,
      -1, 2,
      0, 1,
      -1, 0
    ), ncol = 2, byrow = TRUE)),

    # southwest loop
    sf::st_linestring(matrix(c(
      -1, 0,
      -2, -1,
      -1, -2,
      0, -1,
      -1, 0
    ), ncol = 2, byrow = TRUE)),

    # southeast loop
    sf::st_linestring(matrix(c(
      1, 0,
      2, -1,
      1, -2,
      0, -1,
      1, 0
    ), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)





