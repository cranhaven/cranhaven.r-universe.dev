#' @keywords internal
"_PACKAGE"

# Suppress R CMD check note
# Namespace in Imports field not imported from: PKG
#   All declared Imports should be used.
ignore_unused_imports <- function() {
  PL94171::pl_get_baf
  alarmdata::alarm_50state_map
  censable::build_dec
  easycensus::cens_find_acs
  geomander::geo_match
  ggredist::map_coloring
  redist::redist_map
  redistmetrics::by_plan
  sf::st_crs
  tinytiger::tt_voting_districts
  birdie::birdie
  baf::baf
}
