#' Extract database contents
#'
#' @param territory [`character(.)][character]\cr combination of column name in
#'   the ontology and value to filter that column by to build a tree of the
#'   territories nested into it.
#' @param concept description
#' @param variable description
#' @param level description
#' @param year description
#' @return returns ...
#' @examples
#' if(dev.interactive()){
#' adb_example(path = paste0(tempdir(), "/newDB"))
#'
#' adb_querry(territory = list(al1 = "a_nation"),
#'            concept = list(commodity = "barley"),
#'            variable = "harvested")
#' }
#' @importFrom checkmate assertList assertCharacter assertIntegerish
#'   assertLogical assertSubset
#' @importFrom dplyr mutate select
#' @importFrom ontologics get_class make_tree get_concept
#' @importFrom sf st_read
#' @export

adb_querry <- function(territory = NULL, concept = NULL, variable = NULL,
                       level = NULL, year = NULL){

  assertList(x = territory, types = "character", any.missing = FALSE, null.ok = TRUE)
  assertList(x = concept, types = "character", any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = variable, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = level, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = year, min.len = 1, any.missing = FALSE, null.ok = TRUE)

  adb_path <- getOption(x = "adb_path")
  inv <- readRDS(paste0(adb_path, "/_meta/inventory.rds"))

  gazPath <- paste0(getOption(x = "gazetteer_path"))
  gazClasses <- get_class(ontology = gazPath)
  topClass <- paste0(getOption(x = "gazetteer_top"))

  inv_tables <- inv$tables
  inv_geoms <- inv$geometries
  inv_series <- inv$dataseries

  tables <- list.files(path = paste0(adb_path, "/tables/stage3"), full.names = TRUE)
  geometries <- list.files(path = paste0(adb_path, "/geometries/stage3"), full.names = TRUE, pattern = "\\.gpkg$")

  # first, get all items down until the "topClass"
  allItems <- make_tree(class = topClass, reverse = TRUE, ontology = gazPath)
  top <- get_concept(class = names(territory), label = territory[[1]], ontology = gazPath) %>%
    pull(id)

  # then select those that are determined by "broadest"
  fin <- NULL
  outIDs <- top
  while(is.null(fin)){
    childID <- allItems %>%
      filter(has_broader %in% top) %>%
      pull(id)
    if(length(childID) != 0){
      top <- childID
      outIDs <- c(outIDs, childID)
    } else {
      fin <- TRUE
    }
  }

  tableNames <- allItems |>
    filter(id %in% outIDs & class == "al1")

  metaNames <- c("sources", "years", "min_year", "max_year")

  for(i in seq_along(tableNames$label)){

    nation <- tableNames$label[i]

    table <- readRDS(file = tables[str_detect(string = tables, pattern = paste0(nation, ".rds"))])
    theConcepts <- table |>
      distinct(!!sym(names(concept))) |>
      pull(names(concept))
    theConcepts <- theConcepts[!is.na(theConcepts)]
    theConcept <- unlist(concept, use.names = FALSE)

    assertSubset(x = variable, choices = colnames(table))
    assertSubset(x = names(concept), choices = colnames(table))
    assertSubset(x = theConcept, choices = c(theConcepts, metaNames), .var.name = paste0('concept = list(', names(concept), ' = "', theConcept, '")'))

    # build metadata
    tabIDs <- table |>
      distinct(tabID)

    temp_inv_tables <- inv_tables |>
      filter(tabID %in% tabIDs$tabID)

    if(is.null(level)){
      tempLvls <- temp_inv_tables |>
        distinct(level) |>
        pull()
    } else {
      assertSubset(x = level, choices = unique(temp_inv_tables$level))
      tempLvls <- level
    }

    if(is.null(year)){
      targetYear <- table |>
        distinct(year) |>
        pull(year)
      # targetYear <- get all years and provide a prompt to ask whether all should be printed or just a subset
    } else {
      assertSubset(x = as.character(year), choices = unique(table$year))
      targetYear <- as.character(year)
    }

    # load geometries
    geometry <- suppressMessages(
      read_sf(dsn = geometries[str_detect(string = geometries, pattern = paste0(nation, ".gpkg"))],
              layer = max(tempLvls))) |>
      filter(geoID %in% temp_inv_tables$geoID)

    # build some meta data
    meta <- table |>
      group_by(gazID, geoID) |>
      summarise(n_source = n_distinct(tabID),
                !!paste0("n_", names(concept)) := n_distinct(!!sym(names(concept))),
                n_year = n_distinct(year),
                min_year = min(year),
                max_year = max(year)) |>
      ungroup()

    # summarise variables
    var <- table |>
      filter(!is.na(gazID)) |>
      filter(year %in% targetYear) |>
      # filter(str_detect(ontoMatch, "close")) |>
      pivot_wider(id_cols = c(gazID, year), names_from = names(concept), values_from = all_of(variable), values_fn = mean)

    out <- list(geom = geometry,
                tab = var,
                meta = meta)

    return(out)

    # plotSteps <- function(x){
    #
    #  thisVar <- var |>
    #    filter(year == x)
    #
    #   # put it together
    #  full <- geometry |>
        # filter(geoID %in% temp_inv_tables$geoID) |>
        # left_join(meta, by = c("gazID", "geoID")) |>
        # left_join(thisVar, by = "gazID")

    #   thisGeoID <- full |>
    #     as_tibble() |>
    #     distinct(geoID)
    #
    #   temp_inv_geoms <- inv_geoms |>
    #     filter(geoID %in% thisGeoID$geoID)
    #   temp_inv_series <- inv_series |>
    #     filter(datID %in% temp_inv_geoms$datID)
    #
    #   if(theConcept %in% metaNames){
    #     titleString <- theConcept
    #   } else {
    #     titleString <- paste0(theConcept, " in ", x)
    #   }
    #
    #   p <- ggplot(full) +
    #     geom_sf(aes(fill = !!sym(theConcept)), lwd = 0.05, color = "white") +
    #     scale_fill_gradientn(
    #       colours = c("#F1E4DBFF", "#D7C9BEFF", "#8B775FFF", "#9AA582FF", "#657359FF"),
    #       limits = c(1, max(var[theConcept], na.rm = TRUE)),
    #       name = variable,
    #       transform = "log10"
    #     ) +
    #     labs(
    #       title = titleString,
    #       subtitle = paste0(nation, " at admin level ", max(tempLvls)),
    #       caption = paste0("Source: ", temp_inv_series$description, " (", temp_inv_series$homepage, ")")
    #     ) +
    #     theme_map() +
    #     theme(
    #       legend.position = "top",
    #       legend.justification = 0.5,
    #       legend.key.width = unit(1.75, "cm"),
    #       legend.margin = margin(),
    #       plot.title = element_text(size = 20, hjust = 0.5),
    #       plot.subtitle = element_text(hjust = 0.5)
    #     )
    #   print(p)
    # }

    # if(animate){
    #
    #   saveGIF(for(j in seq_along(targetYear)){
    #     plotSteps(targetYear[j])
    #   }, interval = 1, movie.name = paste0(adb_path, "/", nation, "_", theConcept, "_", min(year), "_", max(year), ".gif"))
    #
    # } else {
    #
    #   for(j in seq_along(targetYear)){
    #     plotSteps(targetYear[j])
    #   }
    #
    # }

  }
}
