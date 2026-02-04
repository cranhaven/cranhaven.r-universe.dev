#' Plot LTR z-scores on map
#' 
#' Plots the results from LRT on a map based on lat/lon info in the database.
#' If no location is found in the data (e.g. using \code{simulte_pops}) nothing is plotted.
#' 
#' @author Torben Tvedebrink, \email{tvede@@math.aau.dk}
#' @param data The output from the \code{genogeo} function
#' @return A map with population z-scores at their geographic origin
#' @export
#' @examples
#' df_ <- simulate_pops(pop_n = 4, aims_n = 50)
#' df_db <- pops_to_DB(df_)
#' profile <- random_AIMs_profile(df_db, keep_pop = TRUE)
#' profile$pop[1] # The true population
#' result <- genogeo(profile[,c("locus","x0")], df = df_db, min_n = 0) 
#' result$lon <- runif(n = 4, min = -125, max = 125)
#' result$lat <- runif(n = 4, min = -50, max = 80)
#' \dontrun{map_plot(result)}
map_plot <- function(data){
  if(names(data)[1] == "pop") return(map_pop_plot(data))
  else if(names(data)[1] == "meta") return(map_meta_plot(data))
  else return(NULL)
}

map_pop_plot <- function(data){
  if(!all(c("lat", "lon") %in% names(data))){
    stop("No latitude (lat) or longitude (lon) information available in data")
  }
  ## build fixes : start ##
  selected_ <- NULL
  lon <- NULL
  lat <- NULL
  LR_listing <- NULL
  group <- NULL
  long <- NULL
  ## build fixes : end ##
  if(!"LR_listing" %in% names(data)) data <- data %>% mutate(LR_listing = "No")
  if(!"selected_" %in% names(data)) data <- data %>% mutate(selected_ = FALSE)
  data <- data %>% mutate(selected_ = ifelse(selected_, "Yes", "No"))
  maplot <- ggplot(data, ### Points
                   aes(x=lon, y = lat, group = labs, size = n, fill = labs, colour = selected_, shape = LR_listing))
  world_map <- maps::map("world", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% fortify()
  ## world_map <- map_data("world")
  maplot <- maplot +
    geom_polygon(data = world_map %>% rename(lon = long), ### World map
                 aes(group=group, size = NULL, colour=NULL, shape = NULL),
                 colour="gray80",fill="gray90", show.legend = FALSE)
  maplot <- maplot + labs(x="Longitude",y="Latitude") +
    guides(fill=FALSE, alpha=FALSE, stroke=FALSE, shape = FALSE, colour = FALSE, size = FALSE) +
    scale_fill_manual(values = bar_colour(data[,c("logP","accept","labs")])) +
    scale_shape_manual("In LR list", values = c("No" = 21, "Yes" = 24)) + 
    scale_colour_manual("Brushed points", values = c("No" = "#000000", "Yes" = "#FFDE00"))
  # if(nrow(data %>% distinct(LR_listing))==1) maplot <- maplot + guides(shape = FALSE)
  # if(nrow(data %>% distinct(selected_))==1) maplot <- maplot + guides(colour=FALSE)
  maplot <- maplot + geom_point()
  ## list(plot = maplot)
  maplot
}

map_meta_plot <- function(data){ ## data is assumed to be KK164 - for non-meta it is result
  ## build fixes : start ##
  lon <- NULL
  lat <- NULL
  meta <- NULL
  LR_listing <- NULL
  group <- NULL
  long <- NULL
  hulls_meta <- NULL
  hulls_meta_lon <- NULL
  hulls_meta_lat <- NULL
  ## build fixes : end ##
  if(!all(c("lat", "lon") %in% names(data))){
    stop("No latitude (lat) or longitude (lon) information available in data")
  }
  if(!"LR_listing" %in% names(data)) data <- data %>% mutate(LR_listing = "No")
  ## Compute convex hulls for meta populations
  meta_cols <- bar_colour(data[,c("logP", "accept", "meta")])
  meta_plot <- ggplot(data, 
                      aes(x=lon, y = lat, fill = meta, colour = meta, shape = LR_listing))
  ## Adding the map
  world_map <- maps::map("world", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% fortify()
  ## world_map <- map_data("world")
  meta_plot <- meta_plot + geom_polygon(data=world_map %>% rename(lon = long), ### World map
                                        aes(group=group, size = NULL, fill = NULL, colour = NULL, shape = NULL),
                                        colour="gray80",fill="gray90", show.legend = FALSE)
  meta_plot <- meta_plot + labs(x="Longitude",y="Latitude") +
    ## Adding the convex hull and corners
    geom_polygon(data = data %>% unnest(hulls_meta, .sep = "_"), 
                 aes(x = hulls_meta_lon, y = hulls_meta_lat, shape = NULL), 
                 show.legend = FALSE, alpha = 0.75, size = 1.25) + 
    ## scale_colour_brewer(palette = "YlGn") +
    scale_colour_manual(values = meta_cols) + 
    scale_shape_manual("In LR list", values = c("No" = 21, "Yes" = 24)) + 
    scale_fill_manual(values = meta_cols) + guides(fill = FALSE, colour = FALSE, shape = FALSE)
  ## if(nrow(data %>% distinct(LR_listing))==1) meta_plot <- meta_plot + guides(shape = FALSE)
  meta_plot <- meta_plot + geom_point(aes(colour = NULL), size = 2, stroke = 1, colour = "black")
##  list(plot = meta_plot, rr = rr)
  meta_plot
}

convex_hulls <- function(db, grouping = "meta"){
  ## build fixes : start ##
  . <- NULL
  out_of_place <- NULL
  lat <- NULL
  ## build fixes : end ##
  grouping_ <- grep(grouping, names(db), value = TRUE)
  db %>% 
    select_("pop", "population", grouping_, "n", "lat", "lon", "out_of_place") %>% 
    distinct() %>% 
    filter(!out_of_place) %>% select(-out_of_place) %>% 
    filter(!is.na(lat)) %>% 
    split(.[[grouping]]) %>% purrr::map_df(~.x[chull(x = .x$lon, y = .x$lat), ] %>% mutate(n_row = n()))
}

