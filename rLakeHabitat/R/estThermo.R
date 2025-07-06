  #' Estimate Average Thermocline Depth
  #'
  #' Estimate average thermocline depth across multiple sites and dates.
  #'
  #' @param data data frame of water column temperature profiles
  #' @param site character giving the name of the site column
  #' @param date character giving the name of the date column
  #' @param depth character giving the name of the depth column
  #' @param temp character giving the name of the temp column
  #' @param combine logical indicating whether or not to average across sites ("sites"), dates ("dates"), or sites and dates ("all"), default = "all"
  #' @return either numeric value of average thermocline depth, standard deviation, and n or data frame of thermocline depths, standard deviations, and n across sites or dates
  #' @author Tristan Blechinger, Department of Zoology & Physiology, University of Wyoming
  #' @export
  #' @import dplyr
  #' @import rLakeAnalyzer
  #' @examples
    #' # load test profile data
    #' data <-  read.csv(system.file("extdata", "example_profile_data.csv", package = 'rLakeHabitat'))
    #' data$date <- base::as.Date(data$date)
    #' #run function
    #' estThermo(data = data, site = "site", date = "date",
    #' depth = "depth", temp = "temp", combine = "all")

estThermo <- function(data, site, date, depth, temp, combine = "all"){

  #checks
  if(!inherits(data, "data.frame"))
    stop("data must be a data.frame!")
  if(!inherits(site, "character"))
    stop("site must be a character giving the site column name")
  if(site %in% names(data) == FALSE)
    stop("The value of site does not appear to be a valid column name")
  if(!inherits(date, "character"))
    stop("date must be a character giving the date column name")
  if(date %in% names(data) == FALSE)
    stop("The value of date does not appear to be a valid column name")
  if(!inherits(data[, date], "Date"))
    stop("data in date column is not formatted as date")
  if(!inherits(depth, "character"))
    stop("depth must be a character giving the depth column name")
  if(depth %in% names(data) == FALSE)
    stop("The value of depth does not appear to be a valid column name")
  if(!inherits(data[, depth], "numeric"))
    stop("data in depth column is not numeric")
  if(!inherits(temp, "character"))
    stop("temp must be a character giving the temp column name")
  if(temp %in% names(data) == FALSE)
    stop("The value of temp does not appear to be a valid column name")
  if(!inherits(data[, temp], "numeric"))
    stop("data in temp column is not numeric")
  if (!is.character(combine))
    stop("combine must be either 'sites', 'dates', or 'all'")
  if(!combine %in% c("sites", "dates", "all"))
    stop("combine must be either 'sites', 'dates', or 'all'")

  #combine columns
  profile_data <- data %>%
    dplyr::select(all_of(c(site, date, depth, temp)))

  #split data by site and date
  prof_splits <- base::split(profile_data, list(profile_data$site, profile_data$date))

  #empty list to store output
  thermos <- list()

  #calculate thermo depth for each profile
  for (i in 1:length(prof_splits)) {

    #subset data from list
    subset <- prof_splits[[i]]

    #remove empty subsets
    if(nrow(subset) > 0){

    #remove duplicate depth values
    subset <- subset %>% dplyr::distinct(depth, .keep_all = T)

    #turn depth and temp values into vectors
    wtr <- base::as.vector(subset$temp)
    depths <- base::as.vector(subset$depth)

    #calculate thermocline depth
    output <- rLakeAnalyzer::thermo.depth(wtr, depths, seasonal = F)

    #format output and save to output list
    output_df <- as.data.frame(list(subset[1,1], subset[1,2], output))
    colnames(output_df) <- c("Site", "Date", "Thermo")

    thermos[[i]] <- output_df
    }
  }

  #return all thermocline depths, calculate mean thermocline depth
  all_thermos <- base::do.call(rbind, thermos)

  if(combine == "all"){
    all <- all_thermos %>%
      dplyr::summarize(mean = base::mean(all_thermos$Thermo),
                sd = stats::sd(all_thermos$Thermo),
                n = nrow(all_thermos))
    return(all)
    #return(c(mean(all_thermos$Thermo, na.rm = T), sd(all_thermos$Thermo, na.rm = T), nrow(all_thermos)))
  }
  if(combine == "sites"){
    grouped_thermos <- all_thermos %>%
      dplyr::group_by(all_thermos$Site)%>%
      dplyr::summarize(meanDepth = base::mean(all_thermos$Thermo, na.rm = T),
                sdDepth = stats::sd(all_thermos$Thermo, na.rm = T),
                n = n())
    return(grouped_thermos)
  }
  if(combine == "dates"){
    grouped_thermos <- all_thermos %>%
      dplyr::group_by(all_thermos$Date)%>%
      dplyr::summarize(meanDepth = base::mean(all_thermos$Thermo, na.rm = T),
                sdDepth = stats::sd(all_thermos$Thermo, na.rm = T),
                n = n())
    return(grouped_thermos)
  }
}
