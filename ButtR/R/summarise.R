

#' Summarise contents of the locally installed OzButterflies database
#'
#' @param save_folder Path of folder that contains the OzButterflies database.
#' @param imgExt Regular expression used to identify files to be counted as
#'   images. Default is `.DNG` or `.ARW` files which are the RGB and UV photos
#'   of specimens. DNG is the Adobe open Digital Negative format and used in
#'   version 4 (and above) of the database, while ARW is the Sony raw file
#'   format, and used in versions 1, 2 and 3.
#'
#' @returns Data frame with 1 row and columns that summarise the database
#'   contents. All summary statistics, apart from the `Images` count, describe
#'   the entire database, regardless of whether the entire database or a subset
#'   is installed locally.
#' @importFrom utils read.csv
#' @importFrom stats aggregate median
#'
#' @export
Oz_butterflies_summary <- function(save_folder = "OzButterflies", imgExt = "\\.DNG$|\\.dng$|\\.ARW$|\\.arw$") {
  # Read meta data
  descr <- read.csv(file.path(save_folder, "Oz_butterflies.csv"))

  imgs <- list.files(save_folder, pattern = imgExt, recursive = TRUE)

  # Individuals per species
  ips <- aggregate(list(Count = descr$ID), by = list(Species = descr$Binomial), FUN = length)

  data.frame(Families = length(unique(descr$Family)),
             Genera = length(unique(descr$Genus)),
             Species = length(unique(descr$Binomial)),
             Specimens = length(unique(descr$ID)),
             Females = sum(descr$Sex == "Female"),
             Males = sum(descr$Sex == "Male"),
             Images = length(imgs),
             Sites = length(unique(descr$Site)),
             "Ind./species max" = max(ips$Count),
             "Ind./species mean" = mean(ips$Count),
             "Ind./species median" = median(ips$Count),
             check.names = FALSE)
}
