#' Filter Lineage Data for Clustering
#'
#' This function filters lineage frequency data to retain only dominant and persistent barcodes
#' suitable for clustering. It removes barcodes that do not meet a specified minimum mean frequency
#' and a minimum number of time points with non-zero frequency. The function saves two CSV files:
#' one with all original barcodes and one with the filtered set.
#'
#' @param input_df A data frame containing the input data. It must have columns `ID`, `Time`, and `Reads`.
#' @param freq_threshold A numeric value specifying the minimum mean frequency required to retain a barcode.
#' @param time_threshold An integer specifying the minimum number of time points where the barcode's frequency is non-zero.
#' @param output_directory A string specifying the directory where plots will be saved.
#' @param input_name A string used as the base name for output files (e.g., "replicate1").
#'
#' @return A data frame containing the ID, relative frequency at each time point, mean frequency, and number of non-zero time points for each retained barcode.
#' @export
#' @name filterData
#' 
#' @examples
#' # Load demo barcode count data (installed with the package)
#' demo_file <- system.file("extdata", "demo_input.csv", package = "doblin")
#' input_dataframe <- readr::read_csv(demo_file, show_col_types = FALSE)
#'
#' # Apply filtering to retain dominant and persistent barcodes
#' filtered_df <- filterData(
#'   input_df = input_dataframe,
#'   freq_threshold = 0.00005,        
#'   time_threshold = 5,            
#'   output_directory = tempdir(),  
#'   input_name = "demo"            
#' )

filterData <- function(input_df, 
                       freq_threshold, 
                       time_threshold,
                       output_directory,
                       input_name){

  sample = reshape2::dcast(input_df, ID ~ Time, value.var = 'Reads')

  m = as.matrix(sample[,-1])
  mat = as.data.frame(sweep(m,2,colSums(m,na.rm = TRUE),`/`))
  mat$ID = sample$ID
  mat[,"mean"] = apply(mat[,-ncol(mat)],1, mean,na.rm=TRUE)

  z = is.na.data.frame(mat)
  mat[z]=0
  mat$points = apply(mat[,-c(ncol(mat)-1, ncol(mat))], 1, function(c)sum(c!=0))

  readr::write_csv(mat,file=paste(output_directory, "/", input_name,"_unfiltered.csv",sep=""),col_names = TRUE) # Contains ALL the lineages from the input file

  sample.clustering = mat[mat$points>=time_threshold & mat$mean>=freq_threshold,]

  # Check if sample.clustering is empty
  if (nrow(sample.clustering) == 0) {
    stop("Error: With the given time & frequency thresholds, there is no eligible
         data for clustering. Please consider adjusting your thresholds.")
  } else {
    # Write sample.clustering to CSV file
    readr::write_csv(sample.clustering, file = paste(output_directory, "/", input_name, "_filtered.csv", sep = ""), col_names = TRUE) # Contains ONLY the "dominant" and "persistent" lineages
  }

  return(sample.clustering)
}
