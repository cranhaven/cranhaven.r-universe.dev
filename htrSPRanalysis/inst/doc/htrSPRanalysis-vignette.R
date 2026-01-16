## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 10
)

## -----------------------------------------------------------------------------
library(htrSPRanalysis)

## ----warning=FALSE, message=FALSE---------------------------------------------
# CRAN has strict limits on package sizes, and the titration files tend to be
#large. Therefore, we will need to download the titration data from the gitlab 
#repository. The following will download necessary files from gitlab onto
#your computer.

# By default, the files will go to the temp directory for this R session. 
# To change this, change the filename in destfile 

destfile <- file.path(tempdir(),"titration_data.xlsx")

download.file("https://gitlab.oit.duke.edu/janice/htrspranalysis/-/raw/master/inst/extdata/titration_data.xlsx?ref_type=heads",
              destfile = destfile, mode = "wb")

sample_sheet_path <- system.file("extdata",
                                 "sample_sheet.xlsx",
                                 package="htrSPRanalysis")

data_file_path <- destfile

output_file_path <- tempdir()

# process the input
processed_input <- process_input(sample_sheet_path,
                    data_file_path,
                    num_cores = 2,
                    output_file_path = output_file_path)



## -----------------------------------------------------------------------------
plots_before_processing <- get_plots_before_baseline(processed_input)


## -----------------------------------------------------------------------------
plots_before_processing[[1]]

## -----------------------------------------------------------------------------
fits_list <- get_fits(processed_input)

## -----------------------------------------------------------------------------
plot_list <- get_fitted_plots(processed_input, fits_list)

## -----------------------------------------------------------------------------
plot_list[[1]]

## -----------------------------------------------------------------------------
rc_list <- get_rc_plots(processed_input)


## -----------------------------------------------------------------------------
rc_list[[5]]

## -----------------------------------------------------------------------------
pdf_pages <- create_pdf(processed_input, fits_list, rc_list, plot_list)

## -----------------------------------------------------------------------------
parameter_table <- create_csv(processed_input, fits_list)

## -----------------------------------------------------------------------------

sample_sheet_path <- system.file("extdata", "bulkshift_example_sample_sheet.xlsx", package="htrSPRanalysis")
data_file_path <- system.file("extdata", "bulkshift_example_titration_data.xlsx", package="htrSPRanalysis")

# process the input

pdfpath <- tempdir()


bulkshift_example <- process_input(sample_sheet_path = sample_sheet_path,
                data_file_path = data_file_path,
                num_cores = 2, 
                output_file_path = pdfpath)

bulkshift_plots_before_processing <- get_plots_before_baseline(bulkshift_example)

bulkshift_fits <- get_fits(bulkshift_example)

bulkshift_plots <- get_fitted_plots(processed_input = bulkshift_example,
                                    fits_list = bulkshift_fits)

bulkshift_rc <- get_rc_plots(processed_input = bulkshift_example)


## -----------------------------------------------------------------------------
bulkshift_plots_before_processing[[1]]

bulkshift_plots[[1]]

bulkshift_rc[[1]]

create_pdf(processed_input = bulkshift_example, 
           fits_list = bulkshift_fits,
           rc_list = bulkshift_rc)

create_csv(processed_input = bulkshift_example, 
           fits_list = bulkshift_fits)

