#' Import  a single ScioSpec data reading
#'
#' @param cur_file The file to import
#' @param shear  Is the chip for shear stress (Defalt FALSE)
#'
#' @returns A vascr dataset containing data from a single sciospec reading
#' 
#' @importFrom stringr str_count
#' @importFrom memoise memoise
#' 
#' @noRd
#'
#' @examples
#' 
#' cur_file = system.file('extdata/instruments/ScioSpec/d1/d1/ECISadapter1/demoexperiment1_00001.spec', package = 'vascr')
#' 
#' import_sciospec_single(cur_file)
#' 
#' cur_file = file_tree[[1]]
#' 
import_sciospec_single = memoise({function(cur_file, shear = FALSE){
  
  vascr_validate_file(cur_file, "spec")
  
  rlang::check_installed(c("data.table", "readr"), reason = "is needed to deal with the ScioSpec data format`")
  
  suppressMessages({
    cur_dat = data.table::fread(cur_file, showProgress = FALSE)
    meta = readr::read_lines(cur_file, n_max = 8, progress = FALSE)
    
  })
  
  
  
  cur_dat$channel = meta[[which(str_count(meta, "Channel") == 1)]]
  cur_dat$time = meta[[which((str_count(meta, "p\\.m\\.$") + str_count(meta, "a\\.m\\.$")) ==1)]]
  
  return(cur_dat)
  
}
  
})




#' Import a ScioSpec folder of files
#'
#' @param data_path 
#' @param shear 
#' @param nth 
#' 
#' @importFrom dplyr mutate tribble left_join select distinct bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace
#'
#' @return A vascr dataframe
#' 
#' @noRd
#'
#' @examples
#' data_path = system.file('extdata/instruments/ScioSpec/', package = 'vascr')
#' import_sciospec(data_path, shear = FALSE)
#' import_sciospec(data_path, shear = TRUE)
#' 
#' data_path = "C:\\Users\\James Hucklesby\\ScioSpec\\250515 Sanity check\\20250516 14.33.47\\Day0Collagentest\\"
#' 
#' data_path = "C:\\Users\\jhuc964\\Documents\\vascr\\devel\\CRT ECIS\\SS v2 250428"
#' data_path = "~/SS v2 250428"
#' data_path = raw
import_sciospec = function(data_path, shear = FALSE, experiment = NA, nth = 1){
  
  # Replace the ~ in the data path with a wd
  data_path = str_replace(data_path, "~", getwd())
  
  rlang::check_installed(c("purrr", "data.table", "readr"), reason = "is needed to deal with the ScioSpec data format`")
  
  # Create the list of files to import, from the master file list
  
  file_tree = list.files(data_path, recursive = TRUE, full.names = TRUE, pattern = "spec")
  file_tree
  
  if(length(file_tree) == 0 ){
    vascr_notify("error", "No files found in designated folder, please check link")
    
  }
  
  file_tree = file_tree[seq(1,length(file_tree), nth)]
  
  # Run the reading from disk, predominantly using paralell purrr::map
  imp = purrr::map(file_tree, import_sciospec_single, .progress = TRUE) %>% bind_rows()
  
  # Clean up times
  
  times = imp %>% select("time") %>% distinct() %>%
    mutate (Time = (.data$time %>% 
                      str_replace("\\.-", "-") %>%
                      sub(":([^:]*)$", ".\\1", .) %>% 
                      str_replace("a.m.", "am") %>% 
                      str_replace("p.m.", "pm") %>%
                      strptime("%d-%B-%Y %I:%M:%OS %p")) %>%
              as.numeric())
  
  times
  
  # Map pins to actual physical locations, depending on the chip type
  
  scio_map = tribble(~channel, ~static, ~shear,
                     "Channel: ECISadapter 1", "D02", "F01",
                     "Channel: ECISadapter 2", "C02", "D01",
                     "Channel: ECISadapter 3", "B02", "B01",
                     "Channel: ECISadapter 4", "A02", "NC",
                     "Channel: ECISadapter 5", "A01", "NC",
                     "Channel: ECISadapter 6", "B01", "A01",
                     "Channel: ECISadapter 7", "C01", "C01",
                     "Channel: ECISadapter 8", "D01", "E01",
                     
                     "Channel: ECISadapter1", "D02", "F01",
                     "Channel: ECISadapter2", "C02", "D01",
                     "Channel: ECISadapter3", "B02", "B01",
                     "Channel: ECISadapter4", "A02", "NC",
                     "Channel: ECISadapter5", "A01", "NC",
                     "Channel: ECISadapter6", "B01", "A01",
                     "Channel: ECISadapter7", "C01", "C01",
                     "Channel: ECISadapter8", "D01", "E01"
  )
  
  
  if(shear)
  {
    scio_map$Well = scio_map$shear
  } else {
    scio_map$Well = scio_map$static
  }
  
  scio_map = scio_map %>% select("channel", "Well")
  
  # Misc cleanup to vascr interoperable fomat
  
  imp2 = imp %>% mutate(R = .data$`Re[Ohm]`, `Re[Ohm]`  = NULL) %>%
    mutate(I = .data$`Im[Ohm]`, `Im[Ohm]` = NULL) %>%
    mutate(Frequency = .data$`frequency[Hz]`, `frequency[Hz]` = NULL) %>%
    pivot_longer(c("R", "I"), names_to = "Unit", values_to = "Value") %>%
    left_join(scio_map) %>%
    mutate(Instrument = "sciospec") %>%
    left_join(times) %>%
    mutate(Experiment = "TEST", Sample = .data$Well) %>%
    mutate(Time = .data$Time - min(.data$Time)) %>%
    mutate(Time = .data$Time/60/60) %>%
    mutate(time = NULL) %>%
    mutate(Value = as.numeric(.data$Value)) %>%
    mutate(SampleID = 5) %>%
    mutate(Excluded = "no") %>%
    mutate(Experiment = experiment)
  
  return(imp2)
  
  
}

