## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  tidy = TRUE
)

## ----eval=FALSE---------------------------------------------------------------
#  library(SticsRFiles)

## ----include=FALSE------------------------------------------------------------
suppressWarnings(library(SticsRFiles))
suppressWarnings(library(tibble))

load_param_names <- function() {
  tibble::as_tibble(
    read.csv2(file = "param_names.csv", stringsAsFactors = FALSE)
    )
}

## ----echo=FALSE---------------------------------------------------------------
stics_version <- get_stics_versions_compat()$latest_version
examples_path <- get_examples_path("xml", stics_version = stics_version)

## ----setup, results="hide", echo=FALSE----------------------------------------
xml_root <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
xml_loc_dir <- file.path(xml_root, "XML")
if (!dir.exists(xml_loc_dir)) dir.create(xml_loc_dir)


## ----rm_files, results='hide', echo=FALSE-------------------------------------
files_list <- list.files(path = xml_loc_dir, pattern = "\\.xml$",
                         full.names = TRUE)
files_list
if (length(files_list))  {
  print("Removing files")
  file.remove(files_list)
}

## ----eval = TRUE, echo = FALSE, class.output="xml"----------------------------
par_xml <- file.path(examples_path, "param_gen.xml")
par <- readLines(par_xml)
idx_water <- grep(pattern = "formalisme", par)[9:10]

## ----results='asis', echo = FALSE---------------------------------------------
cat("```xml\n")
cat(paste(par[idx_water[1]:idx_water[2]], collapse = "\n"))
cat("\n```\n")

## ----eval = TRUE, echo = FALSE, class.output="xml"----------------------------
sol_xml <- file.path(examples_path, "sols.xml")
sol <- readLines(sol_xml)
lines_sol <- c(sol[3], "...", sol[70:79], "...", sol[110:119], "...")

## ----results='asis', echo = FALSE---------------------------------------------
cat("```xml\n")
cat(paste(lines_sol, collapse = "\n"))
cat("\n```\n")

## ----eval = TRUE, echo = FALSE, class.output="xml"----------------------------
tec_xml <- file.path(examples_path, "file_tec.xml")
tec <- readLines(tec_xml)[3:22]
tec <- c(tec, "...", tec[12:20], "...")
tec[23] <- gsub(pattern = "112", replacement = "220", tec[23])

## ----results='asis', echo = FALSE---------------------------------------------
cat("```xml\n")
cat(paste(tec, collapse = "\n"))
cat("\n```\n")

## ----set_xml_dir, echo = FALSE------------------------------------------------
xml_dir <- get_examples_path(file_type = "xml", stics_version = stics_version)


## ----eval=FALSE---------------------------------------------------------------
#  xml_dir <- get_examples_path(file_type = "xml", stics_version = stics_version)
#  
#  # For linux
#  #> "/path/to/user/R/x86_64-pc-linux-gnu-library/3.6/SticsRFiles/
#  #> extdata/xml/examples/V10.2.0"
#  
#  # For windows
#  #> "C:/Users/username/Documents/R/win-lib/3.6/SticsRFiles/
#  #> extdata/xml/examples/V10.2.0"
#  

## -----------------------------------------------------------------------------

xml_files <- list.files(path = xml_dir, pattern = ".xml$", full.names = TRUE)

# Listing only the first three files of the entire list

# For linux
#> [1] "/path/to/user/R/x86_64-pc-linux-gnu-library/3.6/SticsRFiles/extdata/xml
#> /examples/V10.2.0/file_ini.xml"
#> [2] "/path/to/user/R/x86_64-pc-linux-gnu-library/3.6/SticsRFiles/extdata/xml
#> /examples/V10.2.0/file_plt.xml"
#> [3] "/path/to/user/R/x86_64-pc-linux-gnu-library/3.6/SticsRFiles/extdata/xml
#> /examples/V10.2.0/file_sta.xml"

# For windows
#> [1] "C:/Users/username/Documents/R/win-lib/3.6/SticsRFiles/extdata/xml/
#> examples/V10.2.0/file_ini.xml"
#> [2] "C:/Users/username/Documents/R/win-lib/3.6/SticsRFiles/extdata/xml/
#> examples/V10.2.0/file_plt.xml"
#> [3] "C:/Users/username/Documents/R/win-lib/3.6/SticsRFiles/extdata/xml/
#> examples/V10.2.0/file_sta.xml"


## ----echo=FALSE, results="hide"-----------------------------------------------
files_list <- list.files(path = xml_dir, pattern = ".xml$")
legend <- c("initializations", "plant", "station", "crop management",
            "general", "general (new formalisms)", "soils", "usms")
dt <- data.frame(files = files_list, groups = legend)


## ----echo = FALSE-------------------------------------------------------------
knitr::kable(dt)
#, caption = "Correspondence between XML example files names
# and parameters groups")


## -----------------------------------------------------------------------------
# Setting a local directory path
# xml_loc_dir <- "/path/to/local/directory"

file.copy(from = file.path(xml_dir, "sols.xml"),
          to = file.path(xml_loc_dir, "sols.xml"), overwrite = TRUE)

## -----------------------------------------------------------------------------

file.copy(from = xml_files, to = xml_loc_dir)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(read.csv(file = "param_files_keywords.csv", sep = ";"), col.names = c("keyword", "xml file", "parameter kind"))

## ----eval = FALSE-------------------------------------------------------------
#  
#  param_names <- get_param_info()
#  
#  head(param_names)
#  

## ----echo = FALSE-------------------------------------------------------------
param_names <- get_param_info()

head(param_names)

## ----echo=FALSE---------------------------------------------------------------
# Displaying the returned data as a paged table
rmarkdown::paged_table(param_names)

## ----eval = FALSE-------------------------------------------------------------
#  
#  get_param_info(param = "albedo")
#  
#  get_param_info(param = c("albedo", "latitude", "humcapil"))
#  

## ----echo = FALSE-------------------------------------------------------------
param_names[grep("albedo", param_names$name), ]

idx <- grepl("albedo", param_names$name) |
  grepl("latitude", param_names$name) |
  grepl("humcapil", param_names$name)

param_names[idx, ]


## -----------------------------------------------------------------------------

get_param_info(param = "hum")


param_names <- get_param_info(param = c("alb", "hum"))


## ----echo=FALSE---------------------------------------------------------------
# Displaying the returned data as a paged table
rmarkdown::paged_table(param_names)

## ----warning = FALSE, eval = FALSE--------------------------------------------
#  
#  get_param_info(keyword = "hum")
#  
#  
#  param_names <- get_param_info(keyword = c("alb", "hum"))
#  
#  

## ----echo = FALSE-------------------------------------------------------------
param_names <- get_param_info(keyword = c("alb", "hum"))


## ----warning=FALSE, echo = FALSE----------------------------------------------
# Displaying the returned data as a paged table
rmarkdown::paged_table(param_names)


## -----------------------------------------------------------------------------
# Fixing files paths
sols <- file.path(xml_loc_dir, "sols.xml")
par_gen <- file.path(xml_loc_dir, "param_gen.xml")

# A option parameter
get_param_xml(par_gen, param = "codeactimulch")

# A simple parameter
get_param_xml(par_gen, param = "tnitopt")

# Using a conditional selection
get_param_xml(sols, param = "argi", select = "sol",
              select_value = "solcanne")


## -----------------------------------------------------------------------------

# For all soils

get_param_xml(sols, param = "argi")

## -----------------------------------------------------------------------------

# For all soils
get_param_xml(sols, param = c("argi", "pH"))

## -----------------------------------------------------------------------------

# For one soil
get_param_xml(sols, param = c("argi", "pH"), select = "sol",
              select_value = "solcanne")

## -----------------------------------------------------------------------------
# For all soil layers
get_param_xml(sols, param = c("epc", "infil"))

## -----------------------------------------------------------------------------
# For all soil layers
get_param_xml(sols, param = c("epc", "infil"), select = "sol",
              value = "solcanne")

## -----------------------------------------------------------------------------
# For all irrigation supplies
tec <- file.path(xml_loc_dir, "file_tec.xml")
get_param_xml(tec, param = c("julapI_or_sum_upvt", "amount"))


## -----------------------------------------------------------------------------
# For soil layers 1 to 3
get_param_xml(sols, param = c("epc", "infil"), select = "sol",
              select_value = "solcanne", ids = 1:3)

## -----------------------------------------------------------------------------

# For irrigation operations 1 to 5
get_param_xml(tec, param = c("julapI_or_sum_upvt", "amount"), ids = 1:5)


## -----------------------------------------------------------------------------
tec_param_values <- get_param_xml(tec)[[1]]
#
# Displaying only a subset of the list
head(tec_param_values, n = 10)


## -----------------------------------------------------------------------------
param_values <- get_param_xml(c(tec, sols))

# Files list names
names(param_values)

# param_values extract of 5 elements from each file sub-list
head(param_values$file_tec.xml, n = 5)

head(param_values$sols.xml, n = 5)

## -----------------------------------------------------------------------------


## An option parameter

# Initial value
get_param_xml(par_gen, param = "codeactimulch")

# Setting a new one
set_param_xml(par_gen, param = "codeactimulch", values = 2, overwrite = TRUE)

# Controlling the written value
get_param_xml(par_gen, param = "codeactimulch")



## A simple parameter

# Initial value
get_param_xml(par_gen, param = "tnitopt")

# Setting a new one
set_param_xml(par_gen, param = "tnitopt", values = 29.5, overwrite = TRUE)

# Controlling written value
get_param_xml(par_gen, param = "tnitopt")


## Using a conditional selection

# Initial value
get_param_xml(sols, param = "argi", select = "sol",
              select_value = "solcanne")

# Setting a new one
set_param_xml(sols, param = "argi", values = 33, select = "sol",
              select_value = "solcanne", overwrite = TRUE)

# Controlling written value
get_param_xml(sols, param = "argi", select = "sol",
              select_value = "solcanne")


## -----------------------------------------------------------------------------

## For all soils
soils_number <- length(get_soils_list(sols))

# Initial values
get_param_xml(sols, param = "argi")

# One value per occurence
set_param_xml(sols, param = "argi", values = list(1:soils_number),
              overwrite = TRUE)

# Controlling written values
get_param_xml(sols, param = "argi")

# Setting the same value for all occurences
set_param_xml(sols, param = "argi", values = 40, overwrite = TRUE)

# Controlling written values
get_param_xml(sols, param = "argi")



## -----------------------------------------------------------------------------

## For all soils
soils_number <- length(get_soils_list(sols))

# Initial values

# Setting one value per parameters occurence
set_param_xml(sols, param = list("argi", "pH"),
              values = list(1:soils_number, soils_number:1),
              overwrite = TRUE)

# Controlling written values
get_param_xml(sols, param = c("argi", "pH"))

# Setting the same value for all occurences
set_param_xml(sols, param = c("argi", "pH"), values = list(50, 8),
              overwrite = TRUE)

# Controlling written values
get_param_xml(sols, param = c("argi", "pH"))



## -----------------------------------------------------------------------------

## For one soil

# Initial values
get_param_xml(sols, param = c("argi", "pH"), select = "sol",
              select_value = "solcanne")

# Setting new values
set_param_xml(sols, param = c("argi", "pH"), values = list(50, 8),
              select = "sol", select_value = "solcanne", overwrite = TRUE)

# Controlling written values
get_param_xml(sols, param = c("argi", "pH"), select = "sol",
              select_value = "solcanne")

## -----------------------------------------------------------------------------
## For all soil layers

# Initial values
get_param_xml(sols, param = c("epc", "infil"), select = "sol",
              select_value = "solcanne")

# Setting new values
set_param_xml(sols, param = c("epc", "infil"), values = list(18:22, 48:52),
              select = "sol", select_value = "solcanne", overwrite = TRUE)

# Controlling written values
get_param_xml(sols, param = c("epc", "infil"), select = "sol",
              select_value = "solcanne")

## -----------------------------------------------------------------------------
## For all irrigation operations
tec <- file.path(xml_loc_dir, "file_tec.xml")

# Initial values
get_param_xml(tec, param = c("julapI_or_sum_upvt", "amount"))

# Setting new values
set_param_xml(tec, param = c("julapI_or_sum_upvt", "amount"),
              values = list(200:215, 20:35), overwrite = TRUE)

# Controlling written values
get_param_xml(tec, param = c("julapI_or_sum_upvt", "amount"))


## -----------------------------------------------------------------------------
## For soil layers 1 to 3

# Initial values
get_param_xml(sols, param = c("epc", "infil"), select = "sol",
              select_value = "solcanne", ids = 1:3)

# Setting new values
set_param_xml(sols, param = c("epc", "infil"), values = list(20:18, 50:48),
              select = "sol", select_value = "solcanne", overwrite = TRUE,
              ids = 1:3)

# Controlling written values

get_param_xml(sols, param = c("epc", "infil"), select = "sol",
              select_value = "solcanne", ids = 1:3)

## -----------------------------------------------------------------------------

## For irrigation operations 1 to 5 (same indices for all parameters)

# Initial values
get_param_xml(tec, param = c("julapI_or_sum_upvt", "amount"))

# Setting new values
set_param_xml(tec, param = c("julapI_or_sum_upvt", "amount"),
              values = list(204:200, 24:20), overwrite = TRUE, ids = 1:5)

# Controlling written values
get_param_xml(tec, param = c("julapI_or_sum_upvt", "amount"))

