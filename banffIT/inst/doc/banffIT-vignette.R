## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# 
# # To install banffIT
# install.packages('banffIT')
# 
# library(banffIT)
# # If you need help with the package, please use:
# banffIT_website()
# 
# # use example
# version <- 2022
# input_file <- system.file("extdata", paste0(version,"/banff_example.xlsx"),
#                           package = "banffIT")
# banff_launcher(
#   input_file = input_file,
#   output_folder = tempdir(), # 'folder_path/example'
#   version = version,
#   language = 'label:en',
#   option_filter = adequacy == 1,
#   detail = TRUE)
# 

