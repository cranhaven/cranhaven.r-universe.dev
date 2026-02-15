library(SimSurvNMarker)
knit_example <- function(file_name){
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  if(dir.exists("inst/test-data"))
    setwd("inst/test-data")
  if(dir.exists("test-data"))
    setwd("test-data")

  source(file_name, local = TRUE)

  base_name <- gsub("(.+)(\\.R)$", "\\1", file_name)
  args_env$file_name <- base_name
  out <- rmarkdown::render(
      "run-example.Rmd", output_file = paste0(base_name, ".md"),
      envir = args_env, output_format = "github_document")
  file.remove(paste0(base_name, ".html"))
  invisible(out)
}

knit_example("w-all.R")
knit_example("one-marker.R")
knit_example("no-gamma.R")
knit_example("no-B.R")
knit_example("no-delta.R")

local({
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  if(dir.exists("inst/test-data"))
    setwd("inst/test-data")
  if(dir.exists("test-data"))
    setwd("test-data")

  rmarkdown::render("w-ids.Rmd")
  file.remove("w-ids.html")
  invisible()
})
