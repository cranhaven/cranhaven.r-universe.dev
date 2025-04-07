## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(epitweetr)
package_name <- "epitweetr"
package_desc <- packageDescription(package_name)
pkg_version <- package_desc$Version
pkg_date <- package_desc$Date
authors_str <- gsub("^ *|(?<= ) |\n| *$", "", package_desc$Authors, perl = TRUE)
author_list <- eval(parse(text = authors_str))
pkg_authors <- paste(
  format(
    author_list, 
    include = c("given", "family", "email", "comment"), 
    braces = list(email = c("<", ">,<br />"), 
    comment = c("", ""))
  ), 
  collapse = "<br /><br />"
)
pkg_maintainer <- package_desc$Maintainer
pkg_license <- package_desc$License
pkg_url <- package_desc$URL


## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/dashboard.png")

## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/alerts.png")

## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/geotag_evaluation.png")

## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/data_protection.png")

## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/config.png")

## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/troubleshoot.png")

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  install.packages(epitweetr)

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  Sys.getenv("JAVA_HOME")

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(epitweetr)
#  epitweetr_app("data_dir")

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(epitweetr)
#  epitweetr_app("data_dir")

## ----echo=FALSE, fig.cap="", out.width = '50%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/run_tasks.png")

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(epitweetr)
#  fs_loop("data_dir")

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(epitweetr)
#  search_loop("data_dir")

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(epitweetr)
#  fs_loop("data_dir")

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(epitweetr)
#  detect_loop("data_dir")

## ----echo=FALSE, fig.cap="", out.width = '50%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/twitter_authentication.png")

## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/topics.png")

## ----echo=FALSE, fig.cap="", out.width = '80%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/Example_topics_list.png")

## ----echo=FALSE, fig.cap="", out.width = '80%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/Schedule_plan_image.png")

## ----echo=FALSE, fig.cap="", out.width = '80%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/Search_plan_queues.png")

## ----echo=FALSE, fig.cap="", out.width = '80%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/config_languages.png")

## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/subscribers.png")

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  epitweetr_app("data_dir")

## ----echo=FALSE, fig.cap="", out.width = '40%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/topics_countries.png")

## ----echo=FALSE, fig.cap="", out.width = '40%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/period.png")

## ----echo=FALSE, fig.cap="", out.width = '30%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/time_unit.png")

## ----echo=FALSE, fig.cap="", out.width = '30%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/include_retweets.png")

## ----echo=FALSE, fig.cap="", out.width = '30%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/location_type.png")

## ----echo=FALSE, fig.cap="", out.width = '30%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/signal_detection_fpr.png")

## ----echo=FALSE, fig.cap="", out.width = '40%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/outlier_dashboard.png")

## ----echo=FALSE, fig.cap="", out.width = '30%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/Bonferroni_correction.png")

## ----echo=FALSE, fig.cap="", out.width = '30%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/days_in_baseline.png")

## ----echo=FALSE, fig.cap="", out.width = '30%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/same_weekday_baseline.png")

## ----echo=FALSE, fig.cap="", out.width = '30%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/hover_trendline.png")

## ----echo=FALSE, fig.cap="", out.width = '60%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/hover_map.png")

## ----echo=FALSE, fig.cap="", out.width = '50%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/top_words.png")

## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/alerts.png")

## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/geotag_evaluation.png")

## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/config.png")

## ----echo=FALSE, fig.cap="", out.width = '50%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/status.png")

## ----echo=FALSE, fig.cap="", out.width = '70%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/detection_pipeline.png")

## ----echo=FALSE, fig.cap="", out.width = '50%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/signal_detection_config.png")

## ----echo=FALSE, fig.cap="", out.width = '40%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/general.png")

## ----echo=FALSE, fig.cap="", out.width = '30%'--------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/twitter_authentication.png")

## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/topics.png")

## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/languages.png")

## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/troubleshoot.png")

## ----echo=FALSE, fig.cap="", out.width = '100%'-------------------------------
knitr::include_graphics("https://github.com/EU-ECDC/epitweetr/raw/master/img/ebola.png")

