#' Shiny GUI for GWmodelVis package
#' @importFrom shiny runApp
#' @import GWmodel
#' @importFrom shinyjs useShinyjs
#' @import shinyFiles
#' @import ggplot2
#' @import sf
#' @importFrom sp coordinates SpatialPointsDataFrame elide sp2Mondrian
#' @import dplyr 
#' @import magrittr
#' @import tmap
#' @import leaflet
#' @import ggforce
#' @import stringr
#' @import shinydashboard
#' @import readr
#' @import terra
#' @import fontawesome
#' @import htmltools
#' @import leaflet.extras
#' @importFrom av av_encode_video
#' @importFrom tuneR readWave writeWave Wave sine
#' @import servr
#' @import shinyWidgets
#' @importFrom signal butter filtfilt bartlett hanning blackman hamming
#' @importFrom DT dataTableOutput renderDataTable DTOutput
#' @import ggspatial
#' @description runGWmodelVis() loads interactive user interface built using R 'shiny' for multiple geographically
#' weighted models.
#' @details The interactive user interface to provide an easy way for to perform techniques from a subset of
#' spatial statistics known as geographically weighted models.
#' @param launch.browser If true, the system's default web browser will be launched automatically
#' after the app is started. Defaults to true in interactive sessions
#' only. This value ofthis parameter can also be a function to call with
#' the application's URL.
#' @param port is the TCP port that the application should listen on.
#' If the port is not specified,
#' and the shiny.port option is set (with options(shiny.port = XX)),
#' then that port will be used.
#' Otherwise, use a random port.
#' @param host The IPv4 address that the application should listen on.
#' Defaults to the shiny.host option, if set, or "127.0.0.1" if not.
#' @return No return value
#' @examples
#' if(interactive()){
#'   runGWmodelVis()
#' }
#' @export

#' 启动地理加权模型可视化 Shiny 应用
#'
#' @param host 监听地址，默认为 "127.0.0.1"
#' @param port 监听端口，默认为 NULL（自动选择）
#' @param launch.browser 是否自动打开浏览器，默认为 TRUE
#' @return 无返回值
#' @export
#' @examples
#' if (interactive()) {
#'   runGWmodelVis()
#' }
runGWmodelVis <- function(host = "127.0.0.1",
                                port = NULL,launch.browser = TRUE) {
  appDir <- system.file("GWmodelVisApp", package = "GWmodelVis")
  if (appDir == "") {
    stop("Could not find GWmodelVis.
         Try re-installing `GWmodelVis`.", call. = FALSE)
  }

  shiny::runApp(appDir,
                display.mode = "normal",
                launch.browser = launch.browser,
                port = port,
                host = getOption("shiny.host", host)
  )
}