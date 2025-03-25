#' @title Examples of the 'gyro' package
#' @description Some examples of hyperbolic polyhedra realized with the 'gyro'
#'   package.
#'
#' @return No value. The function firstly copies the demo files in a
#'   temporary directory. If you use RStudio, the function opens these files.
#'   Otherwise it prints a message giving the instructions to access to these
#'   files.
#'
#' @note The \emph{BarthLike} file has this name because the figure it
#'   generates looks like the Barth sextic (drawing by Patrice Jeener):
#'
#' \if{html}{
#'
#'   \figure{SextiqueDeBarth.png}{options: style="max-width:75\%; display: block; text-align: center;"}
#'
#' }
#' \if{latex}{
#'
#'   \out{\begin{center}}\figure{SextiqueDeBarth.png}\out{\end{center}}
#'
#' }
#' @export
#'
#' @importFrom rstudioapi isAvailable navigateToFile
#' @importFrom clipr clipr_available write_clip
gyrodemos <- function(){
  folder <- system.file("gyrodemos", package = "gyro")
  tmpdir <- file.path(tempdir(), "gyrodemos")
  dir.create(tmpdir)
  files <- list.files(folder)
  tmpfiles <- file.path(tmpdir, files)
  invisible(file.copy(file.path(folder, files), tmpfiles))
  if(isAvailable(version_needed = "0.99.719")){
    invisible(sapply(tmpfiles, navigateToFile))
    message(sprintf("Opened files: %s.", toString(files)))
  }else{
    line <- sprintf(
      'wd <- setwd("%s")\n',
      normalizePath(tmpdir, winslash = "/")
    )
    message(
      "Copy the following line to go to the demos folder:\n",
      line,
      "Type `setwd(wd)` to come back."
    )
    if(clipr_available()){
      write_clip(line)
      message("The line has been copied to the clipboard.")
    }
  }
}

