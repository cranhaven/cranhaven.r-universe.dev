#' @importFrom shiny addResourcePath
.onAttach <- function(libname, pkgname) { # nolint
  packageStartupMessage("Welcome")
  addResourcePath("www", system.file("www", package = "INSPECTumours"))
}


color_az <- list(Mulberry = rgb(131, 0, 81, max = 255),
                 LimeGreen = rgb(196, 214, 0, max = 255),
                 Navy = rgb(0, 56, 101, max = 255),
                 Graphite = rgb(63, 68, 68, max = 255),
                 LightBlue = rgb(104, 210, 223, max = 255),
                 Magenta = rgb(208, 0, 111, max = 255),
                 Purple = rgb(60, 16, 83, max = 255),
                 Gold = rgb(240, 171, 0, max = 255),
                 Platinum = rgb(157, 176, 172, max = 255))
az_pal <- unname(unlist(color_az))


# assign custom colors for classification plots
classification_colors <- setNames(
  c(
    color_az[["Platinum"]],
    color_az[["Purple"]],
    color_az[["Gold"]],
    color_az[["LightBlue"]],
    color_az[["LimeGreen"]]
    ),
  c(
    "Not reliable",
    "Non-responder",
    "Modest responder",
    "Stable responder",
    "Regressing responder"
    )
)
