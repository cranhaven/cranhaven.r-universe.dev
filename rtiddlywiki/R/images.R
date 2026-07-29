

#' Save ggplot into base64
#'
#' @param plot object for ggplot2 or a function for plot
#' @param width image width
#' @param height image height
#' @param dpi image resolution
#' @param ... Other arguments for plot function
#'
#' @returns character string for base64 image
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- cars |>
#'     ggplot() +
#'     geom_point(aes(speed, dist))
#' p |> save_base64()
#' }
save_base64 <- function(plot, width = NULL, height = NULL, dpi = NULL, ...) {
    if (!requireNamespace("base64enc", quietly = TRUE)) {
        stop("Please install the 'base64enc' package.")
    }

    if (!is.null(width)) {
        stopifnot(length(width) == 1)
        stopifnot(is.numeric(width))
    }

    if (!is.null(height)) {
        stopifnot(length(height) == 1)
        stopifnot(is.numeric(height))
    }
    if (!is.null(dpi)) {
        stopifnot(length(dpi) == 1)
        stopifnot(is.numeric(dpi))
    }
    # Define a null coalescing operator
    `%||%` <- function(a, b) if (!is.null(a)) a else b

    # Determine if knitting is in progress
    in_knitr <- isTRUE(getOption("knitr.in.progress"))

    # Use knitr options if available and width/height are not provided
    if (is.null(width)) {
        width <- if (in_knitr) knitr::opts_current$get("fig.width") %||% 5 else 5
    }

    if (is.null(height)) {
        height <- if (in_knitr) knitr::opts_current$get("fig.height") %||% 4 else 4
    }


    if (is.null(dpi)) {
        dpi <- if (in_knitr) knitr::opts_current$get("dpi") %||% 150 else 150
    }

    # Create temporary file
    temp_file <- tempfile(fileext = ".png")
    # Calculate pixel size
    pixel_width <- width * dpi
    pixel_height <- height * dpi


    grDevices::png(filename = temp_file, width = pixel_width, height = pixel_height, res = dpi)
    on.exit({
        unlink(temp_file)
    }, add = TRUE)

    if (is.function(plot)) {
        plot(...)
    } else {
        print(plot)
    }

    # Turn off device
    grDevices::dev.off()

    # Encode to base64
    base64_image <- base64enc::dataURI(file = temp_file, mime = "image/png")
    return(base64_image)
}


