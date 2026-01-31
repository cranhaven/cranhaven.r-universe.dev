#' @title Monitor Object
#'
#' @description Assigns a monitor object, storing distance and width parameters.
#' @param distance Viewing distance in cm (from eyes to screen).
#' @param mon_width_cm Monitor screen width in cm.
#' @param mon_width_pixel Monitor screen width in pixels.
#' @return A monitor object with the specified parameters, to be used in the
#'   \code{\link{mon_conv}} function.
#' @seealso \code{\link{mon_conv}}
#' @examples
#' # assign monitor with 57 cm viewing distance, screen width 52 cm and 1920 pixels
#' my_mon = mon_neat(distance = 57, mon_width_cm = 52, mon_width_pixel = 1920)
#' @export
mon_neat = function(distance, mon_width_cm, mon_width_pixel) {
    validate_args(match.call(),
                  list(
                      val_arg(distance, c('num'), 1),
                      val_arg(mon_width_cm, c('num'), 1),
                      val_arg(mon_width_pixel, c('num'), 1)
                  ))
    mon_obj = list(
        distance = distance,
        mon_width_cm = mon_width_cm,
        mon_width_pixel = mon_width_pixel
    )
    attr(mon_obj, 'class') = 'mon_neat'
    return(mon_obj)
}

#' @title Monitor Screen Unit Conversion
#'
#' @description Given a specific monitor object, converts specified screen units to
#'   other specified units. The possible units to convert from and to: "cm"
#'   (centimeters), "pix" (pixels), or "deg" (degrees of visual angle).
#' @param mon_obj Monitor object, as assigned with \code{\link{mon_neat}}.
#' @param value Number; value (magnitude) of the given unit to convert from.
#'   (Can be vector as well.)
#' @param from String; unit ("cm", "pix", or "deg") to convert from.
#' @param to String; unit ("cm", "pix", or "deg") to convert to.
#' @return Number (magnitude) in the given output (\code{to}) unit.
#' @seealso \code{\link{mon_neat}}
#' @examples
#' # assign monitor with 50 cm distance, screen width 52 cm and 1920 pixels
#' my_mon = mon_neat(distance = 50, mon_width_cm = 52, mon_width_pixel = 1920)
#'
#' # convert 30.4 pixels to degrees of visual angle, for the specified monitor
#' mon_conv(my_mon, 30.4, 'pix', 'deg') # returns 0.9434492 (degrees)
#'
#' # convert 0.94 degrees of visual angle to pixels
#' mon_conv(my_mon, 0.94, 'deg', 'pix') # returns 30.28885 (pixels)
#'
#' # convert 10 degrees of visual angle to cm
#' mon_conv(my_mon, 10, from = 'deg', to = 'cm')
#'
#' # convert 8.748866 cm to pixels
#' mon_conv(my_mon, 8.748866, from = 'cm', to = 'pix')
#' @export
mon_conv = function(mon_obj, value, from, to) {
    UseMethod('mon_conv')
}

#' @export
mon_conv.default = function(...) {
    cat('This function is to be used with "mon_neat" objects.\n')
}

#' @export
mon_conv.mon_neat = function(mon_obj, value, from, to) {
    validate_args(match.call(),
                  list(
                      val_arg(value, c('num')),
                      val_arg(from, c('char'), 1, c('cm', 'pix', 'deg')),
                      val_arg(to, c('char'), 1, c('cm', 'pix', 'deg'))
                  ))
    if (from == to) {
        return(value)
    }
    if (from == 'pix') {
        pixels = value
        if (to == 'deg') {
            size_cm = mon_obj$mon_width_cm * (pixels / mon_obj$mon_width_pixel)
            deg_res = (atan((size_cm / 2) / mon_obj$distance) * (180 / pi) * 2)
            return(deg_res)
        } else if (to == 'cm') {
            size_cm = mon_obj$mon_width_cm * (pixels / mon_obj$mon_width_pixel)
            return(size_cm)
        }
    } else if (from == 'deg') {
        degrees = value
        if (to == 'pix') {
            size_cm = mon_obj$distance * tan(degrees * (pi / 180) / 2) * 2
            pix_res = mon_obj$mon_width_pixel * (size_cm / mon_obj$mon_width_cm)
            return(pix_res)
        } else if (to == 'cm') {
            size_cm = mon_obj$distance * tan(degrees * (pi / 180) / 2) * 2
            return(size_cm)
        }
    } else if (from == 'cm') {
        size_cm = value
        if (to == 'pix') {
            pix_res = mon_obj$mon_width_pixel * (size_cm / mon_obj$mon_width_cm)
            return(pix_res)
        } else if (to == 'deg') {
            deg_res = (atan((size_cm / 2) / mon_obj$distance) * (180 / pi) * 2)
            return(deg_res)
        }
    }
}