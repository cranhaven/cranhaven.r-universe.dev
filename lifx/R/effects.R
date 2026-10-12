#' "Breathe" effect
#'
#' @param color color The color to use for the breathe effect. use lx_color() as input
#' @param from_color The color to start the effect from. If this parameter is omitted then the color the bulb is currently set to is used instead.
#' @param period The time in seconds for one cycle of the effect.
#' @param cycles The number of times to repeat the effect.
#' @param persist boolean; If FALSE set the light back to its previous value when effect ends, if true leave the last effect color.
#' @param power_on If FALSE, does not turn light on if it is off
#' @param peak Defines where in a period the target color is at its maximum. Minimum 0.0, maximum 1.0.
#' @template param_selector
#' @template param_token
#' @examples
#' \dontrun{
#' lx_effect_breathe(color = "red",from_color = "blue",period = 3,cycles = 5,power_on = TRUE)
#' }
#' @return an 'httr' response object (see \code{\link[httr]{response}})
#' @export
lx_effect_breathe<-function(
  color,
  from_color = NULL,
  period=1,
  cycles = 1,
  persist = FALSE,
  power_on = TRUE,
  peak = 0.5,
  selector = "all",
  token =  lx_get_token()
){

  lx_POST(selector,
          endpoint = paste0("effects","/" ,"breathe"),
          token = token,
          color = color,
          from_color = from_color,
          period = period,
          cycles = cycles,
          persist = persist,
          power_on = power_on,
          peak = peak)
}




#' "Move" effect
#'
#' @param direction Move direction, can be "forward" or "backward".
#' @param period The time in seconds for one cycle of the effect.
#' @param cycles The number of times to move the pattern across the device. Special cases are 0 to switch the effect off, and unspecified to continue near indefinitely (10^10 times).
#' @param power_on Switch any selected device that is off to on before performing the effect.
#' @template param_fast
#' @template param_token
#' @template param_selector
#' @examples
#' \dontrun{
#' lx_effect_move(direction = "backward", period = 2, cycles = 5)
#' }
#' @return an 'httr' response object (see \code{\link[httr]{response}})
#' @export
lx_effect_move<-function(
  direction = "forward",
  period=1,
  cycles = 10^10,
  power_on = TRUE,
  fast = FALSE,

  selector = "all",
  token =  lx_get_token()
){

  lx_POST(selector,
          endpoint = paste0("effects","/" ,"move"),
          token = token,

          direction = direction,
          period = period,
          cycles = cycles,
          power_on = power_on,
          fast = fast)
}



#' "Morph" effect
#'
#' @param period This controls how quickly the morph runs. It is measured in seconds. A lower number means the animation is faster
#' @param duration How long the animation lasts for in seconds. Not specifying a duration makes the animation "never" stop (10^100 cycles). Specifying 0 makes the animation stop. Note that there is a known bug where the tile remains in the animation once it has completed if duration is nonzero.
#' @param palette array of strings (7 colors across the spectrum). You can control the colors in the animation by specifying a list of color specifiers. See \code{\link{lx_color_name}}
#' @param power_on if TRUE (default), switch any selected device that is off to on before performing the effect.
#' @template param_fast
#' @template param_selector
#' @template param_token
#' @examples
#' \dontrun{
#' lx_effect_morph(period = 2, palette = c("red", "blue"))
#  lx_effect_morph(
#   palette = c(
#     lx_color_name(hue=50,brightness = 0.5),
#     lx_color_name(hue=100,brightness = 0.1)
#   )
# )
#' }
#' @return an 'httr' response object (see \code{\link[httr]{response}})
#' @export
lx_effect_morph<-function(
  period = 5,
  duration = 10^10,
  palette,
  power_on = TRUE,
  fast = FALSE,
  selector = "all",
  token =  lx_get_token()
){

  lx_POST(selector,
          endpoint = paste0("effects","/" ,"morph"),
          token = token,

          period = period,
          duration = duration,
          palette = palette,
          fast = fast,
          power_on = power_on)
}




#' "Flame" effect
#'
#' @param period This controls how quickly the flame runs. It is measured in seconds. A lower number means the animation is faster
#' @param duration How long the animation lasts for in seconds. Not specifying a duration makes the animation never stop. Specifying 0 makes the animation stop. Note that there is a known bug where the tile remains in the animation once it has completed if duration is nonzero.
#' @param power_on if TRUE (default), switch any selected device that is off to on before performing the effect.
#' @template param_fast
#' @template param_selector
#' @template param_token
#' @examples
#' \dontrun{
#' lx_effect_flame(period = 2, duration = 3)
#' }
#' @return an 'httr' response object (see \code{\link[httr]{response}})
#' @export
lx_effect_flame<-function(
  period = 5,
  duration = 10^10,
  power_on = TRUE,
  fast = FALSE,
  selector = "all",
  token =  lx_get_token()
){

  lx_POST(selector,
          endpoint = paste0("effects","/" ,"flame"),
          token = token,
          period = period,
          duration = duration,
          power_on = power_on,
          fast = fast
          )
}


#' "Pulse" effect
#'
#' @param color The color to use for the pulse effect. use lx_color() as input
#' @param from_color The color to start the effect from. If this parameter is omitted then the color the bulb is currently set to is used instead.
#' @param period The time in seconds for one cycle of the effect.
#' @param cycles The number of times to repeat the effect.
#' @param persist boolean; If FALSE set the light back to its previous value when effect ends, if true leave the last effect color.
#' @param power_on If FALSE, does not turn light on if it is off
#' @template param_selector
#' @template param_token
#' @examples
#' \dontrun{
#' lx_effect_pulse(color = "red",from_color = "blue", period = 3,cycles = 5, persist = TRUE)
#' }
#' @return an 'httr' response object (see \code{\link[httr]{response}})
#' @export
lx_effect_pulse<-function(
  color,
  from_color = NULL,
  period=1,
  cycles = 1,
  persist = FALSE,
  power_on = TRUE,
  selector = "all",
  token =  lx_get_token()
){

  lx_POST(selector,
          endpoint = paste0("effects","/" ,"pulse"),
          token = token,
          color = color,
          from_color = from_color,
          period = period,
          cycles = cycles,
          persist = persist,
          power_on = power_on
  )
}




#' Turn effects off
#'
#' @param power_off If TRUE, also turns the light(s) off
#' @template param_selector
#' @template param_token
#' @examples \dontrun{lx_effect_off()}
#' @return an 'httr' response object (see \code{\link[httr]{response}})
#' @export
lx_effect_off<-function(
  power_off = FALSE,
  selector = "all",
  token =  lx_get_token()
){

  lx_POST(selector,
          endpoint = paste0("effects","/" ,"off"),
          token = token,
          power_off = power_off
  )
}

