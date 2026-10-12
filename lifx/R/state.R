# generic state modifiers ----------------------------------------------------

#' set light state (lifx API endpoint PUT set state)
#'
#' @template param_power
#' @template param_color_name
#' @param brightness set the brightness (0-1)
#' @template param_infrared
#' @template param_duration
#' @template param_fast
#' @template param_selector
#' @template param_token
#' @return an 'httr' response object (see \code{\link[httr]{response}})
#' @references \url{https://api.developer.lifx.com/docs/set-state}
lx_state<-function(power=NULL,
                   color_name=NULL,
                   brightness=NULL,
                   infrared=NULL,
                   duration=0,
                   fast=FALSE,
                   selector="all",
                   token =  lx_get_token()){

  response <- lx_PUT(endpoint="state", # expected params
                     selector=selector,    # expected params
                     token=token,          # expected params
                     power=power,      # query body as ... (dots):
                     color=color_name,
                     brightness=brightness,
                     infrared=infrared,
                     duration=duration,
                     fast=fast
  )

  invisible(response)
}


#' Change light state relative to current state (wrapper for POST state delta
#' @template param_colors
#' @template param_duration
#' @template param_infrared
#' @template param_power
#' @template param_selector
#' @template param_token
#' @return an 'httr' response object (see \code{\link[httr]{response}})
#' @references \url{https://api.developer.lifx.com/docs/state-delta}
lx_delta<-function(hue = NULL,
                   saturation = NULL,
                   brightness=NULL,
                   kelvin = NULL,
                   infrared=NULL,
                   duration=0,
                   power=NULL,
                   selector="all",
                   token =  lx_get_token() ){

  api_call <- lx_POST
  endpoint <- "state/delta"

  response <- lx_POST(endpoint=endpoint, # expected params
                      selector=selector,    # expected params
                      token=token,          # expected params
                      power=power,      # body params as ... (dots):
                      duration=duration,
                      infrared=infrared,
                      hue = hue,
                      saturation = saturation,
                      brightness=brightness,
                      kelvin = kelvin


  )

  invisible(response)

}


