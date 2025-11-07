

# Validate Config ---------------------------------------------------------



#' Validate thaipdf_config Input
#'
#' @param thai_font (Character) Thai language font name
#' @param line_spacing (Numeric) line spacing multiplier
#'
#' @return display error if invalid
#' @noRd
thaipdf_config_validate <- function(thai_font = "TH Sarabun New",
                                    line_spacing = 1.5
){

  # Validate Thai font
  is_valid_font <- all(is.character(thai_font), length(thai_font) == 1, (thai_font != ""))
  if(!is_valid_font) stop("`thai_font` is invalid. You must provide only 1 valid Thai font name.", call. = FALSE)

  # Validate Line Spacing
  is_valid_lin_sp <- all(is.numeric(line_spacing), length(line_spacing) == 1, line_spacing > 0)
  if(!is_valid_lin_sp) stop("`line_spacing` is invalid. You must provide a numeric value.", call. = FALSE)



}
