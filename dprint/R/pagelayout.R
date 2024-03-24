#' Page Layout
#'
#' Page Layout
#'
#' @param dtype Device type, currently available "rgraphics", "portrait", "landscape"
#' @param margins A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
#' @param pg.dim If custom page dimensions define a vector of c(width, height) units
#' @export
pagelayout <-
function(dtype="rgraphics", # Device type, currently available "rgraphics", "portrait", "landscape"
                       margins=NULL,   # A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
                       pg.dim=NULL     # If custom page dimensions define a vector of c(width, height) units
                       )
{
  if (dtype=="rgraphics")
   {
      page.height <- 7; page.width  <- 7
      if (is.null(margins)) {margins = rep(.2,4)}
   }
  if (dtype == "portrait")
    {
     page.height = 11;  page.width  = 8.5;
     if (is.null(margins)) {margins = c(1, 1, 1.25, 1)}
    }
  if (dtype == "landscape")
    {
     page.height = 8.5; page.width  = 11;
     if (is.null(margins)) {margins = c(1, 0.5, 1, 0.5)}
    }

  # Custom page dimensions over ride dtype
  if (!is.null(pg.dim)) {pg.dim <- rep(pg.dim, 2)[1:2]; page.width<-pg.dim[1]; page.height<-pg.dim[2];}
  #  Handles different ways of defining margins common that left=right, and top =bottom, so c(1, .5) is shorter
  if (length(margins) < 4) {margins <- rep(margins, floor(4/length(margins)))[1:4]}
  # Top left, and bottom right cordinates
  cord.tl <- c(margins[2], page.height-margins[3]) # Top Left
  cord.br <- c(page.width-margins[4], margins[1]) # Bottom right

  return(list(margins=margins, page.height=page.height, page.width=page.width,cord.tl=cord.tl, cord.br=cord.br))
}

