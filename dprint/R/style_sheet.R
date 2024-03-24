#' Format of Table
#'
#' Creates a data structure to define the gpar settings passed to the various grid.* function calls, 
#' as well as other formatting controls
#'
#' @param fontfamily see gpar
#' @param fontface see gpar
#' @param fontsize see gpar
#' @param text.all If TRUE, when frmt.bdy is defined and nothing else, fontfamily, fontface, fontsize, col will be applied to frmt.col and frmt.row to avoid excessive parameters. Need to define some general rules - probably not implemented correct at this time.
#' @param col Color of text
#' @param bg Background Color
#' @param buf number of character space (buffer) to be forced inbetween columns, and after the labels and group display.  When left and right justified, .5 buffer is added and subtracted to respective side of borders
#' @param bty style for box around section of table. "=" = above and below, "o" = completely enclosed, "_"=underneath
#' @param lwd  line size for bty, -1 suppresses
#' @param lty line type for bty
#' @param lcol color of lines
#' @param linespace Number of lines between columns
#' @export
frmt <-
function(fontfamily = NULL,
         fontface = NULL,
         fontsize = NULL,
         text.all=FALSE,    # If TRUE, when frmt.bdy is defined and nothing else, fontfamily, fontface, fontsize, col will be applied to frmt.col and frmt.row
         col = NULL,       # Color of text
         bg = NULL,        # Background Color
         buf = NULL,       # number of character space (buffer) to be forced inbetween columns, and after the labels and group display.  When left and right justified, .5 buffer is added and subtracted to respective side of borders
         bty = NULL,       # style for border "=" - above and below, "o"- rectangle around table
         lwd = NULL,       # line size for rectangle
         lty = NULL,       # Line Size, -1 Suppresses
         lcol=NULL,        # lcol, col for lines of rectange
         linespace = NULL  # linespace  = space between rows, number of characters according to GPAR.TBL, will be converted to metric by PAGE.LAYOUT
             )
{
    return(list(fontfamily = fontfamily,  fontface = fontface,  fontsize = fontsize, col=col, bg=bg, buf=buf,
                bty = bty, lwd = lwd, lty=lty, lcol=lcol,
                 text.all=text.all,linespace=linespace))
}


#' Style Sheet
#'
#' Control the mark up and formats for different sections of table
#'
#' @param frmt.bdy format settings for body of table
#' @param frmt.col format settings for column heading of table
#' @param frmt.colh Only used for borders around column spanning in hiearchy, "o" boxes entrie column, "_" puts line under hiarchy
#' @param frmt.grp format settings for row group labels
#' @param frmt.lbl format settings for row labels
#' @param frmt.main format settings for table title
#' @param frmt.tbl format settings for entire table, currently only for the box around entire table (Except table title)
#' @param frmt.ftn format settings for footnote
#' @param justify justification of text, applies to column heading and body
#' @param indent number of characters ("A") to indent the labels underneath the grouping variable
#' @param tbl.buf The space (vertical) between multiple tables
#' @param cex character expansion
#'  
#' @export
#' @note
#' Defaults are listed below, unless section of table overwrites with its own defaults:
#'
#'   fontfamily = ""
#'   fontface = "plain"
#'   fontsize = 8
#'   col = "black"
#'   bg = "white"
#'   buf = 3
#'   bty = "X" (none)
#'   lwd = 1
#'   lty = 1
#'   lcol="black"
#'   linespace = 2
#'
#' Column heading: fontface="bold",bty="_", lwd=2
#'
#' Column Spanning: bty="_", 
#'
#' Row Group: fontface="bold", bty="="
#'
#' Entire table: bty="="
#'
#' Table title (main): fontface="bold", fontsize=10, linespace=1.5
#'
#' Footnote: linespace=1.5
#' 
#' @examples
#' # My Style
#' # Default, this is what is used in dprint if style parameter is not defined
#' style()
#' # Save style sheet formats in object to pass to multiple calls
#' CBs <- style(frmt.bdy=frmt(fontfamily="HersheySans"), frmt.tbl=frmt(bty="o", lwd=1),
#'             frmt.col=frmt(fontfamily="HersheySans", bg="khaki", fontface="bold",lwd=2,bty="_"),
#'             frmt.grp=frmt(fontfamily="HersheySans",bg="khaki", fontface="bold"),
#'             frmt.main=frmt(fontfamily="HersheySans", fontface="bold", fontsize=12),
#'             frmt.ftn=frmt(fontfamily="HersheySans"),
#'             justify="right")
style <-
function(frmt.bdy=NULL,
        frmt.col=NULL,
        frmt.colh=NULL, # For borders around column hiearchy, "o" boxes entrie column, "_" puts line under hiarchy
        frmt.grp=NULL,
        frmt.lbl=NULL,
        frmt.main=NULL,
        frmt.tbl=NULL,  # Currently only controls border around the entire table
        frmt.ftn=NULL,
        justify = "center", # justification of the elements of table (does not include labels or groups), default "center"
        indent = 2,         # number of characters ("A") to indent the labels underneath the grouping variable
        tbl.buf=.25,        # The space (vertical) between multiple tables
        cex = 1             # cex        = character expansion
                 )
{
   # Set structure if parameter has not been callled at all
  if (is.null(frmt.bdy))  {frmt.bdy  <- frmt()}
  if (is.null(frmt.col))  {frmt.col  <- frmt()}
  if (is.null(frmt.colh)) {frmt.colh <- frmt()}
  if (is.null(frmt.grp))  {frmt.grp  <- frmt()}
  if (is.null(frmt.lbl))  {frmt.lbl  <- frmt()}
  if (is.null(frmt.main)) {frmt.main <- frmt()}
  if (is.null(frmt.tbl))  {frmt.tbl  <- frmt()}
  if (is.null(frmt.ftn))  {frmt.ftn  <- frmt()}
  
   # set BODY formats defaults
    if (is.null(frmt.bdy$fontfamily)) {frmt.bdy$fontfamily=""}
    if (is.null(frmt.bdy$fontface))   {frmt.bdy$fontface="plain"}
    if (is.null(frmt.bdy$fontsize))   {frmt.bdy$fontsize=8}
    if (is.null(frmt.bdy$col))        {frmt.bdy$col="black"}
    if (is.null(frmt.bdy$bg))         {frmt.bdy$bg="white"}
    if (is.null(frmt.bdy$buf))        {frmt.bdy$buf=3}
    if (is.null(frmt.bdy$bty))        {frmt.bdy$bty="X"}
    if (is.null(frmt.bdy$lwd))        {frmt.bdy$lwd=1}
    if (is.null(frmt.bdy$lty))        {frmt.bdy$lty=1}
    if (is.null(frmt.bdy$lcol))       {frmt.bdy$lcol="black"}
    if (is.null(frmt.bdy$linespace))  {frmt.bdy$linespace=2}
   # set COLUMN HEADING formats defaults
    if (is.null(frmt.col$fontfamily)) {frmt.col$fontfamily=""}
    if (is.null(frmt.col$fontface))   {frmt.col$fontface="bold"}
    if (is.null(frmt.col$fontsize))   {frmt.col$fontsize=8}
    if (is.null(frmt.col$col))        {frmt.col$col="black"}
    if (is.null(frmt.col$bg))         {frmt.col$bg="white"}
    if (is.null(frmt.col$buf))        {frmt.col$buf=3}
    if (is.null(frmt.col$bty))        {frmt.col$bty="_"}
    if (is.null(frmt.col$lwd))        {frmt.col$lwd=2}
    if (is.null(frmt.col$lty))        {frmt.col$lty=1}
    if (is.null(frmt.col$lcol))       {frmt.col$lcol="black"}
    if (is.null(frmt.col$linespace))  {frmt.col$linespace=2}
   # set COLUMN SPANNING in hiearchy format defaults
    if (is.null(frmt.colh$fontfamily)) {frmt.colh$fontfamily=""}
    if (is.null(frmt.colh$fontface))   {frmt.colh$fontface="plain"}
    if (is.null(frmt.colh$fontsize))   {frmt.colh$fontsize=8}
    if (is.null(frmt.colh$col))        {frmt.colh$col="black"}
    if (is.null(frmt.colh$bg))         {frmt.colh$bg="white"}
    if (is.null(frmt.colh$buf))        {frmt.colh$buf=3}
    if (is.null(frmt.colh$bty))        {frmt.colh$bty="_"}
    if (is.null(frmt.colh$lwd))        {frmt.colh$lwd=1}
    if (is.null(frmt.colh$lty))        {frmt.colh$lty=1}
    if (is.null(frmt.colh$lcol))       {frmt.colh$lcol="black"}
    if (is.null(frmt.colh$linespace))  {frmt.colh$linespace=2}
    # set ROW GROUP format defaults
    if (is.null(frmt.grp$fontfamily)) {frmt.grp$fontfamily=""}
    if (is.null(frmt.grp$fontface))   {frmt.grp$fontface="bold"}
    if (is.null(frmt.grp$fontsize))   {frmt.grp$fontsize=8}
    if (is.null(frmt.grp$col))        {frmt.grp$col="black"}
    if (is.null(frmt.grp$bg))         {frmt.grp$bg="white"}
    if (is.null(frmt.grp$buf))        {frmt.grp$buf=3}
    if (is.null(frmt.grp$bty))        {frmt.grp$bty="="}
    if (is.null(frmt.grp$lwd))        {frmt.grp$lwd=1}
    if (is.null(frmt.grp$lty))        {frmt.grp$lty=1}
    if (is.null(frmt.grp$lcol))       {frmt.grp$lcol="black"}
    if (is.null(frmt.grp$linespace))  {frmt.grp$linespace=2}
    # set ROW LABEL format defaults
    if (is.null(frmt.lbl$fontfamily)) {frmt.lbl$fontfamily=""}
    if (is.null(frmt.lbl$fontface))   {frmt.lbl$fontface="plain"}
    if (is.null(frmt.lbl$fontsize))   {frmt.lbl$fontsize=8}
    if (is.null(frmt.lbl$col))        {frmt.lbl$col="black"}
    if (is.null(frmt.lbl$bg))         {frmt.lbl$bg="white"}
    if (is.null(frmt.lbl$buf))        {frmt.lbl$buf=3}
    if (is.null(frmt.lbl$bty))        {frmt.lbl$bty="X"}
    if (is.null(frmt.lbl$lwd))        {frmt.lbl$lwd=1}
    if (is.null(frmt.lbl$lty))        {frmt.lbl$lty=1}
    if (is.null(frmt.lbl$lcol))       {frmt.lbl$lcol="black"}
    if (is.null(frmt.lbl$linespace))  {frmt.lbl$linespace=2}
    # set format defaults for Entire table   
    if (is.null(frmt.tbl$fontfamily)) {frmt.tbl$fontfamily=""}
    if (is.null(frmt.tbl$fontface))   {frmt.tbl$fontface="plain"}
    if (is.null(frmt.tbl$fontsize))   {frmt.tbl$fontsize=8}
    if (is.null(frmt.tbl$col))        {frmt.tbl$col="black"}
    if (is.null(frmt.tbl$bg))         {frmt.tbl$bg="white"}
    if (is.null(frmt.tbl$buf))        {frmt.tbl$buf=3}
    if (is.null(frmt.tbl$bty))        {frmt.tbl$bty="="}
    if (is.null(frmt.tbl$lwd))        {frmt.tbl$lwd=1}
    if (is.null(frmt.tbl$lty))        {frmt.tbl$lty=1}
    if (is.null(frmt.tbl$lcol))       {frmt.tbl$lcol="black"}
    if (is.null(frmt.tbl$linespace))  {frmt.tbl$linespace=2}
    # set format defaults for table title
    if (is.null(frmt.main$fontfamily)) {frmt.main$fontfamily=""}
    if (is.null(frmt.main$fontface))   {frmt.main$fontface="bold"}
    if (is.null(frmt.main$fontsize))   {frmt.main$fontsize=10}
    if (is.null(frmt.main$col))        {frmt.main$col="black"}
    if (is.null(frmt.main$bg))         {frmt.main$bg="white"}
    if (is.null(frmt.main$buf))        {frmt.main$buf=3}
    if (is.null(frmt.main$bty))        {frmt.main$bty="X"}
    if (is.null(frmt.main$lwd))        {frmt.main$lwd=1}
    if (is.null(frmt.main$lty))        {frmt.main$lty=1}
    if (is.null(frmt.main$lcol))       {frmt.main$lcol="black"}
    if (is.null(frmt.main$linespace))  {frmt.main$linespace=1.5}
    # set format defaults for footnote
    if (is.null(frmt.ftn$fontfamily)) {frmt.ftn$fontfamily=""}
    if (is.null(frmt.ftn$fontface))   {frmt.ftn$fontface="plain"}
    if (is.null(frmt.ftn$fontsize))   {frmt.ftn$fontsize=8}
    if (is.null(frmt.ftn$col))        {frmt.ftn$col="black"}
    if (is.null(frmt.ftn$bg))         {frmt.ftn$bg="white"}
    if (is.null(frmt.ftn$buf))        {frmt.ftn$buf=3}
    if (is.null(frmt.ftn$bty))        {frmt.ftn$bty="X"}
    if (is.null(frmt.ftn$lwd))        {frmt.ftn$lwd=1}
    if (is.null(frmt.ftn$lty))        {frmt.ftn$lty=1}
    if (is.null(frmt.ftn$lcol))       {frmt.ftn$lcol="black"}
    if (is.null(frmt.ftn$linespace))  {frmt.ftn$linespace=1.5}
    
    # When TRUE, when frmt.bdy is defined and nothing else, fontfamily, fontface, fontsize, col will be applied to frmt.col and frmt.grp
   if (frmt.bdy$text.all)
    {
      frmt.col$fontfamily=frmt.bdy$fontfamily; frmt.col$fontsize=frmt.bdy$fontsize;
      frmt.grp$fontfamily=frmt.bdy$fontfamily; frmt.grp$col=frmt.bdy$col;
      frmt.lbl$fontfamily=frmt.bdy$fontfamily; frmt.lbl$fontface=frmt.bdy$fontface; frmt.lbl$fontsize=frmt.bdy$fontsize; frmt.lbl$col=frmt.bdy$col;
    }

   return(list(frmt.bdy=frmt.bdy, frmt.col=frmt.col, frmt.grp=frmt.grp, frmt.lbl=frmt.lbl, frmt.main=frmt.main, frmt.tbl=frmt.tbl, frmt.colh=frmt.colh, frmt.ftn=frmt.ftn,
               justify = justify, indent = indent, tbl.buf=tbl.buf,
                cex = cex))
}

