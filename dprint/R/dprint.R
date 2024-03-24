#' Print Table to Graphics Device (dprint)
#'
#' Prints tabular data to the graphics device by translating an R object to a tabular presentation.
#' 
#' @param fmla An object of class \dQuote{formula}. Formula interface is used to describe the properties of tabular data to be printed from the \emph{data} object.
#' @param data An object of class found among methods(dprint)
#' @param label Character vector of length 1 used to reference the name of column containing row labels.  Optional to \emph{fmla}. Set to NULL to when using \emph{fmla} or when no row labels exist. Default value is NULL.
#' @param group Character vector of length 1 used to reference the name of column containing grouping of row labels.  Optional to \emph{fmla}. Set to NULL to when using \emph{fmla} or when no row labels exist. Default value is NULL.
#' @param regx Character vector of length 1 used to provide regular expression(s) to remove unwanted text displayed from original column names (e.g. merge applied with .x and .y appended to duplicate column names)
#' @param style Style sheet object used to define font and other settings of the table. See \code{\link{style}} and \code{\link{frmt}}
#' @param main Table title defined by character vector of length 1.  String will be placed on top of table
#' @param footnote Footnote defined by character vector finite length. The text will be printed immediately underneath the tabular presentation.   Each position in the vector will force a new line break.
#' @param dtype Named references to preset \emph{pg.dim} settings. Graphics device type referred to by names, sets default page settings. Device types, currently available "rdevice", "portrait", "landscape" which sets \emph{pg.dim} to c(8,8), (11, 8.5) and (8.5, 11)
#' @param pg.dim  A vector of c(height,width) units used to describe the dimensions of a custom page and over ride dtype. When printing to a multiple page pdf with custom dimensions, dtype should be set to some character other than "rdevice" (i.e. "custom") because dev.new() will be used to start a new window instead of grid.text().
#' @param margins A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches. Other declarations assume a constant for all margins or c(top/bottom,left/right)
#' @param showmargins Boolean, displays margins on R device. Useful for tinkering with presentation
#' @param row.hl Conditional highligh row highlight object.  See \code{\link{row.hl}} 
#' @param fit.width Boolean. If TRUE, forces the table to fit the table horizontally within the \emph{pg.dim} and \emph{margins}.  Exactly fits when vector formats are used, approximation otherwise. 
#' @param fit.height Boolean. If TRUE, forces the table to fit the table vertically within the \emph{pg.dim} and \emph{margins}.  Exactly fits when vector formats are used, approximation otherwise. 
#' @param fit Boolean. If TRUE, forces the table to fit both horizontally and vertically within the \emph{pg.dim} and \emph{margins}.
#' @param newpage Boolean. If TRUE, when the presentation of the table runs out of space on the current page,  within the \emph{pg.dim} and \emph{margins}, a new page will automatically be started. Designed for multiple page pdf reports.
#' @param center.horz Boolean, If TRUE, center table horizontally
#' @param center.vert Boolean, If TRUE, center table vertically.  Only available for single table.
#' @param center Boolean, If TRUE, center both vertically and horizontally. Does not consider the fit.* parameters
#' @param f.hdr Pass a function for printing header. See examples and \code{\link{hdr}}
#' @param f.ftr Pass a function for printing footer. See examples and  \code{\link{ftr}}
#' @param pagenum Starting page number, will override page number from last call
#' @param lastcall Object returned from last call from dprint. Can use this as reference for where a second table should be presented on the same device as the previous call. dprint continues printing to device with fixed separation between tables
#' @details 
#' 
#' The available method functions for dprint are given by methods(dprint).  
#' 
#' \strong{Formula Interface: Operator Definitions}
#' 
#' \itemize{
#' \item \dQuote{~} separates row versus column definitions (LHS vs RHS of equation) 
#' \item Left Hand Side (LHS) - row attributes
#' \itemize{
#'   \item \dQuote{+} delimits up to two row labeling hierarchies (group + level)
#' }
#' \item Right Hand Side (RHS) - column attributes
#' \itemize{
#' \item \dQuote{+} delimits column placement
#' \item \dQuote{:} spanning attribute of a columns
#' \item \dQuote{.} all columns in data frame should be included
#' \item \dQuote{-} drops following column
#' \item \dQuote{|} list variables (delimited by "+") to condition on when print multiple simple tables
#' }
#' }
#' 
#' \strong{Formula Interface: Embedded Functions}
#' \itemize{
#' \item Any algebraic manimulation available in \code{\link{formula}} through \code{\link{I}} is also availble here
#' \item Formatting
#' \itemize{
#' \item Rounding & place holders: \code{\link{Fr}}, \code{\link{Fci}}, \code{\link{Fc}}, and \code{\link{Fb}}
#' \item Use  \code{\link{paste}}  to concatenate one of more fields with other text 
#' }
#' \item Renaming data frame column names to presentable labels.  Names default to column names otherwise
#' \itemize{
#'  \item Rn(column name, \dQuote{Presentation Label})
#'  }
#' }
#' 
#' \dQuote{\\n} can be used in \emph{main} or \emph{footnote} paramters or embedded function \emph{Rn()} to force additional line breaks
#' @author Carlin Brickner
#' @export
#' @examples
#' ### Generate Sample Data Structures ###
#' # Generate some example data frames
#' table1   <- rdesc(4, 5)        # Numeric
#' table1f <- rdesc(4, 5, rnd=TRUE) # Rounded and pretty format so values are character
#' table1a <- table1; table1b<-table1;
#' table1a$group2 <- 1; table1b$group2 <- 2;
#' table2 <- rbind(table1a, table1b)
#' table2a <- table2; table2b<-table2
#' table2a$group3 <- "Zebra"; table2b$group3 <- "Elephant";
#' table3 <- rbind(table2a, table2b)
#' # Create style object
#' CBs <- style(frmt.bdy=frmt(fontfamily="HersheySans"), frmt.tbl=frmt(bty="o", lwd=1),
#'             frmt.col=frmt(fontfamily="HersheySans", bg="khaki",fontface="bold",lwd=2,bty="_"),
#'             frmt.grp=frmt(fontfamily="HersheySans",bg="khaki", fontface="bold"),
#'             frmt.main=frmt(fontfamily="HersheySans", fontface="bold", fontsize=12),
#'             frmt.ftn=frmt(fontfamily="HersheySans"),
#'             justify="right")
#'
#' # dev.new()# All variables, no group or label
#' dprint(~., data=table1f)
#' dev.off()
#' # dev.new() # Spanning,  group level, and apply control and treatments to hierchaies on right
#' dprint(group+level~Control:(Mean1 + Median1 + Variance1) +
#'  Treatment:(Mean2 + Median2 + Variance2) + p.value, data=table1f)
#' dev.off()
#' # dev.new(); #Illegal Names, remove expression
#' dprint(group+level~`This is a Control`:(Mean1 + Median1 + Variance1) +
#'  Treatment.y:(Mean2 + Median2 + Variance2), data=table1f, regx="1|2|.y")
#' dev.off()
#' # dev.new(); #Illegal Names, no group label
#' dprint( ~ `This is a Control`:(Mean1 + Median1 + Variance1) + 
#' Treatment.y:(Mean2 + Median2 + Variance2), data=table1f, regx="1|2|.y")
#' # dev.new(); # all on rhs with exception of p.value
#' dev.off()
#' dprint(group+level~.-p.value, data=table1f)
#' dev.off()
#' \dontrun{
#' # dev.new();
#' dprint(fmla=group+level~., data=table1)
#' dev.off()
#' # dev.new()
#' dprint(fmla=group+level~Rn(round(Mean1, 2), "Mean Trt")+Rn(round(Variance1,2), "Variance"), 
#' data=table1)
#' dev.off()
#' # dev.new()
#' dprint(group+level~Rn(round(Mean1, 2), "Mean Trt")+
#' Variance1+Rn(round(I((Mean1+Mean2)/2),2), "Average of Averages"), data=table1, main="Don't Do this")
#' dev.off()
#' # dev.new()
#' dprint(level~.|group2, data=table2)
#' dev.off()
#' # dev.new();
#' dprint(level~Mean1+Median2|group2, data=table2, main="Descriptives")
#' dev.off()
#' # dev.new(); # Spanning, embedded fuctions, and conditional
#' dprint(group+level~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"),
#'  "Mean Trt (Std)")|group2, data=table2)
#' dev.off()
#' # dev.new(); # Spanning, embedded fuctions, and conditional
#' dprint(~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), 
#' "Mean Trt (Std)")|group2, data=table2)
#' # dev.new(); # Spanning, embedded fuctions, and conditional
#' dev.off()
#' dprint(~Treatment:(Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")+
#' Rn(round(Median1,2), "Median"))|group2, data=table2)
#' dev.off()
#' # dev.new()
#' dprint(~Treatment:Rn(paste(round(Mean1, 2),"(", round(Variance1, 2),")"), "Mean Trt (Std)")+
#' Control:Rn(paste(round(Mean2, 2),"(", round(Variance2, 2),")"), "Mean Trt (Std)")|group2,
#'  data=table2)
#' dev.off()
#'
#' f1 <- group+level~Treatment:Rn(Fc(Mean1, Variance1), "Mean (Std)")+
#' Control:Rn(Fc(Mean2, Variance2), "Mean (Std)") + Rn(round(p.value,2), "P-value")
#' # dev.new()
#' dprint(fmla=f1, data=table1,margins=.2, main="Justify Center")
#' dev.off()
#' # dev.new()
#' dprint(fmla=f1, data=table1,margins=.2, main="Justify Right", 
#' style=style(justify="right", frmt.tbl=frmt(bty="o")))
#' dev.off()
#' # dev.new()
#' dprint(fmla=f1, data=table1,margins=.2, main="Justify Left", 
#' style=style(justify="left", frmt.tbl=frmt(bty="o")))
#' dev.off()
#'
#'  h <- expression(hdr("Test Header", 
#'  pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(1, .5))))
#'  f <- expression(ftr("R Package tabulaR", 
#'  pagelayout.obj=pagelayout(dtype="rgraphics", margins=c(1.25, 1, 1.25,1)), 
#'  pagenum=eval.parent(pagenum, 1)))
#'  # dev.new()
#'  dprint(fmla=f1, data=table1,margins=c(1.25, 1, 1.25,1), showmargins=TRUE, main="Table Left",
#'             style=style(justify="left", frmt.tbl=frmt(bty="o"), 
#'            frmt.bdy=frmt(linespace=1.5, bty="X")),
#'             f.hdr = h, f.ftr=f, pagenum=1)
#' dev.off()
#'
#' # dev.new()
#'  dprint(fmla=f1, data=table1,margins=c(1.25, 1, 1.25,1), showmargins=TRUE, main="Table Left",
#'             style=CBs,
#'             f.hdr = h, f.ftr=f, pagenum=1)
#'  dev.new()
#'  by_var_f1 <- level~Mean1+Median1|group
#'  by_var_f2 <- level~Mean1+Median1|group+group2
#' # If main is default (null) than do not print titles
#'  dprint(fmla=by_var_f1, data=table2)
#'  dev.off()
#' # dev.new()
#' # When a title is defined, and only one conditional variable is defined, just print the values
#' # concatenated to the text
#' dprint(fmla=by_var_f1, data=table2,main=" ")
#' dev.off()
#' # dev.new()
#' # When more than one conditional variable, concatenate the variable name and the
#' # current combination of values
#' dprint(fmla=by_var_f2, data=table2,main="Descriptives for: ")    
#' }
dprint <-
  function(data,              # Input Data.frame
           fmla=NULL,         # Formula interface to define table structure
           label = NULL,      # label & group are characters identifying columns that define the simple table structure
           group = NULL,
           regx=NA,           # Regular Expression to take off of colnames, designed to break unwanted tiebreakers for legal data.frame columnnames
           style=NULL,        # List of graphical parameters or table format options returned from STYLE(),
           main=NA,           # Table Title, Vector of Strings
           footnote=NA,       # Footnote, Vector of Strings
           dtype="rgraphics", # Device type, currently available "rdevice", "portrait", "landscape"
           pg.dim=NULL,       # If custom page dimensions define a vector of c(height, width) units. Custom page dimensions over ride dtype
           margins=NULL,      # A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
           showmargins=FALSE, # Display margins on page in red
           row.hl=NULL,       # Conditional highlight; row.hl(dx=, col=)
           fit.width=FALSE,   # Forces the table to fit the table horizontally
           fit.height=FALSE,  # Forces the table to fit the table vertically, if conditional variabled defined will force the first table to fit one page and same ratio will be applied to all tables
           fit=FALSE,         # Force the table to fit both horizontally&vertically (smaller of two cex calulcations)
           newpage=FALSE,     # when the device runs out of space, when TRUE a new page will automatically be started
           center.horz=FALSE, # Center table horizontally.
           center.vert=FALSE, # Center table vertically, should only be used on one table
           center=FALSE,      # Center both vertically and horizontally. These should probably not be used with the fit.* parameters
           f.hdr=NULL,        # Function for printing header
           f.ftr=NULL,        # Function for printing Footer
           pagenum=NULL,      # Starting page number, will override pagenumber from lastcall
           lastcall=NULL      # Last call from
  )
{ UseMethod("dprint", data)
  #dprint.data.frame(...)
  }

