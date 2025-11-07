#' Report on operations
#'
#' Operations are monitoring operations generated between two dates. In the case of video monitoring
#' or similar, they can be instantaneous
#'
#' @include ref_dc.R
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("report_ope")}.
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @concept report Object
#' @keywords classes
#' @aliases report_ope
#' @export
setClass(
  Class = "report_ope",
  representation = representation(
    data = "data.frame",
    dc = "ref_dc",
    horodatedebut = "ref_horodate",
    horodatefin = "ref_horodate"
  ),
  prototype = prototype(
    data = data.frame(),
    dc = new("ref_dc"),
    horodatedebut = new("ref_horodate"),
    horodatefin = new("ref_horodate")
    
  )
)

#' connect method for report_ope
#'
#' @param object An object of class \link{report_ope-class}
#' load data from the operation table, one dataset per DC
#' @param silent Boolean, TRUE removes messages.
#' @return  An object of class \link{report_ope-class} with slot data \code{@data} filled
#' @aliases connect.report_ope
#' @author cedric.briand
setMethod(
  "connect",
  signature = signature("report_ope"),
  definition = function(object, silent = FALSE) {
    # object<-report_ope
    req <- new("RequeteDBwheredate")
    lesdc <- object@dc@dc_selected
    req@colonnedebut = "ope_date_debut"
    req@colonnefin = "ope_date_debut"
    req@order_by = "ORDER BY ope_dic_identifiant, ope_date_debut"
    req@datedebut <- object@horodatedebut@horodate
    #below to be consistet with BIlanMigrationMult
    req@datefin <-
      as.POSIXlt(object@horodatefin@horodate + as.difftime("23:59:59"))
    req@select <-
      paste("SELECT * FROM  ",
            get_schema(),
            "t_operation_ope ")
    req@and = paste("AND ope_dic_identifiant in",
                    stringr::str_c("(", stringr::str_c(lesdc, collapse = ","), ")"))
    req <-
      stacomirtools::query(req)
    object@data <- req@query
    if (!silent)
      funout(gettext("Loading data for operations", domain = "R-stacomiR"))
    return(object)
  }
)


#' charge method for report_ope
#'
#'
#' used by the graphical interface to retrieve referential classes
#' assigned to envir_stacomi
#' @param object An object of class \link{report_ope-class}
#' @param silent Keeps program silent
#' @return  An object of class \link{report_ope-class}  with slots filled from values assigned in \code{envir_stacomi} environment
#' @aliases charge.report_ope
#' @author cedric.briand
#' @keywords internal
setMethod(
  "charge",
  signature = signature("report_ope"),
  definition = function(object, silent = FALSE) {
    # object<-report_ope
    if (exists("ref_dc", envir = envir_stacomi)) {
      object@dc <- get("ref_dc", envir = envir_stacomi)
    } else {
      funout(
        gettext(
          "You need to choose a counting device, clic on validate\n",
          domain = "R-stacomiR"
        ),
        arret = TRUE
      )
    }
    
    if (exists("report_ope_date_debut", envir = envir_stacomi)) {
      object@horodatedebut@horodate <-
        get("report_ope_date_debut", envir = envir_stacomi)
    } else {
      funout(gettext("You need to choose the starting date\n", domain = "R-stacomiR"),
             arret = TRUE)
    }
    
    if (exists("report_ope_date_fin", envir = envir_stacomi)) {
      object@horodatefin@horodate <-
        get("report_ope_date_fin", envir = envir_stacomi)
    } else {
      funout(gettext("You need to choose the ending date\n", domain = "R-stacomiR"),
             arret = TRUE)
    }
    assign("report_ope", object, envir = envir_stacomi)
    return(object)
  }
)
