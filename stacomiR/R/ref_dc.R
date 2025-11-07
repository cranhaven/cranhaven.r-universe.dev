#' Class 'ref_dc'
#'
#' Description of a control device.
#'
#' @include create_generic.R
#' @slot dc_selected Object of class \code{'integer'}, The selected device
#' @slot ouvrage Object of class \code{'integer'}, the attached dam
#' @slot station Object of class \code{'character'}, the attached migration monitoring station, this is necessary to join the
#' table of escapements calculated at the station level.
#' @slot data Object of class \code{'data.frame'} data pertaining to the control device
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new('ref_dc', dc_selected=integer(), ouvrage=integer(),
#' data=data.frame())}.
#' @author cedric.briand@eptb-vilaine.fr
#' @keywords classes
#' @family referential objects
setClass(
    Class = "ref_dc",
    representation = representation(
        dc_selected = "integer",
        ouvrage = "integer",
        station = "character",
        data = "data.frame"
    ),
    prototype = prototype(
        dc_selected = integer(),
        ouvrage = integer(),
        station = character(),
        data = data.frame()
    )
)



setValidity(
    "ref_dc",
    method = function(object) {
        if (length(object@dc_selected) != 0) {
            if (nrow(object@data) > 0) {
                concord <- object@dc_selected %in% object@data$dc
                if (any(!concord)) {
                    return(paste("No data for DC", object@dc_selected[!concord]))
                    
                } else {
                    return(TRUE)
                }
            } else {
                return(
                    "You tried to set a value for dc_selected without initializing the data slot"
                )
            }
        } else
            return(TRUE)
        
    }
)


#' Method to load the counting devices of the control station
#' @param object An object of class \link{ref_dc-class}
#' @return an object of class ref_dc with data loaded
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
setMethod(
    "charge",
    signature = signature("ref_dc"),
    definition = function(object) {
        requete = new("RequeteDB")
        requete@sql = paste(
            "select dis_identifiant as DC,",
            " dis_date_creation,",
            " dis_date_suppression,",
            " dif_dis_identifiant as DF,",
            " dis_commentaires,",
            " dif_ouv_identifiant,",
            " ouv_libelle,",
            " dif_code as DF_code,",
            " dic_code as DC_code,",
            " dif_localisation,",
            " dif_orientation,",
            " tdf_libelle as type_DF,",
            " tdc_libelle as type_DC,",
            "sta_code",
            " FROM ",
            get_schema(),
            "tg_dispositif_dis",
            " JOIN ",
            get_schema(),
            "t_dispositifcomptage_dic ON dic_dis_identifiant =dis_identifiant",
            " JOIN ",
            get_schema(),
            "t_dispositiffranchissement_dif ON dif_dis_identifiant=dic_dif_identifiant",
            " JOIN ",
            get_schema(),
            "t_ouvrage_ouv on dif_ouv_identifiant=ouv_identifiant",
            " JOIN ",
            get_schema(),
            "t_station_sta on ouv_sta_code=sta_code",
            " JOIN ref.tr_typedc_tdc ON dic_tdc_code=tdc_code",
            " LEFT JOIN (SELECT * FROM ",
            get_schema(),
            "tj_dfesttype_dft",
            " JOIN ref.tr_typedf_tdf ON tdf_code=dft_tdf_code",
            " WHERE  dft_rang=1) sub ON dif_dis_identifiant=dft_df_identifiant",
            " ORDER BY dis_identifiant;",
            sep = ""
        )
        requete <- stacomirtools::query(requete)
				if (grepl("Error",requete@status)) stop(requete@status)	
        # funout(gettext('The query to load counting devices is done
        # \n',domain='R-stacomiR'))
        object@data <- requete@query
        return(object)
    }
)



#' Command line interface to select a counting device
#'
#' the choice_c method is intended to have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line.  The parameters for dc are transformed to integer as the ref_dc only
#' takes integer in the dc slots. The method also loads the stations and ouvrages (dams) associated with the counting device (dc).
#' The values passed to the choice_c method are then checked with the setValidty method.
#' Finally, if an objectreport is passed as a parameter, the method will do a charge_with_filter to select only the taxa present in the counting devices
#' @param object an object of class ref_dc
#' @param dc a character vector of dc chosen
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @return An object of class ref_dc with dc selected
#' @examples
#' \dontrun{
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object=new('ref_dc')
#' object<-charge(object)
#' objectreport=new('report_mig_mult')
#' choice_c(object=object,objectreport=objectreport,dc=1)
#' }
setMethod(
    "choice_c",
    signature = signature("ref_dc"),
    definition = function(object,
                          dc) {
        if (inherits(dc, "numeric")) {
            dc <- as.integer(dc)
        } else if (inherits(dc, "character")) {
            dc = as.integer(as.numeric(dc))
        }
        if (any(is.na(dc)))
            stop("NA values dc")
        
        
        object@dc_selected <- dc
        validObject(object)
        # the method validObject verifies that the dc is in the data slot of
        # ref_dc  
        
        object@station <-
            as.character(object@data$sta_code[object@data$dc %in% object@dc_selected])
        object@ouvrage <-
            object@data$dif_ouv_identifiant[object@data$dc %in% object@dc_selected]
        assign("ref_dc", object, envir = envir_stacomi)
        return(object)
    }
)
