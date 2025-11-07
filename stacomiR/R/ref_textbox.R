#' ref_textbox referencial class 
#' 
#' allows to a put a value within a glabel
#' @author cedric.briand@eptb-vilaine.fr
#' @slot title='character' the title of the box giving the possible choices
#' @slot labels the logical parameters choice
#' @slot checked a vector
setClass(Class = "ref_textbox", representation = representation(title = "character",
    label = "character"))

#' Loading method for ref_textbox referential objects
#' @param object An object of class \link{ref_textbox-class}
#' @param title A title for the frame
#' @param label A label for the TextBox
#' @return An S4 object of class \link{ref_textbox-class}
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new('ref_textbox')
#' charge(object,title='un titre',label='20')
#' }
setMethod("charge", signature = signature("ref_textbox"), definition = function(object,
    title, label) {
    object@title = title
    object@label = label
    return(object)
})


#' Choice_c method for ref_textbox referential objects
#'
#' @param object An object of class \link{ref_textbox-class}
#' @param value The value to set
#' @param nomassign  The name with which the object will be assigned in envir_stacomi
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @return An S4 object of class \link{ref_textbox-class} label selected
setMethod("choice_c", signature = signature("ref_textbox"), definition = function(object,
    value, nomassign = "ref_textbox") {
    object@label <- value
    assign(nomassign, object, envir_stacomi)
    return(object)
})
