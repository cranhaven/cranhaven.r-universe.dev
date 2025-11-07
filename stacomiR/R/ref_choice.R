#' Class 'ref_choice'
#' 
#' ref_choice referential class allows to choose within several values with
#' radiobuttons interface
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new('ref_choice', listechoice=character() ,label=character()
#' ,selected=integer())}.
#' @slot listechoice A character vector giving possible choices
#' @slot label A character, title of the box giving the possible choices
#' @slot selected An \code{Integer}  the initial selected value (as an index), first=1 used in gradio
#' @author cedric.briand@eptb-vilaine.fr
#' @family referential objects
setClass(Class = "ref_choice", representation = representation(listechoice = "ANY",
    label = "character", selected = "integer", selectedvalue = "ANY"), prototype = list(selectedvalue = vector()))

#' Loading method for Refchoice referential objects
#' 
#' @family referential objects
#' @return An S4 object of class \link{ref_choice-class} 
#' @param object An object of class ref_choice
#' @param vecteur A vector of name, see example code.
#' @param label Labels for the choices
#' @param selected An integer indicating which object is selected at launch
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#' object=new('ref_choice')
#' charge(object,vecteur=c('oui','non'),label='essai',selected=as.integer(1))
#' }
setMethod("charge", signature = signature("ref_choice"), definition = function(object,
    vecteur, label, selected) {
    object@listechoice = vecteur
    object@label = label
    object@selected = selected
    object
    return(object)
})


#' Choice_c method for ref_choice referential objects
#' @param object An object of class \link{ref_choice-class}
#' @param selectedvalue the value selected in the combo
#' @return An S4 object of class \link{ref_choice-class} 
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @examples  
#' \dontrun{
#' object=new('ref_list')
#' object<-charge(object,vecteur=c('1','2'),label='please choose')
#' object<-choice_c(object)
#' }
setMethod("choice_c", signature = signature("ref_choice"), definition = function(object,
    selectedvalue) {

    if (length(selectedvalue) > 1)
        stop("valeurchoisie should be a vector of length 1")
    if (inherits(selectedvalue,"numeric"))
        selectedvalue <- as.character(selectedvalue)
    # the charge method must be performed before

    if (!selectedvalue %in% object@listechoice) {
        stop(stringr::str_c("The selected valeur,", selectedvalue, " not in the list of possible values :",
            stringr::str_c(object@listechoice, collapse = ",")))
    } else {
        object@selectedvalue <- selectedvalue
    }
    return(object)


})

