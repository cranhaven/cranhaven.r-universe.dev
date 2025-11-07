UNE_SECONDE     = as.difftime(c("0:0:1"))

UNE_MINUTE      = 60 * UNE_SECONDE

DIX_MINUTES     = 10 * UNE_MINUTE

QUINZE_MINUTES  = 15 * UNE_MINUTE

TRENTE_MINUTES  = 30 * UNE_MINUTE

UNE_HEURE       = 60 * UNE_MINUTE

DOUZE_HEURES    = 12 * UNE_HEURE

UN_JOUR         = 24 * UNE_HEURE

UNE_SEMAINE     = 7 * UN_JOUR

DEUX_SEMAINES   = 2 * UNE_SEMAINE

UN_MOIS         = 30 * UN_JOUR

TROIS_MOIS      = 91 * UN_JOUR

SIX_MOIS        = 182 * UN_JOUR

UN_AN           = 365 * UN_JOUR


Valeurref_timestep = c(
  UNE_SECONDE,
  UNE_MINUTE,
  DIX_MINUTES,
  QUINZE_MINUTES,
  TRENTE_MINUTES,
  UNE_HEURE,
  DOUZE_HEURES,
  UN_JOUR,
  UNE_SEMAINE,
  DEUX_SEMAINES,
  UN_MOIS,
  TROIS_MOIS,
  SIX_MOIS,
  UN_AN
)
Labelref_timestep = c(
  "1 sec",
  "1 min",
  "10 min" ,
  "15 min" ,
  "30 min",
  "1 h"   ,
  "12 h"  ,
  "1 jour"   ,
  "1 sem" ,
  "2 sem"  ,
  "1 mois" ,
  "3 mois" ,
  "6 mois" ,
  "1 an"
)
Lesref_timestep = data.frame("Valeurref_timestep" = Valeurref_timestep)
Lesref_timestep[, "Labelref_timestep"] = Labelref_timestep
rownames(Lesref_timestep) =
  c(
    "UNE_SECONDE",
    "UNE_MINUTE",
    "DIX_MINUTES",
    "QUINZE_MINUTES",
    "TRENTE_MINUTES",
    "UNE_HEURE",
    "DOUZE_HEURES",
    "UN_JOUR",
    "UNE_SEMAINE",
    "DEUX_SEMAINES",
    "UN_MOIS",
    "TROIS_MOIS",
    "SIX_MOIS",
    "UN_AN"
  )
rm(
  UNE_SECONDE,
  UNE_MINUTE,
  DIX_MINUTES,
  QUINZE_MINUTES,
  TRENTE_MINUTES,
  UNE_HEURE,
  DOUZE_HEURES,
  UN_JOUR,
  UNE_SEMAINE,
  DEUX_SEMAINES,
  UN_MOIS,
  TROIS_MOIS,
  SIX_MOIS,
  UN_AN,
  Labelref_timestep
)


validity_ref_timestep = function(object)
{
  retValue = NULL
  rep1 = inherits(object@dateDebut[1], "POSIXlt")
  if (!rep1)
    retValue = "object@dateDebut is not of class POSIXlt"
  rep2 = length(object@step_duration) == 1
  if (!rep2)
    retValue = paste(retValue, "length(object@step_duration) !=1")
  rep3 = length(object@nb_step) == 1
  if (!rep3)
    retValue = paste(retValue, "length(object@nb_step) !=1")
  rep4 = length(object@nocurrent_step) == 1
  if (!rep4)
    retValue = paste(retValue, "length(object@nocurrent_step) !=1")
  return(ifelse(rep1 & rep2 & rep3 & rep4, TRUE, retValue))
}

#' Class "ref_timestep"
#'
#' Describes a time step
#'
#'
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_timestep",
#' dateDebut="POSIXt",step_duration=numeric(),nb_step=numeric(),nocurrent_step=integer())}.
#' \describe{
#' \item{list("dateDebut")}{Object of class \code{"POSIXt"} Starting
#' date }
#' \item{:}{Object of class \code{"POSIXt"} Starting date }
#' \item{list("step_duration")}{Object of class \code{"numeric"} Step length
#' }\item{:}{Object of class \code{"numeric"} Step length }
#' \item{list("nb_step")}{Object of class \code{"numeric"} Number of steps
#' }\item{:}{Object of class \code{"numeric"} Number of steps }
#' \item{list("nocurrent_step")}{Object of class \code{"integer"} Number of the
#' current step }\item{:}{Object of class \code{"integer"} Number of the
#' current step } }
#' @author cedric.briand@eptb-vilaine.fr
#' @seealso \code{\linkS4class{ref_timestep_daily}}
#' @concept report Object
setClass(
  Class = "ref_timestep",
  representation =
    representation(
      dateDebut = "POSIXlt",
      step_duration = "numeric",
      nb_step = "numeric",
      nocurrent_step = "integer"
    ),
  validity = validity_ref_timestep,
  prototype = prototype(
    dateDebut = as.POSIXlt(Hmisc::truncPOSIXt(Sys.time(), "year")),
    step_duration = as.numeric(86400),
    nb_step = as.numeric(1),
    nocurrent_step = as.integer(0)
  )
)
# timestep= new("ref_timestep")


validity_ref_timestepChar = function(object)
{
  rep1 = inherits(object@dateDebut[1],"POSIXlt")
  rep2 = length(object@step_duration) == 1
  rep3 = length(object@nb_step) == 1
  rep4 = length(object@nocurrent_step) == 1
  rep5 = object@step_duration %in% Lesref_timestep[, "Labelref_timestep"]
  return(ifelse(rep1 &
                  rep2 &
                  rep3 & rep4 & rep5, TRUE, c(1:5)[!c(rep1, rep2, rep3, rep4, rep5)]))
}
#' Class "ref_timestepChar"
#'
#' Character to represent a ref_timestep
#'
#'
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_timestepChar", \dots{})}
#' @author cedric.briand@eptb-vilaine.fr
#' @seealso \code{\linkS4class{ref_timestep}}
#' @keywords classes
#' @examples
#'
#' showClass("ref_timestepChar")
#'
setClass(
  Class = "ref_timestepChar",
  representation =
    representation(
      dateDebut = "POSIXlt",
      step_duration = "character",
      nb_step = "numeric",
      nocurrent_step = "integer"
    ),
  validity = validity_ref_timestepChar,
  prototype = prototype(
    dateDebut = as.POSIXlt(
      strptime("2008-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"),
      tz = "GMT"
    ),
    step_duration = as.character("1 jour"),
    nb_step = as.numeric(1),
    nocurrent_step = as.integer(0)
  )
)

setAs("ref_timestepChar", "ref_timestep",   # from to
      function(from, to) {
        index = Lesref_timestep[, "Labelref_timestep"] %in% from@step_duration
        newstep_duration = Lesref_timestep[index, "Valeurref_timestep"]
        new(
          "ref_timestep",
          dateDebut = from@dateDebut,
          step_duration = newstep_duration,
          nb_step = from@nb_step,
          nocurrent_step = from@nocurrent_step
        )
      })
# timestep=as(timestepChar,"ref_timestep")


#' Gets the final horodate for an object of class \link{ref_timestep-class}
#' @param object An object of class \link{ref_timestep-class}
#' @return end_date, The final date corresponding to nb_step*time duration + initial date
#' @keywords internal
setMethod(
  "end_date",
  signature = signature("ref_timestep"),
  definition = function(object) {
    end_date = object@dateDebut + object@step_duration * (object@nb_step)
    # pour les pb de changement d'heure
    
    return(end_date)
  }
)




#' Gets the year or a vector of years corresponding to the timestep ("ref_timestep") object
#' @param object An object of class \link{ref_timestep-class}
#' @return A numeric with year or vector of years corresponding to the timestep
#' @keywords internal
setMethod(
  "get_year",
  signature = signature("ref_timestep"),
  definition = function(object) {
    dateFin = end_date(object)
    dateDebut = object@dateDebut
    seq = seq.POSIXt(from = dateDebut, to = dateFin, by = "day")
    seq = seq[-length(seq)]
    annees = unique(strftime(seq, "%Y"))
    return (as.numeric(annees))
  }
)

