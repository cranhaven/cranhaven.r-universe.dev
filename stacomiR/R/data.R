#' Working environment for stacomiR created when launching stacomi()
#' 
#' This is where the graphical interface stores its objects
#' try \code{ls(envir=envir_stacomi)}
#' @keywords environment
"envir_stacomi"



#' Anguilla migration at the Arzal station (report_mig_mult-class)
#' 
#' This data corresponds to data collected from three fishways
#' and correspond to the migration station at Arzal (Vilaine estuary, France) in 2011 for 
#' three continental stages of eel (Anguilla anguilla) : glass eel, yellow eel
#' and silver eel.
#'
#' @format An object of class report_mig_mult with slots:
#' \describe{
#'   \item{dc}{the \code{ref_dc} object filled with data}
#'   \item{taxa}{the \code{ref_taxa} object filled in with data corresponding to dc}
#'   \item{stage}{the \code{ref_stage} object filled in with data corresponding to dc, and taxa}
#'   \item{timestep}{the \code{ref_timestep_daily} calculated for all 2011}
#'   \item{data}{ A dataframe with 400 rows and 11 variables
#'   \describe{
#'     \item{ope_identifiant}{operation id}
#'     \item{lot_identifiant}{sample id}
#'     \item{lot_identifiant}{sample id}
#'     \item{ope_dic_identifiant}{dc id}
#'     \item{lot_tax_code}{species id}
#'     \item{lot_std_code}{stage id}
#'     \item{value}{the value}
#'     \item{type_de_quantite}{either effectif (number) or poids (weights)}
#'     \item{lot_dev_code}{destination of the fishes}
#'     \item{lot_methode_obtention}{method of data collection, measured, calculated...} 
#'     }
#'   }
#'   \item{calcdata}{slot to be filled with the calcule method} 
#'   \item{coef_conversion}{A data frame with 364 observations with daily coefficients to convert from weight to numbers}
#'   \item{time.sequence}{A time sequence generated for the report, used internally by the object}
#' }
#' @keywords data
"r_mig_mult"


#' Video counting of Marine lamprey (Petromyzon marinus) in 2012 in the Vilaine (France)
#' 
#' This dataset corresponds to the data collected at the vertical slot fishway
#' in 2012, video recording marine lamprey migration
#'
#' @format An object of class report_mig with 8 slots:
#' \describe{
#'   \item{dc}{the \code{ref_dc} object with 4 slots filled with data corresponding to the iav postgres schema}
#'   \item{taxa}{the \code{ref_taxa} the taxa selected}
#'   \item{stage}{the \code{ref_stage} the stage selected}
#'   \item{timestep}{the \code{ref_timestep_daily} calculated for all 2015}
#'   \item{data}{ A dataframe with 10304 rows and 11 variables
#'   \describe{
#'     \item{ope_identifiant}{operation id}
#'     \item{lot_identifiant}{sample id}
#'     \item{lot_identifiant}{sample id}
#'     \item{ope_dic_identifiant}{dc id}
#'     \item{lot_tax_code}{species id}
#'     \item{lot_std_code}{stage id}
#'     \item{value}{the value}
#'     \item{type_de_quantite}{either effectif (number) or poids (weights)}
#'     \item{lot_dev_code}{destination of the fishes}
#'     \item{lot_methode_obtention}{method of data collection, measured, calculated...} 
#'     }
#'   }
#'   \item{coef_conversion}{A data frame with 0 observations : no quantity are reported for video recording of mullets, only numbers}
#'   \item{time.sequence}{A time sequence generated for the report, used internally}
#' }
#' @keywords data
"r_mig"

#' Fishway operation at the Arzal Dam (Vilaine France) (3 Fishways in 2011)
#' 
#' This dataset corresponds to the data collected at three different fishways
#' it is loaded along with \link{r_mig_mult} and used in demonstration for the
#' \link{report_mig_mult-class}
#' @format An object of class report_df  \link{report_df-class}
#' @keywords data
"r_mig_mult_df"


#' Counting device operation for three different counting device in Arzal (Vilaine, France)
#' 
#' This dataset corresponds to data collected at three different control devices.
#' This object is of class \link{report_dc-class} with data loaded
#' it is loaded along with \link{r_mig_mult} and used in demonstration for the
#' \link{report_mig_mult-class}
#' @format An object of class report_dc with 4 slots
#' \describe{
#'   \item{data}{ A dataframe with 25 rows and 7 variables
#'   \describe{
#'     \item{per_dis_identifiant}{the df or dc unique id}
#'     \item{per_date_debut}{the starting date of the counting device operation \code{POSIXct}}
#'     \item{per_date_fin}{the ending date of the counting device operation \code{POSIXct}}
#'     \item{per_commentaires}{comments on the counting device operation}
#'     \item{per_etat_fonctionnement}{Boolean, is the counting device working ?}
#'     \item{lot_std_code}{stage id}
#'     \item{per_tar_code}{The type of operation for the DC, 1 normal operation, 2 device stopped in normal
#'     operation (the stop is considered as normal, e.g. you don't monitor video if a cage has been placed to trap fishes), 
#'     3 stopped for maintenance or other problem, 4 the DC is working but not well (escapement in a tank, high turbidity preventing
#' video counting...), 5 unknown operation.}
#'     \item{libelle}{The label for the type or operation}
#'     }
#' }
#'   \item{dc}{the \code{ref_dc} the DC with 4 slots
#'   \describe{
#'    \item{dc_selected}{the selected device}
#'      \item{ouvrage}{the dam}
#'    \item{station}{the monitoring station, a section of river}
#'    \item{data}{A dataset of all dc present in the database with 10 observations}
#'   }
#' }
#'   \item{horodatedebut}{the beginning date, a \link{ref_horodate-class}}
#'   \item{horodatefin}{the ending date, a \link{ref_horodate-class}}
#' }
#' @keywords data
"r_mig_mult_dc"

#' Counting operations for three different counting device in Arzal (Vilaine, France)
#' 
#' This dataset corresponds to the data collected at three different control devices
#' It is an object of class \link{report_ope-class} with data loaded.
#' it is loaded along with \link{r_mig_mult}
#' @format An object of class report_ope
#' @keywords data
"r_mig_mult_ope"

#' Overview of the fishway operation at Arzal in (Vilaine France).
#' 
#' This dataset corresponds to the data collected at the vertical slot fishway
#' in 2015, the fishway is working daily with a cycle depending on tide. This dataset
#' is used to show an example of detailed output for an object of class \link{report_df-class} with data loaded
#'
#' @format An object of class report_df with 4 slots:
#' \describe{
#'   \item{data}{ A dataframe with 4261 obs. of  7 variables
#'   \describe{
#'     \item{per_dis_identifiant}{The number of the DF}
#'     \item{per_date_debut}{Starting time a POSIXct}
#'     \item{per_date_fin }{Ending time a POSIXct}
#'     \item{ope_dic_identifiant}{DF id}
#'     \item{per_commentaires }{A comment}
#'     \item{per_etat_fonctionnement}{Integer 1= working, 0 not working}
#'     \item{per_tar_code}{The type of operation ('1'=normal operation,
#'              '2'=Device stopped in normal operation (ie lift ascending, high tide...),
#'    '3'='Stopped for maintenance or other problem',
#'              '4'='Works but not fully operational,i.e.flow problem, flood, clogged with debris...',
#'              '5'='Not known')}
#'     \item{libelle}{label corresponding to per_tar_code}
#'            }
#'        }
#'   \item{df}{the \code{ref_df} object with 3 slots filled with data corresponding to the iav postgres schema}
#'   \item{horodatedebut}{the \code{ref_horodate} with horodate set for starting date}
#'   \item{horodatefin}{the \code{ref_horodate} with horodate set for ending date}'   
#' }
#' @keywords data
"r_df"

#' Counting Device (DC) operation from 2000 to 2015 at the Arzal dam (Vilaine, France)
#' 
#' This data corresponds to the data collected at the vertical slot fishway camera
#' from 2000 to 2015. It represents an object of class \link{report_dc-class} 
#' with data loaded
#'
#' @format An object of class report_dc with 4 slots:
#' \describe{
#'   \item{data}{ A dataframe with 544 obs. of  7 variables
#'   \describe{
#'     \item{per_dis_identifiant}{The number of the DC}
#'     \item{per_date_debut}{Starting time a POSIXct}
#'     \item{per_date_fin }{Ending time a POSIXct}
#'     \item{ope_dic_identifiant}{DC id}
#'     \item{per_commentaires }{A comment}
#'     \item{per_etat_fonctionnement}{Integer 1= working, 0 not working}
#'     \item{per_tar_code}{The type of operation ('1'=normal operation,
#'              '2'=Device stopped in normal operation (e.g. the trap is disactivated for the duration of the
#'     fish sorting and counting by operators),
#'    '3'='Stopped for maintenance or other problem',
#'              '4'='Works but not fully operational, i.e. the camera is not working properly because of high turbidity...',
#'              '5'='Not known')}
#'     \item{libelle}{label corresponding to per_tar_code}
#'            }
#'        }
#'   \item{df}{the \code{ref_dc} object with 3 slots filled with data corresponding to the iav postgres schema}
#'   \item{horodatedebut}{the \code{ref_horodate} with horodate set for starting date}
#'   \item{horodatefin}{the \code{ref_horodate} with horodate set for ending date}   
#' }
#' @keywords data
"r_dc"

#' Fishway operation for the vertical slot fishway (Arzal dam, Vilaine, France).
#' 
#' This dataset corresponds to the data collected at in the vertical slot fishway
#' it is loaded along with \link{r_mig} and used to demonstrate the \link{report_mig-class}
#' when the database is not installed. 
#' @format An object of class \link{report_df-class} 
#' @keywords data
"r_mig_df"


#' Counting device operation for the video recording (Arzal dam, Vilaine, France).
#' 
#' This dataset corresponds to the data collected in the vertical slot fishway for the video 
#'  recording operation. It is loaded along with \link{r_mig} to
#' demonstrate the use of the \link{report_mig-class} when the database is not loaded
#' @format An object of class \link{report_dc-class} 
#' @keywords data
"r_mig_dc"

#' An object of class \link{report_ope-class} with data loaded
#' 
#' This dataset corresponds to the data collected at the vertical slot fishway in Arzal (Vilaine river
#' estuary, France). The operation of the fishway is dependent on tide and is recorded every 10 minutes. This dataset has
#' to be loaded along with \link{r_mig} to demonstrate the use of the \link{report_mig-class}
#' @format An object of class report_ope
#' @keywords data
"r_mig_ope"

#' Size of yellow and glass eel at the Arzal dam (Vilaine, France) in the fishway and main eel trapping ladder.
#' 
#' This dataset corresponds to the data collected at two different control devices
#' at the Arzal control station (see example in \link{report_sample_char-class}), all body size 
#' parameters (total size, size converted from pixel in video control) are used in example
#' @format An object of class \link{report_sample_char-class}
#' @keywords data
"r_sample_char"

#' Daily glass eel and elver migration from 1984 to 2016 in the Sevre Niortaise 
#' 
#' The first eel trapping ladder in France was built by Antoine Legault and the team from Rennes
#' in the Sevre Niortaise, Marais Poitevin. Also refurbished several times since 1984 it has been 
#' operational at the same location and provides one of the longest series of eel migration. 
#' For this reason,
#' the dataset has been loaded as an example for the report_mig_interannual-class. It has been
#' kindly provided by the parc du Marais Poitevin. The stage corresponds to small eels (elvers)
#' less than 150 mmm stage name 'PANG'
#' @format An object of class \link{report_mig_interannual-class} with data loaded.
#' @keywords data
"r_mig_interannual"

#' Annual migration of yellow and silver eel for three fishways / counting devices at the
#' Arzal dam (data from 1995 to 2016)
#' 
#' The dataset corresponds to the three fishways located on the Arzal dam, filled with annual data
#' @format An object of class \link{report_annual-class} with data slot loaded.
#' @keywords data
"r_ann"

#' Annual migration of salmon in the Adour and tributaries
#' 
#' The dataset corresponds to the fishways DC=33:40 of the Adour for adult migrant salmons
#' from 1996 to 2005 (annual counts). It has been kindly provided as an example set by the Migradour
#' association.
#' @format An object of class \link{report_annual-class} with data slot loaded.
#' @keywords data
"r_ann_adour"


#' Silver eel migration in the Somme
#' 
#' The dataset corresponds to the silver eel traps ('anguilleres) for 2015-2016.
#' This dataset has been kindly provided by the Federation de Peche de la Somme,
#' given the upstream location of the trap, most individuals are female
#' 
#' @format An object of class \link{report_silver_eel-class} with data slot loaded.
#' @keywords data
"r_silver"

#' Silvering index coefficients from Caroline Durif (2009) to predict silvering stage from morphological parameters
#' 
#' Classification scores are calculated by multiplying the metrics 
#' BL = body length, W = weight, MD = mean eye diameter (Dv+Dh)/2, and FL length of the pectoral fin,
#' with each parameter p as S=Constant+BL*p(bl)+W*p(W)... The stage chosen is the one achieving the 
#' highest score
#' @references Durif, C.M., Guibert, A., and Elie, P. 2009.
#' Morphological discrimination of the silvering stages of the European eel. 
#' In American Fisheries Society Symposium. pp. 103-111.
#'  \url{https://fishlarvae.org/common/SiteMedia/durif\%20et\%20al\%202009b.pdf}
"coef_durif"

#' Wet weight of glass eel from the trapping ladder (Arzal, Vilaine France) 
#' 
#' Data correspond to glass eel collected in the Vilaine at the trapping ladder (Arzal, France). The years selected are 2009 to 2012,
#' the query used in the \link{report_ge_weight-class} loads from 2008-08-01 to 2012-08-01
#' Glass eel are too numerous to be counted. They are weighted and in the stacomi database,
#' a table with daily coefficients (in  N glass eel/g) to transform weight into number.
#' The weight is called a 'wet weight' as we don't wan't to drain any of the mucus in glass eel
#' when weighting them. Samples of 50 to 200 glass eel are weighted and then counted to provide an idea of
#' the seasonal evolution of wet weight.
"r_gew"

#' Seasonality of salmon migration at the Vichy counting station (Loire)
#' 
#' This data corresponds to the data collected at the Vichy fishway
#' between 1997 and 2012, video recording of the Salmo salar upstream migration.
#' This dataset has been kindly provided by Loire Grands Migrateurs.
#'
#' @format An object of class \link{report_mig_interannual-class} with 7 slots:
#' \describe{
#'   \item{dc}{the \code{ref_dc} object with 4 slots filled with data corresponding to the iav postgres schema}
#'   \item{taxa}{the \code{ref_taxa} the taxa selected}
#'   \item{stage}{the \code{ref_stage} the stage selected}
#'   \item{start_year}{the \code{ref_timestep_daily} calculated for all 2015}
#'   \item{end_year}{the \code{ref_timestep_daily} calculated for all 2015}
#'   \item{data}{ A dataframe with 7138 rows and 10 variables
#'   \describe{
#'     \item{bjo_identifiant}{sample id}
#'     \item{bjo_dis_identifiant}{dc id}
#'     \item{bjo_tax_code}{species id}
#'     \item{bjo_std_code}{stage id}
#'     \item{bjo_annee}{year}
#'     \item{bjo_jour}{date}
#'     \item{bjo_labelquantite}{method of data collection, measured, calculated...}
#'     \item{bjo_horodateexport}{date with special format for export}
#'     \item{bjo_org_code}{organisme provided the data}
#'     }
#'   }
#' }
#' @keywords data
"r_mig_interannual_vichy"


#' An object of class report_sea_age with data loaded
#' 
#' This dataset corresponds to the data collected at Vichy (left and right bank fishways) and Decize-Saint 
#' Leger des Vignes fishways (respectively on the Allier and Loire river, France) in 2012 on the size structure of Salmo salar.
#' It has been kindly provided by the Loire Grands Migrateurs (LOGRAMI) association.
#'
#' @format An object of class \link{report_sea_age-class} with 8 slots:
#' \describe{
#'   \item{dc}{the \code{ref_dc} : the control devices selected}
#'   \item{taxa}{the \code{ref_taxa} : Salmo salar selected}
#'   \item{stage}{the \code{ref_stage} : the stages selected}
#'   \item{par}{Object of class \link{ref_par-class}: the parameters used}
#' \item{horodatedebut}{object of class \code{ref_horodate-class} : the start date selected}
#' \item{horodatefin}{object of class \code{ref_horodate-class} : the end date selected} 
#'   \item{limit1hm}{The size limit, in mm between 1 sea winter fishes and 2 sea winter fishes}
#'   \item{limit2hm}{The size limit, in mm between 2 sea winter fishes and 3 sea winter fishes}
#'   \item{data}{ A dataframe with 898  rows and 20 variables
#'   \describe{
#'     \item{ope_identifiant}{operation id}
#'     \item{lot_identifiant}{sample id}
#'     \item{ope_dic_identifiant}{dc id}
#'     \item{ope_date_debut}{start date}
#'     \item{ope_date_fin}{end date}
#'     \item{lot_effectif}{number of fishes}
#'     \item{lot_tax_code}{species id}
#'     \item{lot_std_code}{stages id}
#'     \item{tax_nom_latin}{species latin names}
#'     \item{std_libelle}{stages names} 
#'     \item{dev_code}{destination of the fishes id}
#'     \item{dev_libelle}{destination of the fishes names}
#'     \item{par_nom}{parameter name}
#'     \item{car_par_code}{parameter id}
#'     \item{car_methode_obtention}{method of data collection, measured, calculated...}
#'     \item{car_valeur_quantitatif}{the value of the parameter}
#'     }
#'   }
#'  
#' }
#' @keywords data
"r_seaa"


#' An object of class report_env with data loaded
#' 
#' The dataset corresponds to the daily temperatures and moon phases in Arzal (Vilaine estuary, France). This environmental station is used to
#' analyze conditions in which fish migrated at Arzal dam
#'
#' @format An object of class \link{report_env-class} with data slot loaded:
#' \describe{
#'   \item{stationMesure}{the \code{ref_env} object with 5 slots filled with data corresponding to the iav postgres schema}
#'   \item{horodatedebut}{object of class \code{ref_horodate-class} : the start date selected}
#'   \item{horodatefin}{object of class \code{ref_horodate-class} : the end date selected}
#'   \item{data}{ A dataframe with 723 rows and 6 variables
#'   \describe{
#'     \item{env_date_debut}{start date}
#'     \item{env_date_fin}{end date}
#'     \item{env_methode_obtention}{method of data collection, measured, calculated...}
#'     \item{env_val_identifiant}{the value of the parameter if qualitative}
#'     \item{env_valeur_quantitatif}{the value of the parameter if quantitative}
#'     \item{env_stm_identifiant}{station id}
#'     }
#'   }
#' }
#' @keywords data
"r_env"


#' Qualitative and quantitative parameters describing Salmon migration at Decize (Loire)
#' 
#' The dataset corresponds to the characteristics (qualitative and quantitative) of salmo salar migrating
#' at Decize (Loire river) and Vichy (Allier river) counting device in 2012. It has been loaded as 
#' an example for the report_mig_char-class and kindly provided by Loire Grands Migrateurs (LOGRAMI).
#' @format An object of class \link{report_mig_char-class} with data slot loaded:
#' \describe{
#'   \item{calcdata}{slot to be filled with the calcule method}
#'   \item{data}{ A list of 2 elements
#'   \describe{
#'     \item{parqual}{values of all the qualitative parameters}
#'     \item{parquan}{values of all the quantitative parameters}
#'     }
#'   }
#'  \item{dc}{the \code{ref_dc} : the control devices selected}
#'  \item{taxa}{the \code{ref_taxa} : Salmo salar selected}
#'  \item{stage}{the \code{ref_stage} : the stages selected}
#'  \item{par}{an object of class \link{ref_par-class}: the parameters used}
#'  \item{horodatedebut}{an object of class \code{ref_horodate-class} : the start date selected}
#'  \item{horodatefin}{an object of class \code{ref_horodate-class} : the end date selected}
#' }
#' @keywords data
"r_mig_char"



#' An object of class report_mig_env with data loaded
#' 
#' The dataset correspond to data loaded for the Arzal dam (Vilaine) in 2008, two quantitative
#' parameters (temperature and tide coefficient) and a qualitative parameter (moon phase) are loaded. 
#' @format An object of class \link{report_env-class} with data slot loaded:
#' \describe{
#'   \item{report_mig_mult}{An object of class \link{report_mig_mult-class}}
#'   \item{report_env}{ An object of class \link{report_env-class}}#' 
#'   }
#' @keywords data
"r_mig_env"
