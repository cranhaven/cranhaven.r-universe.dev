

#' @import ggplot2
#' @import RPostgres
#' @import methods
#' @import RColorBrewer
#' @import stacomirtools
#' @import stringr
#' @import xtable
#' @importFrom dplyr select group_by summarize rename do filter mutate min_rank first ungroup desc across all_of
#' @importFrom graphics layout matplot mtext points polygon segments par axis text legend rect axis.Date abline arrows hist
#' @importFrom grDevices gray rainbow adjustcolor gray.colors dev.new
#' @importFrom grid gpar grid.newpage grid.layout pushViewport viewport
#' @importFrom Hmisc wtd.quantile 
#' @importFrom Hmisc capitalize
#' @importFrom intervals interval_overlap closed<- Intervals 
#' @importFrom lattice barchart trellis.par.get trellis.par.set simpleKey
#' @importFrom lubridate round_date floor_date %m+% isoweek years
#' @importFrom magrittr %>%
#' @importFrom mgcv gam
#' @importFrom pool poolClose dbExecute dbWriteTable dbGetQuery
#' @importFrom reshape2 dcast melt
#' @importFrom stats as.formula coef na.fail nls pbeta predict sd coefficients complete.cases sd AIC xtabs ftable
#' @importFrom utils read.csv stack globalVariables select.list write.table data txtProgressBar
#' @importFrom withr defer
NULL

# Variables used in aes arguments generate a note as being assigned to
# .GlobalEnv listing them below removes the warning
# in Rcheck.
utils::globalVariables(c("quinzaine", "mois", "val_quant", "time.sequence", "Effectifs",
				 "Cumsum", "Date", "Effectif", "Effectif_total", "annee", "car_val_identifiant", # "..density.."
				"car_valeur_quantitatif", "coef", "date_format", "debut_pas", "effectif", "effectif_CALCULE",
				"effectif_EXPERT", "effectif_MESURE", "effectif_PONCTUEL", "effectif_total",
				"report_df", "quantite_CALCULE", "quantite_EXPERT", "quantite_MESURE", "quantite_PONCTUEL",
				"libelle", "null", "type", "val_libelle", "lot_effectif", "lot_identifiant",
				"ope_dic_identifiant", "ope_identifiant", "dev_code", "dev_libelle", "ope_date_fin",
				"report_stage_pigm", "ope_date_debut", "p", "g", "poids_moyen", "taxa_stage",
				"jour", "valeur", "mintab", "maxtab", "moyenne", "jour", "total_annuel", "taxa_stage",
				"time.sequence", "sum", "variable", "duree", "Hdeb", "Hfin", "per_tar_code",
				"per_etat_fonctionnement", "std_libelle", "sumduree", "dc", "stage", "taxa",
				"stage", "ouv", "Q0", "Q100", "Q5", "Q50", "Q95", "age", "bjo_annee", "bjo_labelquantite",
				"bjo_valeur", "doy", "pred_weight", "pred_weight_lwr", "pred_weight_upr", "total",
				"w", "year", "sta", "tableauCEst", "stm_libelle", "env_valeur_quantitatif", "env_val_identifiant",
				"DC", "color", "id", "day","density","fortnight","env_date_debut","comp","std_value"))

# variable used by dplyr
utils::globalVariables(c("n0", "newid", "xmin", "xmax", "fin_pas", "value", "type_de_quantite",
				"lot_tax_code", "lot_std_code", "lot_methode_obtention", "no.pas"))

# dataset used in the package
utils::globalVariables(c("coef_durif"))
# Assignation in global environment for the use of gWidget interface (there is
# no way arround this)
# utils::globalVariables(c('win','group','nbligne','ggrouptotal','ggrouptotal1','gSortie',
#     'col.sortie','ggroupboutons','ggroupboutonsbas','groupdate','groupdc',
#     'frame_annee','frame_check','frame_choice','frame_par','frame_parqual','frame_parquan',
#     'frame_std','frame_tax','frame_annee','frame_check','frame_choice','ref_year',
#     'logw','report_stage_pigm','usrname','usrpwd','notebook','values','ind'))
# Progressbar utils::globalVariables(c('progres')) environment
# utils::globalVariables(c('envir_stacomi')) reoxygenize fails if data are not
# loaded setwd('F:/workspace/stacomir/branch0.5/stacomir')
