#' Function to calculate statistics per month
#' @param tableau A table with the following columns : No.pas,debut_pas,fin_pas,              
#' ope_dic_identifiant,lot_tax_code,lot_std_code,type_de_quantite,MESURE,CALCULE,               
#' EXPERT,PONCTUEL,Effectif_total,taux_d_echappement,coe_valeur_coefficient
#' @note this function is intended to be called from within the summary method 
#' @param time.sequence Passed from report_mig or report_mig_mult
#' @param taxa  Taxa
#' @param stage The Stage
#' @param DC  The counting device
#' @param silent Message displayed or not
#' @return No return value, called for side effects
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @export
funstat = function(tableau, time.sequence, taxa, stage, DC, silent) {
    if (!silent)
        funout(gettext("Calculation of the monthly balance sheet\n", domain = "R-stacomiR"))
    mois = strftime(as.POSIXlt(time.sequence), "%m")
    moislab = unique(mois)
    annee = paste(unique(strftime(as.POSIXlt(time.sequence), "%Y")), collapse = ",")
    somme = tapply(tableau$Effectif_total, mois, sum, na.rm = TRUE)  # sums
    moyennes_journalieres = tapply(tableau$Effectif_total, mois, mean, na.rm = TRUE)  # means
    # ecarts_types=tapply(tableau$Effectif_total, mois, sd, na.rm=TRUE) # std.
    # deviations nombre=as.integer(tapply(tableau$Effectif_total, mois,
    # function(x) sum(!is.na(x)))) # counts
    resum = rbind(somme, moyennes_journalieres)  #,moyennes_journalieres,ecarts_types,nombre)
		if (taxa == "Anguilla anguilla" & stage == "civelle") {
        poids_depuis_effectif = tapply(tableau$poids_depuis_effectif, mois, sum,
            na.rm = TRUE)
        poids_mesure = tapply(tableau$Poids_total, mois, sum, na.rm = TRUE)
        Poids_total = poids_depuis_effectif + poids_mesure
        resum = rbind(somme, moyennes_journalieres, poids_depuis_effectif, poids_mesure,
            Poids_total)
    }
    resum = resum[, moislab, drop = FALSE]
    resum = as.data.frame(resum)
    resum["somme", "year"] = round(sum(tableau$Effectif_total, na.rm = TRUE), 2)
    resum["moyennes_journalieres", "year"] = mean(tableau$Effectif_total, na.rm = TRUE)
    # resum['moyennes_journalieres','year']=round(mean(tableau$Effectif_total,
    # na.rm=TRUE),2)
    # resum['ecarts_types','report']=round(sd(tableau$Effectif_total,
    # na.rm=TRUE),2)
    if (taxa == "Anguilla anguilla" & stage == "civelle") {
        resum["poids_depuis_effectif", "year"] = round(sum(tableau$poids_depuis_effectif,
            na.rm = TRUE), 2)
        resum["poids_mesure", "year"] = round(sum(tableau$Poids_total, na.rm = TRUE),
            2)
        resum["Poids_total", "year"] = round(sum(Poids_total, na.rm = TRUE), 2)
    }
    resum = cbind(label = paste("DC", DC, taxa, stage, annee, sep = "_"), resum)
    # funout(paste(DC,taxa,stage,annee,'\n'))
    # funout(paste('DC','code_taxa','code_stage','annee','\n'))
    if (!silent) {
        funout(gettext("Calculation of the monthly balance sheet\n", domain = "R-stacomiR"))
        print(resum["somme", ])
    }
    return(resum)
}
