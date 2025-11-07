#' This writes monthly data in t_reportmensuel_mens table
#' 
#' @note This function is launched by fun_write_daily, the resum 
#' dataset is created by the \link{funstat} function
#' 
#' 
#' @param report_mig an object of class \code{\linkS4class{report_mig}}
#' @param resum data frame with summary per month
#' @param silent Suppresses messages
#' @return No return value, called for side effects
#' @export
fun_write_monthly<-function(report_mig,resum,silent){
	t_reportmigrationmensuel_bme <- stacomirtools::killfactor(
			cbind(report_mig@dc@dc_selected,
					report_mig@taxa@taxa_selected,
					report_mig@stage@stage_selected,
					as.integer(unique(strftime(as.POSIXlt(report_mig@time.sequence),"%Y"))), # une valeur bme_annee			
					rep(rownames(resum),(ncol(resum)-2)), # nb of month except columns report and label # bme_labelquantite
					stack(resum, select=c(2:(ncol(resum)-1))),# stack re-ordonne les tab de donnees !  
					as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
					get_org()
			)
	)
	colnames(t_reportmigrationmensuel_bme) <-
			c("bme_dis_identifiant","bme_tax_code","bme_std_code","bme_annee","bme_labelquantite","bme_valeur",
					"bme_mois","bme_horodateexport","bme_org_code")
	t_reportmigrationmensuel_bme$bme_mois<- as.integer(t_reportmigrationmensuel_bme$bme_mois)
	# ecriture dans la base...
	con <- new("ConnectionDB")
	con <- connect(con)
	on.exit(pool::poolClose(con@connection))
	pool::dbWriteTable(con@connection, 
			name = "temp_t_reportmigrationmensuel_bme", 
			value=t_reportmigrationmensuel_bme, 
			temporary=TRUE,
			overwrite=TRUE)	
	
	sql=paste(
			"INSERT INTO ",
			get_schema(),
			"t_bilanmigrationmensuel_bme (",			
			"bme_dis_identifiant,bme_tax_code,bme_std_code,bme_annee,bme_labelquantite,bme_valeur,
					bme_mois,bme_horodateexport,bme_org_code)",
			" SELECT * FROM temp_t_reportmigrationmensuel_bme")
	
	nline <- pool::dbExecute(con@connection, statement = sql)
	
	if (!silent) funout(gettextf("Writing monthly summary (n=%s) in the database\n", nline, domain="R-stacomiR"))
	return(invisible(NULL))
} # end function

