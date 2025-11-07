context("stacomi base connection")


# while crashing in some test for reportFonctionnement DF or report_dc, 
#the program will set time to GMT, this will cause some errors hard to understand in some of 
# the classes (report_mig, report_mig_mult), with the following you can check this problem
# test_that("Test that the program is running under the right locale",{
# 			skip_on_cran()
# 			if(Sys.info()["sysname"] == "Linux")
# 				expect_equal(Sys.getlocale(category = "LC_TIME"),"fr_FR.UTF-8")
# 			else
# 				expect_equal(Sys.getlocale(category = "LC_TIME"),"French_France.1252")	
# 			
# 		}
# )

test_that("Test that user host and password are set for test",{
			skip_on_cran()
			env_set_test_stacomi()
			expect_true(exists("user"))
			expect_true(exists("password"))
		})


context("Database connection")

test_that("Test that stacomirtools connects",{			
			skip_on_cran()
			env_set_test_stacomi()
			envir_stacomi <- new.env(parent = asNamespace("stacomiR"))
			con <- new("ConnectionDB")			
			con@dbname <- "bd_contmig_nat_test"
			con@host <- 		"localhost"
			con@port <-		"5432"
			con@user <-  getOption("stacomiR.user", default="postgres")
			con@password <- getOption("stacomiR.password", default="postgres")	
			con <- connect(con)
			expect_is(connect(con),'ConnectionDB')
			expect_equal(con@status,"Connection OK")
			pool::poolClose(con@connection)
			rm("envir_stacomi")
		}
)


test_that("Test that positive count for nrow(ref.tr_taxon_tax)",{
			skip_on_cran()
			envir_stacomi <- new.env(parent = asNamespace("stacomiR"))
			env_set_test_stacomi()
			base <- c("bd_contmig_nat_test","localhost","5432",getOption("stacomiR.user", default="postgres"),getOption("stacomiR.password", default="postgres"))	  
			requete=new("RequeteDB")
			requete <- connect(requete,base)
			requete@sql="select count(*) from ref.tr_taxon_tax"
			requete <- stacomirtools::query(requete)
			expect_true(as.numeric(requete@query)>0)
			rm("envir_stacomi")
		})


test_that("Tests positive count for sch.t_operation_ope",{
			skip_on_cran()
			envir_stacomi <- new.env(parent = asNamespace("stacomiR"))
			env_set_test_stacomi()	
			base <- c("bd_contmig_nat_test","localhost","5432",user,password)	  
			requete=new("RequeteDB")
			requete <- connect(requete,base)
			sch <- paste("test",".",sep="")	  
			requete@sql=paste("select count(*) from ",sch,"t_operation_ope",sep="")
			requete <- stacomirtools::query(requete)
			
			expect_true(as.numeric(requete@query)>0)	
			rm("envir_stacomi")
		})

context("Loading program")


test_that("Test that working environment is created",{
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected=TRUE, sch ="test")
			expect_true(exists("envir_stacomi"))
			
		})

test_that("Test get_schema",{
			skip_on_cran()
			env_set_test_stacomi()
			stacomi(database_expected=TRUE, sch ="test")
			sch <- get_schema()
			expect_equal(sch,"test.")		
			rm("sch", envir=envir_stacomi)
			sch <- get_schema(default="volga.")
			expect_equal(sch,"volga.")
			stacomi(database_expected = TRUE,  sch = "test")
			sch <- get_schema()
			expect_equal(sch,"test.")
			org <- get_org()
			expect_equal(org,"TEST")
		})

# pour schema rlang::env_get(envir_stacomi, "sch")
context(stringr::str_c("Database integrity"))


test_that("Test that tickets have been launched",
		{
			skip_on_cran()
			envir_stacomi <- new.env(parent = asNamespace("stacomiR"))
			env_set_test_stacomi()	  
			req=new("RequeteDB")
			sch <- "test."
			req@sql <- paste("select * from ",sch," ts_maintenance_main")
			req <- stacomirtools::query(req)
			result <- req@query
			# using dput(ticket)
			tickets <- structure(list(
							main_ticket = c(59L,
									40L,
									42L, 
									67L,
									72L, 
									121L, 
									122L, 
									81L, 
									61L, 
									152L, 
									147L), main_description = c("creation de la table de maintenance", 
									"ajout des clé étrangères manquantes", 
									"modification des propriétaires sur les tables à séquence et grant select sur ref.tr_typedf_tdf oublié", 
									"org code rajouté dans les tables t_operationmarquage_omq, tj_coefficientconversion_coe,tj_prelevementlot_prl", 
									"creation d'une tableref.ts_messager_msr  pour l'internationalisation", 
									"ajout de la notion de cohorte pour les saumons passant très précocément", 
									"Mise à jour des localisations anatomiques", 
									"Mise à jour vers la version 0.4 alpha, mise à jour des référentiels du SANDRE, script ticjet81_mise_en_conformite_sandre, révision 98", 
									"Mise à jour vers la version 0.4 alpha, mise à jour des constraintes stationmesure modification limites coordonnées géographiques", 
									"Mise à jour vers la version 0.4 alpha, problèmes de clé étrangères, script total", 
									"Mise à jour vers la version 0.4 alpha, creation des masques"
							)), 
					.Names = c("main_ticket", "main_description"), 
					class = "data.frame",
					row.names = c(NA, 
							11L))
			check_exist_tickets=tickets$main_ticket%in%result$main_ticket
			for (i in 1:nrow(tickets)){
				expect_true(check_exist_tickets[i],label=paste('Missing ticket :',tickets$main_ticket[i]))
			}					
			
		})
# test on current schema
#  attention your may need superuser rights for that test 


test_that("All foreign keys are present",
		{
			skip_on_cran()
			env_set_test_stacomi()	
			skip_if_not(test_foreign_keys,"skipping foreign key test, set options in test/helper to change this")
      base <- c("bd_contmig_nat_test","localhost","5432","postgres","postgres")      
  		req <- new("RequeteDB") 
      req <- connect(req,base)
			req@sql=paste(stringr::str_c("SELECT
									distinct on (tc.constraint_name) tc.constraint_name, tc.table_name							
									FROM 
									information_schema.table_constraints AS tc 
									JOIN information_schema.key_column_usage AS kcu
									ON tc.constraint_name = kcu.constraint_name
									JOIN information_schema.constraint_column_usage AS ccu
									ON ccu.constraint_name = tc.constraint_name
									WHERE constraint_type = 'FOREIGN KEY' and  tc.constraint_schema='",paste(schema),"';"))
			req <- query(req)
      
			result <- req@query
			fk <- structure(list(constraint_name = c("c_fk_act_lot_identifiant", 
									"c_fk_act_mqe_reference", "c_fk_act_org_code", "c_fk_bjo_org_code", 
									"c_fk_bjo_std_code", "c_fk_bjo_tax_code", "c_fk_bme_std_code", 
									"c_fk_bme_tax_code", "c_fk_car_lot_identifiant", "c_fk_car_org_code", 
									"c_fk_car_par_code", "c_fk_car_val_par_identifiant", "c_fk_coe_org_code", 
									"c_fk_coe_qte_code", "c_fk_coe_std_code", "c_fk_coe_tax_code", 
									"c_fk_dft_df_identifiant", "c_fk_dft_org_code", "c_fk_dft_tdf_code", 
									"c_fk_dic_dif_identifiant", "c_fk_dic_dis_identifiant", "c_fk_dic_org_code", 
									"c_fk_dic_tdc_code", "c_fk_dif_dis_identifiant", "c_fk_dif_org_code", 
									"c_fk_dif_ouv_identifiant", "c_fk_dtx_dif_identifiant", "c_fk_dtx_org_code", 
									"c_fk_dtx_tax_code", "c_fk_env_org_code", "c_fk_env_stm_identifiant", 
									"c_fk_env_val_identifiant", "c_fk_lot_dev_code", "c_fk_lot_lot_identifiant", 
									"c_fk_lot_ope_identifiant", "c_fk_lot_org_code", "c_fk_lot_qte_code", 
									"c_fk_lot_std_code", "c_fk_lot_tax_code", "c_fk_maa_mal_id", 
									"c_fk_mac_valeurqualitatifdefaut", "c_fk_mae_mao_id", "c_fk_mae_stm_identifiant", 
									"c_fk_mal_mas_id", "c_fk_mao_mas_id", "c_fk_mqe_loc_code", "c_fk_mqe_nmq_code", 
									"c_fk_mqe_omq_reference", "c_fk_mqe_org_code", "c_fk_omq_org_code", 
									"c_fk_ope_dic_identifiant", "c_fk_ope_org_code", "c_fk_ouv_nov_code", 
									"c_fk_ouv_org_code", "c_fk_ouv_sta_code", "c_fk_pco_imp_code", 
									"c_fk_pco_loc_code", "c_fk_pco_lot_identifiant", "c_fk_pco_org_code", 
									"c_fk_pco_pat_code", "c_fk_per_dis_identifiant", "c_fk_per_org_code", 
									"c_fk_per_tar_code", "c_fk_prl_loc_code", "c_fk_prl_lot_identifiant", 
									"c_fk_prl_org_code", "c_fk_prl_pre_nom", "c_fk_prl_typeprelevement", 
									"c_fk_sta_org_code", "c_fk_std_code", "c_fk_stm_org_code", "c_fk_stm_par_code", 
									"c_fk_stm_sta_code", "c_fk_tav_dic_identifiant", "c_fk_tav_org_code", 
									"c_fk_txe_ech_code", "c_fk_txe_org_code", "c_fk_txe_sta_code", 
									"c_fk_txe_std_code", "c_fk_txe_tax_code", "c_fk_txv_org_code", 
									"c_fk_txv_std_code", "c_fk_txv_tax_code"), table_name = c("tj_actionmarquage_act", 
									"tj_actionmarquage_act", "tj_actionmarquage_act", "t_bilanmigrationjournalier_bjo", 
									"t_bilanmigrationjournalier_bjo", "t_bilanmigrationjournalier_bjo", 
									"t_reportmigrationmensuel_bme", "t_reportmigrationmensuel_bme", 
									"tj_caracteristiquelot_car", "tj_caracteristiquelot_car", "tj_caracteristiquelot_car", 
									"tj_caracteristiquelot_car", "tj_coefficientconversion_coe", 
									"tj_coefficientconversion_coe", "tj_coefficientconversion_coe", 
									"tj_coefficientconversion_coe", "tj_dfesttype_dft", "tj_dfesttype_dft", 
									"tj_dfesttype_dft", "t_dispositifcomptage_dic", "t_dispositifcomptage_dic", 
									"t_dispositifcomptage_dic", "t_dispositifcomptage_dic", "t_dispositiffranchissement_dif", 
									"t_dispositiffranchissement_dif", "t_dispositiffranchissement_dif", 
									"tj_dfestdestinea_dtx", "tj_dfestdestinea_dtx", "tj_dfestdestinea_dtx", 
									"tj_conditionenvironnementale_env", "tj_conditionenvironnementale_env", 
									"tj_conditionenvironnementale_env", "t_lot_lot", "t_lot_lot", 
									"t_lot_lot", "t_lot_lot", "t_lot_lot", "t_lot_lot", "t_lot_lot", 
									"ts_masqueordreaffichage_maa", "ts_masquecaracteristiquelot_mac", 
									"ts_masqueconditionsenvironnementales_mae", "ts_masqueconditionsenvironnementales_mae", 
									"ts_masquelot_mal", "ts_masqueope_mao", "t_marque_mqe", "t_marque_mqe", 
									"t_marque_mqe", "t_marque_mqe", "t_operationmarquage_omq", "t_operation_ope", 
									"t_operation_ope", "t_ouvrage_ouv", "t_ouvrage_ouv", "t_ouvrage_ouv", 
									"tj_pathologieconstatee_pco", "tj_pathologieconstatee_pco", "tj_pathologieconstatee_pco", 
									"tj_pathologieconstatee_pco", "tj_pathologieconstatee_pco", "t_periodefonctdispositif_per", 
									"t_periodefonctdispositif_per", "t_periodefonctdispositif_per", 
									"tj_prelevementlot_prl", "tj_prelevementlot_prl", "tj_prelevementlot_prl", 
									"tj_prelevementlot_prl", "tj_prelevementlot_prl", "t_station_sta", 
									"ts_taxavideo_txv", "tj_stationmesure_stm", "tj_stationmesure_stm", 
									"tj_stationmesure_stm", "ts_taillevideo_tav", "ts_taillevideo_tav", 
									"tj_tauxechappement_txe", "tj_tauxechappement_txe", "tj_tauxechappement_txe", 
									"tj_tauxechappement_txe", "tj_tauxechappement_txe", "ts_taxavideo_txv", 
									"ts_taxavideo_txv", "ts_taxavideo_txv")), .Names = c("constraint_name", 
							"table_name"), row.names = c(NA, 83L), class = "data.frame")
			check_exist_fk=fk$constraint_name%in%result$constraint_name
			for (i in 1:nrow(fk)){
				expect_true(check_exist_fk[i],label=paste("Missing foreign key :",fk$constraint_name[i],"table :",fk$table_name[i]))
			}
			rm(list=ls(all=TRUE))	
		})
