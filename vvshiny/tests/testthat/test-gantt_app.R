

test_that("gantt_app runs without errors", {
  df <- dplyr::tribble(
    ~OPL_Onderdeel_CROHO_examen, ~OPL_Onderdeel_CROHO_instroom, ~OPL_CBS_Label_rubriek_examen, ~OPL_CBS_Label_rubriek_instroom,
    "GEDRAG EN MAATSCHAPPIJ", "GEZONDHEIDSZORG", "sociale geografie", "(huis)arts, specialist, geneeskunde",
    "GEDRAG EN MAATSCHAPPIJ", "GEDRAG EN MAATSCHAPPIJ", "sociale geografie", "sociale geografie",
    "GEDRAG EN MAATSCHAPPIJ", "RECHT", "sociale geografie", "notariaat",
    "RECHT", "RECHT", "notariaat", "notariaat",
    "TAAL EN CULTUUR", "RECHT", "niet westerse talen en culturen", "notariaat"
  )

  df_config_gantt <- dplyr::tribble(
    ~Categorie, ~Veldnaam, ~Veldnaam_gebruiker, ~input_var, ~target_var, ~title_start, ~title_end, ~position_y_label,
    "Doorstroom vanuit B",	"OPL_Onderdeel_CROHO_examen",	"B Croho sector", "OPL_Onderdeel_CROHO_examen",	"OPL_Onderdeel_CROHO_instroom",	"Waar stromen", "Bachelor gediplomeerden naar toe?",	"right",
    "Doorstroom vanuit B",	"OPL_CBS_Label_rubriek_examen",	"B ISCED-F Rubriek", "OPL_CBS_Label_rubriek_examen",	"OPL_CBS_Label_rubriek_instroom",	"Waar stromen", "Bachelor gediplomeerden naar toe?",	"right",
    "Instroom bij M", "OPL_Onderdeel_CROHO_instroom", "M Croho sector", "OPL_Onderdeel_CROHO_instroom", "OPL_Onderdeel_CROHO_examen", "Waarvandaan stromen ", "Master studenten in?", "left",
    "Instroom bij M", "OPL_CBS_Label_rubriek_instroom", "M ISCED-F Rubriek", "OPL_CBS_Label_rubriek_instroom", "OPL_CBS_Label_rubriek_examen", "Waarvandaan stromen ", "Master studenten in?", "left"
  )


  gantt_app(df = df, df_config_gantt = df_config_gantt)

  expect_true(TRUE)
})
