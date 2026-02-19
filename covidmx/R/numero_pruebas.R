#' Numero de Pruebas
#'
#' @description
#' `numero_pruebas` Calcula el numero total de pruebas por fecha agrupando (o sin hacerlo)
#' por covariables. Por default calcula la el numero de pruebas de antigeno y PCR por separado
#' para cada estado.
#'
#' @details
#' Las pruebas de PCR (polymerase chain reaction) identifican material genetico de un organismo
#' (por ejemplo un virus como el COVID-19 o la influenza). Las pruebas de antigeno
#' (o pruebas rapidas) detectan algunas proteinas que conforman el virus.
#'
#' Para mas informacion sobre las pruebas y su interpretacion puedes consultar
#' [las guias del CDC](https://espanol.cdc.gov/coronavirus/2019-ncov/symptoms-testing/testing.html)
#'
#' @inheritParams casos
#'
#' @param tipo_prueba (**opcional**) Vector con el tipo de pruebas a incluir `Antigeno`, `PCR`. Por
#' default se incluyen ambas.
#'
#' @param group_by_tipo_prueba (**opcional**) Booleana determinando si regresa la base
#' con cada entrada agrupada por `tipo_prueba`. En caso `TRUE` (cada fecha
#' y entidad reporta separado el los casos de PCR y Antigeno). En caso `FALSE` se juntan
#' los casos de PCR y Antigeno para devolver un unico numero por fecha.
#'
#' @importFrom rlang :=
#'
#' @return Adiciona a la lista de `datos_covid` una nueva entrada de nombre `list_name`
#' (default: `numero_pruebas`) con una base de datos (`tibble` o `duckdb`) con los
#' resultados agregados.
#' \itemize{
#'   \item numero_pruebas - Base de datos generara con los datos agregados (el nombre cambia si
#'   se usa `list_name`).
#'   \item dict - Diccionario de datos
#'   \item dats - Datos originales (conexion a `duckdb` o `tibble`)
#'   \item disconnect  - Función para desconectarte de `duckdb`
#'   \item ... - Cualquier otro elemento que ya existiera en `datos_covid`
#' }
#'
#' @examples
#'
#' # Para el ejemplo usaremos los datos precargados (datosabiertos) pero tu puedes
#' # correr el ejemplo descargando informacion mas reciente.
#' datos_covid <- datosabiertos
#'
#' # Número de pruebas PCR/Antigeno a nivel nacional por estado
#' datos_covid <- datos_covid |> numero_pruebas()
#' head(datos_covid$numero_pruebas)
#'
#' # Número de pruebas nacionales pero sin separar por tipo ni estado
#' datos_covid <- datos_covid |>
#'   numero_pruebas(
#'     group_by_entidad = FALSE, group_by_tipo_prueba = FALSE,
#'     list_name = "Todas_las_pruebas"
#'   )
#' head(datos_covid$Todas_las_pruebas)
#'
#' # Positivos en Baja California Sur
#' datos_covid <- datos_covid |>
#'   numero_pruebas(
#'     entidades = c("BAJA CALIFORNIA SUR"),
#'     list_name = "BCS"
#'   )
#' head(datos_covid$BCS)
#'
#' # Si deseas agrupar por una variable que no este en las opciones asi como tipo paciente
#' datos_covid <- datos_covid |>
#'   numero_pruebas(
#'     tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
#'     group_by_tipo_paciente = TRUE,
#'     .grouping_vars = c("DIABETES"),
#'     list_name = "pruebas_diabetes"
#'   )
#' head(datos_covid$pruebas_diabetes)
#'
#' # Una vez hayas concluido tu trabajo no olvides desconectar
#' datos_covid$disconnect()
#' @export

numero_pruebas <- function(datos_covid,
                           entidades = c(
                             "AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR",
                             "CAMPECHE", "CHIAPAS", "CHIHUAHUA",
                             "CIUDAD DE M\u00c9XICO", "COAHUILA DE ZARAGOZA", "COLIMA",
                             "DURANGO", "GUANAJUATO", "GUERRERO", "HIDALGO",
                             "JALISCO", "M\u00c9XICO", "MICHOAC\u00c1N DE OCAMPO",
                             "MORELOS", "NAYARIT", "NUEVO LE\u00d3N", "OAXACA",
                             "PUEBLA", "QUER\u00c9TARO", "QUINTANA ROO",
                             "SAN LUIS POTOS\u00cd", "SINALOA", "SONORA",
                             "TABASCO", "TAMAULIPAS", "TLAXCALA",
                             "VERACRUZ DE IGNACIO DE LA LLAVE",
                             "YUCAT\u00c1N", "ZACATECAS"
                           ),
                           group_by_entidad = TRUE,
                           entidad_tipo = c(
                             "Unidad Medica", "Residencia",
                             "Nacimiento"
                           ),
                           fecha_tipo = c(
                             "Sintomas", "Ingreso",
                             "Defuncion"
                           ),
                           tipo_prueba = c("Antigeno", "PCR"),
                           group_by_tipo_prueba = TRUE,
                           tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO", "NO ESPECIFICADO"),
                           group_by_tipo_paciente = FALSE,
                           tipo_uci = c("SI", "NO", "NO APLICA", "SE IGNORA", "NO ESPECIFICADO"),
                           group_by_tipo_uci = FALSE,
                           tipo_sector = c(
                             "CRUZ ROJA", "DIF", "ESTATAL", "IMSS",
                             "IMSS-BIENESTAR", "ISSSTE", "MUNICIPAL",
                             "PEMEX", "PRIVADA", "SEDENA", "SEMAR", "SSA",
                             "UNIVERSITARIO", "NO ESPECIFICADO"
                           ),
                           group_by_tipo_sector = FALSE,
                           defunciones = FALSE,
                           edad_cut = NULL,
                           as_tibble = TRUE,
                           fill_zeros = as_tibble,
                           list_name = "numero_pruebas",
                           .grouping_vars = c()) {


  # Chequeo de si existe elemento en la lista y duplicacion
  k <- 0
  in_list <- TRUE
  baselistname <- list_name
  while (in_list) {
    if (any(stringr::str_detect(names(datos_covid), list_name))) {
      k <- k + 1
      list_name <- paste0(baselistname, "_", as.character(k))
    } else {
      in_list <- FALSE
      if (k > 0) {
        cli::cli_alert_warning(
          c(
            "Se guardo el elemento bajo el nombre de {list_name} pues {baselistname} ya existe.",
            " Utiliza {.code list_name = 'nuevo_nombre'} para nombrar a los elementos y evitar",
            " este problema."
          )
        )
      }
    }
  }

  # Entidades en mayuscula
  entidades <- toupper(entidades)

  #> ENTIDAD----
  # Seleccionar la entidad
  entidad_tipo <- dplyr::case_when(
    stringr::str_detect(tolower(entidad_tipo[1]), "m.*dica|entidad_um") ~ "ENTIDAD_UM",
    stringr::str_detect(tolower(entidad_tipo[1]), "residencia|entidad_res") ~ "ENTIDAD_RES",
    stringr::str_detect(tolower(entidad_tipo[1]), "nacimiento|entidad_nac") ~ "ENTIDAD_NAC",
  )

  # Seleccionar la entidad
  fecha_tipo <-
    dplyr::case_when(
      stringr::str_detect(tolower(fecha_tipo[1]), "ingreso") ~ "FECHA_INGRESO",
      stringr::str_detect(tolower(fecha_tipo[1]), "s.*ntomas") ~ "FECHA_SINTOMAS",
      stringr::str_detect(tolower(fecha_tipo[1]), "defunci.*n|fecha_def") ~ "FECHA_DEF"
    )

  # Checamos la variable de entidades
  if (any(stringr::str_detect(entidades, "CIUDAD DE MEXICO|CDMX"))) {
    entidades[stringr::str_detect(entidades, "CIUDAD DE MEXICO")] <-
      "CIUDAD DE M\u00c9XICO"
  }

  if (any(stringr::str_detect(entidades, "COAHUILA"))) {
    entidades[stringr::str_detect(entidades, "COAHUILA")] <-
      "COAHUILA DE ZARAGOZA"
  }

  if (any(stringr::str_detect(entidades, "\\bMEXICO\\b|EDOMEX"))) {
    entidades[stringr::str_detect(entidades, "\\bMEXICO\\b|EDOMEX")] <-
      "M\u00c9XICO"
  }

  if (any(stringr::str_detect(entidades, "MICHOAC\u00c1N|MICHOACAN"))) {
    entidades[stringr::str_detect(entidades, "MICHOAC\u00c1N|MICHOACAN")] <-
      "MICHOAC\u00c1N DE OCAMPO"
  }

  if (any(stringr::str_detect(entidades, "NUEVO LEON"))) {
    entidades[stringr::str_detect(entidades, "NUEVO LEON")] <-
      "NUEVO LE\u00d3N"
  }

  if (any(stringr::str_detect(entidades, "QUERETARO"))) {
    entidades[stringr::str_detect(entidades, "QUERETARO")] <-
      "QUER\u00c9TARO"
  }

  if (any(stringr::str_detect(entidades, "SAN LUIS POTOSI"))) {
    entidades[stringr::str_detect(entidades, "SAN LUIS POTOSI")] <-
      "SAN LUIS POTOS\u00cd"
  }

  if (any(stringr::str_detect(entidades, "VERACRUZ"))) {
    entidades[stringr::str_detect(entidades, "VERACRUZ")] <-
      "VERACRUZ DE IGNACIO DE LA LLAVE"
  }

  if (any(stringr::str_detect(entidades, "YUCATAN|YUC"))) {
    entidades[stringr::str_detect(entidades, "YUCATAN")] <-
      "YUCAT\u00c1N"
  }

  # Filtramos por entidad
  entidades <-
    datos_covid$dict[entidad_tipo][[1]] |>
    dplyr::filter(
      stringr::str_detect(
        get("ENTIDAD_FEDERATIVA"),
        paste0(paste0("^", paste0(entidades, "$")), collapse = "|")
      )
    )

  if (nrow(entidades) < 1) {
    cli::cli_abort("No logramos encontrar esas entidades")
  }

  lista_entidades <- paste0(entidades$CLAVE_ENTIDAD, collapse = "|")
  .num_pruebas <- datos_covid$dats |>
    dplyr::filter(
      stringr::str_detect(!!as.symbol(entidad_tipo), lista_entidades)
    )

  #> TIPO DE PACIENTE----
  # Filtramos por tipo de paciente
  pacientes <-
    datos_covid$dict["PACIENTE"][[1]] |>
    dplyr::filter(
      stringr::str_detect(
        get(stringr::str_subset(colnames(datos_covid$dict["PACIENTE"][[1]]), "DESC*")[1]),
        paste0(
          "\\b",
          paste0(tipo_paciente, collapse = "\\b|\\b"), "\\b"
        )
      )
    )

  lista_claves <- as.numeric(pacientes$CLAVE)
  .num_pruebas <- .num_pruebas |>
    dplyr::filter(!!as.symbol("TIPO_PACIENTE") %in% lista_claves)

  #> TIPO DE UCI----
  # Filtramos por tipo de uci
  ucis <-
    datos_covid$dict["UCI"][[1]] |>
    dplyr::filter(
      stringr::str_detect(
        get(stringr::str_subset(colnames(datos_covid$dict["UCI"][[1]]), "DESC*")),
        paste0(paste0("^", tipo_uci, "$"), collapse = "|")
      )
    )

  lista_claves <- as.numeric(ucis$CLAVE)
  .num_pruebas <- .num_pruebas |>
    dplyr::filter(!!as.symbol("UCI") %in% lista_claves)

  #> TIPO DE SECTOR----
  # Filtramos por tipo de uci
  sectores <-
    datos_covid$dict["SECTOR"][[1]] |>
    dplyr::filter(
      stringr::str_detect(
        get(stringr::str_subset(colnames(datos_covid$dict["SECTOR"][[1]]), "DESC*")),
        paste0(paste0("^", tipo_sector, "$"), collapse = "|")
      )
    )

  lista_claves <- as.numeric(sectores$CLAVE)
  .num_pruebas <- .num_pruebas |>
    dplyr::filter(!!as.symbol("SECTOR") %in% lista_claves)

  #> DEFUNCIONES
  if (defunciones) {
    .num_pruebas <- .num_pruebas |>
      dplyr::filter(!!as.symbol("FECHA_DEF") >= as.POSIXct("2000/01/01"))
  }

  #> EDADES
  if (!is.null(edad_cut)) {
    .num_pruebas <- .num_pruebas |>
      dplyr::mutate(!!as.symbol("EDAD_CAT") := cut(!!as.symbol("EDAD"),
        breaks = edad_cut,
        include.lowest = TRUE
      )) |>
      dplyr::filter(!is.na(!!as.symbol("EDAD_CAT")))
  }

  # Cortamos la base en tipos de pruebas
  is_pcr  <- any(stringr::str_detect(tolower(tipo_prueba), "pcr"))
  is_anti <- any(stringr::str_detect(tolower(tipo_prueba), "ant.*geno"))
  if (is_pcr) {
    .pcr <- .num_pruebas |>
      dplyr::filter(!!as.symbol("TOMA_MUESTRA_LAB") == 1) |>
      dplyr::mutate(!!as.symbol("TIPO_PRUEBA") :=
        dplyr::if_else(!!as.symbol("TOMA_MUESTRA_LAB") == 1, "PCR", NA_character_))
  }

  if (is_anti) {
    .antigeno <- .num_pruebas |>
      dplyr::filter(!!as.symbol("TOMA_MUESTRA_ANTIGENO") == 1) |>
      dplyr::mutate(!!as.symbol("TIPO_PRUEBA") :=
        dplyr::if_else(!!as.symbol("TOMA_MUESTRA_ANTIGENO") == 1, "ANTIGENO", NA_character_))
  }

  if (is_pcr & is_anti) {
    .num_pruebas <- .pcr |>
      dplyr::union_all(.antigeno)
  } else if (is_pcr & !is_anti) {
    .num_pruebas <- .pcr
  } else if (is_anti & !is_pcr) {
    .num_pruebas <- .antigeno
  } else {
    cli::cli_abort("Selecciona PCR y/o Antigeno en {.code tipo_prueba}")
  }

  #> AGRUPACI\u00d3N
  .num_pruebas <- .num_pruebas |>
    dplyr::group_by_at(fecha_tipo)

  if (length(.grouping_vars) > 0) {
    for (var in .grouping_vars) {
      .num_pruebas <- .num_pruebas |>
        dplyr::group_by_at(var, .add = TRUE)
    }
  }

  #> AGRUPACI\u00d3N EDAD
  if (!is.null(edad_cut)) {
    .num_pruebas <- .num_pruebas |>
      dplyr::group_by_at("EDAD_CAT", .add = TRUE)
  }

  # Tomamos el grupo
  if (group_by_entidad) {
    .num_pruebas <- .num_pruebas |>
      dplyr::group_by_at(entidad_tipo, .add = TRUE)
  }

  # Tomamos el grupo
  if (group_by_tipo_prueba) {
    .num_pruebas <- .num_pruebas |>
      dplyr::group_by_at("TIPO_PRUEBA", .add = TRUE)
  }

  if (group_by_tipo_paciente) {
    .num_pruebas <- .num_pruebas |>
      dplyr::group_by_at("TIPO_PACIENTE", .add = TRUE)
  }

  if (group_by_tipo_sector) {
    .num_pruebas <- .num_pruebas |>
      dplyr::group_by_at("SECTOR", .add = TRUE)
  }

  if (group_by_tipo_uci) {
    .num_pruebas <- .num_pruebas |>
      dplyr::group_by_at("UCI", .add = TRUE)
  }

  # Conteo de los .casos
  .num_pruebas <- .num_pruebas |>
    dplyr::tally() |>
    dplyr::ungroup()

  if (as_tibble) {
    .num_pruebas <- .num_pruebas |>
      dplyr::collect()

    if (fill_zeros) {

      # Select the other variables to expand grid
      .grouping_vars <- .num_pruebas |>
        dplyr::select(-dplyr::matches("\\bn\\b")) |>
        dplyr::select(-dplyr::starts_with("FECHA")) |>
        dplyr::distinct()

      # Check the dates to expand
      .fechasminmax <- datos_covid$dats |>
        dplyr::select_at(fecha_tipo) |>
        dplyr::summarise(
          fechamin = min(!!as.symbol(fecha_tipo), na.rm = TRUE),
          fechamax = max(!!as.symbol(fecha_tipo), na.rm = TRUE)
        ) |>
        dplyr::collect()

      .datesq <- seq(.fechasminmax$fechamin[1], .fechasminmax$fechamax[1], by = "1 day")

      # Create grid of all possible dates
      .grid_casos <- tidyr::expand_grid(!!as.symbol(fecha_tipo) := .datesq, .grouping_vars)

      # Full join
      .num_pruebas <- .num_pruebas |>
        dplyr::full_join(.grid_casos, by = colnames(.grid_casos)) |>
        dplyr::mutate(!!as.symbol("n") := tidyr::replace_na(!!as.symbol("n"), 0))
    }
  }

  if (nrow(entidades) > 0 & group_by_entidad) {
    name_join <- c("CLAVE_ENTIDAD")
    names(name_join) <- entidad_tipo
    .num_pruebas <- .num_pruebas |>
      dplyr::left_join(datos_covid$dict[entidad_tipo][[1]], by = name_join)
  }


  if (nrow(pacientes) > 0 & group_by_tipo_paciente) {
    name_join <- c("CLAVE")
    names(name_join) <- "TIPO_PACIENTE"
    paciente_df <- datos_covid$dict["PACIENTE"][[1]]
    colnames(paciente_df) <- c("CLAVE", "DESCRIPCION_TIPO_PACIENTE")
    .num_pruebas <- .num_pruebas |>
      dplyr::left_join(paciente_df, by = name_join)
  }

  if (nrow(ucis) > 0 & group_by_tipo_uci) {
    name_join <- c("CLAVE")
    names(name_join) <- "UCI"
    uci_df <- datos_covid$dict["UCI"][[1]]
    colnames(uci_df) <- c("CLAVE", "DESCRIPCION_TIPO_UCI")
    .num_pruebas <- .num_pruebas |>
      dplyr::left_join(uci_df, by = name_join)
  }

  if (nrow(sectores) > 0 & group_by_tipo_sector) {
    name_join <- c("CLAVE")
    names(name_join) <- "SECTOR"
    sector_df <- datos_covid$dict["SECTOR"][[1]]
    colnames(sector_df) <- c("CLAVE", "DESCRIPCION_TIPO_SECTOR")
    .num_pruebas <- .num_pruebas |>
      dplyr::left_join(sector_df, by = name_join)
  }

  .num_pruebas <- list(.num_pruebas)
  names(.num_pruebas) <- list_name

  return(append(datos_covid, .num_pruebas))
}
