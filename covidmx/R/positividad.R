#' Positividad
#'
#' @description
#' `positividad` Calcula la positividad  por fecha agrupando (o sin hacerlo)
#' por covariables. Por default calcula la positividad de las pruebas
#' haciendo Antigeno y PCR por separado, cada una por fecha y entidad.
#'
#' @details La positividad se define como
#' \deqn{\frac{\# Pruebas positivas}{Total de pruebas}}{%
#' \# Pruebas positivas / Total de pruebas}
#' Si se utiliza la opción `remove_inconclusive = TRUE` el **Total de pruebas** se calcula
#' utilizando solo `POSITIVOS + NEGATIVOS`. Si `remove_inconclusive = FALSE` se calcula
#' utilizando todas las personas que tuvieron prueba:
#' `POSITIVOS + NEGATIVOS + INCONCLUSOS + SIN RESULTADO`.
#'
#' Si no se realizaron pruebas un dia la positividad no esta definida pues el **Total de pruebas**
#' es cero. En ese caso si `fill_NA = TRUE` se devuelven las entradas de esos dias pero
#' con valor `NA`.
#'
#' @inheritParams numero_pruebas
#'
#' @param remove_inconclusive (**opcional**)  Si `TRUE` no considera en el denominador de la
#' positividad las pruebas cuyo resultado es inconcluso o aún no ha sido otorgado. Si `FALSE`
#' considera a todos. Por default es `TRUE`.
#'
#' @param fill_NA (**opcional**)  Regresa observaciones para todas las combinaciones de variables
#' incluyendo como `NA` donde no se observaron casos en el denominador.  En caso contrario no se
#' incluyen las filas donde no se observaron casos.
#'
#' @importFrom rlang :=
#'
#' @return Une a la lista de `datos_covid` una nueva entrada de nombre `list_name`
#' (default: `positividad`) con una base de datos (`tibble`) con los
#' resultados agregados.
#' \itemize{
#'   \item positividad - Base de datos generara con los datos agregados (el nombre cambia si
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
#' # Casos a nivel nacional por estado por tipo de prueba
#' datos_covid <- datos_covid |> positividad()
#' head(datos_covid$positividad)
#'
#' \donttest{
#' # Total nacional sumando todas las pruebas del pais
#' datos_covid <- datos_covid |>
#'   positividad(group_by_entidad = FALSE, list_name = "positividad_nacional")
#' head(datos_covid$positividad_nacional)
#'
#' # Positivos en Baja California y Baja California Sur
#' datos_covid <- datos_covid |>
#'   positividad(
#'     entidades = c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR"),
#'     list_name = "positividad_californiana"
#'   )
#' head(datos_covid$positividad_californiana)
#'
#' # Agrupando ambas pruebas en una sola positividad global
#' datos_covid <- datos_covid |>
#'   positividad(
#'     entidades = c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR"),
#'     group_by_tipo_prueba = FALSE,
#'     list_name = "positividad_californiana_2"
#'   )
#' head(datos_covid$positividad_californiana_2)
#'
#' # Regresa la suma de ambos estados pero dividiendo por tipo de paciente
#' datos_covid <- datos_covid |>
#'   positividad(
#'     entidades = c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR"),
#'     group_by_entidad = FALSE,
#'     tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
#'     group_by_tipo_paciente = TRUE,
#'     list_name = "positividad_paciente"
#'   )
#' head(datos_covid$positividad_paciente)
#'
#' # Si deseas agrupar por una variable que no este en las opciones va en .grouping_vars
#' datos_covid <- datos_covid |>
#'   positividad(
#'     tipo_sector = "IMSS",
#'     .grouping_vars = c("SEXO"),
#'     list_name = "positividad_imss_sexo"
#'   )
#' head(datos_covid$positividad_imss_sexo)
#' }
#' 
#' # Una vez hayas concluido tu trabajo no olvides desconectar
#' datos_covid$disconnect()
#' @references
#'
#' Furuse, Y., Ko, Y. K., Ninomiya, K., Suzuki, M., & Oshitani, H. (2021). Relationship of test
#' positivity rates with COVID-19 epidemic dynamics. International journal of environmental
#' research and public health, 18(9), 4655.
#'
#' Al Dallal, A., AlDallal, U., & Al Dallal, J. (2021). Positivity rate: an indicator for the
#' spread of COVID-19. Current Medical Research and Opinion, 37(12), 2067-2076.
#'
#' @seealso [descarga_datos_abiertos()] [numero_pruebas()] [cfr()] [chr()] [estima_rt()] [casos()]
#' @export

positividad <- function(datos_covid,
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
                        fill_NA = TRUE,
                        list_name = "positividad",
                        remove_inconclusive = TRUE,
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

  # Calculamos el total de pruebas
  is_pcr <- any(stringr::str_detect(tolower(tipo_prueba), "pcr"))
  if (is_pcr) {
    .grouping_vars <- c(.grouping_vars, "RESULTADO_LAB")
  }

  is_anti <- any(stringr::str_detect(tolower(tipo_prueba), "ant.*geno"))
  if (is_anti) {
    .grouping_vars <- c(.grouping_vars, "RESULTADO_ANTIGENO")
  }


  .numero_pruebas <- numero_pruebas(
    datos_covid = datos_covid, entidades = entidades,
    group_by_entidad = group_by_entidad,
    entidad_tipo = entidad_tipo,
    fecha_tipo = fecha_tipo,
    tipo_prueba = tipo_prueba,
    group_by_tipo_prueba = group_by_tipo_prueba,
    tipo_paciente = tipo_paciente,
    group_by_tipo_paciente = group_by_tipo_paciente,
    tipo_uci = tipo_uci,
    group_by_tipo_uci = group_by_tipo_uci,
    tipo_sector = tipo_sector,
    group_by_tipo_sector = group_by_tipo_sector,
    defunciones = defunciones,
    edad_cut = edad_cut,
    as_tibble = TRUE,
    fill_zeros = fill_NA,
    list_name = list_name,
    .grouping_vars = .grouping_vars
  )

  if (is_pcr & group_by_tipo_prueba) {


    # Filtramos los totales
    .pcr_totales <- .numero_pruebas[list_name][[1]] |>
      dplyr::filter(!!as.symbol("TIPO_PRUEBA") == "PCR")

    if (remove_inconclusive) {
      .pcr_totales <- .pcr_totales |>
        dplyr::filter(!!as.symbol("RESULTADO_LAB") %in% c(1, 2)) |>
        dplyr::select(-dplyr::matches("RESULTADO_ANTIGENO|RESULTADO_LAB"))
    }

    # Filtramos los positivos
    .pcr_positivos <- .numero_pruebas[list_name][[1]] |>
      dplyr::filter(!!as.symbol("TIPO_PRUEBA") == "PCR" & !!as.symbol("RESULTADO_LAB") == 1) |>
      dplyr::select(-dplyr::matches("RESULTADO_ANTIGENO|RESULTADO_LAB"))

    # Agrupamos por covariables y contamos
    groups <- stringr::str_subset(colnames(.pcr_positivos),
      "\\bn\\b",
      negate = T
    )

    # Obtenemos los positivos
    .pcr_positivos <- .pcr_positivos |>
      dplyr::group_by_at(groups) |>
      dplyr::summarise(!!as.symbol("n_positivos") := sum(!!as.symbol("n")), .groups = "drop")

    # Obtenemos los totales
    .pcr_totales <- .pcr_totales |>
      dplyr::group_by_at(groups) |>
      dplyr::summarise(!!as.symbol("n_pruebas") := sum(!!as.symbol("n")), .groups = "drop")

    .pcr <- .pcr_totales |>
      dplyr::left_join(.pcr_positivos, by = groups) |>
      dplyr::mutate(!!as.symbol("Positividad") := dplyr::if_else(
        !!as.symbol("n_pruebas") != 0,
        as.numeric(!!as.symbol("n_positivos")) / as.numeric(!!as.symbol("n_pruebas")), NA_real_
      ))
  }

  if (is_anti & group_by_tipo_prueba) {


    # Filtramos los totales
    .anti_totales <- .numero_pruebas[list_name][[1]] |>
      dplyr::filter(!!as.symbol("TIPO_PRUEBA") == "ANTIGENO")

    if (remove_inconclusive) {
      .anti_totales <- .anti_totales |>
        dplyr::filter(!!as.symbol("RESULTADO_ANTIGENO") %in% c(1, 2)) |>
        dplyr::select(-dplyr::matches("RESULTADO_ANTIGENO|RESULTADO_LAB"))
    }

    # Filtramos los positivos
    .anti_positivos <- .numero_pruebas[list_name][[1]] |>
      dplyr::filter(!!as.symbol("TIPO_PRUEBA") == "ANTIGENO" & !!as.symbol("RESULTADO_ANTIGENO") == 1) |>
      dplyr::select(-dplyr::matches("RESULTADO_ANTIGENO|RESULTADO_LAB"))

    # Agrupamos por covariables y contamos
    groups <- stringr::str_subset(colnames(.anti_positivos),
      "\\bn\\b",
      negate = T
    )

    # Obtenemos los positivos
    .anti_positivos <- .anti_positivos |>
      dplyr::group_by_at(groups) |>
      dplyr::summarise(!!as.symbol("n_positivos") := sum(!!as.symbol("n")), .groups = "drop")

    # Obtenemos los totales
    .anti_totales <- .anti_totales |>
      dplyr::group_by_at(groups) |>
      dplyr::summarise(!!as.symbol("n_pruebas") := sum(!!as.symbol("n")), .groups = "drop")

    .anti <- .anti_totales |>
      dplyr::left_join(.anti_positivos, by = groups) |>
      dplyr::mutate(!!as.symbol("Positividad") := dplyr::if_else(
        !!as.symbol("n_pruebas") != 0,
        as.numeric(!!as.symbol("n_positivos")) / as.numeric(!!as.symbol("n_pruebas")), NA_real_
      ))
  }

  # Add both tests
  if (!group_by_tipo_prueba) {
    groups <- stringr::str_subset(colnames(.numero_pruebas[list_name][[1]]),
      "\\bn\\b|RESULTADO_LAB|RESULTADO_ANTIGENO",
      negate = T
    )

    # Obtenemos los totales
    .totales <- .numero_pruebas[list_name][[1]] |>
      dplyr::group_by_at(groups) |>
      dplyr::summarise(!!as.symbol("n_pruebas") := sum(!!as.symbol("n")), .groups = "drop")

    # Obtenemos los positivos
    .positivos <- .numero_pruebas[list_name][[1]] |>
      dplyr::filter(dplyr::if_any(dplyr::starts_with("RESULTADO"), ~ (. == 1))) |>
      dplyr::group_by_at(groups) |>
      dplyr::summarise(!!as.symbol("n_positivos") := sum(!!as.symbol("n")), .groups = "drop")

    .positividad <- .totales |>
      dplyr::left_join(.positivos, by = groups) |>
      dplyr::mutate(!!as.symbol("Positividad") := dplyr::if_else(
        !!as.symbol("n_pruebas") != 0,
        as.numeric(!!as.symbol("n_positivos")) / as.numeric(!!as.symbol("n_pruebas")), NA_real_
      ))
  }


  if (is_pcr & is_anti & group_by_tipo_prueba) {
    .positividad <- .pcr |>
      dplyr::bind_rows(.anti)
  } else if (is_pcr & !is_anti & group_by_tipo_prueba) {
    .positividad <- .pcr
  } else if (!is_pcr & is_anti & group_by_tipo_prueba) {
    .positividad <- .anti
  } else if (!group_by_tipo_prueba) {
    .positividad <- .positividad
  } else {
    cli::cli_abort("Selecciona PCR o Antigeno en pruebas")
  }

  .positividad <- .positividad |>
    dplyr::relocate(!!as.symbol("Positividad"))


  .positividad <- list(.positividad)
  names(.positividad) <- list_name

  return(append(datos_covid, .positividad))
}
