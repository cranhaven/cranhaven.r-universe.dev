#' Case Fatality Rate (CFR)
#'
#' @description Calcula la proporcion de enfermos que fallecen sobre todos los enfermos confirmados
#' en distintas categorias (residencia / edad / etc)
#'
#' @inheritParams casos
#' @inheritParams positividad
#'
#' @importFrom rlang :=
#'
#' @details El case fatality rate se define como
#'
#' \deqn{\frac{\# Defunciones}{Total de enfermos}}{%
#' \# Defunciones / Total de enfermos}
#'
#' Si se utiliza la opción `tipo_clasificacion` se puede cambiar la definicion de enfermo
#' (por default se incluyen solamente `"Confirmados COVID"`).
#'
#' @return Une a la lista de `datos_covid` una nueva entrada de nombre `list_name`
#' (default: `case fatality rate`) con una base de datos (`tibble` o `duckdb`) con los
#' resultados agregados.
#'
#' \itemize{
#'   \item `case fatality rate` - Base de datos generara con los datos agregados (el nombre cambia
#'   si se usa `list_name`).
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
#' # Casos a nivel nacional por entidad
#' datos_covid <- datos_covid |> cfr()
#' head(datos_covid$`case fatality rate`)
#' \donttest{
#' # Agregando todos los estados
#' datos_covid <- datos_covid |>
#'   cfr(list_name = "cfr_nacional", group_by_entidad = FALSE)
#' head(datos_covid$`cfr_nacional`)
#'
#' # CFR en Baja California
#' datos_covid <- datos_covid |>
#'   cfr(entidades = c("BAJA CALIFORNIA"), list_name = "cfr_bc")
#' head(datos_covid$`cfr_bc`)
#'
#' # Calcula el CFR suponiendo toda la base son confirmados
#' datos_covid <- datos_covid |>
#'   cfr(
#'     entidades = c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR"),
#'     tipo_clasificacion = c(
#'       "Sospechosos", "Confirmados COVID",
#'       "Negativo a COVID", "Inv\u00e1lido", "No realizado"
#'     ),
#'     group_by_tipo_clasificacion = TRUE, list_name = "bc_bcs_cfr"
#'   )
#' head(datos_covid$`bc_bcs_cfr`) # Los NA es porque no habia observaciones en el denominador
#'
#' # Distinguiendo entre ambulatorio y hospitalizado
#' datos_covid <- datos_covid |>
#'   cfr(
#'     tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
#'     group_by_tipo_paciente = TRUE,
#'     list_name = "cfr_paciente"
#'   )
#' head(datos_covid$cfr_paciente)
#'
#' # CFR en distintos grupos de edad (0 a 20, 20 a 60 y 60+)
#' datos_covid <- datos_covid |>
#'   cfr(edad_cut = c(0, 20, 60, Inf), list_name = "cfr_edad")
#' head(datos_covid$cfr_edad)
#'
#' # Si deseas agrupar por una variable que no este en las opciones
#' datos_covid <- datos_covid |>
#'   cfr(.grouping_vars = c("DIABETES"), list_name = "cfr_diab")
#' head(datos_covid$cfr_diab)
#' }
#' # Finalmente desconectamos
#' datos_covid$disconnect()
#'
#' @seealso [descarga_datos_abiertos()] [numero_pruebas()] [chr()] [estima_rt()]
#' [positividad()] [casos()]
#'
#' @export

cfr <- function(datos_covid,
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
                tipo_uci = c("SI", "NO", "NO APLICA", "SE IGNORA", "NO ESPECIFICADO"),
                group_by_tipo_uci = FALSE,
                tipo_clasificacion = c("Confirmados COVID"),
                group_by_tipo_clasificacion = FALSE,
                tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO", "NO ESPECIFICADO"),
                group_by_tipo_paciente = FALSE,
                tipo_sector = c(
                  "CRUZ ROJA", "DIF", "ESTATAL", "IMSS",
                  "IMSS-BIENESTAR", "ISSSTE", "MUNICIPAL",
                  "PEMEX", "PRIVADA", "SEDENA", "SEMAR", "SSA",
                  "UNIVERSITARIO", "NO ESPECIFICADO"
                ),
                group_by_tipo_sector = FALSE,
                edad_cut = NULL,
                fill_NA = TRUE,
                list_name = "case fatality rate",
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

  # Trick to get new name
  name_1 <- paste(c(names(datos_covid), "1"), collapse = "")
  name_2 <- paste(c(names(datos_covid), "2"), collapse = "")
  .casos_totales <- casos(
    datos_covid = datos_covid,
    entidades = entidades,
    group_by_entidad = group_by_entidad,
    entidad_tipo = entidad_tipo,
    fecha_tipo = fecha_tipo,
    tipo_clasificacion = tipo_clasificacion,
    group_by_tipo_clasificacion = group_by_tipo_clasificacion,
    tipo_paciente = tipo_paciente,
    group_by_tipo_paciente = group_by_tipo_paciente,
    tipo_uci = tipo_uci,
    group_by_tipo_uci = group_by_tipo_uci,
    tipo_sector = tipo_sector,
    group_by_tipo_sector = group_by_tipo_sector,
    defunciones = FALSE,
    edad_cut = edad_cut,
    as_tibble = TRUE,
    fill_zeros = fill_NA,
    list_name = name_1,
    .grouping_vars = .grouping_vars
  )[[name_1]] |>
    dplyr::mutate(!!as.symbol("n") :=
      ifelse(!!as.symbol("n") == 0, NA, !!as.symbol("n"))) # change to NA the ones with 0

  .casos_defunciones <- casos(
    datos_covid = datos_covid,
    entidades = entidades,
    group_by_entidad = group_by_entidad,
    entidad_tipo = entidad_tipo,
    fecha_tipo = fecha_tipo,
    tipo_clasificacion = tipo_clasificacion,
    group_by_tipo_clasificacion = group_by_tipo_clasificacion,
    tipo_paciente = tipo_paciente,
    group_by_tipo_paciente = group_by_tipo_paciente,
    tipo_uci = tipo_uci,
    group_by_tipo_uci = group_by_tipo_uci,
    tipo_sector = tipo_sector,
    group_by_tipo_sector = group_by_tipo_sector,
    defunciones = TRUE,
    edad_cut = edad_cut,
    fill_zeros = TRUE,
    list_name = name_2,
    .grouping_vars = .grouping_vars
  )[[name_2]]


  .casos_totales <- .casos_totales |>
    dplyr::left_join(.casos_defunciones |>
      dplyr::rename(!!as.symbol("d") := !!as.symbol("n")),
    by = colnames(.casos_totales)[which(colnames(.casos_totales) != "n")]
    ) |>
    dplyr::mutate(!!as.symbol("CASE FATALITY RATE") :=
      dplyr::if_else(!is.na(!!as.symbol("n")),
        as.numeric(!!as.symbol("d")) / as.numeric(!!as.symbol("n")),
        NA_real_
      )) |>
    dplyr::select(-!!as.symbol("n"), -!!as.symbol("d"))


  .casos_totales <- list(.casos_totales)
  names(.casos_totales) <- list_name

  return(append(datos_covid, .casos_totales))
}
