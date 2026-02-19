#' Case Hospitalization Rate (CHR)
#'
#' @description
#' `chr` Calcula la proporción de enfermos que resultan hospitalizados sobre todos los enfermos
#' confirmados en distintas categorías (residencia / edad / etc)
#'
#' @details El case hospitalization rate se define como
#'
#' \deqn{\frac{\# Hospitalizados}{Total de enfermos}}{%
#' \# Hospitalizados / Total de enfermos}
#'
#' Si se utiliza la opción `incluir_paciente_no_especificado` se puede cambiar la definicion
#' de **Total de enfermos** para incluir a los pacientes que dicen `NO ESPECIFICADO`. Estos
#' por default se excluyen justo por su naturaleza desconocida.
#'
#' @inheritParams casos
#' @inheritParams positividad
#'
#' @param incluir_paciente_no_especificado  (**opcional**)  Si en el denominador se incluyen
#' los pacientescuyo tipo es  `NO ESPECIFICADO`. Por default es `FALSE` por lo que sólo
#' se incluyen `AMBULATORIO`, `HOSPITALIZADO`.
#'
#' @importFrom rlang :=
#'
#' @return Une a la lista de `datos_covid` una nueva entrada de nombre `list_name`
#' (default: `case hospitalization rate`) con una base de datos (`tibble` o `duckdb`) con los
#' resultados agregados.
#' \itemize{
#'   \item `case hospitalization rate` - Base de datos generara con los datos agregados (el
#'   nombre cambia si se usa `list_name`).
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
#' # Casos a nivel nacional
#' datos_covid <- datos_covid |> chr()
#' head(datos_covid$`case hospitalization rate`)
#'
#' # Nacional
#' \donttest{
#' datos_covid <- datos_covid |> chr(list_name = "chr_nacional", group_by_entidad = FALSE)
#' head(datos_covid$`chr_nacional`)
#'
#' # CHR en IMSS e ISSSTE
#' datos_covid <- datos_covid |>
#'   chr(tipo_sector = c("IMSS", "ISSSTE"), list_name = "chimss", group_by_tipo_sector = TRUE)
#' head(datos_covid$`chimss`)
#'
#' # Calcula el CHR sobre toda la base
#' datos_covid <- datos_covid |>
#'   chr(
#'     tipo_clasificacion = c(
#'       "Sospechosos", "Confirmados COVID",
#'       "Negativo a COVID", "Inv\u00e1lido", "No realizado"
#'     ),
#'     group_by_tipo_clasificacion = TRUE, list_name = "chr_todos"
#'   )
#' head(datos_covid$`chr_todos`)
#'
#' # Distinguiendo sólo entre defunciones
#' datos_covid <- datos_covid |>
#'   chr(defunciones = TRUE, list_name = "chr_defun")
#' head(datos_covid$`chr_defun`)
#'
#' # Si deseas agrupar por una variable que no este en las opciones
#' datos_covid <- datos_covid |>
#'   chr(.grouping_vars = c("DIABETES"), list_name = "chr_diab")
#' head(datos_covid$chr_diab)
#' }
#' # Finalmente desconectamos
#' datos_covid$disconnect()
#'
#' @seealso [descarga_datos_abiertos()] [numero_pruebas()] [cfr()] [estima_rt()]
#' [positividad()] [casos()]
#' @export

chr <- function(datos_covid,
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
                tipo_clasificacion = c("Confirmados COVID"),
                group_by_tipo_clasificacion = FALSE,
                incluir_paciente_no_especificado = FALSE,
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
                list_name = "case hospitalization rate",
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

  # Obtenemos los tipos de paciente a usar
  if (incluir_paciente_no_especificado) {
    tp <- c("AMBULATORIO", "HOSPITALIZADO", "NO ESPECIFICADO")
  } else {
    tp <- c("AMBULATORIO", "HOSPITALIZADO")
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
    tipo_paciente = tp,
    group_by_tipo_paciente = FALSE,
    tipo_uci = c("SI", "NO", "NO APLICA", "SE IGNORA", "NO ESPECIFICADO"),
    group_by_tipo_uci = FALSE,
    tipo_sector = tipo_sector,
    group_by_tipo_sector = group_by_tipo_sector,
    defunciones = defunciones,
    edad_cut = edad_cut,
    as_tibble = TRUE,
    fill_zeros = fill_NA,
    list_name = name_1,
    .grouping_vars = .grouping_vars
  )[[name_1]] |>
    dplyr::mutate(!!as.symbol("n") :=
      ifelse(!!as.symbol("n") == 0, NA, !!as.symbol("n"))) # change to NA the ones with 0


  .casos_hospitalizados <- casos(
    datos_covid = datos_covid,
    entidades = entidades,
    group_by_entidad = group_by_entidad,
    entidad_tipo = entidad_tipo,
    fecha_tipo = fecha_tipo,
    tipo_clasificacion = tipo_clasificacion,
    group_by_tipo_clasificacion = group_by_tipo_clasificacion,
    tipo_paciente = "HOSPITALIZADO",
    group_by_tipo_paciente = FALSE,
    tipo_uci = c("SI", "NO", "NO APLICA", "SE IGNORA", "NO ESPECIFICADO"),
    group_by_tipo_uci = FALSE,
    tipo_sector = tipo_sector,
    group_by_tipo_sector = group_by_tipo_sector,
    defunciones = defunciones,
    edad_cut = edad_cut,
    fill_zeros = TRUE,
    list_name = name_2,
    .grouping_vars = .grouping_vars
  )[[name_2]]

  .casos_totales <- .casos_totales |>
    dplyr::left_join(.casos_hospitalizados |>
      dplyr::rename(!!as.symbol("h") := !!as.symbol("n")),
    by = colnames(.casos_totales)[which(colnames(.casos_totales) != "n")]
    ) |>
    dplyr::mutate(!!as.symbol("CASE HOSPITALIZATION RATE") :=
      dplyr::if_else(!is.na(!!as.symbol("n")),
        as.numeric(!!as.symbol("h")) / as.numeric(!!as.symbol("n")),
        NA_real_
      )) |>
    dplyr::select(-!!as.symbol("n"), -!!as.symbol("h"))


  .casos_totales <- list(.casos_totales)
  names(.casos_totales) <- list_name

  return(append(datos_covid, .casos_totales))
}
