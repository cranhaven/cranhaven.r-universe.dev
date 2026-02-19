#' RT: Número efectivo de reproducción
#'
#' @description
#' `estima_rt` Calcula el número efectivo de reproducción por fecha y entidad usando
#' los metodos de [EpiEstim::estimate_R()]. Por default calcula el número efectivo de
#' reproducción para cada estado.
#'
#' @details Se sugiere establecer una mínima fecha y una máxima fecha con `min_date` y
#' `max_date` para la estimación pues los intervalos seriales de omicron son distintos
#' a los de la variante delta.
#'
#' @inheritParams casos
#'
#' @param method (**opcional**) Metodo para estimar el RT con [EpiEstim::estimate_R()]. Por
#' default se recomienda el método paramétrico de intervalo serial `parametric_si`.
#'
#' @param config (**opcional**) Configuracion para la estimacion del RT usando
#' [EpiEstim::make_config()]. Por default se utiliza una media del intervalo serial
#' de `mean_si = 2.5` y una desviación estandar de `std_si = 1.6`. Sin embargo, como el intervalo
#' serial depende mucho de la variante se recomienda cambiarlo.
#'
#' @param min_date (**opcional**) Mínima fecha a partir de la cual estimar el RT.
#'
#' @param max_date (**opcional**) Máxima fecha a partir de la cual estimar el RT.
#'
#' @param ... (**opcional**) Parámetros adicionales para [EpiEstim::estimate_R()].
#'
#' @importFrom rlang :=
#'
#' @return Une a la lista de `datos_covid` una nueva entrada de nombre `list_name`
#' (default: `estima_rt`) con una base de datos (`tibble`) con los
#' resultados agregados.
#' \itemize{
#'   \item `estima_rt` - Base de datos generara con los datos agregados (el nombre cambia si
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
#' # Casos a nivel nacional por estado en todos
#' \donttest{
#' suppressWarnings(
#'   datos_covid <- datos_covid |> estima_rt()
#' )
#' head(datos_covid$estima_rt)
#'
#' # Cambios en la fecha de estimacion siguiendo la recomendacion
#' # y obtenemos todo a nivel nacional
#' datos_covid <- datos_covid |> estima_rt(
#'   min_date = as.POSIXct("2021-07-01"),
#'   max_date = as.POSIXct("2021-09-01"),
#'   list_name = "rt_min_max",
#'   group_by_entidad = FALSE
#' )
#' head(datos_covid$rt_min_max)
#'
#' # Casos a nivel nacional en los confirmados
#' datos_covid <- datos_covid |>
#'   estima_rt(
#'     tipo_clasificacion = "Confirmados COVID",
#'     group_by_entidad = FALSE,
#'     list_name = "rt_confirmados"
#'   )
#' head(datos_covid$rt_confirmados)
#'
#' #' # Cambios en los parametros de epiestim
#' # estos parametros no tienen razon de ser mas alla de mostrar como se cambian
#' datos_covid <- datos_covid |>
#'   estima_rt(
#'     group_by_entidad = FALSE,
#'     list_name = "config_rt",
#'     method = "uncertain_si", # Metodo de estimacion
#'     config = EpiEstim::make_config(
#'       mean_si = 2.4,
#'       std_si = 0.3,
#'       std_mean_si = 0.2,
#'       min_mean_si = 2,
#'       max_mean_si = 4,
#'       std_std_si = 0.1,
#'       min_std_si = 0.1,
#'       max_std_si = 1.0
#'     )
#'   )
#' head(datos_covid$config_rt)
#' 
#' # Casos en BC, BCS en los confirmados
#' datos_covid |>
#'   estima_rt(
#'     entidades = c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR"),
#'     tipo_clasificacion = "Confirmados COVID",
#'     group_by_entidad = TRUE,
#'     list_name = "rt_bc_bcs"
#'   ) |>
#'   plot_covid(
#'     df_name = "rt_bc_bcs", df_date_index = "FECHA_SINTOMAS",
#'     df_variable = "Mean(R)", df_covariates = "ENTIDAD_FEDERATIVA"
#'   )
#' }
#'
#' # Finalmente desconectamos
#' datos_covid$disconnect()
#'
#' @seealso [descarga_datos_abiertos()] [numero_pruebas()] [cfr()] [chr()]
#' [positividad()] [casos()]
#'
#' @export
estima_rt <- function(datos_covid,
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
                      tipo_clasificacion = c(
                        "Sospechosos", "Confirmados COVID",
                        "Negativo a COVID", "Inv\u00e1lido",
                        "No realizado"
                      ),
                      tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO", "NO ESPECIFICADO"),
                      list_name = "estima_rt",
                      min_date = as.POSIXct("2020-01-01",
                        tz = Sys.timezone(),
                        format = "%Y-%m-%d"
                      ),
                      max_date = as.POSIXct(Sys.time()),
                      method = "parametric_si",
                      config = if (requireNamespace("EpiEstim", quietly = TRUE)) {
                        EpiEstim::make_config(
                          list(
                            mean_si = 2.5,
                            std_si = 1.6
                          )
                        )
                      } else {
                        NULL
                      },
                      ...) {

  # Chequeo de verificacion de EPIESTIM
  if (!requireNamespace("EpiEstim", quietly = TRUE)) {
    cli::cli_abort(
      "Por favor instala {.code EpiEstim} para poder calcular el RT con
     {.code install.packages('EpiEstim')}"
    )
  }

  # Chequeo de verificacion de lubridate para las fechas
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    cli::cli_abort(
      "Por favor instala {.code lubridate} para poder calcular el RT con
     {.code install.packages('lubridate')}"
    )
  }

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


  .casos <- casos(
    datos_covid = datos_covid,
    entidades = entidades,
    group_by_entidad = group_by_entidad,
    fecha_tipo = fecha_tipo,
    tipo_clasificacion = tipo_clasificacion,
    group_by_tipo_clasificacion = FALSE,
    tipo_paciente = tipo_paciente,
    group_by_tipo_paciente = FALSE,
    tipo_uci = c("SI", "NO", "NO APLICA", "SE IGNORA", "NO ESPECIFICADO"),
    group_by_tipo_uci = FALSE,
    tipo_sector = c(
      "CRUZ ROJA", "DIF", "ESTATAL", "IMSS", "IMSS-BIENESTAR", "ISSSTE",
      "MUNICIPAL", "PEMEX", "PRIVADA", "SEDENA", "SEMAR", "SSA", "UNIVERSITARIO",
      "NO ESPECIFICADO"
    ),
    group_by_tipo_sector = FALSE,
    defunciones = FALSE,
    edad_cut = NULL,
    as_tibble = TRUE,
    fill_zeros = TRUE,
    list_name = list_name,
    .grouping_vars = c()
  )[[list_name]]

  # Detectamos cuál es la fecha
  fecha_name <- stringr::str_subset(colnames(.casos), "FECHA")
  col_gp <- stringr::str_subset(colnames(.casos), "FECHA|\\bn\\b", negate = TRUE)

  .casos <- .casos |>
    dplyr::filter(!!as.symbol(fecha_name) >= !!min_date) |>
    dplyr::filter(!!as.symbol(fecha_name) <= !!max_date)

  if (nrow(.casos) == 0) {
    cli::cli_abort("No hay observaciones entre {min_date} y {max_date}")
  }


  if (length(col_gp) > 0) {
    for (col in col_gp) {
      .casos <- .casos |>
        dplyr::group_by_at(col, .add = TRUE)
    }
  }

  mfec <- .casos[fecha_name] |>
    dplyr::summarise(!!as.symbol("min") := min(!!as.symbol(fecha_name)), .groups = "drop") |>
    as.vector()

  df_rt <- .casos |>
    dplyr::arrange(!!as.symbol(fecha_name)) |>
    dplyr::summarise(
      EpiEstim::estimate_R(as.numeric(!!as.symbol("n")), method = method, config = config, ...)$R,
      .groups = "drop"
    ) |>
    dplyr::mutate(!!as.symbol(paste0(fecha_name, "_start")) := mfec$min[1] + lubridate::days(!!as.symbol("t_start"))) |>
    dplyr::mutate(!!as.symbol(paste0(fecha_name, "_end")) := mfec$min[1] + lubridate::days(!!as.symbol("t_end"))) |>
    dplyr::mutate(!!as.symbol(fecha_name) := mfec$min[1] + lubridate::days((!!as.symbol("t_start") + !!as.symbol("t_end")) / 2))

  df_rt <- list(df_rt)
  names(df_rt) <- list_name

  return(append(datos_covid, df_rt))
}
