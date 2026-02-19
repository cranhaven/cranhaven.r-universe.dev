#' Casos de COVID-19 en Mexico
#'
#' @description
#' `casos` Calcula el numero de casos registrados por fecha agrupando (o sin hacerlo)
#' por diferentes covariables. Por default calcula el total de casos (con y sin prueba positiva)
#'
#' @details
#' La función es un grupo de funciones de `dplyr` optimizadas para velocidad. Por ejemplo calcular
#' los casos por entidad se hace lo siguiente
#'
#' ```
#' datos_covid |> casos()
#' ```
#'
#' es lo mismo que:
#'
#' ```
#' library(dplyr)
#' datos_covid$casos <- datos_covid$dats |>
#'     group_by(ENTIDAD_UM, FECHA_SINTOMAS) |>
#'     tally() |>
#'     left_join(datos_covid$dict$ENTIDAD_UM, by = c("ENTIDAD_UM" = "CLAVE_ENTIDAD"))
#' ```
#'
#' Elaboraciones mas complicadas en casos tienen su equivalente en dplyr por ejemplo:
#'
#' ```
#' datos_covid <- datos_covid |>
#'   casos(
#'     entidad_tipo = "Residencia",
#'     entidades = c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR"),
#'     group_by_tipo_clasificacion = FALSE,
#'     tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
#'     group_by_tipo_paciente = TRUE,
#'     list_name = "bajas"
#'   )
#' ```
#' es equivalente a
#'
#' ```
#' datos_covid$bajas <- datos_covid$dats |>
#'    filter(ENTIDAD_RES == "02" | ENTIDAD_RES == "03") |> #BC/BCS
#'    filter(TIPO_PACIENTE == 1 | TIPO_PACIENTE == 2) |> #Ambulatorio/Hospitalizado
#'    group_by(FECHA_SINTOMAS, ENTIDAD_RES, TIPO_PACIENTE) |>
#'    tally() |>
#'    left_join(datos_covid$dict$ENTIDAD_RES, by = c("ENTIDAD_RES" = "CLAVE_ENTIDAD")) |>
#'    left_join(datos_covid$dict$PACIENTE, by = c("TIPO_PACIENTE" = "CLAVE"))
#' ```
#'
#' @param datos_covid (**obligatorio**) Lista de `tibble`s o `duckdb`s resultante de
#' [descarga_datos_abiertos()] o [read_datos_abiertos()]
#'
#' @param entidades (**opcional**)  Vector con las entidades de las unidades medicas a analizar.
#' Opciones: `AGUASCALIENTES`, `BAJA CALIFORNIA`, `BAJA CALIFORNIA SUR`,
#' `CAMPECHE`, `CHIAPAS`, `CHIHUAHUA`, `CIUDAD DE MEXICO`,
#' `COAHUILA DE ZARAGOZA` , `COLIMA`, `DURANGO`, `GUANAJUATO`, `GUERRERO`,
#' `HIDALGO`, `JALISCO`, `MEXICO`, `MICHOACAN DE OCAMPO`, `MORELOS`,`NAYARIT`
#' `NUEVO LEON`, `OAXACA` ,`PUEBLA`, `QUERETARO`,`QUINTANA ROO`,
#' `SAN LUIS POTOSI`, `SINALOA`, `SONORA`, `TABASCO`, `TAMAULIPAS`,`TLAXCALA`,
#' `VERACRUZ DE IGNACIO DE LA LLAVE`, `YUCATAN`, `ZACATECAS`.
#'
#' @param group_by_entidad (**opcional**) `TRUE` obtiene los casos para cada entidad reportando
#' en cada fecha la entidad y los casos en dicha entidad. `FALSE`  junta las `entidades` sumando
#' sus casos en una sola observacion por cada fecha.
#'
#' @param entidad_tipo (**opcional**) Indica a que se refiere las `entidades` seleccionadas. Elige
#' una de las opciones: `Unidad Medica` (entidad de la unidad medica), `Nacimiento`
#' (entidad de origen del individuo) o `Residencia` (entidad donde reside el individuo).
#'
#' @param fecha_tipo (**opcional**) Selecciona si la fecha que se utiliza es la fecha de `Ingreso`
#' (si aplica), la fecha de `Sintomas` o la de `Defuncion` (si aplica). El default es fecha de `Sintomas`.
#'
#' @param tipo_clasificacion (**opcional**)  Vector con el tipo de clasificaciones (por la prueba)
#' a incluir:`Sospechosos`,`Confirmados COVID`, `Negativo a COVID`, `Inv\u00e1lido`, `No realizado`
#'
#' @param group_by_tipo_clasificacion (**opcional**)  Booleana determinando si regresa la base
#' con cada entrada agrupada por `tipo_clasificacion` (es decir cada fecha
#' se generan tantos observaciones como grupos de tipo de clasificación) en caso `TRUE`. Si
#' `FALSE` suma todos los casos del tipo de clasificacion por fecha dando un solo numero por fecha.
#' El defalt es `FALSE`.
#'
#' @param tipo_paciente (**opcional**) Vector con el tipo de pacientes a incluir. Opciones:
#'  `AMBULATORIO`, `HOSPITALIZADO`, `NO ESPECIFICADO`. Por default se incluyen todos.
#'
#' @param group_by_tipo_paciente (**opcional**) Booleana determinando (caso `TRUE`) si regresa
#' la base con cada entrada agrupada por `tipo_paciente` (es decir cada fecha
#' se genera un renglon para `AMBULATORIO`, un renglon para `HOSPITALIZADO`, etc) o bien
#' si se suman todos los grupos y cada fecha reporta solo la suma de estos
#' (estilo `AMBULATORIO + HOSPITALIZADO` segun las categorias de `tipo_paciente`)
#' El default es `FALSE`.
#'
#' @param tipo_uci (**opcional**)  Vector con el tipo de valores para Unidad de
#' Cuidado Intensivo (UCI) a incluir:  `SI`,`NO`,`NO APLICA`,`SE IGNORA`,`NO ESPECIFICADO`.
#' Por default se incluyen todos.
#'
#' @param group_by_tipo_uci (**opcional**) Booleana. El caso `TRUE` determina si regresa la base
#' con cada fecha teniendo diferentes renglones uno para cada `tipo_uci` (es decir cada fecha
#' se generan tantos observaciones como grupos de tipo de UCI) o bien en una sola fecha
#' se suman todos los tipos de UCI (`FALSE`). El default es `FALSE`.
#'
#' @param tipo_sector (**opcional**) Vector con los sectores del sistema de salud a incluir:
#' `CRUZ ROJA`,`DIF`,`ESTATAL`,`IMSS`,`IMSS-BIENESTAR`,`ISSSTE`, `MUNICIPAL`,`PEMEX`,
#' `PRIVADA`,`SEDENA`,`SEMAR`,`SSA`, `UNIVERSITARIO`,`NO ESPECIFICADO`.
#' Por default se incluyen todos.
#'
#' @param group_by_tipo_sector (**opcional**) Booleana determina en el caso de `TRUE` si regresa
#' la base con cada entrada agrupada por `tipo_sector` (es decir cada fecha
#' tiene una entrada con los del `IMSS`, una entrada distinta con los de `ISSSTE`, etc) o bien
#' en caso de `FALSE` se devuelve una sola entrada por fecha con la suma `IMSS + ISSSTE + etc`
#' segun los  sectores seleccionados. El default es `FALSE`.
#'
#' @param defunciones (**opcional**)  Booleana si incluir sólo defunciones `TRUE` o a todos `FALSE`.
#' El default es `FALSE`.
#'
#' @param edad_cut (**opcional**) Vector con secuencia de edades para hacer grupos. Por ejemplo
#' `edad_cut = c(0, 10, Inf)` arma dos grupos de edad de 0 a 10 y de 10 a infinito o bien
#' `edad_cut = c(15, 20)` deja sólo los registros entre 15 y 20 años. Por default es `NULL`
#' y no arma grupos etarios.
#'
#' @param .grouping_vars (**opcional**) Vector de variables adicionales de agrupacion de los
#' conteos. Por ejemplo si se agrega `.grouping_vars = 'DIABETES'` entonces para cada fecha habra
#' dos conteos de casos uno de los que tienen diabetes y uno de los que no.
#'
#' @param as_tibble (**opcional**) Regresar como `tibble` el resultado. En caso de que `as_tibble`
#' sea `FALSE` se devuelve como conexion en `duckdb`. Se recomienda el default (`tibble`).
#'
#' @param fill_zeros (**opcional**) En caso de que el resultado sea un `tibble` regresa
#' observaciones para todas las combinaciones de variables incluyendo como 0 aquellas fechas
#' cuando no se observaron casos. En caso contrario no se incluyen las filas donde no se
#' observaron casos.
#'
#' @param list_name (**opcional**) Asigna un nombre en la lista de datos a la base generada
#'
#' @importFrom rlang :=
#'
#' @return Une a la lista de `datos_covid` una nueva entrada de nombre `list_name`
#' (default: `casos`) con una base de datos (`tibble` o `dbConnection`) con los
#' resultados agregados.
#' \itemize{
#'   \item casos - Base de datos generara con los datos agregados (el nombre cambia si
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
#' # correr el ejemplo descargando informacion mas reciente:
#' datos_covid <- datosabiertos
#'
#' # Casos por entidad
#' datos_covid <- datos_covid |> casos()
#' head(datos_covid$casos)
#'
#' # Defunciones por entidad
#' datos_covid <- datos_covid |> casos(defunciones = TRUE, list_name = "defunciones")
#' head(datos_covid$defunciones)
#'
#' # Hospitalizados por entidad
#' datos_covid <- datos_covid |>
#'   casos(tipo_paciente = "HOSPITALIZADO", list_name = "hospitalizados")
#' head(datos_covid$hospitalizados)
#'
#' # UCI por entidad
#' \donttest{
#' datos_covid <- datos_covid |> casos(tipo_uci = "SI", list_name = "uci")
#' head(datos_covid$uci)
#'
#' # Solo pacientes IMSS
#' datos_covid <- datos_covid |> casos(tipo_sector = "IMSS", list_name = "imss")
#' head(datos_covid$imss)
#'
#' # Pacientes IMSS y PEMEX separados
#' datos_covid <- datos_covid |> casos(tipo_sector = c("IMSS", "PEMEX"), list_name = "imss_y_pemex")
#' head(datos_covid$imss_y_pemex)
#'
#' # Pacientes IMSS y PEMEX sumados
#' datos_covid <- datos_covid |>
#'   casos(
#'     tipo_sector = c("IMSS", "PEMEX"), list_name = "imss_+_pemex",
#'     group_by_tipo_sector = TRUE
#'   )
#' head(datos_covid$`imss_+_pemex`)
#'
#' # Solo los de BAJA CALIFORNIA
#' datos_covid <- datos_covid |>
#'   casos(entidades = c("BAJA CALIFORNIA"), list_name = "BC")
#' head(datos_covid$BC)
#'
#' # Solo los de BAJA CALIFORNIA por residencia
#' datos_covid <- datos_covid |>
#'   casos(entidades = c("BAJA CALIFORNIA"), entidad_tipo = "Residencia", list_name = "residencia")
#' head(datos_covid$residencia)
#'
#' # Agrupando casos por tipo de clasificacion
#' datos_covid <- datos_covid |>
#'   casos(
#'     entidades = c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR"),
#'     group_by_tipo_clasificacion = TRUE,
#'     list_name = "BC_BCS"
#'   )
#' head(datos_covid$BC_BCS)
#'
#' # Regresa la suma de los de BC + BCS por tipo de paciente
#' datos_covid <- datos_covid |>
#'   casos(
#'     entidades = c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR"),
#'     group_by_tipo_clasificacion = FALSE,
#'     tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
#'     group_by_tipo_paciente = TRUE,
#'     list_name = "BC_+_BCS"
#'   )
#' head(datos_covid$`BC_+_BCS`)
#'
#' # Si deseas agrupar por una variable que no este en las opciones
#' datos_covid <- datos_covid |>
#'   casos(
#'     group_by_entidad = FALSE,
#'     tipo_paciente = c("AMBULATORIO", "HOSPITALIZADO"),
#'     group_by_tipo_paciente = TRUE,
#'     list_name = "sexo",
#'     .grouping_vars = c("SEXO")
#'   )
#' head(datos_covid$sexo)
#'
#' # Si no recuerdas la codificacion de los sexos puedes usar el diccionario:
#' datos_covid$sexo <- datos_covid$sexo |>
#'   dplyr::left_join(datos_covid$dict$SEXO, by = c("SEXO" = "CLAVE"))
#' head(datos_covid$sexo)
#'
#' # Si no recuerdas todas las variables de la base puedes usar glimpse para ver por
#' # que otras variables puedes clasificar
#' datos_covid$dats |> dplyr::glimpse()
#' }
#' 
#' # Una vez hayas concluido tu trabajo no olvides desconectar
#' datos_covid$disconnect()
#'
#' @seealso [descarga_datos_abiertos()] [numero_pruebas()] [cfr()] [chr()] [estima_rt()]
#' [positividad()]
#' @export

casos <- function(datos_covid,
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
                  entidad_tipo = c("Unidad Medica", "Residencia", "Nacimiento"),
                  fecha_tipo = c("Sintomas", "Ingreso", "Defuncion"),
                  tipo_clasificacion = c(
                    "Sospechosos", "Confirmados COVID",
                    "Negativo a COVID", "Inv\u00e1lido",
                    "No realizado"
                  ),
                  group_by_tipo_clasificacion = FALSE,
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
                  list_name = "casos",
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

  if (!as_tibble & fill_zeros) {
    cli::cli_alert_warning(
      "No puedo llenar con ceros si no es tibble. Usa {.code as_tibble = TRUE}"
    )
    fill_zeros <- FALSE
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
    cli::cli_abort("No logramos encontrar las entidades especificadas")
  }

  # Si esta la opcion de fill zeros se agrega
  if (fill_zeros) {

    # Check the dates to expand
    .fechasminmax <- datos_covid$dats |>
      dplyr::select_at(fecha_tipo) |>
      dplyr::summarise(
        fechamin = min(!!as.symbol(fecha_tipo), na.rm = TRUE),
        fechamax = max(!!as.symbol(fecha_tipo), na.rm = TRUE)
      ) |>
      dplyr::collect()

    .datesq <- seq(.fechasminmax$fechamin[1], .fechasminmax$fechamax[1], by = "1 day")

    # Get unique values of grouping vars
    if (length(.grouping_vars) > 0) {
      .group_values <- datos_covid$dats |>
        dplyr::distinct_at(.grouping_vars) |>
        dplyr::collect()

      # Expand to possible combinations
      .group_values <- .group_values |>
        tidyr::expand(!!!.group_values)
    } else {
      .group_values <- NULL
    }
  }

  lista_entidades <- paste0(entidades$CLAVE_ENTIDAD, collapse = "|")
  .casos <- datos_covid$dats |>
    dplyr::filter(
      stringr::str_detect(paste0("\\^", !!as.symbol(entidad_tipo), "\\$"), lista_entidades)
    )

  # Agregamos la entidad a la combinacion si fill zeros
  if (fill_zeros & group_by_entidad) {
    .grouping_entidad <- datos_covid$dict[[entidad_tipo]] |>
      dplyr::filter(stringr::str_detect(!!as.symbol("CLAVE_ENTIDAD"), lista_entidades)) |>
      dplyr::distinct(!!as.symbol("CLAVE_ENTIDAD"), .keep_all = FALSE) |>
      dplyr::rename(!!as.symbol(entidad_tipo) := !!as.symbol("CLAVE_ENTIDAD"))

    if (nrow(.grouping_entidad) == 0) {
      .grouping_entidad <- NULL
    }
  } else if (fill_zeros & !group_by_entidad) {
    .grouping_entidad <- NULL
  }

  #> CLASIFICACION FINAL----
  # Filtramos por tipo de caso
  clasificaciones_finales <- c()

  if (any(stringr::str_detect(tipo_clasificacion, "Sospechoso"))) {
    clasificaciones_finales <- c(clasificaciones_finales, 6) # Clasificacion final = 6: sospechoso
  }

  if (any(stringr::str_detect(tipo_clasificacion, "Confirmado"))) {
    # Clasificacion final = 1,2,3: confirmado
    clasificaciones_finales <- c(clasificaciones_finales, c(1, 2, 3))
  }

  if (any(stringr::str_detect(tipo_clasificacion, "Negativo"))) {
    # Clasificacion final = 7: negativo
    clasificaciones_finales <- c(clasificaciones_finales, 7)
  }

  if (any(stringr::str_detect(tipo_clasificacion, "Inv.*lido"))) {
    # Clasificacion final = 4: inv\u00e1lido
    clasificaciones_finales <- c(clasificaciones_finales, 4)
  }

  if (any(stringr::str_detect(tipo_clasificacion, "No realizado"))) {
    # Clasificacion final = 4: inv\u00e1lido
    clasificaciones_finales <- c(clasificaciones_finales, 5)
  }

  if (length(clasificaciones_finales) > 0) {
    .casos <- .casos |>
      dplyr::filter(!!as.symbol("CLASIFICACION_FINAL") %in% clasificaciones_finales)
  }

  # Agregamos la entidad a la combinacion si fill zeros
  if (fill_zeros & group_by_tipo_clasificacion) {
    .grouping_clasificacion <- datos_covid$dict[["CLASIFICACION_FINAL"]] |>
      dplyr::filter(!!as.symbol("CLAVE") %in% clasificaciones_finales) |>
      dplyr::distinct(!!as.symbol("CLAVE"), .keep_all = FALSE) |>
      dplyr::rename(!!as.symbol("CLASIFICACION_FINAL") := !!as.symbol("CLAVE")) |>
      dplyr::mutate(!!as.symbol("CLASIFICACION_FINAL") :=
        as.integer(!!as.symbol("CLASIFICACION_FINAL")))

    if (nrow(.grouping_clasificacion) == 0) {
      .grouping_clasificacion <- NULL
    }
  } else if (fill_zeros & !group_by_tipo_clasificacion) {
    .grouping_clasificacion <- NULL
  }

  #> TIPO DE PACIENTE----
  # Filtramos por tipo de paciente
  pacientes <-
    datos_covid$dict["PACIENTE"][[1]] |>
    dplyr::filter(
      stringr::str_detect(
        get(stringr::str_subset(colnames(datos_covid$dict["PACIENTE"][[1]]), "DESC*")),
        paste0(
          "\\b",
          paste0(tipo_paciente, collapse = "\\b|\\b"), "\\b"
        )
      )
    )

  lista_claves <- as.numeric(pacientes$CLAVE)
  .casos <- .casos |>
    dplyr::filter(!!as.symbol("TIPO_PACIENTE") %in% lista_claves)

  # Agregamos la entidad a la combinacion si fill zeros
  if (fill_zeros & group_by_tipo_paciente) {
    .grouping_paciente <- datos_covid$dict[["PACIENTE"]] |>
      dplyr::filter(!!as.symbol("CLAVE") %in% lista_claves) |>
      dplyr::distinct(!!as.symbol("CLAVE"), .keep_all = FALSE) |>
      dplyr::rename(!!as.symbol("TIPO_PACIENTE") := !!as.symbol("CLAVE")) |>
      dplyr::mutate(!!as.symbol("TIPO_PACIENTE") :=
        as.integer(!!as.symbol("TIPO_PACIENTE")))

    if (nrow(.grouping_paciente) == 0) {
      .grouping_paciente <- NULL
    }
  } else if (fill_zeros & !group_by_tipo_paciente) {
    .grouping_paciente <- NULL
  }

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
  .casos <- .casos |>
    dplyr::filter(!!as.symbol("UCI") %in% lista_claves)

  # Agregamos la entidad a la combinacion si fill zeros
  if (fill_zeros & group_by_tipo_uci) {
    .grouping_uci <- datos_covid$dict[["UCI"]] |>
      dplyr::filter(!!as.symbol("CLAVE") %in% lista_claves) |>
      dplyr::distinct(!!as.symbol("CLAVE"), .keep_all = FALSE) |>
      dplyr::rename(!!as.symbol("UCI") := !!as.symbol("CLAVE")) |>
      dplyr::mutate(!!as.symbol("UCI") := as.integer(!!as.symbol("UCI")))

    if (nrow(.grouping_uci) == 0) {
      .grouping_uci <- NULL
    }
  } else if (fill_zeros & !group_by_tipo_uci) {
    .grouping_uci <- NULL
  }

  #> TIPO DE SECTOR----
  # Filtramos por tipo de sector
  sectores <-
    datos_covid$dict["SECTOR"][[1]] |>
    dplyr::filter(
      stringr::str_detect(
        get(stringr::str_subset(colnames(datos_covid$dict["SECTOR"][[1]]), "DESC*")),
        paste0(paste0("^", tipo_sector, "$"), collapse = "|")
      )
    )

  lista_claves <- as.numeric(sectores$CLAVE)
  .casos <- .casos |>
    dplyr::filter(!!as.symbol("SECTOR") %in% lista_claves)

  # Agregamos la entidad a la combinacion si fill zeros
  if (fill_zeros & group_by_tipo_sector) {
    .grouping_sector <- datos_covid$dict[["SECTOR"]] |>
      dplyr::filter(!!as.symbol("CLAVE") %in% lista_claves) |>
      dplyr::distinct(!!as.symbol("CLAVE"), .keep_all = FALSE) |>
      dplyr::rename(!!as.symbol("SECTOR") := !!as.symbol("CLAVE")) |>
      dplyr::mutate(!!as.symbol("SECTOR") := as.integer(!!as.symbol("SECTOR")))

    if (nrow(.grouping_sector) == 0) {
      .grouping_sector <- NULL
    }
  } else if (fill_zeros & !group_by_tipo_sector) {
    .grouping_sector <- NULL
  }

  #> DEFUNCIONES----
  if (defunciones) {
    .casos <- .casos |>
      dplyr::filter(!is.na(!!as.symbol("FECHA_DEF")) &
        !!as.symbol("FECHA_DEF") >= as.POSIXct("2000/01/01"))
  }

  #> EDADES----
  if (!is.null(edad_cut)) {
    # Obtenemos los grupos de edad
    .casos <- .casos |>
      dplyr::mutate(!!as.symbol("EDAD_CAT") := cut(!!as.symbol("EDAD"), 
                                                   breaks = edad_cut, 
                                                   include.lowest = TRUE)) |>
      dplyr::filter(!is.na(!!as.symbol("EDAD_CAT")))
  }

  # Agregamos la entidad a la combinacion si fill zeros
  if (fill_zeros & !is.null(edad_cut)) {
    .grouping_edad <- dplyr::tibble(EDAD_CAT = cut(edad_cut, breaks = edad_cut,
                                                   include.lowest = TRUE)) |>
      dplyr::distinct() |> #For n = 2, cut repeats the first label so keep only one
      dplyr::filter(!is.na(!!as.symbol("EDAD_CAT")))

    if (nrow(.grouping_edad) == 0) {
      .grouping_edad <- NULL
    }
  } else if (fill_zeros & is.null(edad_cut)) {
    .grouping_edad <- NULL
  }

  #> AGRUPACI\u00d3N----
  .casos <- .casos |>
    dplyr::group_by_at(fecha_tipo)

  if (length(.grouping_vars) > 0) {
    for (var in .grouping_vars) {
      .casos <- .casos |>
        dplyr::group_by_at(var, .add = TRUE)
    }
  }

  #> AGRUPACI\u00d3N EDAD
  if (!is.null(edad_cut)) {
    .casos <- .casos |>
      dplyr::group_by_at("EDAD_CAT", .add = TRUE)
  }

  # Tomamos el grupo
  if (group_by_entidad) {
    .casos <- .casos |>
      dplyr::group_by_at(entidad_tipo, .add = TRUE)
  }

  # Tomamos el grupo
  if (group_by_tipo_clasificacion) {
    .casos <- .casos |>
      dplyr::group_by_at("CLASIFICACION_FINAL", .add = TRUE)
  }

  if (group_by_tipo_paciente) {
    .casos <- .casos |>
      dplyr::group_by_at("TIPO_PACIENTE", .add = TRUE)
  }

  if (group_by_tipo_sector) {
    .casos <- .casos |>
      dplyr::group_by_at("SECTOR", .add = TRUE)
  }

  if (group_by_tipo_uci) {
    .casos <- .casos |>
      dplyr::group_by_at("UCI", .add = TRUE)
  }

  # Conteo de los .casos
  .casos <- .casos |>
    dplyr::tally() |>
    dplyr::ungroup()

  if (as_tibble) {
    .casos <- .casos |>
      dplyr::collect()

    if (fill_zeros) {
      .groups <- tidyr::expand_grid(
        .group_values, .grouping_entidad,
        .grouping_clasificacion, .grouping_paciente,
        .grouping_uci, .grouping_sector, .grouping_edad
      )

      # Create grid of all possible dates
      .grid_casos <- tidyr::expand_grid(!!as.symbol(fecha_tipo) := .datesq, .groups)


      # Full join
      .casos <- .casos |>
        dplyr::full_join(.grid_casos, by = colnames(.grid_casos)) |>
        dplyr::mutate(!!as.symbol("n") := tidyr::replace_na(!!as.symbol("n"), 0)) |>
        dplyr::ungroup()
    }
  }

  if (nrow(entidades) > 0 & group_by_entidad) {
    name_join <- c("CLAVE_ENTIDAD")
    names(name_join) <- entidad_tipo
    .casos <- .casos |> 
      dplyr::left_join(datos_covid$dict[entidad_tipo][[1]], by = name_join) |>
      dplyr::ungroup()
  }

  if (length(clasificaciones_finales) > 0 & group_by_tipo_clasificacion) {
    name_join <- c("CLAVE")
    names(name_join) <- "CLASIFICACION_FINAL"
    .casos <- .casos |>
      dplyr::left_join(datos_covid$dict["CLASIFICACION_FINAL"][[1]][, c(1, 2)], by = name_join) |>
      dplyr::ungroup()
  }

  if (nrow(pacientes) > 0 & group_by_tipo_paciente) {
    name_join <- c("CLAVE")
    names(name_join) <- "TIPO_PACIENTE"
    paciente_df <- datos_covid$dict["PACIENTE"][[1]]
    colnames(paciente_df) <- c("CLAVE", "DESCRIPCION_TIPO_PACIENTE")
    .casos <- .casos |>
      dplyr::left_join(paciente_df, by = name_join) |>
      dplyr::ungroup()
  }

  if (nrow(ucis) > 0 & group_by_tipo_uci) {
    name_join <- c("CLAVE")
    names(name_join) <- "UCI"
    uci_df <- datos_covid$dict["UCI"][[1]]
    colnames(uci_df) <- c("CLAVE", "DESCRIPCION_TIPO_UCI")
    .casos <- .casos |>
      dplyr::left_join(uci_df, by = name_join) |> 
      dplyr::ungroup()
  }

  if (nrow(sectores) > 0 & group_by_tipo_sector) {
    name_join <- c("CLAVE")
    names(name_join) <- "SECTOR"
    sector_df <- datos_covid$dict["SECTOR"][[1]]
    colnames(sector_df) <- c("CLAVE", "DESCRIPCION_TIPO_SECTOR")
    .casos <- .casos |>
      dplyr::left_join(sector_df, by = name_join) |> 
      dplyr::ungroup()
  }

  .casos <- list(.casos)
  names(.casos) <- list_name

  return(append(datos_covid, .casos))
}
