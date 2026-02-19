#' Cree el consolidado a partir de los microdatos con los que dispone, agrupando
#' por las variables temporales que considere necesarias
#'
#' Esta función permite transformar desde la base de microdatos para obtener el
#' consolidado final con el cual trabaja la mayoría de las funciones gráficas
#' disponibles en `UnalR`.
#'
#' @param datos Un data frame con los microdatos.
#' @param formula Fórmula en la que el primer componente especificado (*antes
#'   del \eqn{\sim}*) hace referencia a la(s) variable(s) de interés, y el segundo
#'   componente (*luego del \eqn{\sim}*) a la(s) variable(s) temporales por las
#'   cuales se quiere agrupar (\emph{separadas por cualquier operador como
#'   `+`, `*`, `:`, etc.}).
#' @param frecuencia Vector o lista (*dependiendo de la cantidad de variables
#'   temporales especificadas*) numérica con los periodos que debería tener cada
#'   una de éstas.
#' @param intervalo Vector o lista (*dependiendo de la cantidad de variables
#'   temporales especificadas*) numérica que contiene los períodos de inicio y
#'   fin con los cuales se quiere realizar el filtro.
#' @param textNA Cadena de caracteres indicando el nombre que se dará a los registros
#'   cuando éstos presenten algún dato faltante (*en la variable seleccionada más
#'   no en las variables temporales especificadas*). El valor por defecto es
#'   "Sin Información".
#' @param ask Si es `TRUE` (*valor predeterminado*) mostrará un mensaje por consola
#'   preguntándole al usuario cuál periodo desea que sea el último por considerar
#'   (*esperando una respuesta por consola*). Si previamente se ha introducido el
#'   argumento `intervalo` éste quedará inhabilitado y no se ejecutará.
#'
#' @returns
#' Un data frame o tibble perteneciente a las clases "tbl_df", "tbl" y "data.frame".
#'
#' @examplesIf all(FALSE)
#' # library(readxl)
#' df1 <- read_excel(read_example("TestConsolidado1.xlsx"))
#' df2 <- read_excel(read_example("TestConsolidado2.xlsx"))
#'
#' # Seleccione para cada dataset (A, B, C y D) una de las opciones que se le muestran
#' # en consola (1, 2, 3 y 4) respectivamente.
#' Agregar(
#'   formula = TIPO_NIVEL ~ ANO + PERIODO,
#'   frecuencia = list("Year" = 2009:2013, "Period" = 1:3), df1
#' ) -> A
#' Agregar(
#'   formula = TIPO_NIVEL ~ ANO + PERIODO,
#'   frecuencia = list("Year" = 2009:2013, "Period" = 1:3), df1
#' ) -> B
#' Agregar(
#'   formula = TIPO_NIVEL ~ ANO + PERIODO,
#'   frecuencia = list("Year" = 2009:2013, "Period" = 1:3), df1
#' ) -> C
#' Agregar(
#'   formula = TIPO_NIVEL ~ ANO + PERIODO,
#'   frecuencia = list("Year" = 2009:2013, "Period" = 1:3), df1
#' ) -> D
#' all.equal(C, D)
#'
#' G <- Agregar(
#'   formula    = TIPO_NIVEL ~ ANO + PERIODO,
#'   frecuencia = list("Year" = 2009:2013, "Period" = 1:2),
#'   intervalo  = list(c(2009, 1), c(2013, 1)),
#'   datos      = df1
#' )
#' H <- Agregar(
#'   formula    = TIPO_NIVEL ~ ANO + PERIODO,
#'   frecuencia = list("Year" = 2009:2013, "Period" = 1:2),
#'   datos      = df1,
#'   ask        = FALSE
#' )
#' all.equal(G, H)
#'
#' # Observe las diferentes opciones que se le muestran por consola.
#' Agregar(formula = SEXO ~ YEAR + PERIODO, frecuencia = list("Year" = 2016:2021, "Period" = 1:3), df2)
#'
#' Agregar(
#'   formula    = SEXO ~ YEAR + PERIODO,
#'   frecuencia = list("Year" = 2016:2021, "Period" = 1:3),
#'   intervalo  = list(c(2018, 1), c(2020, 3)),
#'   datos      = df2
#' )
#' Agregar(
#'   formula    = SEXO ~ YEAR,
#'   frecuencia = 2016:2021,
#'   intervalo  = c(2018, 2020),
#'   datos      = df2
#' )
#'
#' # Observe que puede especificar más de una variable para realizar el agregado
#' # Se harán agregados simples y se unirán las filas en un único df
#' df1$SEXO <- c("Hombre", NA, NA, NA, NA, "Hombre", "Hombre",
#'               "Mujer", NA, NA, "Hombre", "Mujer", "Hombre", "Mujer"
#'               )
#' Agregar(
#'   formula    = TIPO_NIVEL + SEXO ~ ANO + PERIODO,
#'   frecuencia = list("Year" = 2010:2013, "Period" = 1:2),
#'   intervalo  = list(c(2010, 1), c(2013, 2)),
#'   datos      = df1
#' ) -> A
#' Agregar(
#'   formula    = TIPO_NIVEL + SEXO ~ ANO + PERIODO,
#'   frecuencia = list("Year" = 2010:2013, "Period" = 1:2),
#'   datos      = df1
#' ) -> B
#'
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @importFrom rlang :=
#' @importFrom utils menu
#' @importFrom methods is
#' @importFrom forcats fct_unique fct_na_value_to_level
Agregar <- function(datos, formula, frecuencia, intervalo, textNA = "Sin Informaci\u00f3n", ask = FALSE) {

  if (!is(formula, class2 = "formula")) {
    stop("\u00a1La f\u00f3rmula ingresada no pertenece a la clase 'formula'!", call. = FALSE)
  }
  Vars   <- all.vars(formula[[2]])
  Tiempo <- all.vars(formula[[3]])

  CountNa <- datos |> select(all_of(Tiempo)) |>
    summarise_all(~ sum(is.na(.))) |> sum()
  if (CountNa != 0) {
    stop(paste0(
      "\u00a1La(s) variable(s) '", paste0(Tiempo, collapse = "/"), "' presenta(n) datos faltantes!",
      "\n\t - No tiene sentido continuar con la creaci\u00f3n del agregado si no se tiene la informaci\u00f3n completa de \u00e9sta(s)."
      ), call. = FALSE
    )
  }

  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
  tibbleAgregado <- tibble()
  for (i in seq_len(length(Vars))) {
    Var <- Vars[i]
    # Convirtiendo los agentes en factores y haciendo explícitos los valores perdidos
    anyNA <- datos |> select(!!Var) |> is.na() |> sum()
    if (anyNA != 0) {
      datos <- datos |> mutate(!!Var := fct_na_value_to_level(!!sym(Var), level = textNA))
    } else {
      datos <- datos |> mutate_if(is.character, as.factor)
    }
    Step1 <- datos |> mutate_at(all_of(Tiempo), list(~ as.factor(.)))

    # Almacenamos los valores únicos que contiene el factor de la variable de interés
    UniqueFactor <- Step1 |> select(!!Var) |> pull() |> fct_unique()
    # Almacenamos las combinaciones existentes del tramo de tiempo
    UniqueTimes <- Step1 |> unite("X", all_of(Tiempo), sep = "-") |>
      distinct(X) |> pull() |> sort()

    # Creación del agregado
    Step2 <- Step1 |>
      summarise(Total = n(), .by = c(!!!syms(c(Tiempo, Var))))  |>
      rename("Clase" = all_of(Var)) |> mutate("Variable" = Var) |>
      relocate(Variable) |>
      # ■ Volviendo a la clase original del tiempo en cuestión
      mutate_at(all_of(Tiempo), list(~ as.numeric(as.character(.))))

    # Realizando el join con el dataframe que reúne todos los posibles periodos especificados
    is.listNumeric <- function(list) { return(sum(sapply(list, is.numeric)) == 2) }
    if (length(Tiempo) == 1 && is.numeric(frecuencia)) {
      Temp <- expand_grid("Anos" = frecuencia, UniqueFactor) |>
        rename(!!Tiempo[1] := Anos, "Clase" = UniqueFactor)
    } else if (length(Tiempo) == 2 && is.listNumeric(frecuencia)) {
      Temp <- expand_grid("Anos" = frecuencia[[1]], "Periodos" = frecuencia[[2]], UniqueFactor) |>
        rename(!!Tiempo[1] := Anos, !!Tiempo[2] := Periodos, "Clase" = UniqueFactor)
    } else {
      stop(paste0(
        "\u00a1Ha ocurrido alguno(s) de los siguientes problemas!:",
        "\n\t - Ha ingresado en la f\u00f3rmula un numero diferente a 1 o 2 variables temporales.",
        "\n\t - El vector/lista ingresado/a como 'frecuencia' contiene valores no num\u00e9ricos.",
        "\n\t - El n\u00famero de variables temporales y la dimensi\u00f3n de frecuencia no coinciden."
        ), call. = FALSE
      )
    }
    Step3 <- left_join(Temp, Step2, by = c(Tiempo, "Clase"), keep = FALSE) |>
      replace_na(list("Total" = 0, "Variable" = Var)) |> relocate(Variable)

    # Filtrando por el intervalo de tiempo en el caso en que éste sea especificado
    Filtrar1 <- function(intervalo) {
      return(Step3 |> filter(between(!!sym(Tiempo), intervalo[1], intervalo[2])))
    }
    Filtrar2 <- function(intervalo) {
      Abc <- Step3 |>
        filter(!(!!sym(Tiempo[1]) <= intervalo[[1]][1] & !!sym(Tiempo[2]) < intervalo[[1]][2]), !(!!sym(Tiempo[1]) < intervalo[[1]][1])) |> # Desde
        filter(!(!!sym(Tiempo[1]) >= intervalo[[2]][1] & !!sym(Tiempo[2]) > intervalo[[2]][2]), !(!!sym(Tiempo[1]) > intervalo[[2]][1]))    # Hasta
      return(Abc)
    }
    CallFilter <- function(Inicio, Fin) {
      if (length(Fin[[1]]) == 1) {
        Step4 <- Filtrar1(as.numeric(c(Inicio[[1]][1], Fin[[1]][1])))
      } else if (length(Fin[[1]]) == 2) {
        Step4 <- Filtrar2(intervalo = lapply(list(c(Inicio[[1]][1], Inicio[[1]][2]), c(Fin[[1]][1], Fin[[1]][2])), FUN = as.numeric))
      }
      return(Step4)
    }

    # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
    DB_FactTimes <- Step2 |>
      unite("X", all_of(Tiempo), sep = "-") |> distinct(X) |> pull()
    DB_JoinTimes <- Step3 |>
      unite("X", all_of(Tiempo), sep = "-") |> distinct(X) |> pull()

    if (!missingArg(intervalo)) {
      if (length(Tiempo) == 1 && is.numeric(intervalo)) {
        Step4 <- Filtrar1(intervalo)
      } else if (length(Tiempo) == 2 && is.listNumeric(intervalo)) {
        Step4 <- Filtrar2(intervalo)
      } else {
        Step4 <- Step3
        warning(paste0(
          "\u00a1Ha ocurrido alguno(s) de los siguientes problemas!:",
          "\n\t - Ha ingresado en la f\u00f3rmula un numero diferente a 1 o 2 variables temporales.",
          "\n\t - El vector/lista ingresado/a como 'intervalo' contiene valores no num\u00e9ricos.",
          "\n\u00bb\u00bb No se podr\u00e1 realizar el filtrado por el intervalo de tiempo que ha especificado."
          ), call. = FALSE
        )
      }
      Agregado <- Step4
    } else {
      if (ask) {
        if (!exists("CaseFilter")) {
          VarInterface <- menu(c(
            paste0(last(UniqueTimes) , "\t(\u00faltimo registro de la base de datos)"),
            paste0(last(DB_FactTimes), "\t(\u00faltima combinaci\u00f3n creada a partir de los niveles del dataset)"),
            paste0(last(DB_JoinTimes), "\t(\u00faltima combinaci\u00f3n creada a partir del argumento 'frecuencia')"),
            "Salir (usar valor por defecto [3])."
            ),
            title = "\u00bfCu\u00e1l desea que sea el \u00faltimo periodo por considerar?"
          )
          CaseFilter <- switch(
            VarInterface,
            "1" = last(UniqueTimes),
            "2" = last(DB_FactTimes),
            "3" = last(DB_JoinTimes),
            "4" = NULL
          )
        }
        if (VarInterface %in% 1:2) {
          Start    <- strsplit(first(DB_JoinTimes), "-")
          End      <- strsplit(CaseFilter, "-")
          Agregado <- CallFilter(Start, End)
        } else {
          Agregado <- Step3
        }
      } else {
        Agregado <- CallFilter(strsplit(first(DB_JoinTimes), "-"), strsplit(last(UniqueTimes), "-"))
      }
    }
    tibbleAgregado <- bind_rows(tibbleAgregado, Agregado)
  }
  if (length(Vars)>1) { tibbleAgregado <- tibbleAgregado |> mutate(Clase = as.character(Clase)) }
  return(tibbleAgregado)
}
