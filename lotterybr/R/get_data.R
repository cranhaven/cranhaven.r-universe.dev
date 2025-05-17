#' Function to download lottery data from git repository

#' @export
#'
#' @param game select which Brazilian lottery game
#' @param type select if you want the winners database or the numbers result
#' @param language select desired language
#' @return tibble/data.frame containing the downloaded data

#' @description This function downloads the Brazilian lottery game data from Caixa Federal up to the date declared

#' @import dplyr
#' @examples
#' \donttest{
#' library(lotterybr)
#' megasena_ganhadores <- get_data(game= "megasena",type = "winners",language = "eng")
#' megasena_ganhadores
#'
#' lotofacil_dezenas <- get_data(game= "lotofacil",type = "numbers", language = "ptbr")
#' lotofacil_dezenas
#' }
#'

get_data = function(game= c("maismilionaria", "megasena", "lotofacil",  "quina", "lotomania", "duplasena", "diadesorte"),
                    type = c("numbers","winners"),
                    language = "eng"){

  data = NULL
  course = NULL
  accumulated = NULL
  winners = NULL
  prize = NULL
  acertos = NULL
  acumula = NULL
  numbers_clovers = NULL
  numbers = NULL
  numbers1 = NULL
  numbers2 = NULL
  month = NULL
  mes = NULL
  game = tolower(game)
  type = tolower(type)
  type0 = type
  type = switch(
    type,
    'numbers' = 'dezenas',
    'winners' = 'ganhadores'
  )


  url = paste0("https://github.com/tomasbp2/LotteryBrasilDATA/blob/main/",game,"/",type,".rds?raw=true")
  file = readRDS(url(url))

  if(language == "ptbr"){
    data = file

    if (game == "maismilionaria") {
      # maismilionaria ----

      if(type == "ganhadores"){
        {data = data %>%
          dplyr::rename(
            data = date,
            concurso = course,
            acumula = accumulated,
            acertos = match,
            ganhadores = winners,
            premio = prize
          )
        data = data %>%
          dplyr::mutate(
            acertos = dplyr::recode(acertos,
                             "6 + 2 clovers" = "sena + 2 trevos",
                             "6 + 1 or 0 clovers" = "sena + 1 ou 0 trevos",
                             "5 + 2 clovers" = "quina + 2 trevos",
                             "5 + 1 or 0 clovers" = "quina + 1 ou 0 trevos",
                             "4 + 2 clovers" = "quadra + 2 trevos",
                             "4 + 1 or 0 clovers" = "quadra + 1 ou 0 trevos",
                             "3 + 2 clovers" = "terno + 2 trevos",
                             "3 + 1 clovers" = "terno + 1 trevo",
                             "2 + 2 clovers" = "duque + 2 trevos",
                             "2 + 1 clover" = "duque + 1 trevo"
            ),
            acumula = dplyr::recode(acumula,
                             "yes" = "sim",
                             "no" = "nao"     )
          )
        }
      }

      if(type == "dezenas"){
        data = data %>%
          dplyr::rename(
            data = date,
            concurso = course,
            acumula = accumulated,
            dezenas_trevos = numbers_clovers
          )
        data = data %>%
          dplyr::mutate(
            acumula = dplyr::recode(acumula,
                             "yes" = "sim",
                             "no" = "nao"
            )
          )
      }
    }

    if (game == "megasena") {
      # megasena ----

      if(type == "ganhadores"){data = data %>%
        dplyr::rename(
          data = date,
          concurso = course,
          acumula = accumulated,
          acertos = match,
          ganhadores = winners,
          premio = prize
        )
      data = data %>%
        dplyr::mutate(
          acertos = dplyr::recode(acertos,
                           "6" = "sena",
                           "5" = "quina",
                           "4" = "quadra"
          ),
          acumula = dplyr::recode(acumula,
                           "yes" = "sim",
                           "no" = "nao"     )
        )
      }


      if(type == "dezenas"){data = data %>%
        dplyr::rename(
          data = date,
          concurso = course,
          acumula = accumulated,
          dezenas = numbers
        )
      data = data %>%
        dplyr::mutate(
          acumula = dplyr::recode(acumula,
                           "yes" = "sim",
                           "no" = "nao"     )
        )
      }
    }

    if (game == "lotofacil") {
      # lotofacil ----

      if(type == "ganhadores"){data = data %>%
        dplyr::rename(
          data = date,
          concurso = course,
          acumula = accumulated,
          acertos = match,
          ganhadores = winners,
          premio = prize
        )
      data = data %>%
        dplyr::mutate(
          acumula = dplyr::recode(acumula,
                           "yes" = "sim",
                           "no" = "nao"     )
        )
      }

      if(type == "dezenas"){data = data %>%
        dplyr::rename(
          data = date,
          concurso = course,
          acumula = accumulated,
          dezenas = numbers
        )
      data = data %>%
        dplyr::mutate(
          acumula = dplyr::recode(acumula,
                           "yes" = "sim",
                           "no" = "nao"     )
        )
      }
    }

    if (game == "quina") {
      # quina ----

      if(type == "ganhadores"){data = data %>%
        dplyr::rename(
          data = date,
          concurso = course,
          acumula = accumulated,
          acertos = match,
          ganhadores = winners,
          premio = prize
        )
      data = data %>%
        dplyr::mutate(
          acumula = dplyr::recode(acumula,
                           "yes" = "sim",
                           "no" = "nao"     )
        )
      }


      if(type == "dezenas"){data = data %>%
        dplyr::rename(
          data = date,
          concurso = course,
          acumula = accumulated,
          dezenas = numbers
        )
      data = data %>%
        dplyr::mutate(
          acumula = dplyr::recode(acumula,
                           "yes" = "sim",
                           "no" = "nao"     )
        )
      }
    }

    if (game == "lotomania") {
      # lotomania ----

      if(type == "ganhadores"){data = data %>%
        dplyr::rename(
          data = date,
          concurso = course,
          acumula = accumulated,
          acertos = match,
          ganhadores = winners,
          premio = prize
        )
      data = data %>%
        dplyr::mutate(
          acumula = dplyr::recode(acumula,
                           "yes" = "sim",
                           "no" = "nao"     )
        )
      }

      if(type == "dezenas"){data = data %>%
        dplyr::rename(
          data = date,
          concurso = course,
          acumula = accumulated,
          dezenas = numbers
        )
      data = data %>%
        dplyr::mutate(
          acumula = dplyr::recode(acumula,
                           "yes" = "sim",
                           "no" = "nao"     )
        )
      }
    }

    if (game == "duplasena") {
      # duplasena ----

      if(type == "ganhadores"){data = data %>%
        dplyr::rename(
          data = date,
          concurso = course,
          acumula = accumulated,
          acertos = match,
          ganhadores = winners,
          premio = prize
        )
      data = data %>%
        dplyr::mutate(
          acertos = dplyr::recode(acertos,
                           "6" = "sena",
                           "5" = "quina",
                           "4" = "quadra",
                           "3" = "terno"
          ),
          acumula = dplyr::recode(acumula,
                           "yes" = "sim",
                           "no" = "nao"     )
        )
      }

      if(type == "dezenas"){ data = data %>%
        dplyr::rename(
          data = date,
          concurso = course,
          acumula = accumulated,
          dezenas1 = numbers1,
          dezenas2 = numbers2
        )
      data = data %>%
        dplyr::mutate(
          acumula = dplyr::recode(acumula,
                           "yes" = "sim",
                           "no" = "nao"     )
        )
      }
    }

    if (game == "diadesorte") {
      if(type == "ganhadores"){
        data = data %>%
        dplyr::rename(
          data =date ,
          concurso = course,
          acumula = accumulated,
          acertos = match,
          ganhadores = winners ,
          premio = prize
        )
      data = data %>%
        dplyr::mutate(
          acertos = dplyr::recode(acertos,
                           "Lucky Month" = "Mes da Sorte"
          ),
          acumula = dplyr::recode(acumula,
                           "yes" = "sim",
                           "no" = "nao"
          )
        )
      }


      if(type == "dezenas"){data = data %>%
        dplyr::rename(
          data = date,
          concurso = course,
          acumula = accumulated,
          dezenas = numbers,
          mes = month
        )
      data = data %>%
        dplyr::mutate(
          acumula = dplyr::recode(acumula,
                               "yes" = "sim",
                               "no" = "nao"),
          mes = dplyr::recode(mes,
                         "January" = "Janeiro",
                         "February" = "Fevereiro",
                         "March" = "Marco",
                         "April" = "Abril",
                         "May" = "Maio",
                         "June" = "Junho",
                         "July" = "Julho",
                         "August" = "Agosto",
                         "September" = "Setembro",
                         "October" = "Outubro",
                         "November" = "Novembro",
                         "December" = "Dezembro"
          )
        )
      }
    }
  return(data)
  } else{
  return(file)
  }
}
