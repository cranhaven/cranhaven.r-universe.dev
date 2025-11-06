#' Query the data of the loan contracts made through the National Bank for Economic and Social Development (BNDES).
#'
#' Downloads contracts data for the selected years, since 2002, and return it in the form of a dataframe.
#' Note: to access the total amount disbursed, use the function query_desimbursements_data().
#'
#' @importFrom utils download.file unzip
#'
#' @param year selects the years which data will be downloaded. integer.
#'
#' @return a dataframe with data for the selected year.
#'
#' @examples
#' \dontrun{query_contracts(year = 2012)}
#'
#' @export
query_contracts <- function(year = 'all') {

  options(scipen = 999, timeout = 1500)

  data_contrat <- ano <- valor_contratacao_reais <- juros <- subsetor_cnae_agrup <- valor_desembolso_reais <- situacao_operacional <- prazo_carencia_meses <- prazo_amortizacao_meses <-  NULL

  ano_atual <- as.numeric(format(Sys.Date(), "%Y"))

  if ("all" %in% year) {

    year <- c(2002:ano_atual)

  }

  dir.temp <- tempdir()

  # download spreadsheet with direct operations
  url_list <- "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/naoautomaticas/naoautomaticas.xlsx"

  for (i in year) {

    #indiretas

  if(i %in% c(2017:2024)){
    link <- "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2017-01-01_ate_2024-11-30.xlsx"
    url_list <- append(x = url_list, values = link)
  }

  else if(i %in% c(2015:2016)){
    link <- "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2015-01-01_ate_2016-12-31.xlsx"
    url_list <- append(x = url_list, values = link)
    }

  else if(i == 2014){
    link <- "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2014-01-01_ate_2014-12-31.xlsx"
    url_list <- append(x = url_list, values = link)
    }

  else if(i == 2013){
    link <- "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2013-01-01_ate_2013-12-31.xlsx"
    url_list <- append(x = url_list, values = link)
    }

  else if(i == 2012){
    link <- "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2012-01-01_ate_2012-12-31.xlsx"
    url_list <- append(x = url_list, values = link)
    }

  else if(i == 2011){
    link <- "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2011-01-01_ate_2011-12-31.xlsx"
    url_list <- append(x = url_list, values = link)
    }

  else if(i %in% c(2009:2010)){
    link <- "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2009-01-01_ate_2010-12-31.xlsx"
    url_list <- append(x = url_list, values = link)
    }

  else if(i %in% c(2002:2008)){
    link <- "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2002-01-01_ate_2008-12-31.xlsx"
    url_list <- append(x = url_list, values = link)
    }
  }

  # list the urls
  url_list <- unique(url_list)

  for(i in seq_along(url_list)) {

     if(url_list[i] == "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/naoautomaticas/naoautomaticas.xlsx"){
       file_name <- "nao_automatico"

     } else if(url_list[i] == "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2017-01-01_ate_2024-11-30.xlsx") {
       file_name <- "2017-2024"

     } else if(url_list[i] == "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2015-01-01_ate_2016-12-31.xlsx") {
       file_name <- "2015-2016"

     } else if(url_list[i] == "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2014-01-01_ate_2014-12-31.xlsx") {
       file_name <- "2014"

     } else if(url_list[i] == "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2013-01-01_ate_2013-12-31.xlsx") {
       file_name <- "2013"

     } else if(url_list[i] == "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2012-01-01_ate_2012-12-31.xlsx") {
       file_name <- "2012"

     } else if(url_list[i] == "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2011-01-01_ate_2011-12-31.xlsx") {
       file_name <- "2011"

     } else if(url_list[i] == "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2009-01-01_ate_2010-12-31.xlsx") {
       file_name <- "2009-2010"

     } else if(url_list[i] == "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2002-01-01_ate_2008-12-31.xlsx") {
       file_name <- "2002-2008"
     } else {
       file_name <- "data_indeterminada"
     }

      # check if files have already been downloaded
      if(file.exists(paste0(dir.temp, "/financiamentos","_", file_name, ".xlsx"))){
        message(paste0("File ", file_name,".xlsx already downloaded."))

      } else {

        if (RCurl::url.exists(url_list[i] == F)) { # network is down = message (not an error anymore)
          message("No internet connection or data source broken.")
          return(NULL)

        } else {

            message(paste0('Downloading file financiamentos_',file_name,'.xlsx'))

            # depois document, load all, test e submit to cran
            tryCatch({
              download.file(url_list[i],
                            destfile = paste0(dir.temp, "/financiamentos","_", file_name, ".xlsx"),
                            mode = "wb") # download the file in binary mode (needed for these xlsx files)
            },
            # em caso de erro, interrompe a função e mostra msg de erro

            error = function(e) {
              message("Error downloading file. Try again later.", e$message)
              stop("Error downloading file.")  }
            )

            # If the last year files has been updated, it will try to guess the new date

            if(file.size(paste0(dir.temp, "/financiamentos","_", file_name, ".xlsx")) < 100000){

              message('The source site changed the file name. Trying to access the download link.')

              link_date <- "2024-11-30"
              link_date <- lubridate::date(link_date)
              date_today <- Sys.Date()

              dates_vector <- seq(as.Date(link_date), as.Date(date_today), by = "days")

                  for (i in dates_vector) {

                  download.file(paste0("https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/automaticas/operacoes_indiretas_automaticas_2017-01-01_ate_",
                                       as.Date(i, origin = "1970-01-01"), ".xlsx"),
                                destfile = paste0(dir.temp, "/financiamentos","_", file_name, ".xlsx"),
                                mode = "wb") # download the file in binary mode)

                    i <- i + 1

                    if (file.size(paste0(dir.temp, "/financiamentos","_", file_name, ".xlsx")) > 100000){
                      break
                    }
                  }

                  if(file.size(paste0(dir.temp, "/financiamentos","_", file_name, ".xlsx")) < 100000) {
                    message("Could not download file.")
                  }


              }


        }

        }
    }

  # import the files
  suppressWarnings({

  lista.arquivos.locais <- list.files(path = dir.temp, pattern = "^financiamentos.*\\.xlsx$", full.names = F)

  # Import only the files informed in the year argument

  files_import <- "financiamentos_nao_automatico.xlsx"

  for(i in year) {

  if(i %in% c(2002:2008)){
    files_import <- append(x = files_import, values = "financiamentos_2002-2008.xlsx")
  }

  if(i %in% c(2009:2010)){
    files_import <- append(x = files_import, values = "financiamentos_2009-2010.xlsx")
  }

  if(i %in% c(2011)){
    files_import <- append(x = files_import, values = "financiamentos_2011.xlsx")
  }

  if(i %in% c(2012)){
    files_import <- append(x = files_import, values = "financiamentos_2012.xlsx")
  }

  if(i %in% c(2013)){
    files_import <- append(x = files_import, values = "financiamentos_2013.xlsx")
  }

  if(i %in% c(2014)){
    files_import <- append(x = files_import, values = "financiamentos_2014.xlsx")
  }

  if(i %in% c(2015:2016)){
    files_import <- append(x = files_import, values = "financiamentos_2015-2016.xlsx")
  }

  if(i %in% c(2017:2024)){
    files_import <- append(x = files_import, values = "financiamentos_2017-2024.xlsx")
  }

  }

  files_import <- unique(files_import)

  message("Please wait while the files are being imported into R. This may take a while.")

  table_col_names <- c("cliente", "cnpj_cpf", "descricao_projeto",
                       "uf", "municipio", "cod_muni", "num_contrato",
                       "data_contrat", "valor_contratacao_reais",
                       "valor_desembolso_reais", "fonte_desembolso",
                       "custo_financeiro", "juros",
                       "prazo_carencia_meses","prazo_amortizacao_meses",
                       "modalidade", "forma_apoio", "produto",
                       "instrumento_financeiro", "inovacao",
                       "area_operacional", "setor_cnae",
                       "subsetor_cnae_agrup", "cod_subsetor_cnae",
                       "nome_subsetor_cnae", "setor_bndes",
                       "subsetor_bndes", "porte_cliente",
                       "natureza_cliente", "instituicao_financeira",
                       "cnpj_instituicao_financeira", "tipo_garantia",
                       "tipo_excepcionalidade", "situacao_operacional")

  table <- data.frame(matrix(NA, nrow = 0, ncol = 34)) # Create empty data frame
  colnames(table) <- table_col_names

  i <- 1

  for(i in seq_along(files_import)) {

    message(paste0("Importing file ", files_import[i]))

    if (files_import[i] == "financiamentos_nao_automatico.xlsx") {

      tryCatch({
        # import non-automatic operations
        table_temp <- readxl::read_excel(paste0(dir.temp, '/', files_import[i]),
                                         sheet = 1,
                                         skip = 4, # tem que ser 5
                                         col_types = c(rep('text', 34))) |>
          janitor::clean_names()
      },
      # em caso de erro, interrompe a função e mostra msg de erro

      error = function(e) {
        message("Error importing file. Try again later.", e$message)
        stop("Error importing file.")  }
      )

      colnames(table_temp) <- colnames(table)


      # transform non-automatic operations

      table_temp <- table_temp |>
        # format dates
        dplyr::mutate(data_contrat = as.Date(as.numeric(data_contrat),
                                             origin = "1899-12-30",
                                             format = "%Y-%m-%d")) |>
        dplyr::mutate(data_contrat = format(lubridate::ymd(data_contrat), "%d/%m/%Y")) |>
        # add year column
        dplyr::mutate(ano = as.character(lubridate::year(as.Date(data_contrat, format = "%d/%m/%Y")))) |>
        # filter year
        dplyr::filter(ano %in% year) |>
        # contratação
        dplyr::mutate(valor_contratacao_reais = gsub(",", ".", valor_contratacao_reais)) |>
        dplyr::mutate(valor_contratacao_reais = round(as.numeric(valor_contratacao_reais), 2)) |>
        # desembolso
        dplyr::mutate(valor_desembolso_reais = gsub(",", ".", valor_desembolso_reais)) |>
        dplyr::mutate(valor_desembolso_reais = round(as.numeric(valor_desembolso_reais), 2)) |>
        dplyr::mutate(valor_desembolso_reais = ifelse(is.na(valor_desembolso_reais), 0, valor_desembolso_reais)) |>
        # juros
        dplyr::mutate(juros = gsub(",", ".", juros)) |>
        dplyr::mutate(juros = round(as.numeric(juros), 2))

      i <- i + 1

    } else {

      tryCatch({
        # import automatic operations
        table_temp <- readxl::read_excel(paste0(dir.temp, '/', files_import[i]),
                                         sheet = 1,
                                         skip = 5,
                                         col_types = c(rep('text',30))) |>
          janitor::clean_names()
      },
      # em caso de erro, interrompe a função e mostra msg de erro

      error = function(e) {
        message("Error importing file. Try again later.", e$message)
        stop("Error importing file.")  }
      )


      names_auto <- table_col_names[c(1, 2, 4:6, 8:31, 34)]
      colnames(table_temp) <- names_auto
      table_temp$descricao_projeto <- NA
      table_temp$num_contrato <- NA
      table_temp$tipo_garantia <- NA
      table_temp$tipo_excepcionalidade <- NA

      table_temp <- table_temp |>
        dplyr::select(1, 2, 31, 3:7, 32, 8:34)


      # transform automatic operations
      table_temp <- table_temp |>
        # add year column
        dplyr::mutate(ano = lubridate::year(as.Date(data_contrat, format = "%d/%m/%Y"))) |>
        # filter year
        dplyr::filter(ano %in% year) |>
        # contracted values
        dplyr::mutate(valor_contratacao_reais = gsub("[^0-9-]", "", valor_contratacao_reais)) |>
        dplyr::mutate(valor_contratacao_reais = gsub("[^0-9-]", "", valor_contratacao_reais)) |>
        dplyr::mutate(valor_contratacao_reais = as.numeric(valor_contratacao_reais)) |>
        # desimbursed values
        dplyr::mutate(valor_desembolso_reais = gsub("[^0-9-]", "", valor_desembolso_reais)) |>
        dplyr::mutate(valor_desembolso_reais = gsub("[^0-9-]", "", valor_desembolso_reais)) |>
        dplyr::mutate(valor_desembolso_reais = as.numeric(valor_desembolso_reais)) |>
        dplyr::mutate(valor_desembolso_reais = ifelse(is.na(valor_desembolso_reais), 0, valor_desembolso_reais)) |>
        # interest rate
        dplyr::mutate(juros = gsub(",", ".", juros)) |>
        dplyr::mutate(juros = round(as.numeric(juros), 2)) |>
        # standarize values
        dplyr::mutate(subsetor_cnae_agrup = stringr::str_to_upper(subsetor_cnae_agrup))


      i <- i + 1
    }

    table <- rbind(table, table_temp)

  }

  # clean data
  table <- table |>
    # situacao_operacional
    dplyr::mutate(situacao_operacional = replace(situacao_operacional, situacao_operacional == "LIQUIDADA\n", "LIQUIDADO"),
                  situacao_operacional = replace(situacao_operacional, situacao_operacional == "ATIVA\n", "ATIVO")) |>

    dplyr::mutate(prazo_carencia_meses = as.numeric(prazo_carencia_meses)) |>
    dplyr::mutate(prazo_amortizacao_meses = as.numeric(prazo_amortizacao_meses)) |>

    dplyr::mutate(juros = gsub(",", ".", juros)) |>
    dplyr::mutate(juros = round(as.numeric(juros), 2))

  message("Completed data query.")

  old <- options(timeout = 60)
  on.exit(options(old))

  return(table)
  })
}
