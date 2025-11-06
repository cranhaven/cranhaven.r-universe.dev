#' Query data on loan disbursements made through the National Bank for Economic and Social Development (BNDES).
#'
#' Downloads the data on loan disbursements for the selected years, since 1995, and return it in the form of a dataframe.
#'
#' @importFrom utils download.file unzip
#'
#' @param year selects the years which data will be downloaded. integer.
#'
#' @return a dataframe with data for the selected year.
#'
#' @examples
#' \dontrun{query_desimbursements(year = c(1999:2010))}
#'
#' @export
query_desimbursements <- function(year = 'all') {

  options(scipen = 999, timeout = 1500)

  ano <-  NULL

  ano_atual <- as.numeric(format(Sys.Date(), "%Y"))

  if ("all" %in% year) {
    year <- c(1995:ano_atual)
  }

  dir.temp <- tempdir()

  url_list <- c()

  for (i in year) {

    if(i %in% c(1995:2001)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/e2865ce0-5425-442e-bc5b-9693f1b27353/BASE+DE+DADOS+DESEMBOLSO_1995+A+2001.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-e2865ce0-5425-442e-bc5b-9693f1b27353-ohz2o-v"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2002:2008)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/b6a5cb41-e88d-42b5-86a7-35ebaeb42f95/BASE+DE+DADOS+DESEMBOLSO_2002+A+2008.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-b6a5cb41-e88d-42b5-86a7-35ebaeb42f95-ohz39Kp"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2009:2010)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/e6c21560-6a43-46bd-b6c8-a34a7b25fc2c/BASE+DE+DADOS+DESEMBOLSO_2009+E+2010.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-e6c21560-6a43-46bd-b6c8-a34a7b25fc2c-ohz2S79"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2011)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/f349850c-5de1-43d1-8743-cb3c1b4bb07d/BASE+DE+DADOS+DESEMBOLSO_2011.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-f349850c-5de1-43d1-8743-cb3c1b4bb07d-ohz3g8T"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2012)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/9c1ab3d6-707a-4a9a-bd8e-c392ac2dce2e/BASE+DE+DADOS+DESEMBOLSO_2012.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-9c1ab3d6-707a-4a9a-bd8e-c392ac2dce2e-ohz2XhR"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2013)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/82e6909e-8ffd-4995-8d6b-8ea0007712dc/BASE+DE+DADOS+DESEMBOLSO_2013.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-82e6909e-8ffd-4995-8d6b-8ea0007712dc-ohz3kRn"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2014)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/b1b49da4-7038-4a5e-9381-27e53cba4f34/BASE+DE+DADOS+DESEMBOLSO_2014.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-b1b49da4-7038-4a5e-9381-27e53cba4f34-ohz2.Y7"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2015)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/db85f49e-a0c1-4e96-bdfd-eb35a70ccad3/BASE+DE+DADOS+DESEMBOLSO_2015.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-db85f49e-a0c1-4e96-bdfd-eb35a70ccad3-ohz3qcg"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2016:2017)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/fe299328-61bc-489d-8f93-0b18e3925c73/BASE+DE+DADOS+DESEMBOLSO_2016+e+2017.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-fe299328-61bc-489d-8f93-0b18e3925c73-ohz34Qt"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2018)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/2fa8a4c3-da0a-463e-9cba-dfb4a3ffe4d5/BASE+DE+DADOS+DESEMBOLSO_2018.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-2fa8a4c3-da0a-463e-9cba-dfb4a3ffe4d5-ohz3OHe"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2019)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/5b99509d-74e1-43f5-bd51-249ded7bec01/BASE+DE+DADOS+DESEMBOLSO_2019.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-5b99509d-74e1-43f5-bd51-249ded7bec01-ohz3ROg"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2020)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/0405b648-6dc4-4cbc-a2db-635dfc4fbaee/BASE+DE+DADOS+DESEMBOLSO_2020.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-0405b648-6dc4-4cbc-a2db-635dfc4fbaee-ohz3-fq"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2021)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/f4746208-ec83-46a7-8165-9cc702c11d17/BASE+DE+DADOS+DESEMBOLSO_2021.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-f4746208-ec83-46a7-8165-9cc702c11d17-ohz419F"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2022)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/46164445-1557-4991-b816-b9b90491df93/BASE+DE+DADOS+DESEMBOLSO_2022.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-46164445-1557-4991-b816-b9b90491df93-ohz44C-"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2023)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/a0a77a68-eef1-4d2d-83ac-cc122b47003f/BASE+DE+DADOS+DESEMBOLSO_2023.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-a0a77a68-eef1-4d2d-83ac-cc122b47003f-pcsJrcC"
      url_list <- append(x = url_list, values = link)
    }

    if(i %in% c(2024)){
      link <- "https://www.bndes.gov.br/wps/wcm/connect/site/86a46fa7-0f57-4430-8cec-0148b01c19d8/BASE+DE+DADOS+DESEMBOLSO_2024.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-86a46fa7-0f57-4430-8cec-0148b01c19d8-pcsJtTt"
      url_list <- append(x = url_list, values = link)
    }





  }

  # create a vector with the links
  url_list <- unique(url_list)

  for(i in seq_along(url_list)) {

    if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/e2865ce0-5425-442e-bc5b-9693f1b27353/BASE+DE+DADOS+DESEMBOLSO_1995+A+2001.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-e2865ce0-5425-442e-bc5b-9693f1b27353-ohz2o-v"){
      file_name <- "desembolsos_1995_2001"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/b6a5cb41-e88d-42b5-86a7-35ebaeb42f95/BASE+DE+DADOS+DESEMBOLSO_2002+A+2008.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-b6a5cb41-e88d-42b5-86a7-35ebaeb42f95-ohz39Kp") {
      file_name <- "desembolsos_2002_2008"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/e6c21560-6a43-46bd-b6c8-a34a7b25fc2c/BASE+DE+DADOS+DESEMBOLSO_2009+E+2010.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-e6c21560-6a43-46bd-b6c8-a34a7b25fc2c-ohz2S79") {
      file_name <- "desembolsos_2009_2010"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/f349850c-5de1-43d1-8743-cb3c1b4bb07d/BASE+DE+DADOS+DESEMBOLSO_2011.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-f349850c-5de1-43d1-8743-cb3c1b4bb07d-ohz3g8T") {
      file_name <- "desembolsos_2011"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/9c1ab3d6-707a-4a9a-bd8e-c392ac2dce2e/BASE+DE+DADOS+DESEMBOLSO_2012.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-9c1ab3d6-707a-4a9a-bd8e-c392ac2dce2e-ohz2XhR") {
      file_name <- "desembolsos_2012"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/82e6909e-8ffd-4995-8d6b-8ea0007712dc/BASE+DE+DADOS+DESEMBOLSO_2013.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-82e6909e-8ffd-4995-8d6b-8ea0007712dc-ohz3kRn") {
      file_name <- "desembolsos_2013"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/b1b49da4-7038-4a5e-9381-27e53cba4f34/BASE+DE+DADOS+DESEMBOLSO_2014.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-b1b49da4-7038-4a5e-9381-27e53cba4f34-ohz2.Y7") {
      file_name <- "desembolsos_2014"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/db85f49e-a0c1-4e96-bdfd-eb35a70ccad3/BASE+DE+DADOS+DESEMBOLSO_2015.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-db85f49e-a0c1-4e96-bdfd-eb35a70ccad3-ohz3qcg") {
      file_name <- "desembolsos_2015"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/fe299328-61bc-489d-8f93-0b18e3925c73/BASE+DE+DADOS+DESEMBOLSO_2016+e+2017.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-fe299328-61bc-489d-8f93-0b18e3925c73-ohz34Qt") {
      file_name <- "desembolsos_2016_2017"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/2fa8a4c3-da0a-463e-9cba-dfb4a3ffe4d5/BASE+DE+DADOS+DESEMBOLSO_2018.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-2fa8a4c3-da0a-463e-9cba-dfb4a3ffe4d5-ohz3OHe") {
      file_name <- "desembolsos_2018"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/5b99509d-74e1-43f5-bd51-249ded7bec01/BASE+DE+DADOS+DESEMBOLSO_2019.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-5b99509d-74e1-43f5-bd51-249ded7bec01-ohz3ROg") {
      file_name <- "desembolsos_2019"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/0405b648-6dc4-4cbc-a2db-635dfc4fbaee/BASE+DE+DADOS+DESEMBOLSO_2020.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-0405b648-6dc4-4cbc-a2db-635dfc4fbaee-ohz3-fq") {
      file_name <- "desembolsos_2020"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/f4746208-ec83-46a7-8165-9cc702c11d17/BASE+DE+DADOS+DESEMBOLSO_2021.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-f4746208-ec83-46a7-8165-9cc702c11d17-ohz419F") {
      file_name <- "desembolsos_2021"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/46164445-1557-4991-b816-b9b90491df93/BASE+DE+DADOS+DESEMBOLSO_2022.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-46164445-1557-4991-b816-b9b90491df93-ohz44C-") {
      file_name <- "desembolsos_2022"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/a0a77a68-eef1-4d2d-83ac-cc122b47003f/BASE+DE+DADOS+DESEMBOLSO_2023.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-a0a77a68-eef1-4d2d-83ac-cc122b47003f-pcsJrcC") {
      file_name <- "desembolsos_2023"

    } else if(url_list[i] == "https://www.bndes.gov.br/wps/wcm/connect/site/86a46fa7-0f57-4430-8cec-0148b01c19d8/BASE+DE+DADOS+DESEMBOLSO_2024.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-86a46fa7-0f57-4430-8cec-0148b01c19d8-pcsJtTt") {
      file_name <- "desembolsos_2024"

    }

    else {
      file_name <- "data_indeterminada"
    }




    # check if files have already been downloaded
    if(file.exists(paste0(dir.temp, "/", file_name, ".xlsx"))){
      message(paste0("File ", file_name,".xlsx already downloaded."))

    } else {
      if (RCurl::url.exists(url_list[i] == F)) { # network is down = message (not an error anymore)
        message("No internet connection or data source broken.")
        return(NULL)
      } else {

        # nome dos anos de cada arquivo para mostrar no console
        label_arquivos <- c('1995 to 2001', '2002 to 2008', '2009 and 2010')
        label_arquivos_2 <- as.character(2011:2015)
        label_arquivos_3<- c('2016 and 2017')
        label_arquivos_4 <- as.character(2018:2024)
        label_arquivos <- append(label_arquivos,label_arquivos_2)|>
          append(label_arquivos_3)|>
          append(label_arquivos_4)


        message(paste0("Please wait. Downloading data from ", label_arquivos[i]))

        tryCatch({
          download.file(url_list[i],
                        destfile = paste0(dir.temp, "/", file_name, ".xlsx"),
                        mode = "wb")# download the file in binary mode)
        },
        # em caso de erro, interrompe a função e mostra msg de erro

          error = function(e) {
            message("Error downloading file. Try again later.", e$message)
            stop("Error downloading file.")  }
        )
      }
    }
  }

  suppressWarnings({

  # import the files

  lista.arquivos.locais <- list.files(path = dir.temp, pattern = "desembolsos.*\\.xlsx$", full.names = F)

  # Import only the files informed in the year argument

  files_import <- c()

  for(i in year) {

    if(i %in% c(1995:2001)){
      files_import <- append(x = files_import, values = "desembolsos_1995_2001.xlsx")
    }

    if(i %in% c(2002:2008)){
      files_import <- append(x = files_import, values = "desembolsos_2002_2008.xlsx")
    }

    if(i %in% c(2009:2010)){
      files_import <- append(x = files_import, values = "desembolsos_2009_2010.xlsx")
    }

    if(i %in% c(2011)){
      files_import <- append(x = files_import, values = "desembolsos_2011.xlsx")
    }

    if(i %in% c(2012)){
      files_import <- append(x = files_import, values = "desembolsos_2012.xlsx")
    }

    if(i %in% c(2013)){
      files_import <- append(x = files_import, values = "desembolsos_2013.xlsx")
    }

    if(i %in% c(2014)){
      files_import <- append(x = files_import, values = "desembolsos_2014.xlsx")
    }

    if(i %in% c(2015)){
      files_import <- append(x = files_import, values = "desembolsos_2015.xlsx")
    }

    if(i %in% c(2016:2017)){
      files_import <- append(x = files_import, values = "desembolsos_2016_2017.xlsx")
    }

    if(i %in% c(2018)){
      files_import <- append(x = files_import, values = "desembolsos_2018.xlsx")
    }

    if(i %in% c(2019)){
      files_import <- append(x = files_import, values = "desembolsos_2019.xlsx")
    }

    if(i %in% c(2020)){
      files_import <- append(x = files_import, values = "desembolsos_2020.xlsx")
    }

    if(i %in% c(2021)){
      files_import <- append(x = files_import, values = "desembolsos_2021.xlsx")
    }

    if(i %in% c(2022)){
      files_import <- append(x = files_import, values = "desembolsos_2022.xlsx")
    }

    if(i %in% c(2023)){
      files_import <- append(x = files_import, values = "desembolsos_2023.xlsx")
    }

    if(i %in% c(2024)){
      files_import <- append(x = files_import, values = "desembolsos_2024.xlsx")
    }

  }

  files_import <- unique(files_import)

  # print(files_import)

  message("Please wait while the files are being imported into R. This may take a while.")

  table <- data.frame(matrix(NA, nrow = 0, ncol = 16)) # Create empty data frame

  table_col_names <- c("ano","mes","forma_de_apoio","produto","instrumento_financeiro",
                       "inovacao","porte_empresa","regiao","uf","municipio","cod_municipio",
                       "setor_cnae","subsetor_cnae_agrupado","setor_bndes","subsetor_bndes",
                       "desembolsos_reais")

  colnames(table) <- table_col_names


  i <- 1

  for(i in seq_along(files_import)) {

    message(paste0("Importing file ", files_import[i]))


    tryCatch({
      table_temp <- readxl::read_excel(paste0(dir.temp, '/', files_import[i]),
                                       sheet = 2,
                                       skip = 2) |>
        janitor::clean_names()
    },
    # em caso de erro, interrompe a função e mostra msg de erro

     error = function(e) {
        message("Error importing file. Try again later.", e$message)
        stop("Error importing file.")  }
    )


      if(files_import[i] == 'desembolsos_1995_2001.xlsx') {

      names_auto <- table_col_names[c(1:4, 7:16)]
      colnames(table_temp) <- names_auto
      table_temp$instrumento_financeiro <- NA
      table_temp$inovacao <- NA

      table_temp <- table_temp |>
        dplyr::select(1:4,15:16,5:14)

      }

        colnames(table_temp) <- table_col_names

        table <- rbind(table, table_temp)

        i <- i + 1
    }

  table <- table |>
    dplyr::filter(ano %in% year)

  message("Completed data query.")

  old <- options(timeout = 60)
  on.exit(options(old))

  return(table)
  })
}
